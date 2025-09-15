import { describe, expect, it } from 'vitest';
import { HashMap, HashSet, Data, Option } from 'effect';

// Functional Trie implementation with persistent data structures
type TrieNodeFP = {
    readonly children: HashMap.HashMap<string, TrieNodeFP>;
    readonly methods: HashSet.HashSet<string>;
}

type TrieStateFP = {
    readonly currentNode: Option.Option<TrieNodeFP>;  // None means "not found" or "end of path"
    readonly capturedParams: readonly string[];
}

// Helper functions using Data.struct for immutable creation
const createTrieNodeFP = (
    children: HashMap.HashMap<string, TrieNodeFP> = HashMap.empty(),
    methods: HashSet.HashSet<string> = HashSet.empty()
): TrieNodeFP =>
    Data.struct({
        children,
        methods
    });

const createTrieStateFP = (
    currentNode: Option.Option<TrieNodeFP>,
    capturedParams: readonly string[] = []
): TrieStateFP =>
    Data.struct({
        currentNode,
        capturedParams
    });

// Minimal Trie implementation (mutable)
type TrieNode = {
    children: Map<string, TrieNode>;
    methods: Set<string>;  // Store which HTTP methods are registered at this path
}

class RouteTrie {
    private root: TrieNode = {
        children: new Map(),
        methods: new Set()
    };

    addRoute(method: string, path: string): void {
        const segments = path.split('/').filter(s => s !== ''); // Split and remove empty strings
        let current = this.root;

        // Navigate/create path in trie
        for (const segment of segments) {
            let next = current.children.get(segment);
            if (!next) {
                next = {
                    children: new Map(),
                    methods: new Set()
                };
                current.children.set(segment, next);
            }
            current = next;
        }

        // Mark this node as having this HTTP method
        current.methods.add(method);
    }

    lookup(method: string, path: string): { found: boolean; hops: number; path: string[]; capturedParams: string[] } {
        const segments = path
            .split('/')
            .filter(s => s); // Remove heading or trailing slashes
        let current = this.root;
        let hops = 0;
        const pathTaken: string[] = [];  // Just the segments we traversed
        const capturedParams: string[] = [];  // Parameters captured from ? wildcards

        // Navigate through trie
        for (const segment of segments) {
            hops++; // Count each navigation as a hop

            // First try exact match
            let next = current.children.get(segment);

            // If no exact match, try wildcard '?'
            if (!next) {
                next = current.children.get('?');
                if (next) {
                    // Capture the parameter value
                    capturedParams.push(segment);
                }
            }

            if (!next) {
                return { found: false, hops, path: pathTaken, capturedParams };
            }

            current = next;
            pathTaken.push(segment);
        }

        // Check if this method exists at this path
        hops++; // Count method check as a hop
        const found = current.methods.has(method);

        return { found, hops, path: pathTaken, capturedParams };
    }
}

// First, let's understand what we're building
describe('Trie-based HTTP Dispatch', () => {
    it('should find exact route with fewer hops than linear search', () => {
        // We'll build a Trie that can track how many "hops" it takes to find a route
        const trie = new RouteTrie();

        // Add some routes to make the comparison meaningful
        trie.addRoute('GET', '/users');
        trie.addRoute('GET', '/posts');
        trie.addRoute('GET', '/comments');
        trie.addRoute('GET', '/health');
        trie.addRoute('GET', '/api/v1/users');

        // Now lookup /health and track hops
        const result = trie.lookup('GET', '/health');

        expect(result.found).toBe(true);
        expect(result.hops).toBe(2); // 1 hop to 'health' node, 1 to check method

        // Linear search would have checked 4 routes before finding /health
        expect(result.hops).toBeLessThan(4);

        // Show the path taken (just the segments)
        expect(result.path).toEqual(['health']);
    });

    it('should handle parameter matching with ? wildcard', () => {
        const trie = new RouteTrie();

        // Add routes with parameters
        trie.addRoute('GET', '/users/?');  // /users/{id}
        trie.addRoute('GET', '/users');    // exact /users
        trie.addRoute('POST', '/users/?'); // different method

        // Lookup with parameter should match the wildcard
        const result1 = trie.lookup('GET', '/users/123');
        expect(result1.found).toBe(true);
        expect(result1.hops).toBe(3); // hop to 'users', hop to '?' wildcard, hop to check method
        expect(result1.path).toEqual(['users', '123']);
        expect(result1.capturedParams).toEqual(['123']);  // New: captured parameter values

        // Exact match should still work
        const result2 = trie.lookup('GET', '/users');
        expect(result2.found).toBe(true);
        expect(result2.hops).toBe(2); // hop to 'users', hop to check method
        expect(result2.capturedParams).toEqual([]);  // No parameters captured

        // Should respect HTTP method
        const result3 = trie.lookup('DELETE', '/users/123');
        expect(result3.found).toBe(false);
        expect(result3.hops).toBe(3); // Still had to traverse to check
    });
});

// The fold function - normally this would be anonymous inside reduce()
const trieNavigationStepFP = (stateFP: TrieStateFP, segment: string): TrieStateFP => {
    // If we already hit a dead end (None), stay in None state
    return Option.match(stateFP.currentNode, {
        onNone: () => stateFP,  // Already at dead end, remain there
        onSome: (currentNode) => {
            // Try to find the next node for this segment
            const nextNodeOption = HashMap.get(currentNode.children, segment);

            // Create new state with the found node (or None if not found)
            return createTrieStateFP(nextNodeOption, stateFP.capturedParams);
        }
    });
};

// Functional Programming approach - test the fold function directly
describe('FP Trie with testable fold iterations', () => {
    it('should handle fold iteration 0: navigate from root to first segment', () => {
        // Set up a Trie with some structure
        const usersNodeFP = createTrieNodeFP(
            HashMap.empty(),
            HashSet.fromIterable(['GET'])
        );

        const rootWithUsersFP = createTrieNodeFP(
            HashMap.fromIterable([['users', usersNodeFP]]),
            HashSet.empty()
        );

        // Initial state - starting at root
        const initialStateFP = createTrieStateFP(Option.some(rootWithUsersFP), []);

        // This is the fold function we want to test - iteration 0
        const resultFP = trieNavigationStepFP(initialStateFP, 'users');

        expect(resultFP.currentNode).toEqual(Option.some(usersNodeFP)); // navigated to users node
        expect(resultFP.capturedParams).toEqual([]);                   // no params captured yet
    });

    it('should show full path traversal: /users/123 step by step', () => {
        // Build a more complex Trie
        const idNodeFP = createTrieNodeFP(
            HashMap.empty(),
            HashSet.fromIterable(['GET', 'DELETE'])  // Has GET and DELETE methods
        );
        
        const usersNodeFP = createTrieNodeFP(
            HashMap.fromIterable([['?', idNodeFP]]),  // Has wildcard child
            HashSet.fromIterable(['GET'])             // Also handles GET /users
        );
        
        const rootNodeFP = createTrieNodeFP(
            HashMap.fromIterable([['users', usersNodeFP]]),
            HashSet.empty()
        );
        
        // Start at root
        const state0 = createTrieStateFP(Option.some(rootNodeFP), []);
        
        // Iteration 1: Navigate to 'users'
        const state1 = trieNavigationStepFP(state0, 'users');
        expect(Option.isSome(state1.currentNode)).toBe(true);
        expect(state1.capturedParams).toEqual([]);  // No params captured yet
        
        // Iteration 2: Navigate to '123' (should NOT match - we don't handle wildcards yet!)
        const state2 = trieNavigationStepFP(state1, '123');
        expect(Option.isNone(state2.currentNode)).toBe(true); // Should be None - we don't handle '?' yet
        expect(state2.capturedParams).toEqual([]);  // No params captured since we don't handle wildcards
        
        // This test shows we need to update our fold function to handle wildcards!
    });
    
    it('should handle "not found" case - path that does not exist', () => {
        const rootNodeFP = createTrieNodeFP(
            HashMap.fromIterable([['users', createTrieNodeFP()]]),
            HashSet.empty()
        );
        
        const state0 = createTrieStateFP(Option.some(rootNodeFP), []);
        
        // Try to navigate to non-existent path
        const state1 = trieNavigationStepFP(state0, 'posts');
        
        expect(Option.isNone(state1.currentNode)).toBe(true);  // Should be None
        expect(state1.capturedParams).toEqual([]);
        
        // Once we hit None, further navigation stays at None
        const state2 = trieNavigationStepFP(state1, 'anything');
        expect(Option.isNone(state2.currentNode)).toBe(true);  // Still None
    });
    
    it('should demonstrate reduce usage with multiple segments', () => {
        // This shows how you'd actually use the fold function with reduce!
        const idNodeFP = createTrieNodeFP(
            HashMap.empty(),
            HashSet.fromIterable(['GET'])
        );
        
        const usersNodeFP = createTrieNodeFP(
            HashMap.fromIterable([['123', idNodeFP]]),  // Exact match for '123'
            HashSet.empty()
        );
        
        const rootNodeFP = createTrieNodeFP(
            HashMap.fromIterable([['users', usersNodeFP]]),
            HashSet.empty()
        );
        
        const segments = ['users', '123'];
        const initialState = createTrieStateFP(Option.some(rootNodeFP), []);
        
        // This is what a fold/reduce would do!
        const finalState = segments.reduce(trieNavigationStepFP, initialState);
        
        expect(Option.isSome(finalState.currentNode)).toBe(true);
        
        // We can check the final node has the right methods
        Option.match(finalState.currentNode, {
            onNone: () => expect(true).toBe(false), // Should not be None
            onSome: (node) => {
                expect(HashSet.has(node.methods, 'GET')).toBe(true);
            }
        });
    });
});
