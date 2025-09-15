class TrieNode {
    children: Map<string, TrieNode>;
    isEndOfWord: boolean;

    constructor() {
        this.children = new Map();
        this.isEndOfWord = false;
    }
}

class Trie {
    root: TrieNode;

    constructor() {
        this.root = new TrieNode();
    }

    insert(word: string): void {
        let node = this.root;
        for (const char of word) {
            let nextNode = node.children.get(char);
            if (!nextNode) {
                nextNode = new TrieNode();
                node.children.set(char, nextNode);
            }
            node = nextNode;
        }
        node.isEndOfWord = true;
    }

    toString(): string {
        const lines: string[] = [];
        const rootMark = this.root.isEndOfWord ? "•" : "";
        lines.push(`(root)${rootMark}`);

        const sortEntries = (node: TrieNode): [string, TrieNode][] =>
            Array.from(node.children.entries()).sort(([a], [b]) => a.localeCompare(b));

        const walk = (node: TrieNode, prefix: string): void => {
            const entries = sortEntries(node);
            entries.forEach(([ch, child], index) => {
                const isLast = index === entries.length - 1;
                const connector = isLast ? "└─ " : "├─ ";
                const endMark = child.isEndOfWord ? "•" : "";
                lines.push(`${prefix}${connector}${ch}${endMark}`);
                const nextPrefix = prefix + (isLast ? "   " : "│  ");
                walk(child, nextPrefix);
            });
        };

        walk(this.root, "");
        return lines.join("\n");
    }

    search(word: string): boolean {
        let node = this.root;
        for (const char of word) {
            const nextNode = node.children.get(char);
            if (!nextNode) return false;
            node = nextNode;
        }
        return node.isEndOfWord;
    }
}

import { describe, expect, it } from 'vitest';

// // Example usage
// const trie = new Trie();
// trie.insert("apple");
// console.log(trie.search("apple")); // Output: true
// console.log(trie.search("app"));   // Output: false

describe('Simple Trie', () => {
    // --testTimeout=0
    it('should insert and search words', () => {
        const trie = new Trie();
        trie.insert("apple");
        trie.insert("application");
        expect(trie.search("apple")).toBe(true);
        expect(trie.search("app")).toBe(false);
    });

    it('should visualize the trie (ASCII and DOT)', () => {
        const trie = new Trie();
        ["apple", "application", "app", "apt", "bat", "batch"].forEach(w => trie.insert(w));

        const ascii = trie.toString();
        // const dot = trie.toDot();

        // Print for visual inspection
        console.log("\nASCII visualization:\n" + ascii);
        // console.log("\nGraphviz DOT:\n" + dot);

        // Sanity checks
        expect(ascii).toContain("(root)");
        // expect(dot.startsWith("digraph Trie")).toBe(true);
    });
});