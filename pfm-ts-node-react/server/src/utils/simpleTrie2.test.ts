import { describe, it } from "vitest";

type HttpVerb = 'GET' | 'POST' | 'PUT' | 'DELETE' | 'OPTIONS' | 'PATCH';

type PathTrieNodeContinue = { children: Record<string, PathTrieNode> }
type PathTrieNodeTerminal = { execs: Partial<Record<HttpVerb, () => void>> }

type PathTrieNode = PathTrieNodeContinue | PathTrieNodeTerminal;


// Type guards
const isTerminal = (node: PathTrieNode): node is PathTrieNodeTerminal => {
    return 'execs' in node;
};

const isContinue = (node: PathTrieNode): node is PathTrieNodeContinue => {
    return 'children' in node;
};

const pathTrieNodeShow = (root: PathTrieNode): string => {
    const lines: string[] = [];
    lines.push(`(root)`);

    const sortEntries = (node: PathTrieNode): [string, PathTrieNode][] => {
        if (!isContinue(node)) return [];
        return Object.entries(node.children).sort(([a], [b]) => a.localeCompare(b));
    };

    const walk = (node: PathTrieNode, prefix: string): void => {
        const entries = sortEntries(node);
        entries.forEach(([ch, child], index) => {
            const isLast = index === entries.length - 1;
            const connector = isLast ? "└─ " : "├─ ";
            // const endMark = isEmpty(child.children) ? "•" : "";
            const endMark = isTerminal(child) ? "•" : "";
            lines.push(`${prefix}${connector}${ch}${endMark}`);
            const nextPrefix = prefix + (isLast ? "   " : "│  ");
            walk(child, nextPrefix);
        });
    };

    walk(root, "");
    return lines.join("\n");
};


describe('simpleTrie2', () => {

    it("path matching test", () => {

        const root: PathTrieNode = {
            children: {
                'api': {
                    children: {
                        'profile': {
                            execs: {
                                'GET': () => console.log("show profile")
                            }
                        }
                        ,
                        'users': {
                            children: {
                                '?': {
                                    execs: {
                                        GET: () => console.log("list users")
                                    }
                                }
                            }
                        },
                        'posts': {
                            children: {
                                '?': {
                                    execs: {
                                        GET: () => console.log("list all posts"),
                                        POST: () => console.log("create a post"),
                                        DELETE: () => console.log("delete a post"),
                                    }
                                }
                            }
                        },
                        // 'maths': {
                        //     children: {
                        //         'add': { children: { '?': { children: { '?': { execs: () => console.log("add") } } } } },
                        //         'sub': { children: { '?': { children: { '?': { exec: () => console.log("sub") } } } } },
                        //         'mul': { children: { '?': { children: { '?': { exec: () => console.log("mul") } } } } },
                        //         'div': { children: { '?': { children: { '?': { exec: () => console.log("div") } } } } },
                        //     }
                        // }
                    }
                }
            }
        };

        console.log("---");
        console.log(pathTrieNodeShow(root));
        console.log("---");
    });
});