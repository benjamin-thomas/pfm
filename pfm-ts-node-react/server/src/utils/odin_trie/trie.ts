#!/usr/bin/env tsx

// HTTP Router Trie in TypeScript
// Based on the Odin implementation

import * as readline from 'readline';
import * as fs from 'fs';


/*

Usage:

rlwrap does not work well with tsx, so we implemented our own history mechanism.
./trie.ts
echo -e "GET /api/users/1\nGET /api/users/\nGET /api/users\nquit" | ./trie.ts

*/

// Handler is just a function
type Handler = () => void;

// Node is a tagged union - either Children or Exec
type Node = Children | Exec;

// Children maps string keys to nodes (like JS object)
type Children = {
  children: Record<string, Node>;
};

// Exec holds handlers for different HTTP methods
type Exec = {
  exec: {
    get?: Handler;
    post?: Handler;
    delete?: Handler;
  };
};

// Type guards
function isChildren(node: Node): node is Children {
  return 'children' in node;
}

function isExec(node: Node): node is Exec {
  return 'exec' in node;
}

// Sample handlers
const showProfile = (): void => console.log("show profile");
const listUsers = (): void => console.log("list all users");
const getUser = (): void => console.log("get user by id");
const listPosts = (): void => console.log("list all posts");
const createPost = (): void => console.log("create a post");
const deletePost = (): void => console.log("delete a post");

// Route dispatcher - walks the trie to find handler
function dispatch(root: Children, method: string, path: string): boolean {
  // Split path into segments (skip empty strings)
  const segments = path.split('/').filter(seg => seg !== '');

  // Start at root
  let current: Node = root;

  // Walk through each segment
  for (const seg of segments) {
    // Current must be Children to continue
    if (!isChildren(current)) {
      console.log(`Error: hit a leaf node at segment '${seg}'`);
      return false;
    }

    const children: Record<string, Node> = current.children;

    // Try exact match first
    if (children[seg]) {
      current = children[seg];
    } else if (children["?"]) {  // Try wildcard match
      current = children["?"];
    } else {
      console.log(`Not found: no route for segment '${seg}'`);
      return false;
    }
  }

  // Current might be Exec or Children (check for "" key)
  let exec: Exec['exec'];

  if (isExec(current)) {
    exec = current.exec;
  } else if (isChildren(current) && current.children[""]) {
    // If it's Children, look for the empty string key
    const terminal = current.children[""];
    if (isExec(terminal)) {
      exec = terminal.exec;
    } else {
      console.log("Error: terminal node is not an Exec");
      return false;
    }
  } else {
    console.log("Error: path leads to a branch with no terminal handler");
    return false;
  }

  // Get the handler for the method
  let handler: Handler | undefined;
  switch (method) {
    case "GET":
      handler = exec.get;
      break;
    case "POST":
      handler = exec.post;
      break;
    case "DELETE":
      handler = exec.delete;
      break;
    default:
      console.log(`Error: unsupported method '${method}'`);
      return false;
  }

  // Execute if handler exists
  if (handler) {
    handler();
    return true;
  } else {
    console.log(`Error: no handler for ${method} ${path}`);
    return false;
  }
}

// Build the trie
// Use empty string "" as a special key for "handler at this path"
const root: Children = {
  children: {
    "api": {
      children: {
        "profile": {
          exec: { get: showProfile }
        },
        "users": {
          children: {
            "": { exec: { get: listUsers } },  // GET /api/users
            "?": { exec: { get: getUser } }    // GET /api/users/:id
          }
        },
        "posts": {
          children: {
            "?": {
              exec: {
                get: listPosts,
                post: createPost,
                delete: deletePost
              }
            }
          }
        }
      }
    }
  }
};

// History file management
const historyFile = '/tmp/trie_history';

function loadHistory(): string[] {
  try {
    if (fs.existsSync(historyFile)) {
      return fs.readFileSync(historyFile, 'utf8')
        .split('\n')
        .filter(line => line.trim() !== '')
        .slice(-100); // Keep last 100 entries
    }
  } catch (err) {
    console.warn('Failed to load history:', err);
  }
  return [];
}

function saveHistory(history: string[]): void {
  try {
    fs.writeFileSync(historyFile, history.join('\n') + '\n');
  } catch (err) {
    console.warn('Failed to save history:', err);
  }
}

// CLI
console.log("HTTP Router CLI");
console.log("Enter: <METHOD> <PATH> (e.g., 'GET /api/profile')");
console.log("Type 'quit' to exit\n");

const history = loadHistory();
const rl = readline.createInterface({
  input: process.stdin,
  output: process.stdout,
  prompt: '> ',
  history: history,
  historySize: 100,
  removeHistoryDuplicates: true
});

rl.prompt();

rl.on('line', (input: string) => {
  input = input.trim();

  if (input === '' || !input) {
    rl.prompt();
    return;
  }

  if (input === 'quit' || input === 'exit') {
    console.log("Goodbye!");
    rl.close();
    return;
  }

  // Parse input
  const parts = input.split(' ');
  if (parts.length !== 2) {
    console.log("Usage: <METHOD> <PATH>");
    rl.prompt();
    return;
  }

  const [method, path] = parts;
  if (!method || !path) {
    console.log("Usage: <METHOD> <PATH>");
    rl.prompt();
    return;
  }

  // Dispatch the request
  console.log(`Dispatching: ${method} ${path}`);
  if (!dispatch(root, method, path)) {
    console.log("Failed to dispatch request");
  }
  console.log();
  rl.prompt();
});

rl.on('close', () => {
  // Save history to file before exiting
  // eslint-disable-next-line @typescript-eslint/no-explicit-any
  saveHistory((rl as any).history);
  setTimeout(() => {
    process.exit(0);
  }, 100);
});

// Handle SIGINT (Ctrl+C) properly
process.on('SIGINT', () => {
  console.log("\nGoodbye!");
  saveHistory((rl as unknown as { history: string[] }).history); // lol
  rl.close();
});