package trie

import "core:fmt"
import "core:os"
import "core:strings"


/*

Run interactively:
  rlwrap odin run ./trie.odin -file

Test single route:
  echo "GET /api/profile" | odin run ./trie.odin -file

Test multiple routes:
  echo -e "GET /api/profile\nGET /api/users/123\nPOST /api/posts/456\nquit" | odin run ./trie.odin -file

  We handle trailing slashes too!
  echo -e "GET /api/users/1\nGET /api/users/\nGET /api/users\nquit" | odin run ./trie.odin -file

*/

// Handler is just a procedure
Handler :: proc()

// Node is a tagged union - either Children or Exec
Node :: union {
	Children,
	Exec,
}

// Children maps string keys to nodes (like JS object)
Children :: map[string]Node

// Exec holds handlers for different HTTP methods
Exec :: struct {
	get:    Maybe(Handler),
	post:   Maybe(Handler),
	delete: Maybe(Handler),
}

// Sample handlers
show_profile :: proc() {
	fmt.println("show profile")
}

list_users :: proc() {
	fmt.println("list all users")
}

get_user :: proc() {
	fmt.println("get user by id")
}

list_posts :: proc() {
	fmt.println("list all posts")
}

create_post :: proc() {
	fmt.println("create a post")
}

delete_post :: proc() {
	fmt.println("delete a post")
}

// Route dispatcher - walks the trie to find handler
dispatch :: proc(root: Children, method: string, path: string) -> bool {
	// Split path into segments (skip empty strings from leading /)
	segments := strings.split(path, "/")
	defer delete(segments)

	// Start at root
	current: Node = root

	// Walk through each segment
	for seg in segments {
		if seg == "" do continue // skip empty segments

		// Current must be Children to continue
		children, ok := current.(Children)
		if !ok {
			fmt.printf("Error: hit a leaf node at segment '%s'\n", seg)
			return false
		}

		// Try exact match first
		if next, exists := children[seg]; exists {
			current = next
		} else if wildcard, exists := children["?"]; exists {
			// Try wildcard match
			current = wildcard
		} else {
			fmt.printf("Not found: no route for segment '%s'\n", seg)
			return false
		}
	}

	// Current might be Children (check for "" key) or Exec
	exec, is_exec := current.(Exec)
	if !is_exec {
		// If it's Children, look for the empty string key
		if children, is_children := current.(Children); is_children {
			if terminal, exists := children[""]; exists {
				exec, is_exec = terminal.(Exec)
				if !is_exec {
					fmt.println("Error: terminal node is not an Exec")
					return false
				}
			} else {
				fmt.println("Error: path leads to a branch with no terminal handler")
				return false
			}
		} else {
			fmt.println("Error: unexpected node type")
			return false
		}
	}

	// Get the handler for the method
	handler: Maybe(Handler)
	switch method {
	case "GET":
		handler = exec.get
	case "POST":
		handler = exec.post
	case "DELETE":
		handler = exec.delete
	case:
		fmt.printf("Error: unsupported method '%s'\n", method)
		return false
	}

	// Execute if handler exists
	if h, ok := handler.?; ok {
		h()
		return true
	} else {
		fmt.printf("Error: no handler for %s %s\n", method, path)
		return false
	}
}

main :: proc() {
	// Build the trie
	// Use empty string "" as a special key for "handler at this path"
	root := Children {
		"api" = Children {
			"profile" = Exec{get = show_profile},
			"users" = Children {
				"" = Exec{get = list_users}, // GET /api/users
				"?" = Exec{get = get_user}, // GET /api/users/:id
			},
			"posts" = Children {
				"?" = Exec{get = list_posts, post = create_post, delete = delete_post},
			},
		},
	}

	fmt.println("HTTP Router CLI")
	fmt.println("Enter: <METHOD> <PATH> (e.g., 'GET /api/profile')")
	fmt.println("Type 'quit' to exit\n")

	// Read input loop - line by line
	buf: [1024]byte
	for {
		fmt.print("> ")
		n, err := os.read(os.stdin, buf[:])
		if err != 0 || n == 0 {
			break // EOF (Ctrl+D) or error
		}

		// Split input into lines and process each
		input_str := string(buf[:n])
		lines := strings.split(input_str, "\n")
		defer delete(lines)

		for line in lines {
			line := strings.trim_space(line)
			if line == "" {
				continue // Skip empty lines
			}
			if line == "quit" || line == "exit" {
				fmt.println("Goodbye!")
				return
			}

			// Parse input - split only on first space
			parts := strings.split_n(line, " ", 2)
			defer delete(parts)

			if len(parts) != 2 {
				fmt.println("Usage: <METHOD> <PATH>")
				continue
			}

			method := parts[0]
			path := parts[1]

			// Dispatch the request
			fmt.printf("Dispatching: %s %s\n", method, path)
			if !dispatch(root, method, path) {
				fmt.println("Failed to dispatch request")
			}
			fmt.println()
		}
	}

	fmt.println("Goodbye!")
}
