// Modeled after: server/src/utils/simpleTrie2.test.ts

// Compile and run with:
// gcc src/utils/simple_trie.c -o /tmp/simple_trie && /tmp/simple_trie

#include <stdio.h>

// Forward declaration needed for recursive type
// (node_branch will contain pointers to node)
typedef struct node node;

// A leaf contains a function pointer
typedef struct {
  void (*func)(void);
} node_leaf;

// A branch contains child nodes
// In a real trie, this would be a hash map or array
// For now, just a simple linked list of key-value pairs
typedef struct {
  char* key;       // e.g., "api", "users", "?"
  node* child;     // pointer to child node
  node* next;      // next sibling (for multiple children)
} node_branch;

// A node is EITHER a branch OR a leaf
// This is our "sum type" - tagged union pattern
struct node {
  enum { BRANCH, LEAF } tag;  // discriminator/tag
  union {                      // only one of these exists at a time
    node_branch branch;
    node_leaf leaf;
  } data;
};

// Sample functions to store in leaves
void show_profile() {
  printf("Executing: show profile\n");
}

void list_users() {
  printf("Executing: list users\n");
}

int main() {
  // Create a leaf node with a function
  node leaf1;
  leaf1.tag = LEAF;
  leaf1.data.leaf.func = show_profile;

  // Create a branch node that points to the leaf
  node branch1;
  branch1.tag = BRANCH;
  branch1.data.branch.key = "profile";
  branch1.data.branch.child = &leaf1;  // points to our leaf
  branch1.data.branch.next = NULL;     // no siblings

  // Test: navigate and execute
  if (branch1.tag == BRANCH) {
    printf("Branch key: %s\n", branch1.data.branch.key);
    
    // Navigate to child
    node* child = branch1.data.branch.child;
    if (child->tag == LEAF && child->data.leaf.func) {
      child->data.leaf.func();  // Call the function!
    }
  }

  return 0;
}
