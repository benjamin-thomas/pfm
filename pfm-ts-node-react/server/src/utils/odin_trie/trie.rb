#!/usr/bin/env ruby

# HTTP Router Trie in Ruby
# Based on the Odin implementation

# Node is either Children (Hash) or Exec (Hash of handlers)
# We'll use a simple convention:
# - If it has :children key, it's a branch node
# - If it has :exec key, it's a leaf node

# Sample handlers
def show_profile
  puts "show profile"
end

def list_users
  puts "list all users"
end

def get_user
  puts "get user by id"
end

def list_posts
  puts "list all posts"
end

def create_post
  puts "create a post"
end

def delete_post
  puts "delete a post"
end

# Route dispatcher - walks the trie to find handler
def dispatch(root, method, path)
  # Split path into segments (skip empty strings)
  segments = path.split('/').reject(&:empty?)
  
  # Start at root
  current = root
  
  # Walk through each segment
  segments.each do |seg|
    # Current must be Children (has :children key) to continue
    unless current[:children]
      puts "Error: hit a leaf node at segment '#{seg}'"
      return false
    end
    
    children = current[:children]
    
    # Try exact match first
    if children[seg]
      current = children[seg]
    elsif children["?"]  # Try wildcard match
      current = children["?"]
    else
      puts "Not found: no route for segment '#{seg}'"
      return false
    end
  end
  
  # Current might have :exec or need to check for "" key
  if current[:exec]
    exec = current[:exec]
  elsif current[:children] && current[:children][""]
    # If it's Children, look for the empty string key
    terminal = current[:children][""]
    if terminal[:exec]
      exec = terminal[:exec]
    else
      puts "Error: terminal node is not an Exec"
      return false
    end
  else
    puts "Error: path leads to a branch with no terminal handler"
    return false
  end
  
  # Get the handler for the method
  handler = case method
  when "GET" then exec[:get]
  when "POST" then exec[:post]
  when "DELETE" then exec[:delete]
  else
    puts "Error: unsupported method '#{method}'"
    return false
  end
  
  # Execute if handler exists
  if handler
    handler.call
    true
  else
    puts "Error: no handler for #{method} #{path}"
    false
  end
end

# Build the trie
# Use empty string "" as a special key for "handler at this path"
root = {
  children: {
    "api" => {
      children: {
        "profile" => {
          exec: { get: method(:show_profile) }
        },
        "users" => {
          children: {
            "" => { exec: { get: method(:list_users) } },  # GET /api/users
            "?" => { exec: { get: method(:get_user) } }    # GET /api/users/:id
          }
        },
        "posts" => {
          children: {
            "?" => {
              exec: {
                get: method(:list_posts),
                post: method(:create_post),
                delete: method(:delete_post)
              }
            }
          }
        }
      }
    }
  }
}

# CLI
puts "HTTP Router CLI"
puts "Enter: <METHOD> <PATH> (e.g., 'GET /api/profile')"
puts "Type 'quit' to exit\n\n"

loop do
  print "> "
  input = gets
  break unless input  # EOF (Ctrl+D)
  
  input = input.strip
  next if input.empty?  # Skip empty lines
  break if input == "quit" || input == "exit"
  
  # Parse input
  parts = input.split(' ', 2)
  if parts.length != 2
    puts "Usage: <METHOD> <PATH>"
    next
  end
  
  method, path = parts
  
  # Dispatch the request
  puts "Dispatching: #{method} #{path}"
  unless dispatch(root, method, path)
    puts "Failed to dispatch request"
  end
  puts
end

puts "Goodbye!"