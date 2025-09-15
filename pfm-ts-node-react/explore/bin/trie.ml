(* 
   HTTP Route Trie in OCaml
   
   This shows how a Trie naturally works in a functional language.
   Everything is immutable by default, pattern matching is built-in,
   and the fold pattern is idiomatic.
*)

module StringMap = Map.Make(String)
module StringSet = Set.Make(String)

(* The Trie node type - much cleaner than TypeScript! *)
type trie_node = {
  children : trie_node StringMap.t;  (* Map from segment to child node *)
  methods : StringSet.t;              (* Set of HTTP methods at this node *)
}

(* State during traversal - like our TrieStateFP *)
type trie_state = {
  current_node : trie_node option;    (* None means "not found" *)
  captured_params : string list;      (* Parameters captured from ? wildcards *)
}

(* Create an empty node *)
let empty_node = {
  children = StringMap.empty;
  methods = StringSet.empty;
}

(* Add a child to a node - returns new node (immutable!) *)
let add_child node segment child_node =
  { node with children = StringMap.add segment child_node node.children }

(* Add a method to a node *)
let add_method node method_name =
  { node with methods = StringSet.add method_name node.methods }

(* 
   The fold function - this is what gets applied at each step!
   In OCaml, this is much cleaner than TypeScript.
*)
let navigation_step (state : trie_state) (segment : string) : trie_state =
  match state.current_node with
  | None -> 
      (* Already at dead end, stay there *)
      state
  | Some node ->
      (* First try exact match *)
      match StringMap.find_opt segment node.children with
      | Some child -> 
          { current_node = Some child; 
            captured_params = state.captured_params }
      | None ->
          (* No exact match, try wildcard '?' *)
          match StringMap.find_opt "?" node.children with
          | Some child ->
              { current_node = Some child;
                captured_params = segment :: state.captured_params }
          | None ->
              (* No match at all *)
              { current_node = None; 
                captured_params = state.captured_params }

(* Navigate through the trie using fold - this is the key insight! *)
let navigate root segments =
  let initial_state = { current_node = Some root; captured_params = [] } in
  List.fold_left navigation_step initial_state segments

(* Build a sample trie *)
let build_sample_trie () =
  let root = empty_node in
  
  (* Add /users *)
  let users_node = add_method empty_node "GET" in
  let root = add_child root "users" users_node in
  
  (* Add /users/? (wildcard for user ID) *)
  let user_id_node = 
    empty_node 
    |> (fun n -> add_method n "GET")
    |> (fun n -> add_method n "DELETE") in
  let users_node = add_child users_node "?" user_id_node in
  let root = { root with children = StringMap.add "users" users_node root.children } in
  
  (* Add /posts *)
  let posts_node = add_method empty_node "GET" in
  let root = add_child root "posts" posts_node in
  
  root

(* Test the fold iterations step by step *)
let demonstrate_fold_iterations () =
  let root = build_sample_trie () in
  
  print_endline "=== OCaml Trie Navigation Demo ===\n";
  
  (* Show iteration 0: Start at root *)
  let state0 = { current_node = Some root; captured_params = [] } in
  Printf.printf "Iteration 0 (initial): node=%s, params=%s\n" 
    (if Option.is_some state0.current_node then "Some(root)" else "None")
    (String.concat ", " state0.captured_params);
  
  (* Iteration 1: Navigate to "users" *)
  let state1 = navigation_step state0 "users" in
  Printf.printf "Iteration 1 (users):   node=%s, params=%s\n"
    (if Option.is_some state1.current_node then "Some(users)" else "None")
    (String.concat ", " state1.captured_params);
  
  (* Iteration 2: Navigate to "123" (matches wildcard) *)
  let state2 = navigation_step state1 "123" in
  Printf.printf "Iteration 2 (123):     node=%s, params=[%s]\n"
    (if Option.is_some state2.current_node then "Some(?)" else "None")
    (String.concat ", " (List.rev state2.captured_params));
  
  (* Check if GET method exists at final node *)
  (match state2.current_node with
  | Some node -> 
      if StringSet.mem "GET" node.methods then
        print_endline "✓ GET /users/123 found!"
      else
        print_endline "✗ No GET method at this path"
  | None -> 
      print_endline "✗ Path not found");
  
  print_endline "\n=== Using fold_left (the natural way) ===\n";
  
  (* The beautiful part: using fold_left naturally! *)
  let test_paths = [
    (["users"; "123"], "GET");
    (["users"], "GET");
    (["posts"], "GET");
    (["posts"; "456"], "GET");  (* This won't match *)
  ] in
  
  List.iter (fun (segments, method_name) ->
    let final_state = navigate root segments in
    let path = "/" ^ String.concat "/" segments in
    match final_state.current_node with
    | Some node when StringSet.mem method_name node.methods ->
        Printf.printf "✓ %s %s -> Found! Params: [%s]\n" 
          method_name path 
          (String.concat ", " (List.rev final_state.captured_params))
    | Some _ ->
        Printf.printf "✗ %s %s -> Path found but no %s method\n" 
          method_name path method_name
    | None ->
        Printf.printf "✗ %s %s -> Path not found\n" method_name path
  ) test_paths

(* Run the demonstration *)
let () = demonstrate_fold_iterations ()