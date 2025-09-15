(*{|


rlwrap dune exec ./trie.exe
TEST=1 dune exec ./trie.exe

echo -e "GET /api/users\nGET /api/users/123\nPOST /api/posts/456\nquit" | rlwrap dune exec ./trie.exe
echo -e "GET /api/users\nGET /api/users/123\nPOST /api/posts/456\nquit" | ocaml ./trie.ml

|}*)

(* HTTP Router Trie in OCaml *)
(* Based on the Odin implementation *)

module StringMap = Map.Make (String)

(* HTTP Verb type and map *)
type verb =
  | GET
  | POST
  | PUT
  | PATCH
  | DELETE
  | HEAD
  | OPTIONS
  | TRACE
  | CONNECT

module VerbMap = Map.Make (struct
    type t = verb

    let compare = compare
  end)

(* IO Module Type - abstraction for input/output operations *)
module type IO = sig
  val output : string -> unit
  val input : string -> string option (* prompt -> input option *)
end

(* Handler accepts IO module for output *)
type handler = (module IO) -> unit

(* Node is a variant type - either Children or Exec *)
type node =
  | Children of node StringMap.t
  | Exec of exec_handlers

(* Exec holds handlers for different HTTP methods
   Map from HTTP verb to handler function
   Also see: https://developer.mozilla.org/en-US/docs/Web/HTTP/Reference/Methods
*)
and exec_handlers = handler VerbMap.t

(* Convert string to verb *)
let verb_of_string = function
  | "GET" -> Some GET
  | "POST" -> Some POST
  | "PUT" -> Some PUT
  | "PATCH" -> Some PATCH
  | "DELETE" -> Some DELETE
  | "HEAD" -> Some HEAD
  | "OPTIONS" -> Some OPTIONS
  | "TRACE" -> Some TRACE
  | "CONNECT" -> Some CONNECT
  | _ -> None
;;

(* Sample handlers - now use IO module for output *)
let show_profile (module IO : IO) = IO.output "show profile"
let list_users (module IO : IO) = IO.output "list all users"
let get_user (module IO : IO) = IO.output "get user by id"
let get_post (module IO : IO) = IO.output "get post by id"
let list_posts (module IO : IO) = IO.output "list all posts"
let create_post (module IO : IO) = IO.output "create a post"
let update_post action (module IO : IO) = IO.output ("update a post: " ^ action)
let delete_post (module IO : IO) = IO.output "delete a post"
let delete_all_posts (module IO : IO) = IO.output "delete all posts"

(* Route dispatcher - walks the trie to find handler *)
let dispatch (module IO : IO) (root : node) (method_name : string) (path : string) : unit =
  (* Split path into segments (skip empty strings) *)
  let segments = String.split_on_char '/' path |> List.filter (fun s -> s <> "") in
  (* Walk through each segment *)
  let rec walk_segments current = function
    | [] -> current
    | seg :: rest ->
      (match current with
       | Exec _ ->
         ()
         ; IO.output (Printf.sprintf "Error: hit a leaf node at segment '%s'" seg)
         ; failwith "routing error"
       | Children children ->
         (* Try exact match first *)
         (match StringMap.find_opt seg children with
          | Some next -> walk_segments next rest
          | None ->
            (* Try wildcard match *)
            (match StringMap.find_opt "?" children with
             | Some next -> walk_segments next rest
             | None ->
               ()
               ; IO.output (Printf.sprintf "Not found: no route for segment '%s'" seg)
               ; failwith "routing error")))
  in
  try
    let final_node = walk_segments root segments in
    (* Final node might be Exec or Children (check for "" key) *)
    let exec =
      match final_node with
      | Exec handlers -> handlers
      | Children children ->
        (* Look for the empty string key *)
        (match StringMap.find_opt "" children with
         | Some (Exec handlers) -> handlers
         | Some (Children _) ->
           ()
           ; IO.output "Error: terminal node is not an Exec"
           ; failwith "routing error"
         | None ->
           ()
           ; IO.output "Error: path leads to a branch with no terminal handler"
           ; failwith "routing error")
    in
    (* Get the handler for the method *)
    let handler =
      match verb_of_string method_name with
      | Some verb -> VerbMap.find_opt verb exec
      | None ->
        ()
        ; IO.output (Printf.sprintf "Error: unsupported method '%s'" method_name)
        ; None
    in
    (* Execute if handler exists *)
    match handler with
    | Some h -> h (module IO : IO)
    | None -> IO.output (Printf.sprintf "Error: no handler for %s %s" method_name path)
  with
  | Failure _ ->
    IO.output (Printf.sprintf "Error: routing failed for %s %s" method_name path)
;;

(* Build the trie *)
(* Use empty string "" as a special key for "handler at this path" *)
let root =
  Children
    (StringMap.empty
     |> StringMap.add
          "api"
          (Children
             (StringMap.empty
              |> StringMap.add
                   "profile"
                   (Exec (VerbMap.empty |> VerbMap.add GET show_profile))
              |> StringMap.add
                   "users"
                   (Children
                      (StringMap.empty
                       |> StringMap.add
                            ""
                            (Exec (VerbMap.empty |> VerbMap.add GET list_users))
                          (* GET /api/users *)
                       |> StringMap.add
                            "?"
                            (Exec (VerbMap.empty |> VerbMap.add GET get_user))
                          (* GET /api/users/:id *)))
              |> StringMap.add
                   "posts"
                   (Children
                      (StringMap.empty
                       |> StringMap.add
                            ""
                            (Exec
                               (VerbMap.empty
                                |> VerbMap.add GET list_posts
                                |> VerbMap.add POST create_post
                                |> VerbMap.add DELETE delete_all_posts))
                       |> StringMap.add
                            "?"
                            (Exec
                               (VerbMap.empty
                                |> VerbMap.add GET get_post
                                |> VerbMap.add PUT (update_post "replace all")
                                |> VerbMap.add PATCH (update_post "replace part")
                                |> VerbMap.add DELETE delete_post)))))))
;;

(* Console IO - real I/O implementation *)
module ConsoleIO : IO = struct
  let output s = print_endline s

  let input prompt =
    ()
    ; Printf.printf "%s" prompt
    ; flush stdout
    ; try Some (String.trim @@ read_line ()) with
      | End_of_file -> None
  ;;
end

(* Test IO - captures output and uses predefined input *)
module TestIO = struct
  let output_buffer = ref []
  let input_buffer = ref []
  let output s = output_buffer := s :: !output_buffer

  let input prompt =
    ()
    ; output_buffer := prompt :: !output_buffer
    ; match !input_buffer with
      | [] -> None
      | h :: t ->
        ()
        ; input_buffer := t
        ; Some h
  ;;

  let get_output () = List.rev !output_buffer

  let set_input inputs =
    ()
    ; output_buffer := []
    ; input_buffer := inputs
  ;;
end

(* REPL function parameterized by IO module *)
let run_repl (module IO : IO) =
  let rec repl () =
    match IO.input "> " with
    | None | Some "quit" -> IO.output "\nGoodbye!"
    | Some "" -> repl ()
    | Some input ->
      let parts = String.split_on_char ' ' input in
      begin
        match parts with
        | [ method_name; path ] ->
          ()
          ; IO.output (Printf.sprintf "Dispatching: %s %s" method_name path)
          ; dispatch (module IO) root method_name path
          ; IO.output ""
          ; repl ()
        | _ ->
          ()
          ; IO.output "Usage: <METHOD> <PATH>"
          ; repl ()
      end
  in
  ()
  ; IO.output "HTTP Router CLI"
  ; IO.output "Enter: <METHOD> <PATH> (e.g., 'GET /api/profile')"
  ; IO.output "Type 'quit' to exit\n"
  ; repl ()
;;

(* Test utilities *)
let test_dispatch_sequence commands =
  ()
  ; TestIO.set_input commands
  ; run_repl (module TestIO)
  ; TestIO.get_output ()
;;

(* Example test - validates the exact scenario from the user *)
let test_posts_scenario () =
  let commands =
    [ "GET /api/posts"
    ; "GET /api/posts/"
    ; "GET /api/posts/456"
    ; "PUT /api/posts/123"
    ; "PATCH /api/posts/234"
    ; "DELETE /api/posts/789"
    ]
  in
  let output = test_dispatch_sequence commands in
  let expected_output =
    [ "HTTP Router CLI"
    ; "Enter: <METHOD> <PATH> (e.g., 'GET /api/profile')"
    ; "Type 'quit' to exit\n"
    ; "> "
    ; "Dispatching: GET /api/posts"
    ; "list all posts"
    ; ""
    ; "> "
    ; "Dispatching: GET /api/posts/"
    ; "list all posts"
    ; ""
    ; "> "
    ; "Dispatching: GET /api/posts/456"
    ; "get post by id"
    ; ""
    ; "> "
    ; "Dispatching: PUT /api/posts/123"
    ; "update a post: replace all"
    ; ""
    ; "> "
    ; "Dispatching: PATCH /api/posts/234"
    ; "update a post: replace part"
    ; ""
    ; "> "
    ; "Dispatching: DELETE /api/posts/789"
    ; "delete a post"
    ; ""
    ; "> "
    ; "\nGoodbye!"
    ]
  in
  if output = expected_output then
    print_endline "✓ Test passed!"
  else (
    ()
    ; print_endline "✗ Test failed!"
    ; print_endline "\nLine-by-line comparison:"
    ; let rec compare_lines i exp_list got_list =
        match (exp_list, got_list) with
        | ([], []) -> ()
        | (exp :: exp_rest, got :: got_rest) ->
          if exp = got then
            Printf.printf "  %2d: ✓ '%s'\n" i exp
          else (
            ()
            ; Printf.printf "  %2d: ✗ Expected: '%s'\n" i exp
            ; Printf.printf "  %2s:        Got: '%s'\n" "" got
          )
          ; compare_lines (i + 1) exp_rest got_rest
        | (exp :: exp_rest, []) ->
          ()
          ; Printf.printf "  %2d: ✗ Expected: '%s'\n" i exp
          ; Printf.printf "  %2s:        Got: <missing>\n" ""
          ; compare_lines (i + 1) exp_rest []
        | ([], got :: got_rest) ->
          ()
          ; Printf.printf "  %2d: ✗ Expected: <missing>\n" i
          ; Printf.printf "  %2s:        Got: '%s'\n" "" got
          ; compare_lines (i + 1) [] got_rest
      in
      compare_lines 1 expected_output output
  )
;;

let () =
  ()
  ; if Sys.getenv_opt "TEST" = Some "1" then
      test_posts_scenario ()
    else
      run_repl (module ConsoleIO)
;;
