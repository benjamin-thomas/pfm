(*{|

Parameterized Route Dispatcher - HTTP-agnostic trie-based routing
Inspired by the TypeScript httpDispatch implementation

Usage:
  ocaml trie3.ml
  TEST=1 ocaml trie3.ml

echo -e "demo\nGET /health\nGET /greet/Alice\nGET /double/21\nGET /add/15/27\nGET /events/2024-12-25\nGET /resource/550e8400-e29b-41d4-a716-446655440000\nGET /items/10:20\nGET /between/2024-01-01/2024-12-31\nquit" | ocaml trie3.ml

|}*)

(* ======================================================================
   Core Types and Decoders
   ====================================================================== *)

module Core = struct
  (* Type aliases for clarity *)
  type http_method = string
  type route_pattern = string
  type url_path = string

  (* Decoder function - converts string to typed value with validation *)
  type 'a decoder = string -> 'a option

  (* Handler functions - pure functions for testing *)
  type p0_handler = unit -> unit
  type 'a p1_handler = 'a -> unit
  type ('a, 'b) p2_handler = 'a -> 'b -> unit

  (* Route types for different parameter counts *)
  type route_action =
    | P0_Action of p0_handler
    | P1_Action : 'a decoder * 'a p1_handler -> route_action
    | P2_Action : 'a decoder * 'b decoder * ('a, 'b) p2_handler -> route_action
end

(* ======================================================================
   IO Module Type and Test IO Module
   ====================================================================== *)

(* IO Module Type - abstraction for input/output operations *)
module type IO = sig
  val output : string -> unit
  val input : string -> string option (* prompt -> input option *)
end

module TestIO = struct
  let output_buffer = ref []
  let input_buffer = ref []
  let output s = output_buffer := s :: !output_buffer

  let input prompt =
    output_buffer := prompt :: !output_buffer
    ; match !input_buffer with
      | [] -> None
      | h :: t ->
        input_buffer := t
        ; Some h
  ;;

  let get_output () = List.rev !output_buffer

  let set_input inputs =
    output_buffer := []
    ; input_buffer := inputs
  ;;

  let clear () = output_buffer := []
end

(* Create IO module from TestIO *)
module TestIOModule : IO = TestIO

(* Console IO - real I/O implementation *)
module ConsoleIO : IO = struct
  let output s = print_endline s

  let input prompt =
    Printf.printf "%s" prompt
    ; flush stdout
    ; try Some (String.trim @@ read_line ()) with
      | End_of_file -> None
  ;;
end

(* ======================================================================
   Primitive Decoders and Combinators
   ====================================================================== *)

module Decoders = struct
  open Core

  (* Basic primitive decoders *)
  let int : int decoder =
    fun s ->
    try Some (int_of_string s) with
    | _ -> None
  ;;

  let string : string decoder = fun s -> Some s

  let float : float decoder =
    fun s ->
    try Some (float_of_string s) with
    | _ -> None
  ;;

  let bool : bool decoder =
    fun s ->
    match String.lowercase_ascii s with
    | "true" | "1" | "yes" -> Some true
    | "false" | "0" | "no" -> Some false
    | _ -> None
  ;;

  (* Decoder combinators *)

  (* map: Transform successful decode result *)
  let map (f : 'a -> 'b) (decoder : 'a decoder) : 'b decoder =
    fun s -> Option.map f (decoder s)
  ;;

  (* bind: Chain decoders that can fail *)
  let bind (decoder : 'a decoder) (f : 'a -> 'b option) : 'b decoder =
    fun s ->
    match decoder s with
    | Some x -> f x
    | None -> None
  ;;

  (* or_else: Try alternative decoder if first fails *)
  let or_else (decoder1 : 'a decoder) (decoder2 : 'a decoder) : 'a decoder =
    fun s ->
    match decoder1 s with
    | None -> decoder2 s
    | some -> some
  ;;
end

(* ======================================================================
   User-Defined Custom Decoders (examples)
   ====================================================================== *)

module CustomDecoders = struct
  open Core
  open Decoders

  (* UUID decoder with validation *)
  type uuid = UUID of string

  let uuid : uuid decoder =
    fun s ->
    if
      String.length s = 36
      && String.get s 8 = '-'
      && String.get s 13 = '-'
      && String.get s 18 = '-'
      && String.get s 23 = '-'
    then
      Some (UUID s)
    else
      None
  ;;

  (* Date decoder - parses YYYY-MM-DD format *)
  type date =
    { year : int
    ; month : int
    ; day : int
    }

  let date : date decoder =
    fun s ->
    try
      Scanf.sscanf s "%d-%d-%d" (fun y m d ->
        if m >= 1 && m <= 12 && d >= 1 && d <= 31 then
          Some { year = y; month = m; day = d }
        else
          None)
    with
    | _ -> None
  ;;

  (* Range decoder - parses "from:to" format *)
  type range =
    { from : int
    ; until : int
    }

  let range : range decoder =
    fun s ->
    match String.split_on_char ':' s with
    | [ f; t ] ->
      (match (int f, int t) with
       | (Some from, Some until) when from <= until -> Some { from; until }
       | _ -> None)
    | _ -> None
  ;;

  (* Slug decoder - alphanumeric with dashes *)
  type slug = Slug of string

  let slug : slug decoder =
    fun s ->
    if
      String.length s > 0
      && String.for_all
           (fun c -> Char.((c >= 'a' && c <= 'z') || (c >= '0' && c <= '9') || c = '-'))
           s
    then
      Some (Slug s)
    else
      None
  ;;
end

(* ======================================================================
   Trie-based Route Dispatcher
   ====================================================================== *)

module RouteDispatcher = struct
  open Core

  (* Trie node structure - allows both children and handlers like trie2.ml *)
  type trie_node =
    | Children of (string, trie_node) Hashtbl.t
    | Exec of (http_method, route_action) Hashtbl.t

  (* Main dispatcher type *)
  type t = { mutable root : trie_node }

  (* Create new dispatcher *)
  let create () = { root = Children (Hashtbl.create 16) }

  (* Split path into segments *)
  let split_path path = String.split_on_char '/' path |> List.filter (fun s -> s <> "")

  (* Add a route to the trie *)
  let add_route dispatcher method_name pattern action =
    let segments = split_path pattern in
    let rec insert_node node = function
      | [] ->
        (* At the end of path - store handler at empty string key *)
        (match node with
         | Exec handlers -> Hashtbl.replace handlers method_name action
         | Children children ->
           (* Store handler in empty string key within children node *)
           let handlers =
             match Hashtbl.find_opt children "" with
             | Some (Exec h) -> h
             | _ ->
               let h = Hashtbl.create 4 in
               Hashtbl.replace children "" (Exec h); h
           in
           Hashtbl.replace handlers method_name action)
      | seg :: rest ->
        (match node with
         | Exec _ -> failwith "Cannot add child to exec node"
         | Children children ->
           let next_node =
             match Hashtbl.find_opt children seg with
             | Some n -> n
             | None ->
               (* Always create Children nodes to allow both terminal and continuation *)
               let new_node = Children (Hashtbl.create 4) in
               Hashtbl.add children seg new_node; new_node
           in
           insert_node next_node rest)
    in
    insert_node dispatcher.root segments
  ;;

  (* Register routes with different parameter counts *)
  let match_p0 dispatcher method_name pattern handler =
    add_route dispatcher method_name pattern (P0_Action handler)
  ;;

  let match_p1 dispatcher method_name pattern decoder handler =
    add_route dispatcher method_name pattern (P1_Action (decoder, handler))
  ;;

  let match_p2 dispatcher method_name pattern decoder1 decoder2 handler =
    add_route dispatcher method_name pattern (P2_Action (decoder1, decoder2, handler))
  ;;

  (* Execute a route action with captured parameters *)
  let execute_action (module IO : IO) action params =
    match (action, params) with
    | (P0_Action handler, []) -> handler ()
    | (P1_Action (decoder, handler), [ p1 ]) ->
      (match decoder p1 with
       | Some v -> handler v
       | None -> IO.output (Printf.sprintf "Decoding error: invalid parameter '%s'" p1))
    | (P2_Action (dec1, dec2, handler), [ p1; p2 ]) ->
      (match (dec1 p1, dec2 p2) with
       | (Some v1, Some v2) -> handler v1 v2
       | _ ->
         IO.output (Printf.sprintf "Decoding error: invalid parameters '%s', '%s'" p1 p2))
    | _ -> IO.output "Invalid route configuration"
  ;;

  (* Dispatch a request *)
  let dispatch (module IO : IO) dispatcher method_name path =
    let segments = split_path path in
    let captured_params = ref [] in
    let rec find_handler node segments_remaining =
      match (node, segments_remaining) with
      | (Exec handlers, []) ->
        (* Exact match at exec node *)
        (match Hashtbl.find_opt handlers method_name with
         | Some action -> Some (action, List.rev !captured_params)
         | None -> None)
      | (Children children, []) ->
        (* Check for terminal handler at empty string key *)
        (match Hashtbl.find_opt children "" with
         | Some (Exec handlers) ->
           (match Hashtbl.find_opt handlers method_name with
            | Some action -> Some (action, List.rev !captured_params)
            | None -> None)
         | _ -> None)
      | (Children children, seg :: rest) ->
        (* Try exact match first *)
        (match Hashtbl.find_opt children seg with
         | Some next -> find_handler next rest
         | None ->
           (* Try wildcard match *)
           (match Hashtbl.find_opt children "?" with
            | Some next ->
              captured_params := seg :: !captured_params
              ; find_handler next rest
            | None -> None))
      | _ -> None
    in
    match find_handler dispatcher.root segments with
    | Some (action, params) -> execute_action (module IO) action params
    | None -> IO.output (Printf.sprintf "No route found for %s %s" method_name path)
  ;;
end

(* ======================================================================
   Testing Module
   ====================================================================== *)

module Testing = struct
  open Core

  (* Module aliases for qualified access *)
  module D = Decoders
  module CD = CustomDecoders
  module RD = RouteDispatcher

  (* ANSI color codes *)
  let green s = "\027[32m" ^ s ^ "\027[0m"
  let red s = "\027[31m" ^ s ^ "\027[0m"
  let yellow s = "\027[33m" ^ s ^ "\027[0m"
  let cyan s = "\027[36m" ^ s ^ "\027[0m"

  (* Test results accumulator *)
  let test_results = ref []

  (* Simple test runner *)
  let test name f =
    try
      f ()
      ; test_results := (name, true) :: !test_results
      ; Printf.printf "%s %s\n" (green "✓") name
    with
    | e ->
      test_results := (name, false) :: !test_results
      ; Printf.printf "%s %s: %s\n" (red "✗") name (Printexc.to_string e)
  ;;

  (* Run all tests *)
  let run_tests () =
    ()
    ; Printf.printf "\n=== Running Route Dispatcher Tests ===\n\n"
    ; (* Test P0 routes (no parameters) *)
      test "P0: exact route match" (fun () ->
        let dispatcher = RD.create () in
        ()
        ; TestIO.clear ()
        ; RD.match_p0 dispatcher "GET" "/health" (fun () -> TestIO.output "Health OK")
        ; RD.dispatch (module TestIOModule) dispatcher "GET" "/health"
        ; let output = TestIO.get_output () in
          assert (List.mem "Health OK" output))
    ; test "P0: method mismatch" (fun () ->
        let dispatcher = RD.create () in
        ()
        ; TestIO.clear ()
        ; RD.match_p0 dispatcher "GET" "/health" (fun () -> TestIO.output "Health OK")
        ; RD.dispatch (module TestIOModule) dispatcher "POST" "/health"
        ; let output = TestIO.get_output () in
          assert (List.mem "No route found for POST /health" output))
    ; (* Test P1 routes (one parameter) *)
      test "P1: integer parameter" (fun () ->
        let dispatcher = RD.create () in
        ()
        ; TestIO.clear ()
        ; RD.match_p1 dispatcher "GET" "/inc/?" D.int (fun n ->
            TestIO.output (Printf.sprintf "Result: %d" (n + 1)))
        ; RD.dispatch (module TestIOModule) dispatcher "GET" "/inc/123"
        ; let output = TestIO.get_output () in
          assert (List.mem "Result: 124" output))
    ; test "P1: string parameter" (fun () ->
        let dispatcher = RD.create () in
        ()
        ; TestIO.clear ()
        ; RD.match_p1 dispatcher "GET" "/hello/?" D.string (fun s ->
            TestIO.output ("Hello, " ^ s))
        ; RD.dispatch (module TestIOModule) dispatcher "GET" "/hello/world"
        ; let output = TestIO.get_output () in
          assert (List.mem "Hello, world" output))
    ; test "P1: invalid integer parameter" (fun () ->
        let dispatcher = RD.create () in
        ()
        ; TestIO.clear ()
        ; RD.match_p1 dispatcher "GET" "/api/?" D.int (fun n ->
            TestIO.output (Printf.sprintf "Number: %d" n))
        ; RD.dispatch (module TestIOModule) dispatcher "GET" "/api/invalid"
        ; let output = TestIO.get_output () in
          assert (List.mem "Decoding error: invalid parameter 'invalid'" output))
    ; (* Test P2 routes (two parameters) *)
      test "P2: two integer parameters" (fun () ->
        let dispatcher = RD.create () in
        ()
        ; TestIO.clear ()
        ; RD.match_p2 dispatcher "GET" "/add/?/?" D.int D.int (fun a b ->
            TestIO.output (Printf.sprintf "Sum: %d" (a + b)))
        ; RD.dispatch (module TestIOModule) dispatcher "GET" "/add/15/27"
        ; let output = TestIO.get_output () in
          assert (List.mem "Sum: 42" output))
    ; test "P2: mixed parameter types" (fun () ->
        let dispatcher = RD.create () in
        ()
        ; TestIO.clear ()
        ; RD.match_p2 dispatcher "GET" "/greet/?/?" D.string D.int (fun name times ->
            let result = String.concat ", " (List.init times (fun _ -> name)) in
            TestIO.output result)
        ; RD.dispatch (module TestIOModule) dispatcher "GET" "/greet/Hi/3"
        ; let output = TestIO.get_output () in
          assert (List.mem "Hi, Hi, Hi" output))
    ; (* Test custom decoders *)
      test "Custom decoder: UUID" (fun () ->
        let dispatcher = RD.create () in
        ()
        ; TestIO.clear ()
        ; RD.match_p1 dispatcher "GET" "/resource/?" CD.uuid (fun (CD.UUID id) ->
            TestIO.output (Printf.sprintf "Resource ID: %s" id))
        ; RD.dispatch
            (module TestIOModule)
            dispatcher
            "GET"
            "/resource/550e8400-e29b-41d4-a716-446655440000"
        ; let output = TestIO.get_output () in
          assert (List.mem "Resource ID: 550e8400-e29b-41d4-a716-446655440000" output))
    ; test "Custom decoder: date" (fun () ->
        let dispatcher = RD.create () in
        ()
        ; TestIO.clear ()
        ; RD.match_p1 dispatcher "GET" "/events/?" CD.date (fun { year; month; day } ->
            TestIO.output (Printf.sprintf "Date: %04d-%02d-%02d" year month day))
        ; RD.dispatch (module TestIOModule) dispatcher "GET" "/events/2024-03-15"
        ; let output = TestIO.get_output () in
          assert (List.mem "Date: 2024-03-15" output))
    ; test "Custom decoder: range" (fun () ->
        let dispatcher = RD.create () in
        ()
        ; TestIO.clear ()
        ; RD.match_p1 dispatcher "GET" "/items/?" CD.range (fun { from; until } ->
            TestIO.output (Printf.sprintf "Range: %d to %d" from until))
        ; RD.dispatch (module TestIOModule) dispatcher "GET" "/items/10:20"
        ; let output = TestIO.get_output () in
          assert (List.mem "Range: 10 to 20" output))
    ; (* Test route priority *)
      test "Route priority: exact before wildcard" (fun () ->
        let dispatcher = RD.create () in
        ()
        ; TestIO.clear ()
        ; RD.match_p0 dispatcher "GET" "/api/health" (fun () ->
            TestIO.output "Exact route called")
        ; RD.match_p1 dispatcher "GET" "/api/?" D.string (fun _ ->
            TestIO.output "Wildcard route called")
        ; RD.dispatch (module TestIOModule) dispatcher "GET" "/api/health"
        ; let output = TestIO.get_output () in
          assert (List.mem "Exact route called" output)
          ; assert (not (List.mem "Wildcard route called" output)))
    ; test
        "Aggregate routes"
        begin
          fun () ->
            let dispatcher = RD.create () in
            ()
            ; TestIO.clear ()
            ; RD.match_p0 dispatcher "GET" "/api/users" (fun () ->
                TestIO.output "Listing users")
            ; RD.match_p1 dispatcher "GET" "/api/users/?" D.int (fun n ->
                TestIO.output (Printf.sprintf "Exact route called with %d" n))
            ; RD.dispatch (module TestIOModule) dispatcher "GET" "/api/users"
            ; RD.dispatch (module TestIOModule) dispatcher "GET" "/api/users/123"
            ; let output = TestIO.get_output () in
              assert (output = [ "Listing users"; "Exact route called with 123" ])
        end
    ; Printf.printf "\n%s\n" (cyan "=== Test Summary ===")
    ; let passed = List.filter (fun (_, r) -> r) !test_results |> List.length in
      let total = List.length !test_results in
      ()
      ; Printf.printf
          "Passed: %s/%d\n"
          (if passed = total then
             green (string_of_int passed)
           else
             yellow (string_of_int passed))
          total
      ; if passed = total then
          Printf.printf "%s All tests passed!\n" (green "✓")
        else
          Printf.printf "%s Some tests failed\n" (red "✗")
  ;;
end

(* ======================================================================
   REPL for Interactive Testing
   ====================================================================== *)

module Repl = struct
  open Core

  (* Module aliases for qualified access *)
  module D = Decoders
  module CD = CustomDecoders
  module RD = RouteDispatcher

  let dispatcher = RD.create ()

  (* Sample handlers for demo *)
  let setup_demo_routes () =
    (* P0 routes *)
    ()
    ; RD.match_p0 dispatcher "GET" "/health" (fun () -> print_endline "Health check: OK")
    ; RD.match_p0 dispatcher "POST" "/health" (fun () -> print_endline "Health check: OK")
    ; RD.match_p0 dispatcher "GET" "/health/123" (fun () ->
        print_endline "Health check for 123: OK")
    ; RD.match_p0 dispatcher "GET" "/api/info" (fun () -> print_endline "API version 1.0")
    ; (* P1 routes with basic decoders *)
      RD.match_p1 dispatcher "GET" "/greet/?" D.string (fun name ->
        Printf.printf "Hello, %s!\n" name)
    ; RD.match_p1 dispatcher "GET" "/double/?" D.int (fun n ->
        Printf.printf "%d * 2 = %d\n" n (n * 2))
    ; (* P1 routes with custom decoders *)
      RD.match_p1 dispatcher "GET" "/resource/?" CD.uuid (fun (CD.UUID id) ->
        Printf.printf "Fetching resource: %s\n" id)
    ; RD.match_p1 dispatcher "GET" "/events/?" CD.date (fun { year; month; day } ->
        Printf.printf "Events for date: %04d-%02d-%02d\n" year month day)
    ; RD.match_p1 dispatcher "GET" "/items/?" CD.range (fun r ->
        Printf.printf "Items from %d to %d\n" r.from r.until)
    ; RD.match_p1 dispatcher "GET" "/blog/?" CD.slug (fun (CD.Slug s) ->
        Printf.printf "Loading blog post: %s\n" s)
    ; (* P2 routes *)
      RD.match_p2 dispatcher "GET" "/add/?/?" D.int D.int (fun a b ->
        Printf.printf "%d + %d = %d\n" a b (a + b))
    ; RD.match_p2 dispatcher "GET" "/between/?/?" CD.date CD.date (fun d1 d2 ->
        Printf.printf
          "Date range: %04d-%02d-%02d to %04d-%02d-%02d\n"
          d1.year
          d1.month
          d1.day
          d2.year
          d2.month
          d2.day)
    ; print_endline "Demo routes loaded!"
  ;;

  let run_repl () =
    ()
    ; print_endline "\n=== Route Dispatcher REPL ==="
    ; print_endline "Commands:"
    ; print_endline "  <METHOD> <PATH>  - Dispatch a route (e.g., 'GET /health')"
    ; print_endline "  demo             - Load demo routes"
    ; print_endline "  quit             - Exit"
    ; print_endline ""
    ; let rec loop () =
        print_string "> "
        ; flush stdout
        ; match read_line () with
          | "quit" | "exit" -> print_endline "Goodbye!"
          | "demo" ->
            setup_demo_routes ()
            ; loop ()
          | "" -> loop ()
          | input ->
            let parts = String.split_on_char ' ' input in
            begin
              match parts with
              | [ method_name; path ] ->
                RD.dispatch (module ConsoleIO) dispatcher method_name path
                ; print_endline ""
              | _ -> print_endline "Usage: <METHOD> <PATH>"
            end
            ; loop ()
      in
      loop ()
  ;;
end

(* ======================================================================
   Main Entry Point
   ====================================================================== *)

let () =
  if Sys.getenv_opt "TEST" = Some "1" then
    Testing.run_tests ()
  else
    Repl.run_repl ()
;;
