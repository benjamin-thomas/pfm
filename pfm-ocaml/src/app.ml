module Cmd = Tea.Cmd
module H = Tea.Html
module HE = Tea.Html.Events
module HA = Tea.Html.Attributes
module Sub = Tea.Sub
module Nav = Tea.Navigation
module Debug = Tea.Debug
module Task = Tea.Task
module Time = Tea.Time

type model =
  { counter : int
  ; time : Time.t
  }

type msg =
  | GotTime of Time.t
  | Inc
  | Dec
  | Reset
  | LocationChanged of Tea_navigation.Location.t
  | Set of int
[@@deriving accessors]

let msg_to_string (msg : msg) =
  match msg with
  | GotTime _ -> "Got time"
  | Inc -> "Inc"
  | Dec -> "Dec"
  | Reset -> "Reset"
  | LocationChanged _location -> "Location changed"
  | Set i -> "Set to " ^ string_of_int i
;;

let update (model : model) = function
  | Inc -> ({ model with counter = model.counter + 1 }, Cmd.none)
  | Dec -> ({ model with counter = model.counter - 1 }, Cmd.none)
  | Reset -> ({ model with counter = 0 }, Cmd.none)
  | Set v -> ({ model with counter = v }, Cmd.none)
  | LocationChanged _location -> (model, Cmd.none)
  | GotTime time -> ({ model with time }, Cmd.none)
;;

let init () _location =
  ( { counter = 0; time = 0.0 }
  , Cmd.batch
      [ Cmd.none; Task.perform (fun x -> GotTime x) (Task.succeed @@ Js.Date.now ()) ] )
;;

let btn button_text msg = H.button [ HE.onClick msg ] [ H.text button_text ]

let view (model : model) =
  H.div
    []
    [ H.div [] [ H.text "Hello how are you!" ]
    ; H.div [] [ H.text @@ string_of_float model.time ]
    ; H.br []
    ; btn "-" Dec
    ; H.span
        [ HA.style "text-weight" "bold"; HA.style "margin" "0 10px" ]
        [ H.text (string_of_int model.counter) ]
    ; btn "+" Inc
    ; H.br []
    ; H.br []
    ; btn "Set to 42" (Set 42)
    ; begin
        if model.counter <> 0 then
          btn "Reset" Reset
        else
          H.noNode
      end
    ]
;;

let subscriptions _model = Sub.none
let shutdown _model = Cmd.none

let start_app container =
  Nav.navigationProgram
    locationChanged
    { init; update; view; subscriptions; shutdown }
    container
    ()
;;

let start_debug_app ?(init = init) ?(shutdown = shutdown) container =
  Debug.navigationProgram
    locationChanged
    { init; update; view; subscriptions; shutdown }
    msg_to_string
    container
    ()
;;

let start_hot_debug_app container cachedModel =
  (* Replace the existing shutdown function with one that returns the current
   * state of the app, for hot module replacement purposes *)
  (* inspired by https://github.com/walfie/ac-tune-maker *)
  let modelRef = ref None in
  let shutdown model =
    modelRef := Some model;
    Cmd.none
  in
  let init =
    match cachedModel with
    | None -> init
    | Some model ->
      fun flags location ->
        let (_model, cmd) = init flags location in
        (model, cmd)
  in
  let app = start_debug_app ~init ~shutdown container in
  let oldShutdown = app##shutdown in
  let newShutdown () =
    oldShutdown ();
    !modelRef
  in
  let _ = Js.Obj.assign app [%obj { shutdown = newShutdown }] in
  newShutdown
;;
