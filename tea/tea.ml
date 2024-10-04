module type App = sig
  type command
  type event
  type model
  type view

  val init : model
  val view : model -> view
  val update : model -> event -> model * command list
end

module type Platform = sig
  type command
  type event
  type view

  val setup : unit -> unit
  val render : view -> unit
  val poll_events : unit -> event list
  val handle_commands : command list -> unit
end

type no_command = |

let fold_events
  (type command event model)
  (module A : App
    with type command = command
     and type event = event
     and type model = model)
  (model : model)
  (events : event list)
  =
  List.fold_left
    (fun (model, commands) event ->
      let next_model, additional_commands = A.update model event in
      next_model, List.rev_append additional_commands commands)
    (model, [])
    events
  |> fun (model, commands) -> model, List.rev commands
;;

let loop_app
  (type command event view)
  (module A : App with type command = command and type event = event and type view = view)
  (module P : Platform
    with type command = command
     and type event = event
     and type view = view)
  =
  let () = P.setup () in
  let rec loop model =
    P.render (A.view model);
    let events = P.poll_events () in
    let updated, commands = fold_events (module A) model events in
    P.handle_commands commands;
    loop updated
  in
  loop A.init
;;
