module type App = sig
  type event
  type model
  type view

  val init : model
  val view : model -> view
  val update : model -> event -> model
end

module type Platform = sig
  type event
  type view

  val setup : unit -> unit
  val render : view -> unit
  val poll_events : unit -> event list
end

let loop_app
  (type event view)
  (module A : App with type event = event and type view = view)
  (module P : Platform with type event = event and type view = view)
  =
  let () = P.setup () in
  let rec loop model =
    P.render (A.view model);
    let events = P.poll_events () in
    let updated = List.fold_left A.update model events in
    loop updated
  in
  loop A.init
;;
