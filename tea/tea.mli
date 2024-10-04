(** Elm-like application :
    - [model] represents the application state, [init] being the initial state
    - [view] is the graphical projection of this model, here as strings positioned on a window
    - [update] is the pure function computing the updated version of a model in response to a message (here terminal [events])
      The actual tasks of rendering the view and implementing the event loop is left to the platform (see [loop_app]) *)
module type App = sig
  type command
  type event
  type model
  type view

  val init : model
  val view : model -> view
  val update : model -> event -> model * command list
end

(** Represents the actual, potentially impure, engine used to render an application and implement the events poling and distribution *)
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

(** [ loop_app (module A) (module P) terminal out ] calls [P.setup ()] and then loops indefinitely over the sequence:
    + Compute the current view of the current [model: A.model] value (starting with [A.init])
    + Render the view on [out] using [P.render]
    + Read [events] from [P.poll_events]
    + Compute the new model from the current one, feeding events to [A.update] *)
val loop_app
  :  (module App with type command = 'command and type event = 'event and type view = 'view)
  -> (module Platform
        with type command = 'command
         and type event = 'event
         and type view = 'view)
  -> unit
