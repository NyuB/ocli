(** {{:https://guide.elm-lang.org/architecture/} The Elm Architecture} adapted for OCaml apps *)

(** Elm-like application
    - [model] represents the application state, [init] being the initial state
    - [view] is the 'graphical' projection of this model
    - [update] is the pure function computing the updated version of a model in response to a message (here terminal [events])
      The actual tasks of rendering the view and implementing the event loop is left to the platform (see [loop_app])
      In this architecture, no side-effects should be performed directly by this application. Instead, they should be handled by the platofrm, and the application can only require them via [command]s return along application [state]. The only way for the platform to respond to these commands is then via the [event]s. *)
module type App = sig
  (** Types of 'impure' commands (such as I/O or other side-effects) that this application may require the platform to handle *)
  type command

  (** Events that can be handled by this application to update it's [model] *)
  type event

  (** Application state *)
  type model

  (** Graphical representation type of the application *)
  type view

  (** Initial state of the application *)
  val init : model

  (** Projection of the application state into it's graphical representation *)
  val view : model -> view

  (** State update in response to an event. Returns the new state of the application along commands to be handled by the platform *)
  val update : model -> event -> model * command list
end

(** Represents the actual, potentially impure, engine used to render an application and implement the events poling and distribution *)
module type Platform = sig
  (** 'Impure' commands (such as I/O or other side-effects) that this platform can handle *)
  type command

  (** Events this platform can gather from the outside world or by executing application [command]s *)
  type event

  (** Graphical components this platform can render *)
  type view

  (** /!\ MAYBE A GLOBAL OPERATION /!\ Initialize the platform *)
  val setup : unit -> unit

  (** Display the graphical components *)
  val render : view -> unit

  (** Gather events from the outside world, e.g. user inputs or clock ticks *)
  val poll_events : unit -> event list

  (** Execute [command]s requested by an application, potentially adding more events to the next [poll_events] call *)
  val handle_commands : command list -> unit
end

(** Alias for applications that do not use [App.command]s *)
type no_command = |

(** [ loop_app (module A) (module P) terminal out ] calls [P.setup ()] and then loops indefinitely over the sequence:
    + Compute the current view of the current [model: A.model] value (starting with [A.init])
    + Render the view on [out] using [P.render]
    + Read [events] from [P.poll_events]
    + Compute a new [model, commands] pair from the current model, feeding events to [A.update]
    + Handle [commands] using [P.handle_commands] *)
val loop_app
  :  (module App with type command = 'command and type event = 'event and type view = 'view)
  -> (module Platform
        with type command = 'command
         and type event = 'event
         and type view = 'view)
  -> unit
