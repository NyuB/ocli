module List : sig
  include module type of List

  val not_none : 'a option list -> 'a list
end = struct
  include List

  let not_none l = List.filter_map Fun.id l
end

module Out_channel : sig
  include module type of Out_channel

  val output_line : t -> string -> unit
end = struct
  include Out_channel

  let output_line o s =
    output_string o s;
    output_char o '\n'
  ;;
end

let ( |?: ) opt lazy_default =
  match opt with
  | None -> Lazy.force lazy_default
  | Some v -> v
;;

let ( |? ) f opt = Option.map opt f
let ( |?* ) f opt = Option.bind f opt
