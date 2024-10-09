module List : sig
  include module type of List

  (** [not_none l] is the list of non-none elements of [l] *)
  val not_none : 'a option list -> 'a list

  (** [sublist from l] is the sub-list of [l] starting at index [from]. If [from >= List.length l] then result is empty. If [from <= 0] then result is [l] *)
  val sublist : int -> 'a list -> 'a list

  (** [at_most n l] is the [n] first elements of [l]. If [l] has less than [n] elements, the result is [l]. If [n <= 0] result is the empty list *)
  val at_most : int -> 'a list -> 'a list
end = struct
  include List

  let not_none l = List.filter_map Fun.id l

  let rec sublist (from : int) (l : 'a list) : 'a list =
    match from, l with
    | over, res when over <= 0 -> res
    | _, [] -> []
    | pos, _ :: t -> sublist (pos - 1) t
  ;;

  let at_most n l =
    let rec aux acc n = function
      | [] -> List.rev acc
      | h :: t -> if n > 0 then aux (h :: acc) (n - 1) t else List.rev acc
    in
    aux [] n l
  ;;
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

let first (a, _) = a

let ( |?: ) opt lazy_default =
  match opt with
  | None -> Lazy.force lazy_default
  | Some v -> v
;;

let ( |? ) f opt = Option.map opt f
let ( |?* ) f opt = Option.bind f opt
