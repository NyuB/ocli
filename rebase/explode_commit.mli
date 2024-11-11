type t

val init_nothing : t
val init_all : string list -> t
val nothing_exploded : t -> bool
val is_exploded : t -> string -> bool

(** [kept_exploded all_modified t] returns two sets:
    - the first set is the modified files that are to be kept in the original commit
    - the second set is the modifed files that are to be commited separately *)
val kept_exploded : string list -> t -> string list * string list

(** [toggle exploded file] removes (if present) or add (if absent) [file] from/to the [exploded] set *)
val toggle : t -> string -> t

(** [toggle_i all exploded index] is the same as [toggle exploded file] where file is the file at [index] in [all].

    Return [exploded] as is if [index] is not a valid index within [all]*)
val toggle_i : string list -> t -> int -> t

val exploded_list : t -> string list
