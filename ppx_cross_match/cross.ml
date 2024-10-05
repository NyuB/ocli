(** [cross left right] is a list with one entry for each element of [right] corresponding to this element appended to [left] *)
let cross (left : 'a list) (right : 'a list) : 'a list list =
  let left_rev = List.rev left in
  List.map (fun b -> List.rev (b :: left_rev)) right
;;
