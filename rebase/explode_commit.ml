module StringSet = Set.Make (String)

type t = StringSet.t

let init_nothing : t = StringSet.empty
let init_all l : t = StringSet.of_list l
let nothing_exploded (exploded : t) : bool = StringSet.is_empty exploded
let is_exploded (exploded : t) (file : string) : bool = StringSet.mem file exploded

let kept_exploded all (exploded : t) =
  let kept = StringSet.diff (StringSet.of_list all) exploded in
  StringSet.to_list kept, StringSet.to_list exploded
;;

let toggle (exploded : t) (file : string) : t =
  if StringSet.mem file exploded
  then StringSet.remove file exploded
  else StringSet.add file exploded
;;

let toggle_i all (exploded : t) (i : int) : t =
  match List.nth_opt all i with
  | None -> exploded
  | Some file -> toggle exploded file
;;

let exploded_list t = StringSet.to_list t
