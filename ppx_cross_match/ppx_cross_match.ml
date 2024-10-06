(** Ppx rewriter to expand tuple of patterns to a pattern matching all combinations of these patterns
    E.g: ( (A,B), (1,2) ) would generate (A, 1) | (A, 2) | (B, 1) | (B, 2) -> ... *)

open Ppxlib

let combine_patterns ~loc patterns =
  let first, rest = List.hd patterns, List.tl patterns in
  List.fold_left (Ast_builder.Default.ppat_or ~loc) first rest
;;

let variant_pat_of_construct_expr ~loc label =
  let open Ast_builder.Default in
  function
  | Some { pexp_desc = Pexp_constant c; pexp_loc = eloc; _ } ->
    ppat_construct ~loc label (Some (ppat_constant ~loc:eloc c))
  | Some { pexp_desc = Pexp_extension (e, _); _ } when e.txt = "cross_any" ->
    ppat_construct ~loc label (Some (ppat_any ~loc:e.loc))
  | None -> ppat_construct ~loc label None
  | _ -> failwith "Cannot expand non-constant patterns"
;;

let rec pattern_of_expr (e : expression) =
  let open Ast_builder.Default in
  let loc = e.pexp_loc in
  match e.pexp_desc with
  | Pexp_constant c -> ppat_constant ~loc c
  | Pexp_extension (e, _) when e.txt = "cross_any" -> ppat_any ~loc
  | Pexp_construct (label, e) -> variant_pat_of_construct_expr ~loc label e
  | Pexp_tuple es -> ppat_tuple ~loc (List.map pattern_of_expr es)
  | _ -> failwith "Cannot expand non-constant or tuple patterns"
;;

let patterns_of_expr (e : expression) =
  match e.pexp_desc with
  | Pexp_tuple es -> List.map pattern_of_expr es
  | _ -> [ pattern_of_expr e ]
;;

let expand ~ctxt (exprs : expression list) _ =
  let loc = Expansion_context.Extension.extension_point_loc ctxt in
  let patterns_set = List.map patterns_of_expr exprs in
  let crossed =
    List.fold_left
      (fun all pats -> List.concat_map (fun a -> Cross.cross a pats) all)
      [ [] ]
      patterns_set
  in
  let all = List.map (Ast_builder.Default.ppat_tuple ~loc) crossed in
  combine_patterns ~loc all
;;

let matcher =
  let open Ast_pattern in
  pstr (pstr_eval (pexp_tuple __) __ ^:: nil)
;;

let cross_match_extender = Extension.V3.declare "cross_match" Pattern matcher expand
let extender_rule = Context_free.Rule.extension cross_match_extender
let () = Driver.register_transformation ~rules:[ extender_rule ] "cross_match"
