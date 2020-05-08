open Core
open Sorg
open Rule

let contexts_for_rule p r =
  let get_context len_pre len_post =
    let pre, p' = List.split_n p len_pre in
    let p'', post = List.split_n p' (List.length p' - len_post) in
    (p'', (pre, post))
  in
  let len_c  = List.length p - List.length r.lhs in
  let lens_pre = List.range ~start:`inclusive ~stop:`inclusive 0 len_c in
  List.map lens_pre ~f:(fun len_pre -> get_context len_pre (len_c - len_pre))

let reducts_for_rule p r =
  let try_rewrite_with_ctxt (p, c) =
    Option.map (Subst.match_opt r.lhs p)
      ~f:(fun s -> Ctxt.apply c (Subst.apply r.rhs s))
  in
  List.filter_map (contexts_for_rule p r) ~f:try_rewrite_with_ctxt

let reducts p rs = List.concat_map rs ~f:(reducts_for_rule p)

let normal_forms p rs =
  let rec dedup = function
    | [] -> []
    | p :: ps ->
      if List.mem ps p ~equal:Program_schema.equal
      then dedup ps
      else  p :: dedup ps
  in
  let nf_or_reducts p = match reducts p rs with
    | [] -> `Fst p
    | ps -> `Snd ps
  in
  let rec normal_forms nfs ps =
    let nfs', ps' = List.partition_map ps ~f:(nf_or_reducts) in
    let ps = List.concat ps' in
    let nfs = nfs' @ nfs in
    if List.is_empty ps then nfs else normal_forms nfs ps
  in dedup (normal_forms [] [p])

let is_normal_form p rs = List.is_empty (reducts p rs)
