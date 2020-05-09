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

let all_reducts p rs = List.concat_map rs ~f:(reducts_for_rule p)

let normal_forms strategy p rs =
  let rec dedup = function
    | [] -> []
    | p :: ps ->
      if List.mem ps p ~equal:Program_schema.equal
      then dedup ps
      else  p :: dedup ps
  in
  let nf_or_reducts p = match strategy p rs with
    | [] -> `Fst p
    | ps -> `Snd ps
  in
  let rec normal_forms nfs ps =
    let nfs', ps' = List.partition_map ps ~f:(nf_or_reducts) in
    let ps = dedup (List.concat ps') in
    let nfs = nfs' @ nfs in
    if List.is_empty ps then nfs else normal_forms nfs ps
  in normal_forms [] [p]

let is_normal_form p rs = List.is_empty (all_reducts p rs)

let one_reduct p rs = Option.to_list (List.hd (all_reducts p rs))

let count_reducts p rs =
  let add count r c' =
    List.Assoc.add count r c' ~equal:Rule.equal
  in
  List.fold rs ~init:[] ~f:(fun count r ->
      match List.length (reducts_for_rule p r) with
      | 0 -> count
      | c -> add count r c)

let print_counts counts =
  let update count r c' =
    let c =
      Option.value ~default:0 (List.Assoc.find count r ~equal:Rule.equal)
    in
    List.Assoc.add count r (c + c') ~equal:Rule.equal
  in
  List.fold counts ~init:[] ~f:(fun fcounts count ->
      List.fold count ~init:fcounts ~f:(fun fcounts (r, c) ->
          update fcounts r c))
  |> List.Assoc.inverse
  |> List.sort ~compare:(Tuple.T2.compare ~cmp1:Int.compare ~cmp2:Rule.compare)
  |> List.iter ~f:(fun (c, r) -> Format.printf "%d, %s\n" c (Rule.show r))

let rewrite_row row_to_p p_to_row rs out_rew_only row =
  let b = row_to_p row in
  match normal_forms one_reduct b rs with
  | [b'] when Program_schema.equal b b' && out_rew_only -> []
  | nfs -> List.map nfs ~f:(p_to_row row)

let process_csv row_to_p p_to_row in_csv rule_csv out_csv out_rew_only =
  let rs = Csv.Rows.load ~has_header:true rule_csv in
  let rs = List.map rs ~f:Rule_generator.rule in
  let in_csv = Csv.of_channel ~has_header:true (In_channel.create in_csv) in
  let bs = Csv.Rows.input_all in_csv in
  let lenbs = List.length bs in
  let (rbs, counts) = List.foldi bs ~init:([], []) ~f:(fun i (csv, counts) r ->
      Out_channel.print_endline ("Processing row " ^ (Int.to_string i) ^ " of " ^ (Int.to_string lenbs));
      let rr = rewrite_row row_to_p p_to_row rs out_rew_only r in
      let count = count_reducts (row_to_p r) rs in
      (csv @ rr, count :: counts))
  in
  print_counts counts;
  Csv.save out_csv (Csv.Rows.header in_csv :: rbs)

let process_blocks =
  process_csv
    (fun r -> Blk_generator.parse_bytecode (Csv.Row.find r "source bytecode"))
    (fun _ p -> Ebso.Printer.show_ebso_snippet p)

let process_rules =
  let mk_rule row rhs =
    let rule = { lhs = (Rule_generator.rule row).lhs ; rhs = rhs } in
    Sorg.Rule.show_csv (Rule_generator.source row) (Rule_generator.target row) rule
  in
  process_csv (fun row -> (Rule_generator.rule row).rhs)
    mk_rule
