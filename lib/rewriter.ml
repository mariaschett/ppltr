(*   Copyright 2020 Maria A Schett and Julian Nagele

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
*)
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

let count_reducts ps rs =
  let module M = Map.Make_plain(Rule) in
  List.fold ps ~init:M.empty ~f:(fun m p ->
      List.fold rs ~init:m ~f:(fun m r ->
          match List.length (reducts_for_rule p r) with
          | 0 -> m
          | c -> M.update m r ~f:(fun c' -> Option.value c' ~default:0 + c)))
  |> M.to_alist

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

let rewrite_row row_to_ps ps_to_rows rs out_rew_only row =
  let rewrite_block b =
    match normal_forms one_reduct b rs with
    | [] -> None
    | b' :: _ when Program_schema.equal b b' && out_rew_only -> None
    | b' :: _ -> Some b'
  in
  row_to_ps row
  |> List.filter_map ~f:rewrite_block
  |> ps_to_rows row

let process_csv ?(hdr=None) row_to_ps ps_to_rows in_csv rule_csv out_csv out_rew_only =
  let rs = Csv.Rows.load ~has_header:true rule_csv in
  let rs = List.map rs ~f:Rule_generator.rule in
  let in_csv = Csv.of_channel ~has_header:true (In_channel.create in_csv) in
  let bs = Csv.Rows.input_all in_csv in
  let lenbs = List.length bs in
  let (rbs, counts) = List.foldi bs ~init:([], []) ~f:(fun i (csv, counts) r ->
      Out_channel.print_endline ("Processing row " ^ (Int.to_string i) ^ " of " ^ (Int.to_string lenbs));
      let rr = rewrite_row row_to_ps ps_to_rows rs out_rew_only r in
      let count = count_reducts (row_to_ps r) rs in
      (csv @ rr, count :: counts))
  in
  let hdr = Option.value hdr ~default:(Csv.Rows.header in_csv) in
  print_counts counts;
  Csv.save out_csv (hdr :: rbs)

let process_blocks =
  process_csv
    (fun r -> [Blk_generator.parse_bytecode (Csv.Row.find r "source bytecode")])
    (fun _ ps -> List.map ~f:Ebso.Printer.show_ebso_snippet ps)

let process_rules =
  let mk_rule row rhss =
    let rule rhs = { lhs = (Rule_generator.rule row).lhs ; rhs = rhs } in
    List.map rhss ~f:(fun rhs ->
        Sorg.Rule.show_csv (Rule_generator.source row) (Rule_generator.target row) (rule rhs))
  in
  process_csv (fun row -> [(Rule_generator.rule row).rhs])
    mk_rule

let process_contracts =
  let mk_contract row reducts =
    let tgt = List.concat reducts in
    let src = Blk_generator.parse_bytecode (Csv.Row.find row "bytecode") in
    let src_gas = Ebso.Gas_cost.to_int (Ebso.Program.total_gas_cost src) in
    let tgt_gas = Ebso.Gas_cost.to_int (Ebso.Program.total_gas_cost tgt) in
    let saved = src_gas - tgt_gas in
    let tx_count = Int.of_string (Csv.Row.find row "tx_count") in
    let src_len = List.length src in
    [[ Csv.Row.find row "address"
     ; Int.to_string tx_count
     ; Int.to_string saved
     ; Int.to_string (saved * tx_count)
     ; Int.to_string src_len
     ; Int.to_string (src_len - List.length tgt)
    ]]
  in
  process_csv
    ~hdr:(Some ["address"; "tx_count"; "gas_saved_per_tx";
                "gas_saved_total"; "instrs_src"; "instrs_saved"])
    (fun r ->
       Blk_generator.parse_bytecode (Csv.Row.find r "bytecode")
       |> Ebso.Program.split_into_bbs
       |> List.map ~f:(fun bb -> Ebso.Program.concat_bbs [bb]))
    mk_contract
