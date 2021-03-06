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
open Ebso
open Ebso.Instruction.T

let in_header =
  [ "rule lhs"
  ; "rule rhs"
  ; "vars"
  ; "gas saved"
  ; "optimization source"
  ; "optimization target"
  ; "tpdb"
  ]

type stats =
  { duplicates : Csv.Row.t list
  ; multiples : Csv.Row.t list list
  ; timeouts : Csv.Row.t list
  ; count_sorg_rules : int
  ; count_final_rules : int
  }

let equal_mod i1 i2 = match (i1, i2) with
  | PUSH _, PUSH _ -> true
  | SWAP _, SWAP _ -> true
  | DUP _, DUP _ -> true
  | _ -> Instruction.equal i1 i2

let contains p iota =
  List.mem p iota ~equal:(fun i1 i2 -> match i1, i2 with
      | PUSH (Word w1), PUSH (Word w2) -> w1 = w2
      | _ -> equal_mod i1 i2)

let parse_field key parse_with row =
  Csv.Row.find row key |> Sedlexing.Latin1.from_string |> parse_with

let source = parse_field "optimization source" Parser.parse

let target = parse_field "optimization target" Parser.parse

let rule row =
  let open Rule in
  let lhs = parse_field "rule lhs" Program_schema.parse row in
  let rhs = parse_field "rule rhs" Program_schema.parse row in
  {lhs = lhs; rhs = rhs}

let gas_saved row = Int.of_string (Csv.Row.find row "gas saved")

(* assumes group-able rules are consecutive *)
let compute_multiple_optimizations rows =
  List.group ~break:(fun row1 row2 -> not (source row1 = source row2 && target row1 = target row2)) rows
  |> List.filter ~f:(fun group -> List.length group > 1)

let insert_non_dup (rows, dups) row_to_insert =
  if List.exists rows ~f:(fun row -> Rule.equal (rule row) (rule row_to_insert))
  then (rows, row_to_insert :: dups)
  else (row_to_insert :: rows, dups)

let rm_duplicates = List.fold ~init:([], []) ~f:insert_non_dup

let timed_out row = Csv.Row.find row "rule lhs" = ""

let compute_results in_csv =
  let rows = Csv.Rows.load ~has_header:true ~header:in_header in_csv in
  (* remove timeouts from rows *)
  let (timeouts, rows_without_timeouts) = List.partition_tf ~f:timed_out rows in
  (* remove duplciates *)
  let (final_rows, duplicates) = rm_duplicates rows_without_timeouts in
  (* rows where optimizations gave rise to multiple rules *)
  let multiples = compute_multiple_optimizations final_rows in
  (final_rows,
   { duplicates = duplicates
   ; multiples = multiples
   ; timeouts = timeouts
   ; count_sorg_rules = List.length rows - List.length timeouts
   ; count_final_rules = List.length final_rows
   })

let top_btm sort_by rules t b =
  let srtd_rows = List.sort ~compare:sort_by rules in
  let take_continue xs i =
    let (fst_i, tail) = List.split_n xs i in
    match List.last fst_i with
    | None -> []
    | Some x -> fst_i @ List.take_while tail ~f:(fun y -> sort_by x y = 0)
  in
  (take_continue srtd_rows t, take_continue (List.rev srtd_rows) b)

let top_btm_gas_saved =
  top_btm (fun row1 row2 -> Int.compare (gas_saved row2) (gas_saved row1))

let len_diff row = List.length (rule row).lhs - List.length (rule row).rhs

let top_btm_len_diff =
  top_btm (fun row1 row2 -> Int.compare (len_diff row2) (len_diff row1))

let diff_instr l r =
  let rec rm_fst p f = match p with
    | [] -> []
    | h :: tl when f h -> tl
    | h :: tl -> h :: (rm_fst tl f)
  in
  List.fold l ~init:r ~f:(fun r' iota -> rm_fst r' (equal_mod iota))

let rhs_finds_new rows =
  List.filter_map rows ~f:(fun row ->
      let rule = rule row in
      match diff_instr rule.lhs rule.rhs with
      | [] -> None
      | is -> Some ((Program.show_h is) :: Csv.Row.to_list row))

let rmvd_instr rows diff =
  List.fold rows ~init:Instruction.Map.empty ~f:(
    fun m row ->
      List.fold (diff (rule row)) ~init:m ~f:(fun m' iota ->
          Instruction.Map.update m' iota ~f:(Option.value_map ~default:1 ~f:(Int.succ))))

let write_rules out_csv rules =
  let rules' = List.map ~f:(Csv.Row.to_list) rules in
  Csv.save out_csv (in_header :: rules')

let group_duplicates =
  List.fold ~init:[] ~f:(fun gs d ->
      let r = rule d in
      match List.Assoc.find gs ~equal:Rule.equal r with
      | Some i -> List.Assoc.add gs ~equal:Rule.equal r (i+1)
      | None -> List.Assoc.add gs ~equal:Rule.equal r 1
    )

let sorted_grouped_duplicates stats =
  let comp_count (_, c1) (_, c2) = Int.compare c2 c1 in
  List.sort ~compare:comp_count (group_duplicates stats.duplicates)

let show_rule row =
  Printf.sprintf "%s => %s" (Program.show_h (rule row).lhs) (Program.show_h (rule row).rhs)

let show_optimization row =
  Printf.sprintf "%s >= %s" (Program.show_h (source row)) (Program.show_h (target row))

let show_instruction_maps_grouped m_add m_rem =
  let groups =
    [ ("Arith", [ADD; SUB; MUL; ADDMOD; MULMOD; DIV; MOD; SDIV; SMOD]);
      ("Comp", [LT; GT; EQ; SLT; SGT]);
      ("ISZERO", [ISZERO]);
      ("Bitwise", [AND; OR; XOR; NOT]);
      ("DUP", List.filter Instruction.all
         ~f:(fun i -> match i with DUP _ -> true | _ -> false));
      ("SWAP", List.filter Instruction.all
         ~f:(fun i -> match i with SWAP _ -> true | _ -> false));
      ("PUSH", [PUSH Tmpl]);
      ("POP", [POP]);
      ("EnvInfo",
       [ ADDRESS ; BALANCE ; ORIGIN ; CALLER ; CALLVALUE ; CALLDATALOAD ; CALLDATASIZE
       ; CALLDATACOPY ; CODESIZE ; CODECOPY ; GASPRICE ; EXTCODESIZE ; EXTCODECOPY
       ; RETURNDATASIZE ; RETURNDATACOPY ; TIMESTAMP
       ]);
      ("Mem", [MLOAD]);
    ]
  in
  let default_grp =
    List.filter Instruction.all ~f:(fun i ->
        not (List.exists groups ~f:(fun (_, g) -> List.mem g i ~equal:Instruction.equal)))
  in
  let count_grp g m =
    List.sum (module Int) g ~f:(fun i -> match Instruction.Map.find m i with
        | None -> 0
        | Some c -> c)
  in
  Format.printf "\n# Added,Removed Instructions\n";
  List.iter (groups @ [("other", default_grp)]) ~f:(fun (n, g) ->
      Format.printf "%s,%d,%d\n" n (count_grp g m_add) (count_grp g m_rem))

let print_stats stats rules dir =
  let write_stats_csv fn rows = Csv.save (dir ^ fn ^ ".csv") rows ~quote_all:true in

  Format.printf "# Generated Rules\n";
  Format.printf "| rules generated by sorg | %d |\n" stats.count_sorg_rules;
  Format.printf "| timeouts | %d |\n" (List.length stats.timeouts);
  Format.printf "| final rules | %d |\n" stats.count_final_rules;
  Format.printf "| dupcliate rules |  %d |\n" (List.length stats.duplicates);
  Format.printf "| multiple rules | %d |\n" (List.length stats.multiples);

  Format.printf "\ncheck sum: final rules + duplicates : %d = %d : rules generated by sorg \n"
    (stats.count_final_rules + List.length stats.duplicates) stats.count_sorg_rules;

  write_stats_csv "timeouts" (List.map ~f:Csv.Row.to_list stats.timeouts);

  let dups = sorted_grouped_duplicates stats in
  Format.print_newline ();
  Format.printf "# Duplicated Rules\n";
  Format.printf "| duplicated once |  %d |\n" (List.count dups ~f:(fun (_, i) -> i = 1));
  Format.printf "| duplicated>= 15 | %d |\n" (List.fold dups ~init:0 ~f:(fun c (_, i) -> if i >= 15 then c + i else c));
  write_stats_csv "duplicate_rules"
    (["rule"; "count"] :: (List.map dups ~f:(fun (r, c) -> [[%show: Rule.t] r; [%show: int] c])));

  Format.print_newline ();
  Format.printf "# Multiple Rules\n";
  List.iter stats.multiples
    ~f:(fun group ->
       Format.printf "%s" (show_optimization (List.hd_exn group));
       List.iter group ~f:(fun row -> Format.printf "\n  %s" (Rule.show (rule row))));

  Format.print_newline ();
  let m_rem = rmvd_instr rules (fun rule -> diff_instr rule.rhs rule.lhs) in
  let m_add = rmvd_instr rules (fun rule -> diff_instr rule.lhs rule.rhs) in
  show_instruction_maps_grouped m_add m_rem;

  Format.print_newline ();
  let tg, bg = top_btm_gas_saved rules 5 1 in
  Format.printf "# Top Rules for gas saving\n";
  List.iter tg ~f:(fun r -> Format.printf "\n  %s,%d" (show_rule r) (gas_saved r));
  Format.print_newline ();
  Format.print_newline ();
  Format.printf "# Bottom Rules for gas saving\n";
  List.iter bg ~f:(fun r -> Format.printf "\n  %s,%d" (show_rule r) (gas_saved r));

  Format.print_newline ();
  Format.print_newline ();
  let tl, bl = top_btm_len_diff rules 2 1 in
  Format.printf "# Top Rules for length difference\n";
  List.iter tl ~f:(fun r -> Format.printf "\n  %s,%d" (show_rule r) (len_diff r));
  Format.print_newline ();
  Format.print_newline ();
  Format.printf "# Bottom Rules for length difference\n";
  List.iter bl ~f:(fun r -> Format.printf "\n  %s,%d" (show_rule r) (len_diff r));
  Format.print_newline ();

  let news = rhs_finds_new rules in
  write_stats_csv "rhs_finds_new_rules" (("new" :: in_header) :: news)
