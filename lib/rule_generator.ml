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

let contains p iota =
  let equal_incl_push_args i1 i2 = match (i1, i2) with
    | PUSH x, PUSH y -> x = y
    | _ -> Instruction.equal i1 i2 in
  List.mem p iota ~equal:equal_incl_push_args

let parse_row key parse_with row =
  Csv.Row.find row key |> Sedlexing.Latin1.from_string |> parse_with

let source = parse_row "optimization source" Parser.parse

let target = parse_row "optimization target" Parser.parse

let rule row =
  let open Rule in
  let lhs = parse_row "rule lhs" Program_schema.parse row in
  let rhs = parse_row "rule rhs" Program_schema.parse row in
  {lhs = lhs; rhs = rhs}

let gas_saved row = Int.of_string (Csv.Row.find row "gas_saved")

(* assumes group-able rules are consecutive *)
let compute_multiple_optimizations rows =
  List.group ~break:(fun row1 row2 -> not (source row1 = source row2 && target row1 = target row2)) rows
  |> List.filter ~f:(fun group -> List.length group > 1)

let insert_non_dup row_to_insert (rows, dups) =
  if List.exists rows ~f:(fun row  -> Rule.equal (rule row) (rule row_to_insert))
  then (rows, row_to_insert :: dups)
  else (row_to_insert :: rows, dups)

let rm_duplicates =
  List.fold ~init:([],[]) ~f:(fun (rows, dups) row -> insert_non_dup row (rows, dups))

let timed_out row = Csv.Row.find row "rule lhs" = ""

let compute_results in_csv =
  let rows = Csv.Rows.load ~has_header:true ~header:in_header in_csv in
  (* remove timeouts from rows *)
  let (timeouts, rows_without_timeouts) = List.partition_tf ~f:timed_out rows in
  (* remove duplciates *)
  let (final_rows, duplicates) = rm_duplicates rows_without_timeouts in
  (* rows where optimizations gave rise to multiple rules *)
  let multiples = compute_multiple_optimizations final_rows
  in
  (final_rows, {
      duplicates = duplicates;
      multiples = multiples;
      timeouts = timeouts;
      count_sorg_rules = (List.length rows - List.length timeouts);
      count_final_rules = List.length final_rows;
    })

let top_btm sort_by in_csv t b =
  let rows = Csv.Rows.load ~has_header:true ~header:in_header in_csv in
  let srtd_rows = List.sort ~compare:sort_by rows in
  (List.take srtd_rows t, List.take (List.rev (srtd_rows)) b)

let top_btm_gas_saved =
  top_btm (fun row1 row2 -> Int.compare (gas_saved row1) (gas_saved row2))

let top_btm_len_diff =
  let open Rule in
  let len_diff rule = (List.length rule.lhs) - (List.length rule.rhs) in
  top_btm (fun row1 row2 -> Int.compare (len_diff (rule row1)) (len_diff (rule row2)))

let rhs_finds_new in_csv =
  let open Rule in
  let rhs_diff rule = not (List.for_all rule.rhs ~f:(contains rule.lhs)) in
  let rows = Csv.Rows.load ~has_header:true ~header:in_header in_csv in
  List.filter rows ~f:(fun row -> rhs_diff (rule row))

let rmvd_instr in_csv =
  let open Rule in
  let rmvd_instr m row =
    let rule  = (rule row) in
    List.fold rule.lhs ~init:m ~f:(fun m' iota ->
        if not (contains rule.rhs iota)
        then
          Instruction.Map.update m' iota ~f:(Option.value_map ~default:1 ~f:(Int.succ))
        else m'
      )
     in
  let rows = Csv.Rows.load ~has_header:true ~header:in_header in_csv in
  List.fold rows ~init:Instruction.Map.empty ~f:rmvd_instr

let write_rules out_csv rules =
  let rules' = List.map ~f:(Csv.Row.to_list) rules in
  Csv.save out_csv (in_header :: rules')

let print_stats key rows printer =
  Format.printf "\nThe following %s:\n" key;
  List.iter rows ~f:printer;
  Format.printf "\n"

let group_duplicates =
  List.fold ~init:[] ~f:(fun gs d ->
      let r = rule d in
      match List.Assoc.find gs ~equal:Rule.equal r with
      | Some i -> List.Assoc.add gs ~equal:Rule.equal r (i+1)
      | None -> List.Assoc.add gs ~equal:Rule.equal r 1
    )

let print_dups stats =
  let comp_count (_, c1) (_, c2) = Int.compare c2 c1 in
  let sorted_grouped_duplicates = List.sort ~compare:comp_count (group_duplicates stats.duplicates) in
  Format.printf "#single duplicate: %d\n" (List.count sorted_grouped_duplicates ~f:(fun (_, i) -> i = 1));
  Format.printf "#duplicates >= 15: %d\n" (List.fold sorted_grouped_duplicates ~init:0 ~f:(fun c (_, i) -> if i >= 15 then c + i else c));
  print_stats "duplicate rules were generated" sorted_grouped_duplicates
    (fun (r, count) -> Format.printf "%s : %d \n" (Rule.show r) count)

let show_optimization row =
  Printf.sprintf "%s >= %s" (Program.show_h (source row)) (Program.show_h (target row))

let print_muls stats =
  print_stats "optimizations generated multiple rules" stats.multiples
    (fun group ->
       Format.printf "%s" (show_optimization (List.hd_exn group));
       List.iter group ~f:(fun row -> Format.printf "\n  %s" (Rule.show (rule row))))

let print_timeouts stats =
  print_stats "optimizations timed out while rule generation" stats.timeouts
  (fun row -> Format.printf "%s@\n" (show_optimization row))

let print_stats stats =
  Format.printf "#rules generated by sorg: %d\n" stats.count_sorg_rules;
  Format.printf "#timeouts: %d\n" (List.length stats.timeouts);
  Format.printf "#final rules: %d\n" stats.count_final_rules;
  Format.printf "#dupcliate rules: %d\n" (List.length stats.duplicates);
  Format.printf "#multiple rules: %d\n" (List.length stats.multiples);
  Format.printf "#check sum: %d = %d \n"
    (stats.count_final_rules + List.length stats.duplicates)
    stats.count_sorg_rules
  ;
  print_dups stats;
  print_muls stats;
  print_timeouts stats;
