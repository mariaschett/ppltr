open Core
open Sorg
open Ebso

let in_header =
  [ "rule lhs"
  ; "rule rhs"
  ; "gas saved"
  ; "optimization source"
  ; "optimization target"
  ; "tpdb"
  ]

type stats =
  { duplicates : Csv.Row.t list
  ; multiples : Csv.Row.t list list
  ; timeouts : Csv.Row.t list
  }

let parse_row key parse_with row =
  Csv.Row.find row key |> Sedlexing.Latin1.from_string |> parse_with

let source = parse_row "optimization source" Parser.parse

let target = parse_row "optimization target" Parser.parse

let rule row =
  let open Rule in
  let lhs = parse_row "rule lhs" Program_schema.parse row in
  let rhs = parse_row "rule rhs" Program_schema.parse row in
  {lhs = lhs; rhs = rhs}

let compute_multiple_optimizations rows =
  List.group ~break:(fun row1 row2 -> source row1 = source row2 && target row1 = target row2) rows
  |> List.filter ~f:(fun group -> List.length group > 1)

let timed_out row = let rule = rule row in rule.lhs = []

let insert_non_dup row_to_insert (rows, dups) =
  if List.exists rows ~f:(fun row  -> Rule.equal (rule row) (rule row_to_insert))
  then (rows, row_to_insert :: dups)
  else (row_to_insert :: rows, dups)

let rm_duplicates =
  List.fold ~init:([],[]) ~f:(fun (rows, dups) row -> insert_non_dup row (rows, dups))

let compute_results in_csv =
  let rows = Csv.Rows.load ~has_header:true ~header:Optz_generator.out_header in_csv in
  (* remove timeouts from rows *)
  let (timeouts, rows_without_timeouts) = List.partition_tf ~f:(timed_out) rows in
  (* rows where optimizations gave rise to multiple rules *)
  let multiples = compute_multiple_optimizations rows_without_timeouts in
  (* remove duplciates *)
  let (final_rows, duplicates) = rm_duplicates rows_without_timeouts
  in
  (final_rows, { duplicates = duplicates; multiples = multiples; timeouts = timeouts; })

let write_rules out_csv rules =
  let rules' = List.map ~f:(Csv.Row.to_list) rules in
  Csv.save out_csv (in_header :: rules')

let print_dups stats =
  Format.printf
    "\nThe following %i duplicate rules were generated:\n"
    (List.length stats.duplicates);
  List.iter stats.duplicates ~f:(fun row -> Format.printf "%s" (Rule.show (rule row)))

let show_optimization row =
  Printf.sprintf "%s >= %s" (Program.show_h (source row)) (Program.show_h (target row))

let print_muls stats =
  let print_mul group =
    Format.printf "@[%s@\n" (show_optimization (List.hd_exn group));
    List.iter group ~f:(fun row -> Format.printf "  %s@\n" (Rule.show (rule row)));
    Format.printf "@]"
  in
  Format.printf "\nThe following %i optimizations generated multiple rules:"
    (List.length stats.multiples);
  List.iter stats.multiples ~f:print_mul

let print_timeouts stats =
  Format.printf
    "\nFor following %i optimizations rule generation timed out:\n"
    (List.length stats.timeouts);
  List.iter stats.timeouts ~f:(fun row -> Format.printf "%s@\n" (show_optimization row))

let print_stats stats =
  print_dups stats;
  print_muls stats;
  print_timeouts stats
