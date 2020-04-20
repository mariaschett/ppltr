open Core

let parse_bytecode bc =
  Sedlexing.Latin1.from_string bc |> Ebso.Parser.parse ~ignore_data_section:true

let sliding_window xs sz =
  if sz <= 0 then [[]] else
    let rec sw xs acc =
      if List.length xs <= sz then xs :: acc
      else sw (List.tl_exn xs) (List.take xs sz :: acc)
    in
    sw xs [] |> List.rev

let split_into_bound_blocks b sz =
  let open Ebso.Program in
  match b with
  | NotEncodable _ -> []
  | Next b | Terminal (b, _) -> sliding_window b sz

let equiv_mod_wsz b1 b2 =
  let abstract_block b =
    (* to distinguish values between -7 and + 7 *)
    let word_size = 4 in
    Ebso.Program.val_to_const word_size b
  in
  Sorg.Program_schema.alpha_equal (abstract_block b1) (abstract_block b2)

let rec insert_block gbs b = match gbs with
  | [] -> [(b, 1)]
  | (b', n) :: gbs ->
    if equiv_mod_wsz b b' then
      (b', n + 1) :: gbs
    else
      (b', n) :: insert_block gbs b

let generate_blks sz bcs =
  let ps = List.map ~f:parse_bytecode bcs in
  let bs = List.concat_map ps ~f:Ebso.Program.split_into_bbs in
  let bbs = List.concat_map bs ~f:(fun b -> split_into_bound_blocks b sz) in
  List.fold bbs ~init:[] ~f:insert_block

let write_blks in_csv out_csv peephole_sz =
  Csv.Rows.load ~has_header:true in_csv
  |> List.map ~f:(fun r -> Csv.Row.find r "bytecode")
  |> generate_blks peephole_sz
  |> List.rev_map ~f:(fun (p, c) -> Ebso.Printer.show_ebso_snippet p @ [[%show: int] c])
  |> List.cons (Ebso.Printer.ebso_snippet_header @ ["instances"])
  |> Csv.save out_csv
