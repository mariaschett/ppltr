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

let drop_non_encodable b =
  let open Ebso.Program in
  match b with
  | NotEncodable _ -> None
  | Next b | Terminal (b, _) -> Some b

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

let generate_blks bcs =
  List.map ~f:parse_bytecode bcs
  |> List.concat_map ~f:Ebso.Program.split_into_bbs
  |> List.filter_map ~f:drop_non_encodable

let write_blks in_csv out_csv =
  Csv.Rows.load ~has_header:true in_csv
  |> List.map ~f:(fun r -> Csv.Row.find r "bytecode")
  |> generate_blks
  |> List.map ~f:Ebso.Printer.show_ebso_snippet
  |> List.cons Ebso.Printer.ebso_snippet_header
  |> Csv.save out_csv

let window_blks sz bs =
  let bs = List.map ~f:parse_bytecode bs in
  let bbs = List.concat_map bs ~f:(fun b -> sliding_window b sz) in
  let lenbbs = List.length bbs in
  Out_channel.print_endline ("Transformed blocks into " ^ (Int.to_string lenbbs) ^ " windowed blocks.");
  List.foldi bbs ~init:[] ~f:(fun i gbs b ->
      Out_channel.print_endline ("Processing block " ^ (Int.to_string i) ^ " of " ^ (Int.to_string lenbbs));
      insert_block gbs b)

let write_windowed_blks in_csv out_csv peephole_sz =
  Csv.Rows.load ~has_header:true in_csv
  |> List.map ~f:(fun r -> Csv.Row.find r "source bytecode")
  |> window_blks peephole_sz
  |> List.rev_map ~f:(fun (p, c) -> Ebso.Printer.show_ebso_snippet p @ [[%show: int] c])
  |> List.cons (Ebso.Printer.ebso_snippet_header @ ["instances"])
  |> Csv.save out_csv
