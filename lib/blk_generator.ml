open Core

let parse_bytecode bc = Sedlexing.Latin1.from_string bc |> Ebso.Parser.parse

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

let generate_blks sz bcs =
  let ps = List.map ~f:parse_bytecode bcs in
  let bs = List.concat_map ps ~f:Ebso.Program.split_into_bbs in
  let bbs = List.concat_map bs ~f:(fun b -> split_into_bound_blocks b sz) in
  List.fold bbs ~init:[]
    ~f:(fun gbs b -> if List.exists gbs ~f:(equiv_mod_wsz b) then gbs else b :: gbs)
