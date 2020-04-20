open Core
open Ebso

let in_header =
  [ "source bytecode"
  ; "target bytecode"
  ; "target opcode"
  ; "target instruction count"
  ; "source gas"
  ; "target gas"
  ; "gas saved"
  ; "known optimal"
  ; "translation validation"
  ]

(* careful, find expects row to be there *)
let row_to_optimization r =
  let parse s = Parser.parse @@ Sedlexing.Latin1.from_string s in
  let sbc = Csv.Row.find r "source bytecode"
  and tbc = Csv.Row.find r "target bytecode"
  and tv = Csv.Row.find r "translation validation"
  and gs = Csv.Row.find r "gas saved" in
  if String.equal gs "0" || not (String.equal tv "true")
  then None
  else Some (parse sbc, parse tbc)

let write_optz in_csv out_csv =
  Csv.Rows.load ~has_header:true ~header:in_header in_csv
  |> List.filter_map ~f:row_to_optimization
  |> List.map ~f:(fun (lhs, rhs) ->
      [Program.show_hex lhs; Program.show_hex rhs])
  |> List.cons (["lhs"; "rhs"])
  |> Csv.save out_csv
