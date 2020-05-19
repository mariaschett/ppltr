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

let out_header = ["lhs"; "rhs"]

(* careful, find expects column to be there *)
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
  |> List.cons (out_header)
  |> Csv.save out_csv
