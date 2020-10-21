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
open Sorg.Rule
open Z3util

let eq_enc r =
  let ecs = Enc_consts.mk_trans_val r.lhs r.rhs (`User []) in
  Superoptimization.enc_trans_val ecs

let show_eq_enc r =
  let c = eq_enc (Rule_generator.rule r) in
  String.concat
    [ (* print rule in comment at top of encoding *)
      "; " ^ Rule_generator.show_rule r
    ; "\n(set-option :produce-proofs true)\n"
    ; Z3.SMT.benchmark_to_smtstring !ctxt "" "" "unknown" "" [] (Z3.Expr.simplify c None)
    ; "(get-proof)\n"
    ]

let write_equiv_encodings in_csv dir =
  Word.set_wsz 256;
  let rows = Csv.Rows.load ~has_header:true in_csv in
  List.iteri ~f:(fun i r -> Out_channel.write_all (dir ^ "equiv_enc_" ^ [%show: int] i ^ ".smt2" ) ~data:(show_eq_enc r)) rows
