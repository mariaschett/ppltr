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
open Dsc

let list_eq_constr vn =
  let list_eq_constr_for_v v vs =
    List.fold vs ~init:[] ~f:(fun cs v' ->
        if String.equal v v'
        then cs
        (* variable v mapped to different name *)
        else ("(push_eq_dec " ^ v ^ " " ^ v' ^ ")") :: cs
      )
  in
  Hashtbl.fold vn ~init:[]
    ~f:(fun ~key:v ~data:vs eq_const ->
      (list_eq_constr_for_v v vs) @ eq_const)

let generate_eq_constraints vn =
  match list_eq_constr vn with
  | [] -> None
  | es -> Some (String.concat ~sep:" and " es)

let generate_matching_rhs rhs vn =
  (* do not add :: for empty program *)
  let match_rhs = if String.equal rhs "" then rhs else rhs ^ " :: " in
  let rhs =  "Some (" ^ match_rhs ^ " tl)" in
  match generate_eq_constraints vn with
  | None -> rhs
  | Some eqs -> "    if " ^ eqs ^ "\n    then " ^ rhs ^ "\n    else None\n"

let generate_definition rn lhs rhs vn  =
      [
        "Definition " ^ rn ^ " (p : list statement) : option (list statement) :=";
        "  match p with";
        "  | " ^ lhs ^ " :: tl =>";
        generate_matching_rhs rhs vn;
        "  | _ => None";
        "  end.";
      ]
      |> String.concat ~sep:"\n"

let generate_inversion_proof rn lhs vars =
  [
    "Lemma " ^ rn ^ "_inversion : forall p po,";
    " " ^ rn ^ " p = Some po -> exists tl " ^ String.concat vars ~sep:" " ^ ",";
    " p = " ^ lhs ^ ":: tl.";
    "Proof.";
    "  intros.";
    "  unfold " ^ rn ^ " in H.";
    "    revert H.";
    "    refine  (match p with";
    "    | " ^ lhs ^ " :: tl => _";
    "    | _ => _";
    "    end); simpl; intros; try congruence.";
    "    repeat eexists.";
    "Qed."
  ]
  |> String.concat ~sep:"\n"

let generate rn r =
  let vn = Hashtbl.create (module String) in
  let lhs = to_dsc (Some vn) r.lhs in
  let rhs = to_dsc None r.rhs in
  [
      "(* " ^ rn  ^ ": " ^ Rule.show r ^ " *)" ;
      generate_definition rn lhs rhs vn;
      generate_inversion_proof rn lhs (Hashtbl.data vn |> List.concat) ;
      "\n";
    ]
    |> String.concat ~sep:"\n"


let skip_rule r =
  let instr = Ebso.Instruction.T.ISZERO in
  List.mem (r.lhs) instr ~equal:(=) ||  (* ([%eq: Ebso.Instruction.T]) *)
  List.mem (r.rhs) instr ~equal:(=)

let check_and_generate n r =
  if skip_rule r
  then ""
  else generate n r

let header =
  "Require Import Statements.StmtExpressionless.
Require Import backend.phase.Expressionless.Semantics.
Require Import backend.phase.Expressionless.MonotoneGas.
Require Import List.

Require Import backend.Smallstep.
Require Import backend.MachineModel.
Require Import backend.MachineModelLow.
Require Import backend.MemoryModel.
Require Import backend.AbstractData.
Require Import backend.Values.HighValues.
Require Import backend.Values.LowValues.
Require Import backend.AST.

Require Import Omega.

Require Import cclib.Integers.
Require Import Cop.


Section WITH_DATA.
  Context adata {data_ops: CompatDataOps adata}.
  Variable ge: genv.

  Variable me: machine_env adata.
  Variable cd: list val.
  Variable funcKind : function_kind.

"

(* TODO: should be moved to helper Coq file *)
let push_eq =
  "Lemma push_eq_dec (l1 l2 : val + label) : {l1 = l2} + {l1 <> l2}.
Proof.
  decide equality.
  decide equality.
  apply Int256.eq_dec.
  decide equality.
Defined.

"

let write_templates rule_csv fn =
  let rs = Csv.Rows.load ~has_header:true rule_csv in
  let rs = List.map rs ~f:Rule_generator.rule in
  let data = List.foldi ~f:(fun i s r ->
      s ^ check_and_generate ("rule" ^ [%show: int] (i+5)) r) ~init:"" rs in
  Out_channel.write_all (fn ^ "Rules.v") ~data:(header^push_eq^data)
