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

let generate_eq_constraints vn nn =
  let eqs_nn = list_eq_constr nn and eqs_vn = list_eq_constr vn in
  List.fold (eqs_nn @ eqs_vn) ~init:("","") ~f:(fun (pre, post) e ->
      ((pre ^ "    if " ^ e ^ "\n    then ", "\n    else None\n" ^ post)))

let generate_matching_rhs rhs vn nn =
  (* do not add :: for empty program *)
  let match_rhs = if String.equal rhs "" then rhs else rhs ^ " :: " in
  let rhs =  "Some (" ^ match_rhs ^ " tl)" in
  let (pre, post) = generate_eq_constraints vn nn in (pre ^ rhs ^ post)

let generate_definition rn lhs rhs vn nn  =
  [
    "Definition " ^ rn ^ "_rewrite (p : list statement) : option (list statement) :=";
    "  match p with";
    "  | " ^ lhs ^ " :: tl =>";
    generate_matching_rhs rhs vn nn;
      "  | _ => None";
    "  end.";
    "" (* for formatting *)
  ]
  |> String.concat ~sep:"\n"

let generate_inversion_proof rn inv_lhs lhs vars nns =
  let destructs = List.map ~f:(fun eq -> "    destruct (" ^ eq ^ "); try congruence.") nns in
  [
    "Lemma " ^ rn ^ "_inversion : forall p po,";
    " " ^ rn ^ "_rewrite p = Some po -> exists tl " ^ String.concat vars ~sep:" " ^ ",";
    " p = " ^ inv_lhs ^ ":: tl.";
    "Proof.";
    "  intros.";
    "  unfold " ^ rn ^ "_rewrite in H.";
    "    revert H.";
    "    refine  (match p with";
    "    | " ^ lhs ^ " :: tl => _";
    "    | _ => _";
    "    end); simpl; intros; try congruence.";
  ] @ destructs @ [
    "    subst.";
    "    repeat eexists.";
    "Qed.";
    "" (* for formatting *)
  ]
  |> String.concat ~sep:"\n"

let generate_one_proof rn =
  [
    "Proof.";
    "intros ge0 p po H stack stack' stk stk' he he' d d' gas gas' Hp.";
    "(* Some setup to find p and po. *)";
    "assert (H2 := H); apply " ^ rn ^  "_inversion in H2.";
    "subst_progr H.";
    "(* Big chain of inversions for the run Hp *)";
    "repeat run_step.";
    "(* make stack explicit *)";
    "repeat (stack_get_inv || rewrite stack_get_succ in *).";
    "repeat stack_set.";

    "(* Reason about gas *)";
    "assert_gas.";
    "Focus 2.";
    "destruct_gas_monotone.";
    "destruct Hg as [Hstep Hle].";

   " eexists; split.";
    "Focus 2.";

   " (* Prove the other run. *)";
    "repeat other_run.";
    "exact Hstep.";
    "exact Hle.";

   " (* Using less gas *)";
    "gas_decreasing.";
   "Qed.";
  ]

let generate_one rn =
  [
    "Lemma " ^ rn ^ "_one : forall ge p po,";
    " " ^ rn ^ "_rewrite p = Some po ->";
    "forall stack stack' stk stk' he he' d d' gas gas',";
    "    (star (step me cd funcKind)";
    "         ge";
    "         (State stack p stk he d gas)";
    "         (Returnstate stack' stk' he' d' gas'))";
    "->";
    "exists gas1',";
    "  gas1' <= gas' /\\ ";
    " (star (step me cd funcKind)";
    "       ge";
    "       (State stack po stk he d gas)";
    "       (Returnstate stack' stk' he' d' gas1')).";
  ] @ generate_one_proof rn
  |> String.concat ~sep:"\n"

let generate rn r =
  let vn = Hashtbl.create (module String) in
  let nn = Hashtbl.create (module String) in
  let lhs = to_dsc (Some vn) (Some nn) r.lhs in
  let rhs = to_dsc None None r.rhs in
  (* super-ugly, build map for variable renmaing again and rely on
     determinism of construction to have variables replaced, but not
     the integers *)
  let inv_vn = Hashtbl.create (module String) in
  let inv_lhs = to_dsc (Some inv_vn) None r.lhs in
  (* also super-ugly, rely on deternism of construction again to get
     same order of variables as we got in producing them in
     generate_matching_rhs *)
  let eqs_nn = list_eq_constr nn in
  [
    "(* " ^ rn  ^ ": " ^ Rule.show r ^ " *)" ;
    generate_definition rn lhs rhs vn nn;
    generate_inversion_proof rn inv_lhs lhs (Hashtbl.data vn |> List.concat) eqs_nn;
    generate_one rn;
    "\n";
  ]
  |> String.concat ~sep:"\n"

let skip_rule r n =
  let unavail = [
    Ebso.Instruction.T.ISZERO
  ; Ebso.Instruction.T.MSIZE
  ; Ebso.Instruction.T.SLT
  ; Ebso.Instruction.T.SGT
  ; Ebso.Instruction.T.CALLDATASIZE
  ; Ebso.Instruction.T.SIGNEXTEND
  ; Ebso.Instruction.T.ADDMOD
  ; Ebso.Instruction.T.MULMOD
  ; Ebso.Instruction.T.GAS
  ; Ebso.Instruction.T.EXTCODESIZE
  ; Ebso.Instruction.T.BALANCE
  ; Ebso.Instruction.T.RETURNDATASIZE
  ; Ebso.Instruction.T.BYTE
  ]
  in
  (* rule numbers to skip *)
  let ns = [
    6; (* requires SUB X X = 0 *)
    10; (* requires XOR commutative *)
    12; (* requires NOT NOT X = X *)
    13; (* requires MULT *)
    15; (* requires ADD 0 X = X *)
    19; (* requires NOT AND is 0 *)
    28; (* requires LT is inverse of GT *)
    31; (* requires ADD commutative *)
    34; (* requires ADD 0 X = X *)
    35; (* requires NOT NOT X = X *)
    41; (* requires ADD 0 X = X *)
    42; (* requires SUB X X = 0 *)
    44; (* requires NOT NOT X = X *)
    52; (* requires commutativity of AND or OR *)
    57; (* requires ADD 0 X = X *)
    58; (* requires ADD commutative *)
    61; (* requires ADD commutative *)
    63; (* requires SUB X X = 0 *)
    66; (* requires MLOAD from same twice is same *)
    67; (* requires ADD commutative *)
    74; (* requires SUB X X = 0 *)
    83; (* requires NOT AND is 0 *)
    88; (* requires NOT NOT X = X *)
    89; (* requires NOT NOT X = X *)
    91; (* requires NOT NOT X = X *)
    92; (* requires SUB X X = 0 *)
    100; (* requires NOT NOT X = X *)
  ] in
  (List.mem ns n ~equal:(=)) ||
  (List.exists unavail  ~f:(fun instr -> List.mem (r.lhs @ r.rhs) instr ~equal:(=))) ||
  (n > 100)

let check_and_generate i r =
  if skip_rule r i
  then ""
  else generate ("rule" ^ [%show: int] i) r

let write_templates rule_csv fn =
  let rs = Csv.Rows.load ~has_header:true rule_csv in
  let rs = List.map rs ~f:Rule_generator.rule in
  let data = List.foldi ~f:(fun i s r ->
      (* +2 to correspond to line number in csv *)
      s ^ check_and_generate (i + 2) r) ~init:"" rs in
  Out_channel.write_all (fn ^ "Rules.v") ~data:(header^data)
