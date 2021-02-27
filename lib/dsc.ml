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
open Word
open Instruction.T

let val_to_int256 v =
  let n = Z.of_string v in
  let s = if Z.equal n (Z.of_string "0")
    then "Vint Int256.zero"
    else if Z.equal n (Z.of_string "1")
    then "Vint Int256.one"
    else "Vint (Int256.repr " ^ (Z.format "d" n) ^ ")"
  in
  "(inl (" ^ s ^ "))"

let nxt_free_var vnm v =
  let x = "n" ^ (Z.format "d" (Z.of_string v)) in
  let with_idx idx = x ^ "_" ^ ([%show: int] idx) in
  let rec nxt_free_idx idx =
    if Hashtbl.mem vnm (with_idx idx) then nxt_free_idx (idx + 1) else idx
  in
  if Hashtbl.mem vnm x then (with_idx (nxt_free_idx 1)) else x

let to_var_for_int vnm v =
  let i = val_to_int256 v in
  match vnm with
  | None -> i
  | Some vnm -> let x = nxt_free_var vnm v in Hashtbl.set vnm ~key:x ~data:[i]; x

let to_distinct_variables vnm c =  match vnm with
  | Some vn ->
    Hashtbl.findi_and_call vn c
      ~if_found:(fun ~key:c ~data:rns ->
          let c' = c ^ "_" ^ [%show: int] (List.length rns) in
          Hashtbl.set vn ~key:c ~data:(c' :: rns);
          c')
      ~if_not_found:(fun c ->
          Hashtbl.set vn ~key:c ~data:[c]; c)
  | None -> c

let instr_to_dsc vn nn = function
  | STOP -> "Sstop"
  | ADD -> "Sbinop Oadd false"
  | MUL -> "Sbinop Omul false"
  | SUB -> "Sbinop Osub false"
  | DIV -> "Sbinop Odiv false"
  | SDIV -> "Sdiv"
  | MOD -> "Smod"
  | SMOD -> "Ssmod"
  | ADDMOD -> "Saddmod"
  | MULMOD -> "Smulmod"
  | EXP -> "Sbinop Oexp false"
  | SIGNEXTEND -> "Ssignextend"
  | LT -> "Sbinop Olt false"
  | GT -> "Sbinop Ogt false"
  | SLT -> "Sslt"
  | SGT -> "Ssgt"
  | EQ -> "Sbinop Oeq false"
  | ISZERO -> "Siszero"
  | AND -> "Sbinop Oand false"
  | OR -> "Sbinop Oor false"
  | XOR -> "Sbinop Oxor false"
  | NOT -> "Sunop Onotint"
  | BYTE -> "Sbyte"
  | SHL -> "Sshl"
  | SHR -> "Sshr"
  | SAR -> "Ssar"
  | SHA3 -> "Shash"
  | ADDRESS -> "Scall0 Baddress"
  | BALANCE -> "Sbalance"
  | ORIGIN -> "Scall0 Borigin"
  | CALLER -> "Scall0 Bcaller"
  | CALLVALUE -> "Scall0 Bcallvalue"
  | CALLDATALOAD -> "Scalldataload"
  | CALLDATASIZE -> "Scalldatasize"
  | CALLDATACOPY -> "Scalldatacopy"
  | CODESIZE -> "Scodesize"
  | CODECOPY -> "Scodecopy"
  | GASPRICE -> "Sgasprice"
  | EXTCODESIZE -> "Sextcodesize"
  | EXTCODECOPY -> "Sextcodecopy"
  | RETURNDATASIZE -> "Sreturndatasize"
  | RETURNDATACOPY -> "Sreturndatacopy"
  | EXTCODEHASH -> "Sextcodehash"
  | BLOCKHASH -> "Sblockhash"
  | COINBASE -> "Scall0 Bcoinbase"
  | TIMESTAMP -> "Scall0 Btimestamp"
  | NUMBER -> "Scall0 Bnumber"
  | DIFFICULTY -> "Sdifficulty"
  | GASLIMIT -> "Sgaslimit"
  | POP -> "Spop"
  | MLOAD -> "Smload"
  | MSTORE -> "Smstore"
  | MSTORE8 -> "Smstores"
  | SLOAD -> "Ssload"
  | SSTORE -> "Ssstore"
  | JUMP -> "Sjump"
  | JUMPI -> "Sjumpi"
  | PC -> "Spc"
  | MSIZE -> "Smsize"
  | GAS -> "Sgas"
  | JUMPDEST -> "Sjumpdest"
  (* different cases for push args *)
  | PUSH (Word (Val v)) -> "Spush " ^ to_var_for_int nn v
  | PUSH (Word (Const c)) -> "Spush " ^ to_distinct_variables vn c
  | PUSH x -> "Spush " ^  Pusharg.show x
  (* index for DUP/SWAP starts at 0 for DeepSea Compiler *)
  | DUP idx -> "Sdup " ^  [%show: int] ((Instruction.idx_to_enum idx) - 1)
  | SWAP idx -> "Sswap " ^ [%show: int] ((Instruction.idx_to_enum idx) - 1)
  | LOG0 -> "Slog0"
  | LOG1 -> "Slog1"
  | LOG2 -> "Slog2"
  | LOG3 -> "Slog3"
  | LOG4 -> "Slog4"
  | JUMPTO -> "Sjumpto"
  | JUMPIF -> "Sjumpif"
  | JUMPV -> "Sjumpv"
  | JUMPSUB -> "Sjumpsub"
  | JUMPSUBV -> "Sjumpsubv"
  | BEGINSUB -> "Sbeginsub"
  | BEGINDATA -> "Sbegindata"
  | RETURNSUB -> "Sretunrsub"
  | PUTLOCAL -> "Sputlocal"
  | GETLOCAL -> "Sgetlocal"
  | CREATE -> "Screate"
  | CALL -> "Scall"
  | CALLCODE -> "Scallcode"
  | RETURN -> "Sreturn"
  | DELEGATECALL -> "Sdelegatecall"
  | CREATE2 -> "Screate2"
  | STATICCALL -> "Sstaticcall"
  | REVERT -> "Srevert"
  | INVALID -> "Sinvalid"
  | SELFDESTRUCT -> "Sselfdestruct"

let to_dsc vn nn t =
  List.map ~f:(instr_to_dsc vn nn) t
  |> String.concat ~sep:" :: "

(* header *)

let imports =
  "Require Import Statements.StmtExpressionless.
Require Import backend.phase.Expressionless.Semantics.
Require Import backend.phase.Expressionless.OptimizeLemmas.
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

"

let disclaimer =
  "(** AUTOGENERATED do not change! **)

  (* The rules and proofs are autogenerated from
  https://github.com/mariaschett/ppltr/blob/master/eval/17-reduced-rules.csv

  The rule numbers correspond to the line in the csv.
  *)

"

let section_heading =
"
Section WITH_DATA.
  Context adata {data_ops: CompatDataOps adata}.
  Variable ge: genv.

  Variable me: machine_env adata.
  Variable cd: list val.
  Variable funcKind : function_kind.

"

let header = imports ^ disclaimer ^ section_heading

let footer = "End WITH_DATA."
