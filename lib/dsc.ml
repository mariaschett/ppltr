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

let to_var_for_int vnm v =
  let i = val_to_int256 v in
  let x = "n" ^ (Z.format "d" (Z.of_string v)) in
  match vnm with
  | None -> i
  | Some vnm -> Hashtbl.set vnm ~key:x ~data:[i]; x

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

let instr_to_dsc vnm = function
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
  | ADDRESS -> "Saddress"
  | BALANCE -> "Sbalance"
  | ORIGIN -> "Sorigin"
  | CALLER -> "Scaller"
  | CALLVALUE -> "Scallvalue"
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
  | COINBASE -> "Scoinbase"
  | TIMESTAMP -> "Stimestamp"
  | NUMBER -> "Snumber"
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
  | PUSH (Word (Val v)) -> "Spush " ^ to_var_for_int vnm v
  | PUSH (Word (Const c)) -> "Spush " ^ to_distinct_variables vnm c
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

let to_dsc vn t =
  List.map ~f:(instr_to_dsc vn) t
  |> String.concat ~sep:" :: "
