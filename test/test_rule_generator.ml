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
open OUnit2
open Core
open Ppltr
open Rule_generator
module Instruction = Ebso.Instruction
open Instruction.T

let suite =
  "suite" >:::
  [
    "program contains instruction" >::(fun _ ->
        assert_bool ""
          (contains [ADD; MUL; PUSH Tmpl; POP; SUB] ADD)
      );

    "program does not contain instruction" >::(fun _ ->
        assert_bool ""
          (not (contains [ADD; MUL; PUSH Tmpl; POP;] SUB))
      );

    "empty program does not contain any instruction" >::(fun _ ->
        assert_bool ""
          (not (contains [] (PUSH (Word (Val "1")))))
      );

    "program contains instruction PUSH with same word" >::(fun _ ->
        assert_bool ""
          (contains [PUSH (Word (Val "1"))] (PUSH (Word (Val "1"))))
      );

    "program contains instruction PUSH with different words" >::(fun _ ->
        assert_bool ""
          (not (contains [PUSH (Word (Val "1"))] (PUSH (Word (Val "2")))))
      );

    "program contains instruction PUSH with same variables" >::(fun _ ->
        assert_bool ""
          (contains [PUSH (Word (Const "x"))] (PUSH (Word (Const "x"))))
      );

    "program contains instruction PUSH with different variables" >::(fun _ ->
        assert_bool ""
          (not (contains [PUSH (Word (Const "x"))] (PUSH (Word (Const "y")))))
      );

    "instructions are equal">:: (fun _ ->
        let p = [ADD; SUB] in
        assert_equal
          ~cmp:[%eq: Instruction.t list]
          ~printer:[%show: Instruction.t list]
          [] (diff_instr p p)
      );

    "first instructions are empty">:: (fun _ ->
        let p = [ADD; SUB] in
        assert_equal
          ~cmp:[%eq:Instruction.t list]
          ~printer:[%show: Instruction.t list]
          p (diff_instr [] p)
      );

    "snd instructions are empty">:: (fun _ ->
        let p = [ADD; SUB] in
        assert_equal
          ~cmp:[%eq:Instruction.t list]
          ~printer:[%show: Instruction.t list]
          [] (diff_instr p [])
      );

    "instructions occurs twice">:: (fun _ ->
        assert_equal
          ~cmp:[%eq:Instruction.t list]
          ~printer:[%show: Instruction.t list]
          [ADD] (diff_instr [ADD] [ADD; ADD])
      );

    "SWAPs are identified">:: (fun _ ->
        assert_equal
          ~cmp:[%eq:Instruction.t list]
          ~printer:[%show: Instruction.t list]
          [] (diff_instr [SWAP I] [SWAP II])
      );

    "DUPs are identified">:: (fun _ ->
        assert_equal
          ~cmp:[%eq:Instruction.t list]
          ~printer:[%show: Instruction.t list]
          [] (diff_instr [DUP I] [DUP III])
      );

    "PUSHs are identified">:: (fun _ ->
        assert_equal
          ~cmp:[%eq:Instruction.t list]
          ~printer:[%show: Instruction.t list]
          [] (diff_instr [PUSH (Word (Val "1"))] [PUSH (Word (Val "2"))])
      );

    "PUSHs are identified">:: (fun _ ->
        assert_equal
          ~cmp:[%eq:Instruction.t list]
          ~printer:[%show: Instruction.t list]
          [] (diff_instr [PUSH (Word (Const "x"))] [PUSH (Word (Const "y"))])
      );

    "SWAPs are identified for multiple SWAPs">:: (fun _ ->
        let i = diff_instr [SWAP II] [SWAP I; SWAP III] |> List.hd_exn in
        assert_bool "" (i = SWAP I || i = SWAP III)
      );
  ]

let () =
  run_test_tt_main suite
