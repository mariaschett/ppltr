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
open Ppltr
open Sorg
open Rule
open Ebso
open Rewriter
open Ebso.Instruction.T

let suite =
  "suite" >:::
  [
    "test contexts, lhs length 1">:: (fun _ ->
        assert_equal
          ~cmp:[%eq: (Program.t * Ctxt.t) list]
          ~printer:[%show: (Program.t * Ctxt.t) list]
          [([ADD], ([], [SUB; MUL])); ([SUB], ([ADD], [MUL])); ([MUL], ([ADD; SUB], []))]
          (contexts_for_rule [ADD; SUB; MUL] {lhs = [ADD]; rhs = [ADD]})
      );

    "test contexts, lhs length 2">:: (fun _ ->
        assert_equal
          ~cmp:[%eq: (Program.t * Ctxt.t) list]
          ~printer:[%show: (Program.t * Ctxt.t) list]
          [([ADD; SUB], ([], [MUL])); ([SUB; MUL], ([ADD], []))]
          (contexts_for_rule [ADD; SUB; MUL] {lhs = [ADD; SUB]; rhs = [ADD]})
      );

    "test reducts for rule, no context" >:: (fun _ ->
        assert_equal
          ~cmp:[%eq: (Program.t) list]
          ~printer:[%show: (Program.t) list]
          [[]]
          (reducts_for_rule [PUSH (Word (Val "0")); ADD]
             {lhs = [PUSH (Word (Val "0")); ADD]; rhs = []})
      );

    "test reducts for rule, with context" >:: (fun _ ->
        assert_equal
          ~cmp:[%eq: (Program.t) list]
          ~printer:[%show: (Program.t) list]
          [[SUB; MUL]]
          (reducts_for_rule [SUB; PUSH (Word (Val "0")); ADD; MUL]
             {lhs = [PUSH (Word (Val "0")); ADD]; rhs = []})
      );

    "test reducts for rule, with subst" >:: (fun _ ->
        assert_equal
          ~cmp:[%eq: (Program.t) list]
          ~printer:[%show: (Program.t) list]
          [[SUB; MUL]]
          (reducts_for_rule [SUB; PUSH (Word (Val "0")); POP; MUL]
             {lhs = [PUSH (Word (Const "w1")); POP]; rhs = []})
      );

    "test reducts for rule, no match" >:: (fun _ ->
        assert_equal
          ~cmp:[%eq: (Program.t) list]
          ~printer:[%show: (Program.t) list]
          []
          (reducts_for_rule [SUB; PUSH (Word (Val "1")); ADD; MUL]
             {lhs = [PUSH (Word (Val "0")); ADD]; rhs = []})
      );

    "test reducts for duplicating rule" >:: (fun _ ->
        assert_equal
          ~cmp:[%eq: (Program.t) list]
          ~printer:[%show: (Program.t) list]
          [[SUB; PUSH (Word (Val "1")); PUSH (Word (Val "1")); MUL]]
          (reducts_for_rule [SUB; PUSH (Word (Val "1")); DUP I; MUL]
             {lhs = [PUSH (Word (Const "w1")); DUP I];
              rhs = [PUSH (Word (Const "w1")); PUSH (Word (Const "w1"))]})
      );

    "test reducts for non-left-linear rule" >:: (fun _ ->
        assert_equal
          ~cmp:[%eq: (Program.t) list]
          ~printer:[%show: (Program.t) list]
          [[SUB; PUSH (Word (Val "1")); DUP I; MUL]]
          (reducts_for_rule [SUB; PUSH (Word (Val "1")); PUSH (Word (Val "1")); MUL]
             {lhs = [PUSH (Word (Const "w1")); PUSH (Word (Const "w1"))];
              rhs = [PUSH (Word (Const "w1")); DUP I]})
      );

    "test reducts, multiple reducts" >:: (fun _ ->
        assert_equal
          ~cmp:[%eq: (Program.t) list]
          ~printer:[%show: (Program.t) list]
          [[NUMBER; NUMBER; SUB; NUMBER; DUP I]; [NUMBER; DUP I; SUB; NUMBER; NUMBER]]
          (reducts_for_rule [NUMBER; DUP I; SUB; NUMBER; DUP I]
             {lhs = [NUMBER; DUP I]; rhs = [NUMBER; NUMBER]})
      );

    "test normal forms, multiple reducts" >:: (fun _ ->
        assert_equal
          ~cmp:[%eq: (Program.t) list]
          ~printer:[%show: (Program.t) list]
          [[NUMBER; NUMBER; SUB; NUMBER; NUMBER]]
          (normal_forms all_reducts [NUMBER; DUP I; SUB; NUMBER; DUP I]
             [{lhs = [NUMBER; DUP I]; rhs = [NUMBER; NUMBER]}])
      );

    "test normal forms, no reducts" >:: (fun _ ->
        assert_equal
          ~cmp:[%eq: (Program.t) list]
          ~printer:[%show: (Program.t) list]
          [[NUMBER; SUB]]
          (normal_forms all_reducts [NUMBER; SUB]
             [{lhs = [NUMBER; DUP I]; rhs = [NUMBER; NUMBER]}])
      );

  ]

let () =
  run_test_tt_main suite
