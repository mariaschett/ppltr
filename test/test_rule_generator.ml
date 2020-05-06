open OUnit2
open Ppltr
open Rule_generator
open Ebso.Instruction.T

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

  ]

let () =
  run_test_tt_main suite
