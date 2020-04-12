open OUnit2
open Ppltr
open Blk_generator
open Ebso.Instruction.T

let suite =
  "suite" >:::
  [
    "test sliding window">:: (fun _ ->
        assert_equal ~cmp:[%eq: int list list ] ~printer:[%show: int list list ]
          [[1;2;3];[2;3;4];[3;4;5]] (sliding_window [1;2;3;4;5] 3)
      );

    "test sliding window -- small list">:: (fun _ ->
        assert_equal ~cmp:[%eq: int list list ] ~printer:[%show: int list list ]
          [[1;2]] (sliding_window [1;2] 3)
      );

    "test sliding window -- windows size 0">:: (fun _ ->
        assert_equal ~cmp:[%eq: int list list ] ~printer:[%show: int list list ]
          [[]] (sliding_window [1;2] 0)
      );

    "equiv mod wsz -- different arguments greater wsz" >:: (fun _ ->
        assert_bool "299 and 300 not identified"
          (equiv_mod_wsz [PUSH (Word (Val "299"))] [PUSH (Word (Val "300"))])
      );

    "equiv mod wsz -- same arguments greater wsz" >:: (fun _ ->
        assert_bool "299 and 299 not identified"
          (equiv_mod_wsz [PUSH (Word (Val "299"))] [PUSH (Word (Val "299"))])
      );

    "equiv mod wsz -- different arguments smaller wsz" >:: (fun _ ->
        assert_bool "3 and 1 identified"
          (not (equiv_mod_wsz [PUSH (Word (Val "3"))] [PUSH (Word (Val "1"))]))
      );

    "equiv mod wsz -- two PUSHs, same arguments greater wsz" >:: (fun _ ->
        let b1 = [PUSH (Word (Val "299")); PUSH (Word (Val "900"))] in
        let b2 = [PUSH (Word (Val "299")); PUSH (Word (Val "900"))] in
        assert_bool "299 900 and 299 900 not identified"
          (equiv_mod_wsz b1 b2)
      );

    "equiv mod wsz -- two PUSHs, one same argument greater wsz" >:: (fun _ ->
        let b1 = [PUSH (Word (Val "299")); PUSH (Word (Val "900"))] in
        let b2 = [PUSH (Word (Val "299")); PUSH (Word (Val "899"))] in
        assert_bool "299 900 and 299 899 not identified"
          (equiv_mod_wsz b1 b2)
      );

    "equiv mod wsz -- two PUSHs, different arguments" >:: (fun _ ->
        let b1 = [PUSH (Word (Val "900")); PUSH (Word (Val "900"))] in
        let b2 = [PUSH (Word (Val "299")); PUSH (Word (Val "299"))] in
        assert_bool "900 900 and 299 299 not identified"
          (equiv_mod_wsz b1 b2)
      );

    "equiv mod wsz -- two PUSHs, different arguments, one smaller wsz" >:: (fun _ ->
        let b1 = [PUSH (Word (Val "900")); PUSH (Word (Val "900"))] in
        let b2 = [PUSH (Word (Val "299")); PUSH (Word (Val "3"))] in
        assert_bool "900 900 and 299 3 identified"
          (not (equiv_mod_wsz b1 b2))
      );

  ]

let () =
  run_test_tt_main suite
