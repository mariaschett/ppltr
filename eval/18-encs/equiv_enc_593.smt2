; SLOAD CALLVALUE SWAP1 PUSH cw_1 SWAP1 => PUSH cw_1 CALLVALUE SWAP2 SLOAD
(set-option :produce-proofs true)
; 
(set-info :status unknown)
(declare-fun storage_t ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 256)) (_ BitVec 256))
(declare-fun w_1 () (_ BitVec 256))
(declare-fun storage_s ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 256)) (_ BitVec 256))
(declare-fun exc_halt_t (Int) Bool)
(declare-fun exc_halt_s (Int) Bool)
(declare-fun sc_t (Int) (_ BitVec 6))
(declare-fun stack_t ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 6)) (_ BitVec 256))
(declare-fun stack_s ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 6)) (_ BitVec 256))
(declare-fun sc_s (Int) (_ BitVec 6))
(declare-fun used_gas_t ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int) Int)
(declare-fun used_gas_s ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int) Int)
(assert
 (exists ((x_0 (_ BitVec 256)) (x_SLOAD_0 (_ BitVec 256)) (x_CALLVALUE (_ BitVec 256)) )(let (($x9912 (forall ((w (_ BitVec 256)) )(let ((?x8803 (storage_t x_0 x_SLOAD_0 w_1 x_CALLVALUE 4 w)))
 (let ((?x184 (storage_s x_0 x_SLOAD_0 w_1 x_CALLVALUE 5 w)))
 (= ?x184 ?x8803))))
 ))
 (let (($x7854 (exc_halt_t 4)))
 (let (($x11317 (exc_halt_s 5)))
 (let (($x11994 (= $x11317 $x7854)))
 (let (($x80 (forall ((n (_ BitVec 6)) )(let ((?x4818 (sc_t 4)))
 (let (($x7387 (bvsle ?x4818 n)))
 (let ((?x5094 (stack_t x_0 x_SLOAD_0 w_1 x_CALLVALUE 4 n)))
 (let ((?x5715 (stack_s x_0 x_SLOAD_0 w_1 x_CALLVALUE 5 n)))
 (let (($x5777 (= ?x5715 ?x5094)))
 (or $x5777 $x7387)))))))
 ))
 (let ((?x4818 (sc_t 4)))
 (let ((?x4319 (sc_s 5)))
 (let (($x10208 (= ?x4319 ?x4818)))
 (let ((?x5480 (used_gas_t x_0 x_SLOAD_0 w_1 x_CALLVALUE 0)))
 (let ((?x5984 (used_gas_s x_0 x_SLOAD_0 w_1 x_CALLVALUE 0)))
 (let (($x11837 (= ?x5984 ?x5480)))
 (let (($x9485 (forall ((w (_ BitVec 256)) )(let ((?x9931 (storage_t x_0 x_SLOAD_0 w_1 x_CALLVALUE 0 w)))
 (let ((?x3923 (storage_s x_0 x_SLOAD_0 w_1 x_CALLVALUE 0 w)))
 (= ?x3923 ?x9931))))
 ))
 (let (($x56 (exc_halt_t 0)))
 (let (($x57 (exc_halt_s 0)))
 (let (($x58 (= $x57 $x56)))
 (let (($x10687 (forall ((n (_ BitVec 6)) )(let ((?x63 (sc_t 0)))
 (let (($x10756 (bvsle ?x63 n)))
 (let ((?x5189 (stack_t x_0 x_SLOAD_0 w_1 x_CALLVALUE 0 n)))
 (let ((?x1422 (stack_s x_0 x_SLOAD_0 w_1 x_CALLVALUE 0 n)))
 (let (($x4281 (= ?x1422 ?x5189)))
 (or $x4281 $x10756)))))))
 ))
 (let ((?x63 (sc_t 0)))
 (let ((?x72 (sc_s 0)))
 (let (($x73 (= ?x72 ?x63)))
 (let (($x10021 (= $x7854 (or (exc_halt_t 3) (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_t 3))))))))
 (let (($x9576 (forall ((w (_ BitVec 256)) )(let ((?x289 (storage_t x_0 x_SLOAD_0 w_1 x_CALLVALUE 3 w)))
 (let ((?x8803 (storage_t x_0 x_SLOAD_0 w_1 x_CALLVALUE 4 w)))
 (= ?x8803 ?x289))))
 ))
 (let (($x1814 (forall ((n (_ BitVec 6)) )(let ((?x10197 (stack_t x_0 x_SLOAD_0 w_1 x_CALLVALUE 3 n)))
 (let ((?x5094 (stack_t x_0 x_SLOAD_0 w_1 x_CALLVALUE 4 n)))
 (or (bvsle (bvadd (_ bv63 6) (sc_t 3)) n) (= ?x5094 ?x10197)))))
 ))
 (let ((?x11304 (sc_t 3)))
 (let (($x8932 (= ?x4818 ?x11304)))
 (let (($x6266 (= (used_gas_t x_0 x_SLOAD_0 w_1 x_CALLVALUE 4) (+ 200 (used_gas_t x_0 x_SLOAD_0 w_1 x_CALLVALUE 3)))))
 (let ((?x8209 (bvadd (_ bv63 6) ?x11304)))
 (let ((?x7612 (stack_t x_0 x_SLOAD_0 w_1 x_CALLVALUE 3 ?x8209)))
 (let (($x9748 (= (stack_t x_0 x_SLOAD_0 w_1 x_CALLVALUE 4 (bvadd (_ bv63 6) ?x4818)) (storage_t x_0 x_SLOAD_0 w_1 x_CALLVALUE 3 ?x7612))))
 (let (($x3614 (exc_halt_t 3)))
 (let (($x5967 (= $x3614 (or (exc_halt_t 2) (not (bvsle (_ bv0 6) (bvadd (_ bv61 6) (sc_t 2))))))))
 (let (($x7520 (forall ((w (_ BitVec 256)) )(let ((?x786 (storage_t x_0 x_SLOAD_0 w_1 x_CALLVALUE 2 w)))
 (let ((?x289 (storage_t x_0 x_SLOAD_0 w_1 x_CALLVALUE 3 w)))
 (= ?x289 ?x786))))
 ))
 (let (($x8382 (forall ((n (_ BitVec 6)) )(let ((?x2714 (sc_t 2)))
 (let ((?x6623 (bvadd (_ bv61 6) ?x2714)))
 (let (($x10514 (bvsle ?x6623 n)))
 (let ((?x1547 (stack_t x_0 x_SLOAD_0 w_1 x_CALLVALUE 2 n)))
 (let ((?x10197 (stack_t x_0 x_SLOAD_0 w_1 x_CALLVALUE 3 n)))
 (or (= ?x10197 ?x1547) $x10514)))))))
 ))
 (let ((?x2714 (sc_t 2)))
 (let (($x9044 (= ?x11304 ?x2714)))
 (let ((?x10350 (used_gas_t x_0 x_SLOAD_0 w_1 x_CALLVALUE 3)))
 (let (($x8166 (= (stack_t x_0 x_SLOAD_0 w_1 x_CALLVALUE 3 (bvadd (_ bv62 6) ?x11304)) (stack_t x_0 x_SLOAD_0 w_1 x_CALLVALUE 2 (bvadd (_ bv62 6) ?x2714)))))
 (let (($x5471 (= (stack_t x_0 x_SLOAD_0 w_1 x_CALLVALUE 3 (bvadd (_ bv61 6) ?x11304)) (stack_t x_0 x_SLOAD_0 w_1 x_CALLVALUE 2 (bvadd (_ bv63 6) ?x2714)))))
 (let (($x8377 (exc_halt_t 1)))
 (let (($x6611 (or $x8377 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_t 1)))) (_ bv0 1))))))
 (let (($x4500 (exc_halt_t 2)))
 (let (($x1605 (= $x4500 $x6611)))
 (let (($x5173 (forall ((w (_ BitVec 256)) )(let ((?x92 (storage_t x_0 x_SLOAD_0 w_1 x_CALLVALUE 1 w)))
 (let ((?x786 (storage_t x_0 x_SLOAD_0 w_1 x_CALLVALUE 2 w)))
 (= ?x786 ?x92))))
 ))
 (let (($x6555 (forall ((n (_ BitVec 6)) )(let ((?x7154 (sc_t 1)))
 (let (($x1943 (bvsle ?x7154 n)))
 (let ((?x10198 (stack_t x_0 x_SLOAD_0 w_1 x_CALLVALUE 1 n)))
 (let ((?x1547 (stack_t x_0 x_SLOAD_0 w_1 x_CALLVALUE 2 n)))
 (or (= ?x1547 ?x10198) $x1943))))))
 ))
 (let (($x10940 (= ?x2714 (bvadd (_ bv1 6) (sc_t 1)))))
 (let ((?x4971 (used_gas_t x_0 x_SLOAD_0 w_1 x_CALLVALUE 2)))
 (let (($x8120 (= $x8377 (or $x56 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x63))) (_ bv0 1)))))))
 (let (($x8023 (forall ((w (_ BitVec 256)) )(let ((?x9931 (storage_t x_0 x_SLOAD_0 w_1 x_CALLVALUE 0 w)))
 (let ((?x92 (storage_t x_0 x_SLOAD_0 w_1 x_CALLVALUE 1 w)))
 (= ?x92 ?x9931))))
 ))
 (let (($x10550 (forall ((n (_ BitVec 6)) )(let ((?x63 (sc_t 0)))
 (let (($x10756 (bvsle ?x63 n)))
 (let ((?x5189 (stack_t x_0 x_SLOAD_0 w_1 x_CALLVALUE 0 n)))
 (let ((?x10198 (stack_t x_0 x_SLOAD_0 w_1 x_CALLVALUE 1 n)))
 (or (= ?x10198 ?x5189) $x10756))))))
 ))
 (let ((?x7154 (sc_t 1)))
 (let (($x3866 (= ?x7154 (bvadd (_ bv1 6) ?x63))))
 (let (($x9721 (= $x11317 (or (exc_halt_s 4) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 4))))))))
 (let (($x9295 (forall ((w (_ BitVec 256)) )(let ((?x578 (storage_s x_0 x_SLOAD_0 w_1 x_CALLVALUE 4 w)))
 (let ((?x184 (storage_s x_0 x_SLOAD_0 w_1 x_CALLVALUE 5 w)))
 (= ?x184 ?x578))))
 ))
 (let (($x2298 (forall ((n (_ BitVec 6)) )(let ((?x4830 (stack_s x_0 x_SLOAD_0 w_1 x_CALLVALUE 4 n)))
 (let ((?x5715 (stack_s x_0 x_SLOAD_0 w_1 x_CALLVALUE 5 n)))
 (or (bvsle (bvadd (_ bv62 6) (sc_s 4)) n) (= ?x5715 ?x4830)))))
 ))
 (let ((?x4305 (sc_s 4)))
 (let (($x11749 (= ?x4319 ?x4305)))
 (let (($x1306 (= (used_gas_s x_0 x_SLOAD_0 w_1 x_CALLVALUE 5) (+ 3 (used_gas_s x_0 x_SLOAD_0 w_1 x_CALLVALUE 4)))))
 (let (($x11213 (= (stack_s x_0 x_SLOAD_0 w_1 x_CALLVALUE 5 (bvadd (_ bv62 6) ?x4319)) (stack_s x_0 x_SLOAD_0 w_1 x_CALLVALUE 4 (bvadd (_ bv63 6) ?x4305)))))
 (let (($x3210 (= (stack_s x_0 x_SLOAD_0 w_1 x_CALLVALUE 5 (bvadd (_ bv63 6) ?x4319)) (stack_s x_0 x_SLOAD_0 w_1 x_CALLVALUE 4 (bvadd (_ bv62 6) ?x4305)))))
 (let (($x292 (exc_halt_s 3)))
 (let (($x8756 (or $x292 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 3)))) (_ bv0 1))))))
 (let (($x7172 (exc_halt_s 4)))
 (let (($x467 (forall ((w (_ BitVec 256)) )(let ((?x3299 (storage_s x_0 x_SLOAD_0 w_1 x_CALLVALUE 3 w)))
 (let ((?x578 (storage_s x_0 x_SLOAD_0 w_1 x_CALLVALUE 4 w)))
 (= ?x578 ?x3299))))
 ))
 (let (($x4429 (forall ((n (_ BitVec 6)) )(let ((?x5487 (stack_s x_0 x_SLOAD_0 w_1 x_CALLVALUE 3 n)))
 (let ((?x4830 (stack_s x_0 x_SLOAD_0 w_1 x_CALLVALUE 4 n)))
 (or (= ?x4830 ?x5487) (bvsle (sc_s 3) n)))))
 ))
 (let ((?x4989 (used_gas_s x_0 x_SLOAD_0 w_1 x_CALLVALUE 4)))
 (let (($x4020 (= $x292 (or (exc_halt_s 2) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 2))))))))
 (let (($x10597 (forall ((w (_ BitVec 256)) )(let ((?x11275 (storage_s x_0 x_SLOAD_0 w_1 x_CALLVALUE 2 w)))
 (let ((?x3299 (storage_s x_0 x_SLOAD_0 w_1 x_CALLVALUE 3 w)))
 (= ?x3299 ?x11275))))
 ))
 (let (($x5976 (forall ((n (_ BitVec 6)) )(let ((?x3462 (stack_s x_0 x_SLOAD_0 w_1 x_CALLVALUE 2 n)))
 (let ((?x5487 (stack_s x_0 x_SLOAD_0 w_1 x_CALLVALUE 3 n)))
 (let ((?x218 (sc_s 2)))
 (let ((?x1134 (bvadd (_ bv62 6) ?x218)))
 (let (($x8883 (bvsle ?x1134 n)))
 (or $x8883 (= ?x5487 ?x3462))))))))
 ))
 (let ((?x218 (sc_s 2)))
 (let ((?x275 (sc_s 3)))
 (let (($x9337 (= ?x275 ?x218)))
 (let ((?x5754 (used_gas_s x_0 x_SLOAD_0 w_1 x_CALLVALUE 3)))
 (let (($x6144 (= (stack_s x_0 x_SLOAD_0 w_1 x_CALLVALUE 3 (bvadd (_ bv62 6) ?x275)) (stack_s x_0 x_SLOAD_0 w_1 x_CALLVALUE 2 (bvadd (_ bv63 6) ?x218)))))
 (let (($x4643 (= (stack_s x_0 x_SLOAD_0 w_1 x_CALLVALUE 3 (bvadd (_ bv63 6) ?x275)) (stack_s x_0 x_SLOAD_0 w_1 x_CALLVALUE 2 (bvadd (_ bv62 6) ?x218)))))
 (let (($x4355 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 1)))) (_ bv0 1)))))
 (let (($x189 (exc_halt_s 1)))
 (let (($x247 (exc_halt_s 2)))
 (let (($x3261 (= $x247 (or $x189 $x4355))))
 (let (($x10277 (forall ((w (_ BitVec 256)) )(let ((?x4031 (storage_s x_0 x_SLOAD_0 w_1 x_CALLVALUE 1 w)))
 (let ((?x11275 (storage_s x_0 x_SLOAD_0 w_1 x_CALLVALUE 2 w)))
 (= ?x11275 ?x4031))))
 ))
 (let (($x11665 (forall ((n (_ BitVec 6)) )(let ((?x154 (sc_s 1)))
 (let (($x5702 (bvsle ?x154 n)))
 (let ((?x5542 (stack_s x_0 x_SLOAD_0 w_1 x_CALLVALUE 1 n)))
 (let ((?x3462 (stack_s x_0 x_SLOAD_0 w_1 x_CALLVALUE 2 n)))
 (or (= ?x3462 ?x5542) $x5702))))))
 ))
 (let (($x4558 (= ?x218 (bvadd (_ bv1 6) (sc_s 1)))))
 (let ((?x4929 (used_gas_s x_0 x_SLOAD_0 w_1 x_CALLVALUE 2)))
 (let (($x1529 (forall ((w (_ BitVec 256)) )(let ((?x3923 (storage_s x_0 x_SLOAD_0 w_1 x_CALLVALUE 0 w)))
 (let ((?x4031 (storage_s x_0 x_SLOAD_0 w_1 x_CALLVALUE 1 w)))
 (= ?x4031 ?x3923))))
 ))
 (let (($x11032 (forall ((n (_ BitVec 6)) )(let ((?x1422 (stack_s x_0 x_SLOAD_0 w_1 x_CALLVALUE 0 n)))
 (let ((?x5542 (stack_s x_0 x_SLOAD_0 w_1 x_CALLVALUE 1 n)))
 (let ((?x72 (sc_s 0)))
 (let ((?x2515 (bvadd (_ bv63 6) ?x72)))
 (let (($x2898 (bvsle ?x2515 n)))
 (or $x2898 (= ?x5542 ?x1422))))))))
 ))
 (let ((?x4526 (storage_s x_0 x_SLOAD_0 w_1 x_CALLVALUE 0 (stack_s x_0 x_SLOAD_0 w_1 x_CALLVALUE 0 (bvadd (_ bv63 6) ?x72)))))
 (let (($x2376 (= (stack_s x_0 x_SLOAD_0 w_1 x_CALLVALUE 1 (bvadd (_ bv63 6) (sc_s 1))) ?x4526)))
 (let (($x8851 (forall ((w (_ BitVec 256)) )(let (($x5528 (= w (stack_s x_0 x_SLOAD_0 w_1 x_CALLVALUE 0 (bvadd (_ bv63 6) (sc_s 0))))))
 (let ((?x3923 (storage_s x_0 x_SLOAD_0 w_1 x_CALLVALUE 0 w)))
 (= ?x3923 (ite $x5528 x_SLOAD_0 (_ bv0 256))))))
 ))
 (let (($x6379 (= ?x5984 0)))
 (let (($x11625 (not $x57)))
 (let (($x11486 (= (stack_s x_0 x_SLOAD_0 w_1 x_CALLVALUE 0 (_ bv0 6)) x_0)))
 (let (($x7315 (= ?x72 (_ bv1 6))))
 (and $x7315 $x11486 $x11625 $x6379 $x8851 $x2376 (= (used_gas_s x_0 x_SLOAD_0 w_1 x_CALLVALUE 1) (+ 200 ?x5984)) (= (sc_s 1) ?x72) $x11032 $x1529 (= $x189 (or $x57 (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) ?x72))))) (= (stack_s x_0 x_SLOAD_0 w_1 x_CALLVALUE 2 (sc_s 1)) x_CALLVALUE) (= ?x4929 (+ 2 (used_gas_s x_0 x_SLOAD_0 w_1 x_CALLVALUE 1))) $x4558 $x11665 $x10277 $x3261 $x4643 $x6144 (= ?x5754 (+ 3 ?x4929)) $x9337 $x5976 $x10597 $x4020 (= (stack_s x_0 x_SLOAD_0 w_1 x_CALLVALUE 4 ?x275) w_1) (= ?x4989 (+ 3 ?x5754)) (= ?x4305 (bvadd (_ bv1 6) ?x275)) $x4429 $x467 (= $x7172 $x8756) $x3210 $x11213 $x1306 $x11749 $x2298 $x9295 $x9721 (= (stack_t x_0 x_SLOAD_0 w_1 x_CALLVALUE 1 ?x63) w_1) (= (used_gas_t x_0 x_SLOAD_0 w_1 x_CALLVALUE 1) (+ 3 ?x5480)) $x3866 $x10550 $x8023 $x8120 (= (stack_t x_0 x_SLOAD_0 w_1 x_CALLVALUE 2 ?x7154) x_CALLVALUE) (= ?x4971 (+ 2 (used_gas_t x_0 x_SLOAD_0 w_1 x_CALLVALUE 1))) $x10940 $x6555 $x5173 $x1605 (= ?x7612 (stack_t x_0 x_SLOAD_0 w_1 x_CALLVALUE 2 (bvadd (_ bv61 6) ?x2714))) $x5471 $x8166 (= ?x10350 (+ 3 ?x4971)) $x9044 $x8382 $x7520 $x5967 $x9748 $x6266 $x8932 $x1814 $x9576 $x10021 $x73 $x10687 $x58 $x9485 $x11837 (not (and $x10208 $x80 $x11994 $x9912))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
 )
(check-sat)
(get-proof)