; PUSH cw_4 PUSH cw_3 PUSH cw_3 SLOAD SWAP1 SWAP2 => PUSH cw_3 DUP1 SLOAD PUSH cw_4
(set-option :produce-proofs true)
; 
(set-info :status unknown)
(declare-fun storage_t ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 256)) (_ BitVec 256))
(declare-fun w_3 () (_ BitVec 256))
(declare-fun w_4 () (_ BitVec 256))
(declare-fun storage_s ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 256)) (_ BitVec 256))
(declare-fun exc_halt_t (Int) Bool)
(declare-fun exc_halt_s (Int) Bool)
(declare-fun stack_t ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 6)) (_ BitVec 256))
(declare-fun stack_s ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 6)) (_ BitVec 256))
(declare-fun sc_t (Int) (_ BitVec 6))
(declare-fun sc_s (Int) (_ BitVec 6))
(declare-fun used_gas_t ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int) Int)
(declare-fun used_gas_s ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int) Int)
(assert
 (exists ((x_SLOAD_0 (_ BitVec 256)) )(let (($x5850 (forall ((w (_ BitVec 256)) )(let ((?x5727 (storage_t x_SLOAD_0 w_4 w_3 4 w)))
 (let ((?x8835 (storage_s x_SLOAD_0 w_4 w_3 6 w)))
 (= ?x8835 ?x5727))))
 ))
 (let (($x3723 (exc_halt_t 4)))
 (let (($x772 (exc_halt_s 6)))
 (let (($x4774 (= $x772 $x3723)))
 (let (($x1120 (forall ((n (_ BitVec 6)) )(let ((?x5914 (stack_t x_SLOAD_0 w_4 w_3 4 n)))
 (let ((?x7831 (stack_s x_SLOAD_0 w_4 w_3 6 n)))
 (let (($x7480 (= ?x7831 ?x5914)))
 (let ((?x3757 (sc_t 4)))
 (let (($x6104 (bvsle ?x3757 n)))
 (or $x6104 $x7480)))))))
 ))
 (let ((?x3757 (sc_t 4)))
 (let ((?x926 (sc_s 6)))
 (let (($x5923 (= ?x926 ?x3757)))
 (let ((?x8099 (used_gas_t x_SLOAD_0 w_4 w_3 0)))
 (let ((?x5497 (used_gas_s x_SLOAD_0 w_4 w_3 0)))
 (let (($x1286 (= ?x5497 ?x8099)))
 (let (($x7363 (forall ((w (_ BitVec 256)) )(let ((?x5399 (storage_t x_SLOAD_0 w_4 w_3 0 w)))
 (let ((?x8206 (storage_s x_SLOAD_0 w_4 w_3 0 w)))
 (= ?x8206 ?x5399))))
 ))
 (let (($x56 (exc_halt_t 0)))
 (let (($x57 (exc_halt_s 0)))
 (let (($x58 (= $x57 $x56)))
 (let (($x5543 (forall ((n (_ BitVec 6)) )(let ((?x63 (sc_t 0)))
 (let (($x4561 (bvsle ?x63 n)))
 (let ((?x6725 (stack_t x_SLOAD_0 w_4 w_3 0 n)))
 (let ((?x7879 (stack_s x_SLOAD_0 w_4 w_3 0 n)))
 (let (($x1385 (= ?x7879 ?x6725)))
 (or $x1385 $x4561)))))))
 ))
 (let ((?x63 (sc_t 0)))
 (let ((?x72 (sc_s 0)))
 (let (($x73 (= ?x72 ?x63)))
 (let (($x6306 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_t 3)))) (_ bv0 1)))))
 (let (($x10336 (exc_halt_t 3)))
 (let (($x1199 (forall ((w (_ BitVec 256)) )(let ((?x5406 (storage_t x_SLOAD_0 w_4 w_3 3 w)))
 (let ((?x5727 (storage_t x_SLOAD_0 w_4 w_3 4 w)))
 (= ?x5727 ?x5406))))
 ))
 (let (($x2892 (forall ((n (_ BitVec 6)) )(let ((?x2012 (sc_t 3)))
 (let (($x7694 (bvsle ?x2012 n)))
 (let ((?x9217 (stack_t x_SLOAD_0 w_4 w_3 3 n)))
 (let ((?x5914 (stack_t x_SLOAD_0 w_4 w_3 4 n)))
 (or (= ?x5914 ?x9217) $x7694))))))
 ))
 (let (($x6790 (= ?x3757 (bvadd (_ bv1 6) (sc_t 3)))))
 (let (($x1427 (= (used_gas_t x_SLOAD_0 w_4 w_3 4) (+ 3 (used_gas_t x_SLOAD_0 w_4 w_3 3)))))
 (let (($x3760 (= $x10336 (or (exc_halt_t 2) (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_t 2))))))))
 (let (($x422 (forall ((w (_ BitVec 256)) )(let ((?x2029 (storage_t x_SLOAD_0 w_4 w_3 2 w)))
 (let ((?x5406 (storage_t x_SLOAD_0 w_4 w_3 3 w)))
 (= ?x5406 ?x2029))))
 ))
 (let (($x1273 (forall ((n (_ BitVec 6)) )(let ((?x1912 (stack_t x_SLOAD_0 w_4 w_3 2 n)))
 (let ((?x9217 (stack_t x_SLOAD_0 w_4 w_3 3 n)))
 (or (= ?x9217 ?x1912) (bvsle (bvadd (_ bv63 6) (sc_t 2)) n)))))
 ))
 (let ((?x4056 (sc_t 2)))
 (let ((?x2012 (sc_t 3)))
 (let (($x6401 (= ?x2012 ?x4056)))
 (let ((?x5654 (used_gas_t x_SLOAD_0 w_4 w_3 3)))
 (let ((?x6390 (bvadd (_ bv63 6) ?x4056)))
 (let ((?x3509 (stack_t x_SLOAD_0 w_4 w_3 2 ?x6390)))
 (let (($x1775 (= (stack_t x_SLOAD_0 w_4 w_3 3 (bvadd (_ bv63 6) ?x2012)) (storage_t x_SLOAD_0 w_4 w_3 2 ?x3509))))
 (let (($x5393 (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_t 1))))))
 (let (($x1920 (exc_halt_t 1)))
 (let (($x390 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_t 1)))) (_ bv0 1)))))
 (let (($x903 (exc_halt_t 2)))
 (let (($x6415 (= $x903 (or $x390 $x1920 $x5393))))
 (let (($x2879 (forall ((w (_ BitVec 256)) )(let ((?x8867 (storage_t x_SLOAD_0 w_4 w_3 1 w)))
 (let ((?x2029 (storage_t x_SLOAD_0 w_4 w_3 2 w)))
 (= ?x2029 ?x8867))))
 ))
 (let (($x8350 (forall ((n (_ BitVec 6)) )(let ((?x4023 (sc_t 1)))
 (let ((?x7954 (bvadd (_ bv63 6) ?x4023)))
 (let (($x7853 (bvsle ?x7954 n)))
 (let ((?x959 (stack_t x_SLOAD_0 w_4 w_3 1 n)))
 (let ((?x1912 (stack_t x_SLOAD_0 w_4 w_3 2 n)))
 (or (= ?x1912 ?x959) $x7853)))))))
 ))
 (let (($x8461 (= ?x4056 (bvadd (_ bv1 6) (sc_t 1)))))
 (let ((?x1723 (used_gas_t x_SLOAD_0 w_4 w_3 2)))
 (let ((?x4023 (sc_t 1)))
 (let ((?x7954 (bvadd (_ bv63 6) ?x4023)))
 (let ((?x8778 (stack_t x_SLOAD_0 w_4 w_3 1 ?x7954)))
 (let (($x6187 (= $x1920 (or $x56 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x63))) (_ bv0 1)))))))
 (let (($x2401 (forall ((w (_ BitVec 256)) )(let ((?x5399 (storage_t x_SLOAD_0 w_4 w_3 0 w)))
 (let ((?x8867 (storage_t x_SLOAD_0 w_4 w_3 1 w)))
 (= ?x8867 ?x5399))))
 ))
 (let (($x2384 (forall ((n (_ BitVec 6)) )(let ((?x6725 (stack_t x_SLOAD_0 w_4 w_3 0 n)))
 (let ((?x959 (stack_t x_SLOAD_0 w_4 w_3 1 n)))
 (let ((?x63 (sc_t 0)))
 (let (($x4561 (bvsle ?x63 n)))
 (or $x4561 (= ?x959 ?x6725)))))))
 ))
 (let (($x8795 (= ?x4023 (bvadd (_ bv1 6) ?x63))))
 (let (($x2517 (= $x772 (or (exc_halt_s 5) (not (bvsle (_ bv0 6) (bvadd (_ bv61 6) (sc_s 5))))))))
 (let (($x9674 (forall ((w (_ BitVec 256)) )(let ((?x6116 (storage_s x_SLOAD_0 w_4 w_3 5 w)))
 (let ((?x8835 (storage_s x_SLOAD_0 w_4 w_3 6 w)))
 (= ?x8835 ?x6116))))
 ))
 (let (($x9004 (forall ((n (_ BitVec 6)) )(let ((?x805 (sc_s 5)))
 (let ((?x10112 (bvadd (_ bv61 6) ?x805)))
 (let (($x5469 (bvsle ?x10112 n)))
 (let ((?x3517 (stack_s x_SLOAD_0 w_4 w_3 5 n)))
 (let ((?x7831 (stack_s x_SLOAD_0 w_4 w_3 6 n)))
 (or (= ?x7831 ?x3517) $x5469)))))))
 ))
 (let ((?x805 (sc_s 5)))
 (let (($x3879 (= ?x926 ?x805)))
 (let (($x2153 (= (used_gas_s x_SLOAD_0 w_4 w_3 6) (+ 3 (used_gas_s x_SLOAD_0 w_4 w_3 5)))))
 (let ((?x6337 (bvadd (_ bv62 6) ?x805)))
 (let ((?x625 (stack_s x_SLOAD_0 w_4 w_3 5 ?x6337)))
 (let ((?x10909 (bvadd (_ bv63 6) ?x805)))
 (let ((?x8959 (stack_s x_SLOAD_0 w_4 w_3 5 ?x10909)))
 (let (($x1508 (= (stack_s x_SLOAD_0 w_4 w_3 6 (bvadd (_ bv63 6) ?x926)) (stack_s x_SLOAD_0 w_4 w_3 5 (bvadd (_ bv61 6) ?x805)))))
 (let (($x3979 (exc_halt_s 5)))
 (let (($x4848 (= $x3979 (or (exc_halt_s 4) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 4))))))))
 (let (($x9292 (forall ((w (_ BitVec 256)) )(let ((?x5893 (storage_s x_SLOAD_0 w_4 w_3 4 w)))
 (let ((?x6116 (storage_s x_SLOAD_0 w_4 w_3 5 w)))
 (= ?x6116 ?x5893))))
 ))
 (let (($x8608 (forall ((n (_ BitVec 6)) )(let ((?x4305 (sc_s 4)))
 (let ((?x803 (bvadd (_ bv62 6) ?x4305)))
 (let (($x10926 (bvsle ?x803 n)))
 (let ((?x5717 (stack_s x_SLOAD_0 w_4 w_3 4 n)))
 (let ((?x3517 (stack_s x_SLOAD_0 w_4 w_3 5 n)))
 (or (= ?x3517 ?x5717) $x10926)))))))
 ))
 (let ((?x4305 (sc_s 4)))
 (let (($x3284 (= ?x805 ?x4305)))
 (let ((?x1513 (used_gas_s x_SLOAD_0 w_4 w_3 5)))
 (let (($x64 (exc_halt_s 4)))
 (let (($x8204 (= $x64 (or (exc_halt_s 3) (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_s 3))))))))
 (let (($x2899 (forall ((w (_ BitVec 256)) )(let ((?x8785 (storage_s x_SLOAD_0 w_4 w_3 3 w)))
 (let ((?x5893 (storage_s x_SLOAD_0 w_4 w_3 4 w)))
 (= ?x5893 ?x8785))))
 ))
 (let (($x9245 (forall ((n (_ BitVec 6)) )(let ((?x5883 (stack_s x_SLOAD_0 w_4 w_3 3 n)))
 (let ((?x5717 (stack_s x_SLOAD_0 w_4 w_3 4 n)))
 (let ((?x275 (sc_s 3)))
 (let ((?x11143 (bvadd (_ bv63 6) ?x275)))
 (let (($x2455 (bvsle ?x11143 n)))
 (or $x2455 (= ?x5717 ?x5883))))))))
 ))
 (let ((?x275 (sc_s 3)))
 (let (($x5422 (= ?x4305 ?x275)))
 (let ((?x5905 (used_gas_s x_SLOAD_0 w_4 w_3 4)))
 (let ((?x1962 (storage_s x_SLOAD_0 w_4 w_3 3 (stack_s x_SLOAD_0 w_4 w_3 3 (bvadd (_ bv63 6) ?x275)))))
 (let ((?x10988 (bvadd (_ bv63 6) ?x4305)))
 (let ((?x5499 (stack_s x_SLOAD_0 w_4 w_3 4 ?x10988)))
 (let (($x247 (exc_halt_s 2)))
 (let (($x9626 (or $x247 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 2)))) (_ bv0 1))))))
 (let (($x292 (exc_halt_s 3)))
 (let (($x6647 (= $x292 $x9626)))
 (let (($x5542 (forall ((w (_ BitVec 256)) )(let ((?x7623 (storage_s x_SLOAD_0 w_4 w_3 2 w)))
 (let ((?x8785 (storage_s x_SLOAD_0 w_4 w_3 3 w)))
 (= ?x8785 ?x7623))))
 ))
 (let (($x2498 (forall ((n (_ BitVec 6)) )(let ((?x218 (sc_s 2)))
 (let (($x3598 (bvsle ?x218 n)))
 (let ((?x1347 (stack_s x_SLOAD_0 w_4 w_3 2 n)))
 (let ((?x5883 (stack_s x_SLOAD_0 w_4 w_3 3 n)))
 (or (= ?x5883 ?x1347) $x3598))))))
 ))
 (let (($x6679 (= ?x275 (bvadd (_ bv1 6) (sc_s 2)))))
 (let ((?x1345 (used_gas_s x_SLOAD_0 w_4 w_3 3)))
 (let (($x4770 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 1)))) (_ bv0 1)))))
 (let (($x189 (exc_halt_s 1)))
 (let (($x8759 (= $x247 (or $x189 $x4770))))
 (let (($x5751 (forall ((w (_ BitVec 256)) )(let ((?x9257 (storage_s x_SLOAD_0 w_4 w_3 1 w)))
 (let ((?x7623 (storage_s x_SLOAD_0 w_4 w_3 2 w)))
 (= ?x7623 ?x9257))))
 ))
 (let (($x5796 (forall ((n (_ BitVec 6)) )(let ((?x154 (sc_s 1)))
 (let (($x2286 (bvsle ?x154 n)))
 (let ((?x1344 (stack_s x_SLOAD_0 w_4 w_3 1 n)))
 (let ((?x1347 (stack_s x_SLOAD_0 w_4 w_3 2 n)))
 (or (= ?x1347 ?x1344) $x2286))))))
 ))
 (let ((?x218 (sc_s 2)))
 (let (($x10803 (= ?x218 (bvadd (_ bv1 6) (sc_s 1)))))
 (let ((?x2059 (used_gas_s x_SLOAD_0 w_4 w_3 2)))
 (let (($x5657 (= $x189 (or $x57 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x72))) (_ bv0 1)))))))
 (let (($x2096 (forall ((w (_ BitVec 256)) )(let ((?x8206 (storage_s x_SLOAD_0 w_4 w_3 0 w)))
 (let ((?x9257 (storage_s x_SLOAD_0 w_4 w_3 1 w)))
 (= ?x9257 ?x8206))))
 ))
 (let (($x5395 (forall ((n (_ BitVec 6)) )(let ((?x7879 (stack_s x_SLOAD_0 w_4 w_3 0 n)))
 (let ((?x1344 (stack_s x_SLOAD_0 w_4 w_3 1 n)))
 (let ((?x72 (sc_s 0)))
 (let (($x8907 (bvsle ?x72 n)))
 (or $x8907 (= ?x1344 ?x7879)))))))
 ))
 (let ((?x154 (sc_s 1)))
 (let (($x10964 (= ?x154 (bvadd (_ bv1 6) ?x72))))
 (let (($x4442 (forall ((w (_ BitVec 256)) )(let ((?x7692 (ite (= w (stack_s x_SLOAD_0 w_4 w_3 3 (bvadd (_ bv63 6) (sc_s 3)))) x_SLOAD_0 (_ bv0 256))))
 (let ((?x8206 (storage_s x_SLOAD_0 w_4 w_3 0 w)))
 (= ?x8206 ?x7692))))
 ))
 (let (($x8231 (= ?x5497 0)))
 (let (($x11171 (not $x57)))
 (let (($x136 (= ?x72 (_ bv0 6))))
 (and $x136 $x11171 $x8231 $x4442 (= (stack_s x_SLOAD_0 w_4 w_3 1 ?x72) w_4) (= (used_gas_s x_SLOAD_0 w_4 w_3 1) (+ 3 ?x5497)) $x10964 $x5395 $x2096 $x5657 (= (stack_s x_SLOAD_0 w_4 w_3 2 ?x154) w_3) (= ?x2059 (+ 3 (used_gas_s x_SLOAD_0 w_4 w_3 1))) $x10803 $x5796 $x5751 $x8759 (= (stack_s x_SLOAD_0 w_4 w_3 3 ?x218) w_3) (= ?x1345 (+ 3 ?x2059)) $x6679 $x2498 $x5542 $x6647 (= ?x5499 ?x1962) (= ?x5905 (+ 200 ?x1345)) $x5422 $x9245 $x2899 $x8204 (= ?x8959 (stack_s x_SLOAD_0 w_4 w_3 4 (bvadd (_ bv62 6) ?x4305))) (= ?x625 ?x5499) (= ?x1513 (+ 3 ?x5905)) $x3284 $x8608 $x9292 $x4848 $x1508 (= (stack_s x_SLOAD_0 w_4 w_3 6 (bvadd (_ bv61 6) ?x926)) ?x8959) (= (stack_s x_SLOAD_0 w_4 w_3 6 (bvadd (_ bv62 6) ?x926)) ?x625) $x2153 $x3879 $x9004 $x9674 $x2517 (= (stack_t x_SLOAD_0 w_4 w_3 1 ?x63) w_3) (= (used_gas_t x_SLOAD_0 w_4 w_3 1) (+ 3 ?x8099)) $x8795 $x2384 $x2401 $x6187 (= ?x3509 ?x8778) (= (stack_t x_SLOAD_0 w_4 w_3 2 ?x7954) ?x8778) (= ?x1723 (+ 3 (used_gas_t x_SLOAD_0 w_4 w_3 1))) $x8461 $x8350 $x2879 $x6415 $x1775 (= ?x5654 (+ 200 ?x1723)) $x6401 $x1273 $x422 $x3760 (= (stack_t x_SLOAD_0 w_4 w_3 4 ?x2012) w_4) $x1427 $x6790 $x2892 $x1199 (= $x3723 (or $x10336 $x6306)) $x73 $x5543 $x58 $x7363 $x1286 (not (and $x5923 $x1120 $x4774 $x5850))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
 )
(check-sat)
(get-proof)