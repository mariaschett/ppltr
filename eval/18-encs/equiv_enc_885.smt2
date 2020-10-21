; DUP1 SLOAD PUSH cw_1 SWAP1 DUP4 SWAP1 => PUSH cw_1 DUP3 DUP3 SLOAD
(set-option :produce-proofs true)
; 
(set-info :status unknown)
(declare-fun storage_t ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 256)) (_ BitVec 256))
(declare-fun w_1 () (_ BitVec 256))
(declare-fun storage_s ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 256)) (_ BitVec 256))
(declare-fun exc_halt_t (Int) Bool)
(declare-fun exc_halt_s (Int) Bool)
(declare-fun stack_t ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 6)) (_ BitVec 256))
(declare-fun stack_s ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 6)) (_ BitVec 256))
(declare-fun sc_t (Int) (_ BitVec 6))
(declare-fun sc_s (Int) (_ BitVec 6))
(declare-fun used_gas_t ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int) Int)
(declare-fun used_gas_s ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int) Int)
(assert
 (exists ((x_0 (_ BitVec 256)) (x_1 (_ BitVec 256)) (x_SLOAD_0 (_ BitVec 256)) )(let (($x1748 (forall ((w (_ BitVec 256)) )(let ((?x6202 (storage_t x_0 x_1 x_SLOAD_0 w_1 4 w)))
 (let ((?x3949 (storage_s x_0 x_1 x_SLOAD_0 w_1 6 w)))
 (= ?x3949 ?x6202))))
 ))
 (let (($x7854 (exc_halt_t 4)))
 (let (($x7121 (exc_halt_s 6)))
 (let (($x10095 (= $x7121 $x7854)))
 (let (($x8178 (forall ((n (_ BitVec 6)) )(let ((?x11803 (stack_t x_0 x_1 x_SLOAD_0 w_1 4 n)))
 (let ((?x6386 (stack_s x_0 x_1 x_SLOAD_0 w_1 6 n)))
 (let (($x11086 (= ?x6386 ?x11803)))
 (let ((?x7495 (sc_t 4)))
 (let (($x6247 (bvsle ?x7495 n)))
 (or $x6247 $x11086)))))))
 ))
 (let ((?x7495 (sc_t 4)))
 (let ((?x9114 (sc_s 6)))
 (let (($x2362 (= ?x9114 ?x7495)))
 (let ((?x1070 (used_gas_t x_0 x_1 x_SLOAD_0 w_1 0)))
 (let ((?x8857 (used_gas_s x_0 x_1 x_SLOAD_0 w_1 0)))
 (let (($x4950 (= ?x8857 ?x1070)))
 (let (($x9488 (forall ((w (_ BitVec 256)) )(let ((?x2181 (storage_t x_0 x_1 x_SLOAD_0 w_1 0 w)))
 (let ((?x3397 (storage_s x_0 x_1 x_SLOAD_0 w_1 0 w)))
 (= ?x3397 ?x2181))))
 ))
 (let (($x56 (exc_halt_t 0)))
 (let (($x57 (exc_halt_s 0)))
 (let (($x58 (= $x57 $x56)))
 (let (($x7861 (forall ((n (_ BitVec 6)) )(let ((?x9070 (stack_t x_0 x_1 x_SLOAD_0 w_1 0 n)))
 (let ((?x8722 (stack_s x_0 x_1 x_SLOAD_0 w_1 0 n)))
 (let (($x8382 (= ?x8722 ?x9070)))
 (let ((?x63 (sc_t 0)))
 (let (($x3918 (bvsle ?x63 n)))
 (or $x3918 $x8382)))))))
 ))
 (let ((?x63 (sc_t 0)))
 (let ((?x72 (sc_s 0)))
 (let (($x73 (= ?x72 ?x63)))
 (let (($x3615 (= $x7854 (or (exc_halt_t 3) (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_t 3))))))))
 (let (($x1077 (forall ((w (_ BitVec 256)) )(let ((?x8161 (storage_t x_0 x_1 x_SLOAD_0 w_1 3 w)))
 (let ((?x6202 (storage_t x_0 x_1 x_SLOAD_0 w_1 4 w)))
 (= ?x6202 ?x8161))))
 ))
 (let (($x1073 (forall ((n (_ BitVec 6)) )(let ((?x10013 (sc_t 3)))
 (let ((?x103 (bvadd (_ bv63 6) ?x10013)))
 (let (($x11408 (bvsle ?x103 n)))
 (let ((?x11022 (stack_t x_0 x_1 x_SLOAD_0 w_1 3 n)))
 (let ((?x11803 (stack_t x_0 x_1 x_SLOAD_0 w_1 4 n)))
 (or (= ?x11803 ?x11022) $x11408)))))))
 ))
 (let ((?x10013 (sc_t 3)))
 (let (($x1394 (= ?x7495 ?x10013)))
 (let (($x984 (= (used_gas_t x_0 x_1 x_SLOAD_0 w_1 4) (+ 200 (used_gas_t x_0 x_1 x_SLOAD_0 w_1 3)))))
 (let ((?x103 (bvadd (_ bv63 6) ?x10013)))
 (let ((?x1880 (stack_t x_0 x_1 x_SLOAD_0 w_1 3 ?x103)))
 (let (($x2578 (= (stack_t x_0 x_1 x_SLOAD_0 w_1 4 (bvadd (_ bv63 6) ?x7495)) (storage_t x_0 x_1 x_SLOAD_0 w_1 3 ?x1880))))
 (let (($x7582 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_t 2)))) (_ bv0 1)))))
 (let (($x6842 (exc_halt_t 2)))
 (let (($x9131 (exc_halt_t 3)))
 (let (($x5491 (forall ((w (_ BitVec 256)) )(let ((?x7279 (storage_t x_0 x_1 x_SLOAD_0 w_1 2 w)))
 (let ((?x8161 (storage_t x_0 x_1 x_SLOAD_0 w_1 3 w)))
 (= ?x8161 ?x7279))))
 ))
 (let (($x2595 (forall ((n (_ BitVec 6)) )(let ((?x5984 (stack_t x_0 x_1 x_SLOAD_0 w_1 2 n)))
 (let ((?x11022 (stack_t x_0 x_1 x_SLOAD_0 w_1 3 n)))
 (or (bvsle (bvadd (_ bv61 6) (sc_t 2)) n) (= ?x11022 ?x5984)))))
 ))
 (let (($x230 (= ?x10013 (bvadd (_ bv1 6) (sc_t 2)))))
 (let ((?x6116 (used_gas_t x_0 x_1 x_SLOAD_0 w_1 3)))
 (let ((?x9666 (sc_t 2)))
 (let ((?x2733 (bvadd (_ bv63 6) ?x9666)))
 (let ((?x3194 (stack_t x_0 x_1 x_SLOAD_0 w_1 2 ?x2733)))
 (let (($x5208 (= (stack_t x_0 x_1 x_SLOAD_0 w_1 3 (bvadd (_ bv62 6) ?x9666)) (stack_t x_0 x_1 x_SLOAD_0 w_1 2 (bvadd (_ bv62 6) ?x9666)))))
 (let ((?x5752 (bvadd (_ bv61 6) ?x9666)))
 (let ((?x11494 (stack_t x_0 x_1 x_SLOAD_0 w_1 2 ?x5752)))
 (let (($x11030 (exc_halt_t 1)))
 (let (($x8403 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_t 1)))) (_ bv0 1)))))
 (let (($x9974 (forall ((w (_ BitVec 256)) )(let ((?x4763 (storage_t x_0 x_1 x_SLOAD_0 w_1 1 w)))
 (let ((?x7279 (storage_t x_0 x_1 x_SLOAD_0 w_1 2 w)))
 (= ?x7279 ?x4763))))
 ))
 (let (($x5392 (forall ((n (_ BitVec 6)) )(let ((?x7692 (stack_t x_0 x_1 x_SLOAD_0 w_1 1 n)))
 (let ((?x5984 (stack_t x_0 x_1 x_SLOAD_0 w_1 2 n)))
 (or (= ?x5984 ?x7692) (bvsle (bvadd (_ bv61 6) (sc_t 1)) n)))))
 ))
 (let (($x9818 (= ?x9666 (bvadd (_ bv1 6) (sc_t 1)))))
 (let ((?x646 (used_gas_t x_0 x_1 x_SLOAD_0 w_1 2)))
 (let (($x8359 (= (stack_t x_0 x_1 x_SLOAD_0 w_1 2 (bvadd (_ bv63 6) (sc_t 1))) (stack_t x_0 x_1 x_SLOAD_0 w_1 1 (bvadd (_ bv63 6) (sc_t 1))))))
 (let (($x295 (= (stack_t x_0 x_1 x_SLOAD_0 w_1 2 (bvadd (_ bv62 6) (sc_t 1))) (stack_t x_0 x_1 x_SLOAD_0 w_1 1 (bvadd (_ bv62 6) (sc_t 1))))))
 (let ((?x4135 (sc_t 1)))
 (let ((?x1165 (bvadd (_ bv61 6) ?x4135)))
 (let ((?x11292 (stack_t x_0 x_1 x_SLOAD_0 w_1 1 ?x1165)))
 (let (($x8407 (= $x11030 (or $x56 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x63))) (_ bv0 1)))))))
 (let (($x2990 (forall ((w (_ BitVec 256)) )(let ((?x2181 (storage_t x_0 x_1 x_SLOAD_0 w_1 0 w)))
 (let ((?x4763 (storage_t x_0 x_1 x_SLOAD_0 w_1 1 w)))
 (= ?x4763 ?x2181))))
 ))
 (let (($x9691 (forall ((n (_ BitVec 6)) )(let ((?x9070 (stack_t x_0 x_1 x_SLOAD_0 w_1 0 n)))
 (let ((?x7692 (stack_t x_0 x_1 x_SLOAD_0 w_1 1 n)))
 (let ((?x63 (sc_t 0)))
 (let (($x3918 (bvsle ?x63 n)))
 (or $x3918 (= ?x7692 ?x9070)))))))
 ))
 (let (($x4431 (= ?x4135 (bvadd (_ bv1 6) ?x63))))
 (let (($x7952 (= $x7121 (or (exc_halt_s 5) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 5))))))))
 (let (($x8928 (forall ((w (_ BitVec 256)) )(let ((?x8090 (storage_s x_0 x_1 x_SLOAD_0 w_1 5 w)))
 (let ((?x3949 (storage_s x_0 x_1 x_SLOAD_0 w_1 6 w)))
 (= ?x3949 ?x8090))))
 ))
 (let (($x4089 (forall ((n (_ BitVec 6)) )(let ((?x11360 (stack_s x_0 x_1 x_SLOAD_0 w_1 5 n)))
 (let ((?x6386 (stack_s x_0 x_1 x_SLOAD_0 w_1 6 n)))
 (let ((?x4319 (sc_s 5)))
 (let ((?x10710 (bvadd (_ bv62 6) ?x4319)))
 (let (($x2283 (bvsle ?x10710 n)))
 (or $x2283 (= ?x6386 ?x11360))))))))
 ))
 (let ((?x4319 (sc_s 5)))
 (let (($x11055 (= ?x9114 ?x4319)))
 (let (($x3500 (= (used_gas_s x_0 x_1 x_SLOAD_0 w_1 6) (+ 3 (used_gas_s x_0 x_1 x_SLOAD_0 w_1 5)))))
 (let ((?x4243 (bvadd (_ bv63 6) ?x4319)))
 (let ((?x2980 (stack_s x_0 x_1 x_SLOAD_0 w_1 5 ?x4243)))
 (let (($x10914 (= (stack_s x_0 x_1 x_SLOAD_0 w_1 6 (bvadd (_ bv63 6) ?x9114)) (stack_s x_0 x_1 x_SLOAD_0 w_1 5 (bvadd (_ bv62 6) ?x4319)))))
 (let (($x9175 (exc_halt_s 4)))
 (let (($x1255 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 4)))) (_ bv0 1)))))
 (let (($x1862 (exc_halt_s 5)))
 (let (($x11318 (forall ((w (_ BitVec 256)) )(let ((?x5445 (storage_s x_0 x_1 x_SLOAD_0 w_1 4 w)))
 (let ((?x8090 (storage_s x_0 x_1 x_SLOAD_0 w_1 5 w)))
 (= ?x8090 ?x5445))))
 ))
 (let (($x8721 (forall ((n (_ BitVec 6)) )(let ((?x1807 (stack_s x_0 x_1 x_SLOAD_0 w_1 4 n)))
 (let ((?x11360 (stack_s x_0 x_1 x_SLOAD_0 w_1 5 n)))
 (or (bvsle (bvadd (_ bv60 6) (sc_s 4)) n) (= ?x11360 ?x1807)))))
 ))
 (let (($x2529 (= ?x4319 (bvadd (_ bv1 6) (sc_s 4)))))
 (let ((?x10409 (used_gas_s x_0 x_1 x_SLOAD_0 w_1 5)))
 (let ((?x9433 (sc_s 4)))
 (let ((?x11263 (bvadd (_ bv63 6) ?x9433)))
 (let ((?x10494 (stack_s x_0 x_1 x_SLOAD_0 w_1 4 ?x11263)))
 (let ((?x4876 (bvadd (_ bv62 6) ?x9433)))
 (let ((?x5972 (stack_s x_0 x_1 x_SLOAD_0 w_1 4 ?x4876)))
 (let (($x5117 (= (stack_s x_0 x_1 x_SLOAD_0 w_1 5 (bvadd (_ bv61 6) ?x9433)) (stack_s x_0 x_1 x_SLOAD_0 w_1 4 (bvadd (_ bv61 6) ?x9433)))))
 (let ((?x1442 (bvadd (_ bv60 6) ?x9433)))
 (let ((?x10049 (stack_s x_0 x_1 x_SLOAD_0 w_1 4 ?x1442)))
 (let (($x7924 (= $x9175 (or (exc_halt_s 3) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 3))))))))
 (let (($x10642 (forall ((w (_ BitVec 256)) )(let ((?x1155 (storage_s x_0 x_1 x_SLOAD_0 w_1 3 w)))
 (let ((?x5445 (storage_s x_0 x_1 x_SLOAD_0 w_1 4 w)))
 (= ?x5445 ?x1155))))
 ))
 (let (($x7053 (forall ((n (_ BitVec 6)) )(let ((?x3851 (sc_s 3)))
 (let ((?x6235 (bvadd (_ bv62 6) ?x3851)))
 (let (($x11846 (bvsle ?x6235 n)))
 (let ((?x4215 (stack_s x_0 x_1 x_SLOAD_0 w_1 3 n)))
 (let ((?x1807 (stack_s x_0 x_1 x_SLOAD_0 w_1 4 n)))
 (or (= ?x1807 ?x4215) $x11846)))))))
 ))
 (let ((?x3851 (sc_s 3)))
 (let (($x7570 (= ?x9433 ?x3851)))
 (let ((?x7322 (used_gas_s x_0 x_1 x_SLOAD_0 w_1 4)))
 (let (($x5066 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 2)))) (_ bv0 1)))))
 (let (($x10052 (exc_halt_s 2)))
 (let (($x8103 (exc_halt_s 3)))
 (let (($x6723 (forall ((w (_ BitVec 256)) )(let ((?x3801 (storage_s x_0 x_1 x_SLOAD_0 w_1 2 w)))
 (let ((?x1155 (storage_s x_0 x_1 x_SLOAD_0 w_1 3 w)))
 (= ?x1155 ?x3801))))
 ))
 (let (($x11352 (forall ((n (_ BitVec 6)) )(let ((?x2272 (sc_s 2)))
 (let (($x10361 (bvsle ?x2272 n)))
 (let ((?x6532 (stack_s x_0 x_1 x_SLOAD_0 w_1 2 n)))
 (let ((?x4215 (stack_s x_0 x_1 x_SLOAD_0 w_1 3 n)))
 (or (= ?x4215 ?x6532) $x10361))))))
 ))
 (let (($x260 (= ?x3851 (bvadd (_ bv1 6) (sc_s 2)))))
 (let ((?x1820 (used_gas_s x_0 x_1 x_SLOAD_0 w_1 3)))
 (let (($x4454 (= $x10052 (or (exc_halt_s 1) (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_s 1))))))))
 (let (($x5217 (forall ((w (_ BitVec 256)) )(let ((?x6791 (storage_s x_0 x_1 x_SLOAD_0 w_1 1 w)))
 (let ((?x3801 (storage_s x_0 x_1 x_SLOAD_0 w_1 2 w)))
 (= ?x3801 ?x6791))))
 ))
 (let (($x631 (forall ((n (_ BitVec 6)) )(let ((?x1464 (stack_s x_0 x_1 x_SLOAD_0 w_1 1 n)))
 (let ((?x6532 (stack_s x_0 x_1 x_SLOAD_0 w_1 2 n)))
 (let ((?x154 (sc_s 1)))
 (let ((?x7184 (bvadd (_ bv63 6) ?x154)))
 (let (($x3589 (bvsle ?x7184 n)))
 (or $x3589 (= ?x6532 ?x1464))))))))
 ))
 (let ((?x154 (sc_s 1)))
 (let ((?x2272 (sc_s 2)))
 (let (($x6737 (= ?x2272 ?x154)))
 (let ((?x11897 (used_gas_s x_0 x_1 x_SLOAD_0 w_1 2)))
 (let ((?x7184 (bvadd (_ bv63 6) ?x154)))
 (let ((?x3462 (stack_s x_0 x_1 x_SLOAD_0 w_1 1 ?x7184)))
 (let (($x9207 (= (stack_s x_0 x_1 x_SLOAD_0 w_1 2 (bvadd (_ bv63 6) ?x2272)) (storage_s x_0 x_1 x_SLOAD_0 w_1 1 ?x3462))))
 (let (($x5104 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x72))) (_ bv0 1)))))
 (let (($x8780 (exc_halt_s 1)))
 (let (($x5496 (forall ((w (_ BitVec 256)) )(let ((?x3397 (storage_s x_0 x_1 x_SLOAD_0 w_1 0 w)))
 (let ((?x6791 (storage_s x_0 x_1 x_SLOAD_0 w_1 1 w)))
 (= ?x6791 ?x3397))))
 ))
 (let (($x2087 (forall ((n (_ BitVec 6)) )(let ((?x8722 (stack_s x_0 x_1 x_SLOAD_0 w_1 0 n)))
 (let ((?x1464 (stack_s x_0 x_1 x_SLOAD_0 w_1 1 n)))
 (or (= ?x1464 ?x8722) (bvsle (bvadd (_ bv63 6) (sc_s 0)) n)))))
 ))
 (let (($x11116 (= ?x154 (bvadd (_ bv1 6) ?x72))))
 (let ((?x11329 (bvadd (_ bv63 6) ?x72)))
 (let ((?x8468 (stack_s x_0 x_1 x_SLOAD_0 w_1 0 ?x11329)))
 (let (($x2598 (forall ((w (_ BitVec 256)) )(let (($x8999 (= w (stack_s x_0 x_1 x_SLOAD_0 w_1 1 (bvadd (_ bv63 6) (sc_s 1))))))
 (let ((?x3397 (storage_s x_0 x_1 x_SLOAD_0 w_1 0 w)))
 (= ?x3397 (ite $x8999 x_SLOAD_0 (_ bv0 256))))))
 ))
 (let (($x11282 (= ?x8857 0)))
 (let (($x2113 (not $x57)))
 (let (($x9167 (= (stack_s x_0 x_1 x_SLOAD_0 w_1 0 (_ bv1 6)) x_1)))
 (let (($x11343 (= (stack_s x_0 x_1 x_SLOAD_0 w_1 0 (_ bv0 6)) x_0)))
 (let (($x5626 (= ?x72 (_ bv2 6))))
 (and $x5626 $x11343 $x9167 $x2113 $x11282 $x2598 (= ?x3462 ?x8468) (= (stack_s x_0 x_1 x_SLOAD_0 w_1 1 ?x11329) ?x8468) (= (used_gas_s x_0 x_1 x_SLOAD_0 w_1 1) (+ 3 ?x8857)) $x11116 $x2087 $x5496 (= $x8780 (or $x57 $x5104 (not (bvsle (_ bv0 6) ?x11329)))) $x9207 (= ?x11897 (+ 200 (used_gas_s x_0 x_1 x_SLOAD_0 w_1 1))) $x6737 $x631 $x5217 $x4454 (= (stack_s x_0 x_1 x_SLOAD_0 w_1 3 ?x2272) w_1) (= ?x1820 (+ 3 ?x11897)) $x260 $x11352 $x6723 (= $x8103 (or $x10052 $x5066)) (= ?x10494 (stack_s x_0 x_1 x_SLOAD_0 w_1 3 (bvadd (_ bv62 6) ?x3851))) (= ?x5972 (stack_s x_0 x_1 x_SLOAD_0 w_1 3 (bvadd (_ bv63 6) ?x3851))) (= ?x7322 (+ 3 ?x1820)) $x7570 $x7053 $x10642 $x7924 (= ?x2980 ?x10049) (= (stack_s x_0 x_1 x_SLOAD_0 w_1 5 ?x1442) ?x10049) $x5117 (= (stack_s x_0 x_1 x_SLOAD_0 w_1 5 ?x4876) ?x5972) (= (stack_s x_0 x_1 x_SLOAD_0 w_1 5 ?x11263) ?x10494) (= ?x10409 (+ 3 ?x7322)) $x2529 $x8721 $x11318 (= $x1862 (or $x1255 (not (bvsle (_ bv0 6) ?x1442)) $x9175)) $x10914 (= (stack_s x_0 x_1 x_SLOAD_0 w_1 6 (bvadd (_ bv62 6) ?x9114)) ?x2980) $x3500 $x11055 $x4089 $x8928 $x7952 (= (stack_t x_0 x_1 x_SLOAD_0 w_1 1 ?x63) w_1) (= (used_gas_t x_0 x_1 x_SLOAD_0 w_1 1) (+ 3 ?x1070)) $x4431 $x9691 $x2990 $x8407 (= ?x3194 ?x11292) (= (stack_t x_0 x_1 x_SLOAD_0 w_1 2 ?x1165) ?x11292) $x295 $x8359 (= ?x646 (+ 3 (used_gas_t x_0 x_1 x_SLOAD_0 w_1 1))) $x9818 $x5392 $x9974 (= $x6842 (or $x8403 (not (bvsle (_ bv0 6) ?x1165)) $x11030)) (= ?x1880 ?x11494) (= (stack_t x_0 x_1 x_SLOAD_0 w_1 3 ?x5752) ?x11494) $x5208 (= (stack_t x_0 x_1 x_SLOAD_0 w_1 3 ?x2733) ?x3194) (= ?x6116 (+ 3 ?x646)) $x230 $x2595 $x5491 (= $x9131 (or $x6842 (not (bvsle (_ bv0 6) ?x5752)) $x7582)) $x2578 $x984 $x1394 $x1073 $x1077 $x3615 $x73 $x7861 $x58 $x9488 $x4950 (not (and $x2362 $x8178 $x10095 $x1748))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
 )
(check-sat)
(get-proof)
