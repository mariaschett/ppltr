; SLOAD PUSH cw_3 PUSH cw_2 SWAP1 SWAP2 => PUSH cw_2 PUSH cw_3 SWAP2 SLOAD
(set-option :produce-proofs true)
; 
(set-info :status unknown)
(declare-fun storage_t ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 256)) (_ BitVec 256))
(declare-fun w_2 () (_ BitVec 256))
(declare-fun w_3 () (_ BitVec 256))
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
 (exists ((x_0 (_ BitVec 256)) (x_SLOAD_0 (_ BitVec 256)) )(let (($x7606 (forall ((w (_ BitVec 256)) )(let ((?x8405 (storage_t x_0 x_SLOAD_0 w_3 w_2 4 w)))
 (let ((?x2441 (storage_s x_0 x_SLOAD_0 w_3 w_2 5 w)))
 (= ?x2441 ?x8405))))
 ))
 (let (($x7854 (exc_halt_t 4)))
 (let (($x1862 (exc_halt_s 5)))
 (let (($x1994 (= $x1862 $x7854)))
 (let (($x8652 (forall ((n (_ BitVec 6)) )(let ((?x7671 (stack_t x_0 x_SLOAD_0 w_3 w_2 4 n)))
 (let ((?x4929 (stack_s x_0 x_SLOAD_0 w_3 w_2 5 n)))
 (let (($x9265 (= ?x4929 ?x7671)))
 (let ((?x7495 (sc_t 4)))
 (let (($x3588 (bvsle ?x7495 n)))
 (or $x3588 $x9265)))))))
 ))
 (let ((?x7495 (sc_t 4)))
 (let ((?x4319 (sc_s 5)))
 (let (($x3074 (= ?x4319 ?x7495)))
 (let ((?x5988 (used_gas_t x_0 x_SLOAD_0 w_3 w_2 0)))
 (let ((?x2751 (used_gas_s x_0 x_SLOAD_0 w_3 w_2 0)))
 (let (($x10993 (= ?x2751 ?x5988)))
 (let (($x11017 (forall ((w (_ BitVec 256)) )(let ((?x8149 (storage_t x_0 x_SLOAD_0 w_3 w_2 0 w)))
 (let ((?x8233 (storage_s x_0 x_SLOAD_0 w_3 w_2 0 w)))
 (= ?x8233 ?x8149))))
 ))
 (let (($x56 (exc_halt_t 0)))
 (let (($x57 (exc_halt_s 0)))
 (let (($x58 (= $x57 $x56)))
 (let (($x6216 (forall ((n (_ BitVec 6)) )(let ((?x1026 (stack_t x_0 x_SLOAD_0 w_3 w_2 0 n)))
 (let ((?x9063 (stack_s x_0 x_SLOAD_0 w_3 w_2 0 n)))
 (let (($x4455 (= ?x9063 ?x1026)))
 (let ((?x63 (sc_t 0)))
 (let (($x2738 (bvsle ?x63 n)))
 (or $x2738 $x4455)))))))
 ))
 (let ((?x63 (sc_t 0)))
 (let ((?x72 (sc_s 0)))
 (let (($x73 (= ?x72 ?x63)))
 (let (($x9706 (= $x7854 (or (exc_halt_t 3) (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_t 3))))))))
 (let (($x4088 (forall ((w (_ BitVec 256)) )(let ((?x9347 (storage_t x_0 x_SLOAD_0 w_3 w_2 3 w)))
 (let ((?x8405 (storage_t x_0 x_SLOAD_0 w_3 w_2 4 w)))
 (= ?x8405 ?x9347))))
 ))
 (let (($x1037 (forall ((n (_ BitVec 6)) )(let ((?x9232 (sc_t 3)))
 (let ((?x9309 (bvadd (_ bv63 6) ?x9232)))
 (let (($x2727 (bvsle ?x9309 n)))
 (let ((?x3166 (stack_t x_0 x_SLOAD_0 w_3 w_2 3 n)))
 (let ((?x7671 (stack_t x_0 x_SLOAD_0 w_3 w_2 4 n)))
 (or (= ?x7671 ?x3166) $x2727)))))))
 ))
 (let ((?x9232 (sc_t 3)))
 (let (($x8297 (= ?x7495 ?x9232)))
 (let (($x1986 (= (used_gas_t x_0 x_SLOAD_0 w_3 w_2 4) (+ 200 (used_gas_t x_0 x_SLOAD_0 w_3 w_2 3)))))
 (let ((?x9309 (bvadd (_ bv63 6) ?x9232)))
 (let ((?x4250 (stack_t x_0 x_SLOAD_0 w_3 w_2 3 ?x9309)))
 (let (($x3585 (= (stack_t x_0 x_SLOAD_0 w_3 w_2 4 (bvadd (_ bv63 6) ?x7495)) (storage_t x_0 x_SLOAD_0 w_3 w_2 3 ?x4250))))
 (let (($x418 (exc_halt_t 3)))
 (let (($x10481 (= $x418 (or (exc_halt_t 2) (not (bvsle (_ bv0 6) (bvadd (_ bv61 6) (sc_t 2))))))))
 (let (($x533 (forall ((w (_ BitVec 256)) )(let ((?x4040 (storage_t x_0 x_SLOAD_0 w_3 w_2 2 w)))
 (let ((?x9347 (storage_t x_0 x_SLOAD_0 w_3 w_2 3 w)))
 (= ?x9347 ?x4040))))
 ))
 (let (($x9815 (forall ((n (_ BitVec 6)) )(let ((?x7068 (stack_t x_0 x_SLOAD_0 w_3 w_2 2 n)))
 (let ((?x3166 (stack_t x_0 x_SLOAD_0 w_3 w_2 3 n)))
 (or (bvsle (bvadd (_ bv61 6) (sc_t 2)) n) (= ?x3166 ?x7068)))))
 ))
 (let ((?x10833 (used_gas_t x_0 x_SLOAD_0 w_3 w_2 3)))
 (let (($x2931 (= (stack_t x_0 x_SLOAD_0 w_3 w_2 3 (bvadd (_ bv62 6) ?x9232)) (stack_t x_0 x_SLOAD_0 w_3 w_2 2 (bvadd (_ bv62 6) (sc_t 2))))))
 (let (($x7366 (= (stack_t x_0 x_SLOAD_0 w_3 w_2 3 (bvadd (_ bv61 6) ?x9232)) (stack_t x_0 x_SLOAD_0 w_3 w_2 2 (bvadd (_ bv63 6) (sc_t 2))))))
 (let (($x9662 (= ?x4250 (stack_t x_0 x_SLOAD_0 w_3 w_2 2 (bvadd (_ bv61 6) (sc_t 2))))))
 (let (($x529 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_t 1)))) (_ bv0 1)))))
 (let (($x4852 (exc_halt_t 1)))
 (let (($x7905 (exc_halt_t 2)))
 (let (($x872 (= $x7905 (or $x4852 $x529))))
 (let (($x11930 (forall ((w (_ BitVec 256)) )(let ((?x6487 (storage_t x_0 x_SLOAD_0 w_3 w_2 1 w)))
 (let ((?x4040 (storage_t x_0 x_SLOAD_0 w_3 w_2 2 w)))
 (= ?x4040 ?x6487))))
 ))
 (let (($x11880 (forall ((n (_ BitVec 6)) )(let ((?x7411 (stack_t x_0 x_SLOAD_0 w_3 w_2 1 n)))
 (let ((?x7068 (stack_t x_0 x_SLOAD_0 w_3 w_2 2 n)))
 (let ((?x9666 (sc_t 1)))
 (let (($x37 (bvsle ?x9666 n)))
 (or $x37 (= ?x7068 ?x7411)))))))
 ))
 (let ((?x11248 (sc_t 2)))
 (let (($x3655 (= ?x11248 (bvadd (_ bv1 6) (sc_t 1)))))
 (let ((?x845 (used_gas_t x_0 x_SLOAD_0 w_3 w_2 2)))
 (let (($x489 (= $x4852 (or $x56 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x63))) (_ bv0 1)))))))
 (let (($x4937 (forall ((w (_ BitVec 256)) )(let ((?x8149 (storage_t x_0 x_SLOAD_0 w_3 w_2 0 w)))
 (let ((?x6487 (storage_t x_0 x_SLOAD_0 w_3 w_2 1 w)))
 (= ?x6487 ?x8149))))
 ))
 (let (($x848 (forall ((n (_ BitVec 6)) )(let ((?x1026 (stack_t x_0 x_SLOAD_0 w_3 w_2 0 n)))
 (let ((?x7411 (stack_t x_0 x_SLOAD_0 w_3 w_2 1 n)))
 (let ((?x63 (sc_t 0)))
 (let (($x2738 (bvsle ?x63 n)))
 (or $x2738 (= ?x7411 ?x1026)))))))
 ))
 (let ((?x9666 (sc_t 1)))
 (let (($x3631 (= ?x9666 (bvadd (_ bv1 6) ?x63))))
 (let (($x3392 (= $x1862 (or (exc_halt_s 4) (not (bvsle (_ bv0 6) (bvadd (_ bv61 6) (sc_s 4))))))))
 (let (($x11605 (forall ((w (_ BitVec 256)) )(let ((?x4730 (storage_s x_0 x_SLOAD_0 w_3 w_2 4 w)))
 (let ((?x2441 (storage_s x_0 x_SLOAD_0 w_3 w_2 5 w)))
 (= ?x2441 ?x4730))))
 ))
 (let (($x5489 (forall ((n (_ BitVec 6)) )(let ((?x1191 (stack_s x_0 x_SLOAD_0 w_3 w_2 4 n)))
 (let ((?x4929 (stack_s x_0 x_SLOAD_0 w_3 w_2 5 n)))
 (let ((?x9433 (sc_s 4)))
 (let ((?x8761 (bvadd (_ bv61 6) ?x9433)))
 (let (($x1530 (bvsle ?x8761 n)))
 (or $x1530 (= ?x4929 ?x1191))))))))
 ))
 (let ((?x9433 (sc_s 4)))
 (let (($x11305 (= ?x4319 ?x9433)))
 (let (($x7772 (= (used_gas_s x_0 x_SLOAD_0 w_3 w_2 5) (+ 3 (used_gas_s x_0 x_SLOAD_0 w_3 w_2 4)))))
 (let ((?x1557 (bvadd (_ bv62 6) ?x9433)))
 (let ((?x6636 (stack_s x_0 x_SLOAD_0 w_3 w_2 4 ?x1557)))
 (let ((?x11699 (bvadd (_ bv63 6) ?x9433)))
 (let ((?x10636 (stack_s x_0 x_SLOAD_0 w_3 w_2 4 ?x11699)))
 (let (($x9595 (= (stack_s x_0 x_SLOAD_0 w_3 w_2 5 (bvadd (_ bv63 6) ?x4319)) (stack_s x_0 x_SLOAD_0 w_3 w_2 4 (bvadd (_ bv61 6) ?x9433)))))
 (let (($x9175 (exc_halt_s 4)))
 (let (($x10301 (= $x9175 (or (exc_halt_s 3) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 3))))))))
 (let (($x10859 (forall ((w (_ BitVec 256)) )(let ((?x2720 (storage_s x_0 x_SLOAD_0 w_3 w_2 3 w)))
 (let ((?x4730 (storage_s x_0 x_SLOAD_0 w_3 w_2 4 w)))
 (= ?x4730 ?x2720))))
 ))
 (let (($x2189 (forall ((n (_ BitVec 6)) )(let ((?x3620 (stack_s x_0 x_SLOAD_0 w_3 w_2 3 n)))
 (let ((?x1191 (stack_s x_0 x_SLOAD_0 w_3 w_2 4 n)))
 (let ((?x3851 (sc_s 3)))
 (let ((?x3651 (bvadd (_ bv62 6) ?x3851)))
 (let (($x3189 (bvsle ?x3651 n)))
 (or $x3189 (= ?x1191 ?x3620))))))))
 ))
 (let ((?x3851 (sc_s 3)))
 (let (($x2630 (= ?x9433 ?x3851)))
 (let ((?x3087 (used_gas_s x_0 x_SLOAD_0 w_3 w_2 4)))
 (let (($x8580 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 2)))) (_ bv0 1)))))
 (let (($x10052 (exc_halt_s 2)))
 (let (($x8103 (exc_halt_s 3)))
 (let (($x2547 (= $x8103 (or $x10052 $x8580))))
 (let (($x7170 (forall ((w (_ BitVec 256)) )(let ((?x11713 (storage_s x_0 x_SLOAD_0 w_3 w_2 2 w)))
 (let ((?x2720 (storage_s x_0 x_SLOAD_0 w_3 w_2 3 w)))
 (= ?x2720 ?x11713))))
 ))
 (let (($x6215 (forall ((n (_ BitVec 6)) )(let ((?x2272 (sc_s 2)))
 (let (($x8494 (bvsle ?x2272 n)))
 (let ((?x6474 (stack_s x_0 x_SLOAD_0 w_3 w_2 2 n)))
 (let ((?x3620 (stack_s x_0 x_SLOAD_0 w_3 w_2 3 n)))
 (or (= ?x3620 ?x6474) $x8494))))))
 ))
 (let (($x11281 (= ?x3851 (bvadd (_ bv1 6) (sc_s 2)))))
 (let ((?x10529 (used_gas_s x_0 x_SLOAD_0 w_3 w_2 3)))
 (let (($x672 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 1)))) (_ bv0 1)))))
 (let (($x8780 (exc_halt_s 1)))
 (let (($x4433 (= $x10052 (or $x8780 $x672))))
 (let (($x11060 (forall ((w (_ BitVec 256)) )(let ((?x11691 (storage_s x_0 x_SLOAD_0 w_3 w_2 1 w)))
 (let ((?x11713 (storage_s x_0 x_SLOAD_0 w_3 w_2 2 w)))
 (= ?x11713 ?x11691))))
 ))
 (let (($x8492 (forall ((n (_ BitVec 6)) )(let ((?x154 (sc_s 1)))
 (let (($x9053 (bvsle ?x154 n)))
 (let ((?x11598 (stack_s x_0 x_SLOAD_0 w_3 w_2 1 n)))
 (let ((?x6474 (stack_s x_0 x_SLOAD_0 w_3 w_2 2 n)))
 (or (= ?x6474 ?x11598) $x9053))))))
 ))
 (let ((?x2272 (sc_s 2)))
 (let (($x7044 (= ?x2272 (bvadd (_ bv1 6) (sc_s 1)))))
 (let ((?x2596 (used_gas_s x_0 x_SLOAD_0 w_3 w_2 2)))
 (let (($x2281 (forall ((w (_ BitVec 256)) )(let ((?x8233 (storage_s x_0 x_SLOAD_0 w_3 w_2 0 w)))
 (let ((?x11691 (storage_s x_0 x_SLOAD_0 w_3 w_2 1 w)))
 (= ?x11691 ?x8233))))
 ))
 (let (($x9851 (forall ((n (_ BitVec 6)) )(let ((?x9063 (stack_s x_0 x_SLOAD_0 w_3 w_2 0 n)))
 (let ((?x11598 (stack_s x_0 x_SLOAD_0 w_3 w_2 1 n)))
 (or (bvsle (bvadd (_ bv63 6) (sc_s 0)) n) (= ?x11598 ?x9063)))))
 ))
 (let ((?x154 (sc_s 1)))
 (let (($x5930 (= ?x154 ?x72)))
 (let ((?x4711 (storage_s x_0 x_SLOAD_0 w_3 w_2 0 (stack_s x_0 x_SLOAD_0 w_3 w_2 0 (bvadd (_ bv63 6) ?x72)))))
 (let (($x3642 (forall ((w (_ BitVec 256)) )(let (($x4979 (= w (stack_s x_0 x_SLOAD_0 w_3 w_2 0 (bvadd (_ bv63 6) (sc_s 0))))))
 (let ((?x8233 (storage_s x_0 x_SLOAD_0 w_3 w_2 0 w)))
 (= ?x8233 (ite $x4979 x_SLOAD_0 (_ bv0 256))))))
 ))
 (let (($x8888 (= ?x2751 0)))
 (let (($x2276 (not $x57)))
 (let (($x11161 (= (stack_s x_0 x_SLOAD_0 w_3 w_2 0 (_ bv0 6)) x_0)))
 (let (($x7461 (= ?x72 (_ bv1 6))))
 (and $x7461 $x11161 $x2276 $x8888 $x3642 (= (stack_s x_0 x_SLOAD_0 w_3 w_2 1 (bvadd (_ bv63 6) ?x154)) ?x4711) (= (used_gas_s x_0 x_SLOAD_0 w_3 w_2 1) (+ 200 ?x2751)) $x5930 $x9851 $x2281 (= $x8780 (or $x57 (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) ?x72))))) (= (stack_s x_0 x_SLOAD_0 w_3 w_2 2 ?x154) w_3) (= ?x2596 (+ 3 (used_gas_s x_0 x_SLOAD_0 w_3 w_2 1))) $x7044 $x8492 $x11060 $x4433 (= (stack_s x_0 x_SLOAD_0 w_3 w_2 3 ?x2272) w_2) (= ?x10529 (+ 3 ?x2596)) $x11281 $x6215 $x7170 $x2547 (= ?x10636 (stack_s x_0 x_SLOAD_0 w_3 w_2 3 (bvadd (_ bv62 6) ?x3851))) (= ?x6636 (stack_s x_0 x_SLOAD_0 w_3 w_2 3 (bvadd (_ bv63 6) ?x3851))) (= ?x3087 (+ 3 ?x10529)) $x2630 $x2189 $x10859 $x10301 $x9595 (= (stack_s x_0 x_SLOAD_0 w_3 w_2 5 (bvadd (_ bv61 6) ?x4319)) ?x10636) (= (stack_s x_0 x_SLOAD_0 w_3 w_2 5 (bvadd (_ bv62 6) ?x4319)) ?x6636) $x7772 $x11305 $x5489 $x11605 $x3392 (= (stack_t x_0 x_SLOAD_0 w_3 w_2 1 ?x63) w_2) (= (used_gas_t x_0 x_SLOAD_0 w_3 w_2 1) (+ 3 ?x5988)) $x3631 $x848 $x4937 $x489 (= (stack_t x_0 x_SLOAD_0 w_3 w_2 2 ?x9666) w_3) (= ?x845 (+ 3 (used_gas_t x_0 x_SLOAD_0 w_3 w_2 1))) $x3655 $x11880 $x11930 $x872 $x9662 $x7366 $x2931 (= ?x10833 (+ 3 ?x845)) (= ?x9232 ?x11248) $x9815 $x533 $x10481 $x3585 $x1986 $x8297 $x1037 $x4088 $x9706 $x73 $x6216 $x58 $x11017 $x10993 (not (and $x3074 $x8652 $x1994 $x7606))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
 )
(check-sat)
(get-proof)
