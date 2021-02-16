; DUP1 SLOAD SWAP1 POP PUSH cw_1 SWAP1 => PUSH cw_1 SWAP1 SLOAD
(set-option :produce-proofs true)
; 
(set-info :status unknown)
(declare-fun storage_t ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 256)) (_ BitVec 256))
(declare-fun w_1 () (_ BitVec 256))
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
 (exists ((x_0 (_ BitVec 256)) (x_SLOAD_0 (_ BitVec 256)) )(let (($x7769 (forall ((w (_ BitVec 256)) )(let ((?x6638 (storage_t x_0 x_SLOAD_0 w_1 3 w)))
 (let ((?x3774 (storage_s x_0 x_SLOAD_0 w_1 6 w)))
 (= ?x3774 ?x6638))))
 ))
 (let (($x3614 (exc_halt_t 3)))
 (let (($x772 (exc_halt_s 6)))
 (let (($x10130 (= $x772 $x3614)))
 (let (($x11040 (forall ((n (_ BitVec 6)) )(let ((?x11372 (stack_t x_0 x_SLOAD_0 w_1 3 n)))
 (let ((?x1637 (stack_s x_0 x_SLOAD_0 w_1 6 n)))
 (let (($x3697 (= ?x1637 ?x11372)))
 (let ((?x11304 (sc_t 3)))
 (let (($x2198 (bvsle ?x11304 n)))
 (or $x2198 $x3697)))))))
 ))
 (let ((?x11304 (sc_t 3)))
 (let ((?x926 (sc_s 6)))
 (let (($x2971 (= ?x926 ?x11304)))
 (let ((?x4418 (used_gas_t x_0 x_SLOAD_0 w_1 0)))
 (let ((?x5123 (used_gas_s x_0 x_SLOAD_0 w_1 0)))
 (let (($x10538 (= ?x5123 ?x4418)))
 (let (($x8217 (forall ((w (_ BitVec 256)) )(let ((?x2752 (storage_t x_0 x_SLOAD_0 w_1 0 w)))
 (let ((?x6896 (storage_s x_0 x_SLOAD_0 w_1 0 w)))
 (= ?x6896 ?x2752))))
 ))
 (let (($x56 (exc_halt_t 0)))
 (let (($x57 (exc_halt_s 0)))
 (let (($x58 (= $x57 $x56)))
 (let (($x777 (forall ((n (_ BitVec 6)) )(let ((?x63 (sc_t 0)))
 (let (($x10756 (bvsle ?x63 n)))
 (let ((?x7407 (stack_t x_0 x_SLOAD_0 w_1 0 n)))
 (let ((?x2223 (stack_s x_0 x_SLOAD_0 w_1 0 n)))
 (let (($x2755 (= ?x2223 ?x7407)))
 (or $x2755 $x10756)))))))
 ))
 (let ((?x63 (sc_t 0)))
 (let ((?x72 (sc_s 0)))
 (let (($x73 (= ?x72 ?x63)))
 (let (($x6123 (= $x3614 (or (exc_halt_t 2) (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_t 2))))))))
 (let (($x10842 (forall ((w (_ BitVec 256)) )(let ((?x1555 (storage_t x_0 x_SLOAD_0 w_1 2 w)))
 (let ((?x6638 (storage_t x_0 x_SLOAD_0 w_1 3 w)))
 (= ?x6638 ?x1555))))
 ))
 (let (($x1101 (forall ((n (_ BitVec 6)) )(let ((?x10001 (stack_t x_0 x_SLOAD_0 w_1 2 n)))
 (let ((?x11372 (stack_t x_0 x_SLOAD_0 w_1 3 n)))
 (or (= ?x11372 ?x10001) (bvsle (bvadd (_ bv63 6) (sc_t 2)) n)))))
 ))
 (let ((?x2714 (sc_t 2)))
 (let (($x9044 (= ?x11304 ?x2714)))
 (let (($x2731 (= (used_gas_t x_0 x_SLOAD_0 w_1 3) (+ 200 (used_gas_t x_0 x_SLOAD_0 w_1 2)))))
 (let ((?x4862 (bvadd (_ bv63 6) ?x2714)))
 (let ((?x3841 (stack_t x_0 x_SLOAD_0 w_1 2 ?x4862)))
 (let (($x1259 (= (stack_t x_0 x_SLOAD_0 w_1 3 (bvadd (_ bv63 6) ?x11304)) (storage_t x_0 x_SLOAD_0 w_1 2 ?x3841))))
 (let (($x4500 (exc_halt_t 2)))
 (let (($x9191 (= $x4500 (or (exc_halt_t 1) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_t 1))))))))
 (let (($x10969 (forall ((w (_ BitVec 256)) )(let ((?x4494 (storage_t x_0 x_SLOAD_0 w_1 1 w)))
 (let ((?x1555 (storage_t x_0 x_SLOAD_0 w_1 2 w)))
 (= ?x1555 ?x4494))))
 ))
 (let (($x1307 (forall ((n (_ BitVec 6)) )(let ((?x9186 (stack_t x_0 x_SLOAD_0 w_1 1 n)))
 (let ((?x10001 (stack_t x_0 x_SLOAD_0 w_1 2 n)))
 (let (($x11097 (= ?x10001 ?x9186)))
 (or (bvsle (bvadd (_ bv62 6) (sc_t 1)) n) $x11097)))))
 ))
 (let ((?x366 (used_gas_t x_0 x_SLOAD_0 w_1 2)))
 (let (($x485 (= ?x366 (+ 3 (used_gas_t x_0 x_SLOAD_0 w_1 1)))))
 (let ((?x7154 (sc_t 1)))
 (let ((?x1361 (bvadd (_ bv63 6) ?x7154)))
 (let ((?x2461 (stack_t x_0 x_SLOAD_0 w_1 1 ?x1361)))
 (let (($x8377 (exc_halt_t 1)))
 (let (($x8120 (= $x8377 (or $x56 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x63))) (_ bv0 1)))))))
 (let (($x5547 (forall ((w (_ BitVec 256)) )(let ((?x2752 (storage_t x_0 x_SLOAD_0 w_1 0 w)))
 (let ((?x4494 (storage_t x_0 x_SLOAD_0 w_1 1 w)))
 (= ?x4494 ?x2752))))
 ))
 (let (($x3410 (forall ((n (_ BitVec 6)) )(let ((?x63 (sc_t 0)))
 (let (($x10756 (bvsle ?x63 n)))
 (let ((?x7407 (stack_t x_0 x_SLOAD_0 w_1 0 n)))
 (let ((?x9186 (stack_t x_0 x_SLOAD_0 w_1 1 n)))
 (let (($x9022 (= ?x9186 ?x7407)))
 (or $x9022 $x10756)))))))
 ))
 (let (($x3866 (= ?x7154 (bvadd (_ bv1 6) ?x63))))
 (let ((?x3651 (used_gas_t x_0 x_SLOAD_0 w_1 1)))
 (let (($x3591 (= ?x3651 (+ 3 ?x4418))))
 (let (($x2690 (= $x772 (or (exc_halt_s 5) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 5))))))))
 (let (($x4312 (forall ((w (_ BitVec 256)) )(let ((?x2516 (storage_s x_0 x_SLOAD_0 w_1 5 w)))
 (let ((?x3774 (storage_s x_0 x_SLOAD_0 w_1 6 w)))
 (= ?x3774 ?x2516))))
 ))
 (let (($x3616 (forall ((n (_ BitVec 6)) )(let ((?x4319 (sc_s 5)))
 (let ((?x1846 (bvadd (_ bv62 6) ?x4319)))
 (let (($x3036 (bvsle ?x1846 n)))
 (let ((?x392 (stack_s x_0 x_SLOAD_0 w_1 5 n)))
 (let ((?x1637 (stack_s x_0 x_SLOAD_0 w_1 6 n)))
 (or (= ?x1637 ?x392) $x3036)))))))
 ))
 (let ((?x4319 (sc_s 5)))
 (let (($x4783 (= ?x926 ?x4319)))
 (let (($x7174 (= (used_gas_s x_0 x_SLOAD_0 w_1 6) (+ 3 (used_gas_s x_0 x_SLOAD_0 w_1 5)))))
 (let (($x2529 (= (stack_s x_0 x_SLOAD_0 w_1 6 (bvadd (_ bv62 6) ?x926)) (stack_s x_0 x_SLOAD_0 w_1 5 (bvadd (_ bv63 6) ?x4319)))))
 (let (($x8192 (= (stack_s x_0 x_SLOAD_0 w_1 6 (bvadd (_ bv63 6) ?x926)) (stack_s x_0 x_SLOAD_0 w_1 5 (bvadd (_ bv62 6) ?x4319)))))
 (let (($x7172 (exc_halt_s 4)))
 (let (($x9629 (or $x7172 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 4)))) (_ bv0 1))))))
 (let (($x11317 (exc_halt_s 5)))
 (let (($x8321 (= $x11317 $x9629)))
 (let (($x6646 (forall ((w (_ BitVec 256)) )(let ((?x4480 (storage_s x_0 x_SLOAD_0 w_1 4 w)))
 (let ((?x2516 (storage_s x_0 x_SLOAD_0 w_1 5 w)))
 (= ?x2516 ?x4480))))
 ))
 (let (($x10584 (forall ((n (_ BitVec 6)) )(let ((?x4305 (sc_s 4)))
 (let (($x6840 (bvsle ?x4305 n)))
 (let ((?x472 (stack_s x_0 x_SLOAD_0 w_1 4 n)))
 (let ((?x392 (stack_s x_0 x_SLOAD_0 w_1 5 n)))
 (let (($x783 (= ?x392 ?x472)))
 (or $x783 $x6840)))))))
 ))
 (let (($x10371 (= ?x4319 (bvadd (_ bv1 6) (sc_s 4)))))
 (let ((?x5609 (used_gas_s x_0 x_SLOAD_0 w_1 5)))
 (let (($x743 (= $x7172 (or (exc_halt_s 3) (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_s 3))))))))
 (let (($x3642 (forall ((w (_ BitVec 256)) )(let ((?x6682 (storage_s x_0 x_SLOAD_0 w_1 3 w)))
 (let ((?x4480 (storage_s x_0 x_SLOAD_0 w_1 4 w)))
 (= ?x4480 ?x6682))))
 ))
 (let (($x7607 (forall ((n (_ BitVec 6)) )(let ((?x3335 (stack_s x_0 x_SLOAD_0 w_1 3 n)))
 (let ((?x472 (stack_s x_0 x_SLOAD_0 w_1 4 n)))
 (let (($x3968 (= ?x472 ?x3335)))
 (or (bvsle (bvadd (_ bv63 6) (sc_s 3)) n) $x3968)))))
 ))
 (let ((?x275 (sc_s 3)))
 (let ((?x397 (bvadd (_ bv63 6) ?x275)))
 (let ((?x4305 (sc_s 4)))
 (let (($x974 (= ?x4305 ?x397)))
 (let ((?x6664 (used_gas_s x_0 x_SLOAD_0 w_1 4)))
 (let (($x292 (exc_halt_s 3)))
 (let (($x4020 (= $x292 (or (exc_halt_s 2) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 2))))))))
 (let (($x10211 (forall ((w (_ BitVec 256)) )(let ((?x10096 (storage_s x_0 x_SLOAD_0 w_1 2 w)))
 (let ((?x6682 (storage_s x_0 x_SLOAD_0 w_1 3 w)))
 (= ?x6682 ?x10096))))
 ))
 (let (($x9104 (forall ((n (_ BitVec 6)) )(let ((?x8557 (stack_s x_0 x_SLOAD_0 w_1 2 n)))
 (let ((?x3335 (stack_s x_0 x_SLOAD_0 w_1 3 n)))
 (let (($x11136 (= ?x3335 ?x8557)))
 (let ((?x218 (sc_s 2)))
 (let ((?x1134 (bvadd (_ bv62 6) ?x218)))
 (let (($x8883 (bvsle ?x1134 n)))
 (or $x8883 $x11136))))))))
 ))
 (let ((?x218 (sc_s 2)))
 (let (($x9337 (= ?x275 ?x218)))
 (let ((?x2265 (used_gas_s x_0 x_SLOAD_0 w_1 3)))
 (let (($x4437 (= ?x2265 (+ 3 (used_gas_s x_0 x_SLOAD_0 w_1 2)))))
 (let ((?x2958 (bvadd (_ bv63 6) ?x218)))
 (let ((?x679 (stack_s x_0 x_SLOAD_0 w_1 2 ?x2958)))
 (let ((?x9733 (bvadd (_ bv62 6) ?x275)))
 (let ((?x10323 (stack_s x_0 x_SLOAD_0 w_1 3 ?x9733)))
 (let ((?x1134 (bvadd (_ bv62 6) ?x218)))
 (let ((?x11216 (stack_s x_0 x_SLOAD_0 w_1 2 ?x1134)))
 (let ((?x1399 (stack_s x_0 x_SLOAD_0 w_1 3 ?x397)))
 (let (($x247 (exc_halt_s 2)))
 (let (($x6433 (= $x247 (or (exc_halt_s 1) (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_s 1))))))))
 (let (($x8431 (forall ((w (_ BitVec 256)) )(let ((?x7851 (storage_s x_0 x_SLOAD_0 w_1 1 w)))
 (let ((?x10096 (storage_s x_0 x_SLOAD_0 w_1 2 w)))
 (= ?x10096 ?x7851))))
 ))
 (let (($x1596 (forall ((n (_ BitVec 6)) )(let ((?x6362 (stack_s x_0 x_SLOAD_0 w_1 1 n)))
 (let ((?x8557 (stack_s x_0 x_SLOAD_0 w_1 2 n)))
 (or (bvsle (bvadd (_ bv63 6) (sc_s 1)) n) (= ?x8557 ?x6362)))))
 ))
 (let ((?x154 (sc_s 1)))
 (let (($x2831 (= ?x218 ?x154)))
 (let ((?x9665 (used_gas_s x_0 x_SLOAD_0 w_1 2)))
 (let ((?x1744 (bvadd (_ bv63 6) ?x154)))
 (let ((?x2208 (stack_s x_0 x_SLOAD_0 w_1 1 ?x1744)))
 (let (($x717 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x72))) (_ bv0 1)))))
 (let (($x189 (exc_halt_s 1)))
 (let (($x10252 (forall ((w (_ BitVec 256)) )(let ((?x6896 (storage_s x_0 x_SLOAD_0 w_1 0 w)))
 (let ((?x7851 (storage_s x_0 x_SLOAD_0 w_1 1 w)))
 (= ?x7851 ?x6896))))
 ))
 (let (($x1811 (forall ((n (_ BitVec 6)) )(let ((?x2223 (stack_s x_0 x_SLOAD_0 w_1 0 n)))
 (let ((?x6362 (stack_s x_0 x_SLOAD_0 w_1 1 n)))
 (let (($x3560 (= ?x6362 ?x2223)))
 (or (bvsle (bvadd (_ bv63 6) (sc_s 0)) n) $x3560)))))
 ))
 (let (($x4777 (= ?x154 (bvadd (_ bv1 6) ?x72))))
 (let ((?x10010 (used_gas_s x_0 x_SLOAD_0 w_1 1)))
 (let (($x7514 (= ?x10010 (+ 3 ?x5123))))
 (let ((?x2515 (bvadd (_ bv63 6) ?x72)))
 (let ((?x4213 (stack_s x_0 x_SLOAD_0 w_1 0 ?x2515)))
 (let (($x11783 (forall ((w (_ BitVec 256)) )(let ((?x2728 (ite (= w (stack_s x_0 x_SLOAD_0 w_1 1 (bvadd (_ bv63 6) (sc_s 1)))) x_SLOAD_0 (_ bv0 256))))
 (let ((?x6896 (storage_s x_0 x_SLOAD_0 w_1 0 w)))
 (= ?x6896 ?x2728))))
 ))
 (let (($x1195 (= ?x5123 0)))
 (let (($x11625 (not $x57)))
 (let (($x8913 (= (stack_s x_0 x_SLOAD_0 w_1 0 (_ bv0 6)) x_0)))
 (let (($x7315 (= ?x72 (_ bv1 6))))
 (and $x7315 $x8913 $x11625 $x1195 $x11783 (= ?x2208 ?x4213) (= (stack_s x_0 x_SLOAD_0 w_1 1 ?x2515) ?x4213) $x7514 $x4777 $x1811 $x10252 (= $x189 (or $x57 $x717 (not (bvsle (_ bv0 6) ?x2515)))) (= ?x679 (storage_s x_0 x_SLOAD_0 w_1 1 ?x2208)) (= ?x9665 (+ 200 ?x10010)) $x2831 $x1596 $x8431 $x6433 (= ?x1399 ?x11216) (= ?x10323 ?x679) $x4437 $x9337 $x9104 $x10211 $x4020 (= ?x6664 (+ 2 ?x2265)) $x974 $x7607 $x3642 $x743 (= (stack_s x_0 x_SLOAD_0 w_1 5 ?x4305) w_1) (= ?x5609 (+ 3 ?x6664)) $x10371 $x10584 $x6646 $x8321 $x8192 $x2529 $x7174 $x4783 $x3616 $x4312 $x2690 (= (stack_t x_0 x_SLOAD_0 w_1 1 ?x63) w_1) $x3591 $x3866 $x3410 $x5547 $x8120 (= ?x3841 (stack_t x_0 x_SLOAD_0 w_1 1 (bvadd (_ bv62 6) ?x7154))) (= (stack_t x_0 x_SLOAD_0 w_1 2 (bvadd (_ bv62 6) ?x2714)) ?x2461) $x485 (= ?x2714 ?x7154) $x1307 $x10969 $x9191 $x1259 $x2731 $x9044 $x1101 $x10842 $x6123 $x73 $x777 $x58 $x8217 $x10538 (not (and $x2971 $x11040 $x10130 $x7769))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
 )
(check-sat)
(get-proof)