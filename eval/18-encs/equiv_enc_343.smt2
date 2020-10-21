; PUSH cw_3 DUP3 PUSH 0x00 NOT AND PUSH cw_3 => PUSH cw_3 DUP3 DUP2
(set-option :produce-proofs true)
; 
(set-info :status unknown)
(declare-fun storage_t ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 256)) (_ BitVec 256))
(declare-fun w_3 () (_ BitVec 256))
(declare-fun storage_s ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 256)) (_ BitVec 256))
(declare-fun exc_halt_t (Int) Bool)
(declare-fun exc_halt_s (Int) Bool)
(declare-fun sc_t (Int) (_ BitVec 6))
(declare-fun stack_t ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 6)) (_ BitVec 256))
(declare-fun stack_s ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 6)) (_ BitVec 256))
(declare-fun sc_s (Int) (_ BitVec 6))
(declare-fun used_gas_t ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int) Int)
(declare-fun used_gas_s ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int) Int)
(assert
 (exists ((x_0 (_ BitVec 256)) (x_1 (_ BitVec 256)) )(let (($x11808 (forall ((w (_ BitVec 256)) )(let ((?x8041 (storage_t x_0 x_1 w_3 3 w)))
 (let ((?x10715 (storage_s x_0 x_1 w_3 6 w)))
 (= ?x10715 ?x8041))))
 ))
 (let (($x6783 (exc_halt_t 3)))
 (let (($x772 (exc_halt_s 6)))
 (let (($x3870 (= $x772 $x6783)))
 (let (($x1664 (forall ((n (_ BitVec 6)) )(let ((?x6438 (sc_t 3)))
 (let (($x9479 (bvsle ?x6438 n)))
 (let ((?x3341 (stack_t x_0 x_1 w_3 3 n)))
 (let ((?x10280 (stack_s x_0 x_1 w_3 6 n)))
 (let (($x382 (= ?x10280 ?x3341)))
 (or $x382 $x9479)))))))
 ))
 (let ((?x6438 (sc_t 3)))
 (let ((?x926 (sc_s 6)))
 (let (($x5324 (= ?x926 ?x6438)))
 (let ((?x6317 (used_gas_t x_0 x_1 w_3 0)))
 (let ((?x5149 (used_gas_s x_0 x_1 w_3 0)))
 (let (($x2854 (= ?x5149 ?x6317)))
 (let (($x7042 (forall ((w (_ BitVec 256)) )(let ((?x4590 (storage_t x_0 x_1 w_3 0 w)))
 (let ((?x2359 (storage_s x_0 x_1 w_3 0 w)))
 (= ?x2359 ?x4590))))
 ))
 (let (($x56 (exc_halt_t 0)))
 (let (($x57 (exc_halt_s 0)))
 (let (($x58 (= $x57 $x56)))
 (let (($x4330 (forall ((n (_ BitVec 6)) )(let ((?x63 (sc_t 0)))
 (let (($x9737 (bvsle ?x63 n)))
 (let ((?x9175 (stack_t x_0 x_1 w_3 0 n)))
 (let ((?x246 (stack_s x_0 x_1 w_3 0 n)))
 (let (($x5383 (= ?x246 ?x9175)))
 (or $x5383 $x9737)))))))
 ))
 (let ((?x63 (sc_t 0)))
 (let ((?x72 (sc_s 0)))
 (let (($x73 (= ?x72 ?x63)))
 (let (($x8185 (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_t 2))))))
 (let (($x5252 (exc_halt_t 2)))
 (let (($x4830 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_t 2)))) (_ bv0 1)))))
 (let (($x9774 (forall ((w (_ BitVec 256)) )(let ((?x10765 (storage_t x_0 x_1 w_3 2 w)))
 (let ((?x8041 (storage_t x_0 x_1 w_3 3 w)))
 (= ?x8041 ?x10765))))
 ))
 (let (($x11209 (forall ((n (_ BitVec 6)) )(let ((?x2714 (sc_t 2)))
 (let ((?x1614 (bvadd (_ bv62 6) ?x2714)))
 (let (($x6008 (bvsle ?x1614 n)))
 (let ((?x11528 (stack_t x_0 x_1 w_3 2 n)))
 (let ((?x3341 (stack_t x_0 x_1 w_3 3 n)))
 (or (= ?x3341 ?x11528) $x6008)))))))
 ))
 (let (($x8875 (= ?x6438 (bvadd (_ bv1 6) (sc_t 2)))))
 (let (($x11136 (= (used_gas_t x_0 x_1 w_3 3) (+ 3 (used_gas_t x_0 x_1 w_3 2)))))
 (let ((?x2714 (sc_t 2)))
 (let ((?x11413 (bvadd (_ bv63 6) ?x2714)))
 (let ((?x6398 (stack_t x_0 x_1 w_3 2 ?x11413)))
 (let ((?x1614 (bvadd (_ bv62 6) ?x2714)))
 (let ((?x7798 (stack_t x_0 x_1 w_3 2 ?x1614)))
 (let (($x8039 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_t 1)))) (_ bv0 1)))))
 (let (($x6122 (exc_halt_t 1)))
 (let (($x1692 (forall ((w (_ BitVec 256)) )(let ((?x4172 (storage_t x_0 x_1 w_3 1 w)))
 (let ((?x10765 (storage_t x_0 x_1 w_3 2 w)))
 (= ?x10765 ?x4172))))
 ))
 (let (($x10051 (forall ((n (_ BitVec 6)) )(let ((?x3659 (stack_t x_0 x_1 w_3 1 n)))
 (let ((?x11528 (stack_t x_0 x_1 w_3 2 n)))
 (or (bvsle (bvadd (_ bv61 6) (sc_t 1)) n) (= ?x11528 ?x3659)))))
 ))
 (let (($x4440 (= ?x2714 (bvadd (_ bv1 6) (sc_t 1)))))
 (let ((?x2013 (used_gas_t x_0 x_1 w_3 2)))
 (let (($x6981 (= (stack_t x_0 x_1 w_3 2 (bvadd (_ bv63 6) (sc_t 1))) (stack_t x_0 x_1 w_3 1 (bvadd (_ bv63 6) (sc_t 1))))))
 (let (($x7574 (= (stack_t x_0 x_1 w_3 2 (bvadd (_ bv62 6) (sc_t 1))) (stack_t x_0 x_1 w_3 1 (bvadd (_ bv62 6) (sc_t 1))))))
 (let ((?x5151 (sc_t 1)))
 (let ((?x7196 (bvadd (_ bv61 6) ?x5151)))
 (let ((?x9654 (stack_t x_0 x_1 w_3 1 ?x7196)))
 (let (($x1753 (= $x6122 (or $x56 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x63))) (_ bv0 1)))))))
 (let (($x6071 (forall ((w (_ BitVec 256)) )(let ((?x4590 (storage_t x_0 x_1 w_3 0 w)))
 (let ((?x4172 (storage_t x_0 x_1 w_3 1 w)))
 (= ?x4172 ?x4590))))
 ))
 (let (($x10259 (forall ((n (_ BitVec 6)) )(let ((?x63 (sc_t 0)))
 (let (($x9737 (bvsle ?x63 n)))
 (let ((?x9175 (stack_t x_0 x_1 w_3 0 n)))
 (let ((?x3659 (stack_t x_0 x_1 w_3 1 n)))
 (or (= ?x3659 ?x9175) $x9737))))))
 ))
 (let (($x10883 (= ?x5151 (bvadd (_ bv1 6) ?x63))))
 (let (($x11317 (exc_halt_s 5)))
 (let (($x412 (or $x11317 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 5)))) (_ bv0 1))))))
 (let (($x1378 (= $x772 $x412)))
 (let (($x1332 (forall ((w (_ BitVec 256)) )(let ((?x5523 (storage_s x_0 x_1 w_3 5 w)))
 (let ((?x10715 (storage_s x_0 x_1 w_3 6 w)))
 (= ?x10715 ?x5523))))
 ))
 (let (($x4820 (forall ((n (_ BitVec 6)) )(let ((?x8903 (stack_s x_0 x_1 w_3 5 n)))
 (let ((?x10280 (stack_s x_0 x_1 w_3 6 n)))
 (let ((?x4319 (sc_s 5)))
 (let (($x9945 (bvsle ?x4319 n)))
 (or $x9945 (= ?x10280 ?x8903)))))))
 ))
 (let (($x7083 (= ?x926 (bvadd (_ bv1 6) (sc_s 5)))))
 (let (($x10741 (= (used_gas_s x_0 x_1 w_3 6) (+ 3 (used_gas_s x_0 x_1 w_3 5)))))
 (let (($x2852 (= $x11317 (or (exc_halt_s 4) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 4))))))))
 (let (($x5634 (forall ((w (_ BitVec 256)) )(let ((?x10812 (storage_s x_0 x_1 w_3 4 w)))
 (let ((?x5523 (storage_s x_0 x_1 w_3 5 w)))
 (= ?x5523 ?x10812))))
 ))
 (let (($x7767 (forall ((n (_ BitVec 6)) )(let ((?x4305 (sc_s 4)))
 (let ((?x9058 (bvadd (_ bv62 6) ?x4305)))
 (let (($x4752 (bvsle ?x9058 n)))
 (let ((?x4845 (stack_s x_0 x_1 w_3 4 n)))
 (let ((?x8903 (stack_s x_0 x_1 w_3 5 n)))
 (or (= ?x8903 ?x4845) $x4752)))))))
 ))
 (let ((?x4305 (sc_s 4)))
 (let ((?x4445 (bvadd (_ bv63 6) ?x4305)))
 (let ((?x4319 (sc_s 5)))
 (let (($x6275 (= ?x4319 ?x4445)))
 (let ((?x1387 (used_gas_s x_0 x_1 w_3 5)))
 (let ((?x11713 (bvor (bvnot (stack_s x_0 x_1 w_3 4 ?x4445)) (bvnot (stack_s x_0 x_1 w_3 4 (bvadd (_ bv62 6) ?x4305))))))
 (let (($x7172 (exc_halt_s 4)))
 (let (($x11515 (= $x7172 (or (exc_halt_s 3) (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_s 3))))))))
 (let (($x8036 (forall ((w (_ BitVec 256)) )(let ((?x5232 (storage_s x_0 x_1 w_3 3 w)))
 (let ((?x10812 (storage_s x_0 x_1 w_3 4 w)))
 (= ?x10812 ?x5232))))
 ))
 (let (($x714 (forall ((n (_ BitVec 6)) )(let ((?x8634 (stack_s x_0 x_1 w_3 3 n)))
 (let ((?x4845 (stack_s x_0 x_1 w_3 4 n)))
 (or (bvsle (bvadd (_ bv63 6) (sc_s 3)) n) (= ?x4845 ?x8634)))))
 ))
 (let ((?x275 (sc_s 3)))
 (let (($x8378 (= ?x4305 ?x275)))
 (let ((?x8390 (used_gas_s x_0 x_1 w_3 4)))
 (let ((?x10450 (stack_s x_0 x_1 w_3 4 ?x4445)))
 (let (($x247 (exc_halt_s 2)))
 (let (($x9740 (or $x247 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 2)))) (_ bv0 1))))))
 (let (($x292 (exc_halt_s 3)))
 (let (($x6699 (= $x292 $x9740)))
 (let (($x5345 (forall ((w (_ BitVec 256)) )(let ((?x7845 (storage_s x_0 x_1 w_3 2 w)))
 (let ((?x5232 (storage_s x_0 x_1 w_3 3 w)))
 (= ?x5232 ?x7845))))
 ))
 (let (($x5793 (forall ((n (_ BitVec 6)) )(let ((?x11096 (stack_s x_0 x_1 w_3 2 n)))
 (let ((?x8634 (stack_s x_0 x_1 w_3 3 n)))
 (let ((?x218 (sc_s 2)))
 (let (($x3398 (bvsle ?x218 n)))
 (or $x3398 (= ?x8634 ?x11096)))))))
 ))
 (let (($x1061 (= ?x275 (bvadd (_ bv1 6) (sc_s 2)))))
 (let ((?x8033 (used_gas_s x_0 x_1 w_3 3)))
 (let (($x5228 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 1)))) (_ bv0 1)))))
 (let (($x189 (exc_halt_s 1)))
 (let (($x8751 (forall ((w (_ BitVec 256)) )(let ((?x6128 (storage_s x_0 x_1 w_3 1 w)))
 (let ((?x7845 (storage_s x_0 x_1 w_3 2 w)))
 (= ?x7845 ?x6128))))
 ))
 (let (($x9168 (forall ((n (_ BitVec 6)) )(let ((?x9225 (stack_s x_0 x_1 w_3 1 n)))
 (let ((?x11096 (stack_s x_0 x_1 w_3 2 n)))
 (or (bvsle (bvadd (_ bv61 6) (sc_s 1)) n) (= ?x11096 ?x9225)))))
 ))
 (let ((?x218 (sc_s 2)))
 (let (($x9893 (= ?x218 (bvadd (_ bv1 6) (sc_s 1)))))
 (let ((?x1314 (used_gas_s x_0 x_1 w_3 2)))
 (let (($x656 (= (stack_s x_0 x_1 w_3 2 (bvadd (_ bv63 6) (sc_s 1))) (stack_s x_0 x_1 w_3 1 (bvadd (_ bv63 6) (sc_s 1))))))
 (let (($x8042 (= (stack_s x_0 x_1 w_3 2 (bvadd (_ bv62 6) (sc_s 1))) (stack_s x_0 x_1 w_3 1 (bvadd (_ bv62 6) (sc_s 1))))))
 (let ((?x154 (sc_s 1)))
 (let ((?x1595 (bvadd (_ bv61 6) ?x154)))
 (let ((?x1698 (stack_s x_0 x_1 w_3 1 ?x1595)))
 (let (($x779 (= $x189 (or $x57 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x72))) (_ bv0 1)))))))
 (let (($x11574 (forall ((w (_ BitVec 256)) )(let ((?x2359 (storage_s x_0 x_1 w_3 0 w)))
 (let ((?x6128 (storage_s x_0 x_1 w_3 1 w)))
 (= ?x6128 ?x2359))))
 ))
 (let (($x4054 (forall ((n (_ BitVec 6)) )(let ((?x246 (stack_s x_0 x_1 w_3 0 n)))
 (let ((?x9225 (stack_s x_0 x_1 w_3 1 n)))
 (let ((?x72 (sc_s 0)))
 (let (($x3960 (bvsle ?x72 n)))
 (or $x3960 (= ?x9225 ?x246)))))))
 ))
 (let (($x3074 (= ?x154 (bvadd (_ bv1 6) ?x72))))
 (let (($x5843 (forall ((w (_ BitVec 256)) )(let ((?x2359 (storage_s x_0 x_1 w_3 0 w)))
 (= ?x2359 (_ bv0 256))))
 ))
 (let (($x5269 (= ?x5149 0)))
 (let (($x3082 (not $x57)))
 (let (($x5394 (= (stack_s x_0 x_1 w_3 0 (_ bv1 6)) x_1)))
 (let (($x1684 (= (stack_s x_0 x_1 w_3 0 (_ bv0 6)) x_0)))
 (let (($x6396 (= ?x72 (_ bv2 6))))
 (and $x6396 $x1684 $x5394 $x3082 $x5269 $x5843 (= (stack_s x_0 x_1 w_3 1 ?x72) w_3) (= (used_gas_s x_0 x_1 w_3 1) (+ 3 ?x5149)) $x3074 $x4054 $x11574 $x779 (= (stack_s x_0 x_1 w_3 2 (bvadd (_ bv63 6) ?x218)) ?x1698) (= (stack_s x_0 x_1 w_3 2 ?x1595) ?x1698) $x8042 $x656 (= ?x1314 (+ 3 (used_gas_s x_0 x_1 w_3 1))) $x9893 $x9168 $x8751 (= $x247 (or $x189 (not (bvsle (_ bv0 6) ?x1595)) $x5228)) (= (stack_s x_0 x_1 w_3 3 ?x218) (_ bv0 256)) (= ?x8033 (+ 3 ?x1314)) $x1061 $x5793 $x5345 $x6699 (= ?x10450 (bvnot (stack_s x_0 x_1 w_3 3 (bvadd (_ bv63 6) ?x275)))) (= ?x8390 (+ 3 ?x8033)) $x8378 $x714 $x8036 $x11515 (= (stack_s x_0 x_1 w_3 5 (bvadd (_ bv63 6) ?x4319)) (bvnot ?x11713)) (= ?x1387 (+ 3 ?x8390)) $x6275 $x7767 $x5634 $x2852 (= (stack_s x_0 x_1 w_3 6 ?x4319) w_3) $x10741 $x7083 $x4820 $x1332 $x1378 (= (stack_t x_0 x_1 w_3 1 ?x63) w_3) (= (used_gas_t x_0 x_1 w_3 1) (+ 3 ?x6317)) $x10883 $x10259 $x6071 $x1753 (= ?x6398 ?x9654) (= (stack_t x_0 x_1 w_3 2 ?x7196) ?x9654) $x7574 $x6981 (= ?x2013 (+ 3 (used_gas_t x_0 x_1 w_3 1))) $x4440 $x10051 $x1692 (= $x5252 (or $x6122 $x8039 (not (bvsle (_ bv0 6) ?x7196)))) (= (stack_t x_0 x_1 w_3 3 (bvadd (_ bv63 6) ?x6438)) ?x7798) (= (stack_t x_0 x_1 w_3 3 ?x1614) ?x7798) (= (stack_t x_0 x_1 w_3 3 ?x11413) ?x6398) $x11136 $x8875 $x11209 $x9774 (= $x6783 (or $x4830 $x5252 $x8185)) $x73 $x4330 $x58 $x7042 $x2854 (not (and $x5324 $x1664 $x3870 $x11808))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
 )
(check-sat)
(get-proof)
