; PUSH cw_1 SWAP1 DUP3 SWAP1 DUP3 SWAP1 => DUP2 PUSH cw_1 DUP1 SWAP3
(set-option :produce-proofs true)
; 
(set-info :status unknown)
(declare-fun storage_t ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 256)) (_ BitVec 256))
(declare-fun w_1 () (_ BitVec 256))
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
 (exists ((x_0 (_ BitVec 256)) (x_1 (_ BitVec 256)) )(let (($x2414 (forall ((w (_ BitVec 256)) )(let ((?x2186 (storage_t x_0 x_1 w_1 4 w)))
 (let ((?x7721 (storage_s x_0 x_1 w_1 6 w)))
 (= ?x7721 ?x2186))))
 ))
 (let (($x3723 (exc_halt_t 4)))
 (let (($x772 (exc_halt_s 6)))
 (let (($x10342 (= $x772 $x3723)))
 (let (($x11577 (forall ((n (_ BitVec 6)) )(let ((?x1098 (sc_t 4)))
 (let (($x10002 (bvsle ?x1098 n)))
 (let ((?x11709 (stack_t x_0 x_1 w_1 4 n)))
 (let ((?x7665 (stack_s x_0 x_1 w_1 6 n)))
 (let (($x5995 (= ?x7665 ?x11709)))
 (or $x5995 $x10002)))))))
 ))
 (let ((?x1098 (sc_t 4)))
 (let ((?x926 (sc_s 6)))
 (let (($x8120 (= ?x926 ?x1098)))
 (let ((?x8072 (used_gas_t x_0 x_1 w_1 0)))
 (let ((?x6855 (used_gas_s x_0 x_1 w_1 0)))
 (let (($x6782 (= ?x6855 ?x8072)))
 (let (($x2751 (forall ((w (_ BitVec 256)) )(let ((?x7476 (storage_t x_0 x_1 w_1 0 w)))
 (let ((?x5559 (storage_s x_0 x_1 w_1 0 w)))
 (= ?x5559 ?x7476))))
 ))
 (let (($x56 (exc_halt_t 0)))
 (let (($x57 (exc_halt_s 0)))
 (let (($x58 (= $x57 $x56)))
 (let (($x7366 (forall ((n (_ BitVec 6)) )(let ((?x1412 (stack_t x_0 x_1 w_1 0 n)))
 (let ((?x950 (stack_s x_0 x_1 w_1 0 n)))
 (let (($x6210 (= ?x950 ?x1412)))
 (let ((?x63 (sc_t 0)))
 (let (($x5367 (bvsle ?x63 n)))
 (or $x5367 $x6210)))))))
 ))
 (let ((?x63 (sc_t 0)))
 (let ((?x72 (sc_s 0)))
 (let (($x73 (= ?x72 ?x63)))
 (let (($x4558 (= $x3723 (or (exc_halt_t 3) (not (bvsle (_ bv0 6) (bvadd (_ bv60 6) (sc_t 3))))))))
 (let (($x2688 (forall ((w (_ BitVec 256)) )(let ((?x946 (storage_t x_0 x_1 w_1 3 w)))
 (let ((?x2186 (storage_t x_0 x_1 w_1 4 w)))
 (= ?x2186 ?x946))))
 ))
 (let (($x6603 (forall ((n (_ BitVec 6)) )(let ((?x5472 (stack_t x_0 x_1 w_1 3 n)))
 (let ((?x11709 (stack_t x_0 x_1 w_1 4 n)))
 (let (($x11539 (= ?x11709 ?x5472)))
 (or (bvsle (bvadd (_ bv60 6) (sc_t 3)) n) $x11539)))))
 ))
 (let (($x7804 (= (used_gas_t x_0 x_1 w_1 4) (+ 3 (used_gas_t x_0 x_1 w_1 3)))))
 (let ((?x6438 (sc_t 3)))
 (let ((?x8179 (bvadd (_ bv62 6) ?x6438)))
 (let ((?x11784 (stack_t x_0 x_1 w_1 3 ?x8179)))
 (let (($x9667 (= (stack_t x_0 x_1 w_1 4 (bvadd (_ bv61 6) ?x1098)) (stack_t x_0 x_1 w_1 3 (bvadd (_ bv61 6) ?x6438)))))
 (let ((?x8696 (bvadd (_ bv63 6) ?x6438)))
 (let ((?x2674 (stack_t x_0 x_1 w_1 3 ?x8696)))
 (let ((?x2869 (stack_t x_0 x_1 w_1 4 (bvadd (_ bv63 6) ?x1098))))
 (let (($x8421 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_t 2)))) (_ bv0 1)))))
 (let (($x5252 (exc_halt_t 2)))
 (let (($x6783 (exc_halt_t 3)))
 (let (($x3542 (forall ((w (_ BitVec 256)) )(let ((?x2895 (storage_t x_0 x_1 w_1 2 w)))
 (let ((?x946 (storage_t x_0 x_1 w_1 3 w)))
 (= ?x946 ?x2895))))
 ))
 (let (($x11331 (forall ((n (_ BitVec 6)) )(let ((?x10440 (stack_t x_0 x_1 w_1 2 n)))
 (let ((?x5472 (stack_t x_0 x_1 w_1 3 n)))
 (let (($x2274 (= ?x5472 ?x10440)))
 (or $x2274 (bvsle (bvadd (_ bv63 6) (sc_t 2)) n))))))
 ))
 (let (($x4159 (= ?x6438 (bvadd (_ bv1 6) (sc_t 2)))))
 (let ((?x8112 (used_gas_t x_0 x_1 w_1 3)))
 (let (($x10752 (= ?x8112 (+ 3 (used_gas_t x_0 x_1 w_1 2)))))
 (let ((?x2714 (sc_t 2)))
 (let ((?x1119 (bvadd (_ bv63 6) ?x2714)))
 (let ((?x10374 (stack_t x_0 x_1 w_1 2 ?x1119)))
 (let (($x6002 (= (stack_t x_0 x_1 w_1 3 ?x1119) ?x10374)))
 (let (($x4225 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_t 1)))) (_ bv0 1)))))
 (let (($x3508 (exc_halt_t 1)))
 (let (($x9350 (= $x5252 (or $x3508 $x4225))))
 (let (($x4124 (forall ((w (_ BitVec 256)) )(let ((?x10751 (storage_t x_0 x_1 w_1 1 w)))
 (let ((?x2895 (storage_t x_0 x_1 w_1 2 w)))
 (= ?x2895 ?x10751))))
 ))
 (let (($x1663 (forall ((n (_ BitVec 6)) )(let ((?x7703 (stack_t x_0 x_1 w_1 1 n)))
 (let ((?x10440 (stack_t x_0 x_1 w_1 2 n)))
 (let (($x5085 (= ?x10440 ?x7703)))
 (let ((?x8347 (sc_t 1)))
 (let (($x420 (bvsle ?x8347 n)))
 (or $x420 $x5085)))))))
 ))
 (let (($x6339 (= ?x2714 (bvadd (_ bv1 6) (sc_t 1)))))
 (let ((?x11372 (used_gas_t x_0 x_1 w_1 2)))
 (let (($x3048 (= ?x11372 (+ 3 (used_gas_t x_0 x_1 w_1 1)))))
 (let (($x4784 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x63))) (_ bv0 1)))))
 (let (($x10563 (= $x3508 (or $x56 (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) ?x63))) $x4784))))
 (let (($x3918 (forall ((w (_ BitVec 256)) )(let ((?x7476 (storage_t x_0 x_1 w_1 0 w)))
 (let ((?x10751 (storage_t x_0 x_1 w_1 1 w)))
 (= ?x10751 ?x7476))))
 ))
 (let (($x7368 (forall ((n (_ BitVec 6)) )(let ((?x63 (sc_t 0)))
 (let ((?x10150 (bvadd (_ bv62 6) ?x63)))
 (let (($x4902 (bvsle ?x10150 n)))
 (let ((?x1412 (stack_t x_0 x_1 w_1 0 n)))
 (let ((?x7703 (stack_t x_0 x_1 w_1 1 n)))
 (let (($x1582 (= ?x7703 ?x1412)))
 (or $x1582 $x4902))))))))
 ))
 (let ((?x8347 (sc_t 1)))
 (let (($x9887 (= ?x8347 (bvadd (_ bv1 6) ?x63))))
 (let ((?x1950 (used_gas_t x_0 x_1 w_1 1)))
 (let (($x11151 (= ?x1950 (+ 3 ?x8072))))
 (let (($x8014 (= (stack_t x_0 x_1 w_1 1 (bvadd (_ bv63 6) ?x63)) (stack_t x_0 x_1 w_1 0 (bvadd (_ bv63 6) ?x63)))))
 (let ((?x10150 (bvadd (_ bv62 6) ?x63)))
 (let ((?x8381 (stack_t x_0 x_1 w_1 0 ?x10150)))
 (let (($x6416 (= $x772 (or (exc_halt_s 5) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 5))))))))
 (let (($x6978 (forall ((w (_ BitVec 256)) )(let ((?x6479 (storage_s x_0 x_1 w_1 5 w)))
 (let ((?x7721 (storage_s x_0 x_1 w_1 6 w)))
 (= ?x7721 ?x6479))))
 ))
 (let (($x8057 (forall ((n (_ BitVec 6)) )(let ((?x9678 (stack_s x_0 x_1 w_1 5 n)))
 (let ((?x7665 (stack_s x_0 x_1 w_1 6 n)))
 (or (= ?x7665 ?x9678) (bvsle (bvadd (_ bv62 6) (sc_s 5)) n)))))
 ))
 (let (($x5974 (= (used_gas_s x_0 x_1 w_1 6) (+ 3 (used_gas_s x_0 x_1 w_1 5)))))
 (let ((?x4319 (sc_s 5)))
 (let ((?x4935 (bvadd (_ bv63 6) ?x4319)))
 (let ((?x6347 (stack_s x_0 x_1 w_1 5 ?x4935)))
 (let ((?x4212 (bvadd (_ bv62 6) ?x4319)))
 (let ((?x4856 (stack_s x_0 x_1 w_1 5 ?x4212)))
 (let (($x7172 (exc_halt_s 4)))
 (let (($x5067 (or (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 4)))) (_ bv0 1))) (not (bvsle (_ bv0 6) (bvadd (_ bv61 6) (sc_s 4)))) $x7172)))
 (let (($x11317 (exc_halt_s 5)))
 (let (($x552 (= $x11317 $x5067)))
 (let (($x9371 (forall ((w (_ BitVec 256)) )(let ((?x4372 (storage_s x_0 x_1 w_1 4 w)))
 (let ((?x6479 (storage_s x_0 x_1 w_1 5 w)))
 (= ?x6479 ?x4372))))
 ))
 (let (($x11663 (forall ((n (_ BitVec 6)) )(let ((?x9527 (stack_s x_0 x_1 w_1 4 n)))
 (let ((?x9678 (stack_s x_0 x_1 w_1 5 n)))
 (let (($x10844 (= ?x9678 ?x9527)))
 (let ((?x4305 (sc_s 4)))
 (let ((?x142 (bvadd (_ bv61 6) ?x4305)))
 (let (($x3982 (bvsle ?x142 n)))
 (or $x3982 $x10844))))))))
 ))
 (let (($x11797 (= ?x4319 (bvadd (_ bv1 6) (sc_s 4)))))
 (let ((?x2784 (used_gas_s x_0 x_1 w_1 5)))
 (let (($x8413 (= ?x2784 (+ 3 (used_gas_s x_0 x_1 w_1 4)))))
 (let ((?x4305 (sc_s 4)))
 (let ((?x1400 (bvadd (_ bv63 6) ?x4305)))
 (let ((?x11578 (stack_s x_0 x_1 w_1 4 ?x1400)))
 (let ((?x1788 (bvadd (_ bv62 6) ?x4305)))
 (let ((?x7556 (stack_s x_0 x_1 w_1 4 ?x1788)))
 (let ((?x142 (bvadd (_ bv61 6) ?x4305)))
 (let ((?x6874 (stack_s x_0 x_1 w_1 4 ?x142)))
 (let (($x2185 (= $x7172 (or (exc_halt_s 3) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 3))))))))
 (let (($x4404 (forall ((w (_ BitVec 256)) )(let ((?x979 (storage_s x_0 x_1 w_1 3 w)))
 (let ((?x4372 (storage_s x_0 x_1 w_1 4 w)))
 (= ?x4372 ?x979))))
 ))
 (let (($x8562 (forall ((n (_ BitVec 6)) )(let ((?x413 (stack_s x_0 x_1 w_1 3 n)))
 (let ((?x9527 (stack_s x_0 x_1 w_1 4 n)))
 (let (($x6619 (= ?x9527 ?x413)))
 (let ((?x275 (sc_s 3)))
 (let ((?x7390 (bvadd (_ bv62 6) ?x275)))
 (let (($x589 (bvsle ?x7390 n)))
 (or $x589 $x6619))))))))
 ))
 (let ((?x275 (sc_s 3)))
 (let (($x5018 (= ?x4305 ?x275)))
 (let ((?x3642 (used_gas_s x_0 x_1 w_1 4)))
 (let (($x6772 (= ?x3642 (+ 3 (used_gas_s x_0 x_1 w_1 3)))))
 (let (($x247 (exc_halt_s 2)))
 (let (($x1686 (or $x247 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 2)))) (_ bv0 1))) (not (bvsle (_ bv0 6) (bvadd (_ bv61 6) (sc_s 2)))))))
 (let (($x292 (exc_halt_s 3)))
 (let (($x1603 (= $x292 $x1686)))
 (let (($x8630 (forall ((w (_ BitVec 256)) )(let ((?x6943 (storage_s x_0 x_1 w_1 2 w)))
 (let ((?x979 (storage_s x_0 x_1 w_1 3 w)))
 (= ?x979 ?x6943))))
 ))
 (let (($x10670 (forall ((n (_ BitVec 6)) )(let ((?x7287 (stack_s x_0 x_1 w_1 2 n)))
 (let ((?x413 (stack_s x_0 x_1 w_1 3 n)))
 (let (($x6348 (= ?x413 ?x7287)))
 (let ((?x218 (sc_s 2)))
 (let ((?x1288 (bvadd (_ bv61 6) ?x218)))
 (let (($x672 (bvsle ?x1288 n)))
 (or $x672 $x6348))))))))
 ))
 (let (($x6437 (= ?x275 (bvadd (_ bv1 6) (sc_s 2)))))
 (let ((?x5323 (used_gas_s x_0 x_1 w_1 3)))
 (let (($x3996 (= ?x5323 (+ 3 (used_gas_s x_0 x_1 w_1 2)))))
 (let ((?x218 (sc_s 2)))
 (let ((?x6273 (bvadd (_ bv63 6) ?x218)))
 (let ((?x5150 (stack_s x_0 x_1 w_1 2 ?x6273)))
 (let ((?x6994 (bvadd (_ bv62 6) ?x218)))
 (let ((?x7514 (stack_s x_0 x_1 w_1 2 ?x6994)))
 (let ((?x1288 (bvadd (_ bv61 6) ?x218)))
 (let ((?x9049 (stack_s x_0 x_1 w_1 2 ?x1288)))
 (let (($x1299 (= $x247 (or (exc_halt_s 1) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 1))))))))
 (let (($x3913 (forall ((w (_ BitVec 256)) )(let ((?x11491 (storage_s x_0 x_1 w_1 1 w)))
 (let ((?x6943 (storage_s x_0 x_1 w_1 2 w)))
 (= ?x6943 ?x11491))))
 ))
 (let (($x8973 (forall ((n (_ BitVec 6)) )(let ((?x154 (sc_s 1)))
 (let ((?x1306 (bvadd (_ bv62 6) ?x154)))
 (let (($x9742 (bvsle ?x1306 n)))
 (let ((?x27 (stack_s x_0 x_1 w_1 1 n)))
 (let ((?x7287 (stack_s x_0 x_1 w_1 2 n)))
 (let (($x6551 (= ?x7287 ?x27)))
 (or $x6551 $x9742))))))))
 ))
 (let ((?x154 (sc_s 1)))
 (let (($x7496 (= ?x218 ?x154)))
 (let ((?x10530 (used_gas_s x_0 x_1 w_1 2)))
 (let (($x2724 (= ?x10530 (+ 3 (used_gas_s x_0 x_1 w_1 1)))))
 (let (($x189 (exc_halt_s 1)))
 (let (($x6475 (= $x189 (or $x57 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x72))) (_ bv0 1)))))))
 (let (($x9470 (forall ((w (_ BitVec 256)) )(let ((?x5559 (storage_s x_0 x_1 w_1 0 w)))
 (let ((?x11491 (storage_s x_0 x_1 w_1 1 w)))
 (= ?x11491 ?x5559))))
 ))
 (let (($x10523 (forall ((n (_ BitVec 6)) )(let ((?x950 (stack_s x_0 x_1 w_1 0 n)))
 (let ((?x27 (stack_s x_0 x_1 w_1 1 n)))
 (let (($x9080 (= ?x27 ?x950)))
 (let ((?x72 (sc_s 0)))
 (let (($x202 (bvsle ?x72 n)))
 (or $x202 $x9080)))))))
 ))
 (let (($x1612 (= ?x154 (bvadd (_ bv1 6) ?x72))))
 (let ((?x9232 (used_gas_s x_0 x_1 w_1 1)))
 (let (($x7342 (= ?x9232 (+ 3 ?x6855))))
 (let (($x5364 (forall ((w (_ BitVec 256)) )(let ((?x5559 (storage_s x_0 x_1 w_1 0 w)))
 (= ?x5559 (_ bv0 256))))
 ))
 (let (($x7614 (= ?x6855 0)))
 (let (($x3176 (not $x57)))
 (let (($x6973 (= (stack_s x_0 x_1 w_1 0 (_ bv1 6)) x_1)))
 (let (($x5433 (= (stack_s x_0 x_1 w_1 0 (_ bv0 6)) x_0)))
 (let (($x6396 (= ?x72 (_ bv2 6))))
 (and $x6396 $x5433 $x6973 $x3176 $x7614 $x5364 (= (stack_s x_0 x_1 w_1 1 ?x72) w_1) $x7342 $x1612 $x10523 $x9470 $x6475 (= ?x5150 (stack_s x_0 x_1 w_1 1 (bvadd (_ bv62 6) ?x154))) (= ?x7514 (stack_s x_0 x_1 w_1 1 (bvadd (_ bv63 6) ?x154))) $x2724 $x7496 $x8973 $x3913 $x1299 (= (stack_s x_0 x_1 w_1 3 (bvadd (_ bv63 6) ?x275)) ?x9049) (= (stack_s x_0 x_1 w_1 3 ?x1288) ?x9049) (= (stack_s x_0 x_1 w_1 3 ?x6994) ?x7514) (= (stack_s x_0 x_1 w_1 3 ?x6273) ?x5150) $x3996 $x6437 $x10670 $x8630 $x1603 (= ?x11578 (stack_s x_0 x_1 w_1 3 (bvadd (_ bv62 6) ?x275))) (= ?x7556 (stack_s x_0 x_1 w_1 3 (bvadd (_ bv63 6) ?x275))) $x6772 $x5018 $x8562 $x4404 $x2185 (= ?x6347 ?x6874) (= (stack_s x_0 x_1 w_1 5 ?x142) ?x6874) (= (stack_s x_0 x_1 w_1 5 ?x1788) ?x7556) (= (stack_s x_0 x_1 w_1 5 ?x1400) ?x11578) $x8413 $x11797 $x11663 $x9371 $x552 (= (stack_s x_0 x_1 w_1 6 (bvadd (_ bv63 6) ?x926)) ?x4856) (= (stack_s x_0 x_1 w_1 6 (bvadd (_ bv62 6) ?x926)) ?x6347) $x5974 (= ?x926 ?x4319) $x8057 $x6978 $x6416 (= (stack_t x_0 x_1 w_1 1 (bvadd (_ bv63 6) ?x8347)) ?x8381) (= (stack_t x_0 x_1 w_1 1 ?x10150) ?x8381) $x8014 $x11151 $x9887 $x7368 $x3918 $x10563 (= (stack_t x_0 x_1 w_1 2 ?x8347) w_1) $x3048 $x6339 $x1663 $x4124 $x9350 (= ?x2674 ?x10374) $x6002 $x10752 $x4159 $x11331 $x3542 (= $x6783 (or (not (bvsle (_ bv0 6) ?x1119)) $x5252 $x8421)) (= ?x2869 (stack_t x_0 x_1 w_1 3 (bvadd (_ bv60 6) ?x6438))) (= (stack_t x_0 x_1 w_1 4 (bvadd (_ bv60 6) ?x1098)) ?x2674) $x9667 (= (stack_t x_0 x_1 w_1 4 (bvadd (_ bv62 6) ?x1098)) ?x11784) $x7804 (= ?x1098 ?x6438) $x6603 $x2688 $x4558 $x73 $x7366 $x58 $x2751 $x6782 (not (and $x8120 $x11577 $x10342 $x2414)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
 )
(check-sat)
(get-proof)