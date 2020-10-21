; PUSH cw_0 SWAP2 DUP2 SWAP3 POP => DUP1 SWAP2
(set-option :produce-proofs true)
; 
(set-info :status unknown)
(declare-fun storage_t ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 256)) (_ BitVec 256))
(declare-fun w_0 () (_ BitVec 256))
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
 (exists ((x_0 (_ BitVec 256)) (x_1 (_ BitVec 256)) )(let (($x10658 (forall ((w (_ BitVec 256)) )(let ((?x5159 (storage_t x_0 x_1 w_0 2 w)))
 (let ((?x3554 (storage_s x_0 x_1 w_0 5 w)))
 (= ?x3554 ?x5159))))
 ))
 (let (($x903 (exc_halt_t 2)))
 (let (($x3979 (exc_halt_s 5)))
 (let (($x4108 (= $x3979 $x903)))
 (let (($x9945 (forall ((n (_ BitVec 6)) )(let ((?x4056 (sc_t 2)))
 (let (($x10759 (bvsle ?x4056 n)))
 (let ((?x4859 (stack_t x_0 x_1 w_0 2 n)))
 (let ((?x2573 (stack_s x_0 x_1 w_0 5 n)))
 (let (($x8582 (= ?x2573 ?x4859)))
 (or $x8582 $x10759)))))))
 ))
 (let ((?x4056 (sc_t 2)))
 (let ((?x805 (sc_s 5)))
 (let (($x5455 (= ?x805 ?x4056)))
 (let ((?x10078 (used_gas_t x_0 x_1 w_0 0)))
 (let ((?x8576 (used_gas_s x_0 x_1 w_0 0)))
 (let (($x1945 (= ?x8576 ?x10078)))
 (let (($x2256 (forall ((w (_ BitVec 256)) )(let ((?x10392 (storage_t x_0 x_1 w_0 0 w)))
 (let ((?x9730 (storage_s x_0 x_1 w_0 0 w)))
 (= ?x9730 ?x10392))))
 ))
 (let (($x56 (exc_halt_t 0)))
 (let (($x57 (exc_halt_s 0)))
 (let (($x58 (= $x57 $x56)))
 (let (($x744 (forall ((n (_ BitVec 6)) )(let ((?x7803 (stack_t x_0 x_1 w_0 0 n)))
 (let ((?x2463 (stack_s x_0 x_1 w_0 0 n)))
 (let (($x9214 (= ?x2463 ?x7803)))
 (let ((?x63 (sc_t 0)))
 (let (($x515 (bvsle ?x63 n)))
 (or $x515 $x9214)))))))
 ))
 (let ((?x63 (sc_t 0)))
 (let ((?x72 (sc_s 0)))
 (let (($x73 (= ?x72 ?x63)))
 (let (($x10926 (= $x903 (or (exc_halt_t 1) (not (bvsle (_ bv0 6) (bvadd (_ bv61 6) (sc_t 1))))))))
 (let (($x1030 (forall ((w (_ BitVec 256)) )(let ((?x9309 (storage_t x_0 x_1 w_0 1 w)))
 (let ((?x5159 (storage_t x_0 x_1 w_0 2 w)))
 (= ?x5159 ?x9309))))
 ))
 (let (($x11374 (forall ((n (_ BitVec 6)) )(let ((?x1172 (stack_t x_0 x_1 w_0 1 n)))
 (let ((?x4859 (stack_t x_0 x_1 w_0 2 n)))
 (let (($x3207 (= ?x4859 ?x1172)))
 (or $x3207 (bvsle (bvadd (_ bv61 6) (sc_t 1)) n))))))
 ))
 (let ((?x4023 (sc_t 1)))
 (let (($x10802 (= ?x4056 ?x4023)))
 (let (($x7475 (= (used_gas_t x_0 x_1 w_0 2) (+ 3 (used_gas_t x_0 x_1 w_0 1)))))
 (let ((?x3913 (bvadd (_ bv62 6) ?x4023)))
 (let ((?x130 (stack_t x_0 x_1 w_0 1 ?x3913)))
 (let ((?x10675 (bvadd (_ bv63 6) ?x4023)))
 (let ((?x9811 (stack_t x_0 x_1 w_0 1 ?x10675)))
 (let ((?x4110 (bvadd (_ bv63 6) ?x4056)))
 (let ((?x7563 (stack_t x_0 x_1 w_0 2 ?x4110)))
 (let (($x4496 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x63))) (_ bv0 1)))))
 (let (($x2368 (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) ?x63)))))
 (let (($x1920 (exc_halt_t 1)))
 (let (($x10426 (forall ((w (_ BitVec 256)) )(let ((?x10392 (storage_t x_0 x_1 w_0 0 w)))
 (let ((?x9309 (storage_t x_0 x_1 w_0 1 w)))
 (= ?x9309 ?x10392))))
 ))
 (let (($x590 (forall ((n (_ BitVec 6)) )(let ((?x63 (sc_t 0)))
 (let ((?x3266 (bvadd (_ bv63 6) ?x63)))
 (let (($x7966 (bvsle ?x3266 n)))
 (let ((?x7803 (stack_t x_0 x_1 w_0 0 n)))
 (let ((?x1172 (stack_t x_0 x_1 w_0 1 n)))
 (let (($x5502 (= ?x1172 ?x7803)))
 (or $x5502 $x7966))))))))
 ))
 (let (($x6031 (= ?x4023 (bvadd (_ bv1 6) ?x63))))
 (let ((?x5538 (used_gas_t x_0 x_1 w_0 1)))
 (let (($x7328 (= ?x5538 (+ 3 ?x10078))))
 (let ((?x3266 (bvadd (_ bv63 6) ?x63)))
 (let ((?x2488 (stack_t x_0 x_1 w_0 0 ?x3266)))
 (let (($x8550 (= $x3979 (or (exc_halt_s 4) (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_s 4))))))))
 (let (($x7077 (forall ((w (_ BitVec 256)) )(let ((?x3235 (storage_s x_0 x_1 w_0 4 w)))
 (let ((?x3554 (storage_s x_0 x_1 w_0 5 w)))
 (= ?x3554 ?x3235))))
 ))
 (let (($x3026 (forall ((n (_ BitVec 6)) )(let ((?x6088 (stack_s x_0 x_1 w_0 4 n)))
 (let ((?x2573 (stack_s x_0 x_1 w_0 5 n)))
 (let ((?x4305 (sc_s 4)))
 (let ((?x4463 (bvadd (_ bv63 6) ?x4305)))
 (let (($x322 (bvsle ?x4463 n)))
 (or $x322 (= ?x2573 ?x6088))))))))
 ))
 (let ((?x4305 (sc_s 4)))
 (let ((?x4463 (bvadd (_ bv63 6) ?x4305)))
 (let (($x9028 (= ?x805 ?x4463)))
 (let ((?x3739 (used_gas_s x_0 x_1 w_0 5)))
 (let (($x11389 (= ?x3739 (+ 2 (used_gas_s x_0 x_1 w_0 4)))))
 (let (($x64 (exc_halt_s 4)))
 (let (($x7730 (= $x64 (or (exc_halt_s 3) (not (bvsle (_ bv0 6) (bvadd (_ bv60 6) (sc_s 3))))))))
 (let (($x181 (forall ((w (_ BitVec 256)) )(let ((?x686 (storage_s x_0 x_1 w_0 3 w)))
 (let ((?x3235 (storage_s x_0 x_1 w_0 4 w)))
 (= ?x3235 ?x686))))
 ))
 (let (($x5450 (forall ((n (_ BitVec 6)) )(let ((?x4130 (stack_s x_0 x_1 w_0 3 n)))
 (let ((?x6088 (stack_s x_0 x_1 w_0 4 n)))
 (or (bvsle (bvadd (_ bv60 6) (sc_s 3)) n) (= ?x6088 ?x4130)))))
 ))
 (let ((?x275 (sc_s 3)))
 (let (($x9096 (= ?x4305 ?x275)))
 (let ((?x3699 (used_gas_s x_0 x_1 w_0 4)))
 (let (($x6164 (= ?x3699 (+ 3 (used_gas_s x_0 x_1 w_0 3)))))
 (let (($x7371 (= (stack_s x_0 x_1 w_0 4 (bvadd (_ bv62 6) ?x4305)) (stack_s x_0 x_1 w_0 3 (bvadd (_ bv62 6) ?x275)))))
 (let (($x5858 (= (stack_s x_0 x_1 w_0 4 (bvadd (_ bv61 6) ?x4305)) (stack_s x_0 x_1 w_0 3 (bvadd (_ bv61 6) ?x275)))))
 (let ((?x7729 (bvadd (_ bv63 6) ?x275)))
 (let ((?x3725 (stack_s x_0 x_1 w_0 3 ?x7729)))
 (let (($x11349 (= (stack_s x_0 x_1 w_0 4 (bvadd (_ bv60 6) ?x4305)) ?x3725)))
 (let (($x6980 (= (stack_s x_0 x_1 w_0 4 ?x4463) (stack_s x_0 x_1 w_0 3 (bvadd (_ bv60 6) ?x275)))))
 (let (($x1536 (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 2))))))
 (let (($x1186 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 2)))) (_ bv0 1)))))
 (let (($x247 (exc_halt_s 2)))
 (let (($x292 (exc_halt_s 3)))
 (let (($x10499 (= $x292 (or $x247 $x1186 $x1536))))
 (let (($x10843 (forall ((w (_ BitVec 256)) )(let ((?x3885 (storage_s x_0 x_1 w_0 2 w)))
 (let ((?x686 (storage_s x_0 x_1 w_0 3 w)))
 (= ?x686 ?x3885))))
 ))
 (let (($x5426 (forall ((n (_ BitVec 6)) )(let ((?x3769 (stack_s x_0 x_1 w_0 2 n)))
 (let ((?x4130 (stack_s x_0 x_1 w_0 3 n)))
 (let ((?x218 (sc_s 2)))
 (let ((?x2547 (bvadd (_ bv62 6) ?x218)))
 (let (($x1575 (bvsle ?x2547 n)))
 (or $x1575 (= ?x4130 ?x3769))))))))
 ))
 (let (($x10093 (= ?x275 (bvadd (_ bv1 6) (sc_s 2)))))
 (let ((?x6480 (used_gas_s x_0 x_1 w_0 3)))
 (let (($x10347 (= ?x6480 (+ 3 (used_gas_s x_0 x_1 w_0 2)))))
 (let ((?x218 (sc_s 2)))
 (let ((?x5628 (bvadd (_ bv63 6) ?x218)))
 (let ((?x662 (stack_s x_0 x_1 w_0 2 ?x5628)))
 (let (($x841 (= (stack_s x_0 x_1 w_0 3 ?x5628) ?x662)))
 (let ((?x2547 (bvadd (_ bv62 6) ?x218)))
 (let ((?x5485 (stack_s x_0 x_1 w_0 2 ?x2547)))
 (let (($x1163 (= (stack_s x_0 x_1 w_0 3 ?x2547) ?x5485)))
 (let (($x25 (= ?x3725 ?x5485)))
 (let (($x918 (= $x247 (or (exc_halt_s 1) (not (bvsle (_ bv0 6) (bvadd (_ bv61 6) (sc_s 1))))))))
 (let (($x5554 (forall ((w (_ BitVec 256)) )(let ((?x6470 (storage_s x_0 x_1 w_0 1 w)))
 (let ((?x3885 (storage_s x_0 x_1 w_0 2 w)))
 (= ?x3885 ?x6470))))
 ))
 (let (($x764 (forall ((n (_ BitVec 6)) )(let ((?x154 (sc_s 1)))
 (let ((?x5002 (bvadd (_ bv61 6) ?x154)))
 (let (($x6782 (bvsle ?x5002 n)))
 (let ((?x9752 (stack_s x_0 x_1 w_0 1 n)))
 (let ((?x3769 (stack_s x_0 x_1 w_0 2 n)))
 (or (= ?x3769 ?x9752) $x6782)))))))
 ))
 (let ((?x154 (sc_s 1)))
 (let (($x1121 (= ?x218 ?x154)))
 (let ((?x2027 (used_gas_s x_0 x_1 w_0 2)))
 (let (($x6117 (= ?x2027 (+ 3 (used_gas_s x_0 x_1 w_0 1)))))
 (let (($x6520 (= ?x5485 (stack_s x_0 x_1 w_0 1 (bvadd (_ bv62 6) ?x154)))))
 (let (($x7621 (= (stack_s x_0 x_1 w_0 2 (bvadd (_ bv61 6) ?x218)) (stack_s x_0 x_1 w_0 1 (bvadd (_ bv63 6) ?x154)))))
 (let (($x4817 (= ?x662 (stack_s x_0 x_1 w_0 1 (bvadd (_ bv61 6) ?x154)))))
 (let (($x189 (exc_halt_s 1)))
 (let (($x2279 (= $x189 (or $x57 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x72))) (_ bv0 1)))))))
 (let (($x1969 (forall ((w (_ BitVec 256)) )(let ((?x9730 (storage_s x_0 x_1 w_0 0 w)))
 (let ((?x6470 (storage_s x_0 x_1 w_0 1 w)))
 (= ?x6470 ?x9730))))
 ))
 (let (($x9457 (forall ((n (_ BitVec 6)) )(let ((?x2463 (stack_s x_0 x_1 w_0 0 n)))
 (let ((?x9752 (stack_s x_0 x_1 w_0 1 n)))
 (let ((?x72 (sc_s 0)))
 (let (($x2895 (bvsle ?x72 n)))
 (or $x2895 (= ?x9752 ?x2463)))))))
 ))
 (let (($x1866 (= ?x154 (bvadd (_ bv1 6) ?x72))))
 (let ((?x5602 (used_gas_s x_0 x_1 w_0 1)))
 (let (($x3567 (= ?x5602 (+ 3 ?x8576))))
 (let (($x8943 (= (stack_s x_0 x_1 w_0 1 ?x72) w_0)))
 (let (($x5863 (forall ((w (_ BitVec 256)) )(let ((?x9730 (storage_s x_0 x_1 w_0 0 w)))
 (= ?x9730 (_ bv0 256))))
 ))
 (let (($x5537 (= ?x8576 0)))
 (let (($x2460 (not $x57)))
 (let (($x3491 (= (stack_s x_0 x_1 w_0 0 (_ bv1 6)) x_1)))
 (let (($x10243 (= (stack_s x_0 x_1 w_0 0 (_ bv0 6)) x_0)))
 (let (($x7626 (= ?x72 (_ bv2 6))))
 (and $x7626 $x10243 $x3491 $x2460 $x5537 $x5863 $x8943 $x3567 $x1866 $x9457 $x1969 $x2279 $x4817 $x7621 $x6520 $x6117 $x1121 $x764 $x5554 $x918 $x25 $x1163 $x841 $x10347 $x10093 $x5426 $x10843 $x10499 $x6980 $x11349 $x5858 $x7371 $x6164 $x9096 $x5450 $x181 $x7730 $x11389 $x9028 $x3026 $x7077 $x8550 (= ?x9811 ?x2488) (= (stack_t x_0 x_1 w_0 1 ?x3266) ?x2488) $x7328 $x6031 $x590 $x10426 (= $x1920 (or $x56 $x2368 $x4496)) (= ?x7563 (stack_t x_0 x_1 w_0 1 (bvadd (_ bv61 6) ?x4023))) (= (stack_t x_0 x_1 w_0 2 (bvadd (_ bv61 6) ?x4056)) ?x9811) (= (stack_t x_0 x_1 w_0 2 (bvadd (_ bv62 6) ?x4056)) ?x130) $x7475 $x10802 $x11374 $x1030 $x10926 $x73 $x744 $x58 $x2256 $x1945 (not (and $x5455 $x9945 $x4108 $x10658)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
 )
(check-sat)
(get-proof)
