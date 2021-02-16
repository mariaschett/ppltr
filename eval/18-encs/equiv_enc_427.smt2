; PUSH 0x00 DUP1 DUP3 MLOAD GT ISZERO => PUSH 0x00 DUP2 MLOAD ISZERO
(set-option :produce-proofs true)
; 
(set-info :status unknown)
(declare-fun storage_t ((_ BitVec 256) (_ BitVec 256) Int (_ BitVec 256)) (_ BitVec 256))
(declare-fun storage_s ((_ BitVec 256) (_ BitVec 256) Int (_ BitVec 256)) (_ BitVec 256))
(declare-fun exc_halt_t (Int) Bool)
(declare-fun exc_halt_s (Int) Bool)
(declare-fun stack_t ((_ BitVec 256) (_ BitVec 256) Int (_ BitVec 6)) (_ BitVec 256))
(declare-fun stack_s ((_ BitVec 256) (_ BitVec 256) Int (_ BitVec 6)) (_ BitVec 256))
(declare-fun sc_t (Int) (_ BitVec 6))
(declare-fun sc_s (Int) (_ BitVec 6))
(declare-fun used_gas_t ((_ BitVec 256) (_ BitVec 256) Int) Int)
(declare-fun used_gas_s ((_ BitVec 256) (_ BitVec 256) Int) Int)
(declare-fun f_MLOAD ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256)) (_ BitVec 256))
(assert
 (exists ((x_0 (_ BitVec 256)) (x_MLOAD_0 (_ BitVec 256)) )(let (($x5475 (forall ((w (_ BitVec 256)) )(let ((?x4187 (storage_t x_0 x_MLOAD_0 4 w)))
 (let ((?x8646 (storage_s x_0 x_MLOAD_0 6 w)))
 (= ?x8646 ?x4187))))
 ))
 (let (($x7722 (exc_halt_t 4)))
 (let (($x772 (exc_halt_s 6)))
 (let (($x5620 (= $x772 $x7722)))
 (let (($x9748 (forall ((n (_ BitVec 6)) )(let ((?x11381 (stack_t x_0 x_MLOAD_0 4 n)))
 (let ((?x305 (stack_s x_0 x_MLOAD_0 6 n)))
 (let (($x10576 (= ?x305 ?x11381)))
 (let ((?x11631 (sc_t 4)))
 (let (($x5427 (bvsle ?x11631 n)))
 (or $x5427 $x10576)))))))
 ))
 (let ((?x11631 (sc_t 4)))
 (let ((?x926 (sc_s 6)))
 (let (($x11559 (= ?x926 ?x11631)))
 (let ((?x5416 (used_gas_t x_0 x_MLOAD_0 0)))
 (let ((?x5890 (used_gas_s x_0 x_MLOAD_0 0)))
 (let (($x5554 (= ?x5890 ?x5416)))
 (let (($x1371 (forall ((w (_ BitVec 256)) )(let ((?x10167 (storage_t x_0 x_MLOAD_0 0 w)))
 (let ((?x6107 (storage_s x_0 x_MLOAD_0 0 w)))
 (= ?x6107 ?x10167))))
 ))
 (let (($x56 (exc_halt_t 0)))
 (let (($x57 (exc_halt_s 0)))
 (let (($x58 (= $x57 $x56)))
 (let (($x8853 (forall ((n (_ BitVec 6)) )(let ((?x63 (sc_t 0)))
 (let (($x3267 (bvsle ?x63 n)))
 (let ((?x3255 (stack_t x_0 x_MLOAD_0 0 n)))
 (let ((?x3728 (stack_s x_0 x_MLOAD_0 0 n)))
 (let (($x2507 (= ?x3728 ?x3255)))
 (or $x2507 $x3267)))))))
 ))
 (let ((?x63 (sc_t 0)))
 (let ((?x72 (sc_s 0)))
 (let (($x73 (= ?x72 ?x63)))
 (let (($x1165 (= $x7722 (or (exc_halt_t 3) (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_t 3))))))))
 (let (($x5906 (forall ((w (_ BitVec 256)) )(let ((?x8905 (storage_t x_0 x_MLOAD_0 3 w)))
 (let ((?x4187 (storage_t x_0 x_MLOAD_0 4 w)))
 (= ?x4187 ?x8905))))
 ))
 (let (($x920 (forall ((n (_ BitVec 6)) )(or (= (stack_t x_0 x_MLOAD_0 4 n) (stack_t x_0 x_MLOAD_0 3 n)) (bvsle (bvadd (_ bv63 6) (sc_t 3)) n)))
 ))
 (let ((?x6438 (sc_t 3)))
 (let (($x1409 (= ?x11631 ?x6438)))
 (let (($x11192 (= (used_gas_t x_0 x_MLOAD_0 4) (+ 3 (used_gas_t x_0 x_MLOAD_0 3)))))
 (let (($x11055 (= (stack_t x_0 x_MLOAD_0 4 (bvadd (_ bv63 6) ?x11631)) (ite (= (stack_t x_0 x_MLOAD_0 3 (bvadd (_ bv63 6) ?x6438)) (_ bv0 256)) (_ bv1 256) (_ bv0 256)))))
 (let (($x6783 (exc_halt_t 3)))
 (let (($x5173 (= $x6783 (or (exc_halt_t 2) (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_t 2))))))))
 (let (($x4655 (forall ((w (_ BitVec 256)) )(let ((?x737 (storage_t x_0 x_MLOAD_0 2 w)))
 (let ((?x8905 (storage_t x_0 x_MLOAD_0 3 w)))
 (= ?x8905 ?x737))))
 ))
 (let (($x6706 (forall ((n (_ BitVec 6)) )(let ((?x2714 (sc_t 2)))
 (let ((?x5743 (bvadd (_ bv63 6) ?x2714)))
 (let (($x2467 (bvsle ?x5743 n)))
 (or $x2467 (= (stack_t x_0 x_MLOAD_0 3 n) (stack_t x_0 x_MLOAD_0 2 n)))))))
 ))
 (let ((?x2714 (sc_t 2)))
 (let (($x10179 (= ?x6438 ?x2714)))
 (let ((?x8873 (used_gas_t x_0 x_MLOAD_0 3)))
 (let ((?x11838 (bvadd (_ bv63 6) ?x6438)))
 (let ((?x8624 (stack_t x_0 x_MLOAD_0 3 ?x11838)))
 (let (($x4910 (= ?x8624 (f_MLOAD x_0 x_MLOAD_0 (stack_t x_0 x_MLOAD_0 2 (bvadd (_ bv63 6) ?x2714))))))
 (let (($x3508 (exc_halt_t 1)))
 (let (($x1972 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_t 1)))) (_ bv0 1)))))
 (let (($x523 (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_t 1))))))
 (let (($x5252 (exc_halt_t 2)))
 (let (($x9331 (= $x5252 (or $x523 $x1972 $x3508))))
 (let (($x9599 (forall ((w (_ BitVec 256)) )(let ((?x11717 (storage_t x_0 x_MLOAD_0 1 w)))
 (let ((?x737 (storage_t x_0 x_MLOAD_0 2 w)))
 (= ?x737 ?x11717))))
 ))
 (let (($x6299 (forall ((n (_ BitVec 6)) )(let ((?x8347 (sc_t 1)))
 (let ((?x6516 (bvadd (_ bv62 6) ?x8347)))
 (let (($x11603 (bvsle ?x6516 n)))
 (or (= (stack_t x_0 x_MLOAD_0 2 n) (stack_t x_0 x_MLOAD_0 1 n)) $x11603)))))
 ))
 (let (($x1064 (= ?x2714 (bvadd (_ bv1 6) (sc_t 1)))))
 (let ((?x893 (used_gas_t x_0 x_MLOAD_0 2)))
 (let (($x5009 (= (stack_t x_0 x_MLOAD_0 2 (bvadd (_ bv63 6) (sc_t 1))) (stack_t x_0 x_MLOAD_0 1 (bvadd (_ bv63 6) (sc_t 1))))))
 (let ((?x8347 (sc_t 1)))
 (let ((?x6516 (bvadd (_ bv62 6) ?x8347)))
 (let ((?x2468 (stack_t x_0 x_MLOAD_0 1 ?x6516)))
 (let (($x9448 (= $x3508 (or $x56 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x63))) (_ bv0 1)))))))
 (let (($x7630 (forall ((w (_ BitVec 256)) )(let ((?x10167 (storage_t x_0 x_MLOAD_0 0 w)))
 (let ((?x11717 (storage_t x_0 x_MLOAD_0 1 w)))
 (= ?x11717 ?x10167))))
 ))
 (let (($x5709 (forall ((n (_ BitVec 6)) )(let ((?x63 (sc_t 0)))
 (let (($x3267 (bvsle ?x63 n)))
 (or $x3267 (= (stack_t x_0 x_MLOAD_0 1 n) (stack_t x_0 x_MLOAD_0 0 n))))))
 ))
 (let (($x859 (= ?x8347 (bvadd (_ bv1 6) ?x63))))
 (let (($x6619 (= $x772 (or (exc_halt_s 5) (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_s 5))))))))
 (let (($x10930 (forall ((w (_ BitVec 256)) )(let ((?x8987 (storage_s x_0 x_MLOAD_0 5 w)))
 (let ((?x8646 (storage_s x_0 x_MLOAD_0 6 w)))
 (= ?x8646 ?x8987))))
 ))
 (let (($x393 (forall ((n (_ BitVec 6)) )(or (bvsle (bvadd (_ bv63 6) (sc_s 5)) n) (= (stack_s x_0 x_MLOAD_0 6 n) (stack_s x_0 x_MLOAD_0 5 n))))
 ))
 (let ((?x4319 (sc_s 5)))
 (let (($x2373 (= ?x926 ?x4319)))
 (let (($x9392 (= (used_gas_s x_0 x_MLOAD_0 6) (+ 3 (used_gas_s x_0 x_MLOAD_0 5)))))
 (let (($x8126 (= (stack_s x_0 x_MLOAD_0 6 (bvadd (_ bv63 6) ?x926)) (ite (= (stack_s x_0 x_MLOAD_0 5 (bvadd (_ bv63 6) ?x4319)) (_ bv0 256)) (_ bv1 256) (_ bv0 256)))))
 (let (($x11317 (exc_halt_s 5)))
 (let (($x2337 (= $x11317 (or (exc_halt_s 4) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 4))))))))
 (let (($x9959 (forall ((w (_ BitVec 256)) )(let ((?x8237 (storage_s x_0 x_MLOAD_0 4 w)))
 (let ((?x8987 (storage_s x_0 x_MLOAD_0 5 w)))
 (= ?x8987 ?x8237))))
 ))
 (let (($x185 (forall ((n (_ BitVec 6)) )(let ((?x4305 (sc_s 4)))
 (let ((?x10436 (bvadd (_ bv62 6) ?x4305)))
 (let (($x3759 (bvsle ?x10436 n)))
 (or $x3759 (= (stack_s x_0 x_MLOAD_0 5 n) (stack_s x_0 x_MLOAD_0 4 n)))))))
 ))
 (let ((?x4305 (sc_s 4)))
 (let ((?x4062 (bvadd (_ bv63 6) ?x4305)))
 (let (($x9231 (= ?x4319 ?x4062)))
 (let ((?x3650 (used_gas_s x_0 x_MLOAD_0 5)))
 (let ((?x9836 (stack_s x_0 x_MLOAD_0 4 ?x4062)))
 (let ((?x3010 (ite (bvule ?x9836 (stack_s x_0 x_MLOAD_0 4 (bvadd (_ bv62 6) ?x4305))) (_ bv0 256) (_ bv1 256))))
 (let ((?x7318 (bvadd (_ bv63 6) ?x4319)))
 (let ((?x9842 (stack_s x_0 x_MLOAD_0 5 ?x7318)))
 (let (($x7172 (exc_halt_s 4)))
 (let (($x5670 (= $x7172 (or (exc_halt_s 3) (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_s 3))))))))
 (let (($x11494 (forall ((w (_ BitVec 256)) )(let ((?x1382 (storage_s x_0 x_MLOAD_0 3 w)))
 (let ((?x8237 (storage_s x_0 x_MLOAD_0 4 w)))
 (= ?x8237 ?x1382))))
 ))
 (let (($x3383 (forall ((n (_ BitVec 6)) )(let ((?x275 (sc_s 3)))
 (let ((?x6354 (bvadd (_ bv63 6) ?x275)))
 (let (($x8340 (bvsle ?x6354 n)))
 (or (= (stack_s x_0 x_MLOAD_0 4 n) (stack_s x_0 x_MLOAD_0 3 n)) $x8340)))))
 ))
 (let ((?x7485 (used_gas_s x_0 x_MLOAD_0 4)))
 (let ((?x275 (sc_s 3)))
 (let ((?x6354 (bvadd (_ bv63 6) ?x275)))
 (let ((?x11667 (stack_s x_0 x_MLOAD_0 3 ?x6354)))
 (let (($x9346 (not (bvsle (_ bv0 6) (bvadd (_ bv61 6) (sc_s 2))))))
 (let (($x6651 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 2)))) (_ bv0 1)))))
 (let (($x247 (exc_halt_s 2)))
 (let (($x292 (exc_halt_s 3)))
 (let (($x8464 (= $x292 (or $x247 $x6651 $x9346))))
 (let (($x4254 (forall ((w (_ BitVec 256)) )(let ((?x3580 (storage_s x_0 x_MLOAD_0 2 w)))
 (let ((?x1382 (storage_s x_0 x_MLOAD_0 3 w)))
 (= ?x1382 ?x3580))))
 ))
 (let (($x3795 (forall ((n (_ BitVec 6)) )(let ((?x218 (sc_s 2)))
 (let ((?x971 (bvadd (_ bv61 6) ?x218)))
 (let (($x4870 (bvsle ?x971 n)))
 (or $x4870 (= (stack_s x_0 x_MLOAD_0 3 n) (stack_s x_0 x_MLOAD_0 2 n)))))))
 ))
 (let (($x2920 (= ?x275 (bvadd (_ bv1 6) (sc_s 2)))))
 (let ((?x9350 (used_gas_s x_0 x_MLOAD_0 3)))
 (let ((?x218 (sc_s 2)))
 (let ((?x4506 (bvadd (_ bv63 6) ?x218)))
 (let ((?x7482 (stack_s x_0 x_MLOAD_0 2 ?x4506)))
 (let (($x7989 (= (stack_s x_0 x_MLOAD_0 3 (bvadd (_ bv62 6) ?x218)) (stack_s x_0 x_MLOAD_0 2 (bvadd (_ bv62 6) ?x218)))))
 (let ((?x971 (bvadd (_ bv61 6) ?x218)))
 (let ((?x5945 (stack_s x_0 x_MLOAD_0 2 ?x971)))
 (let (($x10410 (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_s 1))))))
 (let (($x2919 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 1)))) (_ bv0 1)))))
 (let (($x189 (exc_halt_s 1)))
 (let (($x3512 (= $x247 (or $x189 $x2919 $x10410))))
 (let (($x2461 (forall ((w (_ BitVec 256)) )(let ((?x6319 (storage_s x_0 x_MLOAD_0 1 w)))
 (let ((?x3580 (storage_s x_0 x_MLOAD_0 2 w)))
 (= ?x3580 ?x6319))))
 ))
 (let (($x6952 (forall ((n (_ BitVec 6)) )(let ((?x154 (sc_s 1)))
 (let ((?x7858 (bvadd (_ bv63 6) ?x154)))
 (let (($x8357 (bvsle ?x7858 n)))
 (or $x8357 (= (stack_s x_0 x_MLOAD_0 2 n) (stack_s x_0 x_MLOAD_0 1 n)))))))
 ))
 (let (($x1224 (= ?x218 (bvadd (_ bv1 6) (sc_s 1)))))
 (let ((?x11051 (used_gas_s x_0 x_MLOAD_0 2)))
 (let ((?x154 (sc_s 1)))
 (let ((?x7858 (bvadd (_ bv63 6) ?x154)))
 (let ((?x11532 (stack_s x_0 x_MLOAD_0 1 ?x7858)))
 (let (($x722 (= $x189 (or $x57 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x72))) (_ bv0 1)))))))
 (let (($x8330 (forall ((w (_ BitVec 256)) )(let ((?x6107 (storage_s x_0 x_MLOAD_0 0 w)))
 (let ((?x6319 (storage_s x_0 x_MLOAD_0 1 w)))
 (= ?x6319 ?x6107))))
 ))
 (let (($x3679 (forall ((n (_ BitVec 6)) )(let ((?x72 (sc_s 0)))
 (let (($x9281 (bvsle ?x72 n)))
 (or (= (stack_s x_0 x_MLOAD_0 1 n) (stack_s x_0 x_MLOAD_0 0 n)) $x9281))))
 ))
 (let (($x7884 (= ?x154 (bvadd (_ bv1 6) ?x72))))
 (let (($x5216 (forall ((w0 (_ BitVec 256)) )(let ((?x9910 (ite (= (stack_s x_0 x_MLOAD_0 3 (bvadd (_ bv63 6) (sc_s 3))) w0) x_MLOAD_0 (_ bv0 256))))
 (let ((?x3600 (f_MLOAD x_0 x_MLOAD_0 w0)))
 (= ?x3600 ?x9910))))
 ))
 (let (($x8759 (forall ((w (_ BitVec 256)) )(let ((?x6107 (storage_s x_0 x_MLOAD_0 0 w)))
 (= ?x6107 (_ bv0 256))))
 ))
 (let (($x641 (= ?x5890 0)))
 (let (($x2751 (not $x57)))
 (let (($x1029 (= (stack_s x_0 x_MLOAD_0 0 (_ bv0 6)) x_0)))
 (let (($x8685 (= ?x72 (_ bv1 6))))
 (and $x8685 $x1029 $x2751 $x641 $x8759 $x5216 (= (stack_s x_0 x_MLOAD_0 1 ?x72) (_ bv0 256)) (= (used_gas_s x_0 x_MLOAD_0 1) (+ 3 ?x5890)) $x7884 $x3679 $x8330 $x722 (= ?x7482 ?x11532) (= (stack_s x_0 x_MLOAD_0 2 ?x7858) ?x11532) (= ?x11051 (+ 3 (used_gas_s x_0 x_MLOAD_0 1))) $x1224 $x6952 $x2461 $x3512 (= ?x11667 ?x5945) (= (stack_s x_0 x_MLOAD_0 3 ?x971) ?x5945) $x7989 (= (stack_s x_0 x_MLOAD_0 3 ?x4506) ?x7482) (= ?x9350 (+ 3 ?x11051)) $x2920 $x3795 $x4254 $x8464 (= ?x9836 (f_MLOAD x_0 x_MLOAD_0 ?x11667)) (= ?x7485 (+ 3 ?x9350)) (= ?x4305 ?x275) $x3383 $x11494 $x5670 (= ?x9842 ?x3010) (= ?x3650 (+ 3 ?x7485)) $x9231 $x185 $x9959 $x2337 $x8126 $x9392 $x2373 $x393 $x10930 $x6619 (= (stack_t x_0 x_MLOAD_0 1 ?x63) (_ bv0 256)) (= (used_gas_t x_0 x_MLOAD_0 1) (+ 3 ?x5416)) $x859 $x5709 $x7630 $x9448 (= (stack_t x_0 x_MLOAD_0 2 (bvadd (_ bv63 6) ?x2714)) ?x2468) (= (stack_t x_0 x_MLOAD_0 2 ?x6516) ?x2468) $x5009 (= ?x893 (+ 3 (used_gas_t x_0 x_MLOAD_0 1))) $x1064 $x6299 $x9599 $x9331 $x4910 (= ?x8873 (+ 3 ?x893)) $x10179 $x6706 $x4655 $x5173 $x11055 $x11192 $x1409 $x920 $x5906 $x1165 $x73 $x8853 $x58 $x1371 $x5554 (not (and $x11559 $x9748 $x5620 $x5475))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
 )
(check-sat)
(get-proof)