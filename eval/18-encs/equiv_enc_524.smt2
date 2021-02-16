; PUSH cw_2 DUP4 PUSH cw_3 SWAP1 SWAP2 SWAP1 => DUP3 PUSH cw_2 PUSH cw_3
(set-option :produce-proofs true)
; 
(set-info :status unknown)
(declare-fun storage_t ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 256)) (_ BitVec 256))
(declare-fun w_3 () (_ BitVec 256))
(declare-fun w_2 () (_ BitVec 256))
(declare-fun storage_s ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 256)) (_ BitVec 256))
(declare-fun exc_halt_t (Int) Bool)
(declare-fun exc_halt_s (Int) Bool)
(declare-fun stack_t ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 6)) (_ BitVec 256))
(declare-fun stack_s ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 6)) (_ BitVec 256))
(declare-fun sc_t (Int) (_ BitVec 6))
(declare-fun sc_s (Int) (_ BitVec 6))
(declare-fun used_gas_t ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int) Int)
(declare-fun used_gas_s ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int) Int)
(assert
 (exists ((x_0 (_ BitVec 256)) (x_1 (_ BitVec 256)) (x_2 (_ BitVec 256)) )(let (($x8520 (forall ((w (_ BitVec 256)) )(let ((?x3028 (storage_t x_0 x_1 x_2 w_2 w_3 3 w)))
 (let ((?x9330 (storage_s x_0 x_1 x_2 w_2 w_3 6 w)))
 (= ?x9330 ?x3028))))
 ))
 (let (($x6783 (exc_halt_t 3)))
 (let (($x772 (exc_halt_s 6)))
 (let (($x6970 (= $x772 $x6783)))
 (let (($x366 (forall ((n (_ BitVec 6)) )(let ((?x10168 (stack_t x_0 x_1 x_2 w_2 w_3 3 n)))
 (let ((?x8772 (stack_s x_0 x_1 x_2 w_2 w_3 6 n)))
 (let (($x9822 (= ?x8772 ?x10168)))
 (let ((?x6438 (sc_t 3)))
 (let (($x5122 (bvsle ?x6438 n)))
 (or $x5122 $x9822)))))))
 ))
 (let ((?x6438 (sc_t 3)))
 (let ((?x926 (sc_s 6)))
 (let (($x9708 (= ?x926 ?x6438)))
 (let ((?x6517 (used_gas_t x_0 x_1 x_2 w_2 w_3 0)))
 (let ((?x8720 (used_gas_s x_0 x_1 x_2 w_2 w_3 0)))
 (let (($x5450 (= ?x8720 ?x6517)))
 (let (($x7953 (forall ((w (_ BitVec 256)) )(let ((?x9902 (storage_t x_0 x_1 x_2 w_2 w_3 0 w)))
 (let ((?x7191 (storage_s x_0 x_1 x_2 w_2 w_3 0 w)))
 (= ?x7191 ?x9902))))
 ))
 (let (($x56 (exc_halt_t 0)))
 (let (($x57 (exc_halt_s 0)))
 (let (($x58 (= $x57 $x56)))
 (let (($x9903 (forall ((n (_ BitVec 6)) )(let ((?x63 (sc_t 0)))
 (let (($x10511 (bvsle ?x63 n)))
 (let ((?x9701 (stack_t x_0 x_1 x_2 w_2 w_3 0 n)))
 (let ((?x7002 (stack_s x_0 x_1 x_2 w_2 w_3 0 n)))
 (let (($x3186 (= ?x7002 ?x9701)))
 (or $x3186 $x10511)))))))
 ))
 (let ((?x63 (sc_t 0)))
 (let ((?x72 (sc_s 0)))
 (let (($x73 (= ?x72 ?x63)))
 (let (($x8424 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_t 2)))) (_ bv0 1)))))
 (let (($x2163 (exc_halt_t 2)))
 (let (($x5035 (= $x6783 (or $x2163 $x8424))))
 (let (($x8911 (forall ((w (_ BitVec 256)) )(let ((?x7170 (storage_t x_0 x_1 x_2 w_2 w_3 2 w)))
 (let ((?x3028 (storage_t x_0 x_1 x_2 w_2 w_3 3 w)))
 (= ?x3028 ?x7170))))
 ))
 (let (($x11449 (forall ((n (_ BitVec 6)) )(let ((?x4869 (stack_t x_0 x_1 x_2 w_2 w_3 2 n)))
 (let ((?x10168 (stack_t x_0 x_1 x_2 w_2 w_3 3 n)))
 (let ((?x2714 (sc_t 2)))
 (let (($x1920 (bvsle ?x2714 n)))
 (or $x1920 (= ?x10168 ?x4869)))))))
 ))
 (let (($x7516 (= ?x6438 (bvadd (_ bv1 6) (sc_t 2)))))
 (let (($x266 (= (used_gas_t x_0 x_1 x_2 w_2 w_3 3) (+ 3 (used_gas_t x_0 x_1 x_2 w_2 w_3 2)))))
 (let (($x9824 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_t 1)))) (_ bv0 1)))))
 (let (($x8377 (exc_halt_t 1)))
 (let (($x6999 (= $x2163 (or $x8377 $x9824))))
 (let (($x8251 (forall ((w (_ BitVec 256)) )(let ((?x8048 (storage_t x_0 x_1 x_2 w_2 w_3 1 w)))
 (let ((?x7170 (storage_t x_0 x_1 x_2 w_2 w_3 2 w)))
 (= ?x7170 ?x8048))))
 ))
 (let (($x9603 (forall ((n (_ BitVec 6)) )(let ((?x7154 (sc_t 1)))
 (let (($x8103 (bvsle ?x7154 n)))
 (let ((?x3871 (stack_t x_0 x_1 x_2 w_2 w_3 1 n)))
 (let ((?x4869 (stack_t x_0 x_1 x_2 w_2 w_3 2 n)))
 (or (= ?x4869 ?x3871) $x8103))))))
 ))
 (let ((?x2714 (sc_t 2)))
 (let (($x44 (= ?x2714 (bvadd (_ bv1 6) (sc_t 1)))))
 (let ((?x8094 (used_gas_t x_0 x_1 x_2 w_2 w_3 2)))
 (let (($x4131 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x63))) (_ bv0 1)))))
 (let (($x3030 (not (bvsle (_ bv0 6) (bvadd (_ bv61 6) ?x63)))))
 (let (($x3343 (forall ((w (_ BitVec 256)) )(let ((?x9902 (storage_t x_0 x_1 x_2 w_2 w_3 0 w)))
 (let ((?x8048 (storage_t x_0 x_1 x_2 w_2 w_3 1 w)))
 (= ?x8048 ?x9902))))
 ))
 (let (($x6932 (forall ((n (_ BitVec 6)) )(let ((?x9701 (stack_t x_0 x_1 x_2 w_2 w_3 0 n)))
 (let ((?x3871 (stack_t x_0 x_1 x_2 w_2 w_3 1 n)))
 (let ((?x63 (sc_t 0)))
 (let ((?x3367 (bvadd (_ bv61 6) ?x63)))
 (let (($x3338 (bvsle ?x3367 n)))
 (or $x3338 (= ?x3871 ?x9701))))))))
 ))
 (let ((?x7154 (sc_t 1)))
 (let (($x888 (= ?x7154 (bvadd (_ bv1 6) ?x63))))
 (let (($x3965 (= (stack_t x_0 x_1 x_2 w_2 w_3 1 (bvadd (_ bv63 6) ?x63)) (stack_t x_0 x_1 x_2 w_2 w_3 0 (bvadd (_ bv63 6) ?x63)))))
 (let (($x10736 (= (stack_t x_0 x_1 x_2 w_2 w_3 1 (bvadd (_ bv62 6) ?x63)) (stack_t x_0 x_1 x_2 w_2 w_3 0 (bvadd (_ bv62 6) ?x63)))))
 (let ((?x3367 (bvadd (_ bv61 6) ?x63)))
 (let ((?x1318 (stack_t x_0 x_1 x_2 w_2 w_3 0 ?x3367)))
 (let (($x9823 (= (stack_t x_0 x_1 x_2 w_2 w_3 1 (bvadd (_ bv63 6) ?x7154)) ?x1318)))
 (let (($x3884 (= $x772 (or (exc_halt_s 5) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 5))))))))
 (let (($x2428 (forall ((w (_ BitVec 256)) )(let ((?x2782 (storage_s x_0 x_1 x_2 w_2 w_3 5 w)))
 (let ((?x9330 (storage_s x_0 x_1 x_2 w_2 w_3 6 w)))
 (= ?x9330 ?x2782))))
 ))
 (let (($x2907 (forall ((n (_ BitVec 6)) )(let ((?x1585 (stack_s x_0 x_1 x_2 w_2 w_3 5 n)))
 (let ((?x8772 (stack_s x_0 x_1 x_2 w_2 w_3 6 n)))
 (let ((?x4319 (sc_s 5)))
 (let ((?x10059 (bvadd (_ bv62 6) ?x4319)))
 (let (($x4069 (bvsle ?x10059 n)))
 (or $x4069 (= ?x8772 ?x1585))))))))
 ))
 (let ((?x4319 (sc_s 5)))
 (let (($x2838 (= ?x926 ?x4319)))
 (let (($x5893 (= (used_gas_s x_0 x_1 x_2 w_2 w_3 6) (+ 3 (used_gas_s x_0 x_1 x_2 w_2 w_3 5)))))
 (let ((?x7641 (bvadd (_ bv63 6) ?x4319)))
 (let ((?x8516 (stack_s x_0 x_1 x_2 w_2 w_3 5 ?x7641)))
 (let (($x9892 (= (stack_s x_0 x_1 x_2 w_2 w_3 6 (bvadd (_ bv62 6) ?x926)) ?x8516)))
 (let ((?x10059 (bvadd (_ bv62 6) ?x4319)))
 (let ((?x8717 (stack_s x_0 x_1 x_2 w_2 w_3 5 ?x10059)))
 (let (($x4056 (= (stack_s x_0 x_1 x_2 w_2 w_3 6 (bvadd (_ bv63 6) ?x926)) ?x8717)))
 (let (($x11317 (exc_halt_s 5)))
 (let (($x492 (= $x11317 (or (exc_halt_s 4) (not (bvsle (_ bv0 6) (bvadd (_ bv61 6) (sc_s 4))))))))
 (let (($x7983 (forall ((w (_ BitVec 256)) )(let ((?x8387 (storage_s x_0 x_1 x_2 w_2 w_3 4 w)))
 (let ((?x2782 (storage_s x_0 x_1 x_2 w_2 w_3 5 w)))
 (= ?x2782 ?x8387))))
 ))
 (let (($x4469 (forall ((n (_ BitVec 6)) )(let ((?x7643 (stack_s x_0 x_1 x_2 w_2 w_3 4 n)))
 (let ((?x1585 (stack_s x_0 x_1 x_2 w_2 w_3 5 n)))
 (let ((?x4305 (sc_s 4)))
 (let ((?x7198 (bvadd (_ bv61 6) ?x4305)))
 (let (($x6711 (bvsle ?x7198 n)))
 (or $x6711 (= ?x1585 ?x7643))))))))
 ))
 (let ((?x4305 (sc_s 4)))
 (let (($x9298 (= ?x4319 ?x4305)))
 (let ((?x8098 (used_gas_s x_0 x_1 x_2 w_2 w_3 5)))
 (let ((?x5260 (bvadd (_ bv62 6) ?x4305)))
 (let ((?x1534 (stack_s x_0 x_1 x_2 w_2 w_3 4 ?x5260)))
 (let ((?x3403 (bvadd (_ bv63 6) ?x4305)))
 (let ((?x6742 (stack_s x_0 x_1 x_2 w_2 w_3 4 ?x3403)))
 (let (($x8549 (= (stack_s x_0 x_1 x_2 w_2 w_3 5 (bvadd (_ bv61 6) ?x4319)) ?x6742)))
 (let (($x9244 (= ?x8516 (stack_s x_0 x_1 x_2 w_2 w_3 4 (bvadd (_ bv61 6) ?x4305)))))
 (let (($x7172 (exc_halt_s 4)))
 (let (($x6172 (= $x7172 (or (exc_halt_s 3) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 3))))))))
 (let (($x6202 (forall ((w (_ BitVec 256)) )(let ((?x6251 (storage_s x_0 x_1 x_2 w_2 w_3 3 w)))
 (let ((?x8387 (storage_s x_0 x_1 x_2 w_2 w_3 4 w)))
 (= ?x8387 ?x6251))))
 ))
 (let (($x3417 (forall ((n (_ BitVec 6)) )(let ((?x275 (sc_s 3)))
 (let ((?x11740 (bvadd (_ bv62 6) ?x275)))
 (let (($x4702 (bvsle ?x11740 n)))
 (let ((?x1483 (stack_s x_0 x_1 x_2 w_2 w_3 3 n)))
 (let ((?x7643 (stack_s x_0 x_1 x_2 w_2 w_3 4 n)))
 (or (= ?x7643 ?x1483) $x4702)))))))
 ))
 (let ((?x275 (sc_s 3)))
 (let (($x4967 (= ?x4305 ?x275)))
 (let ((?x6312 (used_gas_s x_0 x_1 x_2 w_2 w_3 4)))
 (let (($x11336 (= ?x1534 (stack_s x_0 x_1 x_2 w_2 w_3 3 (bvadd (_ bv63 6) ?x275)))))
 (let (($x3460 (= ?x6742 (stack_s x_0 x_1 x_2 w_2 w_3 3 (bvadd (_ bv62 6) ?x275)))))
 (let (($x7451 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 2)))) (_ bv0 1)))))
 (let (($x247 (exc_halt_s 2)))
 (let (($x292 (exc_halt_s 3)))
 (let (($x9479 (= $x292 (or $x247 $x7451))))
 (let (($x2889 (forall ((w (_ BitVec 256)) )(let ((?x7562 (storage_s x_0 x_1 x_2 w_2 w_3 2 w)))
 (let ((?x6251 (storage_s x_0 x_1 x_2 w_2 w_3 3 w)))
 (= ?x6251 ?x7562))))
 ))
 (let (($x8970 (forall ((n (_ BitVec 6)) )(let ((?x8555 (stack_s x_0 x_1 x_2 w_2 w_3 2 n)))
 (let ((?x1483 (stack_s x_0 x_1 x_2 w_2 w_3 3 n)))
 (let ((?x218 (sc_s 2)))
 (let (($x1516 (bvsle ?x218 n)))
 (or $x1516 (= ?x1483 ?x8555)))))))
 ))
 (let (($x7268 (= ?x275 (bvadd (_ bv1 6) (sc_s 2)))))
 (let ((?x2840 (used_gas_s x_0 x_1 x_2 w_2 w_3 3)))
 (let (($x6570 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 1)))) (_ bv0 1)))))
 (let (($x4591 (not (bvsle (_ bv0 6) (bvadd (_ bv60 6) (sc_s 1))))))
 (let (($x189 (exc_halt_s 1)))
 (let (($x7968 (= $x247 (or $x189 $x4591 $x6570))))
 (let (($x6931 (forall ((w (_ BitVec 256)) )(let ((?x788 (storage_s x_0 x_1 x_2 w_2 w_3 1 w)))
 (let ((?x7562 (storage_s x_0 x_1 x_2 w_2 w_3 2 w)))
 (= ?x7562 ?x788))))
 ))
 (let (($x4910 (forall ((n (_ BitVec 6)) )(let ((?x8852 (stack_s x_0 x_1 x_2 w_2 w_3 1 n)))
 (let ((?x8555 (stack_s x_0 x_1 x_2 w_2 w_3 2 n)))
 (let ((?x154 (sc_s 1)))
 (let ((?x338 (bvadd (_ bv60 6) ?x154)))
 (let (($x4649 (bvsle ?x338 n)))
 (or $x4649 (= ?x8555 ?x8852))))))))
 ))
 (let ((?x218 (sc_s 2)))
 (let (($x4497 (= ?x218 (bvadd (_ bv1 6) (sc_s 1)))))
 (let ((?x6774 (used_gas_s x_0 x_1 x_2 w_2 w_3 2)))
 (let (($x6921 (= (stack_s x_0 x_1 x_2 w_2 w_3 2 (bvadd (_ bv63 6) (sc_s 1))) (stack_s x_0 x_1 x_2 w_2 w_3 1 (bvadd (_ bv63 6) (sc_s 1))))))
 (let (($x5784 (= (stack_s x_0 x_1 x_2 w_2 w_3 2 (bvadd (_ bv62 6) (sc_s 1))) (stack_s x_0 x_1 x_2 w_2 w_3 1 (bvadd (_ bv62 6) (sc_s 1))))))
 (let (($x5136 (= (stack_s x_0 x_1 x_2 w_2 w_3 2 (bvadd (_ bv61 6) (sc_s 1))) (stack_s x_0 x_1 x_2 w_2 w_3 1 (bvadd (_ bv61 6) (sc_s 1))))))
 (let ((?x154 (sc_s 1)))
 (let ((?x338 (bvadd (_ bv60 6) ?x154)))
 (let ((?x4504 (stack_s x_0 x_1 x_2 w_2 w_3 1 ?x338)))
 (let (($x7051 (= (stack_s x_0 x_1 x_2 w_2 w_3 2 (bvadd (_ bv63 6) ?x218)) ?x4504)))
 (let (($x6847 (= $x189 (or $x57 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x72))) (_ bv0 1)))))))
 (let (($x4196 (forall ((w (_ BitVec 256)) )(let ((?x7191 (storage_s x_0 x_1 x_2 w_2 w_3 0 w)))
 (let ((?x788 (storage_s x_0 x_1 x_2 w_2 w_3 1 w)))
 (= ?x788 ?x7191))))
 ))
 (let (($x11602 (forall ((n (_ BitVec 6)) )(let ((?x7002 (stack_s x_0 x_1 x_2 w_2 w_3 0 n)))
 (let ((?x8852 (stack_s x_0 x_1 x_2 w_2 w_3 1 n)))
 (let ((?x72 (sc_s 0)))
 (let (($x6228 (bvsle ?x72 n)))
 (or $x6228 (= ?x8852 ?x7002)))))))
 ))
 (let (($x1581 (= ?x154 (bvadd (_ bv1 6) ?x72))))
 (let (($x9283 (forall ((w (_ BitVec 256)) )(let ((?x7191 (storage_s x_0 x_1 x_2 w_2 w_3 0 w)))
 (= ?x7191 (_ bv0 256))))
 ))
 (let (($x4553 (= ?x8720 0)))
 (let (($x7737 (not $x57)))
 (let (($x11370 (= (stack_s x_0 x_1 x_2 w_2 w_3 0 (_ bv2 6)) x_2)))
 (let (($x10510 (= (stack_s x_0 x_1 x_2 w_2 w_3 0 (_ bv1 6)) x_1)))
 (let (($x7864 (= (stack_s x_0 x_1 x_2 w_2 w_3 0 (_ bv0 6)) x_0)))
 (let (($x7797 (= ?x72 (_ bv3 6))))
 (and $x7797 $x7864 $x10510 $x11370 $x7737 $x4553 $x9283 (= (stack_s x_0 x_1 x_2 w_2 w_3 1 ?x72) w_2) (= (used_gas_s x_0 x_1 x_2 w_2 w_3 1) (+ 3 ?x8720)) $x1581 $x11602 $x4196 $x6847 $x7051 (= (stack_s x_0 x_1 x_2 w_2 w_3 2 ?x338) ?x4504) $x5136 $x5784 $x6921 (= ?x6774 (+ 3 (used_gas_s x_0 x_1 x_2 w_2 w_3 1))) $x4497 $x4910 $x6931 $x7968 (= (stack_s x_0 x_1 x_2 w_2 w_3 3 ?x218) w_3) (= ?x2840 (+ 3 ?x6774)) $x7268 $x8970 $x2889 $x9479 $x3460 $x11336 (= ?x6312 (+ 3 ?x2840)) $x4967 $x3417 $x6202 $x6172 $x9244 $x8549 (= ?x8717 ?x1534) (= ?x8098 (+ 3 ?x6312)) $x9298 $x4469 $x7983 $x492 $x4056 $x9892 $x5893 $x2838 $x2907 $x2428 $x3884 $x9823 (= (stack_t x_0 x_1 x_2 w_2 w_3 1 ?x3367) ?x1318) $x10736 $x3965 (= (used_gas_t x_0 x_1 x_2 w_2 w_3 1) (+ 3 ?x6517)) $x888 $x6932 $x3343 (= $x8377 (or $x56 $x3030 $x4131)) (= (stack_t x_0 x_1 x_2 w_2 w_3 2 ?x7154) w_2) (= ?x8094 (+ 3 (used_gas_t x_0 x_1 x_2 w_2 w_3 1))) $x44 $x9603 $x8251 $x6999 (= (stack_t x_0 x_1 x_2 w_2 w_3 3 ?x2714) w_3) $x266 $x7516 $x11449 $x8911 $x5035 $x73 $x9903 $x58 $x7953 $x5450 (not (and $x9708 $x366 $x6970 $x8520))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
 )
(check-sat)
(get-proof)