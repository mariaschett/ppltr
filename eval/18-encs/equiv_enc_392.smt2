; DUP1 PUSH cw_2 PUSH cw_3 SWAP1 SLOAD SWAP1 => DUP1 PUSH cw_2 SLOAD PUSH cw_3
(set-option :produce-proofs true)
; 
(set-info :status unknown)
(declare-fun storage_t ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 256)) (_ BitVec 256))
(declare-fun w_3 () (_ BitVec 256))
(declare-fun w_2 () (_ BitVec 256))
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
 (exists ((x_0 (_ BitVec 256)) (x_SLOAD_0 (_ BitVec 256)) )(let (($x4753 (forall ((w (_ BitVec 256)) )(let ((?x7452 (storage_t x_0 x_SLOAD_0 w_2 w_3 4 w)))
 (let ((?x5064 (storage_s x_0 x_SLOAD_0 w_2 w_3 6 w)))
 (= ?x5064 ?x7452))))
 ))
 (let (($x3723 (exc_halt_t 4)))
 (let (($x772 (exc_halt_s 6)))
 (let (($x10342 (= $x772 $x3723)))
 (let (($x10867 (forall ((n (_ BitVec 6)) )(let ((?x1098 (sc_t 4)))
 (let (($x8721 (bvsle ?x1098 n)))
 (let ((?x1592 (stack_t x_0 x_SLOAD_0 w_2 w_3 4 n)))
 (let ((?x52 (stack_s x_0 x_SLOAD_0 w_2 w_3 6 n)))
 (let (($x8011 (= ?x52 ?x1592)))
 (or $x8011 $x8721)))))))
 ))
 (let ((?x1098 (sc_t 4)))
 (let ((?x926 (sc_s 6)))
 (let (($x7792 (= ?x926 ?x1098)))
 (let ((?x4419 (used_gas_t x_0 x_SLOAD_0 w_2 w_3 0)))
 (let ((?x10648 (used_gas_s x_0 x_SLOAD_0 w_2 w_3 0)))
 (let (($x8970 (= ?x10648 ?x4419)))
 (let (($x1450 (forall ((w (_ BitVec 256)) )(let ((?x8857 (storage_t x_0 x_SLOAD_0 w_2 w_3 0 w)))
 (let ((?x720 (storage_s x_0 x_SLOAD_0 w_2 w_3 0 w)))
 (= ?x720 ?x8857))))
 ))
 (let (($x56 (exc_halt_t 0)))
 (let (($x57 (exc_halt_s 0)))
 (let (($x58 (= $x57 $x56)))
 (let (($x11206 (forall ((n (_ BitVec 6)) )(let ((?x63 (sc_t 0)))
 (let (($x7453 (bvsle ?x63 n)))
 (let ((?x10410 (stack_t x_0 x_SLOAD_0 w_2 w_3 0 n)))
 (let ((?x5115 (stack_s x_0 x_SLOAD_0 w_2 w_3 0 n)))
 (let (($x2007 (= ?x5115 ?x10410)))
 (or $x2007 $x7453)))))))
 ))
 (let ((?x63 (sc_t 0)))
 (let ((?x72 (sc_s 0)))
 (let (($x73 (= ?x72 ?x63)))
 (let (($x2704 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_t 3)))) (_ bv0 1)))))
 (let (($x6783 (exc_halt_t 3)))
 (let (($x4562 (forall ((w (_ BitVec 256)) )(let ((?x10443 (storage_t x_0 x_SLOAD_0 w_2 w_3 3 w)))
 (let ((?x7452 (storage_t x_0 x_SLOAD_0 w_2 w_3 4 w)))
 (= ?x7452 ?x10443))))
 ))
 (let (($x6418 (forall ((n (_ BitVec 6)) )(let ((?x6438 (sc_t 3)))
 (let (($x5094 (bvsle ?x6438 n)))
 (let ((?x6932 (stack_t x_0 x_SLOAD_0 w_2 w_3 3 n)))
 (let ((?x1592 (stack_t x_0 x_SLOAD_0 w_2 w_3 4 n)))
 (or (= ?x1592 ?x6932) $x5094))))))
 ))
 (let (($x5367 (= ?x1098 (bvadd (_ bv1 6) (sc_t 3)))))
 (let (($x8152 (= (used_gas_t x_0 x_SLOAD_0 w_2 w_3 4) (+ 3 (used_gas_t x_0 x_SLOAD_0 w_2 w_3 3)))))
 (let (($x7395 (= $x6783 (or (exc_halt_t 2) (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_t 2))))))))
 (let (($x6380 (forall ((w (_ BitVec 256)) )(let ((?x5438 (storage_t x_0 x_SLOAD_0 w_2 w_3 2 w)))
 (let ((?x10443 (storage_t x_0 x_SLOAD_0 w_2 w_3 3 w)))
 (= ?x10443 ?x5438))))
 ))
 (let (($x10382 (forall ((n (_ BitVec 6)) )(let ((?x395 (stack_t x_0 x_SLOAD_0 w_2 w_3 2 n)))
 (let ((?x6932 (stack_t x_0 x_SLOAD_0 w_2 w_3 3 n)))
 (or (= ?x6932 ?x395) (bvsle (bvadd (_ bv63 6) (sc_t 2)) n)))))
 ))
 (let ((?x2714 (sc_t 2)))
 (let ((?x6438 (sc_t 3)))
 (let (($x11788 (= ?x6438 ?x2714)))
 (let ((?x2777 (used_gas_t x_0 x_SLOAD_0 w_2 w_3 3)))
 (let ((?x2889 (storage_t x_0 x_SLOAD_0 w_2 w_3 2 (stack_t x_0 x_SLOAD_0 w_2 w_3 2 (bvadd (_ bv63 6) ?x2714)))))
 (let (($x1340 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_t 1)))) (_ bv0 1)))))
 (let (($x3508 (exc_halt_t 1)))
 (let (($x5252 (exc_halt_t 2)))
 (let (($x451 (forall ((w (_ BitVec 256)) )(let ((?x5653 (storage_t x_0 x_SLOAD_0 w_2 w_3 1 w)))
 (let ((?x5438 (storage_t x_0 x_SLOAD_0 w_2 w_3 2 w)))
 (= ?x5438 ?x5653))))
 ))
 (let (($x10156 (forall ((n (_ BitVec 6)) )(let ((?x8338 (stack_t x_0 x_SLOAD_0 w_2 w_3 1 n)))
 (let ((?x395 (stack_t x_0 x_SLOAD_0 w_2 w_3 2 n)))
 (or (= ?x395 ?x8338) (bvsle (sc_t 1) n)))))
 ))
 (let (($x2756 (= ?x2714 (bvadd (_ bv1 6) (sc_t 1)))))
 (let ((?x5343 (used_gas_t x_0 x_SLOAD_0 w_2 w_3 2)))
 (let (($x3118 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x63))) (_ bv0 1)))))
 (let (($x5019 (forall ((w (_ BitVec 256)) )(let ((?x8857 (storage_t x_0 x_SLOAD_0 w_2 w_3 0 w)))
 (let ((?x5653 (storage_t x_0 x_SLOAD_0 w_2 w_3 1 w)))
 (= ?x5653 ?x8857))))
 ))
 (let (($x11597 (forall ((n (_ BitVec 6)) )(let ((?x10410 (stack_t x_0 x_SLOAD_0 w_2 w_3 0 n)))
 (let ((?x8338 (stack_t x_0 x_SLOAD_0 w_2 w_3 1 n)))
 (or (bvsle (bvadd (_ bv63 6) (sc_t 0)) n) (= ?x8338 ?x10410)))))
 ))
 (let ((?x8347 (sc_t 1)))
 (let (($x6213 (= ?x8347 (bvadd (_ bv1 6) ?x63))))
 (let ((?x1747 (bvadd (_ bv63 6) ?x63)))
 (let ((?x6078 (stack_t x_0 x_SLOAD_0 w_2 w_3 0 ?x1747)))
 (let (($x10655 (= $x772 (or (exc_halt_s 5) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 5))))))))
 (let (($x484 (forall ((w (_ BitVec 256)) )(let ((?x6087 (storage_s x_0 x_SLOAD_0 w_2 w_3 5 w)))
 (let ((?x5064 (storage_s x_0 x_SLOAD_0 w_2 w_3 6 w)))
 (= ?x5064 ?x6087))))
 ))
 (let (($x3930 (forall ((n (_ BitVec 6)) )(let ((?x5264 (stack_s x_0 x_SLOAD_0 w_2 w_3 5 n)))
 (let ((?x52 (stack_s x_0 x_SLOAD_0 w_2 w_3 6 n)))
 (let ((?x4319 (sc_s 5)))
 (let ((?x8322 (bvadd (_ bv62 6) ?x4319)))
 (let (($x4228 (bvsle ?x8322 n)))
 (or $x4228 (= ?x52 ?x5264))))))))
 ))
 (let ((?x4319 (sc_s 5)))
 (let (($x1737 (= ?x926 ?x4319)))
 (let (($x994 (= (used_gas_s x_0 x_SLOAD_0 w_2 w_3 6) (+ 3 (used_gas_s x_0 x_SLOAD_0 w_2 w_3 5)))))
 (let ((?x3503 (bvadd (_ bv63 6) ?x4319)))
 (let ((?x201 (stack_s x_0 x_SLOAD_0 w_2 w_3 5 ?x3503)))
 (let (($x4797 (= (stack_s x_0 x_SLOAD_0 w_2 w_3 6 (bvadd (_ bv63 6) ?x926)) (stack_s x_0 x_SLOAD_0 w_2 w_3 5 (bvadd (_ bv62 6) ?x4319)))))
 (let (($x11317 (exc_halt_s 5)))
 (let (($x7472 (= $x11317 (or (exc_halt_s 4) (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_s 4))))))))
 (let (($x8415 (forall ((w (_ BitVec 256)) )(let ((?x1529 (storage_s x_0 x_SLOAD_0 w_2 w_3 4 w)))
 (let ((?x6087 (storage_s x_0 x_SLOAD_0 w_2 w_3 5 w)))
 (= ?x6087 ?x1529))))
 ))
 (let (($x8125 (forall ((n (_ BitVec 6)) )(let ((?x4305 (sc_s 4)))
 (let ((?x805 (bvadd (_ bv63 6) ?x4305)))
 (let (($x1380 (bvsle ?x805 n)))
 (let ((?x10998 (stack_s x_0 x_SLOAD_0 w_2 w_3 4 n)))
 (let ((?x5264 (stack_s x_0 x_SLOAD_0 w_2 w_3 5 n)))
 (or (= ?x5264 ?x10998) $x1380)))))))
 ))
 (let ((?x4305 (sc_s 4)))
 (let (($x15 (= ?x4319 ?x4305)))
 (let ((?x8875 (used_gas_s x_0 x_SLOAD_0 w_2 w_3 5)))
 (let ((?x805 (bvadd (_ bv63 6) ?x4305)))
 (let ((?x3701 (stack_s x_0 x_SLOAD_0 w_2 w_3 4 ?x805)))
 (let (($x7172 (exc_halt_s 4)))
 (let (($x7702 (= $x7172 (or (exc_halt_s 3) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 3))))))))
 (let (($x1783 (forall ((w (_ BitVec 256)) )(let ((?x11414 (storage_s x_0 x_SLOAD_0 w_2 w_3 3 w)))
 (let ((?x1529 (storage_s x_0 x_SLOAD_0 w_2 w_3 4 w)))
 (= ?x1529 ?x11414))))
 ))
 (let (($x10492 (forall ((n (_ BitVec 6)) )(let ((?x275 (sc_s 3)))
 (let ((?x9043 (bvadd (_ bv62 6) ?x275)))
 (let (($x6666 (bvsle ?x9043 n)))
 (let ((?x6086 (stack_s x_0 x_SLOAD_0 w_2 w_3 3 n)))
 (let ((?x10998 (stack_s x_0 x_SLOAD_0 w_2 w_3 4 n)))
 (or (= ?x10998 ?x6086) $x6666)))))))
 ))
 (let ((?x275 (sc_s 3)))
 (let (($x10620 (= ?x4305 ?x275)))
 (let ((?x964 (used_gas_s x_0 x_SLOAD_0 w_2 w_3 4)))
 (let (($x8552 (= (stack_s x_0 x_SLOAD_0 w_2 w_3 4 (bvadd (_ bv62 6) ?x4305)) (stack_s x_0 x_SLOAD_0 w_2 w_3 3 (bvadd (_ bv63 6) ?x275)))))
 (let (($x247 (exc_halt_s 2)))
 (let (($x9523 (or $x247 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 2)))) (_ bv0 1))))))
 (let (($x292 (exc_halt_s 3)))
 (let (($x5076 (= $x292 $x9523)))
 (let (($x4007 (forall ((w (_ BitVec 256)) )(let ((?x11156 (storage_s x_0 x_SLOAD_0 w_2 w_3 2 w)))
 (let ((?x11414 (storage_s x_0 x_SLOAD_0 w_2 w_3 3 w)))
 (= ?x11414 ?x11156))))
 ))
 (let (($x10975 (forall ((n (_ BitVec 6)) )(let ((?x218 (sc_s 2)))
 (let (($x11364 (bvsle ?x218 n)))
 (let ((?x3482 (stack_s x_0 x_SLOAD_0 w_2 w_3 2 n)))
 (let ((?x6086 (stack_s x_0 x_SLOAD_0 w_2 w_3 3 n)))
 (or (= ?x6086 ?x3482) $x11364))))))
 ))
 (let (($x4795 (= ?x275 (bvadd (_ bv1 6) (sc_s 2)))))
 (let ((?x3308 (used_gas_s x_0 x_SLOAD_0 w_2 w_3 3)))
 (let (($x8154 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 1)))) (_ bv0 1)))))
 (let (($x189 (exc_halt_s 1)))
 (let (($x961 (= $x247 (or $x189 $x8154))))
 (let (($x11672 (forall ((w (_ BitVec 256)) )(let ((?x9423 (storage_s x_0 x_SLOAD_0 w_2 w_3 1 w)))
 (let ((?x11156 (storage_s x_0 x_SLOAD_0 w_2 w_3 2 w)))
 (= ?x11156 ?x9423))))
 ))
 (let (($x10570 (forall ((n (_ BitVec 6)) )(let ((?x4388 (stack_s x_0 x_SLOAD_0 w_2 w_3 1 n)))
 (let ((?x3482 (stack_s x_0 x_SLOAD_0 w_2 w_3 2 n)))
 (let ((?x154 (sc_s 1)))
 (let (($x1556 (bvsle ?x154 n)))
 (or $x1556 (= ?x3482 ?x4388)))))))
 ))
 (let ((?x218 (sc_s 2)))
 (let (($x1362 (= ?x218 (bvadd (_ bv1 6) (sc_s 1)))))
 (let ((?x9813 (used_gas_s x_0 x_SLOAD_0 w_2 w_3 2)))
 (let (($x10528 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x72))) (_ bv0 1)))))
 (let (($x1399 (= $x189 (or $x57 (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) ?x72))) $x10528))))
 (let (($x3036 (forall ((w (_ BitVec 256)) )(let ((?x720 (storage_s x_0 x_SLOAD_0 w_2 w_3 0 w)))
 (let ((?x9423 (storage_s x_0 x_SLOAD_0 w_2 w_3 1 w)))
 (= ?x9423 ?x720))))
 ))
 (let (($x8999 (forall ((n (_ BitVec 6)) )(let ((?x5115 (stack_s x_0 x_SLOAD_0 w_2 w_3 0 n)))
 (let ((?x4388 (stack_s x_0 x_SLOAD_0 w_2 w_3 1 n)))
 (let ((?x72 (sc_s 0)))
 (let ((?x190 (bvadd (_ bv63 6) ?x72)))
 (let (($x1541 (bvsle ?x190 n)))
 (or $x1541 (= ?x4388 ?x5115))))))))
 ))
 (let ((?x154 (sc_s 1)))
 (let (($x11639 (= ?x154 (bvadd (_ bv1 6) ?x72))))
 (let ((?x190 (bvadd (_ bv63 6) ?x72)))
 (let ((?x2449 (stack_s x_0 x_SLOAD_0 w_2 w_3 0 ?x190)))
 (let (($x5935 (forall ((w (_ BitVec 256)) )(let (($x2921 (= w (stack_s x_0 x_SLOAD_0 w_2 w_3 4 (bvadd (_ bv63 6) (sc_s 4))))))
 (let ((?x720 (storage_s x_0 x_SLOAD_0 w_2 w_3 0 w)))
 (= ?x720 (ite $x2921 x_SLOAD_0 (_ bv0 256))))))
 ))
 (let (($x3977 (= ?x10648 0)))
 (let (($x6901 (not $x57)))
 (let (($x6162 (= (stack_s x_0 x_SLOAD_0 w_2 w_3 0 (_ bv0 6)) x_0)))
 (let (($x8685 (= ?x72 (_ bv1 6))))
 (and $x8685 $x6162 $x6901 $x3977 $x5935 (= (stack_s x_0 x_SLOAD_0 w_2 w_3 1 (bvadd (_ bv63 6) ?x154)) ?x2449) (= (stack_s x_0 x_SLOAD_0 w_2 w_3 1 ?x190) ?x2449) (= (used_gas_s x_0 x_SLOAD_0 w_2 w_3 1) (+ 3 ?x10648)) $x11639 $x8999 $x3036 $x1399 (= (stack_s x_0 x_SLOAD_0 w_2 w_3 2 ?x154) w_2) (= ?x9813 (+ 3 (used_gas_s x_0 x_SLOAD_0 w_2 w_3 1))) $x1362 $x10570 $x11672 $x961 (= (stack_s x_0 x_SLOAD_0 w_2 w_3 3 ?x218) w_3) (= ?x3308 (+ 3 ?x9813)) $x4795 $x10975 $x4007 $x5076 (= ?x3701 (stack_s x_0 x_SLOAD_0 w_2 w_3 3 (bvadd (_ bv62 6) ?x275))) $x8552 (= ?x964 (+ 3 ?x3308)) $x10620 $x10492 $x1783 $x7702 (= ?x201 (storage_s x_0 x_SLOAD_0 w_2 w_3 4 ?x3701)) (= ?x8875 (+ 200 ?x964)) $x15 $x8125 $x8415 $x7472 $x4797 (= (stack_s x_0 x_SLOAD_0 w_2 w_3 6 (bvadd (_ bv62 6) ?x926)) ?x201) $x994 $x1737 $x3930 $x484 $x10655 (= (stack_t x_0 x_SLOAD_0 w_2 w_3 1 (bvadd (_ bv63 6) ?x8347)) ?x6078) (= (stack_t x_0 x_SLOAD_0 w_2 w_3 1 ?x1747) ?x6078) (= (used_gas_t x_0 x_SLOAD_0 w_2 w_3 1) (+ 3 ?x4419)) $x6213 $x11597 $x5019 (= $x3508 (or $x56 $x3118 (not (bvsle (_ bv0 6) ?x1747)))) (= (stack_t x_0 x_SLOAD_0 w_2 w_3 2 ?x8347) w_2) (= ?x5343 (+ 3 (used_gas_t x_0 x_SLOAD_0 w_2 w_3 1))) $x2756 $x10156 $x451 (= $x5252 (or $x3508 $x1340)) (= (stack_t x_0 x_SLOAD_0 w_2 w_3 3 (bvadd (_ bv63 6) ?x6438)) ?x2889) (= ?x2777 (+ 200 ?x5343)) $x11788 $x10382 $x6380 $x7395 (= (stack_t x_0 x_SLOAD_0 w_2 w_3 4 ?x6438) w_3) $x8152 $x5367 $x6418 $x4562 (= $x3723 (or $x6783 $x2704)) $x73 $x11206 $x58 $x1450 $x8970 (not (and $x7792 $x10867 $x10342 $x4753))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
 )
(check-sat)
(get-proof)