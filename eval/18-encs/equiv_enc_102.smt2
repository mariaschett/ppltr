; DUP1 SLOAD PUSH cw_3 PUSH cw_2 SWAP2 => PUSH cw_2 PUSH cw_3 DUP3 SLOAD
(set-option :produce-proofs true)
; 
(set-info :status unknown)
(declare-fun storage_t ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 256)) (_ BitVec 256))
(declare-fun w_2 () (_ BitVec 256))
(declare-fun w_3 () (_ BitVec 256))
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
 (exists ((x_0 (_ BitVec 256)) (x_SLOAD_0 (_ BitVec 256)) )(let (($x4543 (forall ((w (_ BitVec 256)) )(let ((?x4885 (storage_t x_0 x_SLOAD_0 w_3 w_2 4 w)))
 (let ((?x4437 (storage_s x_0 x_SLOAD_0 w_3 w_2 5 w)))
 (= ?x4437 ?x4885))))
 ))
 (let (($x3723 (exc_halt_t 4)))
 (let (($x3979 (exc_halt_s 5)))
 (let (($x7567 (= $x3979 $x3723)))
 (let (($x5020 (forall ((n (_ BitVec 6)) )(let ((?x3757 (sc_t 4)))
 (let (($x8481 (bvsle ?x3757 n)))
 (let ((?x4581 (stack_t x_0 x_SLOAD_0 w_3 w_2 4 n)))
 (let ((?x21 (stack_s x_0 x_SLOAD_0 w_3 w_2 5 n)))
 (let (($x4591 (= ?x21 ?x4581)))
 (or $x4591 $x8481)))))))
 ))
 (let ((?x3757 (sc_t 4)))
 (let ((?x805 (sc_s 5)))
 (let (($x9143 (= ?x805 ?x3757)))
 (let ((?x4430 (used_gas_t x_0 x_SLOAD_0 w_3 w_2 0)))
 (let ((?x4504 (used_gas_s x_0 x_SLOAD_0 w_3 w_2 0)))
 (let (($x688 (= ?x4504 ?x4430)))
 (let (($x701 (forall ((w (_ BitVec 256)) )(let ((?x259 (storage_t x_0 x_SLOAD_0 w_3 w_2 0 w)))
 (let ((?x251 (storage_s x_0 x_SLOAD_0 w_3 w_2 0 w)))
 (= ?x251 ?x259))))
 ))
 (let (($x56 (exc_halt_t 0)))
 (let (($x57 (exc_halt_s 0)))
 (let (($x58 (= $x57 $x56)))
 (let (($x4796 (forall ((n (_ BitVec 6)) )(let ((?x414 (stack_t x_0 x_SLOAD_0 w_3 w_2 0 n)))
 (let ((?x4323 (stack_s x_0 x_SLOAD_0 w_3 w_2 0 n)))
 (let (($x4318 (= ?x4323 ?x414)))
 (let ((?x63 (sc_t 0)))
 (let (($x2788 (bvsle ?x63 n)))
 (or $x2788 $x4318)))))))
 ))
 (let ((?x63 (sc_t 0)))
 (let ((?x72 (sc_s 0)))
 (let (($x73 (= ?x72 ?x63)))
 (let (($x3399 (= $x3723 (or (exc_halt_t 3) (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_t 3))))))))
 (let (($x371 (forall ((w (_ BitVec 256)) )(let ((?x788 (storage_t x_0 x_SLOAD_0 w_3 w_2 3 w)))
 (let ((?x4885 (storage_t x_0 x_SLOAD_0 w_3 w_2 4 w)))
 (= ?x4885 ?x788))))
 ))
 (let (($x4809 (forall ((n (_ BitVec 6)) )(let ((?x4356 (stack_t x_0 x_SLOAD_0 w_3 w_2 3 n)))
 (let ((?x4581 (stack_t x_0 x_SLOAD_0 w_3 w_2 4 n)))
 (or (bvsle (bvadd (_ bv63 6) (sc_t 3)) n) (= ?x4581 ?x4356)))))
 ))
 (let (($x5100 (= (used_gas_t x_0 x_SLOAD_0 w_3 w_2 4) (+ 200 (used_gas_t x_0 x_SLOAD_0 w_3 w_2 3)))))
 (let ((?x2012 (sc_t 3)))
 (let ((?x5224 (bvadd (_ bv63 6) ?x2012)))
 (let ((?x4847 (stack_t x_0 x_SLOAD_0 w_3 w_2 3 ?x5224)))
 (let (($x225 (= (stack_t x_0 x_SLOAD_0 w_3 w_2 4 (bvadd (_ bv63 6) ?x3757)) (storage_t x_0 x_SLOAD_0 w_3 w_2 3 ?x4847))))
 (let (($x1917 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_t 2)))) (_ bv0 1)))))
 (let (($x903 (exc_halt_t 2)))
 (let (($x10336 (exc_halt_t 3)))
 (let (($x6422 (= $x10336 (or $x903 $x1917 (not (bvsle (_ bv0 6) (bvadd (_ bv61 6) (sc_t 2))))))))
 (let (($x4852 (forall ((w (_ BitVec 256)) )(let ((?x4587 (storage_t x_0 x_SLOAD_0 w_3 w_2 2 w)))
 (let ((?x788 (storage_t x_0 x_SLOAD_0 w_3 w_2 3 w)))
 (= ?x788 ?x4587))))
 ))
 (let (($x4708 (forall ((n (_ BitVec 6)) )(let ((?x4056 (sc_t 2)))
 (let ((?x5631 (bvadd (_ bv61 6) ?x4056)))
 (let (($x2460 (bvsle ?x5631 n)))
 (let ((?x862 (stack_t x_0 x_SLOAD_0 w_3 w_2 2 n)))
 (let ((?x4356 (stack_t x_0 x_SLOAD_0 w_3 w_2 3 n)))
 (or (= ?x4356 ?x862) $x2460)))))))
 ))
 (let (($x9325 (= ?x2012 (bvadd (_ bv1 6) (sc_t 2)))))
 (let ((?x9708 (used_gas_t x_0 x_SLOAD_0 w_3 w_2 3)))
 (let (($x4842 (= (stack_t x_0 x_SLOAD_0 w_3 w_2 3 (bvadd (_ bv63 6) (sc_t 2))) (stack_t x_0 x_SLOAD_0 w_3 w_2 2 (bvadd (_ bv63 6) (sc_t 2))))))
 (let (($x4820 (= (stack_t x_0 x_SLOAD_0 w_3 w_2 3 (bvadd (_ bv62 6) (sc_t 2))) (stack_t x_0 x_SLOAD_0 w_3 w_2 2 (bvadd (_ bv62 6) (sc_t 2))))))
 (let ((?x4056 (sc_t 2)))
 (let ((?x5631 (bvadd (_ bv61 6) ?x4056)))
 (let ((?x4838 (stack_t x_0 x_SLOAD_0 w_3 w_2 2 ?x5631)))
 (let (($x5482 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_t 1)))) (_ bv0 1)))))
 (let (($x1920 (exc_halt_t 1)))
 (let (($x8644 (= $x903 (or $x1920 $x5482))))
 (let (($x4839 (forall ((w (_ BitVec 256)) )(let ((?x3194 (storage_t x_0 x_SLOAD_0 w_3 w_2 1 w)))
 (let ((?x4587 (storage_t x_0 x_SLOAD_0 w_3 w_2 2 w)))
 (= ?x4587 ?x3194))))
 ))
 (let (($x4830 (forall ((n (_ BitVec 6)) )(let ((?x4023 (sc_t 1)))
 (let (($x4928 (bvsle ?x4023 n)))
 (let ((?x4471 (stack_t x_0 x_SLOAD_0 w_3 w_2 1 n)))
 (let ((?x862 (stack_t x_0 x_SLOAD_0 w_3 w_2 2 n)))
 (or (= ?x862 ?x4471) $x4928))))))
 ))
 (let (($x2529 (= ?x4056 (bvadd (_ bv1 6) (sc_t 1)))))
 (let ((?x715 (used_gas_t x_0 x_SLOAD_0 w_3 w_2 2)))
 (let (($x6203 (= $x1920 (or $x56 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x63))) (_ bv0 1)))))))
 (let (($x4355 (forall ((w (_ BitVec 256)) )(let ((?x259 (storage_t x_0 x_SLOAD_0 w_3 w_2 0 w)))
 (let ((?x3194 (storage_t x_0 x_SLOAD_0 w_3 w_2 1 w)))
 (= ?x3194 ?x259))))
 ))
 (let (($x5041 (forall ((n (_ BitVec 6)) )(let ((?x414 (stack_t x_0 x_SLOAD_0 w_3 w_2 0 n)))
 (let ((?x4471 (stack_t x_0 x_SLOAD_0 w_3 w_2 1 n)))
 (let ((?x63 (sc_t 0)))
 (let (($x2788 (bvsle ?x63 n)))
 (or $x2788 (= ?x4471 ?x414)))))))
 ))
 (let ((?x4023 (sc_t 1)))
 (let (($x7517 (= ?x4023 (bvadd (_ bv1 6) ?x63))))
 (let (($x4872 (= $x3979 (or (exc_halt_s 4) (not (bvsle (_ bv0 6) (bvadd (_ bv61 6) (sc_s 4))))))))
 (let (($x4533 (forall ((w (_ BitVec 256)) )(let ((?x4596 (storage_s x_0 x_SLOAD_0 w_3 w_2 4 w)))
 (let ((?x4437 (storage_s x_0 x_SLOAD_0 w_3 w_2 5 w)))
 (= ?x4437 ?x4596))))
 ))
 (let (($x4633 (forall ((n (_ BitVec 6)) )(let ((?x5008 (stack_s x_0 x_SLOAD_0 w_3 w_2 4 n)))
 (let ((?x21 (stack_s x_0 x_SLOAD_0 w_3 w_2 5 n)))
 (or (= ?x21 ?x5008) (bvsle (bvadd (_ bv61 6) (sc_s 4)) n)))))
 ))
 (let ((?x4305 (sc_s 4)))
 (let (($x445 (= ?x805 ?x4305)))
 (let (($x3354 (= (used_gas_s x_0 x_SLOAD_0 w_3 w_2 5) (+ 3 (used_gas_s x_0 x_SLOAD_0 w_3 w_2 4)))))
 (let (($x5044 (= (stack_s x_0 x_SLOAD_0 w_3 w_2 5 (bvadd (_ bv62 6) ?x805)) (stack_s x_0 x_SLOAD_0 w_3 w_2 4 (bvadd (_ bv62 6) ?x4305)))))
 (let (($x5027 (= (stack_s x_0 x_SLOAD_0 w_3 w_2 5 (bvadd (_ bv61 6) ?x805)) (stack_s x_0 x_SLOAD_0 w_3 w_2 4 (bvadd (_ bv63 6) ?x4305)))))
 (let (($x5032 (= (stack_s x_0 x_SLOAD_0 w_3 w_2 5 (bvadd (_ bv63 6) ?x805)) (stack_s x_0 x_SLOAD_0 w_3 w_2 4 (bvadd (_ bv61 6) ?x4305)))))
 (let (($x292 (exc_halt_s 3)))
 (let (($x2337 (or $x292 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 3)))) (_ bv0 1))))))
 (let (($x64 (exc_halt_s 4)))
 (let (($x8416 (= $x64 $x2337)))
 (let (($x5040 (forall ((w (_ BitVec 256)) )(let ((?x496 (storage_s x_0 x_SLOAD_0 w_3 w_2 3 w)))
 (let ((?x4596 (storage_s x_0 x_SLOAD_0 w_3 w_2 4 w)))
 (= ?x4596 ?x496))))
 ))
 (let (($x5015 (forall ((n (_ BitVec 6)) )(let ((?x167 (stack_s x_0 x_SLOAD_0 w_3 w_2 3 n)))
 (let ((?x5008 (stack_s x_0 x_SLOAD_0 w_3 w_2 4 n)))
 (let ((?x275 (sc_s 3)))
 (let (($x2175 (bvsle ?x275 n)))
 (or $x2175 (= ?x5008 ?x167)))))))
 ))
 (let (($x6906 (= ?x4305 (bvadd (_ bv1 6) (sc_s 3)))))
 (let ((?x5047 (used_gas_s x_0 x_SLOAD_0 w_3 w_2 4)))
 (let (($x2638 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 2)))) (_ bv0 1)))))
 (let (($x247 (exc_halt_s 2)))
 (let (($x3093 (= $x292 (or $x247 $x2638))))
 (let (($x432 (forall ((w (_ BitVec 256)) )(let ((?x4674 (storage_s x_0 x_SLOAD_0 w_3 w_2 2 w)))
 (let ((?x496 (storage_s x_0 x_SLOAD_0 w_3 w_2 3 w)))
 (= ?x496 ?x4674))))
 ))
 (let (($x4357 (forall ((n (_ BitVec 6)) )(let ((?x4557 (stack_s x_0 x_SLOAD_0 w_3 w_2 2 n)))
 (let ((?x167 (stack_s x_0 x_SLOAD_0 w_3 w_2 3 n)))
 (let ((?x218 (sc_s 2)))
 (let (($x3725 (bvsle ?x218 n)))
 (or $x3725 (= ?x167 ?x4557)))))))
 ))
 (let ((?x275 (sc_s 3)))
 (let (($x1897 (= ?x275 (bvadd (_ bv1 6) (sc_s 2)))))
 (let ((?x4525 (used_gas_s x_0 x_SLOAD_0 w_3 w_2 3)))
 (let (($x3736 (= $x247 (or (exc_halt_s 1) (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_s 1))))))))
 (let (($x4642 (forall ((w (_ BitVec 256)) )(let ((?x552 (storage_s x_0 x_SLOAD_0 w_3 w_2 1 w)))
 (let ((?x4674 (storage_s x_0 x_SLOAD_0 w_3 w_2 2 w)))
 (= ?x4674 ?x552))))
 ))
 (let (($x4593 (forall ((n (_ BitVec 6)) )(let ((?x4522 (stack_s x_0 x_SLOAD_0 w_3 w_2 1 n)))
 (let ((?x4557 (stack_s x_0 x_SLOAD_0 w_3 w_2 2 n)))
 (let ((?x154 (sc_s 1)))
 (let ((?x7444 (bvadd (_ bv63 6) ?x154)))
 (let (($x2123 (bvsle ?x7444 n)))
 (or $x2123 (= ?x4557 ?x4522))))))))
 ))
 (let ((?x154 (sc_s 1)))
 (let ((?x218 (sc_s 2)))
 (let (($x8214 (= ?x218 ?x154)))
 (let ((?x4392 (used_gas_s x_0 x_SLOAD_0 w_3 w_2 2)))
 (let ((?x7444 (bvadd (_ bv63 6) ?x154)))
 (let ((?x4666 (stack_s x_0 x_SLOAD_0 w_3 w_2 1 ?x7444)))
 (let (($x9858 (= (stack_s x_0 x_SLOAD_0 w_3 w_2 2 (bvadd (_ bv63 6) ?x218)) (storage_s x_0 x_SLOAD_0 w_3 w_2 1 ?x4666))))
 (let (($x2400 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x72))) (_ bv0 1)))))
 (let (($x189 (exc_halt_s 1)))
 (let (($x5916 (= $x189 (or $x57 (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) ?x72))) $x2400))))
 (let (($x4766 (forall ((w (_ BitVec 256)) )(let ((?x251 (storage_s x_0 x_SLOAD_0 w_3 w_2 0 w)))
 (let ((?x552 (storage_s x_0 x_SLOAD_0 w_3 w_2 1 w)))
 (= ?x552 ?x251))))
 ))
 (let (($x4752 (forall ((n (_ BitVec 6)) )(let ((?x72 (sc_s 0)))
 (let ((?x2582 (bvadd (_ bv63 6) ?x72)))
 (let (($x5313 (bvsle ?x2582 n)))
 (let ((?x4323 (stack_s x_0 x_SLOAD_0 w_3 w_2 0 n)))
 (let ((?x4522 (stack_s x_0 x_SLOAD_0 w_3 w_2 1 n)))
 (or (= ?x4522 ?x4323) $x5313)))))))
 ))
 (let (($x8455 (= ?x154 (bvadd (_ bv1 6) ?x72))))
 (let ((?x2582 (bvadd (_ bv63 6) ?x72)))
 (let ((?x4772 (stack_s x_0 x_SLOAD_0 w_3 w_2 0 ?x2582)))
 (let (($x4764 (forall ((w (_ BitVec 256)) )(let (($x4624 (= w (stack_s x_0 x_SLOAD_0 w_3 w_2 1 (bvadd (_ bv63 6) (sc_s 1))))))
 (let ((?x251 (storage_s x_0 x_SLOAD_0 w_3 w_2 0 w)))
 (= ?x251 (ite $x4624 x_SLOAD_0 (_ bv0 256))))))
 ))
 (let (($x4794 (= ?x4504 0)))
 (let (($x2233 (not $x57)))
 (let (($x4723 (= (stack_s x_0 x_SLOAD_0 w_3 w_2 0 (_ bv0 6)) x_0)))
 (let (($x8685 (= ?x72 (_ bv1 6))))
 (and $x8685 $x4723 $x2233 $x4794 $x4764 (= ?x4666 ?x4772) (= (stack_s x_0 x_SLOAD_0 w_3 w_2 1 ?x2582) ?x4772) (= (used_gas_s x_0 x_SLOAD_0 w_3 w_2 1) (+ 3 ?x4504)) $x8455 $x4752 $x4766 $x5916 $x9858 (= ?x4392 (+ 200 (used_gas_s x_0 x_SLOAD_0 w_3 w_2 1))) $x8214 $x4593 $x4642 $x3736 (= (stack_s x_0 x_SLOAD_0 w_3 w_2 3 ?x218) w_3) (= ?x4525 (+ 3 ?x4392)) $x1897 $x4357 $x432 $x3093 (= (stack_s x_0 x_SLOAD_0 w_3 w_2 4 ?x275) w_2) (= ?x5047 (+ 3 ?x4525)) $x6906 $x5015 $x5040 $x8416 $x5032 $x5027 $x5044 $x3354 $x445 $x4633 $x4533 $x4872 (= (stack_t x_0 x_SLOAD_0 w_3 w_2 1 ?x63) w_2) (= (used_gas_t x_0 x_SLOAD_0 w_3 w_2 1) (+ 3 ?x4430)) $x7517 $x5041 $x4355 $x6203 (= (stack_t x_0 x_SLOAD_0 w_3 w_2 2 ?x4023) w_3) (= ?x715 (+ 3 (used_gas_t x_0 x_SLOAD_0 w_3 w_2 1))) $x2529 $x4830 $x4839 $x8644 (= ?x4847 ?x4838) (= (stack_t x_0 x_SLOAD_0 w_3 w_2 3 ?x5631) ?x4838) $x4820 $x4842 (= ?x9708 (+ 3 ?x715)) $x9325 $x4708 $x4852 $x6422 $x225 $x5100 (= ?x3757 ?x2012) $x4809 $x371 $x3399 $x73 $x4796 $x58 $x701 $x688 (not (and $x9143 $x5020 $x7567 $x4543))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
 )
(check-sat)
(get-proof)
