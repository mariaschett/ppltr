; SWAP1 POP PUSH cw_1 SLOAD DUP2 SWAP1 => PUSH cw_1 DUP2 SWAP3 POP SLOAD
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
 (exists ((x_0 (_ BitVec 256)) (x_1 (_ BitVec 256)) (x_SLOAD_0 (_ BitVec 256)) )(let (($x4906 (forall ((w (_ BitVec 256)) )(let ((?x4867 (storage_t x_0 x_1 x_SLOAD_0 w_1 5 w)))
 (let ((?x1242 (storage_s x_0 x_1 x_SLOAD_0 w_1 6 w)))
 (= ?x1242 ?x4867))))
 ))
 (let (($x886 (exc_halt_t 5)))
 (let (($x772 (exc_halt_s 6)))
 (let (($x889 (= $x772 $x886)))
 (let (($x4271 (forall ((n (_ BitVec 6)) )(let ((?x754 (stack_t x_0 x_1 x_SLOAD_0 w_1 5 n)))
 (let ((?x11783 (stack_s x_0 x_1 x_SLOAD_0 w_1 6 n)))
 (let (($x11785 (= ?x11783 ?x754)))
 (let ((?x919 (sc_t 5)))
 (let (($x8015 (bvsle ?x919 n)))
 (or $x8015 $x11785)))))))
 ))
 (let ((?x919 (sc_t 5)))
 (let ((?x926 (sc_s 6)))
 (let (($x929 (= ?x926 ?x919)))
 (let ((?x11784 (used_gas_t x_0 x_1 x_SLOAD_0 w_1 0)))
 (let ((?x2811 (used_gas_s x_0 x_1 x_SLOAD_0 w_1 0)))
 (let (($x9471 (= ?x2811 ?x11784)))
 (let (($x8823 (forall ((w (_ BitVec 256)) )(let ((?x11822 (storage_t x_0 x_1 x_SLOAD_0 w_1 0 w)))
 (let ((?x8054 (storage_s x_0 x_1 x_SLOAD_0 w_1 0 w)))
 (= ?x8054 ?x11822))))
 ))
 (let (($x56 (exc_halt_t 0)))
 (let (($x57 (exc_halt_s 0)))
 (let (($x58 (= $x57 $x56)))
 (let (($x233 (forall ((n (_ BitVec 6)) )(let ((?x63 (sc_t 0)))
 (let (($x10077 (bvsle ?x63 n)))
 (let ((?x454 (stack_t x_0 x_1 x_SLOAD_0 w_1 0 n)))
 (let ((?x11774 (stack_s x_0 x_1 x_SLOAD_0 w_1 0 n)))
 (let (($x6074 (= ?x11774 ?x454)))
 (or $x6074 $x10077)))))))
 ))
 (let ((?x63 (sc_t 0)))
 (let ((?x72 (sc_s 0)))
 (let (($x73 (= ?x72 ?x63)))
 (let (($x4316 (= $x886 (or (exc_halt_t 4) (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_t 4))))))))
 (let (($x2457 (forall ((w (_ BitVec 256)) )(let ((?x1176 (storage_t x_0 x_1 x_SLOAD_0 w_1 4 w)))
 (let ((?x4867 (storage_t x_0 x_1 x_SLOAD_0 w_1 5 w)))
 (= ?x4867 ?x1176))))
 ))
 (let (($x1721 (forall ((n (_ BitVec 6)) )(let ((?x3757 (sc_t 4)))
 (let ((?x4637 (bvadd (_ bv63 6) ?x3757)))
 (let (($x8557 (bvsle ?x4637 n)))
 (let ((?x11840 (stack_t x_0 x_1 x_SLOAD_0 w_1 4 n)))
 (let ((?x754 (stack_t x_0 x_1 x_SLOAD_0 w_1 5 n)))
 (or (= ?x754 ?x11840) $x8557)))))))
 ))
 (let (($x5473 (= (used_gas_t x_0 x_1 x_SLOAD_0 w_1 5) (+ 200 (used_gas_t x_0 x_1 x_SLOAD_0 w_1 4)))))
 (let ((?x369 (storage_t x_0 x_1 x_SLOAD_0 w_1 4 (stack_t x_0 x_1 x_SLOAD_0 w_1 4 (bvadd (_ bv63 6) (sc_t 4))))))
 (let (($x3723 (exc_halt_t 4)))
 (let (($x10208 (= $x3723 (or (exc_halt_t 3) (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_t 3))))))))
 (let (($x10248 (forall ((w (_ BitVec 256)) )(let ((?x11836 (storage_t x_0 x_1 x_SLOAD_0 w_1 3 w)))
 (let ((?x1176 (storage_t x_0 x_1 x_SLOAD_0 w_1 4 w)))
 (= ?x1176 ?x11836))))
 ))
 (let (($x5137 (forall ((n (_ BitVec 6)) )(let ((?x11353 (stack_t x_0 x_1 x_SLOAD_0 w_1 3 n)))
 (let ((?x11840 (stack_t x_0 x_1 x_SLOAD_0 w_1 4 n)))
 (let ((?x3005 (sc_t 3)))
 (let ((?x4707 (bvadd (_ bv63 6) ?x3005)))
 (let (($x1150 (bvsle ?x4707 n)))
 (or $x1150 (= ?x11840 ?x11353))))))))
 ))
 (let ((?x3005 (sc_t 3)))
 (let ((?x4707 (bvadd (_ bv63 6) ?x3005)))
 (let ((?x3757 (sc_t 4)))
 (let (($x5938 (= ?x3757 ?x4707)))
 (let ((?x2397 (used_gas_t x_0 x_1 x_SLOAD_0 w_1 4)))
 (let (($x8325 (exc_halt_t 3)))
 (let (($x10245 (= $x8325 (or (exc_halt_t 2) (not (bvsle (_ bv0 6) (bvadd (_ bv60 6) (sc_t 2))))))))
 (let (($x10246 (forall ((w (_ BitVec 256)) )(let ((?x6876 (storage_t x_0 x_1 x_SLOAD_0 w_1 2 w)))
 (let ((?x11836 (storage_t x_0 x_1 x_SLOAD_0 w_1 3 w)))
 (= ?x11836 ?x6876))))
 ))
 (let (($x9786 (forall ((n (_ BitVec 6)) )(let ((?x4056 (sc_t 2)))
 (let ((?x4641 (bvadd (_ bv60 6) ?x4056)))
 (let (($x11571 (bvsle ?x4641 n)))
 (let ((?x8223 (stack_t x_0 x_1 x_SLOAD_0 w_1 2 n)))
 (let ((?x11353 (stack_t x_0 x_1 x_SLOAD_0 w_1 3 n)))
 (or (= ?x11353 ?x8223) $x11571)))))))
 ))
 (let ((?x4056 (sc_t 2)))
 (let (($x5357 (= ?x3005 ?x4056)))
 (let ((?x59 (used_gas_t x_0 x_1 x_SLOAD_0 w_1 3)))
 (let (($x9927 (= (stack_t x_0 x_1 x_SLOAD_0 w_1 3 (bvadd (_ bv62 6) ?x3005)) (stack_t x_0 x_1 x_SLOAD_0 w_1 2 (bvadd (_ bv62 6) ?x4056)))))
 (let (($x4758 (= (stack_t x_0 x_1 x_SLOAD_0 w_1 3 (bvadd (_ bv61 6) ?x3005)) (stack_t x_0 x_1 x_SLOAD_0 w_1 2 (bvadd (_ bv61 6) ?x4056)))))
 (let ((?x5207 (bvadd (_ bv63 6) ?x4056)))
 (let ((?x389 (stack_t x_0 x_1 x_SLOAD_0 w_1 2 ?x5207)))
 (let (($x4752 (= (stack_t x_0 x_1 x_SLOAD_0 w_1 3 ?x4707) (stack_t x_0 x_1 x_SLOAD_0 w_1 2 (bvadd (_ bv60 6) ?x4056)))))
 (let (($x7602 (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_t 1))))))
 (let (($x4670 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_t 1)))) (_ bv0 1)))))
 (let (($x1920 (exc_halt_t 1)))
 (let (($x903 (exc_halt_t 2)))
 (let (($x5650 (forall ((w (_ BitVec 256)) )(let ((?x469 (storage_t x_0 x_1 x_SLOAD_0 w_1 1 w)))
 (let ((?x6876 (storage_t x_0 x_1 x_SLOAD_0 w_1 2 w)))
 (= ?x6876 ?x469))))
 ))
 (let (($x2592 (forall ((n (_ BitVec 6)) )(let ((?x4023 (sc_t 1)))
 (let ((?x8180 (bvadd (_ bv62 6) ?x4023)))
 (let (($x6372 (bvsle ?x8180 n)))
 (let ((?x5709 (stack_t x_0 x_1 x_SLOAD_0 w_1 1 n)))
 (let ((?x8223 (stack_t x_0 x_1 x_SLOAD_0 w_1 2 n)))
 (or (= ?x8223 ?x5709) $x6372)))))))
 ))
 (let (($x7292 (= ?x4056 (bvadd (_ bv1 6) (sc_t 1)))))
 (let ((?x6602 (used_gas_t x_0 x_1 x_SLOAD_0 w_1 2)))
 (let (($x2790 (= (stack_t x_0 x_1 x_SLOAD_0 w_1 2 (bvadd (_ bv63 6) (sc_t 1))) (stack_t x_0 x_1 x_SLOAD_0 w_1 1 (bvadd (_ bv63 6) (sc_t 1))))))
 (let ((?x4023 (sc_t 1)))
 (let ((?x8180 (bvadd (_ bv62 6) ?x4023)))
 (let ((?x8467 (stack_t x_0 x_1 x_SLOAD_0 w_1 1 ?x8180)))
 (let (($x4277 (= $x1920 (or $x56 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x63))) (_ bv0 1)))))))
 (let (($x11725 (forall ((w (_ BitVec 256)) )(let ((?x11822 (storage_t x_0 x_1 x_SLOAD_0 w_1 0 w)))
 (let ((?x469 (storage_t x_0 x_1 x_SLOAD_0 w_1 1 w)))
 (= ?x469 ?x11822))))
 ))
 (let (($x1594 (forall ((n (_ BitVec 6)) )(let ((?x63 (sc_t 0)))
 (let (($x10077 (bvsle ?x63 n)))
 (let ((?x454 (stack_t x_0 x_1 x_SLOAD_0 w_1 0 n)))
 (let ((?x5709 (stack_t x_0 x_1 x_SLOAD_0 w_1 1 n)))
 (or (= ?x5709 ?x454) $x10077))))))
 ))
 (let (($x5112 (= ?x4023 (bvadd (_ bv1 6) ?x63))))
 (let (($x1241 (= $x772 (or (exc_halt_s 5) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 5))))))))
 (let (($x8397 (forall ((w (_ BitVec 256)) )(let ((?x6489 (storage_s x_0 x_1 x_SLOAD_0 w_1 5 w)))
 (let ((?x1242 (storage_s x_0 x_1 x_SLOAD_0 w_1 6 w)))
 (= ?x1242 ?x6489))))
 ))
 (let (($x7765 (forall ((n (_ BitVec 6)) )(let ((?x245 (stack_s x_0 x_1 x_SLOAD_0 w_1 5 n)))
 (let ((?x11783 (stack_s x_0 x_1 x_SLOAD_0 w_1 6 n)))
 (or (= ?x11783 ?x245) (bvsle (bvadd (_ bv62 6) (sc_s 5)) n)))))
 ))
 (let (($x4267 (= (used_gas_s x_0 x_1 x_SLOAD_0 w_1 6) (+ 3 (used_gas_s x_0 x_1 x_SLOAD_0 w_1 5)))))
 (let ((?x805 (sc_s 5)))
 (let ((?x11362 (bvadd (_ bv63 6) ?x805)))
 (let ((?x11568 (stack_s x_0 x_1 x_SLOAD_0 w_1 5 ?x11362)))
 (let (($x2119 (= (stack_s x_0 x_1 x_SLOAD_0 w_1 6 (bvadd (_ bv63 6) ?x926)) (stack_s x_0 x_1 x_SLOAD_0 w_1 5 (bvadd (_ bv62 6) ?x805)))))
 (let (($x10510 (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 4))))))
 (let (($x64 (exc_halt_s 4)))
 (let (($x373 (or $x64 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 4)))) (_ bv0 1))) $x10510)))
 (let (($x3979 (exc_halt_s 5)))
 (let (($x976 (forall ((w (_ BitVec 256)) )(let ((?x3445 (storage_s x_0 x_1 x_SLOAD_0 w_1 4 w)))
 (let ((?x6489 (storage_s x_0 x_1 x_SLOAD_0 w_1 5 w)))
 (= ?x6489 ?x3445))))
 ))
 (let (($x3541 (forall ((n (_ BitVec 6)) )(let ((?x7691 (stack_s x_0 x_1 x_SLOAD_0 w_1 4 n)))
 (let ((?x245 (stack_s x_0 x_1 x_SLOAD_0 w_1 5 n)))
 (let ((?x4305 (sc_s 4)))
 (let ((?x5679 (bvadd (_ bv62 6) ?x4305)))
 (let (($x6848 (bvsle ?x5679 n)))
 (or $x6848 (= ?x245 ?x7691))))))))
 ))
 (let ((?x5461 (used_gas_s x_0 x_1 x_SLOAD_0 w_1 5)))
 (let ((?x4305 (sc_s 4)))
 (let ((?x11391 (bvadd (_ bv63 6) ?x4305)))
 (let ((?x5982 (stack_s x_0 x_1 x_SLOAD_0 w_1 4 ?x11391)))
 (let ((?x5679 (bvadd (_ bv62 6) ?x4305)))
 (let ((?x11640 (stack_s x_0 x_1 x_SLOAD_0 w_1 4 ?x5679)))
 (let (($x11639 (= $x64 (or (exc_halt_s 3) (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_s 3))))))))
 (let (($x9516 (forall ((w (_ BitVec 256)) )(let ((?x4643 (storage_s x_0 x_1 x_SLOAD_0 w_1 3 w)))
 (let ((?x3445 (storage_s x_0 x_1 x_SLOAD_0 w_1 4 w)))
 (= ?x3445 ?x4643))))
 ))
 (let (($x11628 (forall ((n (_ BitVec 6)) )(let ((?x8088 (stack_s x_0 x_1 x_SLOAD_0 w_1 3 n)))
 (let ((?x7691 (stack_s x_0 x_1 x_SLOAD_0 w_1 4 n)))
 (or (= ?x7691 ?x8088) (bvsle (bvadd (_ bv63 6) (sc_s 3)) n)))))
 ))
 (let ((?x275 (sc_s 3)))
 (let (($x7998 (= ?x4305 ?x275)))
 (let ((?x8931 (used_gas_s x_0 x_1 x_SLOAD_0 w_1 4)))
 (let ((?x9430 (storage_s x_0 x_1 x_SLOAD_0 w_1 3 (stack_s x_0 x_1 x_SLOAD_0 w_1 3 (bvadd (_ bv63 6) ?x275)))))
 (let (($x247 (exc_halt_s 2)))
 (let (($x9723 (or $x247 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 2)))) (_ bv0 1))))))
 (let (($x292 (exc_halt_s 3)))
 (let (($x5172 (= $x292 $x9723)))
 (let (($x158 (forall ((w (_ BitVec 256)) )(let ((?x4622 (storage_s x_0 x_1 x_SLOAD_0 w_1 2 w)))
 (let ((?x4643 (storage_s x_0 x_1 x_SLOAD_0 w_1 3 w)))
 (= ?x4643 ?x4622))))
 ))
 (let (($x11742 (forall ((n (_ BitVec 6)) )(let ((?x11825 (stack_s x_0 x_1 x_SLOAD_0 w_1 2 n)))
 (let ((?x8088 (stack_s x_0 x_1 x_SLOAD_0 w_1 3 n)))
 (let ((?x218 (sc_s 2)))
 (let (($x4124 (bvsle ?x218 n)))
 (or $x4124 (= ?x8088 ?x11825)))))))
 ))
 (let (($x1235 (= ?x275 (bvadd (_ bv1 6) (sc_s 2)))))
 (let ((?x7496 (used_gas_s x_0 x_1 x_SLOAD_0 w_1 3)))
 (let (($x9609 (= $x247 (or (exc_halt_s 1) (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_s 1))))))))
 (let (($x11424 (forall ((w (_ BitVec 256)) )(let ((?x11438 (storage_s x_0 x_1 x_SLOAD_0 w_1 1 w)))
 (let ((?x4622 (storage_s x_0 x_1 x_SLOAD_0 w_1 2 w)))
 (= ?x4622 ?x11438))))
 ))
 (let (($x1302 (forall ((n (_ BitVec 6)) )(let ((?x154 (sc_s 1)))
 (let ((?x917 (bvadd (_ bv63 6) ?x154)))
 (let (($x6822 (bvsle ?x917 n)))
 (let ((?x11843 (stack_s x_0 x_1 x_SLOAD_0 w_1 1 n)))
 (let ((?x11825 (stack_s x_0 x_1 x_SLOAD_0 w_1 2 n)))
 (or (= ?x11825 ?x11843) $x6822)))))))
 ))
 (let ((?x8194 (used_gas_s x_0 x_1 x_SLOAD_0 w_1 2)))
 (let (($x189 (exc_halt_s 1)))
 (let (($x3939 (= $x189 (or $x57 (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) ?x72)))))))
 (let (($x7250 (forall ((w (_ BitVec 256)) )(let ((?x8054 (storage_s x_0 x_1 x_SLOAD_0 w_1 0 w)))
 (let ((?x11438 (storage_s x_0 x_1 x_SLOAD_0 w_1 1 w)))
 (= ?x11438 ?x8054))))
 ))
 (let (($x2023 (forall ((n (_ BitVec 6)) )(let ((?x72 (sc_s 0)))
 (let ((?x3868 (bvadd (_ bv62 6) ?x72)))
 (let (($x11328 (bvsle ?x3868 n)))
 (let ((?x11774 (stack_s x_0 x_1 x_SLOAD_0 w_1 0 n)))
 (let ((?x11843 (stack_s x_0 x_1 x_SLOAD_0 w_1 1 n)))
 (or (= ?x11843 ?x11774) $x11328)))))))
 ))
 (let ((?x154 (sc_s 1)))
 (let (($x601 (= ?x154 ?x72)))
 (let (($x9901 (= (stack_s x_0 x_1 x_SLOAD_0 w_1 1 (bvadd (_ bv62 6) ?x154)) (stack_s x_0 x_1 x_SLOAD_0 w_1 0 (bvadd (_ bv63 6) ?x72)))))
 (let (($x11344 (= (stack_s x_0 x_1 x_SLOAD_0 w_1 1 (bvadd (_ bv63 6) ?x154)) (stack_s x_0 x_1 x_SLOAD_0 w_1 0 (bvadd (_ bv62 6) ?x72)))))
 (let (($x584 (forall ((w (_ BitVec 256)) )(let (($x6872 (= w (stack_s x_0 x_1 x_SLOAD_0 w_1 3 (bvadd (_ bv63 6) (sc_s 3))))))
 (let ((?x8054 (storage_s x_0 x_1 x_SLOAD_0 w_1 0 w)))
 (= ?x8054 (ite $x6872 x_SLOAD_0 (_ bv0 256))))))
 ))
 (let (($x1251 (= ?x2811 0)))
 (let (($x1898 (not $x57)))
 (let (($x11560 (= (stack_s x_0 x_1 x_SLOAD_0 w_1 0 (_ bv1 6)) x_1)))
 (let (($x8926 (= (stack_s x_0 x_1 x_SLOAD_0 w_1 0 (_ bv0 6)) x_0)))
 (let (($x11392 (= ?x72 (_ bv2 6))))
 (and $x11392 $x8926 $x11560 $x1898 $x1251 $x584 $x11344 $x9901 (= (used_gas_s x_0 x_1 x_SLOAD_0 w_1 1) (+ 3 ?x2811)) $x601 $x2023 $x7250 $x3939 (= ?x8194 (+ 2 (used_gas_s x_0 x_1 x_SLOAD_0 w_1 1))) (= (sc_s 2) (bvadd (_ bv63 6) ?x154)) $x1302 $x11424 $x9609 (= (stack_s x_0 x_1 x_SLOAD_0 w_1 3 (sc_s 2)) w_1) (= ?x7496 (+ 3 ?x8194)) $x1235 $x11742 $x158 $x5172 (= ?x5982 ?x9430) (= ?x8931 (+ 200 ?x7496)) $x7998 $x11628 $x9516 $x11639 (= ?x11568 ?x11640) (= (stack_s x_0 x_1 x_SLOAD_0 w_1 5 ?x5679) ?x11640) (= (stack_s x_0 x_1 x_SLOAD_0 w_1 5 ?x11391) ?x5982) (= ?x5461 (+ 3 ?x8931)) (= ?x805 (bvadd (_ bv1 6) ?x4305)) $x3541 $x976 (= $x3979 $x373) $x2119 (= (stack_s x_0 x_1 x_SLOAD_0 w_1 6 (bvadd (_ bv62 6) ?x926)) ?x11568) $x4267 (= ?x926 ?x805) $x7765 $x8397 $x1241 (= (stack_t x_0 x_1 x_SLOAD_0 w_1 1 ?x63) w_1) (= (used_gas_t x_0 x_1 x_SLOAD_0 w_1 1) (+ 3 ?x11784)) $x5112 $x1594 $x11725 $x4277 (= ?x389 ?x8467) (= (stack_t x_0 x_1 x_SLOAD_0 w_1 2 ?x8180) ?x8467) $x2790 (= ?x6602 (+ 3 (used_gas_t x_0 x_1 x_SLOAD_0 w_1 1))) $x7292 $x2592 $x5650 (= $x903 (or $x1920 $x4670 $x7602)) $x4752 (= (stack_t x_0 x_1 x_SLOAD_0 w_1 3 (bvadd (_ bv60 6) ?x3005)) ?x389) $x4758 $x9927 (= ?x59 (+ 3 ?x6602)) $x5357 $x9786 $x10246 $x10245 (= ?x2397 (+ 2 ?x59)) $x5938 $x5137 $x10248 $x10208 (= (stack_t x_0 x_1 x_SLOAD_0 w_1 5 (bvadd (_ bv63 6) ?x919)) ?x369) $x5473 (= ?x919 ?x3757) $x1721 $x2457 $x4316 $x73 $x233 $x58 $x8823 $x9471 (not (and $x929 $x4271 $x889 $x4906))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
 )
(check-sat)
(get-proof)