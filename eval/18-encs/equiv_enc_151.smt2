; PUSH cw_4 PUSH cw_3 DUP2 CALLDATALOAD SWAP3 SWAP2 => PUSH cw_4 CALLDATALOAD SWAP1 PUSH cw_3 PUSH cw_4
(set-option :produce-proofs true)
; 
(set-info :status unknown)
(declare-fun storage_t ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 256)) (_ BitVec 256))
(declare-fun w_3 () (_ BitVec 256))
(declare-fun w_4 () (_ BitVec 256))
(declare-fun storage_s ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 256)) (_ BitVec 256))
(declare-fun exc_halt_t (Int) Bool)
(declare-fun exc_halt_s (Int) Bool)
(declare-fun sc_t (Int) (_ BitVec 6))
(declare-fun stack_t ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 6)) (_ BitVec 256))
(declare-fun stack_s ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 6)) (_ BitVec 256))
(declare-fun sc_s (Int) (_ BitVec 6))
(declare-fun used_gas_t ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int) Int)
(declare-fun used_gas_s ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int) Int)
(declare-fun f_CALLDATALOAD ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256)) (_ BitVec 256))
(assert
 (exists ((x_0 (_ BitVec 256)) (x_CALLDATALOAD_0 (_ BitVec 256)) )(let (($x5023 (forall ((w (_ BitVec 256)) )(let ((?x756 (storage_t x_0 w_4 w_3 x_CALLDATALOAD_0 5 w)))
 (let ((?x108 (storage_s x_0 w_4 w_3 x_CALLDATALOAD_0 6 w)))
 (= ?x108 ?x756))))
 ))
 (let (($x886 (exc_halt_t 5)))
 (let (($x772 (exc_halt_s 6)))
 (let (($x889 (= $x772 $x886)))
 (let (($x6887 (forall ((n (_ BitVec 6)) )(let ((?x9108 (stack_t x_0 w_4 w_3 x_CALLDATALOAD_0 5 n)))
 (let ((?x4744 (stack_s x_0 w_4 w_3 x_CALLDATALOAD_0 6 n)))
 (let (($x4716 (= ?x4744 ?x9108)))
 (or $x4716 (bvsle (sc_t 5) n))))))
 ))
 (let ((?x919 (sc_t 5)))
 (let ((?x926 (sc_s 6)))
 (let (($x929 (= ?x926 ?x919)))
 (let ((?x4733 (used_gas_t x_0 w_4 w_3 x_CALLDATALOAD_0 0)))
 (let ((?x4427 (used_gas_s x_0 w_4 w_3 x_CALLDATALOAD_0 0)))
 (let (($x4722 (= ?x4427 ?x4733)))
 (let (($x4889 (forall ((w (_ BitVec 256)) )(let ((?x4584 (storage_t x_0 w_4 w_3 x_CALLDATALOAD_0 0 w)))
 (let ((?x4729 (storage_s x_0 w_4 w_3 x_CALLDATALOAD_0 0 w)))
 (= ?x4729 ?x4584))))
 ))
 (let (($x56 (exc_halt_t 0)))
 (let (($x57 (exc_halt_s 0)))
 (let (($x58 (= $x57 $x56)))
 (let (($x2465 (forall ((n (_ BitVec 6)) )(let ((?x63 (sc_t 0)))
 (let (($x8037 (bvsle ?x63 n)))
 (let ((?x4688 (stack_t x_0 w_4 w_3 x_CALLDATALOAD_0 0 n)))
 (let ((?x4673 (stack_s x_0 w_4 w_3 x_CALLDATALOAD_0 0 n)))
 (let (($x4689 (= ?x4673 ?x4688)))
 (or $x4689 $x8037)))))))
 ))
 (let ((?x63 (sc_t 0)))
 (let ((?x72 (sc_s 0)))
 (let (($x73 (= ?x72 ?x63)))
 (let (($x3723 (exc_halt_t 4)))
 (let (($x1314 (or $x3723 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_t 4)))) (_ bv0 1))))))
 (let (($x5705 (forall ((w (_ BitVec 256)) )(let ((?x4687 (storage_t x_0 w_4 w_3 x_CALLDATALOAD_0 4 w)))
 (let ((?x756 (storage_t x_0 w_4 w_3 x_CALLDATALOAD_0 5 w)))
 (= ?x756 ?x4687))))
 ))
 (let (($x8565 (forall ((n (_ BitVec 6)) )(let ((?x3757 (sc_t 4)))
 (let (($x9074 (bvsle ?x3757 n)))
 (let ((?x4532 (stack_t x_0 w_4 w_3 x_CALLDATALOAD_0 4 n)))
 (let ((?x9108 (stack_t x_0 w_4 w_3 x_CALLDATALOAD_0 5 n)))
 (or (= ?x9108 ?x4532) $x9074))))))
 ))
 (let (($x2393 (= (used_gas_t x_0 w_4 w_3 x_CALLDATALOAD_0 5) (+ 3 (used_gas_t x_0 w_4 w_3 x_CALLDATALOAD_0 4)))))
 (let (($x10336 (exc_halt_t 3)))
 (let (($x8114 (or $x10336 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_t 3)))) (_ bv0 1))))))
 (let (($x9159 (forall ((w (_ BitVec 256)) )(let ((?x11034 (storage_t x_0 w_4 w_3 x_CALLDATALOAD_0 3 w)))
 (let ((?x4687 (storage_t x_0 w_4 w_3 x_CALLDATALOAD_0 4 w)))
 (= ?x4687 ?x11034))))
 ))
 (let (($x5209 (forall ((n (_ BitVec 6)) )(let ((?x2012 (sc_t 3)))
 (let (($x7712 (bvsle ?x2012 n)))
 (let ((?x3001 (stack_t x_0 w_4 w_3 x_CALLDATALOAD_0 3 n)))
 (let ((?x4532 (stack_t x_0 w_4 w_3 x_CALLDATALOAD_0 4 n)))
 (or (= ?x4532 ?x3001) $x7712))))))
 ))
 (let ((?x4499 (used_gas_t x_0 w_4 w_3 x_CALLDATALOAD_0 4)))
 (let (($x1387 (= $x10336 (or (exc_halt_t 2) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_t 2))))))))
 (let (($x9700 (forall ((w (_ BitVec 256)) )(let ((?x6654 (storage_t x_0 w_4 w_3 x_CALLDATALOAD_0 2 w)))
 (let ((?x11034 (storage_t x_0 w_4 w_3 x_CALLDATALOAD_0 3 w)))
 (= ?x11034 ?x6654))))
 ))
 (let (($x556 (forall ((n (_ BitVec 6)) )(let ((?x9078 (stack_t x_0 w_4 w_3 x_CALLDATALOAD_0 2 n)))
 (let ((?x3001 (stack_t x_0 w_4 w_3 x_CALLDATALOAD_0 3 n)))
 (let ((?x4056 (sc_t 2)))
 (let ((?x7715 (bvadd (_ bv62 6) ?x4056)))
 (let (($x1620 (bvsle ?x7715 n)))
 (or $x1620 (= ?x3001 ?x9078))))))))
 ))
 (let ((?x4056 (sc_t 2)))
 (let ((?x2012 (sc_t 3)))
 (let (($x2966 (= ?x2012 ?x4056)))
 (let ((?x10752 (used_gas_t x_0 w_4 w_3 x_CALLDATALOAD_0 3)))
 (let ((?x6079 (bvadd (_ bv63 6) ?x4056)))
 (let ((?x2290 (stack_t x_0 w_4 w_3 x_CALLDATALOAD_0 2 ?x6079)))
 (let (($x1734 (= (stack_t x_0 w_4 w_3 x_CALLDATALOAD_0 3 (bvadd (_ bv63 6) ?x2012)) (stack_t x_0 w_4 w_3 x_CALLDATALOAD_0 2 (bvadd (_ bv62 6) ?x4056)))))
 (let (($x903 (exc_halt_t 2)))
 (let (($x8683 (= $x903 (or (exc_halt_t 1) (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_t 1))))))))
 (let (($x2999 (forall ((w (_ BitVec 256)) )(let ((?x2908 (storage_t x_0 w_4 w_3 x_CALLDATALOAD_0 1 w)))
 (let ((?x6654 (storage_t x_0 w_4 w_3 x_CALLDATALOAD_0 2 w)))
 (= ?x6654 ?x2908))))
 ))
 (let (($x2271 (forall ((n (_ BitVec 6)) )(let ((?x2709 (stack_t x_0 w_4 w_3 x_CALLDATALOAD_0 1 n)))
 (let ((?x9078 (stack_t x_0 w_4 w_3 x_CALLDATALOAD_0 2 n)))
 (let ((?x4023 (sc_t 1)))
 (let ((?x6622 (bvadd (_ bv63 6) ?x4023)))
 (let (($x5230 (bvsle ?x6622 n)))
 (or $x5230 (= ?x9078 ?x2709))))))))
 ))
 (let ((?x10572 (used_gas_t x_0 w_4 w_3 x_CALLDATALOAD_0 2)))
 (let ((?x2340 (f_CALLDATALOAD x_0 w_4 w_3 x_CALLDATALOAD_0 (stack_t x_0 w_4 w_3 x_CALLDATALOAD_0 1 (bvadd (_ bv63 6) (sc_t 1))))))
 (let (($x1920 (exc_halt_t 1)))
 (let (($x2108 (= $x1920 (or $x56 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x63))) (_ bv0 1)))))))
 (let (($x6425 (forall ((w (_ BitVec 256)) )(let ((?x4584 (storage_t x_0 w_4 w_3 x_CALLDATALOAD_0 0 w)))
 (let ((?x2908 (storage_t x_0 w_4 w_3 x_CALLDATALOAD_0 1 w)))
 (= ?x2908 ?x4584))))
 ))
 (let (($x2224 (forall ((n (_ BitVec 6)) )(let ((?x4688 (stack_t x_0 w_4 w_3 x_CALLDATALOAD_0 0 n)))
 (let ((?x2709 (stack_t x_0 w_4 w_3 x_CALLDATALOAD_0 1 n)))
 (let ((?x63 (sc_t 0)))
 (let (($x8037 (bvsle ?x63 n)))
 (or $x8037 (= ?x2709 ?x4688)))))))
 ))
 (let ((?x4023 (sc_t 1)))
 (let (($x2532 (= ?x4023 (bvadd (_ bv1 6) ?x63))))
 (let (($x5424 (= $x772 (or (exc_halt_s 5) (not (bvsle (_ bv0 6) (bvadd (_ bv61 6) (sc_s 5))))))))
 (let (($x9835 (forall ((w (_ BitVec 256)) )(let ((?x726 (storage_s x_0 w_4 w_3 x_CALLDATALOAD_0 5 w)))
 (let ((?x108 (storage_s x_0 w_4 w_3 x_CALLDATALOAD_0 6 w)))
 (= ?x108 ?x726))))
 ))
 (let (($x9278 (forall ((n (_ BitVec 6)) )(let ((?x1347 (stack_s x_0 w_4 w_3 x_CALLDATALOAD_0 5 n)))
 (let ((?x4744 (stack_s x_0 w_4 w_3 x_CALLDATALOAD_0 6 n)))
 (or (= ?x4744 ?x1347) (bvsle (bvadd (_ bv61 6) (sc_s 5)) n)))))
 ))
 (let (($x4660 (= (used_gas_s x_0 w_4 w_3 x_CALLDATALOAD_0 6) (+ 3 (used_gas_s x_0 w_4 w_3 x_CALLDATALOAD_0 5)))))
 (let ((?x805 (sc_s 5)))
 (let ((?x8838 (bvadd (_ bv62 6) ?x805)))
 (let ((?x504 (stack_s x_0 w_4 w_3 x_CALLDATALOAD_0 5 ?x8838)))
 (let ((?x5642 (bvadd (_ bv63 6) ?x805)))
 (let ((?x3312 (stack_s x_0 w_4 w_3 x_CALLDATALOAD_0 5 ?x5642)))
 (let ((?x6911 (bvadd (_ bv61 6) ?x805)))
 (let ((?x5206 (stack_s x_0 w_4 w_3 x_CALLDATALOAD_0 5 ?x6911)))
 (let (($x3979 (exc_halt_s 5)))
 (let (($x1129 (= $x3979 (or (exc_halt_s 4) (not (bvsle (_ bv0 6) (bvadd (_ bv60 6) (sc_s 4))))))))
 (let (($x7334 (forall ((w (_ BitVec 256)) )(let ((?x10098 (storage_s x_0 w_4 w_3 x_CALLDATALOAD_0 4 w)))
 (let ((?x726 (storage_s x_0 w_4 w_3 x_CALLDATALOAD_0 5 w)))
 (= ?x726 ?x10098))))
 ))
 (let (($x8597 (forall ((n (_ BitVec 6)) )(let ((?x5600 (stack_s x_0 w_4 w_3 x_CALLDATALOAD_0 4 n)))
 (let ((?x1347 (stack_s x_0 w_4 w_3 x_CALLDATALOAD_0 5 n)))
 (or (bvsle (bvadd (_ bv60 6) (sc_s 4)) n) (= ?x1347 ?x5600)))))
 ))
 (let ((?x4305 (sc_s 4)))
 (let (($x5571 (= ?x805 ?x4305)))
 (let ((?x5160 (used_gas_s x_0 w_4 w_3 x_CALLDATALOAD_0 5)))
 (let ((?x697 (bvadd (_ bv63 6) ?x4305)))
 (let ((?x1594 (stack_s x_0 w_4 w_3 x_CALLDATALOAD_0 4 ?x697)))
 (let (($x64 (exc_halt_s 4)))
 (let (($x6796 (= $x64 (or (exc_halt_s 3) (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_s 3))))))))
 (let (($x7312 (forall ((w (_ BitVec 256)) )(let ((?x3086 (storage_s x_0 w_4 w_3 x_CALLDATALOAD_0 3 w)))
 (let ((?x10098 (storage_s x_0 w_4 w_3 x_CALLDATALOAD_0 4 w)))
 (= ?x10098 ?x3086))))
 ))
 (let (($x3034 (forall ((n (_ BitVec 6)) )(let ((?x8476 (stack_s x_0 w_4 w_3 x_CALLDATALOAD_0 3 n)))
 (let ((?x5600 (stack_s x_0 w_4 w_3 x_CALLDATALOAD_0 4 n)))
 (or (= ?x5600 ?x8476) (bvsle (bvadd (_ bv63 6) (sc_s 3)) n)))))
 ))
 (let ((?x275 (sc_s 3)))
 (let (($x7714 (= ?x4305 ?x275)))
 (let ((?x6788 (used_gas_s x_0 w_4 w_3 x_CALLDATALOAD_0 4)))
 (let ((?x1725 (bvadd (_ bv63 6) ?x275)))
 (let ((?x5474 (stack_s x_0 w_4 w_3 x_CALLDATALOAD_0 3 ?x1725)))
 (let (($x7784 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 2)))) (_ bv0 1)))))
 (let (($x1398 (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 2))))))
 (let (($x247 (exc_halt_s 2)))
 (let (($x292 (exc_halt_s 3)))
 (let (($x2407 (forall ((w (_ BitVec 256)) )(let ((?x2411 (storage_s x_0 w_4 w_3 x_CALLDATALOAD_0 2 w)))
 (let ((?x3086 (storage_s x_0 w_4 w_3 x_CALLDATALOAD_0 3 w)))
 (= ?x3086 ?x2411))))
 ))
 (let (($x3622 (forall ((n (_ BitVec 6)) )(let ((?x2079 (stack_s x_0 w_4 w_3 x_CALLDATALOAD_0 2 n)))
 (let ((?x8476 (stack_s x_0 w_4 w_3 x_CALLDATALOAD_0 3 n)))
 (let ((?x218 (sc_s 2)))
 (let ((?x5247 (bvadd (_ bv62 6) ?x218)))
 (let (($x4109 (bvsle ?x5247 n)))
 (or $x4109 (= ?x8476 ?x2079))))))))
 ))
 (let (($x5491 (= ?x275 (bvadd (_ bv1 6) (sc_s 2)))))
 (let ((?x3031 (used_gas_s x_0 w_4 w_3 x_CALLDATALOAD_0 3)))
 (let (($x3043 (= (stack_s x_0 w_4 w_3 x_CALLDATALOAD_0 3 (bvadd (_ bv63 6) (sc_s 2))) (stack_s x_0 w_4 w_3 x_CALLDATALOAD_0 2 (bvadd (_ bv63 6) (sc_s 2))))))
 (let ((?x218 (sc_s 2)))
 (let ((?x5247 (bvadd (_ bv62 6) ?x218)))
 (let ((?x1643 (stack_s x_0 w_4 w_3 x_CALLDATALOAD_0 2 ?x5247)))
 (let (($x1895 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 1)))) (_ bv0 1)))))
 (let (($x189 (exc_halt_s 1)))
 (let (($x5493 (= $x247 (or $x189 $x1895))))
 (let (($x6081 (forall ((w (_ BitVec 256)) )(let ((?x5709 (storage_s x_0 w_4 w_3 x_CALLDATALOAD_0 1 w)))
 (let ((?x2411 (storage_s x_0 w_4 w_3 x_CALLDATALOAD_0 2 w)))
 (= ?x2411 ?x5709))))
 ))
 (let (($x472 (forall ((n (_ BitVec 6)) )(let ((?x6261 (stack_s x_0 w_4 w_3 x_CALLDATALOAD_0 1 n)))
 (let ((?x2079 (stack_s x_0 w_4 w_3 x_CALLDATALOAD_0 2 n)))
 (let ((?x154 (sc_s 1)))
 (let (($x2942 (bvsle ?x154 n)))
 (or $x2942 (= ?x2079 ?x6261)))))))
 ))
 (let (($x9161 (= ?x218 (bvadd (_ bv1 6) (sc_s 1)))))
 (let ((?x7928 (used_gas_s x_0 w_4 w_3 x_CALLDATALOAD_0 2)))
 (let (($x7111 (= $x189 (or $x57 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x72))) (_ bv0 1)))))))
 (let (($x10878 (forall ((w (_ BitVec 256)) )(let ((?x4729 (storage_s x_0 w_4 w_3 x_CALLDATALOAD_0 0 w)))
 (let ((?x5709 (storage_s x_0 w_4 w_3 x_CALLDATALOAD_0 1 w)))
 (= ?x5709 ?x4729))))
 ))
 (let (($x9543 (forall ((n (_ BitVec 6)) )(let ((?x72 (sc_s 0)))
 (let (($x5879 (bvsle ?x72 n)))
 (let ((?x4673 (stack_s x_0 w_4 w_3 x_CALLDATALOAD_0 0 n)))
 (let ((?x6261 (stack_s x_0 w_4 w_3 x_CALLDATALOAD_0 1 n)))
 (or (= ?x6261 ?x4673) $x5879))))))
 ))
 (let ((?x154 (sc_s 1)))
 (let (($x6201 (= ?x154 (bvadd (_ bv1 6) ?x72))))
 (let (($x7941 (forall ((w0 (_ BitVec 256)) )(let (($x1769 (= (stack_s x_0 w_4 w_3 x_CALLDATALOAD_0 3 (bvadd (_ bv63 6) (sc_s 3))) w0)))
 (let ((?x317 (f_CALLDATALOAD x_0 w_4 w_3 x_CALLDATALOAD_0 w0)))
 (= ?x317 (ite $x1769 x_CALLDATALOAD_0 (_ bv0 256))))))
 ))
 (let (($x6388 (forall ((w (_ BitVec 256)) )(let ((?x4729 (storage_s x_0 w_4 w_3 x_CALLDATALOAD_0 0 w)))
 (= ?x4729 (_ bv0 256))))
 ))
 (let (($x10884 (= ?x4427 0)))
 (let (($x3899 (not $x57)))
 (let (($x9114 (= (stack_s x_0 w_4 w_3 x_CALLDATALOAD_0 0 (_ bv0 6)) x_0)))
 (let (($x8685 (= ?x72 (_ bv1 6))))
 (and $x8685 $x9114 $x3899 $x10884 $x6388 $x7941 (= (stack_s x_0 w_4 w_3 x_CALLDATALOAD_0 1 ?x72) w_4) (= (used_gas_s x_0 w_4 w_3 x_CALLDATALOAD_0 1) (+ 3 ?x4427)) $x6201 $x9543 $x10878 $x7111 (= (stack_s x_0 w_4 w_3 x_CALLDATALOAD_0 2 ?x154) w_3) (= ?x7928 (+ 3 (used_gas_s x_0 w_4 w_3 x_CALLDATALOAD_0 1))) $x9161 $x472 $x6081 $x5493 (= ?x5474 ?x1643) (= (stack_s x_0 w_4 w_3 x_CALLDATALOAD_0 3 ?x5247) ?x1643) $x3043 (= ?x3031 (+ 3 ?x7928)) $x5491 $x3622 $x2407 (= $x292 (or $x247 $x1398 $x7784)) (= ?x1594 (f_CALLDATALOAD x_0 w_4 w_3 x_CALLDATALOAD_0 ?x5474)) (= ?x6788 (+ 3 ?x3031)) $x7714 $x3034 $x7312 $x6796 (= ?x3312 (stack_s x_0 w_4 w_3 x_CALLDATALOAD_0 4 (bvadd (_ bv60 6) ?x4305))) (= (stack_s x_0 w_4 w_3 x_CALLDATALOAD_0 5 (bvadd (_ bv60 6) ?x805)) ?x1594) (= ?x5206 (stack_s x_0 w_4 w_3 x_CALLDATALOAD_0 4 (bvadd (_ bv61 6) ?x4305))) (= ?x504 (stack_s x_0 w_4 w_3 x_CALLDATALOAD_0 4 (bvadd (_ bv62 6) ?x4305))) (= ?x5160 (+ 3 ?x6788)) $x5571 $x8597 $x7334 $x1129 (= (stack_s x_0 w_4 w_3 x_CALLDATALOAD_0 6 (bvadd (_ bv63 6) ?x926)) ?x5206) (= (stack_s x_0 w_4 w_3 x_CALLDATALOAD_0 6 (bvadd (_ bv61 6) ?x926)) ?x3312) (= (stack_s x_0 w_4 w_3 x_CALLDATALOAD_0 6 (bvadd (_ bv62 6) ?x926)) ?x504) $x4660 (= ?x926 ?x805) $x9278 $x9835 $x5424 (= (stack_t x_0 w_4 w_3 x_CALLDATALOAD_0 1 ?x63) w_4) (= (used_gas_t x_0 w_4 w_3 x_CALLDATALOAD_0 1) (+ 3 ?x4733)) $x2532 $x2224 $x6425 $x2108 (= ?x2290 ?x2340) (= ?x10572 (+ 3 (used_gas_t x_0 w_4 w_3 x_CALLDATALOAD_0 1))) (= ?x4056 ?x4023) $x2271 $x2999 $x8683 $x1734 (= (stack_t x_0 w_4 w_3 x_CALLDATALOAD_0 3 (bvadd (_ bv62 6) ?x2012)) ?x2290) (= ?x10752 (+ 3 ?x10572)) $x2966 $x556 $x9700 $x1387 (= (stack_t x_0 w_4 w_3 x_CALLDATALOAD_0 4 ?x2012) w_3) (= ?x4499 (+ 3 ?x10752)) (= (sc_t 4) (bvadd (_ bv1 6) ?x2012)) $x5209 $x9159 (= $x3723 $x8114) (= (stack_t x_0 w_4 w_3 x_CALLDATALOAD_0 5 (sc_t 4)) w_4) $x2393 (= ?x919 (bvadd (_ bv1 6) (sc_t 4))) $x8565 $x5705 (= $x886 $x1314) $x73 $x2465 $x58 $x4889 $x4722 (not (and $x929 $x6887 $x889 $x5023))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
 )
(check-sat)
(get-proof)
