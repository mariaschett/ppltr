; SWAP1 SHA3 PUSH cw_1 SWAP1 => PUSH cw_1 SWAP2 SHA3
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
(declare-fun f_SHA3 ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256)) (_ BitVec 256))
(assert
 (exists ((x_0 (_ BitVec 256)) (x_1 (_ BitVec 256)) (x_SHA3_0 (_ BitVec 256)) )(let (($x6409 (forall ((w (_ BitVec 256)) )(let ((?x8822 (storage_t x_0 x_1 w_1 x_SHA3_0 3 w)))
 (let ((?x4223 (storage_s x_0 x_1 w_1 x_SHA3_0 4 w)))
 (= ?x4223 ?x8822))))
 ))
 (let (($x4112 (exc_halt_t 3)))
 (let (($x276 (exc_halt_s 4)))
 (let (($x11577 (= $x276 $x4112)))
 (let (($x2890 (forall ((n (_ BitVec 6)) )(let ((?x10246 (stack_t x_0 x_1 w_1 x_SHA3_0 3 n)))
 (let ((?x4036 (stack_s x_0 x_1 w_1 x_SHA3_0 4 n)))
 (let (($x8600 (= ?x4036 ?x10246)))
 (let ((?x11964 (sc_t 3)))
 (let (($x5242 (bvsle ?x11964 n)))
 (or $x5242 $x8600)))))))
 ))
 (let ((?x11964 (sc_t 3)))
 (let ((?x3145 (sc_s 4)))
 (let (($x695 (= ?x3145 ?x11964)))
 (let ((?x4992 (used_gas_t x_0 x_1 w_1 x_SHA3_0 0)))
 (let ((?x1384 (used_gas_s x_0 x_1 w_1 x_SHA3_0 0)))
 (let (($x2148 (= ?x1384 ?x4992)))
 (let (($x6534 (forall ((w (_ BitVec 256)) )(let ((?x9232 (storage_t x_0 x_1 w_1 x_SHA3_0 0 w)))
 (let ((?x1615 (storage_s x_0 x_1 w_1 x_SHA3_0 0 w)))
 (= ?x1615 ?x9232))))
 ))
 (let (($x56 (exc_halt_t 0)))
 (let (($x57 (exc_halt_s 0)))
 (let (($x58 (= $x57 $x56)))
 (let (($x10320 (forall ((n (_ BitVec 6)) )(let ((?x63 (sc_t 0)))
 (let (($x5904 (bvsle ?x63 n)))
 (let ((?x10735 (stack_t x_0 x_1 w_1 x_SHA3_0 0 n)))
 (let ((?x11090 (stack_s x_0 x_1 w_1 x_SHA3_0 0 n)))
 (let (($x4751 (= ?x11090 ?x10735)))
 (or $x4751 $x5904)))))))
 ))
 (let ((?x63 (sc_t 0)))
 (let ((?x72 (sc_s 0)))
 (let (($x73 (= ?x72 ?x63)))
 (let (($x46 (= $x4112 (or (exc_halt_t 2) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_t 2))))))))
 (let (($x1955 (forall ((w (_ BitVec 256)) )(let ((?x9796 (storage_t x_0 x_1 w_1 x_SHA3_0 2 w)))
 (let ((?x8822 (storage_t x_0 x_1 w_1 x_SHA3_0 3 w)))
 (= ?x8822 ?x9796))))
 ))
 (let (($x5529 (forall ((n (_ BitVec 6)) )(let ((?x4942 (stack_t x_0 x_1 w_1 x_SHA3_0 2 n)))
 (let ((?x10246 (stack_t x_0 x_1 w_1 x_SHA3_0 3 n)))
 (or (= ?x10246 ?x4942) (bvsle (bvadd (_ bv62 6) (sc_t 2)) n)))))
 ))
 (let (($x10352 (= (used_gas_t x_0 x_1 w_1 x_SHA3_0 3) (+ 30 (used_gas_t x_0 x_1 w_1 x_SHA3_0 2)))))
 (let ((?x2992 (sc_t 2)))
 (let ((?x4132 (bvadd (_ bv62 6) ?x2992)))
 (let ((?x6049 (stack_t x_0 x_1 w_1 x_SHA3_0 2 ?x4132)))
 (let ((?x2620 (bvadd (_ bv63 6) ?x2992)))
 (let ((?x6613 (stack_t x_0 x_1 w_1 x_SHA3_0 2 ?x2620)))
 (let (($x2646 (= (stack_t x_0 x_1 w_1 x_SHA3_0 3 (bvadd (_ bv63 6) ?x11964)) (f_SHA3 x_0 x_1 w_1 x_SHA3_0 ?x6613 ?x6049))))
 (let (($x9580 (exc_halt_t 2)))
 (let (($x5684 (= $x9580 (or (exc_halt_t 1) (not (bvsle (_ bv0 6) (bvadd (_ bv61 6) (sc_t 1))))))))
 (let (($x3639 (forall ((w (_ BitVec 256)) )(let ((?x10158 (storage_t x_0 x_1 w_1 x_SHA3_0 1 w)))
 (let ((?x9796 (storage_t x_0 x_1 w_1 x_SHA3_0 2 w)))
 (= ?x9796 ?x10158))))
 ))
 (let (($x9243 (forall ((n (_ BitVec 6)) )(let ((?x5222 (stack_t x_0 x_1 w_1 x_SHA3_0 1 n)))
 (let ((?x4942 (stack_t x_0 x_1 w_1 x_SHA3_0 2 n)))
 (or (bvsle (bvadd (_ bv61 6) (sc_t 1)) n) (= ?x4942 ?x5222)))))
 ))
 (let ((?x3379 (sc_t 1)))
 (let (($x10030 (= ?x2992 ?x3379)))
 (let ((?x5799 (used_gas_t x_0 x_1 w_1 x_SHA3_0 2)))
 (let (($x647 (= (stack_t x_0 x_1 w_1 x_SHA3_0 2 (bvadd (_ bv61 6) ?x2992)) (stack_t x_0 x_1 w_1 x_SHA3_0 1 (bvadd (_ bv63 6) ?x3379)))))
 (let (($x408 (exc_halt_t 1)))
 (let (($x11464 (= $x408 (or $x56 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x63))) (_ bv0 1)))))))
 (let (($x455 (forall ((w (_ BitVec 256)) )(let ((?x9232 (storage_t x_0 x_1 w_1 x_SHA3_0 0 w)))
 (let ((?x10158 (storage_t x_0 x_1 w_1 x_SHA3_0 1 w)))
 (= ?x10158 ?x9232))))
 ))
 (let (($x7338 (forall ((n (_ BitVec 6)) )(let ((?x63 (sc_t 0)))
 (let (($x5904 (bvsle ?x63 n)))
 (let ((?x10735 (stack_t x_0 x_1 w_1 x_SHA3_0 0 n)))
 (let ((?x5222 (stack_t x_0 x_1 w_1 x_SHA3_0 1 n)))
 (or (= ?x5222 ?x10735) $x5904))))))
 ))
 (let (($x8894 (= ?x3379 (bvadd (_ bv1 6) ?x63))))
 (let (($x467 (= $x276 (or (exc_halt_s 3) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 3))))))))
 (let (($x8037 (forall ((w (_ BitVec 256)) )(let ((?x11077 (storage_s x_0 x_1 w_1 x_SHA3_0 3 w)))
 (let ((?x4223 (storage_s x_0 x_1 w_1 x_SHA3_0 4 w)))
 (= ?x4223 ?x11077))))
 ))
 (let (($x1547 (forall ((n (_ BitVec 6)) )(let ((?x93 (stack_s x_0 x_1 w_1 x_SHA3_0 3 n)))
 (let ((?x4036 (stack_s x_0 x_1 w_1 x_SHA3_0 4 n)))
 (let ((?x4554 (sc_s 3)))
 (let ((?x7881 (bvadd (_ bv62 6) ?x4554)))
 (let (($x1229 (bvsle ?x7881 n)))
 (or $x1229 (= ?x4036 ?x93))))))))
 ))
 (let ((?x4554 (sc_s 3)))
 (let (($x353 (= ?x3145 ?x4554)))
 (let (($x5013 (= (used_gas_s x_0 x_1 w_1 x_SHA3_0 4) (+ 3 (used_gas_s x_0 x_1 w_1 x_SHA3_0 3)))))
 (let (($x2733 (= (stack_s x_0 x_1 w_1 x_SHA3_0 4 (bvadd (_ bv62 6) ?x3145)) (stack_s x_0 x_1 w_1 x_SHA3_0 3 (bvadd (_ bv63 6) ?x4554)))))
 (let (($x1837 (= (stack_s x_0 x_1 w_1 x_SHA3_0 4 (bvadd (_ bv63 6) ?x3145)) (stack_s x_0 x_1 w_1 x_SHA3_0 3 (bvadd (_ bv62 6) ?x4554)))))
 (let (($x9352 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 2)))) (_ bv0 1)))))
 (let (($x7873 (exc_halt_s 2)))
 (let (($x8777 (exc_halt_s 3)))
 (let (($x8915 (= $x8777 (or $x7873 $x9352))))
 (let (($x1941 (forall ((w (_ BitVec 256)) )(let ((?x6838 (storage_s x_0 x_1 w_1 x_SHA3_0 2 w)))
 (let ((?x11077 (storage_s x_0 x_1 w_1 x_SHA3_0 3 w)))
 (= ?x11077 ?x6838))))
 ))
 (let (($x2943 (forall ((n (_ BitVec 6)) )(let ((?x7378 (sc_s 2)))
 (let (($x5791 (bvsle ?x7378 n)))
 (let ((?x3257 (stack_s x_0 x_1 w_1 x_SHA3_0 2 n)))
 (let ((?x93 (stack_s x_0 x_1 w_1 x_SHA3_0 3 n)))
 (or (= ?x93 ?x3257) $x5791))))))
 ))
 (let (($x2780 (= ?x4554 (bvadd (_ bv1 6) (sc_s 2)))))
 (let ((?x11931 (used_gas_s x_0 x_1 w_1 x_SHA3_0 3)))
 (let (($x8295 (= $x7873 (or (exc_halt_s 1) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 1))))))))
 (let (($x347 (forall ((w (_ BitVec 256)) )(let ((?x9356 (storage_s x_0 x_1 w_1 x_SHA3_0 1 w)))
 (let ((?x6838 (storage_s x_0 x_1 w_1 x_SHA3_0 2 w)))
 (= ?x6838 ?x9356))))
 ))
 (let (($x870 (forall ((n (_ BitVec 6)) )(let ((?x154 (sc_s 1)))
 (let ((?x2778 (bvadd (_ bv62 6) ?x154)))
 (let (($x10951 (bvsle ?x2778 n)))
 (let ((?x5655 (stack_s x_0 x_1 w_1 x_SHA3_0 1 n)))
 (let ((?x3257 (stack_s x_0 x_1 w_1 x_SHA3_0 2 n)))
 (or (= ?x3257 ?x5655) $x10951)))))))
 ))
 (let ((?x310 (used_gas_s x_0 x_1 w_1 x_SHA3_0 2)))
 (let ((?x154 (sc_s 1)))
 (let ((?x2778 (bvadd (_ bv62 6) ?x154)))
 (let ((?x3842 (stack_s x_0 x_1 w_1 x_SHA3_0 1 ?x2778)))
 (let ((?x4689 (bvadd (_ bv63 6) ?x154)))
 (let ((?x9748 (stack_s x_0 x_1 w_1 x_SHA3_0 1 ?x4689)))
 (let (($x2553 (= (stack_s x_0 x_1 w_1 x_SHA3_0 2 (bvadd (_ bv63 6) (sc_s 2))) (f_SHA3 x_0 x_1 w_1 x_SHA3_0 ?x9748 ?x3842))))
 (let (($x2821 (forall ((w (_ BitVec 256)) )(let ((?x1615 (storage_s x_0 x_1 w_1 x_SHA3_0 0 w)))
 (let ((?x9356 (storage_s x_0 x_1 w_1 x_SHA3_0 1 w)))
 (= ?x9356 ?x1615))))
 ))
 (let (($x554 (forall ((n (_ BitVec 6)) )(let ((?x11090 (stack_s x_0 x_1 w_1 x_SHA3_0 0 n)))
 (let ((?x5655 (stack_s x_0 x_1 w_1 x_SHA3_0 1 n)))
 (or (bvsle (bvadd (_ bv62 6) (sc_s 0)) n) (= ?x5655 ?x11090)))))
 ))
 (let (($x11657 (= ?x154 ?x72)))
 (let (($x8447 (forall ((w0 (_ BitVec 256)) (w1 (_ BitVec 256)) )(let (($x2641 (= (stack_s x_0 x_1 w_1 x_SHA3_0 1 (bvadd (_ bv62 6) (sc_s 1))) w1)))
 (let (($x1610 (= (stack_s x_0 x_1 w_1 x_SHA3_0 1 (bvadd (_ bv63 6) (sc_s 1))) w0)))
 (let ((?x2077 (f_SHA3 x_0 x_1 w_1 x_SHA3_0 w0 w1)))
 (= ?x2077 (ite (and $x1610 $x2641) x_SHA3_0 (_ bv0 256)))))))
 ))
 (let (($x10566 (forall ((w (_ BitVec 256)) )(let ((?x1615 (storage_s x_0 x_1 w_1 x_SHA3_0 0 w)))
 (= ?x1615 (_ bv0 256))))
 ))
 (let (($x546 (= ?x1384 0)))
 (let (($x11010 (not $x57)))
 (let (($x10624 (= (stack_s x_0 x_1 w_1 x_SHA3_0 0 (_ bv1 6)) x_1)))
 (let (($x9620 (= (stack_s x_0 x_1 w_1 x_SHA3_0 0 (_ bv0 6)) x_0)))
 (let (($x11128 (= ?x72 (_ bv2 6))))
 (and $x11128 $x9620 $x10624 $x11010 $x546 $x10566 $x8447 (= ?x9748 (stack_s x_0 x_1 w_1 x_SHA3_0 0 (bvadd (_ bv62 6) ?x72))) (= ?x3842 (stack_s x_0 x_1 w_1 x_SHA3_0 0 (bvadd (_ bv63 6) ?x72))) (= (used_gas_s x_0 x_1 w_1 x_SHA3_0 1) (+ 3 ?x1384)) $x11657 $x554 $x2821 (= (exc_halt_s 1) (or $x57 (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) ?x72))))) $x2553 (= ?x310 (+ 30 (used_gas_s x_0 x_1 w_1 x_SHA3_0 1))) (= (sc_s 2) ?x4689) $x870 $x347 $x8295 (= (stack_s x_0 x_1 w_1 x_SHA3_0 3 (sc_s 2)) w_1) (= ?x11931 (+ 3 ?x310)) $x2780 $x2943 $x1941 $x8915 $x1837 $x2733 $x5013 $x353 $x1547 $x8037 $x467 (= (stack_t x_0 x_1 w_1 x_SHA3_0 1 ?x63) w_1) (= (used_gas_t x_0 x_1 w_1 x_SHA3_0 1) (+ 3 ?x4992)) $x8894 $x7338 $x455 $x11464 (= ?x6613 (stack_t x_0 x_1 w_1 x_SHA3_0 1 (bvadd (_ bv61 6) ?x3379))) $x647 (= ?x6049 (stack_t x_0 x_1 w_1 x_SHA3_0 1 (bvadd (_ bv62 6) ?x3379))) (= ?x5799 (+ 3 (used_gas_t x_0 x_1 w_1 x_SHA3_0 1))) $x10030 $x9243 $x3639 $x5684 $x2646 $x10352 (= ?x11964 ?x2620) $x5529 $x1955 $x46 $x73 $x10320 $x58 $x6534 $x2148 (not (and $x695 $x2890 $x11577 $x6409))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
 )
(check-sat)
(get-proof)