; SLOAD CALLVALUE SWAP1 PUSH cw_2 DUP1 SWAP2 => PUSH cw_2 PUSH cw_2 CALLVALUE SWAP3 SLOAD
(set-option :produce-proofs true)
; 
(set-info :status unknown)
(declare-fun storage_t ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 256)) (_ BitVec 256))
(declare-fun w_2 () (_ BitVec 256))
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
 (exists ((x_0 (_ BitVec 256)) (x_SLOAD_0 (_ BitVec 256)) (x_CALLVALUE (_ BitVec 256)) )(let (($x3038 (forall ((w (_ BitVec 256)) )(let ((?x5956 (storage_t x_0 x_SLOAD_0 w_2 x_CALLVALUE 5 w)))
 (let ((?x5563 (storage_s x_0 x_SLOAD_0 w_2 x_CALLVALUE 6 w)))
 (= ?x5563 ?x5956))))
 ))
 (let (($x886 (exc_halt_t 5)))
 (let (($x772 (exc_halt_s 6)))
 (let (($x889 (= $x772 $x886)))
 (let (($x5313 (forall ((n (_ BitVec 6)) )(let ((?x632 (stack_t x_0 x_SLOAD_0 w_2 x_CALLVALUE 5 n)))
 (let ((?x7877 (stack_s x_0 x_SLOAD_0 w_2 x_CALLVALUE 6 n)))
 (let (($x5811 (= ?x7877 ?x632)))
 (let ((?x919 (sc_t 5)))
 (let (($x644 (bvsle ?x919 n)))
 (or $x644 $x5811)))))))
 ))
 (let ((?x919 (sc_t 5)))
 (let ((?x926 (sc_s 6)))
 (let (($x929 (= ?x926 ?x919)))
 (let ((?x6783 (used_gas_t x_0 x_SLOAD_0 w_2 x_CALLVALUE 0)))
 (let ((?x5595 (used_gas_s x_0 x_SLOAD_0 w_2 x_CALLVALUE 0)))
 (let (($x3531 (= ?x5595 ?x6783)))
 (let (($x1292 (forall ((w (_ BitVec 256)) )(let ((?x3851 (storage_t x_0 x_SLOAD_0 w_2 x_CALLVALUE 0 w)))
 (let ((?x5232 (storage_s x_0 x_SLOAD_0 w_2 x_CALLVALUE 0 w)))
 (= ?x5232 ?x3851))))
 ))
 (let (($x56 (exc_halt_t 0)))
 (let (($x57 (exc_halt_s 0)))
 (let (($x58 (= $x57 $x56)))
 (let (($x8601 (forall ((n (_ BitVec 6)) )(let ((?x3836 (stack_t x_0 x_SLOAD_0 w_2 x_CALLVALUE 0 n)))
 (let ((?x7061 (stack_s x_0 x_SLOAD_0 w_2 x_CALLVALUE 0 n)))
 (let (($x7142 (= ?x7061 ?x3836)))
 (let ((?x63 (sc_t 0)))
 (let (($x2695 (bvsle ?x63 n)))
 (or $x2695 $x7142)))))))
 ))
 (let ((?x63 (sc_t 0)))
 (let ((?x72 (sc_s 0)))
 (let (($x73 (= ?x72 ?x63)))
 (let (($x458 (= $x886 (or (exc_halt_t 4) (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_t 4))))))))
 (let (($x2632 (forall ((w (_ BitVec 256)) )(let ((?x9721 (storage_t x_0 x_SLOAD_0 w_2 x_CALLVALUE 4 w)))
 (let ((?x5956 (storage_t x_0 x_SLOAD_0 w_2 x_CALLVALUE 5 w)))
 (= ?x5956 ?x9721))))
 ))
 (let (($x2974 (forall ((n (_ BitVec 6)) )(let ((?x3757 (sc_t 4)))
 (let ((?x3514 (bvadd (_ bv63 6) ?x3757)))
 (let (($x10333 (bvsle ?x3514 n)))
 (let ((?x869 (stack_t x_0 x_SLOAD_0 w_2 x_CALLVALUE 4 n)))
 (let ((?x632 (stack_t x_0 x_SLOAD_0 w_2 x_CALLVALUE 5 n)))
 (or (= ?x632 ?x869) $x10333)))))))
 ))
 (let ((?x3757 (sc_t 4)))
 (let (($x9261 (= ?x919 ?x3757)))
 (let (($x11016 (= (used_gas_t x_0 x_SLOAD_0 w_2 x_CALLVALUE 5) (+ 200 (used_gas_t x_0 x_SLOAD_0 w_2 x_CALLVALUE 4)))))
 (let ((?x3514 (bvadd (_ bv63 6) ?x3757)))
 (let ((?x1304 (stack_t x_0 x_SLOAD_0 w_2 x_CALLVALUE 4 ?x3514)))
 (let (($x11065 (= (stack_t x_0 x_SLOAD_0 w_2 x_CALLVALUE 5 (bvadd (_ bv63 6) ?x919)) (storage_t x_0 x_SLOAD_0 w_2 x_CALLVALUE 4 ?x1304))))
 (let (($x3723 (exc_halt_t 4)))
 (let (($x584 (= $x3723 (or (exc_halt_t 3) (not (bvsle (_ bv0 6) (bvadd (_ bv60 6) (sc_t 3))))))))
 (let (($x8817 (forall ((w (_ BitVec 256)) )(let ((?x5513 (storage_t x_0 x_SLOAD_0 w_2 x_CALLVALUE 3 w)))
 (let ((?x9721 (storage_t x_0 x_SLOAD_0 w_2 x_CALLVALUE 4 w)))
 (= ?x9721 ?x5513))))
 ))
 (let (($x6563 (forall ((n (_ BitVec 6)) )(let ((?x586 (stack_t x_0 x_SLOAD_0 w_2 x_CALLVALUE 3 n)))
 (let ((?x869 (stack_t x_0 x_SLOAD_0 w_2 x_CALLVALUE 4 n)))
 (or (= ?x869 ?x586) (bvsle (bvadd (_ bv60 6) (sc_t 3)) n)))))
 ))
 (let ((?x2012 (sc_t 3)))
 (let (($x7829 (= ?x3757 ?x2012)))
 (let ((?x690 (used_gas_t x_0 x_SLOAD_0 w_2 x_CALLVALUE 4)))
 (let (($x5478 (= (stack_t x_0 x_SLOAD_0 w_2 x_CALLVALUE 4 (bvadd (_ bv62 6) ?x3757)) (stack_t x_0 x_SLOAD_0 w_2 x_CALLVALUE 3 (bvadd (_ bv62 6) ?x2012)))))
 (let (($x10340 (= (stack_t x_0 x_SLOAD_0 w_2 x_CALLVALUE 4 (bvadd (_ bv61 6) ?x3757)) (stack_t x_0 x_SLOAD_0 w_2 x_CALLVALUE 3 (bvadd (_ bv61 6) ?x2012)))))
 (let (($x6488 (= (stack_t x_0 x_SLOAD_0 w_2 x_CALLVALUE 4 (bvadd (_ bv60 6) ?x3757)) (stack_t x_0 x_SLOAD_0 w_2 x_CALLVALUE 3 (bvadd (_ bv63 6) ?x2012)))))
 (let (($x1242 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_t 2)))) (_ bv0 1)))))
 (let (($x903 (exc_halt_t 2)))
 (let (($x10336 (exc_halt_t 3)))
 (let (($x1176 (= $x10336 (or $x903 $x1242))))
 (let (($x6266 (forall ((w (_ BitVec 256)) )(let ((?x4928 (storage_t x_0 x_SLOAD_0 w_2 x_CALLVALUE 2 w)))
 (let ((?x5513 (storage_t x_0 x_SLOAD_0 w_2 x_CALLVALUE 3 w)))
 (= ?x5513 ?x4928))))
 ))
 (let (($x10520 (forall ((n (_ BitVec 6)) )(let ((?x4056 (sc_t 2)))
 (let (($x7395 (bvsle ?x4056 n)))
 (let ((?x5382 (stack_t x_0 x_SLOAD_0 w_2 x_CALLVALUE 2 n)))
 (let ((?x586 (stack_t x_0 x_SLOAD_0 w_2 x_CALLVALUE 3 n)))
 (or (= ?x586 ?x5382) $x7395))))))
 ))
 (let (($x4906 (= ?x2012 (bvadd (_ bv1 6) (sc_t 2)))))
 (let ((?x7957 (used_gas_t x_0 x_SLOAD_0 w_2 x_CALLVALUE 3)))
 (let (($x1208 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_t 1)))) (_ bv0 1)))))
 (let (($x1920 (exc_halt_t 1)))
 (let (($x20 (= $x903 (or $x1920 $x1208))))
 (let (($x718 (forall ((w (_ BitVec 256)) )(let ((?x5326 (storage_t x_0 x_SLOAD_0 w_2 x_CALLVALUE 1 w)))
 (let ((?x4928 (storage_t x_0 x_SLOAD_0 w_2 x_CALLVALUE 2 w)))
 (= ?x4928 ?x5326))))
 ))
 (let (($x10105 (forall ((n (_ BitVec 6)) )(let ((?x4023 (sc_t 1)))
 (let (($x4317 (bvsle ?x4023 n)))
 (let ((?x6613 (stack_t x_0 x_SLOAD_0 w_2 x_CALLVALUE 1 n)))
 (let ((?x5382 (stack_t x_0 x_SLOAD_0 w_2 x_CALLVALUE 2 n)))
 (or (= ?x5382 ?x6613) $x4317))))))
 ))
 (let ((?x4056 (sc_t 2)))
 (let (($x1496 (= ?x4056 (bvadd (_ bv1 6) (sc_t 1)))))
 (let ((?x6661 (used_gas_t x_0 x_SLOAD_0 w_2 x_CALLVALUE 2)))
 (let (($x5833 (= $x1920 (or $x56 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x63))) (_ bv0 1)))))))
 (let (($x1401 (forall ((w (_ BitVec 256)) )(let ((?x3851 (storage_t x_0 x_SLOAD_0 w_2 x_CALLVALUE 0 w)))
 (let ((?x5326 (storage_t x_0 x_SLOAD_0 w_2 x_CALLVALUE 1 w)))
 (= ?x5326 ?x3851))))
 ))
 (let (($x912 (forall ((n (_ BitVec 6)) )(let ((?x63 (sc_t 0)))
 (let (($x2695 (bvsle ?x63 n)))
 (let ((?x3836 (stack_t x_0 x_SLOAD_0 w_2 x_CALLVALUE 0 n)))
 (let ((?x6613 (stack_t x_0 x_SLOAD_0 w_2 x_CALLVALUE 1 n)))
 (or (= ?x6613 ?x3836) $x2695))))))
 ))
 (let ((?x4023 (sc_t 1)))
 (let (($x10931 (= ?x4023 (bvadd (_ bv1 6) ?x63))))
 (let (($x9507 (= $x772 (or (exc_halt_s 5) (not (bvsle (_ bv0 6) (bvadd (_ bv61 6) (sc_s 5))))))))
 (let (($x6652 (forall ((w (_ BitVec 256)) )(let ((?x7175 (storage_s x_0 x_SLOAD_0 w_2 x_CALLVALUE 5 w)))
 (let ((?x5563 (storage_s x_0 x_SLOAD_0 w_2 x_CALLVALUE 6 w)))
 (= ?x5563 ?x7175))))
 ))
 (let (($x6578 (forall ((n (_ BitVec 6)) )(let ((?x805 (sc_s 5)))
 (let ((?x7666 (bvadd (_ bv61 6) ?x805)))
 (let (($x9949 (bvsle ?x7666 n)))
 (let ((?x7963 (stack_s x_0 x_SLOAD_0 w_2 x_CALLVALUE 5 n)))
 (let ((?x7877 (stack_s x_0 x_SLOAD_0 w_2 x_CALLVALUE 6 n)))
 (or (= ?x7877 ?x7963) $x9949)))))))
 ))
 (let ((?x805 (sc_s 5)))
 (let (($x8973 (= ?x926 ?x805)))
 (let (($x10214 (= (used_gas_s x_0 x_SLOAD_0 w_2 x_CALLVALUE 6) (+ 3 (used_gas_s x_0 x_SLOAD_0 w_2 x_CALLVALUE 5)))))
 (let (($x5290 (= (stack_s x_0 x_SLOAD_0 w_2 x_CALLVALUE 6 (bvadd (_ bv62 6) ?x926)) (stack_s x_0 x_SLOAD_0 w_2 x_CALLVALUE 5 (bvadd (_ bv62 6) ?x805)))))
 (let ((?x1911 (bvadd (_ bv63 6) ?x805)))
 (let ((?x6842 (stack_s x_0 x_SLOAD_0 w_2 x_CALLVALUE 5 ?x1911)))
 (let (($x6246 (= (stack_s x_0 x_SLOAD_0 w_2 x_CALLVALUE 6 (bvadd (_ bv63 6) ?x926)) (stack_s x_0 x_SLOAD_0 w_2 x_CALLVALUE 5 (bvadd (_ bv61 6) ?x805)))))
 (let (($x9256 (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_s 4))))))
 (let (($x3274 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 4)))) (_ bv0 1)))))
 (let (($x64 (exc_halt_s 4)))
 (let (($x3979 (exc_halt_s 5)))
 (let (($x3707 (forall ((w (_ BitVec 256)) )(let ((?x9005 (storage_s x_0 x_SLOAD_0 w_2 x_CALLVALUE 4 w)))
 (let ((?x7175 (storage_s x_0 x_SLOAD_0 w_2 x_CALLVALUE 5 w)))
 (= ?x7175 ?x9005))))
 ))
 (let (($x3925 (forall ((n (_ BitVec 6)) )(let ((?x363 (stack_s x_0 x_SLOAD_0 w_2 x_CALLVALUE 4 n)))
 (let ((?x7963 (stack_s x_0 x_SLOAD_0 w_2 x_CALLVALUE 5 n)))
 (let ((?x4305 (sc_s 4)))
 (let ((?x4412 (bvadd (_ bv63 6) ?x4305)))
 (let (($x6314 (bvsle ?x4412 n)))
 (or $x6314 (= ?x7963 ?x363))))))))
 ))
 (let (($x4735 (= ?x805 (bvadd (_ bv1 6) (sc_s 4)))))
 (let ((?x7232 (used_gas_s x_0 x_SLOAD_0 w_2 x_CALLVALUE 5)))
 (let ((?x4305 (sc_s 4)))
 (let ((?x4412 (bvadd (_ bv63 6) ?x4305)))
 (let ((?x9111 (stack_s x_0 x_SLOAD_0 w_2 x_CALLVALUE 4 ?x4412)))
 (let (($x4995 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 3)))) (_ bv0 1)))))
 (let (($x292 (exc_halt_s 3)))
 (let (($x9551 (= $x64 (or $x292 $x4995))))
 (let (($x6502 (forall ((w (_ BitVec 256)) )(let ((?x6004 (storage_s x_0 x_SLOAD_0 w_2 x_CALLVALUE 3 w)))
 (let ((?x9005 (storage_s x_0 x_SLOAD_0 w_2 x_CALLVALUE 4 w)))
 (= ?x9005 ?x6004))))
 ))
 (let (($x9367 (forall ((n (_ BitVec 6)) )(let ((?x3059 (stack_s x_0 x_SLOAD_0 w_2 x_CALLVALUE 3 n)))
 (let ((?x363 (stack_s x_0 x_SLOAD_0 w_2 x_CALLVALUE 4 n)))
 (let ((?x275 (sc_s 3)))
 (let (($x1115 (bvsle ?x275 n)))
 (or $x1115 (= ?x363 ?x3059)))))))
 ))
 (let (($x7686 (= ?x4305 (bvadd (_ bv1 6) (sc_s 3)))))
 (let ((?x9280 (used_gas_s x_0 x_SLOAD_0 w_2 x_CALLVALUE 4)))
 (let (($x2187 (= $x292 (or (exc_halt_s 2) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 2))))))))
 (let (($x174 (forall ((w (_ BitVec 256)) )(let ((?x445 (storage_s x_0 x_SLOAD_0 w_2 x_CALLVALUE 2 w)))
 (let ((?x6004 (storage_s x_0 x_SLOAD_0 w_2 x_CALLVALUE 3 w)))
 (= ?x6004 ?x445))))
 ))
 (let (($x10033 (forall ((n (_ BitVec 6)) )(let ((?x3004 (stack_s x_0 x_SLOAD_0 w_2 x_CALLVALUE 2 n)))
 (let ((?x3059 (stack_s x_0 x_SLOAD_0 w_2 x_CALLVALUE 3 n)))
 (let ((?x218 (sc_s 2)))
 (let ((?x1632 (bvadd (_ bv62 6) ?x218)))
 (let (($x1459 (bvsle ?x1632 n)))
 (or $x1459 (= ?x3059 ?x3004))))))))
 ))
 (let ((?x218 (sc_s 2)))
 (let ((?x275 (sc_s 3)))
 (let (($x10403 (= ?x275 ?x218)))
 (let ((?x3818 (used_gas_s x_0 x_SLOAD_0 w_2 x_CALLVALUE 3)))
 (let (($x6891 (= (stack_s x_0 x_SLOAD_0 w_2 x_CALLVALUE 3 (bvadd (_ bv62 6) ?x275)) (stack_s x_0 x_SLOAD_0 w_2 x_CALLVALUE 2 (bvadd (_ bv63 6) ?x218)))))
 (let (($x10375 (= (stack_s x_0 x_SLOAD_0 w_2 x_CALLVALUE 3 (bvadd (_ bv63 6) ?x275)) (stack_s x_0 x_SLOAD_0 w_2 x_CALLVALUE 2 (bvadd (_ bv62 6) ?x218)))))
 (let (($x10968 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 1)))) (_ bv0 1)))))
 (let (($x189 (exc_halt_s 1)))
 (let (($x247 (exc_halt_s 2)))
 (let (($x4335 (= $x247 (or $x189 $x10968))))
 (let (($x6761 (forall ((w (_ BitVec 256)) )(let ((?x9859 (storage_s x_0 x_SLOAD_0 w_2 x_CALLVALUE 1 w)))
 (let ((?x445 (storage_s x_0 x_SLOAD_0 w_2 x_CALLVALUE 2 w)))
 (= ?x445 ?x9859))))
 ))
 (let (($x190 (forall ((n (_ BitVec 6)) )(let ((?x154 (sc_s 1)))
 (let (($x10569 (bvsle ?x154 n)))
 (let ((?x7554 (stack_s x_0 x_SLOAD_0 w_2 x_CALLVALUE 1 n)))
 (let ((?x3004 (stack_s x_0 x_SLOAD_0 w_2 x_CALLVALUE 2 n)))
 (or (= ?x3004 ?x7554) $x10569))))))
 ))
 (let (($x6833 (= ?x218 (bvadd (_ bv1 6) (sc_s 1)))))
 (let ((?x5423 (used_gas_s x_0 x_SLOAD_0 w_2 x_CALLVALUE 2)))
 (let (($x7030 (forall ((w (_ BitVec 256)) )(let ((?x5232 (storage_s x_0 x_SLOAD_0 w_2 x_CALLVALUE 0 w)))
 (let ((?x9859 (storage_s x_0 x_SLOAD_0 w_2 x_CALLVALUE 1 w)))
 (= ?x9859 ?x5232))))
 ))
 (let (($x1614 (forall ((n (_ BitVec 6)) )(let ((?x7061 (stack_s x_0 x_SLOAD_0 w_2 x_CALLVALUE 0 n)))
 (let ((?x7554 (stack_s x_0 x_SLOAD_0 w_2 x_CALLVALUE 1 n)))
 (let ((?x72 (sc_s 0)))
 (let ((?x1510 (bvadd (_ bv63 6) ?x72)))
 (let (($x5652 (bvsle ?x1510 n)))
 (or $x5652 (= ?x7554 ?x7061))))))))
 ))
 (let ((?x154 (sc_s 1)))
 (let (($x1336 (= ?x154 ?x72)))
 (let ((?x1346 (storage_s x_0 x_SLOAD_0 w_2 x_CALLVALUE 0 (stack_s x_0 x_SLOAD_0 w_2 x_CALLVALUE 0 (bvadd (_ bv63 6) ?x72)))))
 (let (($x6224 (forall ((w (_ BitVec 256)) )(let (($x6149 (= w (stack_s x_0 x_SLOAD_0 w_2 x_CALLVALUE 0 (bvadd (_ bv63 6) (sc_s 0))))))
 (let ((?x5232 (storage_s x_0 x_SLOAD_0 w_2 x_CALLVALUE 0 w)))
 (= ?x5232 (ite $x6149 x_SLOAD_0 (_ bv0 256))))))
 ))
 (let (($x4379 (= ?x5595 0)))
 (let (($x4490 (not $x57)))
 (let (($x6471 (= (stack_s x_0 x_SLOAD_0 w_2 x_CALLVALUE 0 (_ bv0 6)) x_0)))
 (let (($x8685 (= ?x72 (_ bv1 6))))
 (and $x8685 $x6471 $x4490 $x4379 $x6224 (= (stack_s x_0 x_SLOAD_0 w_2 x_CALLVALUE 1 (bvadd (_ bv63 6) ?x154)) ?x1346) (= (used_gas_s x_0 x_SLOAD_0 w_2 x_CALLVALUE 1) (+ 200 ?x5595)) $x1336 $x1614 $x7030 (= $x189 (or $x57 (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) ?x72))))) (= (stack_s x_0 x_SLOAD_0 w_2 x_CALLVALUE 2 ?x154) x_CALLVALUE) (= ?x5423 (+ 2 (used_gas_s x_0 x_SLOAD_0 w_2 x_CALLVALUE 1))) $x6833 $x190 $x6761 $x4335 $x10375 $x6891 (= ?x3818 (+ 3 ?x5423)) $x10403 $x10033 $x174 $x2187 (= (stack_s x_0 x_SLOAD_0 w_2 x_CALLVALUE 4 ?x275) w_2) (= ?x9280 (+ 3 ?x3818)) $x7686 $x9367 $x6502 $x9551 (= ?x6842 ?x9111) (= (stack_s x_0 x_SLOAD_0 w_2 x_CALLVALUE 5 ?x4412) ?x9111) (= ?x7232 (+ 3 ?x9280)) $x4735 $x3925 $x3707 (= $x3979 (or $x64 $x3274 $x9256)) $x6246 (= (stack_s x_0 x_SLOAD_0 w_2 x_CALLVALUE 6 (bvadd (_ bv61 6) ?x926)) ?x6842) $x5290 $x10214 $x8973 $x6578 $x6652 $x9507 (= (stack_t x_0 x_SLOAD_0 w_2 x_CALLVALUE 1 ?x63) w_2) (= (used_gas_t x_0 x_SLOAD_0 w_2 x_CALLVALUE 1) (+ 3 ?x6783)) $x10931 $x912 $x1401 $x5833 (= (stack_t x_0 x_SLOAD_0 w_2 x_CALLVALUE 2 ?x4023) w_2) (= ?x6661 (+ 3 (used_gas_t x_0 x_SLOAD_0 w_2 x_CALLVALUE 1))) $x1496 $x10105 $x718 $x20 (= (stack_t x_0 x_SLOAD_0 w_2 x_CALLVALUE 3 ?x4056) x_CALLVALUE) (= ?x7957 (+ 2 ?x6661)) $x4906 $x10520 $x6266 $x1176 (= ?x1304 (stack_t x_0 x_SLOAD_0 w_2 x_CALLVALUE 3 (bvadd (_ bv60 6) ?x2012))) $x6488 $x10340 $x5478 (= ?x690 (+ 3 ?x7957)) $x7829 $x6563 $x8817 $x584 $x11065 $x11016 $x9261 $x2974 $x2632 $x458 $x73 $x8601 $x58 $x1292 $x3531 (not (and $x929 $x5313 $x889 $x3038))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
 )
(check-sat)
(get-proof)
