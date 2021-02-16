; CALLER DUP1 EXTCODESIZE SWAP2 POP => CALLER EXTCODESIZE SWAP1 POP CALLER
(set-option :produce-proofs true)
; 
(set-info :status unknown)
(declare-fun storage_t ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 256)) (_ BitVec 256))
(declare-fun storage_s ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 256)) (_ BitVec 256))
(declare-fun exc_halt_t (Int) Bool)
(declare-fun exc_halt_s (Int) Bool)
(declare-fun stack_t ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 6)) (_ BitVec 256))
(declare-fun stack_s ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 6)) (_ BitVec 256))
(declare-fun sc_t (Int) (_ BitVec 6))
(declare-fun sc_s (Int) (_ BitVec 6))
(declare-fun used_gas_t ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int) Int)
(declare-fun used_gas_s ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int) Int)
(declare-fun f_EXTCODESIZE ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256)) (_ BitVec 256))
(assert
 (exists ((x_0 (_ BitVec 256)) (x_CALLER (_ BitVec 256)) (x_EXTCODESIZE_0 (_ BitVec 256)) )(let (($x10114 (forall ((w (_ BitVec 256)) )(let ((?x5290 (storage_t x_0 x_CALLER x_EXTCODESIZE_0 5 w)))
 (let ((?x7503 (storage_s x_0 x_CALLER x_EXTCODESIZE_0 5 w)))
 (= ?x7503 ?x5290))))
 ))
 (let (($x886 (exc_halt_t 5)))
 (let (($x3979 (exc_halt_s 5)))
 (let (($x2261 (= $x3979 $x886)))
 (let (($x7335 (forall ((n (_ BitVec 6)) )(let ((?x5959 (stack_t x_0 x_CALLER x_EXTCODESIZE_0 5 n)))
 (let ((?x7803 (stack_s x_0 x_CALLER x_EXTCODESIZE_0 5 n)))
 (let (($x6627 (= ?x7803 ?x5959)))
 (let ((?x919 (sc_t 5)))
 (let (($x2999 (bvsle ?x919 n)))
 (or $x2999 $x6627)))))))
 ))
 (let ((?x919 (sc_t 5)))
 (let ((?x805 (sc_s 5)))
 (let (($x10040 (= ?x805 ?x919)))
 (let ((?x7354 (used_gas_t x_0 x_CALLER x_EXTCODESIZE_0 0)))
 (let ((?x3856 (used_gas_s x_0 x_CALLER x_EXTCODESIZE_0 0)))
 (let (($x2813 (= ?x3856 ?x7354)))
 (let (($x4339 (forall ((w (_ BitVec 256)) )(let ((?x8923 (storage_t x_0 x_CALLER x_EXTCODESIZE_0 0 w)))
 (let ((?x8073 (storage_s x_0 x_CALLER x_EXTCODESIZE_0 0 w)))
 (= ?x8073 ?x8923))))
 ))
 (let (($x56 (exc_halt_t 0)))
 (let (($x57 (exc_halt_s 0)))
 (let (($x58 (= $x57 $x56)))
 (let (($x8083 (forall ((n (_ BitVec 6)) )(let ((?x3831 (stack_t x_0 x_CALLER x_EXTCODESIZE_0 0 n)))
 (let ((?x1587 (stack_s x_0 x_CALLER x_EXTCODESIZE_0 0 n)))
 (let (($x10721 (= ?x1587 ?x3831)))
 (let ((?x63 (sc_t 0)))
 (let (($x4577 (bvsle ?x63 n)))
 (or $x4577 $x10721)))))))
 ))
 (let ((?x63 (sc_t 0)))
 (let ((?x72 (sc_s 0)))
 (let (($x73 (= ?x72 ?x63)))
 (let (($x3723 (exc_halt_t 4)))
 (let (($x11583 (or $x3723 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_t 4)))) (_ bv0 1))))))
 (let (($x3988 (forall ((w (_ BitVec 256)) )(let ((?x912 (storage_t x_0 x_CALLER x_EXTCODESIZE_0 4 w)))
 (let ((?x5290 (storage_t x_0 x_CALLER x_EXTCODESIZE_0 5 w)))
 (= ?x5290 ?x912))))
 ))
 (let (($x4249 (forall ((n (_ BitVec 6)) )(let ((?x1401 (stack_t x_0 x_CALLER x_EXTCODESIZE_0 4 n)))
 (let ((?x5959 (stack_t x_0 x_CALLER x_EXTCODESIZE_0 5 n)))
 (let ((?x3757 (sc_t 4)))
 (let (($x1649 (bvsle ?x3757 n)))
 (or $x1649 (= ?x5959 ?x1401)))))))
 ))
 (let (($x6772 (= (used_gas_t x_0 x_CALLER x_EXTCODESIZE_0 5) (+ 2 (used_gas_t x_0 x_CALLER x_EXTCODESIZE_0 4)))))
 (let (($x4488 (= $x3723 (or (exc_halt_t 3) (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_t 3))))))))
 (let (($x4121 (forall ((w (_ BitVec 256)) )(let ((?x586 (storage_t x_0 x_CALLER x_EXTCODESIZE_0 3 w)))
 (let ((?x912 (storage_t x_0 x_CALLER x_EXTCODESIZE_0 4 w)))
 (= ?x912 ?x586))))
 ))
 (let (($x11587 (forall ((n (_ BitVec 6)) )(let ((?x2012 (sc_t 3)))
 (let ((?x6604 (bvadd (_ bv63 6) ?x2012)))
 (let (($x4481 (bvsle ?x6604 n)))
 (let ((?x5331 (stack_t x_0 x_CALLER x_EXTCODESIZE_0 3 n)))
 (let ((?x1401 (stack_t x_0 x_CALLER x_EXTCODESIZE_0 4 n)))
 (or (= ?x1401 ?x5331) $x4481)))))))
 ))
 (let ((?x2012 (sc_t 3)))
 (let ((?x6604 (bvadd (_ bv63 6) ?x2012)))
 (let ((?x3757 (sc_t 4)))
 (let (($x4952 (= ?x3757 ?x6604)))
 (let ((?x6333 (used_gas_t x_0 x_CALLER x_EXTCODESIZE_0 4)))
 (let (($x10336 (exc_halt_t 3)))
 (let (($x240 (= $x10336 (or (exc_halt_t 2) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_t 2))))))))
 (let (($x4130 (forall ((w (_ BitVec 256)) )(let ((?x632 (storage_t x_0 x_CALLER x_EXTCODESIZE_0 2 w)))
 (let ((?x586 (storage_t x_0 x_CALLER x_EXTCODESIZE_0 3 w)))
 (= ?x586 ?x632))))
 ))
 (let (($x11557 (forall ((n (_ BitVec 6)) )(let ((?x4056 (sc_t 2)))
 (let ((?x9041 (bvadd (_ bv62 6) ?x4056)))
 (let (($x4688 (bvsle ?x9041 n)))
 (let ((?x9721 (stack_t x_0 x_CALLER x_EXTCODESIZE_0 2 n)))
 (let ((?x5331 (stack_t x_0 x_CALLER x_EXTCODESIZE_0 3 n)))
 (or (= ?x5331 ?x9721) $x4688)))))))
 ))
 (let ((?x4056 (sc_t 2)))
 (let (($x2424 (= ?x2012 ?x4056)))
 (let ((?x9057 (used_gas_t x_0 x_CALLER x_EXTCODESIZE_0 3)))
 (let ((?x1385 (bvadd (_ bv63 6) ?x4056)))
 (let ((?x459 (stack_t x_0 x_CALLER x_EXTCODESIZE_0 2 ?x1385)))
 (let (($x11527 (= (stack_t x_0 x_CALLER x_EXTCODESIZE_0 3 ?x6604) (stack_t x_0 x_CALLER x_EXTCODESIZE_0 2 (bvadd (_ bv62 6) ?x4056)))))
 (let (($x903 (exc_halt_t 2)))
 (let (($x11524 (= $x903 (or (exc_halt_t 1) (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_t 1))))))))
 (let (($x8426 (forall ((w (_ BitVec 256)) )(let ((?x2301 (storage_t x_0 x_CALLER x_EXTCODESIZE_0 1 w)))
 (let ((?x632 (storage_t x_0 x_CALLER x_EXTCODESIZE_0 2 w)))
 (= ?x632 ?x2301))))
 ))
 (let (($x11570 (forall ((n (_ BitVec 6)) )(let ((?x4023 (sc_t 1)))
 (let ((?x5283 (bvadd (_ bv63 6) ?x4023)))
 (let (($x9110 (bvsle ?x5283 n)))
 (let ((?x380 (stack_t x_0 x_CALLER x_EXTCODESIZE_0 1 n)))
 (let ((?x9721 (stack_t x_0 x_CALLER x_EXTCODESIZE_0 2 n)))
 (or (= ?x9721 ?x380) $x9110)))))))
 ))
 (let ((?x4023 (sc_t 1)))
 (let (($x2682 (= ?x4056 ?x4023)))
 (let ((?x6453 (used_gas_t x_0 x_CALLER x_EXTCODESIZE_0 2)))
 (let ((?x11265 (f_EXTCODESIZE x_0 x_CALLER x_EXTCODESIZE_0 (stack_t x_0 x_CALLER x_EXTCODESIZE_0 1 (bvadd (_ bv63 6) ?x4023)))))
 (let (($x1920 (exc_halt_t 1)))
 (let (($x2260 (= $x1920 (or $x56 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x63))) (_ bv0 1)))))))
 (let (($x8283 (forall ((w (_ BitVec 256)) )(let ((?x8923 (storage_t x_0 x_CALLER x_EXTCODESIZE_0 0 w)))
 (let ((?x2301 (storage_t x_0 x_CALLER x_EXTCODESIZE_0 1 w)))
 (= ?x2301 ?x8923))))
 ))
 (let (($x3258 (forall ((n (_ BitVec 6)) )(let ((?x3831 (stack_t x_0 x_CALLER x_EXTCODESIZE_0 0 n)))
 (let ((?x380 (stack_t x_0 x_CALLER x_EXTCODESIZE_0 1 n)))
 (let ((?x63 (sc_t 0)))
 (let (($x4577 (bvsle ?x63 n)))
 (or $x4577 (= ?x380 ?x3831)))))))
 ))
 (let (($x245 (= ?x4023 (bvadd (_ bv1 6) ?x63))))
 (let (($x1810 (= $x3979 (or (exc_halt_s 4) (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_s 4))))))))
 (let (($x3362 (forall ((w (_ BitVec 256)) )(let ((?x9465 (storage_s x_0 x_CALLER x_EXTCODESIZE_0 4 w)))
 (let ((?x7503 (storage_s x_0 x_CALLER x_EXTCODESIZE_0 5 w)))
 (= ?x7503 ?x9465))))
 ))
 (let (($x8728 (forall ((n (_ BitVec 6)) )(let ((?x4305 (sc_s 4)))
 (let ((?x1489 (bvadd (_ bv63 6) ?x4305)))
 (let (($x9956 (bvsle ?x1489 n)))
 (let ((?x5392 (stack_s x_0 x_CALLER x_EXTCODESIZE_0 4 n)))
 (let ((?x7803 (stack_s x_0 x_CALLER x_EXTCODESIZE_0 5 n)))
 (or (= ?x7803 ?x5392) $x9956)))))))
 ))
 (let ((?x4305 (sc_s 4)))
 (let ((?x1489 (bvadd (_ bv63 6) ?x4305)))
 (let (($x6499 (= ?x805 ?x1489)))
 (let (($x2256 (= (used_gas_s x_0 x_CALLER x_EXTCODESIZE_0 5) (+ 2 (used_gas_s x_0 x_CALLER x_EXTCODESIZE_0 4)))))
 (let (($x64 (exc_halt_s 4)))
 (let (($x7345 (= $x64 (or (exc_halt_s 3) (not (bvsle (_ bv0 6) (bvadd (_ bv61 6) (sc_s 3))))))))
 (let (($x9214 (forall ((w (_ BitVec 256)) )(let ((?x6580 (storage_s x_0 x_CALLER x_EXTCODESIZE_0 3 w)))
 (let ((?x9465 (storage_s x_0 x_CALLER x_EXTCODESIZE_0 4 w)))
 (= ?x9465 ?x6580))))
 ))
 (let (($x10575 (forall ((n (_ BitVec 6)) )(let ((?x8177 (stack_s x_0 x_CALLER x_EXTCODESIZE_0 3 n)))
 (let ((?x5392 (stack_s x_0 x_CALLER x_EXTCODESIZE_0 4 n)))
 (let (($x4408 (= ?x5392 ?x8177)))
 (let ((?x275 (sc_s 3)))
 (let ((?x10438 (bvadd (_ bv61 6) ?x275)))
 (let (($x1703 (bvsle ?x10438 n)))
 (or $x1703 $x4408))))))))
 ))
 (let ((?x275 (sc_s 3)))
 (let (($x180 (= ?x4305 ?x275)))
 (let ((?x8527 (used_gas_s x_0 x_CALLER x_EXTCODESIZE_0 4)))
 (let (($x11072 (= (stack_s x_0 x_CALLER x_EXTCODESIZE_0 4 (bvadd (_ bv62 6) ?x4305)) (stack_s x_0 x_CALLER x_EXTCODESIZE_0 3 (bvadd (_ bv62 6) ?x275)))))
 (let ((?x403 (bvadd (_ bv63 6) ?x275)))
 (let ((?x10440 (stack_s x_0 x_CALLER x_EXTCODESIZE_0 3 ?x403)))
 (let (($x11478 (= (stack_s x_0 x_CALLER x_EXTCODESIZE_0 4 ?x1489) (stack_s x_0 x_CALLER x_EXTCODESIZE_0 3 (bvadd (_ bv61 6) ?x275)))))
 (let (($x292 (exc_halt_s 3)))
 (let (($x10677 (= $x292 (or (exc_halt_s 2) (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_s 2))))))))
 (let (($x3040 (forall ((w (_ BitVec 256)) )(let ((?x7552 (storage_s x_0 x_CALLER x_EXTCODESIZE_0 2 w)))
 (let ((?x6580 (storage_s x_0 x_CALLER x_EXTCODESIZE_0 3 w)))
 (= ?x6580 ?x7552))))
 ))
 (let (($x185 (forall ((n (_ BitVec 6)) )(let ((?x9915 (stack_s x_0 x_CALLER x_EXTCODESIZE_0 2 n)))
 (let ((?x8177 (stack_s x_0 x_CALLER x_EXTCODESIZE_0 3 n)))
 (or (bvsle (bvadd (_ bv63 6) (sc_s 2)) n) (= ?x8177 ?x9915)))))
 ))
 (let ((?x218 (sc_s 2)))
 (let (($x1374 (= ?x275 ?x218)))
 (let ((?x6324 (used_gas_s x_0 x_CALLER x_EXTCODESIZE_0 3)))
 (let ((?x712 (bvadd (_ bv63 6) ?x218)))
 (let ((?x9234 (stack_s x_0 x_CALLER x_EXTCODESIZE_0 2 ?x712)))
 (let (($x6305 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 1)))) (_ bv0 1)))))
 (let (($x763 (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_s 1))))))
 (let (($x189 (exc_halt_s 1)))
 (let (($x247 (exc_halt_s 2)))
 (let (($x7821 (= $x247 (or $x189 $x763 $x6305))))
 (let (($x2347 (forall ((w (_ BitVec 256)) )(let ((?x10537 (storage_s x_0 x_CALLER x_EXTCODESIZE_0 1 w)))
 (let ((?x7552 (storage_s x_0 x_CALLER x_EXTCODESIZE_0 2 w)))
 (= ?x7552 ?x10537))))
 ))
 (let (($x11367 (forall ((n (_ BitVec 6)) )(let ((?x8697 (stack_s x_0 x_CALLER x_EXTCODESIZE_0 1 n)))
 (let ((?x9915 (stack_s x_0 x_CALLER x_EXTCODESIZE_0 2 n)))
 (let (($x7457 (= ?x9915 ?x8697)))
 (let ((?x154 (sc_s 1)))
 (let ((?x1435 (bvadd (_ bv63 6) ?x154)))
 (let (($x3626 (bvsle ?x1435 n)))
 (or $x3626 $x7457))))))))
 ))
 (let (($x2153 (= ?x218 (bvadd (_ bv1 6) (sc_s 1)))))
 (let ((?x3491 (used_gas_s x_0 x_CALLER x_EXTCODESIZE_0 2)))
 (let (($x3590 (= ?x3491 (+ 3 (used_gas_s x_0 x_CALLER x_EXTCODESIZE_0 1)))))
 (let ((?x154 (sc_s 1)))
 (let ((?x1435 (bvadd (_ bv63 6) ?x154)))
 (let ((?x1581 (stack_s x_0 x_CALLER x_EXTCODESIZE_0 1 ?x1435)))
 (let (($x5867 (= (stack_s x_0 x_CALLER x_EXTCODESIZE_0 2 ?x1435) ?x1581)))
 (let (($x4891 (= $x189 (or $x57 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x72))) (_ bv0 1)))))))
 (let (($x896 (forall ((w (_ BitVec 256)) )(let ((?x8073 (storage_s x_0 x_CALLER x_EXTCODESIZE_0 0 w)))
 (let ((?x10537 (storage_s x_0 x_CALLER x_EXTCODESIZE_0 1 w)))
 (= ?x10537 ?x8073))))
 ))
 (let (($x11456 (forall ((n (_ BitVec 6)) )(let ((?x72 (sc_s 0)))
 (let (($x7832 (bvsle ?x72 n)))
 (let ((?x1587 (stack_s x_0 x_CALLER x_EXTCODESIZE_0 0 n)))
 (let ((?x8697 (stack_s x_0 x_CALLER x_EXTCODESIZE_0 1 n)))
 (let (($x2691 (= ?x8697 ?x1587)))
 (or $x2691 $x7832)))))))
 ))
 (let (($x3745 (= ?x154 (bvadd (_ bv1 6) ?x72))))
 (let (($x7828 (forall ((w0 (_ BitVec 256)) )(let ((?x11385 (ite (= (stack_s x_0 x_CALLER x_EXTCODESIZE_0 2 (bvadd (_ bv63 6) (sc_s 2))) w0) x_EXTCODESIZE_0 (_ bv0 256))))
 (let ((?x2484 (f_EXTCODESIZE x_0 x_CALLER x_EXTCODESIZE_0 w0)))
 (= ?x2484 ?x11385))))
 ))
 (let (($x129 (forall ((w (_ BitVec 256)) )(let ((?x8073 (storage_s x_0 x_CALLER x_EXTCODESIZE_0 0 w)))
 (= ?x8073 (_ bv0 256))))
 ))
 (let (($x6542 (= ?x3856 0)))
 (let (($x5252 (not $x57)))
 (let (($x867 (= (stack_s x_0 x_CALLER x_EXTCODESIZE_0 0 (_ bv0 6)) x_0)))
 (let (($x8685 (= ?x72 (_ bv1 6))))
 (and $x8685 $x867 $x5252 $x6542 $x129 $x7828 (= (stack_s x_0 x_CALLER x_EXTCODESIZE_0 1 ?x72) x_CALLER) (= (used_gas_s x_0 x_CALLER x_EXTCODESIZE_0 1) (+ 2 ?x3856)) $x3745 $x11456 $x896 $x4891 (= ?x9234 ?x1581) $x5867 $x3590 $x2153 $x11367 $x2347 $x7821 (= ?x10440 (f_EXTCODESIZE x_0 x_CALLER x_EXTCODESIZE_0 ?x9234)) (= ?x6324 (+ 700 ?x3491)) $x1374 $x185 $x3040 $x10677 $x11478 (= (stack_s x_0 x_CALLER x_EXTCODESIZE_0 4 (bvadd (_ bv61 6) ?x4305)) ?x10440) $x11072 (= ?x8527 (+ 3 ?x6324)) $x180 $x10575 $x9214 $x7345 $x2256 $x6499 $x8728 $x3362 $x1810 (= (stack_t x_0 x_CALLER x_EXTCODESIZE_0 1 ?x63) x_CALLER) (= (used_gas_t x_0 x_CALLER x_EXTCODESIZE_0 1) (+ 2 ?x7354)) $x245 $x3258 $x8283 $x2260 (= ?x459 ?x11265) (= ?x6453 (+ 700 (used_gas_t x_0 x_CALLER x_EXTCODESIZE_0 1))) $x2682 $x11570 $x8426 $x11524 $x11527 (= (stack_t x_0 x_CALLER x_EXTCODESIZE_0 3 (bvadd (_ bv62 6) ?x2012)) ?x459) (= ?x9057 (+ 3 ?x6453)) $x2424 $x11557 $x4130 $x240 (= ?x6333 (+ 2 ?x9057)) $x4952 $x11587 $x4121 $x4488 (= (stack_t x_0 x_CALLER x_EXTCODESIZE_0 5 ?x3757) x_CALLER) $x6772 (= ?x919 (bvadd (_ bv1 6) ?x3757)) $x4249 $x3988 (= $x886 $x11583) $x73 $x8083 $x58 $x4339 $x2813 (not (and $x10040 $x7335 $x2261 $x10114))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
 )
(check-sat)
(get-proof)