; DUP1 CALLDATALOAD NOT NOT SWAP1 POP => CALLDATALOAD
(set-option :produce-proofs true)
; 
(set-info :status unknown)
(declare-fun storage_t ((_ BitVec 256) (_ BitVec 256) Int (_ BitVec 256)) (_ BitVec 256))
(declare-fun storage_s ((_ BitVec 256) (_ BitVec 256) Int (_ BitVec 256)) (_ BitVec 256))
(declare-fun exc_halt_t (Int) Bool)
(declare-fun exc_halt_s (Int) Bool)
(declare-fun sc_t (Int) (_ BitVec 6))
(declare-fun stack_t ((_ BitVec 256) (_ BitVec 256) Int (_ BitVec 6)) (_ BitVec 256))
(declare-fun stack_s ((_ BitVec 256) (_ BitVec 256) Int (_ BitVec 6)) (_ BitVec 256))
(declare-fun sc_s (Int) (_ BitVec 6))
(declare-fun used_gas_t ((_ BitVec 256) (_ BitVec 256) Int) Int)
(declare-fun used_gas_s ((_ BitVec 256) (_ BitVec 256) Int) Int)
(declare-fun f_CALLDATALOAD ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256)) (_ BitVec 256))
(assert
 (exists ((x_0 (_ BitVec 256)) (x_CALLDATALOAD_0 (_ BitVec 256)) )(let (($x1599 (forall ((w (_ BitVec 256)) )(let ((?x1659 (storage_t x_0 x_CALLDATALOAD_0 1 w)))
 (let ((?x2272 (storage_s x_0 x_CALLDATALOAD_0 6 w)))
 (= ?x2272 ?x1659))))
 ))
 (let (($x1920 (exc_halt_t 1)))
 (let (($x772 (exc_halt_s 6)))
 (let (($x3517 (= $x772 $x1920)))
 (let (($x1809 (forall ((n (_ BitVec 6)) )(let ((?x4023 (sc_t 1)))
 (let (($x4928 (bvsle ?x4023 n)))
 (let ((?x2886 (stack_t x_0 x_CALLDATALOAD_0 1 n)))
 (let ((?x2090 (stack_s x_0 x_CALLDATALOAD_0 6 n)))
 (let (($x2168 (= ?x2090 ?x2886)))
 (or $x2168 $x4928)))))))
 ))
 (let ((?x4023 (sc_t 1)))
 (let ((?x926 (sc_s 6)))
 (let (($x6116 (= ?x926 ?x4023)))
 (let ((?x5778 (used_gas_t x_0 x_CALLDATALOAD_0 0)))
 (let ((?x960 (used_gas_s x_0 x_CALLDATALOAD_0 0)))
 (let (($x1516 (= ?x960 ?x5778)))
 (let (($x1400 (forall ((w (_ BitVec 256)) )(let ((?x1740 (storage_t x_0 x_CALLDATALOAD_0 0 w)))
 (let ((?x1412 (storage_s x_0 x_CALLDATALOAD_0 0 w)))
 (= ?x1412 ?x1740))))
 ))
 (let (($x56 (exc_halt_t 0)))
 (let (($x57 (exc_halt_s 0)))
 (let (($x58 (= $x57 $x56)))
 (let (($x1573 (forall ((n (_ BitVec 6)) )(let ((?x5438 (stack_t x_0 x_CALLDATALOAD_0 0 n)))
 (let ((?x5914 (stack_s x_0 x_CALLDATALOAD_0 0 n)))
 (let (($x5748 (= ?x5914 ?x5438)))
 (let ((?x63 (sc_t 0)))
 (let (($x2788 (bvsle ?x63 n)))
 (or $x2788 $x5748)))))))
 ))
 (let ((?x63 (sc_t 0)))
 (let ((?x72 (sc_s 0)))
 (let (($x73 (= ?x72 ?x63)))
 (let (($x1483 (forall ((w (_ BitVec 256)) )(let ((?x1740 (storage_t x_0 x_CALLDATALOAD_0 0 w)))
 (let ((?x1659 (storage_t x_0 x_CALLDATALOAD_0 1 w)))
 (= ?x1659 ?x1740))))
 ))
 (let (($x1779 (forall ((n (_ BitVec 6)) )(or (bvsle (bvadd (_ bv63 6) (sc_t 0)) n) (= (stack_t x_0 x_CALLDATALOAD_0 1 n) (stack_t x_0 x_CALLDATALOAD_0 0 n))))
 ))
 (let (($x1094 (= (stack_t x_0 x_CALLDATALOAD_0 1 (bvadd (_ bv63 6) ?x4023)) (f_CALLDATALOAD x_0 x_CALLDATALOAD_0 (stack_t x_0 x_CALLDATALOAD_0 0 (bvadd (_ bv63 6) ?x63))))))
 (let (($x9067 (= $x772 (or (exc_halt_s 5) (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_s 5))))))))
 (let (($x1419 (forall ((w (_ BitVec 256)) )(let ((?x7415 (storage_s x_0 x_CALLDATALOAD_0 5 w)))
 (let ((?x2272 (storage_s x_0 x_CALLDATALOAD_0 6 w)))
 (= ?x2272 ?x7415))))
 ))
 (let (($x959 (forall ((n (_ BitVec 6)) )(let ((?x805 (sc_s 5)))
 (let ((?x2394 (bvadd (_ bv63 6) ?x805)))
 (let (($x8867 (bvsle ?x2394 n)))
 (or $x8867 (= (stack_s x_0 x_CALLDATALOAD_0 6 n) (stack_s x_0 x_CALLDATALOAD_0 5 n)))))))
 ))
 (let ((?x805 (sc_s 5)))
 (let ((?x2394 (bvadd (_ bv63 6) ?x805)))
 (let (($x2577 (= ?x926 ?x2394)))
 (let (($x1633 (= (used_gas_s x_0 x_CALLDATALOAD_0 6) (+ 2 (used_gas_s x_0 x_CALLDATALOAD_0 5)))))
 (let (($x3979 (exc_halt_s 5)))
 (let (($x6033 (= $x3979 (or (exc_halt_s 4) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 4))))))))
 (let (($x1275 (forall ((w (_ BitVec 256)) )(let ((?x2029 (storage_s x_0 x_CALLDATALOAD_0 4 w)))
 (let ((?x7415 (storage_s x_0 x_CALLDATALOAD_0 5 w)))
 (= ?x7415 ?x2029))))
 ))
 (let (($x5790 (forall ((n (_ BitVec 6)) )(let ((?x4305 (sc_s 4)))
 (let ((?x9282 (bvadd (_ bv62 6) ?x4305)))
 (let (($x1651 (bvsle ?x9282 n)))
 (or $x1651 (= (stack_s x_0 x_CALLDATALOAD_0 5 n) (stack_s x_0 x_CALLDATALOAD_0 4 n)))))))
 ))
 (let ((?x4305 (sc_s 4)))
 (let (($x445 (= ?x805 ?x4305)))
 (let ((?x9521 (used_gas_s x_0 x_CALLDATALOAD_0 5)))
 (let ((?x8635 (bvadd (_ bv63 6) ?x4305)))
 (let ((?x8912 (stack_s x_0 x_CALLDATALOAD_0 4 ?x8635)))
 (let (($x4332 (= (stack_s x_0 x_CALLDATALOAD_0 5 ?x2394) (stack_s x_0 x_CALLDATALOAD_0 4 (bvadd (_ bv62 6) ?x4305)))))
 (let (($x64 (exc_halt_s 4)))
 (let (($x1102 (= $x64 (or (exc_halt_s 3) (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_s 3))))))))
 (let (($x5786 (forall ((w (_ BitVec 256)) )(let ((?x5753 (storage_s x_0 x_CALLDATALOAD_0 3 w)))
 (let ((?x2029 (storage_s x_0 x_CALLDATALOAD_0 4 w)))
 (= ?x2029 ?x5753))))
 ))
 (let (($x5769 (forall ((n (_ BitVec 6)) )(let ((?x275 (sc_s 3)))
 (let ((?x8615 (bvadd (_ bv63 6) ?x275)))
 (let (($x912 (bvsle ?x8615 n)))
 (or $x912 (= (stack_s x_0 x_CALLDATALOAD_0 4 n) (stack_s x_0 x_CALLDATALOAD_0 3 n)))))))
 ))
 (let ((?x275 (sc_s 3)))
 (let (($x858 (= ?x4305 ?x275)))
 (let ((?x9827 (used_gas_s x_0 x_CALLDATALOAD_0 4)))
 (let (($x292 (exc_halt_s 3)))
 (let (($x3338 (= $x292 (or (exc_halt_s 2) (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_s 2))))))))
 (let (($x7260 (forall ((w (_ BitVec 256)) )(let ((?x6725 (storage_s x_0 x_CALLDATALOAD_0 2 w)))
 (let ((?x5753 (storage_s x_0 x_CALLDATALOAD_0 3 w)))
 (= ?x5753 ?x6725))))
 ))
 (let (($x9217 (forall ((n (_ BitVec 6)) )(let ((?x218 (sc_s 2)))
 (let ((?x11283 (bvadd (_ bv63 6) ?x218)))
 (let (($x1729 (bvsle ?x11283 n)))
 (or $x1729 (= (stack_s x_0 x_CALLDATALOAD_0 3 n) (stack_s x_0 x_CALLDATALOAD_0 2 n)))))))
 ))
 (let ((?x218 (sc_s 2)))
 (let (($x9415 (= ?x275 ?x218)))
 (let ((?x1609 (used_gas_s x_0 x_CALLDATALOAD_0 3)))
 (let ((?x8615 (bvadd (_ bv63 6) ?x275)))
 (let ((?x5557 (stack_s x_0 x_CALLDATALOAD_0 3 ?x8615)))
 (let (($x247 (exc_halt_s 2)))
 (let (($x3736 (= $x247 (or (exc_halt_s 1) (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_s 1))))))))
 (let (($x1385 (forall ((w (_ BitVec 256)) )(let ((?x614 (storage_s x_0 x_CALLDATALOAD_0 1 w)))
 (let ((?x6725 (storage_s x_0 x_CALLDATALOAD_0 2 w)))
 (= ?x6725 ?x614))))
 ))
 (let (($x2605 (forall ((n (_ BitVec 6)) )(let ((?x154 (sc_s 1)))
 (let ((?x7444 (bvadd (_ bv63 6) ?x154)))
 (let (($x2123 (bvsle ?x7444 n)))
 (or $x2123 (= (stack_s x_0 x_CALLDATALOAD_0 2 n) (stack_s x_0 x_CALLDATALOAD_0 1 n)))))))
 ))
 (let ((?x154 (sc_s 1)))
 (let (($x8214 (= ?x218 ?x154)))
 (let ((?x1286 (used_gas_s x_0 x_CALLDATALOAD_0 2)))
 (let ((?x11283 (bvadd (_ bv63 6) ?x218)))
 (let ((?x1340 (stack_s x_0 x_CALLDATALOAD_0 2 ?x11283)))
 (let (($x5497 (= ?x1340 (f_CALLDATALOAD x_0 x_CALLDATALOAD_0 (stack_s x_0 x_CALLDATALOAD_0 1 (bvadd (_ bv63 6) ?x154))))))
 (let (($x2400 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x72))) (_ bv0 1)))))
 (let (($x189 (exc_halt_s 1)))
 (let (($x8070 (forall ((w (_ BitVec 256)) )(let ((?x1412 (storage_s x_0 x_CALLDATALOAD_0 0 w)))
 (let ((?x614 (storage_s x_0 x_CALLDATALOAD_0 1 w)))
 (= ?x614 ?x1412))))
 ))
 (let (($x5161 (forall ((n (_ BitVec 6)) )(or (bvsle (bvadd (_ bv63 6) (sc_s 0)) n) (= (stack_s x_0 x_CALLDATALOAD_0 1 n) (stack_s x_0 x_CALLDATALOAD_0 0 n))))
 ))
 (let (($x8455 (= ?x154 (bvadd (_ bv1 6) ?x72))))
 (let ((?x2582 (bvadd (_ bv63 6) ?x72)))
 (let ((?x1384 (stack_s x_0 x_CALLDATALOAD_0 0 ?x2582)))
 (let (($x9021 (forall ((w0 (_ BitVec 256)) )(let ((?x5802 (ite (= (stack_s x_0 x_CALLDATALOAD_0 1 (bvadd (_ bv63 6) (sc_s 1))) w0) x_CALLDATALOAD_0 (_ bv0 256))))
 (let ((?x5576 (f_CALLDATALOAD x_0 x_CALLDATALOAD_0 w0)))
 (= ?x5576 ?x5802))))
 ))
 (let (($x825 (forall ((w (_ BitVec 256)) )(let ((?x1412 (storage_s x_0 x_CALLDATALOAD_0 0 w)))
 (= ?x1412 (_ bv0 256))))
 ))
 (let (($x1544 (= ?x960 0)))
 (let (($x2233 (not $x57)))
 (let (($x1171 (= (stack_s x_0 x_CALLDATALOAD_0 0 (_ bv0 6)) x_0)))
 (let (($x8685 (= ?x72 (_ bv1 6))))
 (and $x8685 $x1171 $x2233 $x1544 $x825 $x9021 (= (stack_s x_0 x_CALLDATALOAD_0 1 (bvadd (_ bv63 6) ?x154)) ?x1384) (= (stack_s x_0 x_CALLDATALOAD_0 1 ?x2582) ?x1384) (= (used_gas_s x_0 x_CALLDATALOAD_0 1) (+ 3 ?x960)) $x8455 $x5161 $x8070 (= $x189 (or $x57 (not (bvsle (_ bv0 6) ?x2582)) $x2400)) $x5497 (= ?x1286 (+ 3 (used_gas_s x_0 x_CALLDATALOAD_0 1))) $x8214 $x2605 $x1385 $x3736 (= ?x5557 (bvnot ?x1340)) (= ?x1609 (+ 3 ?x1286)) $x9415 $x9217 $x7260 $x3338 (= ?x8912 (bvnot ?x5557)) (= ?x9827 (+ 3 ?x1609)) $x858 $x5769 $x5786 $x1102 $x4332 (= (stack_s x_0 x_CALLDATALOAD_0 5 (bvadd (_ bv62 6) ?x805)) ?x8912) (= ?x9521 (+ 3 ?x9827)) $x445 $x5790 $x1275 $x6033 $x1633 $x2577 $x959 $x1419 $x9067 $x1094 (= (used_gas_t x_0 x_CALLDATALOAD_0 1) (+ 3 ?x5778)) (= ?x4023 ?x63) $x1779 $x1483 (= $x1920 (or $x56 (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) ?x63))))) $x73 $x1573 $x58 $x1400 $x1516 (not (and $x6116 $x1809 $x3517 $x1599))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
 )
(check-sat)
(get-proof)
