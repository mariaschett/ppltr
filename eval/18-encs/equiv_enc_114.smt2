; PUSH cw_4 MLOAD PUSH cw_5 DUP1 SWAP2 DUP3 => PUSH cw_5 PUSH cw_5 PUSH cw_4 MLOAD PUSH cw_5
(set-option :produce-proofs true)
; 
(set-info :status unknown)
(declare-fun storage_t ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 256)) (_ BitVec 256))
(declare-fun w_5 () (_ BitVec 256))
(declare-fun w_4 () (_ BitVec 256))
(declare-fun storage_s ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 256)) (_ BitVec 256))
(declare-fun exc_halt_t (Int) Bool)
(declare-fun exc_halt_s (Int) Bool)
(declare-fun stack_t ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 6)) (_ BitVec 256))
(declare-fun stack_s ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 6)) (_ BitVec 256))
(declare-fun sc_t (Int) (_ BitVec 6))
(declare-fun sc_s (Int) (_ BitVec 6))
(declare-fun used_gas_t ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int) Int)
(declare-fun used_gas_s ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int) Int)
(declare-fun f_MLOAD ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256)) (_ BitVec 256))
(assert
 (exists ((x_MLOAD_0 (_ BitVec 256)) )(let (($x8140 (forall ((w (_ BitVec 256)) )(let ((?x3897 (storage_t w_4 w_5 x_MLOAD_0 5 w)))
 (let ((?x6520 (storage_s w_4 w_5 x_MLOAD_0 6 w)))
 (= ?x6520 ?x3897))))
 ))
 (let (($x886 (exc_halt_t 5)))
 (let (($x772 (exc_halt_s 6)))
 (let (($x889 (= $x772 $x886)))
 (let (($x10036 (forall ((n (_ BitVec 6)) )(let ((?x1819 (stack_t w_4 w_5 x_MLOAD_0 5 n)))
 (let ((?x8752 (stack_s w_4 w_5 x_MLOAD_0 6 n)))
 (let (($x6641 (= ?x8752 ?x1819)))
 (let ((?x919 (sc_t 5)))
 (let (($x4107 (bvsle ?x919 n)))
 (or $x4107 $x6641)))))))
 ))
 (let ((?x919 (sc_t 5)))
 (let ((?x926 (sc_s 6)))
 (let (($x929 (= ?x926 ?x919)))
 (let ((?x9429 (used_gas_t w_4 w_5 x_MLOAD_0 0)))
 (let ((?x5226 (used_gas_s w_4 w_5 x_MLOAD_0 0)))
 (let (($x914 (= ?x5226 ?x9429)))
 (let (($x9619 (forall ((w (_ BitVec 256)) )(let ((?x10870 (storage_t w_4 w_5 x_MLOAD_0 0 w)))
 (let ((?x6911 (storage_s w_4 w_5 x_MLOAD_0 0 w)))
 (= ?x6911 ?x10870))))
 ))
 (let (($x56 (exc_halt_t 0)))
 (let (($x57 (exc_halt_s 0)))
 (let (($x58 (= $x57 $x56)))
 (let (($x10192 (forall ((n (_ BitVec 6)) )(let ((?x9625 (stack_t w_4 w_5 x_MLOAD_0 0 n)))
 (let ((?x9485 (stack_s w_4 w_5 x_MLOAD_0 0 n)))
 (let (($x9490 (= ?x9485 ?x9625)))
 (let ((?x63 (sc_t 0)))
 (let (($x4561 (bvsle ?x63 n)))
 (or $x4561 $x9490)))))))
 ))
 (let ((?x63 (sc_t 0)))
 (let ((?x72 (sc_s 0)))
 (let (($x73 (= ?x72 ?x63)))
 (let (($x3723 (exc_halt_t 4)))
 (let (($x7314 (or $x3723 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_t 4)))) (_ bv0 1))))))
 (let (($x10279 (forall ((w (_ BitVec 256)) )(let ((?x8982 (storage_t w_4 w_5 x_MLOAD_0 4 w)))
 (let ((?x3897 (storage_t w_4 w_5 x_MLOAD_0 5 w)))
 (= ?x3897 ?x8982))))
 ))
 (let (($x5887 (forall ((n (_ BitVec 6)) )(let ((?x6439 (stack_t w_4 w_5 x_MLOAD_0 4 n)))
 (let ((?x1819 (stack_t w_4 w_5 x_MLOAD_0 5 n)))
 (or (bvsle (sc_t 4) n) (= ?x1819 ?x6439)))))
 ))
 (let (($x7413 (= (used_gas_t w_4 w_5 x_MLOAD_0 5) (+ 3 (used_gas_t w_4 w_5 x_MLOAD_0 4)))))
 (let (($x10548 (= $x3723 (or (exc_halt_t 3) (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_t 3))))))))
 (let (($x7400 (forall ((w (_ BitVec 256)) )(let ((?x10589 (storage_t w_4 w_5 x_MLOAD_0 3 w)))
 (let ((?x8982 (storage_t w_4 w_5 x_MLOAD_0 4 w)))
 (= ?x8982 ?x10589))))
 ))
 (let (($x7532 (forall ((n (_ BitVec 6)) )(let ((?x5437 (stack_t w_4 w_5 x_MLOAD_0 3 n)))
 (let ((?x6439 (stack_t w_4 w_5 x_MLOAD_0 4 n)))
 (or (bvsle (bvadd (_ bv63 6) (sc_t 3)) n) (= ?x6439 ?x5437)))))
 ))
 (let ((?x9455 (used_gas_t w_4 w_5 x_MLOAD_0 4)))
 (let ((?x7671 (f_MLOAD w_4 w_5 x_MLOAD_0 (stack_t w_4 w_5 x_MLOAD_0 3 (bvadd (_ bv63 6) (sc_t 3))))))
 (let (($x6776 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_t 2)))) (_ bv0 1)))))
 (let (($x903 (exc_halt_t 2)))
 (let (($x10336 (exc_halt_t 3)))
 (let (($x8059 (= $x10336 (or $x903 $x6776))))
 (let (($x7062 (forall ((w (_ BitVec 256)) )(let ((?x1456 (storage_t w_4 w_5 x_MLOAD_0 2 w)))
 (let ((?x10589 (storage_t w_4 w_5 x_MLOAD_0 3 w)))
 (= ?x10589 ?x1456))))
 ))
 (let (($x3552 (forall ((n (_ BitVec 6)) )(let ((?x5565 (stack_t w_4 w_5 x_MLOAD_0 2 n)))
 (let ((?x5437 (stack_t w_4 w_5 x_MLOAD_0 3 n)))
 (let ((?x4056 (sc_t 2)))
 (let (($x4536 (bvsle ?x4056 n)))
 (or $x4536 (= ?x5437 ?x5565)))))))
 ))
 (let ((?x2012 (sc_t 3)))
 (let (($x8282 (= ?x2012 (bvadd (_ bv1 6) (sc_t 2)))))
 (let ((?x10381 (used_gas_t w_4 w_5 x_MLOAD_0 3)))
 (let (($x390 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_t 1)))) (_ bv0 1)))))
 (let (($x1920 (exc_halt_t 1)))
 (let (($x9144 (= $x903 (or $x1920 $x390))))
 (let (($x9667 (forall ((w (_ BitVec 256)) )(let ((?x5673 (storage_t w_4 w_5 x_MLOAD_0 1 w)))
 (let ((?x1456 (storage_t w_4 w_5 x_MLOAD_0 2 w)))
 (= ?x1456 ?x5673))))
 ))
 (let (($x374 (forall ((n (_ BitVec 6)) )(let ((?x338 (stack_t w_4 w_5 x_MLOAD_0 1 n)))
 (let ((?x5565 (stack_t w_4 w_5 x_MLOAD_0 2 n)))
 (let ((?x4023 (sc_t 1)))
 (let (($x717 (bvsle ?x4023 n)))
 (or $x717 (= ?x5565 ?x338)))))))
 ))
 (let ((?x4056 (sc_t 2)))
 (let (($x8461 (= ?x4056 (bvadd (_ bv1 6) (sc_t 1)))))
 (let ((?x3143 (used_gas_t w_4 w_5 x_MLOAD_0 2)))
 (let (($x6187 (= $x1920 (or $x56 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x63))) (_ bv0 1)))))))
 (let (($x7541 (forall ((w (_ BitVec 256)) )(let ((?x10870 (storage_t w_4 w_5 x_MLOAD_0 0 w)))
 (let ((?x5673 (storage_t w_4 w_5 x_MLOAD_0 1 w)))
 (= ?x5673 ?x10870))))
 ))
 (let (($x9568 (forall ((n (_ BitVec 6)) )(let ((?x9625 (stack_t w_4 w_5 x_MLOAD_0 0 n)))
 (let ((?x338 (stack_t w_4 w_5 x_MLOAD_0 1 n)))
 (let ((?x63 (sc_t 0)))
 (let (($x4561 (bvsle ?x63 n)))
 (or $x4561 (= ?x338 ?x9625)))))))
 ))
 (let ((?x4023 (sc_t 1)))
 (let (($x8795 (= ?x4023 (bvadd (_ bv1 6) ?x63))))
 (let (($x7645 (not (bvsle (_ bv0 6) (bvadd (_ bv61 6) (sc_s 5))))))
 (let (($x3979 (exc_halt_s 5)))
 (let (($x10200 (or $x3979 $x7645 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 5)))) (_ bv0 1))))))
 (let (($x9739 (forall ((w (_ BitVec 256)) )(let ((?x528 (storage_s w_4 w_5 x_MLOAD_0 5 w)))
 (let ((?x6520 (storage_s w_4 w_5 x_MLOAD_0 6 w)))
 (= ?x6520 ?x528))))
 ))
 (let (($x1053 (forall ((n (_ BitVec 6)) )(let ((?x805 (sc_s 5)))
 (let ((?x10112 (bvadd (_ bv61 6) ?x805)))
 (let (($x5469 (bvsle ?x10112 n)))
 (let ((?x5590 (stack_s w_4 w_5 x_MLOAD_0 5 n)))
 (let ((?x8752 (stack_s w_4 w_5 x_MLOAD_0 6 n)))
 (or (= ?x8752 ?x5590) $x5469)))))))
 ))
 (let (($x7276 (= (used_gas_s w_4 w_5 x_MLOAD_0 6) (+ 3 (used_gas_s w_4 w_5 x_MLOAD_0 5)))))
 (let ((?x805 (sc_s 5)))
 (let ((?x10909 (bvadd (_ bv63 6) ?x805)))
 (let ((?x103 (stack_s w_4 w_5 x_MLOAD_0 5 ?x10909)))
 (let ((?x6337 (bvadd (_ bv62 6) ?x805)))
 (let ((?x6357 (stack_s w_4 w_5 x_MLOAD_0 5 ?x6337)))
 (let ((?x10112 (bvadd (_ bv61 6) ?x805)))
 (let ((?x9746 (stack_s w_4 w_5 x_MLOAD_0 5 ?x10112)))
 (let (($x6319 (= $x3979 (or (exc_halt_s 4) (not (bvsle (_ bv0 6) (bvadd (_ bv61 6) (sc_s 4))))))))
 (let (($x10297 (forall ((w (_ BitVec 256)) )(let ((?x10523 (storage_s w_4 w_5 x_MLOAD_0 4 w)))
 (let ((?x528 (storage_s w_4 w_5 x_MLOAD_0 5 w)))
 (= ?x528 ?x10523))))
 ))
 (let (($x7046 (forall ((n (_ BitVec 6)) )(let ((?x7346 (stack_s w_4 w_5 x_MLOAD_0 4 n)))
 (let ((?x5590 (stack_s w_4 w_5 x_MLOAD_0 5 n)))
 (or (= ?x5590 ?x7346) (bvsle (bvadd (_ bv61 6) (sc_s 4)) n)))))
 ))
 (let ((?x4305 (sc_s 4)))
 (let (($x3284 (= ?x805 ?x4305)))
 (let ((?x284 (used_gas_s w_4 w_5 x_MLOAD_0 5)))
 (let (($x6200 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 3)))) (_ bv0 1)))))
 (let (($x5962 (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_s 3))))))
 (let (($x292 (exc_halt_s 3)))
 (let (($x64 (exc_halt_s 4)))
 (let (($x9054 (forall ((w (_ BitVec 256)) )(let ((?x9586 (storage_s w_4 w_5 x_MLOAD_0 3 w)))
 (let ((?x10523 (storage_s w_4 w_5 x_MLOAD_0 4 w)))
 (= ?x10523 ?x9586))))
 ))
 (let (($x10447 (forall ((n (_ BitVec 6)) )(let ((?x7665 (stack_s w_4 w_5 x_MLOAD_0 3 n)))
 (let ((?x7346 (stack_s w_4 w_5 x_MLOAD_0 4 n)))
 (let ((?x275 (sc_s 3)))
 (let ((?x11143 (bvadd (_ bv63 6) ?x275)))
 (let (($x2455 (bvsle ?x11143 n)))
 (or $x2455 (= ?x7346 ?x7665))))))))
 ))
 (let (($x9387 (= ?x4305 (bvadd (_ bv1 6) (sc_s 3)))))
 (let ((?x6352 (used_gas_s w_4 w_5 x_MLOAD_0 4)))
 (let ((?x275 (sc_s 3)))
 (let ((?x11143 (bvadd (_ bv63 6) ?x275)))
 (let ((?x3505 (stack_s w_4 w_5 x_MLOAD_0 3 ?x11143)))
 (let (($x247 (exc_halt_s 2)))
 (let (($x9626 (or $x247 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 2)))) (_ bv0 1))))))
 (let (($x3801 (forall ((w (_ BitVec 256)) )(let ((?x6977 (storage_s w_4 w_5 x_MLOAD_0 2 w)))
 (let ((?x9586 (storage_s w_4 w_5 x_MLOAD_0 3 w)))
 (= ?x9586 ?x6977))))
 ))
 (let (($x1143 (forall ((n (_ BitVec 6)) )(let ((?x650 (stack_s w_4 w_5 x_MLOAD_0 2 n)))
 (let ((?x7665 (stack_s w_4 w_5 x_MLOAD_0 3 n)))
 (or (bvsle (sc_s 2) n) (= ?x7665 ?x650)))))
 ))
 (let ((?x9972 (used_gas_s w_4 w_5 x_MLOAD_0 3)))
 (let (($x295 (= $x247 (or (exc_halt_s 1) (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_s 1))))))))
 (let (($x2979 (forall ((w (_ BitVec 256)) )(let ((?x5539 (storage_s w_4 w_5 x_MLOAD_0 1 w)))
 (let ((?x6977 (storage_s w_4 w_5 x_MLOAD_0 2 w)))
 (= ?x6977 ?x5539))))
 ))
 (let (($x5166 (forall ((n (_ BitVec 6)) )(let ((?x3469 (stack_s w_4 w_5 x_MLOAD_0 1 n)))
 (let ((?x650 (stack_s w_4 w_5 x_MLOAD_0 2 n)))
 (let ((?x154 (sc_s 1)))
 (let ((?x11178 (bvadd (_ bv63 6) ?x154)))
 (let (($x6612 (bvsle ?x11178 n)))
 (or $x6612 (= ?x650 ?x3469))))))))
 ))
 (let ((?x9623 (used_gas_s w_4 w_5 x_MLOAD_0 2)))
 (let ((?x7583 (f_MLOAD w_4 w_5 x_MLOAD_0 (stack_s w_4 w_5 x_MLOAD_0 1 (bvadd (_ bv63 6) (sc_s 1))))))
 (let (($x189 (exc_halt_s 1)))
 (let (($x5657 (= $x189 (or $x57 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x72))) (_ bv0 1)))))))
 (let (($x5678 (forall ((w (_ BitVec 256)) )(let ((?x6911 (storage_s w_4 w_5 x_MLOAD_0 0 w)))
 (let ((?x5539 (storage_s w_4 w_5 x_MLOAD_0 1 w)))
 (= ?x5539 ?x6911))))
 ))
 (let (($x10535 (forall ((n (_ BitVec 6)) )(let ((?x9485 (stack_s w_4 w_5 x_MLOAD_0 0 n)))
 (let ((?x3469 (stack_s w_4 w_5 x_MLOAD_0 1 n)))
 (or (= ?x3469 ?x9485) (bvsle (sc_s 0) n)))))
 ))
 (let ((?x154 (sc_s 1)))
 (let (($x10964 (= ?x154 (bvadd (_ bv1 6) ?x72))))
 (let (($x7311 (forall ((w0 (_ BitVec 256)) )(let ((?x5622 (ite (= (stack_s w_4 w_5 x_MLOAD_0 1 (bvadd (_ bv63 6) (sc_s 1))) w0) x_MLOAD_0 (_ bv0 256))))
 (let ((?x9829 (f_MLOAD w_4 w_5 x_MLOAD_0 w0)))
 (= ?x9829 ?x5622))))
 ))
 (let (($x9025 (forall ((w (_ BitVec 256)) )(let ((?x6911 (storage_s w_4 w_5 x_MLOAD_0 0 w)))
 (= ?x6911 (_ bv0 256))))
 ))
 (let (($x1619 (= ?x5226 0)))
 (let (($x11171 (not $x57)))
 (let (($x136 (= ?x72 (_ bv0 6))))
 (and $x136 $x11171 $x1619 $x9025 $x7311 (= (stack_s w_4 w_5 x_MLOAD_0 1 ?x72) w_4) (= (used_gas_s w_4 w_5 x_MLOAD_0 1) (+ 3 ?x5226)) $x10964 $x10535 $x5678 $x5657 (= (stack_s w_4 w_5 x_MLOAD_0 2 (bvadd (_ bv63 6) (sc_s 2))) ?x7583) (= ?x9623 (+ 3 (used_gas_s w_4 w_5 x_MLOAD_0 1))) (= (sc_s 2) ?x154) $x5166 $x2979 $x295 (= (stack_s w_4 w_5 x_MLOAD_0 3 (sc_s 2)) w_5) (= ?x9972 (+ 3 ?x9623)) (= ?x275 (bvadd (_ bv1 6) (sc_s 2))) $x1143 $x3801 (= $x292 $x9626) (= (stack_s w_4 w_5 x_MLOAD_0 4 (bvadd (_ bv63 6) ?x4305)) ?x3505) (= (stack_s w_4 w_5 x_MLOAD_0 4 ?x11143) ?x3505) (= ?x6352 (+ 3 ?x9972)) $x9387 $x10447 $x9054 (= $x64 (or $x292 $x5962 $x6200)) (= ?x103 (stack_s w_4 w_5 x_MLOAD_0 4 (bvadd (_ bv61 6) ?x4305))) (= ?x9746 (stack_s w_4 w_5 x_MLOAD_0 4 (bvadd (_ bv63 6) ?x4305))) (= ?x6357 (stack_s w_4 w_5 x_MLOAD_0 4 (bvadd (_ bv62 6) ?x4305))) (= ?x284 (+ 3 ?x6352)) $x3284 $x7046 $x10297 $x6319 (= (stack_s w_4 w_5 x_MLOAD_0 6 (bvadd (_ bv63 6) ?x926)) ?x9746) (= (stack_s w_4 w_5 x_MLOAD_0 6 ?x10112) ?x9746) (= (stack_s w_4 w_5 x_MLOAD_0 6 ?x6337) ?x6357) (= (stack_s w_4 w_5 x_MLOAD_0 6 ?x10909) ?x103) $x7276 (= ?x926 (bvadd (_ bv1 6) ?x805)) $x1053 $x9739 (= $x772 $x10200) (= (stack_t w_4 w_5 x_MLOAD_0 1 ?x63) w_5) (= (used_gas_t w_4 w_5 x_MLOAD_0 1) (+ 3 ?x9429)) $x8795 $x9568 $x7541 $x6187 (= (stack_t w_4 w_5 x_MLOAD_0 2 ?x4023) w_5) (= ?x3143 (+ 3 (used_gas_t w_4 w_5 x_MLOAD_0 1))) $x8461 $x374 $x9667 $x9144 (= (stack_t w_4 w_5 x_MLOAD_0 3 ?x4056) w_4) (= ?x10381 (+ 3 ?x3143)) $x8282 $x3552 $x7062 $x8059 (= (stack_t w_4 w_5 x_MLOAD_0 4 (bvadd (_ bv63 6) (sc_t 4))) ?x7671) (= ?x9455 (+ 3 ?x10381)) (= (sc_t 4) ?x2012) $x7532 $x7400 $x10548 (= (stack_t w_4 w_5 x_MLOAD_0 5 (sc_t 4)) w_5) $x7413 (= ?x919 (bvadd (_ bv1 6) (sc_t 4))) $x5887 $x10279 (= $x886 $x7314) $x73 $x10192 $x58 $x9619 $x914 (not (and $x929 $x10036 $x889 $x8140))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
 )
(check-sat)
(get-proof)
