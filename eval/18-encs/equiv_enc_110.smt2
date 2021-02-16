; DUP2 DUP3 SWAP2 SSTORE SLT => SSTORE PUSH 0x00
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
(assert
 (exists ((x_0 (_ BitVec 256)) (x_1 (_ BitVec 256)) (x_SSTORE_0 (_ BitVec 256)) )(let (($x11054 (forall ((w (_ BitVec 256)) )(let ((?x11015 (storage_t x_0 x_1 x_SSTORE_0 2 w)))
 (let ((?x11055 (storage_s x_0 x_1 x_SSTORE_0 5 w)))
 (= ?x11055 ?x11015))))
 ))
 (let (($x903 (exc_halt_t 2)))
 (let (($x3979 (exc_halt_s 5)))
 (let (($x6812 (= $x3979 $x903)))
 (let (($x4559 (forall ((n (_ BitVec 6)) )(let ((?x11099 (stack_t x_0 x_1 x_SSTORE_0 2 n)))
 (let ((?x3033 (stack_s x_0 x_1 x_SSTORE_0 5 n)))
 (let (($x7544 (= ?x3033 ?x11099)))
 (or (bvsle (sc_t 2) n) $x7544)))))
 ))
 (let ((?x4056 (sc_t 2)))
 (let ((?x805 (sc_s 5)))
 (let (($x2588 (= ?x805 ?x4056)))
 (let ((?x11035 (used_gas_t x_0 x_1 x_SSTORE_0 0)))
 (let ((?x5283 (used_gas_s x_0 x_1 x_SSTORE_0 0)))
 (let (($x10930 (= ?x5283 ?x11035)))
 (let (($x10418 (forall ((w (_ BitVec 256)) )(let ((?x11004 (storage_t x_0 x_1 x_SSTORE_0 0 w)))
 (let ((?x11030 (storage_s x_0 x_1 x_SSTORE_0 0 w)))
 (= ?x11030 ?x11004))))
 ))
 (let (($x56 (exc_halt_t 0)))
 (let (($x57 (exc_halt_s 0)))
 (let (($x58 (= $x57 $x56)))
 (let (($x8657 (forall ((n (_ BitVec 6)) )(let ((?x10575 (stack_t x_0 x_1 x_SSTORE_0 0 n)))
 (let ((?x1500 (stack_s x_0 x_1 x_SSTORE_0 0 n)))
 (let (($x10408 (= ?x1500 ?x10575)))
 (or (bvsle (sc_t 0) n) $x10408)))))
 ))
 (let ((?x63 (sc_t 0)))
 (let ((?x72 (sc_s 0)))
 (let (($x73 (= ?x72 ?x63)))
 (let (($x1920 (exc_halt_t 1)))
 (let (($x4988 (or $x1920 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_t 1)))) (_ bv0 1))))))
 (let (($x8444 (forall ((w (_ BitVec 256)) )(let ((?x584 (storage_t x_0 x_1 x_SSTORE_0 1 w)))
 (let ((?x11015 (storage_t x_0 x_1 x_SSTORE_0 2 w)))
 (= ?x11015 ?x584))))
 ))
 (let (($x885 (forall ((n (_ BitVec 6)) )(let ((?x1333 (stack_t x_0 x_1 x_SSTORE_0 1 n)))
 (let ((?x11099 (stack_t x_0 x_1 x_SSTORE_0 2 n)))
 (or (bvsle (sc_t 1) n) (= ?x11099 ?x1333)))))
 ))
 (let (($x2842 (= (used_gas_t x_0 x_1 x_SSTORE_0 2) (+ 3 (used_gas_t x_0 x_1 x_SSTORE_0 1)))))
 (let (($x490 (forall ((n (_ BitVec 6)) )(let ((?x10575 (stack_t x_0 x_1 x_SSTORE_0 0 n)))
 (let ((?x1333 (stack_t x_0 x_1 x_SSTORE_0 1 n)))
 (or (bvsle (bvadd (_ bv62 6) (sc_t 0)) n) (= ?x1333 ?x10575)))))
 ))
 (let (($x1217 (= (stack_t x_0 x_1 x_SSTORE_0 0 (bvadd (_ bv62 6) ?x63)) (_ bv0 256))))
 (let ((?x4629 (storage_t x_0 x_1 x_SSTORE_0 0 (stack_t x_0 x_1 x_SSTORE_0 0 (bvadd (_ bv63 6) ?x63)))))
 (let ((?x5098 (+ ?x11035 (ite (= ?x4629 (_ bv0 256)) (ite $x1217 5000 20000) (ite $x1217 (- 10000) 5000)))))
 (let ((?x3472 (used_gas_t x_0 x_1 x_SSTORE_0 1)))
 (let (($x10452 (forall ((w (_ BitVec 256)) )(let ((?x11004 (storage_t x_0 x_1 x_SSTORE_0 0 w)))
 (let ((?x6405 (ite (= w (stack_t x_0 x_1 x_SSTORE_0 0 (bvadd (_ bv63 6) (sc_t 0)))) (stack_t x_0 x_1 x_SSTORE_0 0 (bvadd (_ bv62 6) (sc_t 0))) ?x11004)))
 (let ((?x584 (storage_t x_0 x_1 x_SSTORE_0 1 w)))
 (= ?x584 ?x6405)))))
 ))
 (let (($x4848 (= $x3979 (or (exc_halt_s 4) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 4))))))))
 (let (($x10670 (forall ((w (_ BitVec 256)) )(let ((?x10939 (storage_s x_0 x_1 x_SSTORE_0 4 w)))
 (let ((?x11055 (storage_s x_0 x_1 x_SSTORE_0 5 w)))
 (= ?x11055 ?x10939))))
 ))
 (let (($x4491 (forall ((n (_ BitVec 6)) )(let ((?x8375 (stack_s x_0 x_1 x_SSTORE_0 4 n)))
 (let ((?x3033 (stack_s x_0 x_1 x_SSTORE_0 5 n)))
 (or (= ?x3033 ?x8375) (bvsle (bvadd (_ bv62 6) (sc_s 4)) n)))))
 ))
 (let (($x4495 (= (used_gas_s x_0 x_1 x_SSTORE_0 5) (+ 3 (used_gas_s x_0 x_1 x_SSTORE_0 4)))))
 (let (($x3669 (bvsle (stack_s x_0 x_1 x_SSTORE_0 4 (bvadd (_ bv62 6) (sc_s 4))) (stack_s x_0 x_1 x_SSTORE_0 4 (bvadd (_ bv63 6) (sc_s 4))))))
 (let (($x10956 (= (stack_s x_0 x_1 x_SSTORE_0 5 (bvadd (_ bv63 6) ?x805)) (ite $x3669 (_ bv0 256) (_ bv1 256)))))
 (let (($x64 (exc_halt_s 4)))
 (let (($x559 (= $x64 (or (exc_halt_s 3) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 3))))))))
 (let (($x10596 (forall ((n (_ BitVec 6)) )(let ((?x5253 (stack_s x_0 x_1 x_SSTORE_0 3 n)))
 (let ((?x8375 (stack_s x_0 x_1 x_SSTORE_0 4 n)))
 (or (bvsle (bvadd (_ bv62 6) (sc_s 3)) n) (= ?x8375 ?x5253)))))
 ))
 (let ((?x275 (sc_s 3)))
 (let ((?x7469 (bvadd (_ bv62 6) ?x275)))
 (let ((?x7690 (stack_s x_0 x_1 x_SSTORE_0 3 ?x7469)))
 (let (($x5588 (= ?x7690 (_ bv0 256))))
 (let ((?x11143 (bvadd (_ bv63 6) ?x275)))
 (let ((?x357 (stack_s x_0 x_1 x_SSTORE_0 3 ?x11143)))
 (let ((?x11170 (ite (= (storage_s x_0 x_1 x_SSTORE_0 3 ?x357) (_ bv0 256)) (ite $x5588 5000 20000) (ite $x5588 (- 10000) 5000))))
 (let ((?x10641 (used_gas_s x_0 x_1 x_SSTORE_0 3)))
 (let ((?x3150 (used_gas_s x_0 x_1 x_SSTORE_0 4)))
 (let (($x8058 (forall ((w (_ BitVec 256)) )(let ((?x10760 (storage_s x_0 x_1 x_SSTORE_0 3 w)))
 (let (($x11204 (= w (stack_s x_0 x_1 x_SSTORE_0 3 (bvadd (_ bv63 6) (sc_s 3))))))
 (let ((?x10882 (ite $x11204 (stack_s x_0 x_1 x_SSTORE_0 3 (bvadd (_ bv62 6) (sc_s 3))) ?x10760)))
 (let ((?x10939 (storage_s x_0 x_1 x_SSTORE_0 4 w)))
 (= ?x10939 ?x10882))))))
 ))
 (let (($x292 (exc_halt_s 3)))
 (let (($x5994 (= $x292 (or (exc_halt_s 2) (not (bvsle (_ bv0 6) (bvadd (_ bv61 6) (sc_s 2))))))))
 (let (($x230 (forall ((w (_ BitVec 256)) )(let ((?x4685 (storage_s x_0 x_1 x_SSTORE_0 2 w)))
 (let ((?x10760 (storage_s x_0 x_1 x_SSTORE_0 3 w)))
 (= ?x10760 ?x4685))))
 ))
 (let (($x10859 (forall ((n (_ BitVec 6)) )(let ((?x10846 (stack_s x_0 x_1 x_SSTORE_0 2 n)))
 (let ((?x5253 (stack_s x_0 x_1 x_SSTORE_0 3 n)))
 (or (bvsle (bvadd (_ bv61 6) (sc_s 2)) n) (= ?x5253 ?x10846)))))
 ))
 (let ((?x8837 (stack_s x_0 x_1 x_SSTORE_0 2 (bvadd (_ bv63 6) (sc_s 2)))))
 (let (($x189 (exc_halt_s 1)))
 (let (($x10923 (or $x189 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 1)))) (_ bv0 1))) (not (bvsle (_ bv0 6) (bvadd (_ bv61 6) (sc_s 1)))))))
 (let (($x247 (exc_halt_s 2)))
 (let (($x264 (forall ((w (_ BitVec 256)) )(let ((?x6583 (storage_s x_0 x_1 x_SSTORE_0 1 w)))
 (let ((?x4685 (storage_s x_0 x_1 x_SSTORE_0 2 w)))
 (= ?x4685 ?x6583))))
 ))
 (let (($x255 (forall ((n (_ BitVec 6)) )(let ((?x4947 (stack_s x_0 x_1 x_SSTORE_0 1 n)))
 (let ((?x10846 (stack_s x_0 x_1 x_SSTORE_0 2 n)))
 (or (= ?x10846 ?x4947) (bvsle (bvadd (_ bv61 6) (sc_s 1)) n)))))
 ))
 (let ((?x11176 (used_gas_s x_0 x_1 x_SSTORE_0 2)))
 (let ((?x154 (sc_s 1)))
 (let ((?x11178 (bvadd (_ bv63 6) ?x154)))
 (let ((?x11205 (stack_s x_0 x_1 x_SSTORE_0 1 ?x11178)))
 (let (($x10357 (= (stack_s x_0 x_1 x_SSTORE_0 2 (bvadd (_ bv62 6) ?x154)) (stack_s x_0 x_1 x_SSTORE_0 1 (bvadd (_ bv62 6) ?x154)))))
 (let ((?x3661 (bvadd (_ bv61 6) ?x154)))
 (let ((?x10403 (stack_s x_0 x_1 x_SSTORE_0 1 ?x3661)))
 (let (($x10957 (or $x57 (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) ?x72))) (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x72))) (_ bv0 1))))))
 (let (($x3743 (forall ((w (_ BitVec 256)) )(let ((?x11030 (storage_s x_0 x_1 x_SSTORE_0 0 w)))
 (let ((?x6583 (storage_s x_0 x_1 x_SSTORE_0 1 w)))
 (= ?x6583 ?x11030))))
 ))
 (let (($x8030 (forall ((n (_ BitVec 6)) )(let ((?x1500 (stack_s x_0 x_1 x_SSTORE_0 0 n)))
 (let ((?x4947 (stack_s x_0 x_1 x_SSTORE_0 1 n)))
 (or (= ?x4947 ?x1500) (bvsle (bvadd (_ bv62 6) (sc_s 0)) n)))))
 ))
 (let (($x3740 (= (stack_s x_0 x_1 x_SSTORE_0 1 (bvadd (_ bv63 6) ?x72)) (stack_s x_0 x_1 x_SSTORE_0 0 (bvadd (_ bv63 6) ?x72)))))
 (let ((?x11206 (bvadd (_ bv62 6) ?x72)))
 (let ((?x4989 (stack_s x_0 x_1 x_SSTORE_0 0 ?x11206)))
 (let (($x5062 (forall ((w (_ BitVec 256)) )(let (($x11204 (= w (stack_s x_0 x_1 x_SSTORE_0 3 (bvadd (_ bv63 6) (sc_s 3))))))
 (let ((?x11030 (storage_s x_0 x_1 x_SSTORE_0 0 w)))
 (= ?x11030 (ite $x11204 x_SSTORE_0 (_ bv0 256))))))
 ))
 (let (($x6268 (= ?x5283 0)))
 (let (($x71 (= (stack_s x_0 x_1 x_SSTORE_0 0 (_ bv1 6)) x_1)))
 (let (($x4803 (= (stack_s x_0 x_1 x_SSTORE_0 0 (_ bv0 6)) x_0)))
 (let (($x5694 (= ?x72 (_ bv2 6))))
 (and $x5694 $x4803 $x71 (not $x57) $x6268 $x5062 (= ?x11205 ?x4989) (= (stack_s x_0 x_1 x_SSTORE_0 1 ?x11206) ?x4989) $x3740 (= (used_gas_s x_0 x_1 x_SSTORE_0 1) (+ 3 ?x5283)) (= ?x154 (bvadd (_ bv1 6) ?x72)) $x8030 $x3743 (= $x189 $x10957) (= ?x8837 ?x10403) (= (stack_s x_0 x_1 x_SSTORE_0 2 ?x3661) ?x10403) $x10357 (= (stack_s x_0 x_1 x_SSTORE_0 2 ?x11178) ?x11205) (= ?x11176 (+ 3 (used_gas_s x_0 x_1 x_SSTORE_0 1))) (= (sc_s 2) (bvadd (_ bv1 6) ?x154)) $x255 $x264 (= $x247 $x10923) (= ?x357 (stack_s x_0 x_1 x_SSTORE_0 2 (bvadd (_ bv61 6) (sc_s 2)))) (= (stack_s x_0 x_1 x_SSTORE_0 3 (bvadd (_ bv61 6) ?x275)) ?x8837) (= ?x7690 (stack_s x_0 x_1 x_SSTORE_0 2 (bvadd (_ bv62 6) (sc_s 2)))) (= ?x10641 (+ 3 ?x11176)) (= ?x275 (sc_s 2)) $x10859 $x230 $x5994 $x8058 (= ?x3150 (+ ?x10641 ?x11170)) (= (sc_s 4) ?x7469) $x10596 $x559 $x10956 $x4495 (= ?x805 (bvadd (_ bv63 6) (sc_s 4))) $x4491 $x10670 $x4848 $x10452 (= ?x3472 ?x5098) (= (sc_t 1) (bvadd (_ bv62 6) ?x63)) $x490 (= $x1920 (or $x56 (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) ?x63))))) (= (stack_t x_0 x_1 x_SSTORE_0 2 (sc_t 1)) (_ bv0 256)) $x2842 (= ?x4056 (bvadd (_ bv1 6) (sc_t 1))) $x885 $x8444 (= $x903 $x4988) $x73 $x8657 $x58 $x10418 $x10930 (not (and $x2588 $x4559 $x6812 $x11054)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
 )
(check-sat)
(get-proof)