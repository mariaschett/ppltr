; DUP3 DUP5 ADD SWAP2 SWAP1 SWAP2 => SWAP1 DUP3 DUP5 ADD
(set-option :produce-proofs true)
; 
(set-info :status unknown)
(declare-fun storage_t ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 256)) (_ BitVec 256))
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
 (exists ((x_0 (_ BitVec 256)) (x_1 (_ BitVec 256)) (x_2 (_ BitVec 256)) (x_3 (_ BitVec 256)) )(let (($x4100 (forall ((w (_ BitVec 256)) )(let ((?x6202 (storage_t x_0 x_1 x_2 x_3 4 w)))
 (let ((?x1061 (storage_s x_0 x_1 x_2 x_3 6 w)))
 (= ?x1061 ?x6202))))
 ))
 (let (($x3723 (exc_halt_t 4)))
 (let (($x772 (exc_halt_s 6)))
 (let (($x10342 (= $x772 $x3723)))
 (let (($x9405 (forall ((n (_ BitVec 6)) )(let ((?x7754 (stack_t x_0 x_1 x_2 x_3 4 n)))
 (let ((?x5492 (stack_s x_0 x_1 x_2 x_3 6 n)))
 (let (($x11810 (= ?x5492 ?x7754)))
 (let ((?x1098 (sc_t 4)))
 (let (($x8721 (bvsle ?x1098 n)))
 (or $x8721 $x11810)))))))
 ))
 (let ((?x1098 (sc_t 4)))
 (let ((?x926 (sc_s 6)))
 (let (($x7792 (= ?x926 ?x1098)))
 (let (($x6673 (not (and $x7792 $x9405 $x10342 $x4100))))
 (let ((?x9937 (used_gas_t x_0 x_1 x_2 x_3 0)))
 (let ((?x9065 (used_gas_s x_0 x_1 x_2 x_3 0)))
 (let (($x3803 (= ?x9065 ?x9937)))
 (let (($x3991 (forall ((w (_ BitVec 256)) )(let ((?x6668 (storage_t x_0 x_1 x_2 x_3 0 w)))
 (let ((?x7080 (storage_s x_0 x_1 x_2 x_3 0 w)))
 (= ?x7080 ?x6668))))
 ))
 (let (($x56 (exc_halt_t 0)))
 (let (($x57 (exc_halt_s 0)))
 (let (($x58 (= $x57 $x56)))
 (let (($x9141 (forall ((n (_ BitVec 6)) )(let ((?x63 (sc_t 0)))
 (let (($x7453 (bvsle ?x63 n)))
 (let ((?x3572 (stack_t x_0 x_1 x_2 x_3 0 n)))
 (let ((?x5284 (stack_s x_0 x_1 x_2 x_3 0 n)))
 (let (($x6536 (= ?x5284 ?x3572)))
 (or $x6536 $x7453)))))))
 ))
 (let ((?x63 (sc_t 0)))
 (let ((?x72 (sc_s 0)))
 (let (($x73 (= ?x72 ?x63)))
 (let (($x7275 (= $x3723 (or (exc_halt_t 3) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_t 3))))))))
 (let (($x756 (forall ((w (_ BitVec 256)) )(let ((?x3276 (storage_t x_0 x_1 x_2 x_3 3 w)))
 (let ((?x6202 (storage_t x_0 x_1 x_2 x_3 4 w)))
 (= ?x6202 ?x3276))))
 ))
 (let (($x5267 (forall ((n (_ BitVec 6)) )(let ((?x11058 (stack_t x_0 x_1 x_2 x_3 3 n)))
 (let ((?x7754 (stack_t x_0 x_1 x_2 x_3 4 n)))
 (let (($x7698 (= ?x7754 ?x11058)))
 (let ((?x6438 (sc_t 3)))
 (let ((?x9210 (bvadd (_ bv62 6) ?x6438)))
 (let (($x6607 (bvsle ?x9210 n)))
 (or $x6607 $x7698))))))))
 ))
 (let ((?x6438 (sc_t 3)))
 (let ((?x9822 (bvadd (_ bv63 6) ?x6438)))
 (let (($x5696 (= ?x1098 ?x9822)))
 (let ((?x6010 (used_gas_t x_0 x_1 x_2 x_3 4)))
 (let (($x7252 (= ?x6010 (+ 3 (used_gas_t x_0 x_1 x_2 x_3 3)))))
 (let ((?x9210 (bvadd (_ bv62 6) ?x6438)))
 (let ((?x9171 (stack_t x_0 x_1 x_2 x_3 3 ?x9210)))
 (let ((?x7356 (stack_t x_0 x_1 x_2 x_3 3 ?x9822)))
 (let ((?x10239 (bvadd (_ bv63 6) ?x1098)))
 (let ((?x9277 (stack_t x_0 x_1 x_2 x_3 4 ?x10239)))
 (let (($x8259 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_t 2)))) (_ bv0 1)))))
 (let (($x5252 (exc_halt_t 2)))
 (let (($x6783 (exc_halt_t 3)))
 (let (($x7093 (forall ((w (_ BitVec 256)) )(let ((?x11019 (storage_t x_0 x_1 x_2 x_3 2 w)))
 (let ((?x3276 (storage_t x_0 x_1 x_2 x_3 3 w)))
 (= ?x3276 ?x11019))))
 ))
 (let (($x5066 (forall ((n (_ BitVec 6)) )(let ((?x3399 (stack_t x_0 x_1 x_2 x_3 2 n)))
 (let ((?x11058 (stack_t x_0 x_1 x_2 x_3 3 n)))
 (let (($x2919 (= ?x11058 ?x3399)))
 (or $x2919 (bvsle (bvadd (_ bv59 6) (sc_t 2)) n))))))
 ))
 (let (($x8645 (= ?x6438 (bvadd (_ bv1 6) (sc_t 2)))))
 (let ((?x2338 (used_gas_t x_0 x_1 x_2 x_3 3)))
 (let (($x7788 (= ?x2338 (+ 3 (used_gas_t x_0 x_1 x_2 x_3 2)))))
 (let ((?x2714 (sc_t 2)))
 (let ((?x408 (bvadd (_ bv63 6) ?x2714)))
 (let ((?x2609 (stack_t x_0 x_1 x_2 x_3 2 ?x408)))
 (let ((?x7799 (bvadd (_ bv62 6) ?x2714)))
 (let ((?x7283 (stack_t x_0 x_1 x_2 x_3 2 ?x7799)))
 (let ((?x8432 (bvadd (_ bv61 6) ?x2714)))
 (let ((?x8465 (stack_t x_0 x_1 x_2 x_3 2 ?x8432)))
 (let ((?x10086 (bvadd (_ bv60 6) ?x2714)))
 (let ((?x10260 (stack_t x_0 x_1 x_2 x_3 2 ?x10086)))
 (let ((?x50 (bvadd (_ bv59 6) ?x2714)))
 (let ((?x10502 (stack_t x_0 x_1 x_2 x_3 2 ?x50)))
 (let (($x3508 (exc_halt_t 1)))
 (let (($x1340 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_t 1)))) (_ bv0 1)))))
 (let (($x3105 (= $x5252 (or $x1340 $x3508 (not (bvsle (_ bv0 6) (bvadd (_ bv61 6) (sc_t 1))))))))
 (let (($x6301 (forall ((w (_ BitVec 256)) )(let ((?x6132 (storage_t x_0 x_1 x_2 x_3 1 w)))
 (let ((?x11019 (storage_t x_0 x_1 x_2 x_3 2 w)))
 (= ?x11019 ?x6132))))
 ))
 (let (($x6424 (forall ((n (_ BitVec 6)) )(let ((?x7231 (stack_t x_0 x_1 x_2 x_3 1 n)))
 (let ((?x3399 (stack_t x_0 x_1 x_2 x_3 2 n)))
 (let (($x9996 (= ?x3399 ?x7231)))
 (let ((?x8347 (sc_t 1)))
 (let ((?x7903 (bvadd (_ bv61 6) ?x8347)))
 (let (($x4168 (bvsle ?x7903 n)))
 (or $x4168 $x9996))))))))
 ))
 (let (($x2756 (= ?x2714 (bvadd (_ bv1 6) (sc_t 1)))))
 (let ((?x1798 (used_gas_t x_0 x_1 x_2 x_3 2)))
 (let (($x7706 (= ?x1798 (+ 3 (used_gas_t x_0 x_1 x_2 x_3 1)))))
 (let ((?x8347 (sc_t 1)))
 (let ((?x5756 (bvadd (_ bv63 6) ?x8347)))
 (let ((?x10553 (stack_t x_0 x_1 x_2 x_3 1 ?x5756)))
 (let (($x5574 (= (stack_t x_0 x_1 x_2 x_3 2 ?x5756) ?x10553)))
 (let ((?x256 (bvadd (_ bv62 6) ?x8347)))
 (let ((?x9793 (stack_t x_0 x_1 x_2 x_3 1 ?x256)))
 (let (($x3743 (= (stack_t x_0 x_1 x_2 x_3 2 ?x256) ?x9793)))
 (let ((?x7903 (bvadd (_ bv61 6) ?x8347)))
 (let ((?x221 (stack_t x_0 x_1 x_2 x_3 1 ?x7903)))
 (let (($x8880 (= (stack_t x_0 x_1 x_2 x_3 2 ?x7903) ?x221)))
 (let (($x1337 (= ?x2609 ?x221)))
 (let (($x6339 (forall ((w (_ BitVec 256)) )(let ((?x6668 (storage_t x_0 x_1 x_2 x_3 0 w)))
 (let ((?x6132 (storage_t x_0 x_1 x_2 x_3 1 w)))
 (= ?x6132 ?x6668))))
 ))
 (let (($x8392 (forall ((n (_ BitVec 6)) )(let ((?x3572 (stack_t x_0 x_1 x_2 x_3 0 n)))
 (let ((?x7231 (stack_t x_0 x_1 x_2 x_3 1 n)))
 (let (($x8587 (= ?x7231 ?x3572)))
 (let ((?x63 (sc_t 0)))
 (let ((?x7387 (bvadd (_ bv62 6) ?x63)))
 (let (($x2346 (bvsle ?x7387 n)))
 (or $x2346 $x8587))))))))
 ))
 (let ((?x7151 (used_gas_t x_0 x_1 x_2 x_3 1)))
 (let (($x3847 (= ?x7151 (+ 3 ?x9937))))
 (let ((?x7387 (bvadd (_ bv62 6) ?x63)))
 (let ((?x11412 (stack_t x_0 x_1 x_2 x_3 0 ?x7387)))
 (let (($x6807 (= ?x10553 ?x11412)))
 (let (($x6759 (= $x772 (or (exc_halt_s 5) (not (bvsle (_ bv0 6) (bvadd (_ bv61 6) (sc_s 5))))))))
 (let (($x471 (forall ((w (_ BitVec 256)) )(let ((?x6700 (storage_s x_0 x_1 x_2 x_3 5 w)))
 (let ((?x1061 (storage_s x_0 x_1 x_2 x_3 6 w)))
 (= ?x1061 ?x6700))))
 ))
 (let (($x11149 (forall ((n (_ BitVec 6)) )(let ((?x7007 (stack_s x_0 x_1 x_2 x_3 5 n)))
 (let ((?x5492 (stack_s x_0 x_1 x_2 x_3 6 n)))
 (let (($x8355 (= ?x5492 ?x7007)))
 (or (bvsle (bvadd (_ bv61 6) (sc_s 5)) n) $x8355)))))
 ))
 (let ((?x4319 (sc_s 5)))
 (let (($x1737 (= ?x926 ?x4319)))
 (let (($x2333 (= (used_gas_s x_0 x_1 x_2 x_3 6) (+ 3 (used_gas_s x_0 x_1 x_2 x_3 5)))))
 (let ((?x8322 (bvadd (_ bv62 6) ?x4319)))
 (let ((?x2597 (stack_s x_0 x_1 x_2 x_3 5 ?x8322)))
 (let ((?x2005 (bvadd (_ bv62 6) ?x926)))
 (let ((?x11634 (stack_s x_0 x_1 x_2 x_3 6 ?x2005)))
 (let ((?x3503 (bvadd (_ bv63 6) ?x4319)))
 (let ((?x6473 (stack_s x_0 x_1 x_2 x_3 5 ?x3503)))
 (let ((?x9625 (bvadd (_ bv61 6) ?x4319)))
 (let ((?x9029 (stack_s x_0 x_1 x_2 x_3 5 ?x9625)))
 (let ((?x2259 (bvadd (_ bv63 6) ?x926)))
 (let ((?x7160 (stack_s x_0 x_1 x_2 x_3 6 ?x2259)))
 (let (($x11317 (exc_halt_s 5)))
 (let (($x5022 (= $x11317 (or (exc_halt_s 4) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 4))))))))
 (let (($x1426 (forall ((w (_ BitVec 256)) )(let ((?x4772 (storage_s x_0 x_1 x_2 x_3 4 w)))
 (let ((?x6700 (storage_s x_0 x_1 x_2 x_3 5 w)))
 (= ?x6700 ?x4772))))
 ))
 (let (($x1140 (forall ((n (_ BitVec 6)) )(let ((?x4305 (sc_s 4)))
 (let ((?x9984 (bvadd (_ bv62 6) ?x4305)))
 (let (($x10287 (bvsle ?x9984 n)))
 (let ((?x11398 (stack_s x_0 x_1 x_2 x_3 4 n)))
 (let ((?x7007 (stack_s x_0 x_1 x_2 x_3 5 n)))
 (let (($x9781 (= ?x7007 ?x11398)))
 (or $x9781 $x10287))))))))
 ))
 (let ((?x4305 (sc_s 4)))
 (let (($x15 (= ?x4319 ?x4305)))
 (let ((?x9420 (used_gas_s x_0 x_1 x_2 x_3 5)))
 (let (($x5374 (= ?x9420 (+ 3 (used_gas_s x_0 x_1 x_2 x_3 4)))))
 (let ((?x9984 (bvadd (_ bv62 6) ?x4305)))
 (let ((?x7476 (stack_s x_0 x_1 x_2 x_3 4 ?x9984)))
 (let (($x3615 (= ?x6473 ?x7476)))
 (let (($x7172 (exc_halt_s 4)))
 (let (($x11359 (= $x7172 (or (exc_halt_s 3) (not (bvsle (_ bv0 6) (bvadd (_ bv61 6) (sc_s 3))))))))
 (let (($x396 (forall ((w (_ BitVec 256)) )(let ((?x8354 (storage_s x_0 x_1 x_2 x_3 3 w)))
 (let ((?x4772 (storage_s x_0 x_1 x_2 x_3 4 w)))
 (= ?x4772 ?x8354))))
 ))
 (let (($x9136 (forall ((n (_ BitVec 6)) )(let ((?x275 (sc_s 3)))
 (let ((?x3143 (bvadd (_ bv61 6) ?x275)))
 (let (($x9678 (bvsle ?x3143 n)))
 (let ((?x330 (stack_s x_0 x_1 x_2 x_3 3 n)))
 (let ((?x11398 (stack_s x_0 x_1 x_2 x_3 4 n)))
 (let (($x5655 (= ?x11398 ?x330)))
 (or $x5655 $x9678))))))))
 ))
 (let ((?x275 (sc_s 3)))
 (let (($x10620 (= ?x4305 ?x275)))
 (let ((?x4039 (used_gas_s x_0 x_1 x_2 x_3 4)))
 (let (($x1383 (= ?x4039 (+ 3 (used_gas_s x_0 x_1 x_2 x_3 3)))))
 (let ((?x9812 (bvadd (_ bv63 6) ?x275)))
 (let ((?x7075 (stack_s x_0 x_1 x_2 x_3 3 ?x9812)))
 (let ((?x3143 (bvadd (_ bv61 6) ?x275)))
 (let ((?x7635 (stack_s x_0 x_1 x_2 x_3 3 ?x3143)))
 (let ((?x805 (bvadd (_ bv63 6) ?x4305)))
 (let ((?x609 (stack_s x_0 x_1 x_2 x_3 4 ?x805)))
 (let (($x292 (exc_halt_s 3)))
 (let (($x6649 (= $x292 (or (exc_halt_s 2) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 2))))))))
 (let (($x674 (forall ((w (_ BitVec 256)) )(let ((?x3226 (storage_s x_0 x_1 x_2 x_3 2 w)))
 (let ((?x8354 (storage_s x_0 x_1 x_2 x_3 3 w)))
 (= ?x8354 ?x3226))))
 ))
 (let (($x1740 (forall ((n (_ BitVec 6)) )(let ((?x218 (sc_s 2)))
 (let ((?x2513 (bvadd (_ bv62 6) ?x218)))
 (let (($x4614 (bvsle ?x2513 n)))
 (let ((?x11480 (stack_s x_0 x_1 x_2 x_3 2 n)))
 (let ((?x330 (stack_s x_0 x_1 x_2 x_3 3 n)))
 (let (($x3417 (= ?x330 ?x11480)))
 (or $x3417 $x4614))))))))
 ))
 (let ((?x218 (sc_s 2)))
 (let ((?x7855 (bvadd (_ bv63 6) ?x218)))
 (let (($x6590 (= ?x275 ?x7855)))
 (let ((?x5099 (used_gas_s x_0 x_1 x_2 x_3 3)))
 (let (($x9081 (= ?x5099 (+ 3 (used_gas_s x_0 x_1 x_2 x_3 2)))))
 (let ((?x2513 (bvadd (_ bv62 6) ?x218)))
 (let ((?x10444 (stack_s x_0 x_1 x_2 x_3 2 ?x2513)))
 (let ((?x7071 (stack_s x_0 x_1 x_2 x_3 2 ?x7855)))
 (let (($x8154 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 1)))) (_ bv0 1)))))
 (let (($x189 (exc_halt_s 1)))
 (let (($x247 (exc_halt_s 2)))
 (let (($x6084 (forall ((w (_ BitVec 256)) )(let ((?x1172 (storage_s x_0 x_1 x_2 x_3 1 w)))
 (let ((?x3226 (storage_s x_0 x_1 x_2 x_3 2 w)))
 (= ?x3226 ?x1172))))
 ))
 (let (($x11491 (forall ((n (_ BitVec 6)) )(let ((?x420 (stack_s x_0 x_1 x_2 x_3 1 n)))
 (let ((?x11480 (stack_s x_0 x_1 x_2 x_3 2 n)))
 (let (($x241 (= ?x11480 ?x420)))
 (or $x241 (bvsle (bvadd (_ bv59 6) (sc_s 1)) n))))))
 ))
 (let (($x1362 (= ?x218 (bvadd (_ bv1 6) (sc_s 1)))))
 (let ((?x2786 (used_gas_s x_0 x_1 x_2 x_3 2)))
 (let (($x9583 (= ?x2786 (+ 3 (used_gas_s x_0 x_1 x_2 x_3 1)))))
 (let ((?x154 (sc_s 1)))
 (let ((?x2245 (bvadd (_ bv63 6) ?x154)))
 (let ((?x162 (stack_s x_0 x_1 x_2 x_3 1 ?x2245)))
 (let (($x3359 (= (stack_s x_0 x_1 x_2 x_3 2 ?x2245) ?x162)))
 (let ((?x9962 (bvadd (_ bv62 6) ?x154)))
 (let ((?x7257 (stack_s x_0 x_1 x_2 x_3 1 ?x9962)))
 (let (($x8787 (= (stack_s x_0 x_1 x_2 x_3 2 ?x9962) ?x7257)))
 (let ((?x164 (bvadd (_ bv61 6) ?x154)))
 (let ((?x7756 (stack_s x_0 x_1 x_2 x_3 1 ?x164)))
 (let (($x5466 (= (stack_s x_0 x_1 x_2 x_3 2 ?x164) ?x7756)))
 (let ((?x10749 (bvadd (_ bv60 6) ?x154)))
 (let ((?x11776 (stack_s x_0 x_1 x_2 x_3 1 ?x10749)))
 (let ((?x5091 (bvadd (_ bv59 6) ?x154)))
 (let ((?x6943 (stack_s x_0 x_1 x_2 x_3 1 ?x5091)))
 (let (($x10528 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x72))) (_ bv0 1)))))
 (let (($x11075 (forall ((w (_ BitVec 256)) )(let ((?x7080 (storage_s x_0 x_1 x_2 x_3 0 w)))
 (let ((?x1172 (storage_s x_0 x_1 x_2 x_3 1 w)))
 (= ?x1172 ?x7080))))
 ))
 (let (($x1481 (forall ((n (_ BitVec 6)) )(let ((?x5284 (stack_s x_0 x_1 x_2 x_3 0 n)))
 (let ((?x420 (stack_s x_0 x_1 x_2 x_3 1 n)))
 (let (($x2601 (= ?x420 ?x5284)))
 (or $x2601 (bvsle (bvadd (_ bv61 6) (sc_s 0)) n))))))
 ))
 (let (($x11639 (= ?x154 (bvadd (_ bv1 6) ?x72))))
 (let ((?x11596 (used_gas_s x_0 x_1 x_2 x_3 1)))
 (let (($x5212 (= ?x11596 (+ 3 ?x9065))))
 (let ((?x190 (bvadd (_ bv63 6) ?x72)))
 (let ((?x7983 (stack_s x_0 x_1 x_2 x_3 0 ?x190)))
 (let (($x3590 (= (stack_s x_0 x_1 x_2 x_3 1 ?x190) ?x7983)))
 (let ((?x531 (bvadd (_ bv62 6) ?x72)))
 (let ((?x6287 (stack_s x_0 x_1 x_2 x_3 0 ?x531)))
 (let (($x4060 (= (stack_s x_0 x_1 x_2 x_3 1 ?x531) ?x6287)))
 (let ((?x1766 (bvadd (_ bv61 6) ?x72)))
 (let ((?x6057 (stack_s x_0 x_1 x_2 x_3 0 ?x1766)))
 (let (($x7207 (= (stack_s x_0 x_1 x_2 x_3 1 ?x1766) ?x6057)))
 (let (($x2162 (forall ((w (_ BitVec 256)) )(let ((?x7080 (storage_s x_0 x_1 x_2 x_3 0 w)))
 (= ?x7080 (_ bv0 256))))
 ))
 (let (($x7412 (= ?x9065 0)))
 (let (($x6901 (not $x57)))
 (let (($x5404 (= (stack_s x_0 x_1 x_2 x_3 0 (_ bv3 6)) x_3)))
 (let (($x1304 (= (stack_s x_0 x_1 x_2 x_3 0 (_ bv2 6)) x_2)))
 (let (($x9104 (= (stack_s x_0 x_1 x_2 x_3 0 (_ bv1 6)) x_1)))
 (let (($x10281 (= (stack_s x_0 x_1 x_2 x_3 0 (_ bv0 6)) x_0)))
 (let (($x289 (= ?x72 (_ bv4 6))))
 (and $x289 $x10281 $x9104 $x1304 $x5404 $x6901 $x7412 $x2162 (= ?x162 ?x6057) $x7207 $x4060 $x3590 $x5212 $x11639 $x1481 $x11075 (= $x189 (or $x57 $x10528 (not (bvsle (_ bv0 6) ?x1766)))) (= ?x7071 ?x6943) (= (stack_s x_0 x_1 x_2 x_3 2 ?x5091) ?x6943) (= (stack_s x_0 x_1 x_2 x_3 2 ?x10749) ?x11776) $x5466 $x8787 $x3359 $x9583 $x1362 $x11491 $x6084 (= $x247 (or $x189 (not (bvsle (_ bv0 6) ?x5091)) $x8154)) (= ?x7075 (bvadd ?x7071 ?x10444)) $x9081 $x6590 $x1740 $x674 $x6649 (= ?x609 ?x7635) (= (stack_s x_0 x_1 x_2 x_3 4 (bvadd (_ bv61 6) ?x4305)) ?x7075) (= ?x7476 (stack_s x_0 x_1 x_2 x_3 3 (bvadd (_ bv62 6) ?x275))) $x1383 $x10620 $x9136 $x396 $x11359 $x3615 (= ?x2597 ?x609) $x5374 $x15 $x1140 $x1426 $x5022 (= ?x7160 ?x9029) (= (stack_s x_0 x_1 x_2 x_3 6 (bvadd (_ bv61 6) ?x926)) ?x6473) (= ?x11634 ?x2597) $x2333 $x1737 $x11149 $x471 $x6759 $x6807 (= ?x9793 (stack_t x_0 x_1 x_2 x_3 0 (bvadd (_ bv63 6) ?x63))) $x3847 (= ?x8347 ?x63) $x8392 $x6339 (= $x3508 (or $x56 (not (bvsle (_ bv0 6) ?x7387)))) $x1337 $x8880 $x3743 $x5574 $x7706 $x2756 $x6424 $x6301 $x3105 (= ?x7356 ?x10502) (= (stack_t x_0 x_1 x_2 x_3 3 ?x50) ?x10502) (= (stack_t x_0 x_1 x_2 x_3 3 ?x10086) ?x10260) (= (stack_t x_0 x_1 x_2 x_3 3 ?x8432) ?x8465) (= (stack_t x_0 x_1 x_2 x_3 3 ?x7799) ?x7283) (= (stack_t x_0 x_1 x_2 x_3 3 ?x408) ?x2609) $x7788 $x8645 $x5066 $x7093 (= $x6783 (or $x5252 (not (bvsle (_ bv0 6) ?x50)) $x8259)) (= ?x9277 (bvadd ?x7356 ?x9171)) $x7252 $x5696 $x5267 $x756 $x7275 $x73 $x9141 $x58 $x3991 $x3803 $x6673))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
 )
(check-sat)
(get-proof)
