; DUP3 PUSH 0x02 DUP2 GT ISZERO => DUP3 PUSH 0x03 DUP5 LT
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
 (exists ((x_0 (_ BitVec 256)) (x_1 (_ BitVec 256)) (x_2 (_ BitVec 256)) )(let (($x2819 (forall ((w (_ BitVec 256)) )(let ((?x183 (storage_t x_0 x_1 x_2 4 w)))
 (let ((?x2399 (storage_s x_0 x_1 x_2 5 w)))
 (= ?x2399 ?x183))))
 ))
 (let (($x7722 (exc_halt_t 4)))
 (let (($x11317 (exc_halt_s 5)))
 (let (($x10519 (= $x11317 $x7722)))
 (let (($x5614 (forall ((n (_ BitVec 6)) )(let ((?x5748 (stack_t x_0 x_1 x_2 4 n)))
 (let ((?x2086 (stack_s x_0 x_1 x_2 5 n)))
 (let (($x8120 (= ?x2086 ?x5748)))
 (let ((?x11631 (sc_t 4)))
 (let (($x3774 (bvsle ?x11631 n)))
 (or $x3774 $x8120)))))))
 ))
 (let ((?x11631 (sc_t 4)))
 (let ((?x4319 (sc_s 5)))
 (let (($x7416 (= ?x4319 ?x11631)))
 (let ((?x136 (used_gas_t x_0 x_1 x_2 0)))
 (let ((?x5127 (used_gas_s x_0 x_1 x_2 0)))
 (let (($x3688 (= ?x5127 ?x136)))
 (let (($x10648 (forall ((w (_ BitVec 256)) )(let ((?x9986 (storage_t x_0 x_1 x_2 0 w)))
 (let ((?x6876 (storage_s x_0 x_1 x_2 0 w)))
 (= ?x6876 ?x9986))))
 ))
 (let (($x56 (exc_halt_t 0)))
 (let (($x57 (exc_halt_s 0)))
 (let (($x58 (= $x57 $x56)))
 (let (($x9272 (forall ((n (_ BitVec 6)) )(let ((?x160 (stack_t x_0 x_1 x_2 0 n)))
 (let ((?x8095 (stack_s x_0 x_1 x_2 0 n)))
 (let (($x9503 (= ?x8095 ?x160)))
 (let ((?x63 (sc_t 0)))
 (let (($x5499 (bvsle ?x63 n)))
 (or $x5499 $x9503)))))))
 ))
 (let ((?x63 (sc_t 0)))
 (let ((?x72 (sc_s 0)))
 (let (($x73 (= ?x72 ?x63)))
 (let (($x8059 (= $x7722 (or (exc_halt_t 3) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_t 3))))))))
 (let (($x1191 (forall ((w (_ BitVec 256)) )(let ((?x6699 (storage_t x_0 x_1 x_2 3 w)))
 (let ((?x183 (storage_t x_0 x_1 x_2 4 w)))
 (= ?x183 ?x6699))))
 ))
 (let (($x2081 (forall ((n (_ BitVec 6)) )(let ((?x11248 (stack_t x_0 x_1 x_2 3 n)))
 (let ((?x5748 (stack_t x_0 x_1 x_2 4 n)))
 (or (bvsle (bvadd (_ bv62 6) (sc_t 3)) n) (= ?x5748 ?x11248)))))
 ))
 (let (($x7870 (= (used_gas_t x_0 x_1 x_2 4) (+ 3 (used_gas_t x_0 x_1 x_2 3)))))
 (let ((?x6438 (sc_t 3)))
 (let ((?x7970 (bvadd (_ bv63 6) ?x6438)))
 (let ((?x3715 (stack_t x_0 x_1 x_2 3 ?x7970)))
 (let ((?x9596 (ite (bvule (stack_t x_0 x_1 x_2 3 (bvadd (_ bv62 6) ?x6438)) ?x3715) (_ bv0 256) (_ bv1 256))))
 (let (($x10268 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_t 2)))) (_ bv0 1)))))
 (let (($x2163 (exc_halt_t 2)))
 (let (($x6783 (exc_halt_t 3)))
 (let (($x3153 (forall ((w (_ BitVec 256)) )(let ((?x5606 (storage_t x_0 x_1 x_2 2 w)))
 (let ((?x6699 (storage_t x_0 x_1 x_2 3 w)))
 (= ?x6699 ?x5606))))
 ))
 (let (($x11026 (forall ((n (_ BitVec 6)) )(let ((?x8849 (stack_t x_0 x_1 x_2 2 n)))
 (let ((?x11248 (stack_t x_0 x_1 x_2 3 n)))
 (let (($x1455 (= ?x11248 ?x8849)))
 (or $x1455 (bvsle (bvadd (_ bv59 6) (sc_t 2)) n))))))
 ))
 (let (($x3148 (= ?x6438 (bvadd (_ bv1 6) (sc_t 2)))))
 (let ((?x5579 (used_gas_t x_0 x_1 x_2 3)))
 (let ((?x2714 (sc_t 2)))
 (let ((?x9853 (bvadd (_ bv63 6) ?x2714)))
 (let ((?x6495 (stack_t x_0 x_1 x_2 2 ?x9853)))
 (let ((?x1572 (bvadd (_ bv62 6) ?x2714)))
 (let ((?x4876 (stack_t x_0 x_1 x_2 2 ?x1572)))
 (let ((?x6087 (bvadd (_ bv61 6) ?x2714)))
 (let ((?x3745 (stack_t x_0 x_1 x_2 2 ?x6087)))
 (let ((?x146 (bvadd (_ bv60 6) ?x2714)))
 (let ((?x7391 (stack_t x_0 x_1 x_2 2 ?x146)))
 (let ((?x1164 (bvadd (_ bv59 6) ?x2714)))
 (let ((?x4343 (stack_t x_0 x_1 x_2 2 ?x1164)))
 (let (($x9396 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_t 1)))) (_ bv0 1)))))
 (let (($x3508 (exc_halt_t 1)))
 (let (($x6429 (= $x2163 (or $x3508 $x9396))))
 (let (($x729 (forall ((w (_ BitVec 256)) )(let ((?x8390 (storage_t x_0 x_1 x_2 1 w)))
 (let ((?x5606 (storage_t x_0 x_1 x_2 2 w)))
 (= ?x5606 ?x8390))))
 ))
 (let (($x7574 (forall ((n (_ BitVec 6)) )(let ((?x7505 (stack_t x_0 x_1 x_2 1 n)))
 (let ((?x8849 (stack_t x_0 x_1 x_2 2 n)))
 (let (($x5537 (= ?x8849 ?x7505)))
 (let ((?x8347 (sc_t 1)))
 (let (($x142 (bvsle ?x8347 n)))
 (or $x142 $x5537)))))))
 ))
 (let (($x134 (= ?x2714 (bvadd (_ bv1 6) (sc_t 1)))))
 (let ((?x9856 (used_gas_t x_0 x_1 x_2 2)))
 (let (($x4650 (= ?x9856 (+ 3 (used_gas_t x_0 x_1 x_2 1)))))
 (let (($x3193 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x63))) (_ bv0 1)))))
 (let (($x9202 (forall ((w (_ BitVec 256)) )(let ((?x9986 (storage_t x_0 x_1 x_2 0 w)))
 (let ((?x8390 (storage_t x_0 x_1 x_2 1 w)))
 (= ?x8390 ?x9986))))
 ))
 (let (($x7188 (forall ((n (_ BitVec 6)) )(let ((?x160 (stack_t x_0 x_1 x_2 0 n)))
 (let ((?x7505 (stack_t x_0 x_1 x_2 1 n)))
 (let (($x9923 (= ?x7505 ?x160)))
 (or (bvsle (bvadd (_ bv61 6) (sc_t 0)) n) $x9923)))))
 ))
 (let ((?x8347 (sc_t 1)))
 (let (($x11604 (= ?x8347 (bvadd (_ bv1 6) ?x63))))
 (let ((?x1905 (used_gas_t x_0 x_1 x_2 1)))
 (let (($x3655 (= ?x1905 (+ 3 ?x136))))
 (let ((?x6761 (bvadd (_ bv63 6) ?x63)))
 (let ((?x1280 (stack_t x_0 x_1 x_2 0 ?x6761)))
 (let (($x11699 (= (stack_t x_0 x_1 x_2 1 ?x6761) ?x1280)))
 (let (($x5739 (= (stack_t x_0 x_1 x_2 1 (bvadd (_ bv62 6) ?x63)) (stack_t x_0 x_1 x_2 0 (bvadd (_ bv62 6) ?x63)))))
 (let ((?x4148 (bvadd (_ bv61 6) ?x63)))
 (let ((?x5471 (stack_t x_0 x_1 x_2 0 ?x4148)))
 (let (($x10101 (= $x11317 (or (exc_halt_s 4) (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_s 4))))))))
 (let (($x5240 (forall ((w (_ BitVec 256)) )(let ((?x964 (storage_s x_0 x_1 x_2 4 w)))
 (let ((?x2399 (storage_s x_0 x_1 x_2 5 w)))
 (= ?x2399 ?x964))))
 ))
 (let (($x624 (forall ((n (_ BitVec 6)) )(let ((?x2738 (stack_s x_0 x_1 x_2 4 n)))
 (let ((?x2086 (stack_s x_0 x_1 x_2 5 n)))
 (let (($x10096 (= ?x2086 ?x2738)))
 (or (bvsle (bvadd (_ bv63 6) (sc_s 4)) n) $x10096)))))
 ))
 (let ((?x4305 (sc_s 4)))
 (let (($x11171 (= ?x4319 ?x4305)))
 (let (($x8241 (= (used_gas_s x_0 x_1 x_2 5) (+ 3 (used_gas_s x_0 x_1 x_2 4)))))
 (let ((?x11331 (ite (= (stack_s x_0 x_1 x_2 4 (bvadd (_ bv63 6) ?x4305)) (_ bv0 256)) (_ bv1 256) (_ bv0 256))))
 (let ((?x8409 (bvadd (_ bv63 6) ?x4319)))
 (let ((?x8957 (stack_s x_0 x_1 x_2 5 ?x8409)))
 (let (($x7172 (exc_halt_s 4)))
 (let (($x4712 (= $x7172 (or (exc_halt_s 3) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 3))))))))
 (let (($x11616 (forall ((w (_ BitVec 256)) )(let ((?x8942 (storage_s x_0 x_1 x_2 3 w)))
 (let ((?x964 (storage_s x_0 x_1 x_2 4 w)))
 (= ?x964 ?x8942))))
 ))
 (let (($x3030 (forall ((n (_ BitVec 6)) )(let ((?x11561 (stack_s x_0 x_1 x_2 3 n)))
 (let ((?x2738 (stack_s x_0 x_1 x_2 4 n)))
 (let (($x5627 (= ?x2738 ?x11561)))
 (let ((?x275 (sc_s 3)))
 (let ((?x1922 (bvadd (_ bv62 6) ?x275)))
 (let (($x2009 (bvsle ?x1922 n)))
 (or $x2009 $x5627))))))))
 ))
 (let ((?x275 (sc_s 3)))
 (let ((?x891 (bvadd (_ bv63 6) ?x275)))
 (let (($x8640 (= ?x4305 ?x891)))
 (let ((?x11815 (used_gas_s x_0 x_1 x_2 4)))
 (let (($x10538 (= ?x11815 (+ 3 (used_gas_s x_0 x_1 x_2 3)))))
 (let ((?x8045 (stack_s x_0 x_1 x_2 3 ?x891)))
 (let ((?x10513 (ite (bvule ?x8045 (stack_s x_0 x_1 x_2 3 (bvadd (_ bv62 6) ?x275))) (_ bv0 256) (_ bv1 256))))
 (let ((?x6745 (bvadd (_ bv63 6) ?x4305)))
 (let ((?x5817 (stack_s x_0 x_1 x_2 4 ?x6745)))
 (let (($x4615 (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 2))))))
 (let (($x2505 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 2)))) (_ bv0 1)))))
 (let (($x247 (exc_halt_s 2)))
 (let (($x292 (exc_halt_s 3)))
 (let (($x8002 (= $x292 (or $x247 $x2505 $x4615))))
 (let (($x7352 (forall ((w (_ BitVec 256)) )(let ((?x1568 (storage_s x_0 x_1 x_2 2 w)))
 (let ((?x8942 (storage_s x_0 x_1 x_2 3 w)))
 (= ?x8942 ?x1568))))
 ))
 (let (($x9905 (forall ((n (_ BitVec 6)) )(let ((?x11570 (stack_s x_0 x_1 x_2 2 n)))
 (let ((?x11561 (stack_s x_0 x_1 x_2 3 n)))
 (let ((?x218 (sc_s 2)))
 (let ((?x6986 (bvadd (_ bv62 6) ?x218)))
 (let (($x9254 (bvsle ?x6986 n)))
 (or $x9254 (= ?x11561 ?x11570))))))))
 ))
 (let (($x2296 (= ?x275 (bvadd (_ bv1 6) (sc_s 2)))))
 (let ((?x3930 (used_gas_s x_0 x_1 x_2 3)))
 (let (($x5162 (= ?x3930 (+ 3 (used_gas_s x_0 x_1 x_2 2)))))
 (let (($x2865 (= (stack_s x_0 x_1 x_2 3 (bvadd (_ bv63 6) (sc_s 2))) (stack_s x_0 x_1 x_2 2 (bvadd (_ bv63 6) (sc_s 2))))))
 (let ((?x218 (sc_s 2)))
 (let ((?x6986 (bvadd (_ bv62 6) ?x218)))
 (let ((?x3942 (stack_s x_0 x_1 x_2 2 ?x6986)))
 (let (($x8396 (= (stack_s x_0 x_1 x_2 3 ?x6986) ?x3942)))
 (let (($x6313 (= ?x8045 ?x3942)))
 (let (($x419 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 1)))) (_ bv0 1)))))
 (let (($x189 (exc_halt_s 1)))
 (let (($x1121 (= $x247 (or $x189 $x419))))
 (let (($x7448 (forall ((w (_ BitVec 256)) )(let ((?x1546 (storage_s x_0 x_1 x_2 1 w)))
 (let ((?x1568 (storage_s x_0 x_1 x_2 2 w)))
 (= ?x1568 ?x1546))))
 ))
 (let (($x9535 (forall ((n (_ BitVec 6)) )(let ((?x154 (sc_s 1)))
 (let (($x2887 (bvsle ?x154 n)))
 (let ((?x9403 (stack_s x_0 x_1 x_2 1 n)))
 (let ((?x11570 (stack_s x_0 x_1 x_2 2 n)))
 (let (($x469 (= ?x11570 ?x9403)))
 (or $x469 $x2887)))))))
 ))
 (let (($x2550 (= ?x218 (bvadd (_ bv1 6) (sc_s 1)))))
 (let ((?x2355 (used_gas_s x_0 x_1 x_2 2)))
 (let (($x2150 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x72))) (_ bv0 1)))))
 (let (($x759 (not (bvsle (_ bv0 6) (bvadd (_ bv61 6) ?x72)))))
 (let (($x3573 (forall ((w (_ BitVec 256)) )(let ((?x6876 (storage_s x_0 x_1 x_2 0 w)))
 (let ((?x1546 (storage_s x_0 x_1 x_2 1 w)))
 (= ?x1546 ?x6876))))
 ))
 (let (($x1449 (forall ((n (_ BitVec 6)) )(let ((?x72 (sc_s 0)))
 (let ((?x8826 (bvadd (_ bv61 6) ?x72)))
 (let (($x7037 (bvsle ?x8826 n)))
 (let ((?x8095 (stack_s x_0 x_1 x_2 0 n)))
 (let ((?x9403 (stack_s x_0 x_1 x_2 1 n)))
 (or (= ?x9403 ?x8095) $x7037)))))))
 ))
 (let ((?x154 (sc_s 1)))
 (let (($x6368 (= ?x154 (bvadd (_ bv1 6) ?x72))))
 (let ((?x1278 (used_gas_s x_0 x_1 x_2 1)))
 (let (($x4272 (= ?x1278 (+ 3 ?x5127))))
 (let ((?x4397 (bvadd (_ bv63 6) ?x72)))
 (let ((?x4536 (stack_s x_0 x_1 x_2 0 ?x4397)))
 (let ((?x4584 (bvadd (_ bv62 6) ?x72)))
 (let ((?x5214 (stack_s x_0 x_1 x_2 0 ?x4584)))
 (let ((?x8826 (bvadd (_ bv61 6) ?x72)))
 (let ((?x7566 (stack_s x_0 x_1 x_2 0 ?x8826)))
 (let (($x6058 (= (stack_s x_0 x_1 x_2 1 (bvadd (_ bv63 6) ?x154)) ?x7566)))
 (let (($x9990 (forall ((w (_ BitVec 256)) )(let ((?x6876 (storage_s x_0 x_1 x_2 0 w)))
 (= ?x6876 (_ bv0 256))))
 ))
 (let (($x5142 (= ?x5127 0)))
 (let (($x11443 (not $x57)))
 (let (($x8775 (= (stack_s x_0 x_1 x_2 0 (_ bv2 6)) x_2)))
 (let (($x2777 (= (stack_s x_0 x_1 x_2 0 (_ bv1 6)) x_1)))
 (let (($x1697 (= (stack_s x_0 x_1 x_2 0 (_ bv0 6)) x_0)))
 (let (($x5315 (= ?x72 (_ bv3 6))))
 (and $x5315 $x1697 $x2777 $x8775 $x11443 $x5142 $x9990 $x6058 (= (stack_s x_0 x_1 x_2 1 ?x8826) ?x7566) (= (stack_s x_0 x_1 x_2 1 ?x4584) ?x5214) (= (stack_s x_0 x_1 x_2 1 ?x4397) ?x4536) $x4272 $x6368 $x1449 $x3573 (= $x189 (or $x57 $x759 $x2150)) (= (stack_s x_0 x_1 x_2 2 ?x154) (_ bv2 256)) (= ?x2355 (+ 3 ?x1278)) $x2550 $x9535 $x7448 $x1121 $x6313 $x8396 $x2865 $x5162 $x2296 $x9905 $x7352 $x8002 (= ?x5817 ?x10513) $x10538 $x8640 $x3030 $x11616 $x4712 (= ?x8957 ?x11331) $x8241 $x11171 $x624 $x5240 $x10101 (= (stack_t x_0 x_1 x_2 1 (bvadd (_ bv63 6) ?x8347)) ?x5471) (= (stack_t x_0 x_1 x_2 1 ?x4148) ?x5471) $x5739 $x11699 $x3655 $x11604 $x7188 $x9202 (= $x3508 (or $x56 $x3193 (not (bvsle (_ bv0 6) ?x4148)))) (= (stack_t x_0 x_1 x_2 2 ?x8347) (_ bv3 256)) $x4650 $x134 $x7574 $x729 $x6429 (= ?x3715 ?x4343) (= (stack_t x_0 x_1 x_2 3 ?x1164) ?x4343) (= (stack_t x_0 x_1 x_2 3 ?x146) ?x7391) (= (stack_t x_0 x_1 x_2 3 ?x6087) ?x3745) (= (stack_t x_0 x_1 x_2 3 ?x1572) ?x4876) (= (stack_t x_0 x_1 x_2 3 ?x9853) ?x6495) (= ?x5579 (+ 3 ?x9856)) $x3148 $x11026 $x3153 (= $x6783 (or (not (bvsle (_ bv0 6) ?x1164)) $x2163 $x10268)) (= (stack_t x_0 x_1 x_2 4 (bvadd (_ bv63 6) ?x11631)) ?x9596) $x7870 (= ?x11631 ?x7970) $x2081 $x1191 $x8059 $x73 $x9272 $x58 $x10648 $x3688 (not (and $x7416 $x5614 $x10519 $x2819)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
 )
(check-sat)
(get-proof)