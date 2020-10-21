; PUSH 0x00 DUP3 EQ => DUP2 ISZERO
(set-option :produce-proofs true)
; 
(set-info :status unknown)
(declare-fun storage_t ((_ BitVec 256) (_ BitVec 256) Int (_ BitVec 256)) (_ BitVec 256))
(declare-fun storage_s ((_ BitVec 256) (_ BitVec 256) Int (_ BitVec 256)) (_ BitVec 256))
(declare-fun exc_halt_t (Int) Bool)
(declare-fun exc_halt_s (Int) Bool)
(declare-fun stack_t ((_ BitVec 256) (_ BitVec 256) Int (_ BitVec 6)) (_ BitVec 256))
(declare-fun stack_s ((_ BitVec 256) (_ BitVec 256) Int (_ BitVec 6)) (_ BitVec 256))
(declare-fun sc_t (Int) (_ BitVec 6))
(declare-fun sc_s (Int) (_ BitVec 6))
(declare-fun used_gas_t ((_ BitVec 256) (_ BitVec 256) Int) Int)
(declare-fun used_gas_s ((_ BitVec 256) (_ BitVec 256) Int) Int)
(assert
 (exists ((x_0 (_ BitVec 256)) (x_1 (_ BitVec 256)) )(let (($x10547 (forall ((w (_ BitVec 256)) )(let ((?x10395 (storage_t x_0 x_1 2 w)))
 (let ((?x5762 (storage_s x_0 x_1 3 w)))
 (= ?x5762 ?x10395))))
 ))
 (let (($x6842 (exc_halt_t 2)))
 (let (($x8103 (exc_halt_s 3)))
 (let (($x6920 (= $x8103 $x6842)))
 (let (($x2784 (forall ((n (_ BitVec 6)) )(let ((?x4622 (stack_t x_0 x_1 2 n)))
 (let ((?x4620 (stack_s x_0 x_1 3 n)))
 (let (($x8075 (= ?x4620 ?x4622)))
 (let ((?x9666 (sc_t 2)))
 (let (($x6374 (bvsle ?x9666 n)))
 (or $x6374 $x8075)))))))
 ))
 (let ((?x9666 (sc_t 2)))
 (let ((?x3851 (sc_s 3)))
 (let (($x1482 (= ?x3851 ?x9666)))
 (let ((?x9451 (used_gas_t x_0 x_1 0)))
 (let ((?x6619 (used_gas_s x_0 x_1 0)))
 (let (($x10396 (= ?x6619 ?x9451)))
 (let (($x7699 (forall ((w (_ BitVec 256)) )(let ((?x11232 (storage_t x_0 x_1 0 w)))
 (let ((?x9724 (storage_s x_0 x_1 0 w)))
 (= ?x9724 ?x11232))))
 ))
 (let (($x56 (exc_halt_t 0)))
 (let (($x57 (exc_halt_s 0)))
 (let (($x58 (= $x57 $x56)))
 (let (($x10859 (forall ((n (_ BitVec 6)) )(let ((?x7791 (stack_t x_0 x_1 0 n)))
 (let ((?x4938 (stack_s x_0 x_1 0 n)))
 (let (($x5246 (= ?x4938 ?x7791)))
 (let ((?x63 (sc_t 0)))
 (let (($x3918 (bvsle ?x63 n)))
 (or $x3918 $x5246)))))))
 ))
 (let ((?x63 (sc_t 0)))
 (let ((?x72 (sc_s 0)))
 (let (($x73 (= ?x72 ?x63)))
 (let (($x5178 (= $x6842 (or (exc_halt_t 1) (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_t 1))))))))
 (let (($x1295 (forall ((w (_ BitVec 256)) )(let ((?x2917 (storage_t x_0 x_1 1 w)))
 (let ((?x10395 (storage_t x_0 x_1 2 w)))
 (= ?x10395 ?x2917))))
 ))
 (let (($x6982 (forall ((n (_ BitVec 6)) )(let ((?x4135 (sc_t 1)))
 (let ((?x7017 (bvadd (_ bv63 6) ?x4135)))
 (let (($x11561 (bvsle ?x7017 n)))
 (or (= (stack_t x_0 x_1 2 n) (stack_t x_0 x_1 1 n)) $x11561)))))
 ))
 (let ((?x4135 (sc_t 1)))
 (let (($x2280 (= ?x9666 ?x4135)))
 (let (($x9063 (= (used_gas_t x_0 x_1 2) (+ 3 (used_gas_t x_0 x_1 1)))))
 (let (($x10641 (= (stack_t x_0 x_1 2 (bvadd (_ bv63 6) ?x9666)) (ite (= (stack_t x_0 x_1 1 (bvadd (_ bv63 6) ?x4135)) (_ bv0 256)) (_ bv1 256) (_ bv0 256)))))
 (let (($x9457 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x63))) (_ bv0 1)))))
 (let (($x3791 (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) ?x63)))))
 (let (($x11030 (exc_halt_t 1)))
 (let (($x4530 (= $x11030 (or $x56 $x3791 $x9457))))
 (let (($x2751 (forall ((w (_ BitVec 256)) )(let ((?x11232 (storage_t x_0 x_1 0 w)))
 (let ((?x2917 (storage_t x_0 x_1 1 w)))
 (= ?x2917 ?x11232))))
 ))
 (let (($x6097 (forall ((n (_ BitVec 6)) )(let ((?x63 (sc_t 0)))
 (let ((?x10865 (bvadd (_ bv62 6) ?x63)))
 (let (($x2729 (bvsle ?x10865 n)))
 (or (= (stack_t x_0 x_1 1 n) (stack_t x_0 x_1 0 n)) $x2729)))))
 ))
 (let (($x4431 (= ?x4135 (bvadd (_ bv1 6) ?x63))))
 (let (($x5307 (= (stack_t x_0 x_1 1 (bvadd (_ bv63 6) ?x63)) (stack_t x_0 x_1 0 (bvadd (_ bv63 6) ?x63)))))
 (let ((?x10865 (bvadd (_ bv62 6) ?x63)))
 (let ((?x5323 (stack_t x_0 x_1 0 ?x10865)))
 (let (($x9650 (= $x8103 (or (exc_halt_s 2) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 2))))))))
 (let (($x3280 (forall ((w (_ BitVec 256)) )(let ((?x6346 (storage_s x_0 x_1 2 w)))
 (let ((?x5762 (storage_s x_0 x_1 3 w)))
 (= ?x5762 ?x6346))))
 ))
 (let (($x8402 (forall ((n (_ BitVec 6)) )(let ((?x2272 (sc_s 2)))
 (let ((?x5352 (bvadd (_ bv62 6) ?x2272)))
 (let (($x3489 (bvsle ?x5352 n)))
 (or $x3489 (= (stack_s x_0 x_1 3 n) (stack_s x_0 x_1 2 n)))))))
 ))
 (let ((?x2272 (sc_s 2)))
 (let ((?x3887 (bvadd (_ bv63 6) ?x2272)))
 (let (($x8559 (= ?x3851 ?x3887)))
 (let (($x5748 (= (used_gas_s x_0 x_1 3) (+ 3 (used_gas_s x_0 x_1 2)))))
 (let ((?x3135 (stack_s x_0 x_1 2 ?x3887)))
 (let (($x4489 (= (stack_s x_0 x_1 3 (bvadd (_ bv63 6) ?x3851)) (ite (= ?x3135 (stack_s x_0 x_1 2 (bvadd (_ bv62 6) ?x2272))) (_ bv1 256) (_ bv0 256)))))
 (let (($x8780 (exc_halt_s 1)))
 (let (($x3883 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 1)))) (_ bv0 1)))))
 (let (($x10052 (exc_halt_s 2)))
 (let (($x5990 (forall ((w (_ BitVec 256)) )(let ((?x2095 (storage_s x_0 x_1 1 w)))
 (let ((?x6346 (storage_s x_0 x_1 2 w)))
 (= ?x6346 ?x2095))))
 ))
 (let (($x5058 (forall ((n (_ BitVec 6)) )(or (bvsle (bvadd (_ bv61 6) (sc_s 1)) n) (= (stack_s x_0 x_1 2 n) (stack_s x_0 x_1 1 n))))
 ))
 (let (($x4571 (= ?x2272 (bvadd (_ bv1 6) (sc_s 1)))))
 (let ((?x9740 (used_gas_s x_0 x_1 2)))
 (let (($x8411 (= (stack_s x_0 x_1 2 (bvadd (_ bv63 6) (sc_s 1))) (stack_s x_0 x_1 1 (bvadd (_ bv63 6) (sc_s 1))))))
 (let (($x6869 (= (stack_s x_0 x_1 2 (bvadd (_ bv62 6) (sc_s 1))) (stack_s x_0 x_1 1 (bvadd (_ bv62 6) (sc_s 1))))))
 (let ((?x154 (sc_s 1)))
 (let ((?x5334 (bvadd (_ bv61 6) ?x154)))
 (let ((?x10144 (stack_s x_0 x_1 1 ?x5334)))
 (let (($x11279 (= $x8780 (or $x57 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x72))) (_ bv0 1)))))))
 (let (($x6388 (forall ((w (_ BitVec 256)) )(let ((?x9724 (storage_s x_0 x_1 0 w)))
 (let ((?x2095 (storage_s x_0 x_1 1 w)))
 (= ?x2095 ?x9724))))
 ))
 (let (($x6171 (forall ((n (_ BitVec 6)) )(let ((?x72 (sc_s 0)))
 (let (($x5715 (bvsle ?x72 n)))
 (or $x5715 (= (stack_s x_0 x_1 1 n) (stack_s x_0 x_1 0 n))))))
 ))
 (let (($x11116 (= ?x154 (bvadd (_ bv1 6) ?x72))))
 (let (($x6052 (forall ((w (_ BitVec 256)) )(let ((?x9724 (storage_s x_0 x_1 0 w)))
 (= ?x9724 (_ bv0 256))))
 ))
 (let (($x6134 (= ?x6619 0)))
 (let (($x2113 (not $x57)))
 (let (($x1067 (= (stack_s x_0 x_1 0 (_ bv1 6)) x_1)))
 (let (($x8991 (= (stack_s x_0 x_1 0 (_ bv0 6)) x_0)))
 (let (($x5626 (= ?x72 (_ bv2 6))))
 (and $x5626 $x8991 $x1067 $x2113 $x6134 $x6052 (= (stack_s x_0 x_1 1 ?x72) (_ bv0 256)) (= (used_gas_s x_0 x_1 1) (+ 3 ?x6619)) $x11116 $x6171 $x6388 $x11279 (= ?x3135 ?x10144) (= (stack_s x_0 x_1 2 ?x5334) ?x10144) $x6869 $x8411 (= ?x9740 (+ 3 (used_gas_s x_0 x_1 1))) $x4571 $x5058 $x5990 (= $x10052 (or $x3883 (not (bvsle (_ bv0 6) ?x5334)) $x8780)) $x4489 $x5748 $x8559 $x8402 $x3280 $x9650 (= (stack_t x_0 x_1 1 (bvadd (_ bv63 6) ?x4135)) ?x5323) (= (stack_t x_0 x_1 1 ?x10865) ?x5323) $x5307 (= (used_gas_t x_0 x_1 1) (+ 3 ?x9451)) $x4431 $x6097 $x2751 $x4530 $x10641 $x9063 $x2280 $x6982 $x1295 $x5178 $x73 $x10859 $x58 $x7699 $x10396 (not (and $x1482 $x2784 $x6920 $x10547)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
 )
(check-sat)
(get-proof)
