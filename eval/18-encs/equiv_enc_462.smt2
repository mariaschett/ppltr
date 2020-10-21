; SWAP1 DUP2 AND SWAP2 DUP3 SWAP1 => DUP1 SWAP2 AND DUP1 SWAP3
(set-option :produce-proofs true)
; 
(set-info :status unknown)
(declare-fun storage_t ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 256)) (_ BitVec 256))
(declare-fun storage_s ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 256)) (_ BitVec 256))
(declare-fun exc_halt_t (Int) Bool)
(declare-fun exc_halt_s (Int) Bool)
(declare-fun sc_t (Int) (_ BitVec 6))
(declare-fun stack_t ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 6)) (_ BitVec 256))
(declare-fun stack_s ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 6)) (_ BitVec 256))
(declare-fun sc_s (Int) (_ BitVec 6))
(declare-fun used_gas_t ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int) Int)
(declare-fun used_gas_s ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int) Int)
(assert
 (exists ((x_0 (_ BitVec 256)) (x_1 (_ BitVec 256)) (x_2 (_ BitVec 256)) )(let (($x6686 (forall ((w (_ BitVec 256)) )(let ((?x6876 (storage_t x_0 x_1 x_2 5 w)))
 (let ((?x11725 (storage_s x_0 x_1 x_2 6 w)))
 (= ?x11725 ?x6876))))
 ))
 (let (($x886 (exc_halt_t 5)))
 (let (($x772 (exc_halt_s 6)))
 (let (($x889 (= $x772 $x886)))
 (let (($x8849 (forall ((n (_ BitVec 6)) )(let ((?x919 (sc_t 5)))
 (let (($x11300 (bvsle ?x919 n)))
 (let ((?x5127 (stack_t x_0 x_1 x_2 5 n)))
 (let ((?x7183 (stack_s x_0 x_1 x_2 6 n)))
 (let (($x7397 (= ?x7183 ?x5127)))
 (or $x7397 $x11300)))))))
 ))
 (let ((?x919 (sc_t 5)))
 (let ((?x926 (sc_s 6)))
 (let (($x929 (= ?x926 ?x919)))
 (let ((?x11719 (used_gas_t x_0 x_1 x_2 0)))
 (let ((?x7240 (used_gas_s x_0 x_1 x_2 0)))
 (let (($x5606 (= ?x7240 ?x11719)))
 (let (($x10987 (forall ((w (_ BitVec 256)) )(let ((?x7497 (storage_t x_0 x_1 x_2 0 w)))
 (let ((?x6937 (storage_s x_0 x_1 x_2 0 w)))
 (= ?x6937 ?x7497))))
 ))
 (let (($x56 (exc_halt_t 0)))
 (let (($x57 (exc_halt_s 0)))
 (let (($x58 (= $x57 $x56)))
 (let (($x4198 (forall ((n (_ BitVec 6)) )(let ((?x5893 (stack_t x_0 x_1 x_2 0 n)))
 (let ((?x1594 (stack_s x_0 x_1 x_2 0 n)))
 (let (($x530 (= ?x1594 ?x5893)))
 (let ((?x63 (sc_t 0)))
 (let (($x368 (bvsle ?x63 n)))
 (or $x368 $x530)))))))
 ))
 (let ((?x63 (sc_t 0)))
 (let ((?x72 (sc_s 0)))
 (let (($x73 (= ?x72 ?x63)))
 (let (($x9503 (= $x886 (or (exc_halt_t 4) (not (bvsle (_ bv0 6) (bvadd (_ bv60 6) (sc_t 4))))))))
 (let (($x10648 (forall ((w (_ BitVec 256)) )(let ((?x9880 (storage_t x_0 x_1 x_2 4 w)))
 (let ((?x6876 (storage_t x_0 x_1 x_2 5 w)))
 (= ?x6876 ?x9880))))
 ))
 (let (($x9986 (forall ((n (_ BitVec 6)) )(let ((?x569 (stack_t x_0 x_1 x_2 4 n)))
 (let ((?x5127 (stack_t x_0 x_1 x_2 5 n)))
 (or (bvsle (bvadd (_ bv60 6) (sc_t 4)) n) (= ?x5127 ?x569)))))
 ))
 (let ((?x11631 (sc_t 4)))
 (let (($x297 (= ?x919 ?x11631)))
 (let (($x4857 (= (used_gas_t x_0 x_1 x_2 5) (+ 3 (used_gas_t x_0 x_1 x_2 4)))))
 (let (($x689 (= (stack_t x_0 x_1 x_2 5 (bvadd (_ bv62 6) ?x919)) (stack_t x_0 x_1 x_2 4 (bvadd (_ bv62 6) ?x11631)))))
 (let (($x10867 (= (stack_t x_0 x_1 x_2 5 (bvadd (_ bv61 6) ?x919)) (stack_t x_0 x_1 x_2 4 (bvadd (_ bv61 6) ?x11631)))))
 (let ((?x9366 (bvadd (_ bv63 6) ?x11631)))
 (let ((?x9406 (stack_t x_0 x_1 x_2 4 ?x9366)))
 (let (($x7873 (= (stack_t x_0 x_1 x_2 5 (bvadd (_ bv63 6) ?x919)) (stack_t x_0 x_1 x_2 4 (bvadd (_ bv60 6) ?x11631)))))
 (let (($x6783 (exc_halt_t 3)))
 (let (($x4329 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_t 3)))) (_ bv0 1)))))
 (let (($x7722 (exc_halt_t 4)))
 (let (($x3343 (= $x7722 (or $x4329 $x6783 (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_t 3))))))))
 (let (($x5846 (forall ((w (_ BitVec 256)) )(let ((?x10290 (storage_t x_0 x_1 x_2 3 w)))
 (let ((?x9880 (storage_t x_0 x_1 x_2 4 w)))
 (= ?x9880 ?x10290))))
 ))
 (let (($x3118 (forall ((n (_ BitVec 6)) )(let ((?x5814 (stack_t x_0 x_1 x_2 3 n)))
 (let ((?x569 (stack_t x_0 x_1 x_2 4 n)))
 (let ((?x6438 (sc_t 3)))
 (let ((?x6883 (bvadd (_ bv63 6) ?x6438)))
 (let (($x4167 (bvsle ?x6883 n)))
 (or $x4167 (= ?x569 ?x5814))))))))
 ))
 (let (($x10865 (= ?x11631 (bvadd (_ bv1 6) (sc_t 3)))))
 (let ((?x4210 (used_gas_t x_0 x_1 x_2 4)))
 (let ((?x6438 (sc_t 3)))
 (let ((?x6883 (bvadd (_ bv63 6) ?x6438)))
 (let ((?x10397 (stack_t x_0 x_1 x_2 3 ?x6883)))
 (let (($x6380 (= $x6783 (or (exc_halt_t 2) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_t 2))))))))
 (let (($x539 (forall ((w (_ BitVec 256)) )(let ((?x8825 (storage_t x_0 x_1 x_2 2 w)))
 (let ((?x10290 (storage_t x_0 x_1 x_2 3 w)))
 (= ?x10290 ?x8825))))
 ))
 (let (($x11629 (forall ((n (_ BitVec 6)) )(let ((?x3204 (stack_t x_0 x_1 x_2 2 n)))
 (let ((?x5814 (stack_t x_0 x_1 x_2 3 n)))
 (let (($x9339 (= ?x5814 ?x3204)))
 (let ((?x2714 (sc_t 2)))
 (let ((?x718 (bvadd (_ bv62 6) ?x2714)))
 (let (($x4152 (bvsle ?x718 n)))
 (or $x4152 $x9339))))))))
 ))
 (let ((?x2714 (sc_t 2)))
 (let ((?x10249 (bvadd (_ bv63 6) ?x2714)))
 (let (($x7102 (= ?x6438 ?x10249)))
 (let ((?x4859 (used_gas_t x_0 x_1 x_2 3)))
 (let (($x10847 (= ?x4859 (+ 3 (used_gas_t x_0 x_1 x_2 2)))))
 (let ((?x5880 (bvor (bvnot (stack_t x_0 x_1 x_2 2 ?x10249)) (bvnot (stack_t x_0 x_1 x_2 2 (bvadd (_ bv62 6) ?x2714))))))
 (let (($x10055 (exc_halt_t 2)))
 (let (($x11449 (= $x10055 (or (exc_halt_t 1) (not (bvsle (_ bv0 6) (bvadd (_ bv61 6) (sc_t 1))))))))
 (let (($x11371 (forall ((w (_ BitVec 256)) )(let ((?x2892 (storage_t x_0 x_1 x_2 1 w)))
 (let ((?x8825 (storage_t x_0 x_1 x_2 2 w)))
 (= ?x8825 ?x2892))))
 ))
 (let (($x4443 (forall ((n (_ BitVec 6)) )(let ((?x8347 (sc_t 1)))
 (let ((?x4736 (bvadd (_ bv61 6) ?x8347)))
 (let (($x5121 (bvsle ?x4736 n)))
 (let ((?x7330 (stack_t x_0 x_1 x_2 1 n)))
 (let ((?x3204 (stack_t x_0 x_1 x_2 2 n)))
 (or (= ?x3204 ?x7330) $x5121)))))))
 ))
 (let ((?x8347 (sc_t 1)))
 (let (($x8174 (= ?x2714 ?x8347)))
 (let ((?x9948 (used_gas_t x_0 x_1 x_2 2)))
 (let (($x977 (= ?x9948 (+ 3 (used_gas_t x_0 x_1 x_2 1)))))
 (let ((?x732 (bvadd (_ bv62 6) ?x8347)))
 (let ((?x7919 (stack_t x_0 x_1 x_2 1 ?x732)))
 (let ((?x718 (bvadd (_ bv62 6) ?x2714)))
 (let ((?x3619 (stack_t x_0 x_1 x_2 2 ?x718)))
 (let (($x1274 (= ?x3619 ?x7919)))
 (let ((?x4465 (bvadd (_ bv63 6) ?x8347)))
 (let ((?x5109 (stack_t x_0 x_1 x_2 1 ?x4465)))
 (let ((?x5805 (bvadd (_ bv61 6) ?x2714)))
 (let ((?x10736 (stack_t x_0 x_1 x_2 2 ?x5805)))
 (let (($x6033 (= ?x10736 ?x5109)))
 (let ((?x4736 (bvadd (_ bv61 6) ?x8347)))
 (let ((?x2428 (stack_t x_0 x_1 x_2 1 ?x4736)))
 (let ((?x7212 (stack_t x_0 x_1 x_2 2 ?x10249)))
 (let (($x2907 (= ?x7212 ?x2428)))
 (let (($x10059 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x63))) (_ bv0 1)))))
 (let (($x2998 (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) ?x63)))))
 (let (($x3508 (exc_halt_t 1)))
 (let (($x1318 (forall ((w (_ BitVec 256)) )(let ((?x7497 (storage_t x_0 x_1 x_2 0 w)))
 (let ((?x2892 (storage_t x_0 x_1 x_2 1 w)))
 (= ?x2892 ?x7497))))
 ))
 (let (($x1854 (forall ((n (_ BitVec 6)) )(let ((?x63 (sc_t 0)))
 (let ((?x208 (bvadd (_ bv63 6) ?x63)))
 (let (($x6808 (bvsle ?x208 n)))
 (let ((?x5893 (stack_t x_0 x_1 x_2 0 n)))
 (let ((?x7330 (stack_t x_0 x_1 x_2 1 n)))
 (let (($x3353 (= ?x7330 ?x5893)))
 (or $x3353 $x6808))))))))
 ))
 (let (($x7773 (= ?x8347 (bvadd (_ bv1 6) ?x63))))
 (let ((?x9974 (used_gas_t x_0 x_1 x_2 1)))
 (let (($x8161 (= ?x9974 (+ 3 ?x11719))))
 (let ((?x208 (bvadd (_ bv63 6) ?x63)))
 (let ((?x6964 (stack_t x_0 x_1 x_2 0 ?x208)))
 (let (($x7524 (= $x772 (or (exc_halt_s 5) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 5))))))))
 (let (($x4432 (forall ((w (_ BitVec 256)) )(let ((?x4796 (storage_s x_0 x_1 x_2 5 w)))
 (let ((?x11725 (storage_s x_0 x_1 x_2 6 w)))
 (= ?x11725 ?x4796))))
 ))
 (let (($x1576 (forall ((n (_ BitVec 6)) )(let ((?x5164 (stack_s x_0 x_1 x_2 5 n)))
 (let ((?x7183 (stack_s x_0 x_1 x_2 6 n)))
 (let ((?x4319 (sc_s 5)))
 (let ((?x1762 (bvadd (_ bv62 6) ?x4319)))
 (let (($x1021 (bvsle ?x1762 n)))
 (or $x1021 (= ?x7183 ?x5164))))))))
 ))
 (let ((?x4319 (sc_s 5)))
 (let (($x7598 (= ?x926 ?x4319)))
 (let (($x4010 (= (used_gas_s x_0 x_1 x_2 6) (+ 3 (used_gas_s x_0 x_1 x_2 5)))))
 (let ((?x3925 (bvadd (_ bv63 6) ?x4319)))
 (let ((?x2690 (stack_s x_0 x_1 x_2 5 ?x3925)))
 (let (($x8065 (= (stack_s x_0 x_1 x_2 6 (bvadd (_ bv63 6) ?x926)) (stack_s x_0 x_1 x_2 5 (bvadd (_ bv62 6) ?x4319)))))
 (let (($x7547 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 4)))) (_ bv0 1)))))
 (let (($x7172 (exc_halt_s 4)))
 (let (($x11317 (exc_halt_s 5)))
 (let (($x7690 (forall ((w (_ BitVec 256)) )(let ((?x9244 (storage_s x_0 x_1 x_2 4 w)))
 (let ((?x4796 (storage_s x_0 x_1 x_2 5 w)))
 (= ?x4796 ?x9244))))
 ))
 (let (($x10789 (forall ((n (_ BitVec 6)) )(let ((?x3067 (stack_s x_0 x_1 x_2 4 n)))
 (let ((?x5164 (stack_s x_0 x_1 x_2 5 n)))
 (or (bvsle (bvadd (_ bv61 6) (sc_s 4)) n) (= ?x5164 ?x3067)))))
 ))
 (let (($x10984 (= ?x4319 (bvadd (_ bv1 6) (sc_s 4)))))
 (let ((?x7899 (used_gas_s x_0 x_1 x_2 5)))
 (let ((?x4305 (sc_s 4)))
 (let ((?x2253 (bvadd (_ bv63 6) ?x4305)))
 (let ((?x2338 (stack_s x_0 x_1 x_2 4 ?x2253)))
 (let ((?x3448 (bvadd (_ bv62 6) ?x4305)))
 (let ((?x2837 (stack_s x_0 x_1 x_2 4 ?x3448)))
 (let ((?x5681 (bvadd (_ bv61 6) ?x4305)))
 (let ((?x7054 (stack_s x_0 x_1 x_2 4 ?x5681)))
 (let (($x2921 (= $x7172 (or (exc_halt_s 3) (not (bvsle (_ bv0 6) (bvadd (_ bv61 6) (sc_s 3))))))))
 (let (($x8765 (forall ((w (_ BitVec 256)) )(let ((?x4575 (storage_s x_0 x_1 x_2 3 w)))
 (let ((?x9244 (storage_s x_0 x_1 x_2 4 w)))
 (= ?x9244 ?x4575))))
 ))
 (let (($x9833 (forall ((n (_ BitVec 6)) )(let ((?x1885 (stack_s x_0 x_1 x_2 3 n)))
 (let ((?x3067 (stack_s x_0 x_1 x_2 4 n)))
 (let (($x7612 (= ?x3067 ?x1885)))
 (or (bvsle (bvadd (_ bv61 6) (sc_s 3)) n) $x7612)))))
 ))
 (let ((?x275 (sc_s 3)))
 (let (($x2450 (= ?x4305 ?x275)))
 (let ((?x3425 (used_gas_s x_0 x_1 x_2 4)))
 (let (($x4100 (= ?x3425 (+ 3 (used_gas_s x_0 x_1 x_2 3)))))
 (let (($x292 (exc_halt_s 3)))
 (let (($x1868 (= $x292 (or (exc_halt_s 2) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 2))))))))
 (let (($x6260 (forall ((w (_ BitVec 256)) )(let ((?x10004 (storage_s x_0 x_1 x_2 2 w)))
 (let ((?x4575 (storage_s x_0 x_1 x_2 3 w)))
 (= ?x4575 ?x10004))))
 ))
 (let (($x8516 (forall ((n (_ BitVec 6)) )(let ((?x1413 (stack_s x_0 x_1 x_2 2 n)))
 (let ((?x1885 (stack_s x_0 x_1 x_2 3 n)))
 (let (($x6202 (= ?x1885 ?x1413)))
 (let ((?x218 (sc_s 2)))
 (let ((?x7822 (bvadd (_ bv62 6) ?x218)))
 (let (($x3245 (bvsle ?x7822 n)))
 (or $x3245 $x6202))))))))
 ))
 (let ((?x218 (sc_s 2)))
 (let ((?x6729 (bvadd (_ bv63 6) ?x218)))
 (let (($x11085 (= ?x275 ?x6729)))
 (let ((?x3988 (used_gas_s x_0 x_1 x_2 3)))
 (let (($x6685 (= ?x3988 (+ 3 (used_gas_s x_0 x_1 x_2 2)))))
 (let ((?x11226 (bvor (bvnot (stack_s x_0 x_1 x_2 2 ?x6729)) (bvnot (stack_s x_0 x_1 x_2 2 (bvadd (_ bv62 6) ?x218))))))
 (let ((?x11810 (bvadd (_ bv63 6) ?x275)))
 (let ((?x10486 (stack_s x_0 x_1 x_2 3 ?x11810)))
 (let (($x2756 (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 1))))))
 (let (($x1680 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 1)))) (_ bv0 1)))))
 (let (($x189 (exc_halt_s 1)))
 (let (($x247 (exc_halt_s 2)))
 (let (($x3772 (= $x247 (or $x189 $x1680 $x2756))))
 (let (($x3106 (forall ((w (_ BitVec 256)) )(let ((?x9764 (storage_s x_0 x_1 x_2 1 w)))
 (let ((?x10004 (storage_s x_0 x_1 x_2 2 w)))
 (= ?x10004 ?x9764))))
 ))
 (let (($x3743 (forall ((n (_ BitVec 6)) )(let ((?x154 (sc_s 1)))
 (let ((?x6807 (bvadd (_ bv62 6) ?x154)))
 (let (($x11512 (bvsle ?x6807 n)))
 (let ((?x5710 (stack_s x_0 x_1 x_2 1 n)))
 (let ((?x1413 (stack_s x_0 x_1 x_2 2 n)))
 (or (= ?x1413 ?x5710) $x11512)))))))
 ))
 (let (($x430 (= ?x218 (bvadd (_ bv1 6) (sc_s 1)))))
 (let ((?x1145 (used_gas_s x_0 x_1 x_2 2)))
 (let (($x4072 (= ?x1145 (+ 3 (used_gas_s x_0 x_1 x_2 1)))))
 (let ((?x154 (sc_s 1)))
 (let ((?x7935 (bvadd (_ bv63 6) ?x154)))
 (let ((?x3296 (stack_s x_0 x_1 x_2 1 ?x7935)))
 (let ((?x6807 (bvadd (_ bv62 6) ?x154)))
 (let ((?x9793 (stack_s x_0 x_1 x_2 1 ?x6807)))
 (let ((?x2597 (stack_s x_0 x_1 x_2 2 ?x6729)))
 (let (($x9269 (= ?x2597 ?x9793)))
 (let (($x6187 (forall ((w (_ BitVec 256)) )(let ((?x6937 (storage_s x_0 x_1 x_2 0 w)))
 (let ((?x9764 (storage_s x_0 x_1 x_2 1 w)))
 (= ?x9764 ?x6937))))
 ))
 (let (($x3417 (forall ((n (_ BitVec 6)) )(let ((?x72 (sc_s 0)))
 (let ((?x4410 (bvadd (_ bv62 6) ?x72)))
 (let (($x1931 (bvsle ?x4410 n)))
 (let ((?x1594 (stack_s x_0 x_1 x_2 0 n)))
 (let ((?x5710 (stack_s x_0 x_1 x_2 1 n)))
 (let (($x1630 (= ?x5710 ?x1594)))
 (or $x1630 $x1931))))))))
 ))
 (let (($x11336 (= ?x154 ?x72)))
 (let ((?x10571 (used_gas_s x_0 x_1 x_2 1)))
 (let (($x9020 (= ?x10571 (+ 3 ?x7240))))
 (let ((?x4410 (bvadd (_ bv62 6) ?x72)))
 (let ((?x4544 (stack_s x_0 x_1 x_2 0 ?x4410)))
 (let (($x4601 (= ?x3296 ?x4544)))
 (let (($x76 (forall ((w (_ BitVec 256)) )(let ((?x6937 (storage_s x_0 x_1 x_2 0 w)))
 (= ?x6937 (_ bv0 256))))
 ))
 (let (($x4921 (= ?x7240 0)))
 (let (($x4102 (not $x57)))
 (let (($x3658 (= (stack_s x_0 x_1 x_2 0 (_ bv2 6)) x_2)))
 (let (($x6604 (= (stack_s x_0 x_1 x_2 0 (_ bv1 6)) x_1)))
 (let (($x489 (= (stack_s x_0 x_1 x_2 0 (_ bv0 6)) x_0)))
 (let (($x5315 (= ?x72 (_ bv3 6))))
 (and $x5315 $x489 $x6604 $x3658 $x4102 $x4921 $x76 $x4601 (= ?x9793 (stack_s x_0 x_1 x_2 0 (bvadd (_ bv63 6) ?x72))) $x9020 $x11336 $x3417 $x6187 (= $x189 (or $x57 (not (bvsle (_ bv0 6) ?x4410)))) $x9269 (= (stack_s x_0 x_1 x_2 2 ?x6807) ?x9793) (= (stack_s x_0 x_1 x_2 2 ?x7935) ?x3296) $x4072 $x430 $x3743 $x3106 $x3772 (= ?x10486 (bvnot ?x11226)) $x6685 $x11085 $x8516 $x6260 $x1868 (= ?x2338 (stack_s x_0 x_1 x_2 3 (bvadd (_ bv61 6) ?x275))) (= ?x7054 ?x10486) (= ?x2837 (stack_s x_0 x_1 x_2 3 (bvadd (_ bv62 6) ?x275))) $x4100 $x2450 $x9833 $x8765 $x2921 (= ?x2690 ?x7054) (= (stack_s x_0 x_1 x_2 5 ?x5681) ?x7054) (= (stack_s x_0 x_1 x_2 5 ?x3448) ?x2837) (= (stack_s x_0 x_1 x_2 5 ?x2253) ?x2338) (= ?x7899 (+ 3 ?x3425)) $x10984 $x10789 $x7690 (= $x11317 (or (not (bvsle (_ bv0 6) ?x5681)) $x7172 $x7547)) $x8065 (= (stack_s x_0 x_1 x_2 6 (bvadd (_ bv62 6) ?x926)) ?x2690) $x4010 $x7598 $x1576 $x4432 $x7524 (= ?x5109 ?x6964) (= (stack_t x_0 x_1 x_2 1 ?x208) ?x6964) $x8161 $x7773 $x1854 $x1318 (= $x3508 (or $x56 $x2998 $x10059)) $x2907 $x6033 $x1274 $x977 $x8174 $x4443 $x11371 $x11449 (= ?x10397 (bvnot ?x5880)) $x10847 $x7102 $x11629 $x539 $x6380 (= ?x9406 ?x10397) (= (stack_t x_0 x_1 x_2 4 ?x6883) ?x10397) (= ?x4210 (+ 3 ?x4859)) $x10865 $x3118 $x5846 $x3343 $x7873 (= (stack_t x_0 x_1 x_2 5 (bvadd (_ bv60 6) ?x919)) ?x9406) $x10867 $x689 $x4857 $x297 $x9986 $x10648 $x9503 $x73 $x4198 $x58 $x10987 $x5606 (not (and $x929 $x8849 $x889 $x6686))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
 )
(check-sat)
(get-proof)
