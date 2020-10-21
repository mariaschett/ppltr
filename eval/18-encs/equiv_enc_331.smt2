; DUP1 SLOAD SWAP1 DUP3 SWAP1 => DUP2 DUP2 SLOAD SWAP2
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
 (exists ((x_0 (_ BitVec 256)) (x_1 (_ BitVec 256)) (x_SLOAD_0 (_ BitVec 256)) )(let (($x9262 (forall ((w (_ BitVec 256)) )(let ((?x9839 (storage_t x_0 x_1 x_SLOAD_0 4 w)))
 (let ((?x7157 (storage_s x_0 x_1 x_SLOAD_0 5 w)))
 (= ?x7157 ?x9839))))
 ))
 (let (($x3723 (exc_halt_t 4)))
 (let (($x3979 (exc_halt_s 5)))
 (let (($x7567 (= $x3979 $x3723)))
 (let (($x9226 (forall ((n (_ BitVec 6)) )(let ((?x3757 (sc_t 4)))
 (let (($x9962 (bvsle ?x3757 n)))
 (let ((?x10108 (stack_t x_0 x_1 x_SLOAD_0 4 n)))
 (let ((?x11143 (stack_s x_0 x_1 x_SLOAD_0 5 n)))
 (let (($x5946 (= ?x11143 ?x10108)))
 (or $x5946 $x9962)))))))
 ))
 (let ((?x3757 (sc_t 4)))
 (let ((?x805 (sc_s 5)))
 (let (($x9143 (= ?x805 ?x3757)))
 (let ((?x6031 (used_gas_t x_0 x_1 x_SLOAD_0 0)))
 (let ((?x9515 (used_gas_s x_0 x_1 x_SLOAD_0 0)))
 (let (($x7236 (= ?x9515 ?x6031)))
 (let (($x3892 (forall ((w (_ BitVec 256)) )(let ((?x10272 (storage_t x_0 x_1 x_SLOAD_0 0 w)))
 (let ((?x9337 (storage_s x_0 x_1 x_SLOAD_0 0 w)))
 (= ?x9337 ?x10272))))
 ))
 (let (($x56 (exc_halt_t 0)))
 (let (($x57 (exc_halt_s 0)))
 (let (($x58 (= $x57 $x56)))
 (let (($x5422 (forall ((n (_ BitVec 6)) )(let ((?x7976 (stack_t x_0 x_1 x_SLOAD_0 0 n)))
 (let ((?x8358 (stack_s x_0 x_1 x_SLOAD_0 0 n)))
 (let (($x5540 (= ?x8358 ?x7976)))
 (let ((?x63 (sc_t 0)))
 (let (($x2337 (bvsle ?x63 n)))
 (or $x2337 $x5540)))))))
 ))
 (let ((?x63 (sc_t 0)))
 (let ((?x72 (sc_s 0)))
 (let (($x73 (= ?x72 ?x63)))
 (let (($x7694 (= $x3723 (or (exc_halt_t 3) (not (bvsle (_ bv0 6) (bvadd (_ bv61 6) (sc_t 3))))))))
 (let (($x554 (forall ((w (_ BitVec 256)) )(let ((?x11763 (storage_t x_0 x_1 x_SLOAD_0 3 w)))
 (let ((?x9839 (storage_t x_0 x_1 x_SLOAD_0 4 w)))
 (= ?x9839 ?x11763))))
 ))
 (let (($x1986 (forall ((n (_ BitVec 6)) )(let ((?x3700 (stack_t x_0 x_1 x_SLOAD_0 3 n)))
 (let ((?x10108 (stack_t x_0 x_1 x_SLOAD_0 4 n)))
 (let ((?x6438 (sc_t 3)))
 (let ((?x10284 (bvadd (_ bv61 6) ?x6438)))
 (let (($x4797 (bvsle ?x10284 n)))
 (or $x4797 (= ?x10108 ?x3700))))))))
 ))
 (let (($x1923 (= (used_gas_t x_0 x_1 x_SLOAD_0 4) (+ 3 (used_gas_t x_0 x_1 x_SLOAD_0 3)))))
 (let (($x6157 (= (stack_t x_0 x_1 x_SLOAD_0 4 (bvadd (_ bv62 6) ?x3757)) (stack_t x_0 x_1 x_SLOAD_0 3 (bvadd (_ bv62 6) (sc_t 3))))))
 (let ((?x6438 (sc_t 3)))
 (let ((?x10240 (bvadd (_ bv63 6) ?x6438)))
 (let ((?x11269 (stack_t x_0 x_1 x_SLOAD_0 3 ?x10240)))
 (let (($x1635 (= (stack_t x_0 x_1 x_SLOAD_0 4 (bvadd (_ bv63 6) ?x3757)) (stack_t x_0 x_1 x_SLOAD_0 3 (bvadd (_ bv61 6) ?x6438)))))
 (let (($x6783 (exc_halt_t 3)))
 (let (($x10022 (= $x6783 (or (exc_halt_t 2) (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_t 2))))))))
 (let (($x9357 (forall ((w (_ BitVec 256)) )(let ((?x9153 (storage_t x_0 x_1 x_SLOAD_0 2 w)))
 (let ((?x11763 (storage_t x_0 x_1 x_SLOAD_0 3 w)))
 (= ?x11763 ?x9153))))
 ))
 (let (($x4562 (forall ((n (_ BitVec 6)) )(let ((?x2714 (sc_t 2)))
 (let ((?x8825 (bvadd (_ bv63 6) ?x2714)))
 (let (($x8412 (bvsle ?x8825 n)))
 (let ((?x1816 (stack_t x_0 x_1 x_SLOAD_0 2 n)))
 (let ((?x3700 (stack_t x_0 x_1 x_SLOAD_0 3 n)))
 (let (($x5729 (= ?x3700 ?x1816)))
 (or $x5729 $x8412))))))))
 ))
 (let ((?x6199 (used_gas_t x_0 x_1 x_SLOAD_0 3)))
 (let ((?x2714 (sc_t 2)))
 (let ((?x8825 (bvadd (_ bv63 6) ?x2714)))
 (let ((?x8941 (stack_t x_0 x_1 x_SLOAD_0 2 ?x8825)))
 (let (($x9927 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_t 1)))) (_ bv0 1)))))
 (let (($x326 (exc_halt_t 1)))
 (let (($x5252 (exc_halt_t 2)))
 (let (($x1859 (= $x5252 (or $x326 (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_t 1)))) $x9927))))
 (let (($x11364 (forall ((w (_ BitVec 256)) )(let ((?x8039 (storage_t x_0 x_1 x_SLOAD_0 1 w)))
 (let ((?x9153 (storage_t x_0 x_1 x_SLOAD_0 2 w)))
 (= ?x9153 ?x8039))))
 ))
 (let (($x5012 (forall ((n (_ BitVec 6)) )(let ((?x8091 (stack_t x_0 x_1 x_SLOAD_0 1 n)))
 (let ((?x1816 (stack_t x_0 x_1 x_SLOAD_0 2 n)))
 (let (($x4207 (= ?x1816 ?x8091)))
 (let ((?x11560 (sc_t 1)))
 (let ((?x2726 (bvadd (_ bv62 6) ?x11560)))
 (let (($x2848 (bvsle ?x2726 n)))
 (or $x2848 $x4207))))))))
 ))
 (let (($x10865 (= ?x2714 (bvadd (_ bv1 6) (sc_t 1)))))
 (let ((?x2847 (used_gas_t x_0 x_1 x_SLOAD_0 2)))
 (let (($x4374 (= ?x2847 (+ 3 (used_gas_t x_0 x_1 x_SLOAD_0 1)))))
 (let ((?x11560 (sc_t 1)))
 (let ((?x8596 (bvadd (_ bv63 6) ?x11560)))
 (let ((?x2576 (stack_t x_0 x_1 x_SLOAD_0 1 ?x8596)))
 (let ((?x2726 (bvadd (_ bv62 6) ?x11560)))
 (let ((?x1693 (stack_t x_0 x_1 x_SLOAD_0 1 ?x2726)))
 (let (($x3424 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x63))) (_ bv0 1)))))
 (let (($x2258 (= $x326 (or $x56 $x3424 (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) ?x63)))))))
 (let (($x9896 (forall ((w (_ BitVec 256)) )(let ((?x10272 (storage_t x_0 x_1 x_SLOAD_0 0 w)))
 (let ((?x8039 (storage_t x_0 x_1 x_SLOAD_0 1 w)))
 (= ?x8039 ?x10272))))
 ))
 (let (($x498 (forall ((n (_ BitVec 6)) )(let ((?x7976 (stack_t x_0 x_1 x_SLOAD_0 0 n)))
 (let ((?x8091 (stack_t x_0 x_1 x_SLOAD_0 1 n)))
 (or (bvsle (bvadd (_ bv62 6) (sc_t 0)) n) (= ?x8091 ?x7976)))))
 ))
 (let (($x7646 (= ?x11560 (bvadd (_ bv1 6) ?x63))))
 (let ((?x3106 (used_gas_t x_0 x_1 x_SLOAD_0 1)))
 (let (($x9843 (= ?x3106 (+ 3 ?x6031))))
 (let (($x4440 (= (stack_t x_0 x_1 x_SLOAD_0 1 (bvadd (_ bv63 6) ?x63)) (stack_t x_0 x_1 x_SLOAD_0 0 (bvadd (_ bv63 6) ?x63)))))
 (let ((?x11081 (bvadd (_ bv62 6) ?x63)))
 (let ((?x201 (stack_t x_0 x_1 x_SLOAD_0 0 ?x11081)))
 (let (($x6078 (= (stack_t x_0 x_1 x_SLOAD_0 1 ?x11081) ?x201)))
 (let (($x9147 (= ?x2576 ?x201)))
 (let (($x9010 (= $x3979 (or (exc_halt_s 4) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 4))))))))
 (let (($x7841 (forall ((w (_ BitVec 256)) )(let ((?x356 (storage_s x_0 x_1 x_SLOAD_0 4 w)))
 (let ((?x7157 (storage_s x_0 x_1 x_SLOAD_0 5 w)))
 (= ?x7157 ?x356))))
 ))
 (let (($x7947 (forall ((n (_ BitVec 6)) )(let ((?x4305 (sc_s 4)))
 (let ((?x8912 (bvadd (_ bv62 6) ?x4305)))
 (let (($x9203 (bvsle ?x8912 n)))
 (let ((?x582 (stack_s x_0 x_1 x_SLOAD_0 4 n)))
 (let ((?x11143 (stack_s x_0 x_1 x_SLOAD_0 5 n)))
 (or (= ?x11143 ?x582) $x9203)))))))
 ))
 (let (($x7871 (= (used_gas_s x_0 x_1 x_SLOAD_0 5) (+ 3 (used_gas_s x_0 x_1 x_SLOAD_0 4)))))
 (let ((?x4305 (sc_s 4)))
 (let ((?x10093 (bvadd (_ bv63 6) ?x4305)))
 (let ((?x7006 (stack_s x_0 x_1 x_SLOAD_0 4 ?x10093)))
 (let ((?x8912 (bvadd (_ bv62 6) ?x4305)))
 (let ((?x1660 (stack_s x_0 x_1 x_SLOAD_0 4 ?x8912)))
 (let (($x1602 (= (stack_s x_0 x_1 x_SLOAD_0 5 (bvadd (_ bv63 6) ?x805)) ?x1660)))
 (let (($x7104 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 3)))) (_ bv0 1)))))
 (let (($x292 (exc_halt_s 3)))
 (let (($x7172 (exc_halt_s 4)))
 (let (($x1806 (forall ((w (_ BitVec 256)) )(let ((?x845 (storage_s x_0 x_1 x_SLOAD_0 3 w)))
 (let ((?x356 (storage_s x_0 x_1 x_SLOAD_0 4 w)))
 (= ?x356 ?x845))))
 ))
 (let (($x3074 (forall ((n (_ BitVec 6)) )(let ((?x9008 (stack_s x_0 x_1 x_SLOAD_0 3 n)))
 (let ((?x582 (stack_s x_0 x_1 x_SLOAD_0 4 n)))
 (let (($x3319 (= ?x582 ?x9008)))
 (or (bvsle (bvadd (_ bv61 6) (sc_s 3)) n) $x3319)))))
 ))
 (let (($x9578 (= ?x4305 (bvadd (_ bv1 6) (sc_s 3)))))
 (let ((?x11133 (used_gas_s x_0 x_1 x_SLOAD_0 4)))
 (let (($x3906 (= ?x11133 (+ 3 (used_gas_s x_0 x_1 x_SLOAD_0 3)))))
 (let ((?x275 (sc_s 3)))
 (let ((?x6666 (bvadd (_ bv63 6) ?x275)))
 (let ((?x6388 (stack_s x_0 x_1 x_SLOAD_0 3 ?x6666)))
 (let ((?x6122 (bvadd (_ bv62 6) ?x275)))
 (let ((?x6828 (stack_s x_0 x_1 x_SLOAD_0 3 ?x6122)))
 (let ((?x8405 (bvadd (_ bv61 6) ?x275)))
 (let ((?x8238 (stack_s x_0 x_1 x_SLOAD_0 3 ?x8405)))
 (let (($x4102 (= $x292 (or (exc_halt_s 2) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 2))))))))
 (let (($x1058 (forall ((w (_ BitVec 256)) )(let ((?x7248 (storage_s x_0 x_1 x_SLOAD_0 2 w)))
 (let ((?x845 (storage_s x_0 x_1 x_SLOAD_0 3 w)))
 (= ?x845 ?x7248))))
 ))
 (let (($x11534 (forall ((n (_ BitVec 6)) )(let ((?x6114 (stack_s x_0 x_1 x_SLOAD_0 2 n)))
 (let ((?x9008 (stack_s x_0 x_1 x_SLOAD_0 3 n)))
 (or (bvsle (bvadd (_ bv62 6) (sc_s 2)) n) (= ?x9008 ?x6114)))))
 ))
 (let ((?x235 (used_gas_s x_0 x_1 x_SLOAD_0 3)))
 (let (($x6940 (= ?x235 (+ 3 (used_gas_s x_0 x_1 x_SLOAD_0 2)))))
 (let (($x247 (exc_halt_s 2)))
 (let (($x4622 (= $x247 (or (exc_halt_s 1) (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_s 1))))))))
 (let (($x8795 (forall ((w (_ BitVec 256)) )(let ((?x10566 (storage_s x_0 x_1 x_SLOAD_0 1 w)))
 (let ((?x7248 (storage_s x_0 x_1 x_SLOAD_0 2 w)))
 (= ?x7248 ?x10566))))
 ))
 (let (($x4493 (forall ((n (_ BitVec 6)) )(let ((?x11496 (stack_s x_0 x_1 x_SLOAD_0 1 n)))
 (let ((?x6114 (stack_s x_0 x_1 x_SLOAD_0 2 n)))
 (let (($x9941 (= ?x6114 ?x11496)))
 (let ((?x154 (sc_s 1)))
 (let ((?x10146 (bvadd (_ bv63 6) ?x154)))
 (let (($x811 (bvsle ?x10146 n)))
 (or $x811 $x9941))))))))
 ))
 (let ((?x154 (sc_s 1)))
 (let ((?x218 (sc_s 2)))
 (let (($x7667 (= ?x218 ?x154)))
 (let ((?x7200 (used_gas_s x_0 x_1 x_SLOAD_0 2)))
 (let ((?x10146 (bvadd (_ bv63 6) ?x154)))
 (let ((?x5708 (stack_s x_0 x_1 x_SLOAD_0 1 ?x10146)))
 (let ((?x5548 (bvadd (_ bv63 6) ?x218)))
 (let ((?x9727 (stack_s x_0 x_1 x_SLOAD_0 2 ?x5548)))
 (let (($x7088 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x72))) (_ bv0 1)))))
 (let (($x189 (exc_halt_s 1)))
 (let (($x10738 (forall ((w (_ BitVec 256)) )(let ((?x9337 (storage_s x_0 x_1 x_SLOAD_0 0 w)))
 (let ((?x10566 (storage_s x_0 x_1 x_SLOAD_0 1 w)))
 (= ?x10566 ?x9337))))
 ))
 (let (($x5993 (forall ((n (_ BitVec 6)) )(let ((?x8358 (stack_s x_0 x_1 x_SLOAD_0 0 n)))
 (let ((?x11496 (stack_s x_0 x_1 x_SLOAD_0 1 n)))
 (let (($x6592 (= ?x11496 ?x8358)))
 (or $x6592 (bvsle (bvadd (_ bv63 6) (sc_s 0)) n))))))
 ))
 (let (($x3297 (= ?x154 (bvadd (_ bv1 6) ?x72))))
 (let ((?x10015 (used_gas_s x_0 x_1 x_SLOAD_0 1)))
 (let (($x1099 (= ?x10015 (+ 3 ?x9515))))
 (let ((?x3826 (bvadd (_ bv63 6) ?x72)))
 (let ((?x2300 (stack_s x_0 x_1 x_SLOAD_0 0 ?x3826)))
 (let (($x1736 (forall ((w (_ BitVec 256)) )(let ((?x6107 (ite (= w (stack_s x_0 x_1 x_SLOAD_0 1 (bvadd (_ bv63 6) (sc_s 1)))) x_SLOAD_0 (_ bv0 256))))
 (let ((?x9337 (storage_s x_0 x_1 x_SLOAD_0 0 w)))
 (= ?x9337 ?x6107))))
 ))
 (let (($x5407 (= ?x9515 0)))
 (let (($x4887 (not $x57)))
 (let (($x7624 (= (stack_s x_0 x_1 x_SLOAD_0 0 (_ bv1 6)) x_1)))
 (let (($x10253 (= (stack_s x_0 x_1 x_SLOAD_0 0 (_ bv0 6)) x_0)))
 (let (($x1103 (= ?x72 (_ bv2 6))))
 (and $x1103 $x10253 $x7624 $x4887 $x5407 $x1736 (= ?x5708 ?x2300) (= (stack_s x_0 x_1 x_SLOAD_0 1 ?x3826) ?x2300) $x1099 $x3297 $x5993 $x10738 (= $x189 (or $x57 $x7088 (not (bvsle (_ bv0 6) ?x3826)))) (= ?x9727 (storage_s x_0 x_1 x_SLOAD_0 1 ?x5708)) (= ?x7200 (+ 200 ?x10015)) $x7667 $x4493 $x8795 $x4622 (= ?x6388 (stack_s x_0 x_1 x_SLOAD_0 2 (bvadd (_ bv62 6) ?x218))) (= ?x6828 ?x9727) $x6940 (= ?x275 ?x218) $x11534 $x1058 $x4102 (= ?x7006 ?x8238) (= (stack_s x_0 x_1 x_SLOAD_0 4 ?x8405) ?x8238) (= (stack_s x_0 x_1 x_SLOAD_0 4 ?x6122) ?x6828) (= (stack_s x_0 x_1 x_SLOAD_0 4 ?x6666) ?x6388) $x3906 $x9578 $x3074 $x1806 (= $x7172 (or $x292 (not (bvsle (_ bv0 6) ?x8405)) $x7104)) $x1602 (= (stack_s x_0 x_1 x_SLOAD_0 5 (bvadd (_ bv62 6) ?x805)) ?x7006) $x7871 (= ?x805 ?x4305) $x7947 $x7841 $x9010 $x9147 $x6078 $x4440 $x9843 $x7646 $x498 $x9896 $x2258 (= ?x8941 ?x1693) (= (stack_t x_0 x_1 x_SLOAD_0 2 ?x2726) ?x1693) (= (stack_t x_0 x_1 x_SLOAD_0 2 ?x8596) ?x2576) $x4374 $x10865 $x5012 $x11364 $x1859 (= ?x11269 (storage_t x_0 x_1 x_SLOAD_0 2 ?x8941)) (= ?x6199 (+ 200 ?x2847)) (= ?x6438 ?x2714) $x4562 $x9357 $x10022 $x1635 (= (stack_t x_0 x_1 x_SLOAD_0 4 (bvadd (_ bv61 6) ?x3757)) ?x11269) $x6157 $x1923 (= ?x3757 ?x6438) $x1986 $x554 $x7694 $x73 $x5422 $x58 $x3892 $x7236 (not (and $x9143 $x9226 $x7567 $x9262)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
 )
(check-sat)
(get-proof)
