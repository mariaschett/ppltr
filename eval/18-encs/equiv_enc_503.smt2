; PUSH cw_2 DUP1 MLOAD SWAP2 DUP3 SWAP1 => PUSH cw_2 PUSH cw_2 MLOAD DUP1 SWAP3
(set-option :produce-proofs true)
; 
(set-info :status unknown)
(declare-fun storage_t ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 256)) (_ BitVec 256))
(declare-fun w_2 () (_ BitVec 256))
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
 (exists ((x_0 (_ BitVec 256)) (x_MLOAD_0 (_ BitVec 256)) )(let (($x5783 (forall ((w (_ BitVec 256)) )(let ((?x4108 (storage_t x_0 w_2 x_MLOAD_0 5 w)))
 (let ((?x9260 (storage_s x_0 w_2 x_MLOAD_0 6 w)))
 (= ?x9260 ?x4108))))
 ))
 (let (($x886 (exc_halt_t 5)))
 (let (($x772 (exc_halt_s 6)))
 (let (($x889 (= $x772 $x886)))
 (let (($x10055 (forall ((n (_ BitVec 6)) )(let ((?x350 (stack_t x_0 w_2 x_MLOAD_0 5 n)))
 (let ((?x9403 (stack_s x_0 w_2 x_MLOAD_0 6 n)))
 (let (($x8095 (= ?x9403 ?x350)))
 (let ((?x919 (sc_t 5)))
 (let (($x3927 (bvsle ?x919 n)))
 (or $x3927 $x8095)))))))
 ))
 (let ((?x919 (sc_t 5)))
 (let ((?x926 (sc_s 6)))
 (let (($x929 (= ?x926 ?x919)))
 (let ((?x3908 (used_gas_t x_0 w_2 x_MLOAD_0 0)))
 (let ((?x1688 (used_gas_s x_0 w_2 x_MLOAD_0 0)))
 (let (($x8060 (= ?x1688 ?x3908)))
 (let (($x4642 (forall ((w (_ BitVec 256)) )(let ((?x10951 (storage_t x_0 w_2 x_MLOAD_0 0 w)))
 (let ((?x7274 (storage_s x_0 w_2 x_MLOAD_0 0 w)))
 (= ?x7274 ?x10951))))
 ))
 (let (($x56 (exc_halt_t 0)))
 (let (($x57 (exc_halt_s 0)))
 (let (($x58 (= $x57 $x56)))
 (let (($x6748 (forall ((n (_ BitVec 6)) )(let ((?x63 (sc_t 0)))
 (let (($x2855 (bvsle ?x63 n)))
 (let ((?x848 (stack_t x_0 w_2 x_MLOAD_0 0 n)))
 (let ((?x851 (stack_s x_0 w_2 x_MLOAD_0 0 n)))
 (let (($x1705 (= ?x851 ?x848)))
 (or $x1705 $x2855)))))))
 ))
 (let ((?x63 (sc_t 0)))
 (let ((?x72 (sc_s 0)))
 (let (($x73 (= ?x72 ?x63)))
 (let (($x6936 (= $x886 (or (exc_halt_t 4) (not (bvsle (_ bv0 6) (bvadd (_ bv60 6) (sc_t 4))))))))
 (let (($x1103 (forall ((w (_ BitVec 256)) )(let ((?x1825 (storage_t x_0 w_2 x_MLOAD_0 4 w)))
 (let ((?x4108 (storage_t x_0 w_2 x_MLOAD_0 5 w)))
 (= ?x4108 ?x1825))))
 ))
 (let (($x5891 (forall ((n (_ BitVec 6)) )(let ((?x1546 (stack_t x_0 w_2 x_MLOAD_0 4 n)))
 (let ((?x350 (stack_t x_0 w_2 x_MLOAD_0 5 n)))
 (or (bvsle (bvadd (_ bv60 6) (sc_t 4)) n) (= ?x350 ?x1546)))))
 ))
 (let (($x4858 (= (used_gas_t x_0 w_2 x_MLOAD_0 5) (+ 3 (used_gas_t x_0 w_2 x_MLOAD_0 4)))))
 (let (($x5639 (= (stack_t x_0 w_2 x_MLOAD_0 5 (bvadd (_ bv62 6) ?x919)) (stack_t x_0 w_2 x_MLOAD_0 4 (bvadd (_ bv62 6) (sc_t 4))))))
 (let (($x2582 (= (stack_t x_0 w_2 x_MLOAD_0 5 (bvadd (_ bv61 6) ?x919)) (stack_t x_0 w_2 x_MLOAD_0 4 (bvadd (_ bv61 6) (sc_t 4))))))
 (let ((?x4818 (sc_t 4)))
 (let ((?x282 (bvadd (_ bv63 6) ?x4818)))
 (let ((?x2002 (stack_t x_0 w_2 x_MLOAD_0 4 ?x282)))
 (let (($x7467 (= (stack_t x_0 w_2 x_MLOAD_0 5 (bvadd (_ bv63 6) ?x919)) (stack_t x_0 w_2 x_MLOAD_0 4 (bvadd (_ bv60 6) ?x4818)))))
 (let (($x6783 (exc_halt_t 3)))
 (let (($x3007 (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_t 3))))))
 (let (($x740 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_t 3)))) (_ bv0 1)))))
 (let (($x7854 (exc_halt_t 4)))
 (let (($x3447 (forall ((w (_ BitVec 256)) )(let ((?x2280 (storage_t x_0 w_2 x_MLOAD_0 3 w)))
 (let ((?x1825 (storage_t x_0 w_2 x_MLOAD_0 4 w)))
 (= ?x1825 ?x2280))))
 ))
 (let (($x10878 (forall ((n (_ BitVec 6)) )(let ((?x6438 (sc_t 3)))
 (let ((?x5797 (bvadd (_ bv63 6) ?x6438)))
 (let (($x9142 (bvsle ?x5797 n)))
 (let ((?x7767 (stack_t x_0 w_2 x_MLOAD_0 3 n)))
 (let ((?x1546 (stack_t x_0 w_2 x_MLOAD_0 4 n)))
 (or (= ?x1546 ?x7767) $x9142)))))))
 ))
 (let (($x10007 (= ?x4818 (bvadd (_ bv1 6) (sc_t 3)))))
 (let ((?x10649 (used_gas_t x_0 w_2 x_MLOAD_0 4)))
 (let ((?x6438 (sc_t 3)))
 (let ((?x5797 (bvadd (_ bv63 6) ?x6438)))
 (let ((?x10072 (stack_t x_0 w_2 x_MLOAD_0 3 ?x5797)))
 (let (($x8365 (= $x6783 (or (exc_halt_t 2) (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_t 2))))))))
 (let (($x6848 (forall ((w (_ BitVec 256)) )(let ((?x474 (storage_t x_0 w_2 x_MLOAD_0 2 w)))
 (let ((?x2280 (storage_t x_0 w_2 x_MLOAD_0 3 w)))
 (= ?x2280 ?x474))))
 ))
 (let (($x23 (forall ((n (_ BitVec 6)) )(let ((?x7609 (stack_t x_0 w_2 x_MLOAD_0 2 n)))
 (let ((?x7767 (stack_t x_0 w_2 x_MLOAD_0 3 n)))
 (or (bvsle (bvadd (_ bv63 6) (sc_t 2)) n) (= ?x7767 ?x7609)))))
 ))
 (let ((?x2714 (sc_t 2)))
 (let (($x4836 (= ?x6438 ?x2714)))
 (let ((?x1027 (used_gas_t x_0 w_2 x_MLOAD_0 3)))
 (let (($x8020 (= ?x1027 (+ 3 (used_gas_t x_0 w_2 x_MLOAD_0 2)))))
 (let ((?x5618 (bvadd (_ bv63 6) ?x2714)))
 (let ((?x11390 (stack_t x_0 w_2 x_MLOAD_0 2 ?x5618)))
 (let (($x5562 (= ?x10072 (f_MLOAD x_0 w_2 x_MLOAD_0 ?x11390))))
 (let (($x6916 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_t 1)))) (_ bv0 1)))))
 (let (($x8377 (exc_halt_t 1)))
 (let (($x2163 (exc_halt_t 2)))
 (let (($x11314 (= $x2163 (or $x8377 $x6916))))
 (let (($x3844 (forall ((w (_ BitVec 256)) )(let ((?x6104 (storage_t x_0 w_2 x_MLOAD_0 1 w)))
 (let ((?x474 (storage_t x_0 w_2 x_MLOAD_0 2 w)))
 (= ?x474 ?x6104))))
 ))
 (let (($x11572 (forall ((n (_ BitVec 6)) )(let ((?x10130 (stack_t x_0 w_2 x_MLOAD_0 1 n)))
 (let ((?x7609 (stack_t x_0 w_2 x_MLOAD_0 2 n)))
 (let (($x8840 (= ?x7609 ?x10130)))
 (let ((?x7154 (sc_t 1)))
 (let (($x1020 (bvsle ?x7154 n)))
 (or $x1020 $x8840)))))))
 ))
 (let (($x135 (= ?x2714 (bvadd (_ bv1 6) (sc_t 1)))))
 (let ((?x9988 (used_gas_t x_0 w_2 x_MLOAD_0 2)))
 (let (($x1588 (= ?x9988 (+ 3 (used_gas_t x_0 w_2 x_MLOAD_0 1)))))
 (let (($x7349 (= $x8377 (or $x56 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x63))) (_ bv0 1)))))))
 (let (($x9463 (forall ((w (_ BitVec 256)) )(let ((?x10951 (storage_t x_0 w_2 x_MLOAD_0 0 w)))
 (let ((?x6104 (storage_t x_0 w_2 x_MLOAD_0 1 w)))
 (= ?x6104 ?x10951))))
 ))
 (let (($x8629 (forall ((n (_ BitVec 6)) )(let ((?x848 (stack_t x_0 w_2 x_MLOAD_0 0 n)))
 (let ((?x10130 (stack_t x_0 w_2 x_MLOAD_0 1 n)))
 (let ((?x63 (sc_t 0)))
 (let (($x2855 (bvsle ?x63 n)))
 (or $x2855 (= ?x10130 ?x848)))))))
 ))
 (let ((?x7154 (sc_t 1)))
 (let (($x6089 (= ?x7154 (bvadd (_ bv1 6) ?x63))))
 (let ((?x4862 (used_gas_t x_0 w_2 x_MLOAD_0 1)))
 (let (($x4627 (= ?x4862 (+ 3 ?x3908))))
 (let (($x10709 (= (stack_t x_0 w_2 x_MLOAD_0 1 ?x63) w_2)))
 (let (($x6274 (= $x772 (or (exc_halt_s 5) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 5))))))))
 (let (($x2828 (forall ((w (_ BitVec 256)) )(let ((?x4536 (storage_s x_0 w_2 x_MLOAD_0 5 w)))
 (let ((?x9260 (storage_s x_0 w_2 x_MLOAD_0 6 w)))
 (= ?x9260 ?x4536))))
 ))
 (let (($x10292 (forall ((n (_ BitVec 6)) )(let ((?x4319 (sc_s 5)))
 (let ((?x4138 (bvadd (_ bv62 6) ?x4319)))
 (let (($x7050 (bvsle ?x4138 n)))
 (let ((?x5142 (stack_s x_0 w_2 x_MLOAD_0 5 n)))
 (let ((?x9403 (stack_s x_0 w_2 x_MLOAD_0 6 n)))
 (or (= ?x9403 ?x5142) $x7050)))))))
 ))
 (let ((?x4319 (sc_s 5)))
 (let (($x3654 (= ?x926 ?x4319)))
 (let (($x8220 (= (used_gas_s x_0 w_2 x_MLOAD_0 6) (+ 3 (used_gas_s x_0 w_2 x_MLOAD_0 5)))))
 (let ((?x8324 (bvadd (_ bv63 6) ?x4319)))
 (let ((?x6243 (stack_s x_0 w_2 x_MLOAD_0 5 ?x8324)))
 (let (($x3423 (= (stack_s x_0 w_2 x_MLOAD_0 6 (bvadd (_ bv63 6) ?x926)) (stack_s x_0 w_2 x_MLOAD_0 5 (bvadd (_ bv62 6) ?x4319)))))
 (let (($x10728 (not (bvsle (_ bv0 6) (bvadd (_ bv61 6) (sc_s 4))))))
 (let (($x7172 (exc_halt_s 4)))
 (let (($x1305 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 4)))) (_ bv0 1)))))
 (let (($x11317 (exc_halt_s 5)))
 (let (($x674 (forall ((w (_ BitVec 256)) )(let ((?x2126 (storage_s x_0 w_2 x_MLOAD_0 4 w)))
 (let ((?x4536 (storage_s x_0 w_2 x_MLOAD_0 5 w)))
 (= ?x4536 ?x2126))))
 ))
 (let (($x5943 (forall ((n (_ BitVec 6)) )(let ((?x4305 (sc_s 4)))
 (let ((?x8585 (bvadd (_ bv61 6) ?x4305)))
 (let (($x11137 (bvsle ?x8585 n)))
 (let ((?x5906 (stack_s x_0 w_2 x_MLOAD_0 4 n)))
 (let ((?x5142 (stack_s x_0 w_2 x_MLOAD_0 5 n)))
 (or (= ?x5142 ?x5906) $x11137)))))))
 ))
 (let (($x10302 (= ?x4319 (bvadd (_ bv1 6) (sc_s 4)))))
 (let ((?x5447 (used_gas_s x_0 w_2 x_MLOAD_0 5)))
 (let ((?x4305 (sc_s 4)))
 (let ((?x4388 (bvadd (_ bv63 6) ?x4305)))
 (let ((?x3882 (stack_s x_0 w_2 x_MLOAD_0 4 ?x4388)))
 (let ((?x5394 (bvadd (_ bv62 6) ?x4305)))
 (let ((?x5275 (stack_s x_0 w_2 x_MLOAD_0 4 ?x5394)))
 (let ((?x8585 (bvadd (_ bv61 6) ?x4305)))
 (let ((?x1515 (stack_s x_0 w_2 x_MLOAD_0 4 ?x8585)))
 (let (($x7694 (= $x7172 (or (exc_halt_s 3) (not (bvsle (_ bv0 6) (bvadd (_ bv61 6) (sc_s 3))))))))
 (let (($x7532 (forall ((w (_ BitVec 256)) )(let ((?x6852 (storage_s x_0 w_2 x_MLOAD_0 3 w)))
 (let ((?x2126 (storage_s x_0 w_2 x_MLOAD_0 4 w)))
 (= ?x2126 ?x6852))))
 ))
 (let (($x10058 (forall ((n (_ BitVec 6)) )(let ((?x7978 (stack_s x_0 w_2 x_MLOAD_0 3 n)))
 (let ((?x5906 (stack_s x_0 w_2 x_MLOAD_0 4 n)))
 (let (($x7597 (= ?x5906 ?x7978)))
 (let ((?x275 (sc_s 3)))
 (let ((?x6308 (bvadd (_ bv61 6) ?x275)))
 (let (($x2513 (bvsle ?x6308 n)))
 (or $x2513 $x7597))))))))
 ))
 (let ((?x275 (sc_s 3)))
 (let (($x2594 (= ?x4305 ?x275)))
 (let ((?x11244 (used_gas_s x_0 w_2 x_MLOAD_0 4)))
 (let (($x9743 (= ?x11244 (+ 3 (used_gas_s x_0 w_2 x_MLOAD_0 3)))))
 (let (($x292 (exc_halt_s 3)))
 (let (($x9269 (= $x292 (or (exc_halt_s 2) (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_s 2))))))))
 (let (($x10778 (forall ((w (_ BitVec 256)) )(let ((?x7989 (storage_s x_0 w_2 x_MLOAD_0 2 w)))
 (let ((?x6852 (storage_s x_0 w_2 x_MLOAD_0 3 w)))
 (= ?x6852 ?x7989))))
 ))
 (let (($x9098 (forall ((n (_ BitVec 6)) )(let ((?x218 (sc_s 2)))
 (let ((?x7113 (bvadd (_ bv63 6) ?x218)))
 (let (($x10404 (bvsle ?x7113 n)))
 (let ((?x8729 (stack_s x_0 w_2 x_MLOAD_0 2 n)))
 (let ((?x7978 (stack_s x_0 w_2 x_MLOAD_0 3 n)))
 (let (($x5355 (= ?x7978 ?x8729)))
 (or $x5355 $x10404))))))))
 ))
 (let ((?x218 (sc_s 2)))
 (let (($x6815 (= ?x275 ?x218)))
 (let ((?x4652 (used_gas_s x_0 w_2 x_MLOAD_0 3)))
 (let (($x246 (= ?x4652 (+ 3 (used_gas_s x_0 w_2 x_MLOAD_0 2)))))
 (let ((?x7113 (bvadd (_ bv63 6) ?x218)))
 (let ((?x8018 (stack_s x_0 w_2 x_MLOAD_0 2 ?x7113)))
 (let ((?x9635 (bvadd (_ bv63 6) ?x275)))
 (let ((?x4987 (stack_s x_0 w_2 x_MLOAD_0 3 ?x9635)))
 (let (($x7459 (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_s 1))))))
 (let (($x1803 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 1)))) (_ bv0 1)))))
 (let (($x189 (exc_halt_s 1)))
 (let (($x247 (exc_halt_s 2)))
 (let (($x355 (= $x247 (or $x189 $x1803 $x7459))))
 (let (($x3167 (forall ((w (_ BitVec 256)) )(let ((?x11518 (storage_s x_0 w_2 x_MLOAD_0 1 w)))
 (let ((?x7989 (storage_s x_0 w_2 x_MLOAD_0 2 w)))
 (= ?x7989 ?x11518))))
 ))
 (let (($x7717 (forall ((n (_ BitVec 6)) )(let ((?x154 (sc_s 1)))
 (let ((?x11673 (bvadd (_ bv63 6) ?x154)))
 (let (($x10494 (bvsle ?x11673 n)))
 (let ((?x4967 (stack_s x_0 w_2 x_MLOAD_0 1 n)))
 (let ((?x8729 (stack_s x_0 w_2 x_MLOAD_0 2 n)))
 (or (= ?x8729 ?x4967) $x10494)))))))
 ))
 (let (($x11672 (= ?x218 (bvadd (_ bv1 6) (sc_s 1)))))
 (let ((?x253 (used_gas_s x_0 w_2 x_MLOAD_0 2)))
 (let (($x4760 (= ?x253 (+ 3 (used_gas_s x_0 w_2 x_MLOAD_0 1)))))
 (let ((?x154 (sc_s 1)))
 (let ((?x11673 (bvadd (_ bv63 6) ?x154)))
 (let ((?x6359 (stack_s x_0 w_2 x_MLOAD_0 1 ?x11673)))
 (let (($x9208 (= $x189 (or $x57 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x72))) (_ bv0 1)))))))
 (let (($x1371 (forall ((w (_ BitVec 256)) )(let ((?x7274 (storage_s x_0 w_2 x_MLOAD_0 0 w)))
 (let ((?x11518 (storage_s x_0 w_2 x_MLOAD_0 1 w)))
 (= ?x11518 ?x7274))))
 ))
 (let (($x6208 (forall ((n (_ BitVec 6)) )(let ((?x72 (sc_s 0)))
 (let (($x10612 (bvsle ?x72 n)))
 (let ((?x851 (stack_s x_0 w_2 x_MLOAD_0 0 n)))
 (let ((?x4967 (stack_s x_0 w_2 x_MLOAD_0 1 n)))
 (let (($x1531 (= ?x4967 ?x851)))
 (or $x1531 $x10612)))))))
 ))
 (let (($x6169 (= ?x154 (bvadd (_ bv1 6) ?x72))))
 (let ((?x10492 (used_gas_s x_0 w_2 x_MLOAD_0 1)))
 (let (($x5591 (= ?x10492 (+ 3 ?x1688))))
 (let (($x3606 (forall ((w0 (_ BitVec 256)) )(let ((?x8579 (ite (= (stack_s x_0 w_2 x_MLOAD_0 2 (bvadd (_ bv63 6) (sc_s 2))) w0) x_MLOAD_0 (_ bv0 256))))
 (let ((?x11189 (f_MLOAD x_0 w_2 x_MLOAD_0 w0)))
 (= ?x11189 ?x8579))))
 ))
 (let (($x2972 (forall ((w (_ BitVec 256)) )(let ((?x7274 (storage_s x_0 w_2 x_MLOAD_0 0 w)))
 (= ?x7274 (_ bv0 256))))
 ))
 (let (($x8577 (= ?x1688 0)))
 (let (($x10766 (not $x57)))
 (let (($x79 (= (stack_s x_0 w_2 x_MLOAD_0 0 (_ bv0 6)) x_0)))
 (let (($x7865 (= ?x72 (_ bv1 6))))
 (and $x7865 $x79 $x10766 $x8577 $x2972 $x3606 (= (stack_s x_0 w_2 x_MLOAD_0 1 ?x72) w_2) $x5591 $x6169 $x6208 $x1371 $x9208 (= ?x8018 ?x6359) (= (stack_s x_0 w_2 x_MLOAD_0 2 ?x11673) ?x6359) $x4760 $x11672 $x7717 $x3167 $x355 (= ?x4987 (f_MLOAD x_0 w_2 x_MLOAD_0 ?x8018)) $x246 $x6815 $x9098 $x10778 $x9269 (= ?x3882 (stack_s x_0 w_2 x_MLOAD_0 3 (bvadd (_ bv61 6) ?x275))) (= ?x1515 ?x4987) (= ?x5275 (stack_s x_0 w_2 x_MLOAD_0 3 (bvadd (_ bv62 6) ?x275))) $x9743 $x2594 $x10058 $x7532 $x7694 (= ?x6243 ?x1515) (= (stack_s x_0 w_2 x_MLOAD_0 5 ?x8585) ?x1515) (= (stack_s x_0 w_2 x_MLOAD_0 5 ?x5394) ?x5275) (= (stack_s x_0 w_2 x_MLOAD_0 5 ?x4388) ?x3882) (= ?x5447 (+ 3 ?x11244)) $x10302 $x5943 $x674 (= $x11317 (or $x1305 $x7172 $x10728)) $x3423 (= (stack_s x_0 w_2 x_MLOAD_0 6 (bvadd (_ bv62 6) ?x926)) ?x6243) $x8220 $x3654 $x10292 $x2828 $x6274 $x10709 $x4627 $x6089 $x8629 $x9463 $x7349 (= (stack_t x_0 w_2 x_MLOAD_0 2 ?x7154) w_2) $x1588 $x135 $x11572 $x3844 $x11314 $x5562 $x8020 $x4836 $x23 $x6848 $x8365 (= ?x2002 ?x10072) (= (stack_t x_0 w_2 x_MLOAD_0 4 ?x5797) ?x10072) (= ?x10649 (+ 3 ?x1027)) $x10007 $x10878 $x3447 (= $x7854 (or $x740 $x3007 $x6783)) $x7467 (= (stack_t x_0 w_2 x_MLOAD_0 5 (bvadd (_ bv60 6) ?x919)) ?x2002) $x2582 $x5639 $x4858 (= ?x919 ?x4818) $x5891 $x1103 $x6936 $x73 $x6748 $x58 $x4642 $x8060 (not (and $x929 $x10055 $x889 $x5783)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
 )
(check-sat)
(get-proof)