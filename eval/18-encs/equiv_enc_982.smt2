; DUP1 DUP4 DUP4 DUP3 SWAP1 => DUP1 DUP4 DUP3 DUP5
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
 (exists ((x_0 (_ BitVec 256)) (x_1 (_ BitVec 256)) (x_2 (_ BitVec 256)) )(let (($x1301 (forall ((w (_ BitVec 256)) )(let ((?x11484 (storage_t x_0 x_1 x_2 4 w)))
 (let ((?x6471 (storage_s x_0 x_1 x_2 5 w)))
 (= ?x6471 ?x11484))))
 ))
 (let (($x7854 (exc_halt_t 4)))
 (let (($x1862 (exc_halt_s 5)))
 (let (($x11605 (= $x1862 $x7854)))
 (let (($x4302 (forall ((n (_ BitVec 6)) )(let ((?x2388 (stack_t x_0 x_1 x_2 4 n)))
 (let ((?x7174 (stack_s x_0 x_1 x_2 5 n)))
 (let (($x10730 (= ?x7174 ?x2388)))
 (let ((?x7495 (sc_t 4)))
 (let (($x6775 (bvsle ?x7495 n)))
 (or $x6775 $x10730)))))))
 ))
 (let ((?x7495 (sc_t 4)))
 (let ((?x4319 (sc_s 5)))
 (let (($x11865 (= ?x4319 ?x7495)))
 (let ((?x10808 (used_gas_t x_0 x_1 x_2 0)))
 (let ((?x10708 (used_gas_s x_0 x_1 x_2 0)))
 (let (($x3132 (= ?x10708 ?x10808)))
 (let (($x1216 (forall ((w (_ BitVec 256)) )(let ((?x5626 (storage_t x_0 x_1 x_2 0 w)))
 (let ((?x3677 (storage_s x_0 x_1 x_2 0 w)))
 (= ?x3677 ?x5626))))
 ))
 (let (($x56 (exc_halt_t 0)))
 (let (($x57 (exc_halt_s 0)))
 (let (($x58 (= $x57 $x56)))
 (let (($x3459 (forall ((n (_ BitVec 6)) )(let ((?x5952 (stack_t x_0 x_1 x_2 0 n)))
 (let ((?x6109 (stack_s x_0 x_1 x_2 0 n)))
 (let (($x7997 (= ?x6109 ?x5952)))
 (let ((?x63 (sc_t 0)))
 (let (($x6655 (bvsle ?x63 n)))
 (or $x6655 $x7997)))))))
 ))
 (let ((?x63 (sc_t 0)))
 (let ((?x72 (sc_s 0)))
 (let (($x73 (= ?x72 ?x63)))
 (let (($x8164 (exc_halt_t 3)))
 (let (($x4848 (not (bvsle (_ bv0 6) (bvadd (_ bv59 6) (sc_t 3))))))
 (let (($x3792 (or $x4848 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_t 3)))) (_ bv0 1))) $x8164)))
 (let (($x10855 (forall ((w (_ BitVec 256)) )(let ((?x8146 (storage_t x_0 x_1 x_2 3 w)))
 (let ((?x11484 (storage_t x_0 x_1 x_2 4 w)))
 (= ?x11484 ?x8146))))
 ))
 (let (($x11250 (forall ((n (_ BitVec 6)) )(let ((?x7477 (stack_t x_0 x_1 x_2 3 n)))
 (let ((?x2388 (stack_t x_0 x_1 x_2 4 n)))
 (or (bvsle (bvadd (_ bv59 6) (sc_t 3)) n) (= ?x2388 ?x7477)))))
 ))
 (let (($x3534 (= (used_gas_t x_0 x_1 x_2 4) (+ 3 (used_gas_t x_0 x_1 x_2 3)))))
 (let ((?x7878 (sc_t 3)))
 (let ((?x2884 (bvadd (_ bv63 6) ?x7878)))
 (let ((?x2885 (stack_t x_0 x_1 x_2 3 ?x2884)))
 (let ((?x9693 (bvadd (_ bv62 6) ?x7878)))
 (let ((?x5979 (stack_t x_0 x_1 x_2 3 ?x9693)))
 (let ((?x160 (bvadd (_ bv61 6) ?x7878)))
 (let ((?x2737 (stack_t x_0 x_1 x_2 3 ?x160)))
 (let ((?x4411 (bvadd (_ bv60 6) ?x7878)))
 (let ((?x5460 (stack_t x_0 x_1 x_2 3 ?x4411)))
 (let ((?x5074 (bvadd (_ bv59 6) ?x7878)))
 (let ((?x10423 (stack_t x_0 x_1 x_2 3 ?x5074)))
 (let (($x4968 (= (stack_t x_0 x_1 x_2 4 (bvadd (_ bv63 6) ?x7495)) ?x10423)))
 (let (($x9258 (exc_halt_t 2)))
 (let (($x3476 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_t 2)))) (_ bv0 1)))))
 (let (($x5535 (forall ((w (_ BitVec 256)) )(let ((?x5167 (storage_t x_0 x_1 x_2 2 w)))
 (let ((?x8146 (storage_t x_0 x_1 x_2 3 w)))
 (= ?x8146 ?x5167))))
 ))
 (let (($x2491 (forall ((n (_ BitVec 6)) )(let ((?x4772 (stack_t x_0 x_1 x_2 2 n)))
 (let ((?x7477 (stack_t x_0 x_1 x_2 3 n)))
 (let (($x11421 (= ?x7477 ?x4772)))
 (or (bvsle (bvadd (_ bv61 6) (sc_t 2)) n) $x11421)))))
 ))
 (let (($x2121 (= ?x7878 (bvadd (_ bv1 6) (sc_t 2)))))
 (let ((?x7313 (used_gas_t x_0 x_1 x_2 3)))
 (let (($x681 (= ?x7313 (+ 3 (used_gas_t x_0 x_1 x_2 2)))))
 (let ((?x4896 (sc_t 2)))
 (let ((?x7366 (bvadd (_ bv63 6) ?x4896)))
 (let ((?x8364 (stack_t x_0 x_1 x_2 2 ?x7366)))
 (let (($x2255 (= (stack_t x_0 x_1 x_2 3 ?x7366) ?x8364)))
 (let ((?x4054 (bvadd (_ bv62 6) ?x4896)))
 (let ((?x5705 (stack_t x_0 x_1 x_2 2 ?x4054)))
 (let (($x3131 (= (stack_t x_0 x_1 x_2 3 ?x4054) ?x5705)))
 (let ((?x1535 (bvadd (_ bv61 6) ?x4896)))
 (let ((?x11334 (stack_t x_0 x_1 x_2 2 ?x1535)))
 (let (($x7526 (exc_halt_t 1)))
 (let (($x6309 (or (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_t 1)))) (_ bv0 1))) (not (bvsle (_ bv0 6) (bvadd (_ bv60 6) (sc_t 1)))) $x7526)))
 (let (($x5016 (forall ((w (_ BitVec 256)) )(let ((?x5145 (storage_t x_0 x_1 x_2 1 w)))
 (let ((?x5167 (storage_t x_0 x_1 x_2 2 w)))
 (= ?x5167 ?x5145))))
 ))
 (let (($x9980 (forall ((n (_ BitVec 6)) )(let ((?x8339 (stack_t x_0 x_1 x_2 1 n)))
 (let ((?x4772 (stack_t x_0 x_1 x_2 2 n)))
 (let (($x4610 (= ?x4772 ?x8339)))
 (or (bvsle (bvadd (_ bv60 6) (sc_t 1)) n) $x4610)))))
 ))
 (let ((?x7512 (used_gas_t x_0 x_1 x_2 2)))
 (let (($x887 (= ?x7512 (+ 3 (used_gas_t x_0 x_1 x_2 1)))))
 (let ((?x1473 (sc_t 1)))
 (let ((?x11928 (bvadd (_ bv63 6) ?x1473)))
 (let ((?x11107 (stack_t x_0 x_1 x_2 1 ?x11928)))
 (let ((?x7015 (bvadd (_ bv62 6) ?x1473)))
 (let ((?x8933 (stack_t x_0 x_1 x_2 1 ?x7015)))
 (let (($x3592 (= (stack_t x_0 x_1 x_2 2 (bvadd (_ bv61 6) ?x1473)) (stack_t x_0 x_1 x_2 1 (bvadd (_ bv61 6) ?x1473)))))
 (let ((?x7104 (bvadd (_ bv60 6) ?x1473)))
 (let ((?x6784 (stack_t x_0 x_1 x_2 1 ?x7104)))
 (let (($x6472 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x63))) (_ bv0 1)))))
 (let (($x1201 (forall ((w (_ BitVec 256)) )(let ((?x5626 (storage_t x_0 x_1 x_2 0 w)))
 (let ((?x5145 (storage_t x_0 x_1 x_2 1 w)))
 (= ?x5145 ?x5626))))
 ))
 (let (($x11106 (forall ((n (_ BitVec 6)) )(let ((?x5952 (stack_t x_0 x_1 x_2 0 n)))
 (let ((?x8339 (stack_t x_0 x_1 x_2 1 n)))
 (let (($x9898 (= ?x8339 ?x5952)))
 (or (bvsle (bvadd (_ bv63 6) (sc_t 0)) n) $x9898)))))
 ))
 (let (($x5345 (= ?x1473 (bvadd (_ bv1 6) ?x63))))
 (let ((?x1817 (used_gas_t x_0 x_1 x_2 1)))
 (let (($x6132 (= ?x1817 (+ 3 ?x10808))))
 (let ((?x3284 (bvadd (_ bv63 6) ?x63)))
 (let ((?x11174 (stack_t x_0 x_1 x_2 0 ?x3284)))
 (let (($x6062 (= (stack_t x_0 x_1 x_2 1 ?x3284) ?x11174)))
 (let (($x269 (= $x1862 (or (exc_halt_s 4) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 4))))))))
 (let (($x7206 (forall ((w (_ BitVec 256)) )(let ((?x9279 (storage_s x_0 x_1 x_2 4 w)))
 (let ((?x6471 (storage_s x_0 x_1 x_2 5 w)))
 (= ?x6471 ?x9279))))
 ))
 (let (($x5061 (forall ((n (_ BitVec 6)) )(let ((?x1174 (stack_s x_0 x_1 x_2 4 n)))
 (let ((?x7174 (stack_s x_0 x_1 x_2 5 n)))
 (let (($x11948 (= ?x7174 ?x1174)))
 (or (bvsle (bvadd (_ bv62 6) (sc_s 4)) n) $x11948)))))
 ))
 (let ((?x9433 (sc_s 4)))
 (let (($x2805 (= ?x4319 ?x9433)))
 (let ((?x3865 (used_gas_s x_0 x_1 x_2 5)))
 (let (($x2103 (= ?x3865 (+ 3 (used_gas_s x_0 x_1 x_2 4)))))
 (let ((?x10009 (bvadd (_ bv63 6) ?x9433)))
 (let ((?x2218 (stack_s x_0 x_1 x_2 4 ?x10009)))
 (let ((?x6780 (bvadd (_ bv62 6) ?x4319)))
 (let ((?x9137 (stack_s x_0 x_1 x_2 5 ?x6780)))
 (let ((?x11088 (bvadd (_ bv62 6) ?x9433)))
 (let ((?x6821 (stack_s x_0 x_1 x_2 4 ?x11088)))
 (let ((?x11593 (bvadd (_ bv63 6) ?x4319)))
 (let ((?x8288 (stack_s x_0 x_1 x_2 5 ?x11593)))
 (let (($x8953 (not (bvsle (_ bv0 6) (bvadd (_ bv61 6) (sc_s 3))))))
 (let (($x8103 (exc_halt_s 3)))
 (let (($x4979 (or $x8103 $x8953 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 3)))) (_ bv0 1))))))
 (let (($x9175 (exc_halt_s 4)))
 (let (($x6265 (forall ((w (_ BitVec 256)) )(let ((?x2474 (storage_s x_0 x_1 x_2 3 w)))
 (let ((?x9279 (storage_s x_0 x_1 x_2 4 w)))
 (= ?x9279 ?x2474))))
 ))
 (let (($x11781 (forall ((n (_ BitVec 6)) )(let ((?x5434 (stack_s x_0 x_1 x_2 3 n)))
 (let ((?x1174 (stack_s x_0 x_1 x_2 4 n)))
 (or (= ?x1174 ?x5434) (bvsle (bvadd (_ bv61 6) (sc_s 3)) n)))))
 ))
 (let ((?x5134 (used_gas_s x_0 x_1 x_2 4)))
 (let (($x393 (= ?x5134 (+ 3 (used_gas_s x_0 x_1 x_2 3)))))
 (let ((?x3851 (sc_s 3)))
 (let ((?x5352 (bvadd (_ bv63 6) ?x3851)))
 (let ((?x6279 (stack_s x_0 x_1 x_2 3 ?x5352)))
 (let ((?x8586 (bvadd (_ bv62 6) ?x3851)))
 (let ((?x10643 (stack_s x_0 x_1 x_2 3 ?x8586)))
 (let ((?x9017 (bvadd (_ bv61 6) ?x3851)))
 (let ((?x10835 (stack_s x_0 x_1 x_2 3 ?x9017)))
 (let (($x2078 (= ?x2218 ?x10835)))
 (let (($x10052 (exc_halt_s 2)))
 (let (($x1625 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 2)))) (_ bv0 1)))))
 (let (($x9095 (forall ((w (_ BitVec 256)) )(let ((?x3347 (storage_s x_0 x_1 x_2 2 w)))
 (let ((?x2474 (storage_s x_0 x_1 x_2 3 w)))
 (= ?x2474 ?x3347))))
 ))
 (let (($x952 (forall ((n (_ BitVec 6)) )(let ((?x6446 (stack_s x_0 x_1 x_2 2 n)))
 (let ((?x5434 (stack_s x_0 x_1 x_2 3 n)))
 (let (($x186 (= ?x5434 ?x6446)))
 (or $x186 (bvsle (bvadd (_ bv60 6) (sc_s 2)) n))))))
 ))
 (let (($x9342 (= ?x3851 (bvadd (_ bv1 6) (sc_s 2)))))
 (let ((?x5793 (used_gas_s x_0 x_1 x_2 3)))
 (let (($x5043 (= ?x5793 (+ 3 (used_gas_s x_0 x_1 x_2 2)))))
 (let ((?x2272 (sc_s 2)))
 (let ((?x8521 (bvadd (_ bv63 6) ?x2272)))
 (let ((?x5959 (stack_s x_0 x_1 x_2 2 ?x8521)))
 (let (($x3317 (= (stack_s x_0 x_1 x_2 3 ?x8521) ?x5959)))
 (let ((?x6038 (bvadd (_ bv62 6) ?x2272)))
 (let ((?x4656 (stack_s x_0 x_1 x_2 2 ?x6038)))
 (let (($x1831 (= (stack_s x_0 x_1 x_2 3 ?x6038) ?x4656)))
 (let ((?x878 (bvadd (_ bv61 6) ?x2272)))
 (let ((?x11486 (stack_s x_0 x_1 x_2 2 ?x878)))
 (let (($x8169 (= (stack_s x_0 x_1 x_2 3 ?x878) ?x11486)))
 (let ((?x6409 (bvadd (_ bv60 6) ?x2272)))
 (let ((?x11699 (stack_s x_0 x_1 x_2 2 ?x6409)))
 (let (($x8780 (exc_halt_s 1)))
 (let (($x9830 (or (not (bvsle (_ bv0 6) (bvadd (_ bv60 6) (sc_s 1)))) $x8780 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 1)))) (_ bv0 1))))))
 (let (($x3575 (forall ((w (_ BitVec 256)) )(let ((?x5384 (storage_s x_0 x_1 x_2 1 w)))
 (let ((?x3347 (storage_s x_0 x_1 x_2 2 w)))
 (= ?x3347 ?x5384))))
 ))
 (let (($x7948 (forall ((n (_ BitVec 6)) )(let ((?x4948 (stack_s x_0 x_1 x_2 1 n)))
 (let ((?x6446 (stack_s x_0 x_1 x_2 2 n)))
 (let (($x3246 (= ?x6446 ?x4948)))
 (or $x3246 (bvsle (bvadd (_ bv60 6) (sc_s 1)) n))))))
 ))
 (let ((?x456 (used_gas_s x_0 x_1 x_2 2)))
 (let (($x8837 (= ?x456 (+ 3 (used_gas_s x_0 x_1 x_2 1)))))
 (let ((?x154 (sc_s 1)))
 (let ((?x11510 (bvadd (_ bv63 6) ?x154)))
 (let ((?x396 (stack_s x_0 x_1 x_2 1 ?x11510)))
 (let ((?x10884 (bvadd (_ bv62 6) ?x154)))
 (let ((?x4803 (stack_s x_0 x_1 x_2 1 ?x10884)))
 (let ((?x3048 (bvadd (_ bv61 6) ?x154)))
 (let ((?x1967 (stack_s x_0 x_1 x_2 1 ?x3048)))
 (let ((?x8017 (bvadd (_ bv60 6) ?x154)))
 (let ((?x2009 (stack_s x_0 x_1 x_2 1 ?x8017)))
 (let (($x984 (or $x57 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x72))) (_ bv0 1))) (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) ?x72))))))
 (let (($x1790 (forall ((w (_ BitVec 256)) )(let ((?x3677 (storage_s x_0 x_1 x_2 0 w)))
 (let ((?x5384 (storage_s x_0 x_1 x_2 1 w)))
 (= ?x5384 ?x3677))))
 ))
 (let (($x10576 (forall ((n (_ BitVec 6)) )(let ((?x6109 (stack_s x_0 x_1 x_2 0 n)))
 (let ((?x4948 (stack_s x_0 x_1 x_2 1 n)))
 (let (($x3859 (= ?x4948 ?x6109)))
 (or $x3859 (bvsle (bvadd (_ bv63 6) (sc_s 0)) n))))))
 ))
 (let ((?x1659 (used_gas_s x_0 x_1 x_2 1)))
 (let (($x10495 (= ?x1659 (+ 3 ?x10708))))
 (let ((?x1118 (bvadd (_ bv63 6) ?x72)))
 (let ((?x1629 (stack_s x_0 x_1 x_2 0 ?x1118)))
 (let (($x10662 (forall ((w (_ BitVec 256)) )(let ((?x3677 (storage_s x_0 x_1 x_2 0 w)))
 (= ?x3677 (_ bv0 256))))
 ))
 (let (($x7976 (= ?x10708 0)))
 (let (($x7433 (not $x57)))
 (let (($x3673 (= (stack_s x_0 x_1 x_2 0 (_ bv2 6)) x_2)))
 (let (($x4303 (= (stack_s x_0 x_1 x_2 0 (_ bv1 6)) x_1)))
 (let (($x2957 (= (stack_s x_0 x_1 x_2 0 (_ bv0 6)) x_0)))
 (let (($x9108 (= ?x72 (_ bv3 6))))
 (and $x9108 $x2957 $x4303 $x3673 $x7433 $x7976 $x10662 (= ?x396 ?x1629) (= (stack_s x_0 x_1 x_2 1 ?x1118) ?x1629) $x10495 (= ?x154 (bvadd (_ bv1 6) ?x72)) $x10576 $x1790 (= $x8780 $x984) (= ?x5959 ?x2009) (= (stack_s x_0 x_1 x_2 2 ?x8017) ?x2009) (= (stack_s x_0 x_1 x_2 2 ?x3048) ?x1967) (= (stack_s x_0 x_1 x_2 2 ?x10884) ?x4803) (= (stack_s x_0 x_1 x_2 2 ?x11510) ?x396) $x8837 (= ?x2272 (bvadd (_ bv1 6) ?x154)) $x7948 $x3575 (= $x10052 $x9830) (= ?x6279 ?x11699) (= (stack_s x_0 x_1 x_2 3 ?x6409) ?x11699) $x8169 $x1831 $x3317 $x5043 $x9342 $x952 $x9095 (= $x8103 (or $x1625 $x10052 (not (bvsle (_ bv0 6) ?x6409)))) $x2078 (= (stack_s x_0 x_1 x_2 4 ?x9017) ?x10835) (= (stack_s x_0 x_1 x_2 4 ?x8586) ?x10643) (= (stack_s x_0 x_1 x_2 4 ?x5352) ?x6279) $x393 (= ?x9433 (bvadd (_ bv1 6) ?x3851)) $x11781 $x6265 (= $x9175 $x4979) (= ?x8288 ?x6821) (= ?x9137 ?x2218) $x2103 $x2805 $x5061 $x7206 $x269 (= ?x11107 ?x11174) $x6062 $x6132 $x5345 $x11106 $x1201 (= $x7526 (or $x56 $x6472 (not (bvsle (_ bv0 6) ?x3284)))) (= ?x8364 ?x6784) (= (stack_t x_0 x_1 x_2 2 ?x7104) ?x6784) $x3592 (= (stack_t x_0 x_1 x_2 2 ?x7015) ?x8933) (= (stack_t x_0 x_1 x_2 2 ?x11928) ?x11107) $x887 (= ?x4896 (bvadd (_ bv1 6) ?x1473)) $x9980 $x5016 (= $x9258 $x6309) (= ?x2885 ?x11334) (= (stack_t x_0 x_1 x_2 3 ?x1535) ?x11334) $x3131 $x2255 $x681 $x2121 $x2491 $x5535 (= $x8164 (or (not (bvsle (_ bv0 6) ?x1535)) $x3476 $x9258)) $x4968 (= (stack_t x_0 x_1 x_2 4 ?x5074) ?x10423) (= (stack_t x_0 x_1 x_2 4 ?x4411) ?x5460) (= (stack_t x_0 x_1 x_2 4 ?x160) ?x2737) (= (stack_t x_0 x_1 x_2 4 ?x9693) ?x5979) (= (stack_t x_0 x_1 x_2 4 ?x2884) ?x2885) $x3534 (= ?x7495 (bvadd (_ bv1 6) ?x7878)) $x11250 $x10855 (= $x7854 $x3792) $x73 $x3459 $x58 $x1216 $x3132 (not (and $x11865 $x4302 $x11605 $x1301)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
 )
(check-sat)
(get-proof)