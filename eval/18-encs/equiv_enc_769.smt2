; PUSH cw_3 DUP1 DUP1 CALLDATALOAD SWAP1 => PUSH cw_3 PUSH cw_3 CALLDATALOAD DUP2
(set-option :produce-proofs true)
; 
(set-info :status unknown)
(declare-fun storage_t ((_ BitVec 256) (_ BitVec 256) Int (_ BitVec 256)) (_ BitVec 256))
(declare-fun w_3 () (_ BitVec 256))
(declare-fun storage_s ((_ BitVec 256) (_ BitVec 256) Int (_ BitVec 256)) (_ BitVec 256))
(declare-fun exc_halt_t (Int) Bool)
(declare-fun exc_halt_s (Int) Bool)
(declare-fun stack_t ((_ BitVec 256) (_ BitVec 256) Int (_ BitVec 6)) (_ BitVec 256))
(declare-fun stack_s ((_ BitVec 256) (_ BitVec 256) Int (_ BitVec 6)) (_ BitVec 256))
(declare-fun sc_t (Int) (_ BitVec 6))
(declare-fun sc_s (Int) (_ BitVec 6))
(declare-fun used_gas_t ((_ BitVec 256) (_ BitVec 256) Int) Int)
(declare-fun used_gas_s ((_ BitVec 256) (_ BitVec 256) Int) Int)
(declare-fun f_CALLDATALOAD ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256)) (_ BitVec 256))
(assert
 (exists ((x_CALLDATALOAD_0 (_ BitVec 256)) )(let (($x5377 (forall ((w (_ BitVec 256)) )(let ((?x6740 (storage_t w_3 x_CALLDATALOAD_0 4 w)))
 (let ((?x700 (storage_s w_3 x_CALLDATALOAD_0 5 w)))
 (= ?x700 ?x6740))))
 ))
 (let (($x7854 (exc_halt_t 4)))
 (let (($x1862 (exc_halt_s 5)))
 (let (($x2802 (= $x1862 $x7854)))
 (let (($x691 (forall ((n (_ BitVec 6)) )(let ((?x2330 (stack_t w_3 x_CALLDATALOAD_0 4 n)))
 (let ((?x5162 (stack_s w_3 x_CALLDATALOAD_0 5 n)))
 (let (($x11843 (= ?x5162 ?x2330)))
 (let ((?x8101 (sc_t 4)))
 (let (($x4028 (bvsle ?x8101 n)))
 (or $x4028 $x11843)))))))
 ))
 (let ((?x8101 (sc_t 4)))
 (let ((?x4319 (sc_s 5)))
 (let (($x2078 (= ?x4319 ?x8101)))
 (let ((?x10362 (used_gas_t w_3 x_CALLDATALOAD_0 0)))
 (let ((?x383 (used_gas_s w_3 x_CALLDATALOAD_0 0)))
 (let (($x7946 (= ?x383 ?x10362)))
 (let (($x3491 (forall ((w (_ BitVec 256)) )(let ((?x2338 (storage_t w_3 x_CALLDATALOAD_0 0 w)))
 (let ((?x7606 (storage_s w_3 x_CALLDATALOAD_0 0 w)))
 (= ?x7606 ?x2338))))
 ))
 (let (($x56 (exc_halt_t 0)))
 (let (($x57 (exc_halt_s 0)))
 (let (($x58 (= $x57 $x56)))
 (let (($x1139 (forall ((n (_ BitVec 6)) )(let ((?x5643 (stack_t w_3 x_CALLDATALOAD_0 0 n)))
 (let ((?x6981 (stack_s w_3 x_CALLDATALOAD_0 0 n)))
 (let (($x1375 (= ?x6981 ?x5643)))
 (let ((?x63 (sc_t 0)))
 (let (($x632 (bvsle ?x63 n)))
 (or $x632 $x1375)))))))
 ))
 (let ((?x63 (sc_t 0)))
 (let ((?x72 (sc_s 0)))
 (let (($x73 (= ?x72 ?x63)))
 (let (($x11169 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_t 3)))) (_ bv0 1)))))
 (let (($x8096 (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_t 3))))))
 (let (($x4112 (exc_halt_t 3)))
 (let (($x2571 (forall ((w (_ BitVec 256)) )(let ((?x8597 (storage_t w_3 x_CALLDATALOAD_0 3 w)))
 (let ((?x6740 (storage_t w_3 x_CALLDATALOAD_0 4 w)))
 (= ?x6740 ?x8597))))
 ))
 (let (($x11766 (forall ((n (_ BitVec 6)) )(let ((?x11964 (sc_t 3)))
 (let ((?x10044 (bvadd (_ bv62 6) ?x11964)))
 (let (($x8156 (bvsle ?x10044 n)))
 (or $x8156 (= (stack_t w_3 x_CALLDATALOAD_0 4 n) (stack_t w_3 x_CALLDATALOAD_0 3 n)))))))
 ))
 (let (($x6788 (= ?x8101 (bvadd (_ bv1 6) (sc_t 3)))))
 (let (($x4600 (= (used_gas_t w_3 x_CALLDATALOAD_0 4) (+ 3 (used_gas_t w_3 x_CALLDATALOAD_0 3)))))
 (let ((?x11964 (sc_t 3)))
 (let ((?x3587 (bvadd (_ bv63 6) ?x11964)))
 (let ((?x8738 (stack_t w_3 x_CALLDATALOAD_0 3 ?x3587)))
 (let ((?x10044 (bvadd (_ bv62 6) ?x11964)))
 (let ((?x5286 (stack_t w_3 x_CALLDATALOAD_0 3 ?x10044)))
 (let (($x3954 (= $x4112 (or (exc_halt_t 2) (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_t 2))))))))
 (let (($x2931 (forall ((w (_ BitVec 256)) )(let ((?x8647 (storage_t w_3 x_CALLDATALOAD_0 2 w)))
 (let ((?x8597 (storage_t w_3 x_CALLDATALOAD_0 3 w)))
 (= ?x8597 ?x8647))))
 ))
 (let (($x10102 (forall ((n (_ BitVec 6)) )(let ((?x6602 (sc_t 2)))
 (let ((?x8103 (bvadd (_ bv63 6) ?x6602)))
 (let (($x3046 (bvsle ?x8103 n)))
 (or $x3046 (= (stack_t w_3 x_CALLDATALOAD_0 3 n) (stack_t w_3 x_CALLDATALOAD_0 2 n)))))))
 ))
 (let ((?x6602 (sc_t 2)))
 (let (($x11444 (= ?x11964 ?x6602)))
 (let ((?x3902 (used_gas_t w_3 x_CALLDATALOAD_0 3)))
 (let ((?x3502 (f_CALLDATALOAD w_3 x_CALLDATALOAD_0 (stack_t w_3 x_CALLDATALOAD_0 2 (bvadd (_ bv63 6) ?x6602)))))
 (let (($x6468 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_t 1)))) (_ bv0 1)))))
 (let (($x5011 (exc_halt_t 1)))
 (let (($x9181 (exc_halt_t 2)))
 (let (($x11152 (= $x9181 (or $x5011 $x6468))))
 (let (($x6340 (forall ((w (_ BitVec 256)) )(let ((?x601 (storage_t w_3 x_CALLDATALOAD_0 1 w)))
 (let ((?x8647 (storage_t w_3 x_CALLDATALOAD_0 2 w)))
 (= ?x8647 ?x601))))
 ))
 (let (($x8489 (forall ((n (_ BitVec 6)) )(let ((?x6377 (sc_t 1)))
 (let (($x3006 (bvsle ?x6377 n)))
 (or $x3006 (= (stack_t w_3 x_CALLDATALOAD_0 2 n) (stack_t w_3 x_CALLDATALOAD_0 1 n))))))
 ))
 (let (($x3613 (= ?x6602 (bvadd (_ bv1 6) (sc_t 1)))))
 (let ((?x5882 (used_gas_t w_3 x_CALLDATALOAD_0 2)))
 (let (($x7243 (= $x5011 (or $x56 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x63))) (_ bv0 1)))))))
 (let (($x3911 (forall ((w (_ BitVec 256)) )(let ((?x2338 (storage_t w_3 x_CALLDATALOAD_0 0 w)))
 (let ((?x601 (storage_t w_3 x_CALLDATALOAD_0 1 w)))
 (= ?x601 ?x2338))))
 ))
 (let (($x827 (forall ((n (_ BitVec 6)) )(let ((?x63 (sc_t 0)))
 (let (($x632 (bvsle ?x63 n)))
 (or $x632 (= (stack_t w_3 x_CALLDATALOAD_0 1 n) (stack_t w_3 x_CALLDATALOAD_0 0 n))))))
 ))
 (let ((?x6377 (sc_t 1)))
 (let (($x11310 (= ?x6377 (bvadd (_ bv1 6) ?x63))))
 (let (($x5223 (= $x1862 (or (exc_halt_s 4) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 4))))))))
 (let (($x3736 (forall ((w (_ BitVec 256)) )(let ((?x11376 (storage_s w_3 x_CALLDATALOAD_0 4 w)))
 (let ((?x700 (storage_s w_3 x_CALLDATALOAD_0 5 w)))
 (= ?x700 ?x11376))))
 ))
 (let (($x8651 (forall ((n (_ BitVec 6)) )(let ((?x500 (sc_s 4)))
 (let ((?x7626 (bvadd (_ bv62 6) ?x500)))
 (let (($x2358 (bvsle ?x7626 n)))
 (or $x2358 (= (stack_s w_3 x_CALLDATALOAD_0 5 n) (stack_s w_3 x_CALLDATALOAD_0 4 n)))))))
 ))
 (let ((?x500 (sc_s 4)))
 (let (($x8952 (= ?x4319 ?x500)))
 (let (($x3487 (= (used_gas_s w_3 x_CALLDATALOAD_0 5) (+ 3 (used_gas_s w_3 x_CALLDATALOAD_0 4)))))
 (let ((?x4132 (bvadd (_ bv63 6) ?x500)))
 (let ((?x2863 (stack_s w_3 x_CALLDATALOAD_0 4 ?x4132)))
 (let (($x4678 (= (stack_s w_3 x_CALLDATALOAD_0 5 (bvadd (_ bv63 6) ?x4319)) (stack_s w_3 x_CALLDATALOAD_0 4 (bvadd (_ bv62 6) ?x500)))))
 (let (($x3743 (exc_halt_s 4)))
 (let (($x11166 (= $x3743 (or (exc_halt_s 3) (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_s 3))))))))
 (let (($x3417 (forall ((w (_ BitVec 256)) )(let ((?x8726 (storage_s w_3 x_CALLDATALOAD_0 3 w)))
 (let ((?x11376 (storage_s w_3 x_CALLDATALOAD_0 4 w)))
 (= ?x11376 ?x8726))))
 ))
 (let (($x6936 (forall ((n (_ BitVec 6)) )(let ((?x1974 (sc_s 3)))
 (let ((?x2640 (bvadd (_ bv63 6) ?x1974)))
 (let (($x5751 (bvsle ?x2640 n)))
 (or $x5751 (= (stack_s w_3 x_CALLDATALOAD_0 4 n) (stack_s w_3 x_CALLDATALOAD_0 3 n)))))))
 ))
 (let ((?x1974 (sc_s 3)))
 (let (($x7752 (= ?x500 ?x1974)))
 (let ((?x8524 (used_gas_s w_3 x_CALLDATALOAD_0 4)))
 (let ((?x2640 (bvadd (_ bv63 6) ?x1974)))
 (let ((?x8159 (stack_s w_3 x_CALLDATALOAD_0 3 ?x2640)))
 (let (($x9761 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 2)))) (_ bv0 1)))))
 (let (($x5122 (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_s 2))))))
 (let (($x3200 (exc_halt_s 2)))
 (let (($x8780 (exc_halt_s 3)))
 (let (($x8643 (forall ((w (_ BitVec 256)) )(let ((?x3509 (storage_s w_3 x_CALLDATALOAD_0 2 w)))
 (let ((?x8726 (storage_s w_3 x_CALLDATALOAD_0 3 w)))
 (= ?x8726 ?x3509))))
 ))
 (let (($x6747 (forall ((n (_ BitVec 6)) )(let ((?x2620 (sc_s 2)))
 (let ((?x5957 (bvadd (_ bv63 6) ?x2620)))
 (let (($x9374 (bvsle ?x5957 n)))
 (or (= (stack_s w_3 x_CALLDATALOAD_0 3 n) (stack_s w_3 x_CALLDATALOAD_0 2 n)) $x9374)))))
 ))
 (let (($x8893 (= ?x1974 (bvadd (_ bv1 6) (sc_s 2)))))
 (let ((?x4844 (used_gas_s w_3 x_CALLDATALOAD_0 3)))
 (let ((?x2620 (sc_s 2)))
 (let ((?x5957 (bvadd (_ bv63 6) ?x2620)))
 (let ((?x59 (stack_s w_3 x_CALLDATALOAD_0 2 ?x5957)))
 (let (($x10558 (exc_halt_s 1)))
 (let (($x10267 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 1)))) (_ bv0 1)))))
 (let (($x6476 (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_s 1))))))
 (let (($x10325 (= $x3200 (or $x6476 $x10267 $x10558))))
 (let (($x7485 (forall ((w (_ BitVec 256)) )(let ((?x7710 (storage_s w_3 x_CALLDATALOAD_0 1 w)))
 (let ((?x3509 (storage_s w_3 x_CALLDATALOAD_0 2 w)))
 (= ?x3509 ?x7710))))
 ))
 (let (($x4979 (forall ((n (_ BitVec 6)) )(let ((?x154 (sc_s 1)))
 (let ((?x8586 (bvadd (_ bv63 6) ?x154)))
 (let (($x4241 (bvsle ?x8586 n)))
 (or $x4241 (= (stack_s w_3 x_CALLDATALOAD_0 2 n) (stack_s w_3 x_CALLDATALOAD_0 1 n)))))))
 ))
 (let (($x10641 (= ?x2620 (bvadd (_ bv1 6) (sc_s 1)))))
 (let ((?x1711 (used_gas_s w_3 x_CALLDATALOAD_0 2)))
 (let ((?x154 (sc_s 1)))
 (let ((?x8586 (bvadd (_ bv63 6) ?x154)))
 (let ((?x3196 (stack_s w_3 x_CALLDATALOAD_0 1 ?x8586)))
 (let (($x5896 (= $x10558 (or $x57 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x72))) (_ bv0 1)))))))
 (let (($x5036 (forall ((w (_ BitVec 256)) )(let ((?x7606 (storage_s w_3 x_CALLDATALOAD_0 0 w)))
 (let ((?x7710 (storage_s w_3 x_CALLDATALOAD_0 1 w)))
 (= ?x7710 ?x7606))))
 ))
 (let (($x6218 (forall ((n (_ BitVec 6)) )(let ((?x72 (sc_s 0)))
 (let (($x10377 (bvsle ?x72 n)))
 (or $x10377 (= (stack_s w_3 x_CALLDATALOAD_0 1 n) (stack_s w_3 x_CALLDATALOAD_0 0 n))))))
 ))
 (let (($x7558 (= ?x154 (bvadd (_ bv1 6) ?x72))))
 (let (($x4875 (forall ((w0 (_ BitVec 256)) )(let ((?x1632 (ite (= (stack_s w_3 x_CALLDATALOAD_0 3 (bvadd (_ bv63 6) (sc_s 3))) w0) x_CALLDATALOAD_0 (_ bv0 256))))
 (let ((?x9251 (f_CALLDATALOAD w_3 x_CALLDATALOAD_0 w0)))
 (= ?x9251 ?x1632))))
 ))
 (let (($x6693 (forall ((w (_ BitVec 256)) )(let ((?x7606 (storage_s w_3 x_CALLDATALOAD_0 0 w)))
 (= ?x7606 (_ bv0 256))))
 ))
 (let (($x2937 (= ?x383 0)))
 (let (($x10917 (not $x57)))
 (let (($x1978 (= ?x72 (_ bv0 6))))
 (and $x1978 $x10917 $x2937 $x6693 $x4875 (= (stack_s w_3 x_CALLDATALOAD_0 1 ?x72) w_3) (= (used_gas_s w_3 x_CALLDATALOAD_0 1) (+ 3 ?x383)) $x7558 $x6218 $x5036 $x5896 (= ?x59 ?x3196) (= (stack_s w_3 x_CALLDATALOAD_0 2 ?x8586) ?x3196) (= ?x1711 (+ 3 (used_gas_s w_3 x_CALLDATALOAD_0 1))) $x10641 $x4979 $x7485 $x10325 (= ?x8159 ?x59) (= (stack_s w_3 x_CALLDATALOAD_0 3 ?x5957) ?x59) (= ?x4844 (+ 3 ?x1711)) $x8893 $x6747 $x8643 (= $x8780 (or $x3200 $x5122 $x9761)) (= ?x2863 (f_CALLDATALOAD w_3 x_CALLDATALOAD_0 ?x8159)) (= ?x8524 (+ 3 ?x4844)) $x7752 $x6936 $x3417 $x11166 $x4678 (= (stack_s w_3 x_CALLDATALOAD_0 5 (bvadd (_ bv62 6) ?x4319)) ?x2863) $x3487 $x8952 $x8651 $x3736 $x5223 (= (stack_t w_3 x_CALLDATALOAD_0 1 ?x63) w_3) (= (used_gas_t w_3 x_CALLDATALOAD_0 1) (+ 3 ?x10362)) $x11310 $x827 $x3911 $x7243 (= (stack_t w_3 x_CALLDATALOAD_0 2 ?x6377) w_3) (= ?x5882 (+ 3 (used_gas_t w_3 x_CALLDATALOAD_0 1))) $x3613 $x8489 $x6340 $x11152 (= ?x8738 ?x3502) (= ?x3902 (+ 3 ?x5882)) $x11444 $x10102 $x2931 $x3954 (= (stack_t w_3 x_CALLDATALOAD_0 4 (bvadd (_ bv63 6) ?x8101)) ?x5286) (= (stack_t w_3 x_CALLDATALOAD_0 4 ?x10044) ?x5286) (= (stack_t w_3 x_CALLDATALOAD_0 4 ?x3587) ?x8738) $x4600 $x6788 $x11766 $x2571 (= $x7854 (or $x4112 $x8096 $x11169)) $x73 $x1139 $x58 $x3491 $x7946 (not (and $x2078 $x691 $x2802 $x5377))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
 )
(check-sat)
(get-proof)