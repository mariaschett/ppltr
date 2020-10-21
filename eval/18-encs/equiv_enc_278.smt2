; DUP1 CALLDATALOAD SWAP2 SWAP1 PUSH cw_1 SWAP1 => PUSH cw_1 DUP2 CALLDATALOAD SWAP3 SWAP2
(set-option :produce-proofs true)
; 
(set-info :status unknown)
(declare-fun storage_t ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 256)) (_ BitVec 256))
(declare-fun w_1 () (_ BitVec 256))
(declare-fun storage_s ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 256)) (_ BitVec 256))
(declare-fun exc_halt_t (Int) Bool)
(declare-fun exc_halt_s (Int) Bool)
(declare-fun stack_t ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 6)) (_ BitVec 256))
(declare-fun stack_s ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 6)) (_ BitVec 256))
(declare-fun sc_t (Int) (_ BitVec 6))
(declare-fun sc_s (Int) (_ BitVec 6))
(declare-fun used_gas_t ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int) Int)
(declare-fun used_gas_s ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int) Int)
(declare-fun f_CALLDATALOAD ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256)) (_ BitVec 256))
(assert
 (exists ((x_0 (_ BitVec 256)) (x_1 (_ BitVec 256)) (x_CALLDATALOAD_0 (_ BitVec 256)) )(let (($x10934 (forall ((w (_ BitVec 256)) )(let ((?x1853 (storage_t x_0 x_1 w_1 x_CALLDATALOAD_0 5 w)))
 (let ((?x689 (storage_s x_0 x_1 w_1 x_CALLDATALOAD_0 6 w)))
 (= ?x689 ?x1853))))
 ))
 (let (($x886 (exc_halt_t 5)))
 (let (($x772 (exc_halt_s 6)))
 (let (($x889 (= $x772 $x886)))
 (let (($x6177 (forall ((n (_ BitVec 6)) )(let ((?x11441 (stack_t x_0 x_1 w_1 x_CALLDATALOAD_0 5 n)))
 (let ((?x4231 (stack_s x_0 x_1 w_1 x_CALLDATALOAD_0 6 n)))
 (let (($x6625 (= ?x4231 ?x11441)))
 (or (bvsle (sc_t 5) n) $x6625)))))
 ))
 (let ((?x919 (sc_t 5)))
 (let ((?x926 (sc_s 6)))
 (let (($x929 (= ?x926 ?x919)))
 (let ((?x2697 (used_gas_t x_0 x_1 w_1 x_CALLDATALOAD_0 0)))
 (let ((?x11425 (used_gas_s x_0 x_1 w_1 x_CALLDATALOAD_0 0)))
 (let (($x4989 (= ?x11425 ?x2697)))
 (let (($x3916 (forall ((w (_ BitVec 256)) )(let ((?x8450 (storage_t x_0 x_1 w_1 x_CALLDATALOAD_0 0 w)))
 (let ((?x3767 (storage_s x_0 x_1 w_1 x_CALLDATALOAD_0 0 w)))
 (= ?x3767 ?x8450))))
 ))
 (let (($x56 (exc_halt_t 0)))
 (let (($x57 (exc_halt_s 0)))
 (let (($x58 (= $x57 $x56)))
 (let (($x1128 (forall ((n (_ BitVec 6)) )(let ((?x63 (sc_t 0)))
 (let (($x9173 (bvsle ?x63 n)))
 (let ((?x11166 (stack_t x_0 x_1 w_1 x_CALLDATALOAD_0 0 n)))
 (let ((?x2342 (stack_s x_0 x_1 w_1 x_CALLDATALOAD_0 0 n)))
 (let (($x5561 (= ?x2342 ?x11166)))
 (or $x5561 $x9173)))))))
 ))
 (let ((?x63 (sc_t 0)))
 (let ((?x72 (sc_s 0)))
 (let (($x73 (= ?x72 ?x63)))
 (let (($x1125 (= $x886 (or (exc_halt_t 4) (not (bvsle (_ bv0 6) (bvadd (_ bv61 6) (sc_t 4))))))))
 (let (($x10737 (forall ((w (_ BitVec 256)) )(let ((?x10228 (storage_t x_0 x_1 w_1 x_CALLDATALOAD_0 4 w)))
 (let ((?x1853 (storage_t x_0 x_1 w_1 x_CALLDATALOAD_0 5 w)))
 (= ?x1853 ?x10228))))
 ))
 (let (($x11753 (forall ((n (_ BitVec 6)) )(let ((?x7941 (stack_t x_0 x_1 w_1 x_CALLDATALOAD_0 4 n)))
 (let ((?x11441 (stack_t x_0 x_1 w_1 x_CALLDATALOAD_0 5 n)))
 (or (bvsle (bvadd (_ bv61 6) (sc_t 4)) n) (= ?x11441 ?x7941)))))
 ))
 (let (($x1122 (= (used_gas_t x_0 x_1 w_1 x_CALLDATALOAD_0 5) (+ 3 (used_gas_t x_0 x_1 w_1 x_CALLDATALOAD_0 4)))))
 (let ((?x5270 (stack_t x_0 x_1 w_1 x_CALLDATALOAD_0 4 (bvadd (_ bv62 6) (sc_t 4)))))
 (let ((?x10661 (stack_t x_0 x_1 w_1 x_CALLDATALOAD_0 4 (bvadd (_ bv63 6) (sc_t 4)))))
 (let ((?x3757 (sc_t 4)))
 (let ((?x3171 (bvadd (_ bv61 6) ?x3757)))
 (let ((?x550 (stack_t x_0 x_1 w_1 x_CALLDATALOAD_0 4 ?x3171)))
 (let (($x3723 (exc_halt_t 4)))
 (let (($x1083 (= $x3723 (or (exc_halt_t 3) (not (bvsle (_ bv0 6) (bvadd (_ bv60 6) (sc_t 3))))))))
 (let (($x11605 (forall ((w (_ BitVec 256)) )(let ((?x4644 (storage_t x_0 x_1 w_1 x_CALLDATALOAD_0 3 w)))
 (let ((?x10228 (storage_t x_0 x_1 w_1 x_CALLDATALOAD_0 4 w)))
 (= ?x10228 ?x4644))))
 ))
 (let (($x11609 (forall ((n (_ BitVec 6)) )(let ((?x7705 (stack_t x_0 x_1 w_1 x_CALLDATALOAD_0 3 n)))
 (let ((?x7941 (stack_t x_0 x_1 w_1 x_CALLDATALOAD_0 4 n)))
 (or (= ?x7941 ?x7705) (bvsle (bvadd (_ bv60 6) (sc_t 3)) n)))))
 ))
 (let ((?x2397 (used_gas_t x_0 x_1 w_1 x_CALLDATALOAD_0 4)))
 (let (($x4921 (= ?x5270 (stack_t x_0 x_1 w_1 x_CALLDATALOAD_0 3 (bvadd (_ bv62 6) (sc_t 3))))))
 (let (($x10247 (= ?x550 (stack_t x_0 x_1 w_1 x_CALLDATALOAD_0 3 (bvadd (_ bv61 6) (sc_t 3))))))
 (let ((?x8025 (stack_t x_0 x_1 w_1 x_CALLDATALOAD_0 3 (bvadd (_ bv63 6) (sc_t 3)))))
 (let (($x5529 (= ?x10661 (stack_t x_0 x_1 w_1 x_CALLDATALOAD_0 3 (bvadd (_ bv60 6) (sc_t 3))))))
 (let (($x2075 (exc_halt_t 3)))
 (let (($x3252 (= $x2075 (or (exc_halt_t 2) (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_t 2))))))))
 (let (($x6542 (forall ((w (_ BitVec 256)) )(let ((?x9269 (storage_t x_0 x_1 w_1 x_CALLDATALOAD_0 2 w)))
 (let ((?x4644 (storage_t x_0 x_1 w_1 x_CALLDATALOAD_0 3 w)))
 (= ?x4644 ?x9269))))
 ))
 (let (($x7029 (forall ((n (_ BitVec 6)) )(let ((?x7808 (stack_t x_0 x_1 w_1 x_CALLDATALOAD_0 2 n)))
 (let ((?x7705 (stack_t x_0 x_1 w_1 x_CALLDATALOAD_0 3 n)))
 (let ((?x6629 (sc_t 2)))
 (let ((?x839 (bvadd (_ bv63 6) ?x6629)))
 (let (($x1190 (bvsle ?x839 n)))
 (or $x1190 (= ?x7705 ?x7808))))))))
 ))
 (let ((?x317 (used_gas_t x_0 x_1 w_1 x_CALLDATALOAD_0 3)))
 (let ((?x6629 (sc_t 2)))
 (let ((?x839 (bvadd (_ bv63 6) ?x6629)))
 (let ((?x169 (stack_t x_0 x_1 w_1 x_CALLDATALOAD_0 2 ?x839)))
 (let (($x7802 (exc_halt_t 1)))
 (let (($x9833 (or (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_t 1)))) $x7802 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_t 1)))) (_ bv0 1))))))
 (let (($x2810 (exc_halt_t 2)))
 (let (($x6926 (forall ((w (_ BitVec 256)) )(let ((?x1092 (storage_t x_0 x_1 w_1 x_CALLDATALOAD_0 1 w)))
 (let ((?x9269 (storage_t x_0 x_1 w_1 x_CALLDATALOAD_0 2 w)))
 (= ?x9269 ?x1092))))
 ))
 (let (($x4404 (forall ((n (_ BitVec 6)) )(let ((?x3107 (stack_t x_0 x_1 w_1 x_CALLDATALOAD_0 1 n)))
 (let ((?x7808 (stack_t x_0 x_1 w_1 x_CALLDATALOAD_0 2 n)))
 (or (= ?x7808 ?x3107) (bvsle (bvadd (_ bv62 6) (sc_t 1)) n)))))
 ))
 (let ((?x10413 (used_gas_t x_0 x_1 w_1 x_CALLDATALOAD_0 2)))
 (let (($x3602 (= (stack_t x_0 x_1 w_1 x_CALLDATALOAD_0 2 (bvadd (_ bv63 6) (sc_t 1))) (stack_t x_0 x_1 w_1 x_CALLDATALOAD_0 1 (bvadd (_ bv63 6) (sc_t 1))))))
 (let ((?x11292 (sc_t 1)))
 (let ((?x11293 (bvadd (_ bv62 6) ?x11292)))
 (let ((?x10408 (stack_t x_0 x_1 w_1 x_CALLDATALOAD_0 1 ?x11293)))
 (let (($x6221 (= $x7802 (or $x56 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x63))) (_ bv0 1)))))))
 (let (($x2565 (forall ((w (_ BitVec 256)) )(let ((?x8450 (storage_t x_0 x_1 w_1 x_CALLDATALOAD_0 0 w)))
 (let ((?x1092 (storage_t x_0 x_1 w_1 x_CALLDATALOAD_0 1 w)))
 (= ?x1092 ?x8450))))
 ))
 (let (($x2694 (forall ((n (_ BitVec 6)) )(let ((?x63 (sc_t 0)))
 (let (($x9173 (bvsle ?x63 n)))
 (let ((?x11166 (stack_t x_0 x_1 w_1 x_CALLDATALOAD_0 0 n)))
 (let ((?x3107 (stack_t x_0 x_1 w_1 x_CALLDATALOAD_0 1 n)))
 (or (= ?x3107 ?x11166) $x9173))))))
 ))
 (let (($x6136 (= $x772 (or (exc_halt_s 5) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 5))))))))
 (let (($x9649 (forall ((w (_ BitVec 256)) )(let ((?x7876 (storage_s x_0 x_1 w_1 x_CALLDATALOAD_0 5 w)))
 (let ((?x689 (storage_s x_0 x_1 w_1 x_CALLDATALOAD_0 6 w)))
 (= ?x689 ?x7876))))
 ))
 (let (($x9381 (forall ((n (_ BitVec 6)) )(let ((?x4613 (stack_s x_0 x_1 w_1 x_CALLDATALOAD_0 5 n)))
 (let ((?x4231 (stack_s x_0 x_1 w_1 x_CALLDATALOAD_0 6 n)))
 (or (bvsle (bvadd (_ bv62 6) (sc_s 5)) n) (= ?x4231 ?x4613)))))
 ))
 (let (($x10594 (= (used_gas_s x_0 x_1 w_1 x_CALLDATALOAD_0 6) (+ 3 (used_gas_s x_0 x_1 w_1 x_CALLDATALOAD_0 5)))))
 (let (($x3843 (= (stack_s x_0 x_1 w_1 x_CALLDATALOAD_0 6 (bvadd (_ bv62 6) ?x926)) (stack_s x_0 x_1 w_1 x_CALLDATALOAD_0 5 (bvadd (_ bv63 6) (sc_s 5))))))
 (let (($x2570 (= (stack_s x_0 x_1 w_1 x_CALLDATALOAD_0 6 (bvadd (_ bv63 6) ?x926)) (stack_s x_0 x_1 w_1 x_CALLDATALOAD_0 5 (bvadd (_ bv62 6) (sc_s 5))))))
 (let (($x11568 (exc_halt_s 4)))
 (let (($x5451 (or $x11568 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 4)))) (_ bv0 1))))))
 (let (($x3979 (exc_halt_s 5)))
 (let (($x11469 (forall ((w (_ BitVec 256)) )(let ((?x10761 (storage_s x_0 x_1 w_1 x_CALLDATALOAD_0 4 w)))
 (let ((?x7876 (storage_s x_0 x_1 w_1 x_CALLDATALOAD_0 5 w)))
 (= ?x7876 ?x10761))))
 ))
 (let (($x5448 (forall ((n (_ BitVec 6)) )(let ((?x568 (stack_s x_0 x_1 w_1 x_CALLDATALOAD_0 4 n)))
 (let ((?x4613 (stack_s x_0 x_1 w_1 x_CALLDATALOAD_0 5 n)))
 (or (= ?x4613 ?x568) (bvsle (sc_s 4) n)))))
 ))
 (let ((?x411 (used_gas_s x_0 x_1 w_1 x_CALLDATALOAD_0 5)))
 (let (($x6681 (= $x11568 (or (exc_halt_s 3) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 3))))))))
 (let (($x11770 (forall ((w (_ BitVec 256)) )(let ((?x2877 (storage_s x_0 x_1 w_1 x_CALLDATALOAD_0 3 w)))
 (let ((?x10761 (storage_s x_0 x_1 w_1 x_CALLDATALOAD_0 4 w)))
 (= ?x10761 ?x2877))))
 ))
 (let (($x1862 (forall ((n (_ BitVec 6)) )(let ((?x4141 (stack_s x_0 x_1 w_1 x_CALLDATALOAD_0 3 n)))
 (let ((?x568 (stack_s x_0 x_1 w_1 x_CALLDATALOAD_0 4 n)))
 (let ((?x275 (sc_s 3)))
 (let ((?x9210 (bvadd (_ bv62 6) ?x275)))
 (let (($x5322 (bvsle ?x9210 n)))
 (or $x5322 (= ?x568 ?x4141))))))))
 ))
 (let ((?x275 (sc_s 3)))
 (let ((?x4305 (sc_s 4)))
 (let (($x1519 (= ?x4305 ?x275)))
 (let ((?x8342 (used_gas_s x_0 x_1 w_1 x_CALLDATALOAD_0 4)))
 (let ((?x4952 (bvadd (_ bv63 6) ?x275)))
 (let ((?x10576 (stack_s x_0 x_1 w_1 x_CALLDATALOAD_0 3 ?x4952)))
 (let ((?x9210 (bvadd (_ bv62 6) ?x275)))
 (let ((?x7385 (stack_s x_0 x_1 w_1 x_CALLDATALOAD_0 3 ?x9210)))
 (let (($x292 (exc_halt_s 3)))
 (let (($x2864 (= $x292 (or (exc_halt_s 2) (not (bvsle (_ bv0 6) (bvadd (_ bv61 6) (sc_s 2))))))))
 (let (($x53 (forall ((w (_ BitVec 256)) )(let ((?x920 (storage_s x_0 x_1 w_1 x_CALLDATALOAD_0 2 w)))
 (let ((?x2877 (storage_s x_0 x_1 w_1 x_CALLDATALOAD_0 3 w)))
 (= ?x2877 ?x920))))
 ))
 (let (($x7580 (forall ((n (_ BitVec 6)) )(let ((?x4213 (stack_s x_0 x_1 w_1 x_CALLDATALOAD_0 2 n)))
 (let ((?x4141 (stack_s x_0 x_1 w_1 x_CALLDATALOAD_0 3 n)))
 (or (bvsle (bvadd (_ bv61 6) (sc_s 2)) n) (= ?x4141 ?x4213)))))
 ))
 (let ((?x11700 (used_gas_s x_0 x_1 w_1 x_CALLDATALOAD_0 3)))
 (let (($x10443 (= ?x7385 (stack_s x_0 x_1 w_1 x_CALLDATALOAD_0 2 (bvadd (_ bv62 6) (sc_s 2))))))
 (let ((?x218 (sc_s 2)))
 (let ((?x10597 (bvadd (_ bv63 6) ?x218)))
 (let ((?x9617 (stack_s x_0 x_1 w_1 x_CALLDATALOAD_0 2 ?x10597)))
 (let (($x247 (exc_halt_s 2)))
 (let (($x11639 (= $x247 (or (exc_halt_s 1) (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_s 1))))))))
 (let (($x3607 (forall ((w (_ BitVec 256)) )(let ((?x11456 (storage_s x_0 x_1 w_1 x_CALLDATALOAD_0 1 w)))
 (let ((?x920 (storage_s x_0 x_1 w_1 x_CALLDATALOAD_0 2 w)))
 (= ?x920 ?x11456))))
 ))
 (let (($x5842 (forall ((n (_ BitVec 6)) )(let ((?x4253 (stack_s x_0 x_1 w_1 x_CALLDATALOAD_0 1 n)))
 (let ((?x4213 (stack_s x_0 x_1 w_1 x_CALLDATALOAD_0 2 n)))
 (let ((?x154 (sc_s 1)))
 (let ((?x3369 (bvadd (_ bv63 6) ?x154)))
 (let (($x950 (bvsle ?x3369 n)))
 (or $x950 (= ?x4213 ?x4253))))))))
 ))
 (let ((?x5851 (used_gas_s x_0 x_1 w_1 x_CALLDATALOAD_0 2)))
 (let ((?x154 (sc_s 1)))
 (let ((?x3369 (bvadd (_ bv63 6) ?x154)))
 (let ((?x11327 (stack_s x_0 x_1 w_1 x_CALLDATALOAD_0 1 ?x3369)))
 (let (($x10940 (or $x57 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x72))) (_ bv0 1))) (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) ?x72))))))
 (let (($x189 (exc_halt_s 1)))
 (let (($x4642 (forall ((w (_ BitVec 256)) )(let ((?x3767 (storage_s x_0 x_1 w_1 x_CALLDATALOAD_0 0 w)))
 (let ((?x11456 (storage_s x_0 x_1 w_1 x_CALLDATALOAD_0 1 w)))
 (= ?x11456 ?x3767))))
 ))
 (let (($x6654 (forall ((n (_ BitVec 6)) )(let ((?x2342 (stack_s x_0 x_1 w_1 x_CALLDATALOAD_0 0 n)))
 (let ((?x4253 (stack_s x_0 x_1 w_1 x_CALLDATALOAD_0 1 n)))
 (or (bvsle (bvadd (_ bv63 6) (sc_s 0)) n) (= ?x4253 ?x2342)))))
 ))
 (let ((?x10426 (bvadd (_ bv63 6) ?x72)))
 (let ((?x8728 (stack_s x_0 x_1 w_1 x_CALLDATALOAD_0 0 ?x10426)))
 (let (($x3365 (forall ((w0 (_ BitVec 256)) )(let (($x3659 (= (stack_s x_0 x_1 w_1 x_CALLDATALOAD_0 1 (bvadd (_ bv63 6) (sc_s 1))) w0)))
 (let ((?x11523 (f_CALLDATALOAD x_0 x_1 w_1 x_CALLDATALOAD_0 w0)))
 (= ?x11523 (ite $x3659 x_CALLDATALOAD_0 (_ bv0 256))))))
 ))
 (let (($x4770 (forall ((w (_ BitVec 256)) )(let ((?x3767 (storage_s x_0 x_1 w_1 x_CALLDATALOAD_0 0 w)))
 (= ?x3767 (_ bv0 256))))
 ))
 (let (($x3195 (= ?x11425 0)))
 (let (($x11555 (not $x57)))
 (let (($x4124 (= (stack_s x_0 x_1 w_1 x_CALLDATALOAD_0 0 (_ bv1 6)) x_1)))
 (let (($x3692 (= (stack_s x_0 x_1 w_1 x_CALLDATALOAD_0 0 (_ bv0 6)) x_0)))
 (let (($x10965 (= ?x72 (_ bv2 6))))
 (and $x10965 $x3692 $x4124 $x11555 $x3195 $x4770 $x3365 (= ?x11327 ?x8728) (= (stack_s x_0 x_1 w_1 x_CALLDATALOAD_0 1 ?x10426) ?x8728) (= (used_gas_s x_0 x_1 w_1 x_CALLDATALOAD_0 1) (+ 3 ?x11425)) (= ?x154 (bvadd (_ bv1 6) ?x72)) $x6654 $x4642 (= $x189 $x10940) (= ?x9617 (f_CALLDATALOAD x_0 x_1 w_1 x_CALLDATALOAD_0 ?x11327)) (= ?x5851 (+ 3 (used_gas_s x_0 x_1 w_1 x_CALLDATALOAD_0 1))) (= ?x218 ?x154) $x5842 $x3607 $x11639 (= ?x10576 (stack_s x_0 x_1 w_1 x_CALLDATALOAD_0 2 (bvadd (_ bv61 6) ?x218))) (= (stack_s x_0 x_1 w_1 x_CALLDATALOAD_0 3 (bvadd (_ bv61 6) ?x275)) ?x9617) $x10443 (= ?x11700 (+ 3 ?x5851)) (= ?x275 ?x218) $x7580 $x53 $x2864 (= (stack_s x_0 x_1 w_1 x_CALLDATALOAD_0 4 (bvadd (_ bv63 6) ?x4305)) ?x7385) (= (stack_s x_0 x_1 w_1 x_CALLDATALOAD_0 4 (bvadd (_ bv62 6) ?x4305)) ?x10576) (= ?x8342 (+ 3 ?x11700)) $x1519 $x1862 $x11770 $x6681 (= (stack_s x_0 x_1 w_1 x_CALLDATALOAD_0 5 ?x4305) w_1) (= ?x411 (+ 3 ?x8342)) (= (sc_s 5) (bvadd (_ bv1 6) ?x4305)) $x5448 $x11469 (= $x3979 $x5451) $x2570 $x3843 $x10594 (= ?x926 (sc_s 5)) $x9381 $x9649 $x6136 (= (stack_t x_0 x_1 w_1 x_CALLDATALOAD_0 1 ?x63) w_1) (= (used_gas_t x_0 x_1 w_1 x_CALLDATALOAD_0 1) (+ 3 ?x2697)) (= ?x11292 (bvadd (_ bv1 6) ?x63)) $x2694 $x2565 $x6221 (= ?x169 ?x10408) (= (stack_t x_0 x_1 w_1 x_CALLDATALOAD_0 2 ?x11293) ?x10408) $x3602 (= ?x10413 (+ 3 (used_gas_t x_0 x_1 w_1 x_CALLDATALOAD_0 1))) (= ?x6629 (bvadd (_ bv1 6) ?x11292)) $x4404 $x6926 (= $x2810 $x9833) (= ?x8025 (f_CALLDATALOAD x_0 x_1 w_1 x_CALLDATALOAD_0 ?x169)) (= ?x317 (+ 3 ?x10413)) (= (sc_t 3) ?x6629) $x7029 $x6542 $x3252 $x5529 (= (stack_t x_0 x_1 w_1 x_CALLDATALOAD_0 4 (bvadd (_ bv60 6) ?x3757)) ?x8025) $x10247 $x4921 (= ?x2397 (+ 3 ?x317)) (= ?x3757 (sc_t 3)) $x11609 $x11605 $x1083 (= (stack_t x_0 x_1 w_1 x_CALLDATALOAD_0 5 (bvadd (_ bv63 6) ?x919)) ?x550) (= (stack_t x_0 x_1 w_1 x_CALLDATALOAD_0 5 (bvadd (_ bv61 6) ?x919)) ?x10661) (= (stack_t x_0 x_1 w_1 x_CALLDATALOAD_0 5 (bvadd (_ bv62 6) ?x919)) ?x5270) $x1122 (= ?x919 ?x3757) $x11753 $x10737 $x1125 $x73 $x1128 $x58 $x3916 $x4989 (not (and $x929 $x6177 $x889 $x10934)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
 )
(check-sat)
(get-proof)
