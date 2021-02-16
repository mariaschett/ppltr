; PUSH cw_1 SLOAD SWAP1 SWAP2 SWAP1 => SWAP1 PUSH cw_1 SLOAD
(set-option :produce-proofs true)
; 
(set-info :status unknown)
(declare-fun storage_t ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 256)) (_ BitVec 256))
(declare-fun w_1 () (_ BitVec 256))
(declare-fun storage_s ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 256)) (_ BitVec 256))
(declare-fun exc_halt_t (Int) Bool)
(declare-fun exc_halt_s (Int) Bool)
(declare-fun sc_t (Int) (_ BitVec 6))
(declare-fun stack_t ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 6)) (_ BitVec 256))
(declare-fun stack_s ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 6)) (_ BitVec 256))
(declare-fun sc_s (Int) (_ BitVec 6))
(declare-fun used_gas_t ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int) Int)
(declare-fun used_gas_s ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int) Int)
(assert
 (exists ((x_0 (_ BitVec 256)) (x_1 (_ BitVec 256)) (x_SLOAD_0 (_ BitVec 256)) )(let (($x9861 (forall ((w (_ BitVec 256)) )(let ((?x1065 (storage_t x_0 x_1 x_SLOAD_0 w_1 3 w)))
 (let ((?x10178 (storage_s x_0 x_1 x_SLOAD_0 w_1 5 w)))
 (= ?x10178 ?x1065))))
 ))
 (let (($x4112 (exc_halt_t 3)))
 (let (($x1862 (exc_halt_s 5)))
 (let (($x34 (= $x1862 $x4112)))
 (let (($x3064 (forall ((n (_ BitVec 6)) )(let ((?x11964 (sc_t 3)))
 (let (($x6076 (bvsle ?x11964 n)))
 (let ((?x9358 (stack_t x_0 x_1 x_SLOAD_0 w_1 3 n)))
 (let ((?x9832 (stack_s x_0 x_1 x_SLOAD_0 w_1 5 n)))
 (let (($x4317 (= ?x9832 ?x9358)))
 (or $x4317 $x6076)))))))
 ))
 (let ((?x11964 (sc_t 3)))
 (let ((?x4319 (sc_s 5)))
 (let (($x3659 (= ?x4319 ?x11964)))
 (let ((?x8973 (used_gas_t x_0 x_1 x_SLOAD_0 w_1 0)))
 (let ((?x10353 (used_gas_s x_0 x_1 x_SLOAD_0 w_1 0)))
 (let (($x1850 (= ?x10353 ?x8973)))
 (let (($x1210 (forall ((w (_ BitVec 256)) )(let ((?x10810 (storage_t x_0 x_1 x_SLOAD_0 w_1 0 w)))
 (let ((?x7072 (storage_s x_0 x_1 x_SLOAD_0 w_1 0 w)))
 (= ?x7072 ?x10810))))
 ))
 (let (($x56 (exc_halt_t 0)))
 (let (($x57 (exc_halt_s 0)))
 (let (($x58 (= $x57 $x56)))
 (let (($x9975 (forall ((n (_ BitVec 6)) )(let ((?x5437 (stack_t x_0 x_1 x_SLOAD_0 w_1 0 n)))
 (let ((?x2079 (stack_s x_0 x_1 x_SLOAD_0 w_1 0 n)))
 (let (($x1047 (= ?x2079 ?x5437)))
 (let ((?x63 (sc_t 0)))
 (let (($x632 (bvsle ?x63 n)))
 (or $x632 $x1047)))))))
 ))
 (let ((?x63 (sc_t 0)))
 (let ((?x72 (sc_s 0)))
 (let (($x73 (= ?x72 ?x63)))
 (let (($x3954 (= $x4112 (or (exc_halt_t 2) (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_t 2))))))))
 (let (($x2856 (forall ((w (_ BitVec 256)) )(let ((?x11267 (storage_t x_0 x_1 x_SLOAD_0 w_1 2 w)))
 (let ((?x1065 (storage_t x_0 x_1 x_SLOAD_0 w_1 3 w)))
 (= ?x1065 ?x11267))))
 ))
 (let (($x3307 (forall ((n (_ BitVec 6)) )(let ((?x6602 (sc_t 2)))
 (let ((?x8103 (bvadd (_ bv63 6) ?x6602)))
 (let (($x3046 (bvsle ?x8103 n)))
 (let ((?x7407 (stack_t x_0 x_1 x_SLOAD_0 w_1 2 n)))
 (let ((?x9358 (stack_t x_0 x_1 x_SLOAD_0 w_1 3 n)))
 (or (= ?x9358 ?x7407) $x3046)))))))
 ))
 (let ((?x6602 (sc_t 2)))
 (let (($x11444 (= ?x11964 ?x6602)))
 (let (($x5186 (= (used_gas_t x_0 x_1 x_SLOAD_0 w_1 3) (+ 200 (used_gas_t x_0 x_1 x_SLOAD_0 w_1 2)))))
 (let ((?x3658 (storage_t x_0 x_1 x_SLOAD_0 w_1 2 (stack_t x_0 x_1 x_SLOAD_0 w_1 2 (bvadd (_ bv63 6) ?x6602)))))
 (let (($x6468 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_t 1)))) (_ bv0 1)))))
 (let (($x5011 (exc_halt_t 1)))
 (let (($x9181 (exc_halt_t 2)))
 (let (($x8058 (forall ((w (_ BitVec 256)) )(let ((?x5544 (storage_t x_0 x_1 x_SLOAD_0 w_1 1 w)))
 (let ((?x11267 (storage_t x_0 x_1 x_SLOAD_0 w_1 2 w)))
 (= ?x11267 ?x5544))))
 ))
 (let (($x187 (forall ((n (_ BitVec 6)) )(let ((?x7603 (stack_t x_0 x_1 x_SLOAD_0 w_1 1 n)))
 (let ((?x7407 (stack_t x_0 x_1 x_SLOAD_0 w_1 2 n)))
 (or (bvsle (sc_t 1) n) (= ?x7407 ?x7603)))))
 ))
 (let (($x3613 (= ?x6602 (bvadd (_ bv1 6) (sc_t 1)))))
 (let ((?x6309 (used_gas_t x_0 x_1 x_SLOAD_0 w_1 2)))
 (let (($x9848 (= $x5011 (or $x56 (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) ?x63)))))))
 (let (($x169 (forall ((w (_ BitVec 256)) )(let ((?x10810 (storage_t x_0 x_1 x_SLOAD_0 w_1 0 w)))
 (let ((?x5544 (storage_t x_0 x_1 x_SLOAD_0 w_1 1 w)))
 (= ?x5544 ?x10810))))
 ))
 (let (($x7625 (forall ((n (_ BitVec 6)) )(let ((?x5437 (stack_t x_0 x_1 x_SLOAD_0 w_1 0 n)))
 (let ((?x7603 (stack_t x_0 x_1 x_SLOAD_0 w_1 1 n)))
 (let ((?x63 (sc_t 0)))
 (let ((?x11487 (bvadd (_ bv62 6) ?x63)))
 (let (($x1401 (bvsle ?x11487 n)))
 (or $x1401 (= ?x7603 ?x5437))))))))
 ))
 (let (($x10998 (= (stack_t x_0 x_1 x_SLOAD_0 w_1 1 (bvadd (_ bv62 6) (sc_t 1))) (stack_t x_0 x_1 x_SLOAD_0 w_1 0 (bvadd (_ bv63 6) ?x63)))))
 (let (($x5827 (= (stack_t x_0 x_1 x_SLOAD_0 w_1 1 (bvadd (_ bv63 6) (sc_t 1))) (stack_t x_0 x_1 x_SLOAD_0 w_1 0 (bvadd (_ bv62 6) ?x63)))))
 (let (($x5223 (= $x1862 (or (exc_halt_s 4) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 4))))))))
 (let (($x10653 (forall ((w (_ BitVec 256)) )(let ((?x3038 (storage_s x_0 x_1 x_SLOAD_0 w_1 4 w)))
 (let ((?x10178 (storage_s x_0 x_1 x_SLOAD_0 w_1 5 w)))
 (= ?x10178 ?x3038))))
 ))
 (let (($x488 (forall ((n (_ BitVec 6)) )(let ((?x86 (stack_s x_0 x_1 x_SLOAD_0 w_1 4 n)))
 (let ((?x9832 (stack_s x_0 x_1 x_SLOAD_0 w_1 5 n)))
 (let ((?x500 (sc_s 4)))
 (let ((?x7626 (bvadd (_ bv62 6) ?x500)))
 (let (($x2358 (bvsle ?x7626 n)))
 (or $x2358 (= ?x9832 ?x86))))))))
 ))
 (let ((?x500 (sc_s 4)))
 (let (($x8952 (= ?x4319 ?x500)))
 (let (($x8843 (= (used_gas_s x_0 x_1 x_SLOAD_0 w_1 5) (+ 3 (used_gas_s x_0 x_1 x_SLOAD_0 w_1 4)))))
 (let ((?x4132 (bvadd (_ bv63 6) ?x500)))
 (let ((?x8468 (stack_s x_0 x_1 x_SLOAD_0 w_1 4 ?x4132)))
 (let ((?x7626 (bvadd (_ bv62 6) ?x500)))
 (let ((?x1511 (stack_s x_0 x_1 x_SLOAD_0 w_1 4 ?x7626)))
 (let (($x3743 (exc_halt_s 4)))
 (let (($x9223 (= $x3743 (or (exc_halt_s 3) (not (bvsle (_ bv0 6) (bvadd (_ bv61 6) (sc_s 3))))))))
 (let (($x1357 (forall ((w (_ BitVec 256)) )(let ((?x10139 (storage_s x_0 x_1 x_SLOAD_0 w_1 3 w)))
 (let ((?x3038 (storage_s x_0 x_1 x_SLOAD_0 w_1 4 w)))
 (= ?x3038 ?x10139))))
 ))
 (let (($x7225 (forall ((n (_ BitVec 6)) )(let ((?x5948 (stack_s x_0 x_1 x_SLOAD_0 w_1 3 n)))
 (let ((?x86 (stack_s x_0 x_1 x_SLOAD_0 w_1 4 n)))
 (let ((?x1974 (sc_s 3)))
 (let ((?x9214 (bvadd (_ bv61 6) ?x1974)))
 (let (($x7052 (bvsle ?x9214 n)))
 (or $x7052 (= ?x86 ?x5948))))))))
 ))
 (let ((?x1974 (sc_s 3)))
 (let (($x7752 (= ?x500 ?x1974)))
 (let ((?x3365 (used_gas_s x_0 x_1 x_SLOAD_0 w_1 4)))
 (let ((?x2640 (bvadd (_ bv63 6) ?x1974)))
 (let ((?x10832 (stack_s x_0 x_1 x_SLOAD_0 w_1 3 ?x2640)))
 (let (($x8780 (exc_halt_s 3)))
 (let (($x9458 (= $x8780 (or (exc_halt_s 2) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 2))))))))
 (let (($x10887 (forall ((w (_ BitVec 256)) )(let ((?x308 (storage_s x_0 x_1 x_SLOAD_0 w_1 2 w)))
 (let ((?x10139 (storage_s x_0 x_1 x_SLOAD_0 w_1 3 w)))
 (= ?x10139 ?x308))))
 ))
 (let (($x7579 (forall ((n (_ BitVec 6)) )(let ((?x2973 (stack_s x_0 x_1 x_SLOAD_0 w_1 2 n)))
 (let ((?x5948 (stack_s x_0 x_1 x_SLOAD_0 w_1 3 n)))
 (or (= ?x5948 ?x2973) (bvsle (bvadd (_ bv62 6) (sc_s 2)) n)))))
 ))
 (let ((?x2620 (sc_s 2)))
 (let (($x7925 (= ?x1974 ?x2620)))
 (let ((?x6490 (used_gas_s x_0 x_1 x_SLOAD_0 w_1 3)))
 (let ((?x5957 (bvadd (_ bv63 6) ?x2620)))
 (let ((?x958 (stack_s x_0 x_1 x_SLOAD_0 w_1 2 ?x5957)))
 (let ((?x7556 (bvadd (_ bv62 6) ?x1974)))
 (let ((?x5102 (stack_s x_0 x_1 x_SLOAD_0 w_1 3 ?x7556)))
 (let (($x3200 (exc_halt_s 2)))
 (let (($x6882 (= $x3200 (or (exc_halt_s 1) (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_s 1))))))))
 (let (($x11459 (forall ((w (_ BitVec 256)) )(let ((?x8926 (storage_s x_0 x_1 x_SLOAD_0 w_1 1 w)))
 (let ((?x308 (storage_s x_0 x_1 x_SLOAD_0 w_1 2 w)))
 (= ?x308 ?x8926))))
 ))
 (let (($x9273 (forall ((n (_ BitVec 6)) )(let ((?x5660 (stack_s x_0 x_1 x_SLOAD_0 w_1 1 n)))
 (let ((?x2973 (stack_s x_0 x_1 x_SLOAD_0 w_1 2 n)))
 (let ((?x154 (sc_s 1)))
 (let ((?x8586 (bvadd (_ bv63 6) ?x154)))
 (let (($x4241 (bvsle ?x8586 n)))
 (or $x4241 (= ?x2973 ?x5660))))))))
 ))
 (let ((?x154 (sc_s 1)))
 (let (($x6125 (= ?x2620 ?x154)))
 (let ((?x9976 (used_gas_s x_0 x_1 x_SLOAD_0 w_1 2)))
 (let ((?x4017 (storage_s x_0 x_1 x_SLOAD_0 w_1 1 (stack_s x_0 x_1 x_SLOAD_0 w_1 1 (bvadd (_ bv63 6) ?x154)))))
 (let (($x10558 (exc_halt_s 1)))
 (let (($x5896 (= $x10558 (or $x57 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x72))) (_ bv0 1)))))))
 (let (($x2268 (forall ((w (_ BitVec 256)) )(let ((?x7072 (storage_s x_0 x_1 x_SLOAD_0 w_1 0 w)))
 (let ((?x8926 (storage_s x_0 x_1 x_SLOAD_0 w_1 1 w)))
 (= ?x8926 ?x7072))))
 ))
 (let (($x10895 (forall ((n (_ BitVec 6)) )(let ((?x72 (sc_s 0)))
 (let (($x10377 (bvsle ?x72 n)))
 (let ((?x2079 (stack_s x_0 x_1 x_SLOAD_0 w_1 0 n)))
 (let ((?x5660 (stack_s x_0 x_1 x_SLOAD_0 w_1 1 n)))
 (or (= ?x5660 ?x2079) $x10377))))))
 ))
 (let (($x7558 (= ?x154 (bvadd (_ bv1 6) ?x72))))
 (let (($x10023 (forall ((w (_ BitVec 256)) )(let (($x9027 (= w (stack_s x_0 x_1 x_SLOAD_0 w_1 1 (bvadd (_ bv63 6) (sc_s 1))))))
 (let ((?x7072 (storage_s x_0 x_1 x_SLOAD_0 w_1 0 w)))
 (= ?x7072 (ite $x9027 x_SLOAD_0 (_ bv0 256))))))
 ))
 (let (($x1842 (= ?x10353 0)))
 (let (($x10917 (not $x57)))
 (let (($x11608 (= (stack_s x_0 x_1 x_SLOAD_0 w_1 0 (_ bv1 6)) x_1)))
 (let (($x11540 (= (stack_s x_0 x_1 x_SLOAD_0 w_1 0 (_ bv0 6)) x_0)))
 (let (($x11128 (= ?x72 (_ bv2 6))))
 (and $x11128 $x11540 $x11608 $x10917 $x1842 $x10023 (= (stack_s x_0 x_1 x_SLOAD_0 w_1 1 ?x72) w_1) (= (used_gas_s x_0 x_1 x_SLOAD_0 w_1 1) (+ 3 ?x10353)) $x7558 $x10895 $x2268 $x5896 (= ?x958 ?x4017) (= ?x9976 (+ 200 (used_gas_s x_0 x_1 x_SLOAD_0 w_1 1))) $x6125 $x9273 $x11459 $x6882 (= ?x10832 (stack_s x_0 x_1 x_SLOAD_0 w_1 2 (bvadd (_ bv62 6) ?x2620))) (= ?x5102 ?x958) (= ?x6490 (+ 3 ?x9976)) $x7925 $x7579 $x10887 $x9458 (= ?x8468 (stack_s x_0 x_1 x_SLOAD_0 w_1 3 (bvadd (_ bv61 6) ?x1974))) (= (stack_s x_0 x_1 x_SLOAD_0 w_1 4 (bvadd (_ bv61 6) ?x500)) ?x10832) (= ?x1511 ?x5102) (= ?x3365 (+ 3 ?x6490)) $x7752 $x7225 $x1357 $x9223 (= (stack_s x_0 x_1 x_SLOAD_0 w_1 5 (bvadd (_ bv63 6) ?x4319)) ?x1511) (= (stack_s x_0 x_1 x_SLOAD_0 w_1 5 (bvadd (_ bv62 6) ?x4319)) ?x8468) $x8843 $x8952 $x488 $x10653 $x5223 $x5827 $x10998 (= (used_gas_t x_0 x_1 x_SLOAD_0 w_1 1) (+ 3 ?x8973)) (= (sc_t 1) ?x63) $x7625 $x169 $x9848 (= (stack_t x_0 x_1 x_SLOAD_0 w_1 2 (sc_t 1)) w_1) (= ?x6309 (+ 3 (used_gas_t x_0 x_1 x_SLOAD_0 w_1 1))) $x3613 $x187 $x8058 (= $x9181 (or $x5011 $x6468)) (= (stack_t x_0 x_1 x_SLOAD_0 w_1 3 (bvadd (_ bv63 6) ?x11964)) ?x3658) $x5186 $x11444 $x3307 $x2856 $x3954 $x73 $x9975 $x58 $x1210 $x1850 (not (and $x3659 $x3064 $x34 $x9861)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
 )
(check-sat)
(get-proof)