; DUP1 PUSH cw_3 DUP5 SWAP2 POP => DUP3 PUSH cw_3
(set-option :produce-proofs true)
; 
(set-info :status unknown)
(declare-fun storage_t ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 256)) (_ BitVec 256))
(declare-fun w_3 () (_ BitVec 256))
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
 (exists ((x_0 (_ BitVec 256)) (x_1 (_ BitVec 256)) (x_2 (_ BitVec 256)) )(let (($x8436 (forall ((w (_ BitVec 256)) )(let ((?x6859 (storage_t x_0 x_1 x_2 w_3 2 w)))
 (let ((?x2962 (storage_s x_0 x_1 x_2 w_3 5 w)))
 (= ?x2962 ?x6859))))
 ))
 (let (($x7890 (exc_halt_t 2)))
 (let (($x11317 (exc_halt_s 5)))
 (let (($x4114 (= $x11317 $x7890)))
 (let (($x6414 (forall ((n (_ BitVec 6)) )(let ((?x2714 (sc_t 2)))
 (let (($x4079 (bvsle ?x2714 n)))
 (let ((?x5677 (stack_t x_0 x_1 x_2 w_3 2 n)))
 (let ((?x1114 (stack_s x_0 x_1 x_2 w_3 5 n)))
 (let (($x1789 (= ?x1114 ?x5677)))
 (or $x1789 $x4079)))))))
 ))
 (let ((?x2714 (sc_t 2)))
 (let ((?x4319 (sc_s 5)))
 (let (($x1914 (= ?x4319 ?x2714)))
 (let ((?x462 (used_gas_t x_0 x_1 x_2 w_3 0)))
 (let ((?x3795 (used_gas_s x_0 x_1 x_2 w_3 0)))
 (let (($x683 (= ?x3795 ?x462)))
 (let (($x10133 (forall ((w (_ BitVec 256)) )(let ((?x5228 (storage_t x_0 x_1 x_2 w_3 0 w)))
 (let ((?x7942 (storage_s x_0 x_1 x_2 w_3 0 w)))
 (= ?x7942 ?x5228))))
 ))
 (let (($x56 (exc_halt_t 0)))
 (let (($x57 (exc_halt_s 0)))
 (let (($x58 (= $x57 $x56)))
 (let (($x11183 (forall ((n (_ BitVec 6)) )(let ((?x63 (sc_t 0)))
 (let (($x8381 (bvsle ?x63 n)))
 (let ((?x2320 (stack_t x_0 x_1 x_2 w_3 0 n)))
 (let ((?x10598 (stack_s x_0 x_1 x_2 w_3 0 n)))
 (let (($x2103 (= ?x10598 ?x2320)))
 (or $x2103 $x8381)))))))
 ))
 (let ((?x63 (sc_t 0)))
 (let ((?x72 (sc_s 0)))
 (let (($x73 (= ?x72 ?x63)))
 (let (($x1144 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_t 1)))) (_ bv0 1)))))
 (let (($x8377 (exc_halt_t 1)))
 (let (($x6949 (= $x7890 (or $x8377 $x1144))))
 (let (($x5259 (forall ((w (_ BitVec 256)) )(let ((?x11630 (storage_t x_0 x_1 x_2 w_3 1 w)))
 (let ((?x6859 (storage_t x_0 x_1 x_2 w_3 2 w)))
 (= ?x6859 ?x11630))))
 ))
 (let (($x5506 (forall ((n (_ BitVec 6)) )(let ((?x11817 (stack_t x_0 x_1 x_2 w_3 1 n)))
 (let ((?x5677 (stack_t x_0 x_1 x_2 w_3 2 n)))
 (let ((?x7154 (sc_t 1)))
 (let (($x3297 (bvsle ?x7154 n)))
 (or $x3297 (= ?x5677 ?x11817)))))))
 ))
 (let (($x6935 (= ?x2714 (bvadd (_ bv1 6) (sc_t 1)))))
 (let (($x3612 (= (used_gas_t x_0 x_1 x_2 w_3 2) (+ 3 (used_gas_t x_0 x_1 x_2 w_3 1)))))
 (let (($x3416 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x63))) (_ bv0 1)))))
 (let (($x10173 (forall ((w (_ BitVec 256)) )(let ((?x5228 (storage_t x_0 x_1 x_2 w_3 0 w)))
 (let ((?x11630 (storage_t x_0 x_1 x_2 w_3 1 w)))
 (= ?x11630 ?x5228))))
 ))
 (let (($x10825 (forall ((n (_ BitVec 6)) )(let ((?x2320 (stack_t x_0 x_1 x_2 w_3 0 n)))
 (let ((?x11817 (stack_t x_0 x_1 x_2 w_3 1 n)))
 (or (bvsle (bvadd (_ bv61 6) (sc_t 0)) n) (= ?x11817 ?x2320)))))
 ))
 (let ((?x7154 (sc_t 1)))
 (let (($x262 (= ?x7154 (bvadd (_ bv1 6) ?x63))))
 (let (($x1554 (= (stack_t x_0 x_1 x_2 w_3 1 (bvadd (_ bv63 6) ?x63)) (stack_t x_0 x_1 x_2 w_3 0 (bvadd (_ bv63 6) ?x63)))))
 (let (($x7408 (= (stack_t x_0 x_1 x_2 w_3 1 (bvadd (_ bv62 6) ?x63)) (stack_t x_0 x_1 x_2 w_3 0 (bvadd (_ bv62 6) ?x63)))))
 (let ((?x10750 (bvadd (_ bv61 6) ?x63)))
 (let ((?x6248 (stack_t x_0 x_1 x_2 w_3 0 ?x10750)))
 (let (($x9476 (= $x11317 (or (exc_halt_s 4) (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_s 4))))))))
 (let (($x9229 (forall ((w (_ BitVec 256)) )(let ((?x1466 (storage_s x_0 x_1 x_2 w_3 4 w)))
 (let ((?x2962 (storage_s x_0 x_1 x_2 w_3 5 w)))
 (= ?x2962 ?x1466))))
 ))
 (let (($x6874 (forall ((n (_ BitVec 6)) )(let ((?x4305 (sc_s 4)))
 (let ((?x5445 (bvadd (_ bv63 6) ?x4305)))
 (let (($x10725 (bvsle ?x5445 n)))
 (let ((?x174 (stack_s x_0 x_1 x_2 w_3 4 n)))
 (let ((?x1114 (stack_s x_0 x_1 x_2 w_3 5 n)))
 (or (= ?x1114 ?x174) $x10725)))))))
 ))
 (let ((?x4305 (sc_s 4)))
 (let ((?x5445 (bvadd (_ bv63 6) ?x4305)))
 (let (($x4629 (= ?x4319 ?x5445)))
 (let (($x577 (= (used_gas_s x_0 x_1 x_2 w_3 5) (+ 2 (used_gas_s x_0 x_1 x_2 w_3 4)))))
 (let (($x7172 (exc_halt_s 4)))
 (let (($x8206 (= $x7172 (or (exc_halt_s 3) (not (bvsle (_ bv0 6) (bvadd (_ bv61 6) (sc_s 3))))))))
 (let (($x5154 (forall ((w (_ BitVec 256)) )(let ((?x972 (storage_s x_0 x_1 x_2 w_3 3 w)))
 (let ((?x1466 (storage_s x_0 x_1 x_2 w_3 4 w)))
 (= ?x1466 ?x972))))
 ))
 (let (($x3064 (forall ((n (_ BitVec 6)) )(let ((?x2932 (stack_s x_0 x_1 x_2 w_3 3 n)))
 (let ((?x174 (stack_s x_0 x_1 x_2 w_3 4 n)))
 (let ((?x275 (sc_s 3)))
 (let ((?x10482 (bvadd (_ bv61 6) ?x275)))
 (let (($x2169 (bvsle ?x10482 n)))
 (or $x2169 (= ?x174 ?x2932))))))))
 ))
 (let ((?x275 (sc_s 3)))
 (let (($x8517 (= ?x4305 ?x275)))
 (let ((?x11114 (used_gas_s x_0 x_1 x_2 w_3 4)))
 (let (($x10730 (= (stack_s x_0 x_1 x_2 w_3 4 (bvadd (_ bv62 6) ?x4305)) (stack_s x_0 x_1 x_2 w_3 3 (bvadd (_ bv62 6) ?x275)))))
 (let ((?x10091 (bvadd (_ bv63 6) ?x275)))
 (let ((?x11134 (stack_s x_0 x_1 x_2 w_3 3 ?x10091)))
 (let (($x1980 (= (stack_s x_0 x_1 x_2 w_3 4 ?x5445) (stack_s x_0 x_1 x_2 w_3 3 (bvadd (_ bv61 6) ?x275)))))
 (let (($x5208 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 2)))) (_ bv0 1)))))
 (let (($x247 (exc_halt_s 2)))
 (let (($x292 (exc_halt_s 3)))
 (let (($x4227 (forall ((w (_ BitVec 256)) )(let ((?x11653 (storage_s x_0 x_1 x_2 w_3 2 w)))
 (let ((?x972 (storage_s x_0 x_1 x_2 w_3 3 w)))
 (= ?x972 ?x11653))))
 ))
 (let (($x6230 (forall ((n (_ BitVec 6)) )(let ((?x10094 (stack_s x_0 x_1 x_2 w_3 2 n)))
 (let ((?x2932 (stack_s x_0 x_1 x_2 w_3 3 n)))
 (or (bvsle (bvadd (_ bv59 6) (sc_s 2)) n) (= ?x2932 ?x10094)))))
 ))
 (let (($x5654 (= ?x275 (bvadd (_ bv1 6) (sc_s 2)))))
 (let ((?x645 (used_gas_s x_0 x_1 x_2 w_3 3)))
 (let (($x1335 (= (stack_s x_0 x_1 x_2 w_3 3 (bvadd (_ bv63 6) (sc_s 2))) (stack_s x_0 x_1 x_2 w_3 2 (bvadd (_ bv63 6) (sc_s 2))))))
 (let (($x2300 (= (stack_s x_0 x_1 x_2 w_3 3 (bvadd (_ bv62 6) (sc_s 2))) (stack_s x_0 x_1 x_2 w_3 2 (bvadd (_ bv62 6) (sc_s 2))))))
 (let (($x6770 (= (stack_s x_0 x_1 x_2 w_3 3 (bvadd (_ bv61 6) (sc_s 2))) (stack_s x_0 x_1 x_2 w_3 2 (bvadd (_ bv61 6) (sc_s 2))))))
 (let (($x9735 (= (stack_s x_0 x_1 x_2 w_3 3 (bvadd (_ bv60 6) (sc_s 2))) (stack_s x_0 x_1 x_2 w_3 2 (bvadd (_ bv60 6) (sc_s 2))))))
 (let ((?x218 (sc_s 2)))
 (let ((?x7631 (bvadd (_ bv59 6) ?x218)))
 (let ((?x1269 (stack_s x_0 x_1 x_2 w_3 2 ?x7631)))
 (let (($x2786 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 1)))) (_ bv0 1)))))
 (let (($x189 (exc_halt_s 1)))
 (let (($x9126 (= $x247 (or $x189 $x2786))))
 (let (($x8740 (forall ((w (_ BitVec 256)) )(let ((?x5604 (storage_s x_0 x_1 x_2 w_3 1 w)))
 (let ((?x11653 (storage_s x_0 x_1 x_2 w_3 2 w)))
 (= ?x11653 ?x5604))))
 ))
 (let (($x8613 (forall ((n (_ BitVec 6)) )(let ((?x154 (sc_s 1)))
 (let (($x7280 (bvsle ?x154 n)))
 (let ((?x3796 (stack_s x_0 x_1 x_2 w_3 1 n)))
 (let ((?x10094 (stack_s x_0 x_1 x_2 w_3 2 n)))
 (or (= ?x10094 ?x3796) $x7280))))))
 ))
 (let (($x7269 (= ?x218 (bvadd (_ bv1 6) (sc_s 1)))))
 (let ((?x864 (used_gas_s x_0 x_1 x_2 w_3 2)))
 (let (($x7719 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x72))) (_ bv0 1)))))
 (let (($x11169 (= $x189 (or $x57 (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) ?x72))) $x7719))))
 (let (($x4406 (forall ((w (_ BitVec 256)) )(let ((?x7942 (storage_s x_0 x_1 x_2 w_3 0 w)))
 (let ((?x5604 (storage_s x_0 x_1 x_2 w_3 1 w)))
 (= ?x5604 ?x7942))))
 ))
 (let (($x9851 (forall ((n (_ BitVec 6)) )(let ((?x10598 (stack_s x_0 x_1 x_2 w_3 0 n)))
 (let ((?x3796 (stack_s x_0 x_1 x_2 w_3 1 n)))
 (let ((?x72 (sc_s 0)))
 (let ((?x2326 (bvadd (_ bv63 6) ?x72)))
 (let (($x3783 (bvsle ?x2326 n)))
 (or $x3783 (= ?x3796 ?x10598))))))))
 ))
 (let ((?x154 (sc_s 1)))
 (let (($x8600 (= ?x154 (bvadd (_ bv1 6) ?x72))))
 (let ((?x2326 (bvadd (_ bv63 6) ?x72)))
 (let ((?x11383 (stack_s x_0 x_1 x_2 w_3 0 ?x2326)))
 (let (($x11132 (forall ((w (_ BitVec 256)) )(let ((?x7942 (storage_s x_0 x_1 x_2 w_3 0 w)))
 (= ?x7942 (_ bv0 256))))
 ))
 (let (($x7685 (= ?x3795 0)))
 (let (($x4855 (not $x57)))
 (let (($x10093 (= (stack_s x_0 x_1 x_2 w_3 0 (_ bv2 6)) x_2)))
 (let (($x1818 (= (stack_s x_0 x_1 x_2 w_3 0 (_ bv1 6)) x_1)))
 (let (($x5695 (= (stack_s x_0 x_1 x_2 w_3 0 (_ bv0 6)) x_0)))
 (let (($x8897 (= ?x72 (_ bv3 6))))
 (and $x8897 $x5695 $x1818 $x10093 $x4855 $x7685 $x11132 (= (stack_s x_0 x_1 x_2 w_3 1 (bvadd (_ bv63 6) ?x154)) ?x11383) (= (stack_s x_0 x_1 x_2 w_3 1 ?x2326) ?x11383) (= (used_gas_s x_0 x_1 x_2 w_3 1) (+ 3 ?x3795)) $x8600 $x9851 $x4406 $x11169 (= (stack_s x_0 x_1 x_2 w_3 2 ?x154) w_3) (= ?x864 (+ 3 (used_gas_s x_0 x_1 x_2 w_3 1))) $x7269 $x8613 $x8740 $x9126 (= ?x11134 ?x1269) (= (stack_s x_0 x_1 x_2 w_3 3 ?x7631) ?x1269) $x9735 $x6770 $x2300 $x1335 (= ?x645 (+ 3 ?x864)) $x5654 $x6230 $x4227 (= $x292 (or $x247 (not (bvsle (_ bv0 6) ?x7631)) $x5208)) $x1980 (= (stack_s x_0 x_1 x_2 w_3 4 (bvadd (_ bv61 6) ?x4305)) ?x11134) $x10730 (= ?x11114 (+ 3 ?x645)) $x8517 $x3064 $x5154 $x8206 $x577 $x4629 $x6874 $x9229 $x9476 (= (stack_t x_0 x_1 x_2 w_3 1 (bvadd (_ bv63 6) ?x7154)) ?x6248) (= (stack_t x_0 x_1 x_2 w_3 1 ?x10750) ?x6248) $x7408 $x1554 (= (used_gas_t x_0 x_1 x_2 w_3 1) (+ 3 ?x462)) $x262 $x10825 $x10173 (= $x8377 (or $x56 $x3416 (not (bvsle (_ bv0 6) ?x10750)))) (= (stack_t x_0 x_1 x_2 w_3 2 ?x7154) w_3) $x3612 $x6935 $x5506 $x5259 $x6949 $x73 $x11183 $x58 $x10133 $x683 (not (and $x1914 $x6414 $x4114 $x8436)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
 )
(check-sat)
(get-proof)