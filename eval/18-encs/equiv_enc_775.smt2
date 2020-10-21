; POP DUP2 SWAP1 SSTORE POP => ISZERO POP SSTORE
(set-option :produce-proofs true)
; 
(set-info :status unknown)
(declare-fun storage_t ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 256)) (_ BitVec 256))
(declare-fun storage_s ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 256)) (_ BitVec 256))
(declare-fun exc_halt_t (Int) Bool)
(declare-fun exc_halt_s (Int) Bool)
(declare-fun stack_t ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 6)) (_ BitVec 256))
(declare-fun stack_s ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 6)) (_ BitVec 256))
(declare-fun sc_t (Int) (_ BitVec 6))
(declare-fun sc_s (Int) (_ BitVec 6))
(declare-fun used_gas_t ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int) Int)
(declare-fun used_gas_s ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int) Int)
(assert
 (exists ((x_0 (_ BitVec 256)) (x_1 (_ BitVec 256)) (x_2 (_ BitVec 256)) (x_SSTORE_0 (_ BitVec 256)) )(let (($x11543 (forall ((w (_ BitVec 256)) )(let ((?x10022 (storage_t x_0 x_1 x_2 x_SSTORE_0 3 w)))
 (let ((?x5027 (storage_s x_0 x_1 x_2 x_SSTORE_0 5 w)))
 (= ?x5027 ?x10022))))
 ))
 (let (($x4112 (exc_halt_t 3)))
 (let (($x1862 (exc_halt_s 5)))
 (let (($x34 (= $x1862 $x4112)))
 (let (($x4109 (forall ((n (_ BitVec 6)) )(let ((?x2784 (stack_t x_0 x_1 x_2 x_SSTORE_0 3 n)))
 (let ((?x10018 (stack_s x_0 x_1 x_2 x_SSTORE_0 5 n)))
 (let (($x8128 (= ?x10018 ?x2784)))
 (let ((?x11964 (sc_t 3)))
 (let (($x7010 (bvsle ?x11964 n)))
 (or $x7010 $x8128)))))))
 ))
 (let ((?x11964 (sc_t 3)))
 (let ((?x4319 (sc_s 5)))
 (let (($x3659 (= ?x4319 ?x11964)))
 (let ((?x3775 (used_gas_t x_0 x_1 x_2 x_SSTORE_0 0)))
 (let ((?x2391 (used_gas_s x_0 x_1 x_2 x_SSTORE_0 0)))
 (let (($x1796 (= ?x2391 ?x3775)))
 (let (($x8747 (forall ((w (_ BitVec 256)) )(let ((?x3854 (storage_t x_0 x_1 x_2 x_SSTORE_0 0 w)))
 (let ((?x8582 (storage_s x_0 x_1 x_2 x_SSTORE_0 0 w)))
 (= ?x8582 ?x3854))))
 ))
 (let (($x56 (exc_halt_t 0)))
 (let (($x57 (exc_halt_s 0)))
 (let (($x58 (= $x57 $x56)))
 (let (($x10515 (forall ((n (_ BitVec 6)) )(let ((?x5460 (stack_t x_0 x_1 x_2 x_SSTORE_0 0 n)))
 (let ((?x8046 (stack_s x_0 x_1 x_2 x_SSTORE_0 0 n)))
 (let (($x6065 (= ?x8046 ?x5460)))
 (let ((?x63 (sc_t 0)))
 (let (($x721 (bvsle ?x63 n)))
 (or $x721 $x6065)))))))
 ))
 (let ((?x63 (sc_t 0)))
 (let ((?x72 (sc_s 0)))
 (let (($x73 (= ?x72 ?x63)))
 (let (($x1657 (= $x4112 (or (exc_halt_t 2) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_t 2))))))))
 (let (($x8490 (forall ((n (_ BitVec 6)) )(let ((?x6109 (stack_t x_0 x_1 x_2 x_SSTORE_0 2 n)))
 (let ((?x2784 (stack_t x_0 x_1 x_2 x_SSTORE_0 3 n)))
 (or (bvsle (bvadd (_ bv62 6) (sc_t 2)) n) (= ?x2784 ?x6109)))))
 ))
 (let (($x11209 (= (stack_t x_0 x_1 x_2 x_SSTORE_0 2 (bvadd (_ bv62 6) (sc_t 2))) (_ bv0 256))))
 (let ((?x668 (storage_t x_0 x_1 x_2 x_SSTORE_0 2 (stack_t x_0 x_1 x_2 x_SSTORE_0 2 (bvadd (_ bv63 6) (sc_t 2))))))
 (let ((?x8540 (used_gas_t x_0 x_1 x_2 x_SSTORE_0 2)))
 (let ((?x638 (+ ?x8540 (ite (= ?x668 (_ bv0 256)) (ite $x11209 5000 20000) (ite $x11209 (- 10000) 5000)))))
 (let (($x7529 (forall ((w (_ BitVec 256)) )(let ((?x1260 (storage_t x_0 x_1 x_2 x_SSTORE_0 2 w)))
 (let (($x7310 (= w (stack_t x_0 x_1 x_2 x_SSTORE_0 2 (bvadd (_ bv63 6) (sc_t 2))))))
 (let ((?x9503 (ite $x7310 (stack_t x_0 x_1 x_2 x_SSTORE_0 2 (bvadd (_ bv62 6) (sc_t 2))) ?x1260)))
 (let ((?x10022 (storage_t x_0 x_1 x_2 x_SSTORE_0 3 w)))
 (= ?x10022 ?x9503))))))
 ))
 (let (($x3706 (exc_halt_t 2)))
 (let (($x7247 (= $x3706 (or (exc_halt_t 1) (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_t 1))))))))
 (let (($x11839 (forall ((w (_ BitVec 256)) )(let ((?x2809 (storage_t x_0 x_1 x_2 x_SSTORE_0 1 w)))
 (let ((?x1260 (storage_t x_0 x_1 x_2 x_SSTORE_0 2 w)))
 (= ?x1260 ?x2809))))
 ))
 (let (($x8730 (forall ((n (_ BitVec 6)) )(let ((?x3484 (stack_t x_0 x_1 x_2 x_SSTORE_0 1 n)))
 (let ((?x6109 (stack_t x_0 x_1 x_2 x_SSTORE_0 2 n)))
 (or (bvsle (bvadd (_ bv63 6) (sc_t 1)) n) (= ?x6109 ?x3484)))))
 ))
 (let (($x10029 (forall ((w (_ BitVec 256)) )(let ((?x3854 (storage_t x_0 x_1 x_2 x_SSTORE_0 0 w)))
 (let ((?x2809 (storage_t x_0 x_1 x_2 x_SSTORE_0 1 w)))
 (= ?x2809 ?x3854))))
 ))
 (let (($x10217 (forall ((n (_ BitVec 6)) )(let ((?x63 (sc_t 0)))
 (let ((?x3657 (bvadd (_ bv63 6) ?x63)))
 (let (($x8661 (bvsle ?x3657 n)))
 (let ((?x5460 (stack_t x_0 x_1 x_2 x_SSTORE_0 0 n)))
 (let ((?x3484 (stack_t x_0 x_1 x_2 x_SSTORE_0 1 n)))
 (or (= ?x3484 ?x5460) $x8661)))))))
 ))
 (let ((?x2250 (ite (= (stack_t x_0 x_1 x_2 x_SSTORE_0 0 (bvadd (_ bv63 6) ?x63)) (_ bv0 256)) (_ bv1 256) (_ bv0 256))))
 (let (($x9894 (= (stack_t x_0 x_1 x_2 x_SSTORE_0 1 (bvadd (_ bv63 6) (sc_t 1))) ?x2250)))
 (let (($x9062 (= $x1862 (or (exc_halt_s 4) (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_s 4))))))))
 (let (($x9941 (forall ((w (_ BitVec 256)) )(let ((?x3343 (storage_s x_0 x_1 x_2 x_SSTORE_0 4 w)))
 (let ((?x5027 (storage_s x_0 x_1 x_2 x_SSTORE_0 5 w)))
 (= ?x5027 ?x3343))))
 ))
 (let (($x1281 (forall ((n (_ BitVec 6)) )(let ((?x9433 (sc_s 4)))
 (let ((?x5670 (bvadd (_ bv63 6) ?x9433)))
 (let (($x11419 (bvsle ?x5670 n)))
 (let ((?x3106 (stack_s x_0 x_1 x_2 x_SSTORE_0 4 n)))
 (let ((?x10018 (stack_s x_0 x_1 x_2 x_SSTORE_0 5 n)))
 (or (= ?x10018 ?x3106) $x11419)))))))
 ))
 (let (($x7178 (= (used_gas_s x_0 x_1 x_2 x_SSTORE_0 5) (+ 2 (used_gas_s x_0 x_1 x_2 x_SSTORE_0 4)))))
 (let (($x9175 (exc_halt_s 4)))
 (let (($x7542 (= $x9175 (or (exc_halt_s 3) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 3))))))))
 (let (($x4408 (forall ((n (_ BitVec 6)) )(let ((?x3668 (stack_s x_0 x_1 x_2 x_SSTORE_0 3 n)))
 (let ((?x3106 (stack_s x_0 x_1 x_2 x_SSTORE_0 4 n)))
 (let ((?x3851 (sc_s 3)))
 (let ((?x6476 (bvadd (_ bv62 6) ?x3851)))
 (let (($x2289 (bvsle ?x6476 n)))
 (or $x2289 (= ?x3106 ?x3668))))))))
 ))
 (let ((?x3851 (sc_s 3)))
 (let ((?x6476 (bvadd (_ bv62 6) ?x3851)))
 (let ((?x10571 (stack_s x_0 x_1 x_2 x_SSTORE_0 3 ?x6476)))
 (let (($x6519 (= ?x10571 (_ bv0 256))))
 (let ((?x39 (bvadd (_ bv63 6) ?x3851)))
 (let ((?x1340 (stack_s x_0 x_1 x_2 x_SSTORE_0 3 ?x39)))
 (let ((?x11937 (ite (= (storage_s x_0 x_1 x_2 x_SSTORE_0 3 ?x1340) (_ bv0 256)) (ite $x6519 5000 20000) (ite $x6519 (- 10000) 5000))))
 (let ((?x4285 (used_gas_s x_0 x_1 x_2 x_SSTORE_0 3)))
 (let ((?x8004 (used_gas_s x_0 x_1 x_2 x_SSTORE_0 4)))
 (let (($x2389 (forall ((w (_ BitVec 256)) )(let ((?x8305 (storage_s x_0 x_1 x_2 x_SSTORE_0 3 w)))
 (let (($x11753 (= w (stack_s x_0 x_1 x_2 x_SSTORE_0 3 (bvadd (_ bv63 6) (sc_s 3))))))
 (let ((?x11074 (ite $x11753 (stack_s x_0 x_1 x_2 x_SSTORE_0 3 (bvadd (_ bv62 6) (sc_s 3))) ?x8305)))
 (let ((?x3343 (storage_s x_0 x_1 x_2 x_SSTORE_0 4 w)))
 (= ?x3343 ?x11074))))))
 ))
 (let (($x8103 (exc_halt_s 3)))
 (let (($x2063 (= $x8103 (or (exc_halt_s 2) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 2))))))))
 (let (($x6998 (forall ((w (_ BitVec 256)) )(let ((?x7832 (storage_s x_0 x_1 x_2 x_SSTORE_0 2 w)))
 (let ((?x8305 (storage_s x_0 x_1 x_2 x_SSTORE_0 3 w)))
 (= ?x8305 ?x7832))))
 ))
 (let (($x7930 (forall ((n (_ BitVec 6)) )(let ((?x7670 (stack_s x_0 x_1 x_2 x_SSTORE_0 2 n)))
 (let ((?x3668 (stack_s x_0 x_1 x_2 x_SSTORE_0 3 n)))
 (let ((?x2272 (sc_s 2)))
 (let ((?x9641 (bvadd (_ bv62 6) ?x2272)))
 (let (($x6847 (bvsle ?x9641 n)))
 (or $x6847 (= ?x3668 ?x7670))))))))
 ))
 (let ((?x2272 (sc_s 2)))
 (let (($x11383 (= ?x3851 ?x2272)))
 (let (($x10953 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 1)))) (_ bv0 1)))))
 (let (($x8780 (exc_halt_s 1)))
 (let (($x10052 (exc_halt_s 2)))
 (let (($x9152 (forall ((w (_ BitVec 256)) )(let ((?x9037 (storage_s x_0 x_1 x_2 x_SSTORE_0 1 w)))
 (let ((?x7832 (storage_s x_0 x_1 x_2 x_SSTORE_0 2 w)))
 (= ?x7832 ?x9037))))
 ))
 (let (($x1854 (forall ((n (_ BitVec 6)) )(let ((?x3028 (stack_s x_0 x_1 x_2 x_SSTORE_0 1 n)))
 (let ((?x7670 (stack_s x_0 x_1 x_2 x_SSTORE_0 2 n)))
 (or (= ?x7670 ?x3028) (bvsle (bvadd (_ bv62 6) (sc_s 1)) n)))))
 ))
 (let (($x7440 (= ?x2272 (bvadd (_ bv1 6) (sc_s 1)))))
 (let ((?x2228 (used_gas_s x_0 x_1 x_2 x_SSTORE_0 2)))
 (let (($x7223 (= (stack_s x_0 x_1 x_2 x_SSTORE_0 2 (bvadd (_ bv63 6) (sc_s 1))) (stack_s x_0 x_1 x_2 x_SSTORE_0 1 (bvadd (_ bv63 6) (sc_s 1))))))
 (let ((?x154 (sc_s 1)))
 (let ((?x7667 (bvadd (_ bv62 6) ?x154)))
 (let ((?x2281 (stack_s x_0 x_1 x_2 x_SSTORE_0 1 ?x7667)))
 (let (($x10488 (forall ((w (_ BitVec 256)) )(let ((?x8582 (storage_s x_0 x_1 x_2 x_SSTORE_0 0 w)))
 (let ((?x9037 (storage_s x_0 x_1 x_2 x_SSTORE_0 1 w)))
 (= ?x9037 ?x8582))))
 ))
 (let (($x7382 (forall ((n (_ BitVec 6)) )(let ((?x72 (sc_s 0)))
 (let ((?x2654 (bvadd (_ bv63 6) ?x72)))
 (let (($x7688 (bvsle ?x2654 n)))
 (let ((?x8046 (stack_s x_0 x_1 x_2 x_SSTORE_0 0 n)))
 (let ((?x3028 (stack_s x_0 x_1 x_2 x_SSTORE_0 1 n)))
 (or (= ?x3028 ?x8046) $x7688)))))))
 ))
 (let (($x6520 (forall ((w (_ BitVec 256)) )(let (($x11753 (= w (stack_s x_0 x_1 x_2 x_SSTORE_0 3 (bvadd (_ bv63 6) (sc_s 3))))))
 (let ((?x8582 (storage_s x_0 x_1 x_2 x_SSTORE_0 0 w)))
 (= ?x8582 (ite $x11753 x_SSTORE_0 (_ bv0 256))))))
 ))
 (let (($x1167 (= ?x2391 0)))
 (let (($x3720 (not $x57)))
 (let (($x2694 (= (stack_s x_0 x_1 x_2 x_SSTORE_0 0 (_ bv2 6)) x_2)))
 (let (($x9981 (= (stack_s x_0 x_1 x_2 x_SSTORE_0 0 (_ bv1 6)) x_1)))
 (let (($x332 (= (stack_s x_0 x_1 x_2 x_SSTORE_0 0 (_ bv0 6)) x_0)))
 (let (($x8058 (= ?x72 (_ bv3 6))))
 (and $x8058 $x332 $x9981 $x2694 $x3720 $x1167 $x6520 (= (used_gas_s x_0 x_1 x_2 x_SSTORE_0 1) (+ 2 ?x2391)) (= ?x154 (bvadd (_ bv63 6) ?x72)) $x7382 $x10488 (= $x8780 (or $x57 (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) ?x72))))) (= (stack_s x_0 x_1 x_2 x_SSTORE_0 2 (bvadd (_ bv63 6) ?x2272)) ?x2281) (= (stack_s x_0 x_1 x_2 x_SSTORE_0 2 ?x7667) ?x2281) $x7223 (= ?x2228 (+ 3 (used_gas_s x_0 x_1 x_2 x_SSTORE_0 1))) $x7440 $x1854 $x9152 (= $x10052 (or (not (bvsle (_ bv0 6) ?x7667)) $x8780 $x10953)) (= ?x1340 (stack_s x_0 x_1 x_2 x_SSTORE_0 2 (bvadd (_ bv62 6) ?x2272))) (= ?x10571 (stack_s x_0 x_1 x_2 x_SSTORE_0 2 (bvadd (_ bv63 6) ?x2272))) (= ?x4285 (+ 3 ?x2228)) $x11383 $x7930 $x6998 $x2063 $x2389 (= ?x8004 (+ ?x4285 ?x11937)) (= (sc_s 4) ?x6476) $x4408 $x7542 $x7178 (= ?x4319 (bvadd (_ bv63 6) (sc_s 4))) $x1281 $x9941 $x9062 $x9894 (= (used_gas_t x_0 x_1 x_2 x_SSTORE_0 1) (+ 3 ?x3775)) (= (sc_t 1) ?x63) $x10217 $x10029 (= (exc_halt_t 1) (or $x56 (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) ?x63))))) (= ?x8540 (+ 2 (used_gas_t x_0 x_1 x_2 x_SSTORE_0 1))) (= (sc_t 2) (bvadd (_ bv63 6) (sc_t 1))) $x8730 $x11839 $x7247 $x7529 (= (used_gas_t x_0 x_1 x_2 x_SSTORE_0 3) ?x638) (= ?x11964 (bvadd (_ bv62 6) (sc_t 2))) $x8490 $x1657 $x73 $x10515 $x58 $x8747 $x1796 (not (and $x3659 $x4109 $x34 $x11543)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
 )
(check-sat)
(get-proof)
