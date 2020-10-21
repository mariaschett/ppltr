; PUSH cw_3 SLOAD PUSH cw_2 SWAP1 DUP3 SWAP1 => PUSH cw_2 DUP2 PUSH cw_3 SLOAD
(set-option :produce-proofs true)
; 
(set-info :status unknown)
(declare-fun storage_t ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 256)) (_ BitVec 256))
(declare-fun w_2 () (_ BitVec 256))
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
 (exists ((x_0 (_ BitVec 256)) (x_SLOAD_0 (_ BitVec 256)) )(let (($x2131 (forall ((w (_ BitVec 256)) )(let ((?x6133 (storage_t x_0 x_SLOAD_0 w_3 w_2 4 w)))
 (let ((?x5827 (storage_s x_0 x_SLOAD_0 w_3 w_2 6 w)))
 (= ?x5827 ?x6133))))
 ))
 (let (($x7854 (exc_halt_t 4)))
 (let (($x7121 (exc_halt_s 6)))
 (let (($x10095 (= $x7121 $x7854)))
 (let (($x3393 (forall ((n (_ BitVec 6)) )(let ((?x2452 (stack_t x_0 x_SLOAD_0 w_3 w_2 4 n)))
 (let ((?x826 (stack_s x_0 x_SLOAD_0 w_3 w_2 6 n)))
 (let (($x5015 (= ?x826 ?x2452)))
 (or $x5015 (bvsle (sc_t 4) n))))))
 ))
 (let ((?x7495 (sc_t 4)))
 (let ((?x9114 (sc_s 6)))
 (let (($x2362 (= ?x9114 ?x7495)))
 (let ((?x8413 (used_gas_t x_0 x_SLOAD_0 w_3 w_2 0)))
 (let ((?x11202 (used_gas_s x_0 x_SLOAD_0 w_3 w_2 0)))
 (let (($x2343 (= ?x11202 ?x8413)))
 (let (($x1916 (forall ((w (_ BitVec 256)) )(let ((?x3270 (storage_t x_0 x_SLOAD_0 w_3 w_2 0 w)))
 (let ((?x4579 (storage_s x_0 x_SLOAD_0 w_3 w_2 0 w)))
 (= ?x4579 ?x3270))))
 ))
 (let (($x56 (exc_halt_t 0)))
 (let (($x57 (exc_halt_s 0)))
 (let (($x58 (= $x57 $x56)))
 (let (($x913 (forall ((n (_ BitVec 6)) )(let ((?x11697 (stack_t x_0 x_SLOAD_0 w_3 w_2 0 n)))
 (let ((?x6716 (stack_s x_0 x_SLOAD_0 w_3 w_2 0 n)))
 (let (($x8117 (= ?x6716 ?x11697)))
 (let ((?x63 (sc_t 0)))
 (let (($x3918 (bvsle ?x63 n)))
 (or $x3918 $x8117)))))))
 ))
 (let ((?x63 (sc_t 0)))
 (let ((?x72 (sc_s 0)))
 (let (($x73 (= ?x72 ?x63)))
 (let (($x3615 (= $x7854 (or (exc_halt_t 3) (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_t 3))))))))
 (let (($x1955 (forall ((w (_ BitVec 256)) )(let ((?x4584 (storage_t x_0 x_SLOAD_0 w_3 w_2 3 w)))
 (let ((?x6133 (storage_t x_0 x_SLOAD_0 w_3 w_2 4 w)))
 (= ?x6133 ?x4584))))
 ))
 (let (($x10735 (forall ((n (_ BitVec 6)) )(let ((?x10667 (stack_t x_0 x_SLOAD_0 w_3 w_2 3 n)))
 (let ((?x2452 (stack_t x_0 x_SLOAD_0 w_3 w_2 4 n)))
 (or (= ?x2452 ?x10667) (bvsle (bvadd (_ bv63 6) (sc_t 3)) n)))))
 ))
 (let (($x5267 (= (used_gas_t x_0 x_SLOAD_0 w_3 w_2 4) (+ 200 (used_gas_t x_0 x_SLOAD_0 w_3 w_2 3)))))
 (let ((?x6178 (storage_t x_0 x_SLOAD_0 w_3 w_2 3 (stack_t x_0 x_SLOAD_0 w_3 w_2 3 (bvadd (_ bv63 6) (sc_t 3))))))
 (let (($x6842 (exc_halt_t 2)))
 (let (($x4050 (or $x6842 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_t 2)))) (_ bv0 1))))))
 (let (($x9131 (exc_halt_t 3)))
 (let (($x11160 (forall ((w (_ BitVec 256)) )(let ((?x7766 (storage_t x_0 x_SLOAD_0 w_3 w_2 2 w)))
 (let ((?x4584 (storage_t x_0 x_SLOAD_0 w_3 w_2 3 w)))
 (= ?x4584 ?x7766))))
 ))
 (let (($x2148 (forall ((n (_ BitVec 6)) )(let ((?x6667 (stack_t x_0 x_SLOAD_0 w_3 w_2 2 n)))
 (let ((?x10667 (stack_t x_0 x_SLOAD_0 w_3 w_2 3 n)))
 (or (= ?x10667 ?x6667) (bvsle (sc_t 2) n)))))
 ))
 (let ((?x1265 (used_gas_t x_0 x_SLOAD_0 w_3 w_2 3)))
 (let (($x11030 (exc_halt_t 1)))
 (let (($x5390 (or (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_t 1)))) (_ bv0 1))) $x11030 (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_t 1)))))))
 (let (($x6322 (forall ((w (_ BitVec 256)) )(let ((?x8136 (storage_t x_0 x_SLOAD_0 w_3 w_2 1 w)))
 (let ((?x7766 (storage_t x_0 x_SLOAD_0 w_3 w_2 2 w)))
 (= ?x7766 ?x8136))))
 ))
 (let (($x3231 (forall ((n (_ BitVec 6)) )(let ((?x3763 (stack_t x_0 x_SLOAD_0 w_3 w_2 1 n)))
 (let ((?x6667 (stack_t x_0 x_SLOAD_0 w_3 w_2 2 n)))
 (or (bvsle (bvadd (_ bv62 6) (sc_t 1)) n) (= ?x6667 ?x3763)))))
 ))
 (let ((?x1852 (used_gas_t x_0 x_SLOAD_0 w_3 w_2 2)))
 (let (($x2653 (= (stack_t x_0 x_SLOAD_0 w_3 w_2 2 (bvadd (_ bv63 6) (sc_t 1))) (stack_t x_0 x_SLOAD_0 w_3 w_2 1 (bvadd (_ bv63 6) (sc_t 1))))))
 (let ((?x4135 (sc_t 1)))
 (let ((?x11941 (bvadd (_ bv62 6) ?x4135)))
 (let ((?x2553 (stack_t x_0 x_SLOAD_0 w_3 w_2 1 ?x11941)))
 (let (($x5126 (= (stack_t x_0 x_SLOAD_0 w_3 w_2 2 (bvadd (_ bv63 6) (sc_t 2))) ?x2553)))
 (let (($x8407 (= $x11030 (or $x56 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x63))) (_ bv0 1)))))))
 (let (($x5082 (forall ((w (_ BitVec 256)) )(let ((?x3270 (storage_t x_0 x_SLOAD_0 w_3 w_2 0 w)))
 (let ((?x8136 (storage_t x_0 x_SLOAD_0 w_3 w_2 1 w)))
 (= ?x8136 ?x3270))))
 ))
 (let (($x966 (forall ((n (_ BitVec 6)) )(let ((?x11697 (stack_t x_0 x_SLOAD_0 w_3 w_2 0 n)))
 (let ((?x3763 (stack_t x_0 x_SLOAD_0 w_3 w_2 1 n)))
 (let ((?x63 (sc_t 0)))
 (let (($x3918 (bvsle ?x63 n)))
 (or $x3918 (= ?x3763 ?x11697)))))))
 ))
 (let (($x7952 (= $x7121 (or (exc_halt_s 5) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 5))))))))
 (let (($x11077 (forall ((w (_ BitVec 256)) )(let ((?x7723 (storage_s x_0 x_SLOAD_0 w_3 w_2 5 w)))
 (let ((?x5827 (storage_s x_0 x_SLOAD_0 w_3 w_2 6 w)))
 (= ?x5827 ?x7723))))
 ))
 (let (($x6926 (forall ((n (_ BitVec 6)) )(let ((?x9445 (stack_s x_0 x_SLOAD_0 w_3 w_2 5 n)))
 (let ((?x826 (stack_s x_0 x_SLOAD_0 w_3 w_2 6 n)))
 (or (bvsle (bvadd (_ bv62 6) (sc_s 5)) n) (= ?x826 ?x9445)))))
 ))
 (let (($x1972 (= (used_gas_s x_0 x_SLOAD_0 w_3 w_2 6) (+ 3 (used_gas_s x_0 x_SLOAD_0 w_3 w_2 5)))))
 (let ((?x4585 (stack_s x_0 x_SLOAD_0 w_3 w_2 5 (bvadd (_ bv63 6) (sc_s 5)))))
 (let (($x11388 (= (stack_s x_0 x_SLOAD_0 w_3 w_2 6 (bvadd (_ bv63 6) ?x9114)) (stack_s x_0 x_SLOAD_0 w_3 w_2 5 (bvadd (_ bv62 6) (sc_s 5))))))
 (let (($x9175 (exc_halt_s 4)))
 (let (($x7650 (or (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 4)))) (_ bv0 1))) (not (bvsle (_ bv0 6) (bvadd (_ bv61 6) (sc_s 4)))) $x9175)))
 (let (($x1862 (exc_halt_s 5)))
 (let (($x11101 (forall ((w (_ BitVec 256)) )(let ((?x756 (storage_s x_0 x_SLOAD_0 w_3 w_2 4 w)))
 (let ((?x7723 (storage_s x_0 x_SLOAD_0 w_3 w_2 5 w)))
 (= ?x7723 ?x756))))
 ))
 (let (($x425 (forall ((n (_ BitVec 6)) )(let ((?x5344 (stack_s x_0 x_SLOAD_0 w_3 w_2 4 n)))
 (let ((?x9445 (stack_s x_0 x_SLOAD_0 w_3 w_2 5 n)))
 (or (bvsle (bvadd (_ bv61 6) (sc_s 4)) n) (= ?x9445 ?x5344)))))
 ))
 (let ((?x7060 (used_gas_s x_0 x_SLOAD_0 w_3 w_2 5)))
 (let ((?x9433 (sc_s 4)))
 (let ((?x11263 (bvadd (_ bv63 6) ?x9433)))
 (let ((?x10403 (stack_s x_0 x_SLOAD_0 w_3 w_2 4 ?x11263)))
 (let ((?x4876 (bvadd (_ bv62 6) ?x9433)))
 (let ((?x11552 (stack_s x_0 x_SLOAD_0 w_3 w_2 4 ?x4876)))
 (let ((?x4610 (bvadd (_ bv61 6) ?x9433)))
 (let ((?x5798 (stack_s x_0 x_SLOAD_0 w_3 w_2 4 ?x4610)))
 (let (($x5510 (= $x9175 (or (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 3)))) (exc_halt_s 3)))))
 (let (($x9467 (forall ((w (_ BitVec 256)) )(let ((?x2233 (storage_s x_0 x_SLOAD_0 w_3 w_2 3 w)))
 (let ((?x756 (storage_s x_0 x_SLOAD_0 w_3 w_2 4 w)))
 (= ?x756 ?x2233))))
 ))
 (let (($x11495 (forall ((n (_ BitVec 6)) )(let ((?x7028 (stack_s x_0 x_SLOAD_0 w_3 w_2 3 n)))
 (let ((?x5344 (stack_s x_0 x_SLOAD_0 w_3 w_2 4 n)))
 (or (= ?x5344 ?x7028) (bvsle (bvadd (_ bv62 6) (sc_s 3)) n)))))
 ))
 (let ((?x6418 (used_gas_s x_0 x_SLOAD_0 w_3 w_2 4)))
 (let (($x22 (= ?x11552 (stack_s x_0 x_SLOAD_0 w_3 w_2 3 (bvadd (_ bv63 6) (sc_s 3))))))
 (let (($x11997 (= ?x10403 (stack_s x_0 x_SLOAD_0 w_3 w_2 3 (bvadd (_ bv62 6) (sc_s 3))))))
 (let (($x10052 (exc_halt_s 2)))
 (let (($x11942 (or (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 2)))) (_ bv0 1))) $x10052)))
 (let (($x8103 (exc_halt_s 3)))
 (let (($x1562 (forall ((w (_ BitVec 256)) )(let ((?x7870 (storage_s x_0 x_SLOAD_0 w_3 w_2 2 w)))
 (let ((?x2233 (storage_s x_0 x_SLOAD_0 w_3 w_2 3 w)))
 (= ?x2233 ?x7870))))
 ))
 (let (($x4292 (forall ((n (_ BitVec 6)) )(let ((?x8461 (stack_s x_0 x_SLOAD_0 w_3 w_2 2 n)))
 (let ((?x7028 (stack_s x_0 x_SLOAD_0 w_3 w_2 3 n)))
 (or (= ?x7028 ?x8461) (bvsle (sc_s 2) n)))))
 ))
 (let ((?x9049 (used_gas_s x_0 x_SLOAD_0 w_3 w_2 3)))
 (let (($x2995 (= $x10052 (or (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_s 1)))) (exc_halt_s 1)))))
 (let (($x11728 (forall ((w (_ BitVec 256)) )(let ((?x6586 (storage_s x_0 x_SLOAD_0 w_3 w_2 1 w)))
 (let ((?x7870 (storage_s x_0 x_SLOAD_0 w_3 w_2 2 w)))
 (= ?x7870 ?x6586))))
 ))
 (let (($x1574 (forall ((n (_ BitVec 6)) )(let ((?x10267 (stack_s x_0 x_SLOAD_0 w_3 w_2 1 n)))
 (let ((?x8461 (stack_s x_0 x_SLOAD_0 w_3 w_2 2 n)))
 (or (bvsle (bvadd (_ bv63 6) (sc_s 1)) n) (= ?x8461 ?x10267)))))
 ))
 (let ((?x11170 (used_gas_s x_0 x_SLOAD_0 w_3 w_2 2)))
 (let ((?x6659 (storage_s x_0 x_SLOAD_0 w_3 w_2 1 (stack_s x_0 x_SLOAD_0 w_3 w_2 1 (bvadd (_ bv63 6) (sc_s 1))))))
 (let (($x6231 (= (stack_s x_0 x_SLOAD_0 w_3 w_2 2 (bvadd (_ bv63 6) (sc_s 2))) ?x6659)))
 (let (($x8780 (exc_halt_s 1)))
 (let (($x11279 (= $x8780 (or $x57 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x72))) (_ bv0 1)))))))
 (let (($x10388 (forall ((w (_ BitVec 256)) )(let ((?x4579 (storage_s x_0 x_SLOAD_0 w_3 w_2 0 w)))
 (let ((?x6586 (storage_s x_0 x_SLOAD_0 w_3 w_2 1 w)))
 (= ?x6586 ?x4579))))
 ))
 (let (($x5365 (forall ((n (_ BitVec 6)) )(let ((?x6716 (stack_s x_0 x_SLOAD_0 w_3 w_2 0 n)))
 (let ((?x10267 (stack_s x_0 x_SLOAD_0 w_3 w_2 1 n)))
 (or (bvsle (sc_s 0) n) (= ?x10267 ?x6716)))))
 ))
 (let (($x10006 (forall ((w (_ BitVec 256)) )(let (($x5558 (= w (stack_s x_0 x_SLOAD_0 w_3 w_2 1 (bvadd (_ bv63 6) (sc_s 1))))))
 (let ((?x4579 (storage_s x_0 x_SLOAD_0 w_3 w_2 0 w)))
 (= ?x4579 (ite $x5558 x_SLOAD_0 (_ bv0 256))))))
 ))
 (let (($x3586 (= ?x11202 0)))
 (let (($x6435 (= (stack_s x_0 x_SLOAD_0 w_3 w_2 0 (_ bv0 6)) x_0)))
 (let (($x7461 (= ?x72 (_ bv1 6))))
 (and $x7461 $x6435 (not $x57) $x3586 $x10006 (= (stack_s x_0 x_SLOAD_0 w_3 w_2 1 ?x72) w_3) (= (used_gas_s x_0 x_SLOAD_0 w_3 w_2 1) (+ 3 ?x11202)) (= (sc_s 1) (bvadd (_ bv1 6) ?x72)) $x5365 $x10388 $x11279 $x6231 (= ?x11170 (+ 200 (used_gas_s x_0 x_SLOAD_0 w_3 w_2 1))) (= (sc_s 2) (sc_s 1)) $x1574 $x11728 $x2995 (= (stack_s x_0 x_SLOAD_0 w_3 w_2 3 (sc_s 2)) w_2) (= ?x9049 (+ 3 ?x11170)) (= (sc_s 3) (bvadd (_ bv1 6) (sc_s 2))) $x4292 $x1562 (= $x8103 $x11942) $x11997 $x22 (= ?x6418 (+ 3 ?x9049)) (= ?x9433 (sc_s 3)) $x11495 $x9467 $x5510 (= ?x4585 ?x5798) (= (stack_s x_0 x_SLOAD_0 w_3 w_2 5 ?x4610) ?x5798) (= (stack_s x_0 x_SLOAD_0 w_3 w_2 5 ?x4876) ?x11552) (= (stack_s x_0 x_SLOAD_0 w_3 w_2 5 ?x11263) ?x10403) (= ?x7060 (+ 3 ?x6418)) (= (sc_s 5) (bvadd (_ bv1 6) ?x9433)) $x425 $x11101 (= $x1862 $x7650) $x11388 (= (stack_s x_0 x_SLOAD_0 w_3 w_2 6 (bvadd (_ bv62 6) ?x9114)) ?x4585) $x1972 (= ?x9114 (sc_s 5)) $x6926 $x11077 $x7952 (= (stack_t x_0 x_SLOAD_0 w_3 w_2 1 ?x63) w_2) (= (used_gas_t x_0 x_SLOAD_0 w_3 w_2 1) (+ 3 ?x8413)) (= ?x4135 (bvadd (_ bv1 6) ?x63)) $x966 $x5082 $x8407 $x5126 (= (stack_t x_0 x_SLOAD_0 w_3 w_2 2 ?x11941) ?x2553) $x2653 (= ?x1852 (+ 3 (used_gas_t x_0 x_SLOAD_0 w_3 w_2 1))) (= (sc_t 2) (bvadd (_ bv1 6) ?x4135)) $x3231 $x6322 (= $x6842 $x5390) (= (stack_t x_0 x_SLOAD_0 w_3 w_2 3 (sc_t 2)) w_3) (= ?x1265 (+ 3 ?x1852)) (= (sc_t 3) (bvadd (_ bv1 6) (sc_t 2))) $x2148 $x11160 (= $x9131 $x4050) (= (stack_t x_0 x_SLOAD_0 w_3 w_2 4 (bvadd (_ bv63 6) ?x7495)) ?x6178) $x5267 (= ?x7495 (sc_t 3)) $x10735 $x1955 $x3615 $x73 $x913 $x58 $x1916 $x2343 (not (and $x2362 $x3393 $x10095 $x2131))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
 )
(check-sat)
(get-proof)
