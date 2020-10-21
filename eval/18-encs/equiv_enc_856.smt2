; SWAP1 SWAP2 SSTORE PUSH cw_1 DUP2 SWAP1 => DUP2 SWAP3 SSTORE PUSH cw_1
(set-option :produce-proofs true)
; 
(set-info :status unknown)
(declare-fun storage_t ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 256)) (_ BitVec 256))
(declare-fun w_1 () (_ BitVec 256))
(declare-fun storage_s ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 256)) (_ BitVec 256))
(declare-fun exc_halt_t (Int) Bool)
(declare-fun exc_halt_s (Int) Bool)
(declare-fun sc_t (Int) (_ BitVec 6))
(declare-fun stack_t ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 6)) (_ BitVec 256))
(declare-fun stack_s ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 6)) (_ BitVec 256))
(declare-fun sc_s (Int) (_ BitVec 6))
(declare-fun used_gas_t ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int) Int)
(declare-fun used_gas_s ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int) Int)
(assert
 (exists ((x_0 (_ BitVec 256)) (x_1 (_ BitVec 256)) (x_2 (_ BitVec 256)) (x_SSTORE_0 (_ BitVec 256)) )(let (($x3367 (forall ((w (_ BitVec 256)) )(let ((?x6194 (storage_t x_0 x_1 x_2 x_SSTORE_0 w_1 4 w)))
 (let ((?x1723 (storage_s x_0 x_1 x_2 x_SSTORE_0 w_1 6 w)))
 (= ?x1723 ?x6194))))
 ))
 (let (($x7854 (exc_halt_t 4)))
 (let (($x7121 (exc_halt_s 6)))
 (let (($x2100 (= $x7121 $x7854)))
 (let (($x3285 (forall ((n (_ BitVec 6)) )(let ((?x7495 (sc_t 4)))
 (let (($x9682 (bvsle ?x7495 n)))
 (let ((?x4689 (stack_t x_0 x_1 x_2 x_SSTORE_0 w_1 4 n)))
 (let ((?x11978 (stack_s x_0 x_1 x_2 x_SSTORE_0 w_1 6 n)))
 (let (($x5918 (= ?x11978 ?x4689)))
 (or $x5918 $x9682)))))))
 ))
 (let ((?x7495 (sc_t 4)))
 (let ((?x9114 (sc_s 6)))
 (let (($x7142 (= ?x9114 ?x7495)))
 (let ((?x3077 (used_gas_t x_0 x_1 x_2 x_SSTORE_0 w_1 0)))
 (let ((?x2218 (used_gas_s x_0 x_1 x_2 x_SSTORE_0 w_1 0)))
 (let (($x3782 (= ?x2218 ?x3077)))
 (let (($x8814 (forall ((w (_ BitVec 256)) )(let ((?x9639 (storage_t x_0 x_1 x_2 x_SSTORE_0 w_1 0 w)))
 (let ((?x7863 (storage_s x_0 x_1 x_2 x_SSTORE_0 w_1 0 w)))
 (= ?x7863 ?x9639))))
 ))
 (let (($x56 (exc_halt_t 0)))
 (let (($x57 (exc_halt_s 0)))
 (let (($x58 (= $x57 $x56)))
 (let (($x8677 (forall ((n (_ BitVec 6)) )(let ((?x63 (sc_t 0)))
 (let (($x7659 (bvsle ?x63 n)))
 (let ((?x5057 (stack_t x_0 x_1 x_2 x_SSTORE_0 w_1 0 n)))
 (let ((?x9116 (stack_s x_0 x_1 x_2 x_SSTORE_0 w_1 0 n)))
 (let (($x3567 (= ?x9116 ?x5057)))
 (or $x3567 $x7659)))))))
 ))
 (let ((?x63 (sc_t 0)))
 (let ((?x72 (sc_s 0)))
 (let (($x73 (= ?x72 ?x63)))
 (let (($x9131 (exc_halt_t 3)))
 (let (($x689 (or $x9131 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_t 3)))) (_ bv0 1))))))
 (let (($x7209 (forall ((w (_ BitVec 256)) )(let ((?x9325 (storage_t x_0 x_1 x_2 x_SSTORE_0 w_1 3 w)))
 (let ((?x6194 (storage_t x_0 x_1 x_2 x_SSTORE_0 w_1 4 w)))
 (= ?x6194 ?x9325))))
 ))
 (let (($x5892 (forall ((n (_ BitVec 6)) )(let ((?x10013 (sc_t 3)))
 (let (($x9392 (bvsle ?x10013 n)))
 (let ((?x10631 (stack_t x_0 x_1 x_2 x_SSTORE_0 w_1 3 n)))
 (let ((?x4689 (stack_t x_0 x_1 x_2 x_SSTORE_0 w_1 4 n)))
 (or (= ?x4689 ?x10631) $x9392))))))
 ))
 (let (($x6122 (= (used_gas_t x_0 x_1 x_2 x_SSTORE_0 w_1 4) (+ 3 (used_gas_t x_0 x_1 x_2 x_SSTORE_0 w_1 3)))))
 (let (($x6907 (= $x9131 (or (exc_halt_t 2) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_t 2))))))))
 (let (($x5886 (forall ((n (_ BitVec 6)) )(let ((?x1685 (stack_t x_0 x_1 x_2 x_SSTORE_0 w_1 2 n)))
 (let ((?x10631 (stack_t x_0 x_1 x_2 x_SSTORE_0 w_1 3 n)))
 (or (= ?x10631 ?x1685) (bvsle (bvadd (_ bv62 6) (sc_t 2)) n)))))
 ))
 (let ((?x11248 (sc_t 2)))
 (let ((?x1412 (bvadd (_ bv62 6) ?x11248)))
 (let ((?x334 (stack_t x_0 x_1 x_2 x_SSTORE_0 w_1 2 ?x1412)))
 (let (($x4669 (= ?x334 (_ bv0 256))))
 (let ((?x6443 (bvadd (_ bv63 6) ?x11248)))
 (let ((?x112 (stack_t x_0 x_1 x_2 x_SSTORE_0 w_1 2 ?x6443)))
 (let ((?x8546 (ite (= (storage_t x_0 x_1 x_2 x_SSTORE_0 w_1 2 ?x112) (_ bv0 256)) (ite $x4669 5000 20000) (ite $x4669 (- 10000) 5000))))
 (let ((?x11285 (used_gas_t x_0 x_1 x_2 x_SSTORE_0 w_1 2)))
 (let ((?x11118 (used_gas_t x_0 x_1 x_2 x_SSTORE_0 w_1 3)))
 (let (($x114 (forall ((w (_ BitVec 256)) )(let ((?x7661 (storage_t x_0 x_1 x_2 x_SSTORE_0 w_1 2 w)))
 (let (($x4173 (= w (stack_t x_0 x_1 x_2 x_SSTORE_0 w_1 2 (bvadd (_ bv63 6) (sc_t 2))))))
 (let ((?x4565 (ite $x4173 (stack_t x_0 x_1 x_2 x_SSTORE_0 w_1 2 (bvadd (_ bv62 6) (sc_t 2))) ?x7661)))
 (let ((?x9325 (storage_t x_0 x_1 x_2 x_SSTORE_0 w_1 3 w)))
 (= ?x9325 ?x4565))))))
 ))
 (let (($x8186 (exc_halt_t 2)))
 (let (($x5196 (= $x8186 (or (exc_halt_t 1) (not (bvsle (_ bv0 6) (bvadd (_ bv60 6) (sc_t 1))))))))
 (let (($x8571 (forall ((w (_ BitVec 256)) )(let ((?x8340 (storage_t x_0 x_1 x_2 x_SSTORE_0 w_1 1 w)))
 (let ((?x7661 (storage_t x_0 x_1 x_2 x_SSTORE_0 w_1 2 w)))
 (= ?x7661 ?x8340))))
 ))
 (let (($x11950 (forall ((n (_ BitVec 6)) )(let ((?x9666 (sc_t 1)))
 (let ((?x1460 (bvadd (_ bv60 6) ?x9666)))
 (let (($x5000 (bvsle ?x1460 n)))
 (let ((?x10271 (stack_t x_0 x_1 x_2 x_SSTORE_0 w_1 1 n)))
 (let ((?x1685 (stack_t x_0 x_1 x_2 x_SSTORE_0 w_1 2 n)))
 (or (= ?x1685 ?x10271) $x5000)))))))
 ))
 (let (($x7617 (= ?x334 (stack_t x_0 x_1 x_2 x_SSTORE_0 w_1 1 (bvadd (_ bv62 6) (sc_t 1))))))
 (let (($x8126 (= (stack_t x_0 x_1 x_2 x_SSTORE_0 w_1 2 (bvadd (_ bv61 6) ?x11248)) (stack_t x_0 x_1 x_2 x_SSTORE_0 w_1 1 (bvadd (_ bv61 6) (sc_t 1))))))
 (let ((?x9666 (sc_t 1)))
 (let ((?x3832 (bvadd (_ bv63 6) ?x9666)))
 (let ((?x8792 (stack_t x_0 x_1 x_2 x_SSTORE_0 w_1 1 ?x3832)))
 (let (($x124 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x63))) (_ bv0 1)))))
 (let (($x4852 (exc_halt_t 1)))
 (let (($x8293 (= $x4852 (or $x56 $x124 (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) ?x63)))))))
 (let (($x10552 (forall ((w (_ BitVec 256)) )(let ((?x9639 (storage_t x_0 x_1 x_2 x_SSTORE_0 w_1 0 w)))
 (let ((?x8340 (storage_t x_0 x_1 x_2 x_SSTORE_0 w_1 1 w)))
 (= ?x8340 ?x9639))))
 ))
 (let (($x11012 (forall ((n (_ BitVec 6)) )(let ((?x63 (sc_t 0)))
 (let ((?x8595 (bvadd (_ bv62 6) ?x63)))
 (let (($x9451 (bvsle ?x8595 n)))
 (let ((?x5057 (stack_t x_0 x_1 x_2 x_SSTORE_0 w_1 0 n)))
 (let ((?x10271 (stack_t x_0 x_1 x_2 x_SSTORE_0 w_1 1 n)))
 (or (= ?x10271 ?x5057) $x9451)))))))
 ))
 (let (($x5368 (= ?x9666 (bvadd (_ bv1 6) ?x63))))
 (let (($x1802 (= (stack_t x_0 x_1 x_2 x_SSTORE_0 w_1 1 (bvadd (_ bv63 6) ?x63)) (stack_t x_0 x_1 x_2 x_SSTORE_0 w_1 0 (bvadd (_ bv63 6) ?x63)))))
 (let ((?x8595 (bvadd (_ bv62 6) ?x63)))
 (let ((?x5608 (stack_t x_0 x_1 x_2 x_SSTORE_0 w_1 0 ?x8595)))
 (let (($x5016 (= $x7121 (or (exc_halt_s 5) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 5))))))))
 (let (($x640 (forall ((w (_ BitVec 256)) )(let ((?x6917 (storage_s x_0 x_1 x_2 x_SSTORE_0 w_1 5 w)))
 (let ((?x1723 (storage_s x_0 x_1 x_2 x_SSTORE_0 w_1 6 w)))
 (= ?x1723 ?x6917))))
 ))
 (let (($x2134 (forall ((n (_ BitVec 6)) )(let ((?x4319 (sc_s 5)))
 (let ((?x11919 (bvadd (_ bv62 6) ?x4319)))
 (let (($x8604 (bvsle ?x11919 n)))
 (let ((?x412 (stack_s x_0 x_1 x_2 x_SSTORE_0 w_1 5 n)))
 (let ((?x11978 (stack_s x_0 x_1 x_2 x_SSTORE_0 w_1 6 n)))
 (or (= ?x11978 ?x412) $x8604)))))))
 ))
 (let ((?x4319 (sc_s 5)))
 (let (($x5723 (= ?x9114 ?x4319)))
 (let (($x5493 (= (used_gas_s x_0 x_1 x_2 x_SSTORE_0 w_1 6) (+ 3 (used_gas_s x_0 x_1 x_2 x_SSTORE_0 w_1 5)))))
 (let ((?x2623 (bvadd (_ bv63 6) ?x4319)))
 (let ((?x4828 (stack_s x_0 x_1 x_2 x_SSTORE_0 w_1 5 ?x2623)))
 (let (($x11830 (= (stack_s x_0 x_1 x_2 x_SSTORE_0 w_1 6 (bvadd (_ bv63 6) ?x9114)) (stack_s x_0 x_1 x_2 x_SSTORE_0 w_1 5 (bvadd (_ bv62 6) ?x4319)))))
 (let (($x9175 (exc_halt_s 4)))
 (let (($x8671 (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 4))))))
 (let (($x7357 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 4)))) (_ bv0 1)))))
 (let (($x1862 (exc_halt_s 5)))
 (let (($x11589 (forall ((w (_ BitVec 256)) )(let ((?x8703 (storage_s x_0 x_1 x_2 x_SSTORE_0 w_1 4 w)))
 (let ((?x6917 (storage_s x_0 x_1 x_2 x_SSTORE_0 w_1 5 w)))
 (= ?x6917 ?x8703))))
 ))
 (let (($x3935 (forall ((n (_ BitVec 6)) )(let ((?x5534 (stack_s x_0 x_1 x_2 x_SSTORE_0 w_1 4 n)))
 (let ((?x412 (stack_s x_0 x_1 x_2 x_SSTORE_0 w_1 5 n)))
 (let ((?x9433 (sc_s 4)))
 (let ((?x3168 (bvadd (_ bv62 6) ?x9433)))
 (let (($x6251 (bvsle ?x3168 n)))
 (or $x6251 (= ?x412 ?x5534))))))))
 ))
 (let (($x6407 (= ?x4319 (bvadd (_ bv1 6) (sc_s 4)))))
 (let ((?x4520 (used_gas_s x_0 x_1 x_2 x_SSTORE_0 w_1 5)))
 (let (($x1596 (= (stack_s x_0 x_1 x_2 x_SSTORE_0 w_1 5 (bvadd (_ bv63 6) (sc_s 4))) (stack_s x_0 x_1 x_2 x_SSTORE_0 w_1 4 (bvadd (_ bv63 6) (sc_s 4))))))
 (let ((?x9433 (sc_s 4)))
 (let ((?x3168 (bvadd (_ bv62 6) ?x9433)))
 (let ((?x783 (stack_s x_0 x_1 x_2 x_SSTORE_0 w_1 4 ?x3168)))
 (let (($x6433 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 3)))) (_ bv0 1)))))
 (let (($x8103 (exc_halt_s 3)))
 (let (($x6214 (forall ((w (_ BitVec 256)) )(let ((?x1391 (storage_s x_0 x_1 x_2 x_SSTORE_0 w_1 3 w)))
 (let ((?x8703 (storage_s x_0 x_1 x_2 x_SSTORE_0 w_1 4 w)))
 (= ?x8703 ?x1391))))
 ))
 (let (($x5073 (forall ((n (_ BitVec 6)) )(let ((?x8490 (stack_s x_0 x_1 x_2 x_SSTORE_0 w_1 3 n)))
 (let ((?x5534 (stack_s x_0 x_1 x_2 x_SSTORE_0 w_1 4 n)))
 (or (bvsle (sc_s 3) n) (= ?x5534 ?x8490)))))
 ))
 (let (($x2045 (= ?x9433 (bvadd (_ bv1 6) (sc_s 3)))))
 (let ((?x2563 (used_gas_s x_0 x_1 x_2 x_SSTORE_0 w_1 4)))
 (let (($x9252 (= $x8103 (or (exc_halt_s 2) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 2))))))))
 (let (($x4895 (forall ((n (_ BitVec 6)) )(let ((?x3718 (stack_s x_0 x_1 x_2 x_SSTORE_0 w_1 2 n)))
 (let ((?x8490 (stack_s x_0 x_1 x_2 x_SSTORE_0 w_1 3 n)))
 (let ((?x2272 (sc_s 2)))
 (let ((?x5303 (bvadd (_ bv62 6) ?x2272)))
 (let (($x1494 (bvsle ?x5303 n)))
 (or $x1494 (= ?x8490 ?x3718))))))))
 ))
 (let ((?x2272 (sc_s 2)))
 (let ((?x5303 (bvadd (_ bv62 6) ?x2272)))
 (let ((?x1804 (stack_s x_0 x_1 x_2 x_SSTORE_0 w_1 2 ?x5303)))
 (let (($x416 (= ?x1804 (_ bv0 256))))
 (let ((?x8276 (bvadd (_ bv63 6) ?x2272)))
 (let ((?x11836 (stack_s x_0 x_1 x_2 x_SSTORE_0 w_1 2 ?x8276)))
 (let ((?x7806 (ite (= (storage_s x_0 x_1 x_2 x_SSTORE_0 w_1 2 ?x11836) (_ bv0 256)) (ite $x416 5000 20000) (ite $x416 (- 10000) 5000))))
 (let ((?x9697 (used_gas_s x_0 x_1 x_2 x_SSTORE_0 w_1 2)))
 (let ((?x11594 (used_gas_s x_0 x_1 x_2 x_SSTORE_0 w_1 3)))
 (let (($x1144 (forall ((w (_ BitVec 256)) )(let ((?x5649 (storage_s x_0 x_1 x_2 x_SSTORE_0 w_1 2 w)))
 (let (($x5870 (= w (stack_s x_0 x_1 x_2 x_SSTORE_0 w_1 2 (bvadd (_ bv63 6) (sc_s 2))))))
 (let ((?x4168 (ite $x5870 (stack_s x_0 x_1 x_2 x_SSTORE_0 w_1 2 (bvadd (_ bv62 6) (sc_s 2))) ?x5649)))
 (let ((?x1391 (storage_s x_0 x_1 x_2 x_SSTORE_0 w_1 3 w)))
 (= ?x1391 ?x4168))))))
 ))
 (let (($x10052 (exc_halt_s 2)))
 (let (($x9951 (= $x10052 (or (exc_halt_s 1) (not (bvsle (_ bv0 6) (bvadd (_ bv61 6) (sc_s 1))))))))
 (let (($x5464 (forall ((w (_ BitVec 256)) )(let ((?x4600 (storage_s x_0 x_1 x_2 x_SSTORE_0 w_1 1 w)))
 (let ((?x5649 (storage_s x_0 x_1 x_2 x_SSTORE_0 w_1 2 w)))
 (= ?x5649 ?x4600))))
 ))
 (let (($x11511 (forall ((n (_ BitVec 6)) )(let ((?x8951 (stack_s x_0 x_1 x_2 x_SSTORE_0 w_1 1 n)))
 (let ((?x3718 (stack_s x_0 x_1 x_2 x_SSTORE_0 w_1 2 n)))
 (let ((?x154 (sc_s 1)))
 (let ((?x9844 (bvadd (_ bv61 6) ?x154)))
 (let (($x3348 (bvsle ?x9844 n)))
 (or $x3348 (= ?x3718 ?x8951))))))))
 ))
 (let ((?x154 (sc_s 1)))
 (let (($x2298 (= ?x2272 ?x154)))
 (let ((?x11468 (bvadd (_ bv63 6) ?x154)))
 (let ((?x11471 (stack_s x_0 x_1 x_2 x_SSTORE_0 w_1 1 ?x11468)))
 (let (($x2484 (forall ((w (_ BitVec 256)) )(let ((?x7863 (storage_s x_0 x_1 x_2 x_SSTORE_0 w_1 0 w)))
 (let ((?x4600 (storage_s x_0 x_1 x_2 x_SSTORE_0 w_1 1 w)))
 (= ?x4600 ?x7863))))
 ))
 (let (($x3187 (forall ((n (_ BitVec 6)) )(let ((?x9116 (stack_s x_0 x_1 x_2 x_SSTORE_0 w_1 0 n)))
 (let ((?x8951 (stack_s x_0 x_1 x_2 x_SSTORE_0 w_1 1 n)))
 (or (= ?x8951 ?x9116) (bvsle (bvadd (_ bv62 6) (sc_s 0)) n)))))
 ))
 (let ((?x7703 (bvadd (_ bv62 6) ?x154)))
 (let ((?x455 (stack_s x_0 x_1 x_2 x_SSTORE_0 w_1 1 ?x7703)))
 (let (($x2379 (forall ((w (_ BitVec 256)) )(let (($x5870 (= w (stack_s x_0 x_1 x_2 x_SSTORE_0 w_1 2 (bvadd (_ bv63 6) (sc_s 2))))))
 (let ((?x7863 (storage_s x_0 x_1 x_2 x_SSTORE_0 w_1 0 w)))
 (= ?x7863 (ite $x5870 x_SSTORE_0 (_ bv0 256))))))
 ))
 (let (($x1570 (= ?x2218 0)))
 (let (($x6704 (not $x57)))
 (let (($x4783 (= (stack_s x_0 x_1 x_2 x_SSTORE_0 w_1 0 (_ bv2 6)) x_2)))
 (let (($x5962 (= (stack_s x_0 x_1 x_2 x_SSTORE_0 w_1 0 (_ bv1 6)) x_1)))
 (let (($x7562 (= (stack_s x_0 x_1 x_2 x_SSTORE_0 w_1 0 (_ bv0 6)) x_0)))
 (let (($x9108 (= ?x72 (_ bv3 6))))
 (and $x9108 $x7562 $x5962 $x4783 $x6704 $x1570 $x2379 (= ?x11471 (stack_s x_0 x_1 x_2 x_SSTORE_0 w_1 0 (bvadd (_ bv62 6) ?x72))) (= ?x455 (stack_s x_0 x_1 x_2 x_SSTORE_0 w_1 0 (bvadd (_ bv63 6) ?x72))) (= (used_gas_s x_0 x_1 x_2 x_SSTORE_0 w_1 1) (+ 3 ?x2218)) (= ?x154 ?x72) $x3187 $x2484 (= (exc_halt_s 1) (or $x57 (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) ?x72))))) (= ?x11836 (stack_s x_0 x_1 x_2 x_SSTORE_0 w_1 1 (bvadd (_ bv61 6) ?x154))) (= (stack_s x_0 x_1 x_2 x_SSTORE_0 w_1 2 (bvadd (_ bv61 6) ?x2272)) ?x11471) (= ?x1804 ?x455) (= ?x9697 (+ 3 (used_gas_s x_0 x_1 x_2 x_SSTORE_0 w_1 1))) $x2298 $x11511 $x5464 $x9951 $x1144 (= ?x11594 (+ ?x9697 ?x7806)) (= (sc_s 3) ?x5303) $x4895 $x9252 (= (stack_s x_0 x_1 x_2 x_SSTORE_0 w_1 4 (sc_s 3)) w_1) (= ?x2563 (+ 3 ?x11594)) $x2045 $x5073 $x6214 (= $x9175 (or $x8103 $x6433)) (= ?x4828 ?x783) (= (stack_s x_0 x_1 x_2 x_SSTORE_0 w_1 5 ?x3168) ?x783) $x1596 (= ?x4520 (+ 3 ?x2563)) $x6407 $x3935 $x11589 (= $x1862 (or $x7357 $x8671 $x9175)) $x11830 (= (stack_s x_0 x_1 x_2 x_SSTORE_0 w_1 6 (bvadd (_ bv62 6) ?x9114)) ?x4828) $x5493 $x5723 $x2134 $x640 $x5016 (= ?x8792 ?x5608) (= (stack_t x_0 x_1 x_2 x_SSTORE_0 w_1 1 ?x8595) ?x5608) $x1802 (= (used_gas_t x_0 x_1 x_2 x_SSTORE_0 w_1 1) (+ 3 ?x3077)) $x5368 $x11012 $x10552 $x8293 (= ?x112 (stack_t x_0 x_1 x_2 x_SSTORE_0 w_1 1 (bvadd (_ bv60 6) ?x9666))) (= (stack_t x_0 x_1 x_2 x_SSTORE_0 w_1 2 (bvadd (_ bv60 6) ?x11248)) ?x8792) $x8126 $x7617 (= ?x11285 (+ 3 (used_gas_t x_0 x_1 x_2 x_SSTORE_0 w_1 1))) (= ?x11248 ?x9666) $x11950 $x8571 $x5196 $x114 (= ?x11118 (+ ?x11285 ?x8546)) (= (sc_t 3) ?x1412) $x5886 $x6907 (= (stack_t x_0 x_1 x_2 x_SSTORE_0 w_1 4 (sc_t 3)) w_1) $x6122 (= ?x7495 (bvadd (_ bv1 6) (sc_t 3))) $x5892 $x7209 (= $x7854 $x689) $x73 $x8677 $x58 $x8814 $x3782 (not (and $x7142 $x3285 $x2100 $x3367))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
 )
(check-sat)
(get-proof)
