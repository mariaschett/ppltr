; PUSH cw_2 PUSH cw_1 POP DUP2 SWAP1 => DUP1 PUSH cw_2
(set-option :produce-proofs true)
; 
(set-info :status unknown)
(declare-fun storage_t ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 256)) (_ BitVec 256))
(declare-fun w_1 () (_ BitVec 256))
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
(assert
 (exists ((x_0 (_ BitVec 256)) )(let (($x4450 (forall ((w (_ BitVec 256)) )(let ((?x6347 (storage_t x_0 w_2 w_1 2 w)))
 (let ((?x1129 (storage_s x_0 w_2 w_1 5 w)))
 (= ?x1129 ?x6347))))
 ))
 (let (($x10349 (exc_halt_t 2)))
 (let (($x1862 (exc_halt_s 5)))
 (let (($x3062 (= $x1862 $x10349)))
 (let (($x11405 (forall ((n (_ BitVec 6)) )(let ((?x10385 (stack_t x_0 w_2 w_1 2 n)))
 (let ((?x661 (stack_s x_0 w_2 w_1 5 n)))
 (let (($x10988 (= ?x661 ?x10385)))
 (let ((?x6198 (sc_t 2)))
 (let (($x9180 (bvsle ?x6198 n)))
 (or $x9180 $x10988)))))))
 ))
 (let ((?x6198 (sc_t 2)))
 (let ((?x4319 (sc_s 5)))
 (let (($x6146 (= ?x4319 ?x6198)))
 (let ((?x937 (used_gas_t x_0 w_2 w_1 0)))
 (let ((?x4425 (used_gas_s x_0 w_2 w_1 0)))
 (let (($x3817 (= ?x4425 ?x937)))
 (let (($x7116 (forall ((w (_ BitVec 256)) )(let ((?x6501 (storage_t x_0 w_2 w_1 0 w)))
 (let ((?x2836 (storage_s x_0 w_2 w_1 0 w)))
 (= ?x2836 ?x6501))))
 ))
 (let (($x56 (exc_halt_t 0)))
 (let (($x57 (exc_halt_s 0)))
 (let (($x58 (= $x57 $x56)))
 (let (($x4941 (forall ((n (_ BitVec 6)) )(let ((?x4738 (stack_t x_0 w_2 w_1 0 n)))
 (let ((?x7659 (stack_s x_0 w_2 w_1 0 n)))
 (let (($x10056 (= ?x7659 ?x4738)))
 (let ((?x63 (sc_t 0)))
 (let (($x5202 (bvsle ?x63 n)))
 (or $x5202 $x10056)))))))
 ))
 (let ((?x63 (sc_t 0)))
 (let ((?x72 (sc_s 0)))
 (let (($x73 (= ?x72 ?x63)))
 (let (($x3194 (exc_halt_t 1)))
 (let (($x6059 (or $x3194 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_t 1)))) (_ bv0 1))))))
 (let (($x10362 (= $x10349 $x6059)))
 (let (($x599 (forall ((w (_ BitVec 256)) )(let ((?x8373 (storage_t x_0 w_2 w_1 1 w)))
 (let ((?x6347 (storage_t x_0 w_2 w_1 2 w)))
 (= ?x6347 ?x8373))))
 ))
 (let (($x8207 (forall ((n (_ BitVec 6)) )(let ((?x6855 (sc_t 1)))
 (let (($x9995 (bvsle ?x6855 n)))
 (let ((?x8186 (stack_t x_0 w_2 w_1 1 n)))
 (let ((?x10385 (stack_t x_0 w_2 w_1 2 n)))
 (or (= ?x10385 ?x8186) $x9995))))))
 ))
 (let (($x10355 (= ?x6198 (bvadd (_ bv1 6) (sc_t 1)))))
 (let (($x9953 (= (used_gas_t x_0 w_2 w_1 2) (+ 3 (used_gas_t x_0 w_2 w_1 1)))))
 (let (($x5071 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x63))) (_ bv0 1)))))
 (let (($x1605 (= $x3194 (or $x56 (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) ?x63))) $x5071))))
 (let (($x5752 (forall ((w (_ BitVec 256)) )(let ((?x6501 (storage_t x_0 w_2 w_1 0 w)))
 (let ((?x8373 (storage_t x_0 w_2 w_1 1 w)))
 (= ?x8373 ?x6501))))
 ))
 (let (($x3570 (forall ((n (_ BitVec 6)) )(let ((?x4738 (stack_t x_0 w_2 w_1 0 n)))
 (let ((?x8186 (stack_t x_0 w_2 w_1 1 n)))
 (let ((?x63 (sc_t 0)))
 (let ((?x7987 (bvadd (_ bv63 6) ?x63)))
 (let (($x6333 (bvsle ?x7987 n)))
 (or $x6333 (= ?x8186 ?x4738))))))))
 ))
 (let ((?x6855 (sc_t 1)))
 (let (($x3505 (= ?x6855 (bvadd (_ bv1 6) ?x63))))
 (let ((?x7987 (bvadd (_ bv63 6) ?x63)))
 (let ((?x4182 (stack_t x_0 w_2 w_1 0 ?x7987)))
 (let (($x10695 (= $x1862 (or (exc_halt_s 4) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 4))))))))
 (let (($x2108 (forall ((w (_ BitVec 256)) )(let ((?x11825 (storage_s x_0 w_2 w_1 4 w)))
 (let ((?x1129 (storage_s x_0 w_2 w_1 5 w)))
 (= ?x1129 ?x11825))))
 ))
 (let (($x1291 (forall ((n (_ BitVec 6)) )(let ((?x9433 (sc_s 4)))
 (let ((?x3745 (bvadd (_ bv62 6) ?x9433)))
 (let (($x7729 (bvsle ?x3745 n)))
 (let ((?x4006 (stack_s x_0 w_2 w_1 4 n)))
 (let ((?x661 (stack_s x_0 w_2 w_1 5 n)))
 (or (= ?x661 ?x4006) $x7729)))))))
 ))
 (let ((?x9433 (sc_s 4)))
 (let (($x5582 (= ?x4319 ?x9433)))
 (let (($x11147 (= (used_gas_s x_0 w_2 w_1 5) (+ 3 (used_gas_s x_0 w_2 w_1 4)))))
 (let ((?x11350 (bvadd (_ bv63 6) ?x9433)))
 (let ((?x5454 (stack_s x_0 w_2 w_1 4 ?x11350)))
 (let (($x7716 (= (stack_s x_0 w_2 w_1 5 (bvadd (_ bv63 6) ?x4319)) (stack_s x_0 w_2 w_1 4 (bvadd (_ bv62 6) ?x9433)))))
 (let (($x8103 (exc_halt_s 3)))
 (let (($x6330 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 3)))) (_ bv0 1)))))
 (let (($x872 (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 3))))))
 (let (($x9175 (exc_halt_s 4)))
 (let (($x7140 (forall ((w (_ BitVec 256)) )(let ((?x3208 (storage_s x_0 w_2 w_1 3 w)))
 (let ((?x11825 (storage_s x_0 w_2 w_1 4 w)))
 (= ?x11825 ?x3208))))
 ))
 (let (($x3820 (forall ((n (_ BitVec 6)) )(let ((?x606 (stack_s x_0 w_2 w_1 3 n)))
 (let ((?x4006 (stack_s x_0 w_2 w_1 4 n)))
 (let ((?x3851 (sc_s 3)))
 (let ((?x7986 (bvadd (_ bv62 6) ?x3851)))
 (let (($x1115 (bvsle ?x7986 n)))
 (or $x1115 (= ?x4006 ?x606))))))))
 ))
 (let (($x9431 (= ?x9433 (bvadd (_ bv1 6) (sc_s 3)))))
 (let ((?x11348 (used_gas_s x_0 w_2 w_1 4)))
 (let (($x10860 (= (stack_s x_0 w_2 w_1 4 (bvadd (_ bv63 6) (sc_s 3))) (stack_s x_0 w_2 w_1 3 (bvadd (_ bv63 6) (sc_s 3))))))
 (let ((?x3851 (sc_s 3)))
 (let ((?x7986 (bvadd (_ bv62 6) ?x3851)))
 (let ((?x2099 (stack_s x_0 w_2 w_1 3 ?x7986)))
 (let (($x7742 (= $x8103 (or (exc_halt_s 2) (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_s 2))))))))
 (let (($x11791 (forall ((w (_ BitVec 256)) )(let ((?x9606 (storage_s x_0 w_2 w_1 2 w)))
 (let ((?x3208 (storage_s x_0 w_2 w_1 3 w)))
 (= ?x3208 ?x9606))))
 ))
 (let (($x806 (forall ((n (_ BitVec 6)) )(let ((?x10427 (stack_s x_0 w_2 w_1 2 n)))
 (let ((?x606 (stack_s x_0 w_2 w_1 3 n)))
 (or (= ?x606 ?x10427) (bvsle (bvadd (_ bv63 6) (sc_s 2)) n)))))
 ))
 (let ((?x2272 (sc_s 2)))
 (let ((?x11939 (bvadd (_ bv63 6) ?x2272)))
 (let (($x2609 (= ?x3851 ?x11939)))
 (let ((?x8167 (used_gas_s x_0 w_2 w_1 3)))
 (let (($x7458 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 1)))) (_ bv0 1)))))
 (let (($x8780 (exc_halt_s 1)))
 (let (($x10052 (exc_halt_s 2)))
 (let (($x7520 (forall ((w (_ BitVec 256)) )(let ((?x9470 (storage_s x_0 w_2 w_1 1 w)))
 (let ((?x9606 (storage_s x_0 w_2 w_1 2 w)))
 (= ?x9606 ?x9470))))
 ))
 (let (($x6168 (forall ((n (_ BitVec 6)) )(let ((?x154 (sc_s 1)))
 (let (($x5171 (bvsle ?x154 n)))
 (let ((?x1327 (stack_s x_0 w_2 w_1 1 n)))
 (let ((?x10427 (stack_s x_0 w_2 w_1 2 n)))
 (or (= ?x10427 ?x1327) $x5171))))))
 ))
 (let (($x773 (= ?x2272 (bvadd (_ bv1 6) (sc_s 1)))))
 (let ((?x2802 (used_gas_s x_0 w_2 w_1 2)))
 (let (($x3070 (= $x8780 (or $x57 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x72))) (_ bv0 1)))))))
 (let (($x8384 (forall ((w (_ BitVec 256)) )(let ((?x2836 (storage_s x_0 w_2 w_1 0 w)))
 (let ((?x9470 (storage_s x_0 w_2 w_1 1 w)))
 (= ?x9470 ?x2836))))
 ))
 (let (($x2749 (forall ((n (_ BitVec 6)) )(let ((?x72 (sc_s 0)))
 (let (($x11523 (bvsle ?x72 n)))
 (let ((?x7659 (stack_s x_0 w_2 w_1 0 n)))
 (let ((?x1327 (stack_s x_0 w_2 w_1 1 n)))
 (or (= ?x1327 ?x7659) $x11523))))))
 ))
 (let ((?x154 (sc_s 1)))
 (let (($x2608 (= ?x154 (bvadd (_ bv1 6) ?x72))))
 (let (($x8381 (forall ((w (_ BitVec 256)) )(let ((?x2836 (storage_s x_0 w_2 w_1 0 w)))
 (= ?x2836 (_ bv0 256))))
 ))
 (let (($x7979 (= ?x4425 0)))
 (let (($x3623 (not $x57)))
 (let (($x9653 (= (stack_s x_0 w_2 w_1 0 (_ bv0 6)) x_0)))
 (let (($x7461 (= ?x72 (_ bv1 6))))
 (and $x7461 $x9653 $x3623 $x7979 $x8381 (= (stack_s x_0 w_2 w_1 1 ?x72) w_2) (= (used_gas_s x_0 w_2 w_1 1) (+ 3 ?x4425)) $x2608 $x2749 $x8384 $x3070 (= (stack_s x_0 w_2 w_1 2 ?x154) w_1) (= ?x2802 (+ 3 (used_gas_s x_0 w_2 w_1 1))) $x773 $x6168 $x7520 (= $x10052 (or $x8780 $x7458)) (= ?x8167 (+ 2 ?x2802)) $x2609 $x806 $x11791 $x7742 (= ?x5454 ?x2099) (= (stack_s x_0 w_2 w_1 4 ?x7986) ?x2099) $x10860 (= ?x11348 (+ 3 ?x8167)) $x9431 $x3820 $x7140 (= $x9175 (or $x872 $x6330 $x8103)) $x7716 (= (stack_s x_0 w_2 w_1 5 (bvadd (_ bv62 6) ?x4319)) ?x5454) $x11147 $x5582 $x1291 $x2108 $x10695 (= (stack_t x_0 w_2 w_1 1 (bvadd (_ bv63 6) ?x6855)) ?x4182) (= (stack_t x_0 w_2 w_1 1 ?x7987) ?x4182) (= (used_gas_t x_0 w_2 w_1 1) (+ 3 ?x937)) $x3505 $x3570 $x5752 $x1605 (= (stack_t x_0 w_2 w_1 2 ?x6855) w_2) $x9953 $x10355 $x8207 $x599 $x10362 $x73 $x4941 $x58 $x7116 $x3817 (not (and $x6146 $x11405 $x3062 $x4450)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
 )
(check-sat)
(get-proof)