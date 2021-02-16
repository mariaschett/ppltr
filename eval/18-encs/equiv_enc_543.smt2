; PUSH cw_1 PUSH cw_2 SWAP1 POP => PUSH cw_2
(set-option :produce-proofs true)
; 
(set-info :status unknown)
(declare-fun storage_t ((_ BitVec 256) (_ BitVec 256) Int (_ BitVec 256)) (_ BitVec 256))
(declare-fun w_2 () (_ BitVec 256))
(declare-fun w_1 () (_ BitVec 256))
(declare-fun storage_s ((_ BitVec 256) (_ BitVec 256) Int (_ BitVec 256)) (_ BitVec 256))
(declare-fun exc_halt_t (Int) Bool)
(declare-fun exc_halt_s (Int) Bool)
(declare-fun stack_t ((_ BitVec 256) (_ BitVec 256) Int (_ BitVec 6)) (_ BitVec 256))
(declare-fun stack_s ((_ BitVec 256) (_ BitVec 256) Int (_ BitVec 6)) (_ BitVec 256))
(declare-fun sc_t (Int) (_ BitVec 6))
(declare-fun sc_s (Int) (_ BitVec 6))
(declare-fun used_gas_t ((_ BitVec 256) (_ BitVec 256) Int) Int)
(declare-fun used_gas_s ((_ BitVec 256) (_ BitVec 256) Int) Int)
(assert
 (let (($x2582 (forall ((w (_ BitVec 256)) )(let ((?x6910 (storage_t w_1 w_2 1 w)))
 (let ((?x6398 (storage_s w_1 w_2 4 w)))
 (= ?x6398 ?x6910))))
 ))
 (let (($x8377 (exc_halt_t 1)))
 (let (($x7172 (exc_halt_s 4)))
 (let (($x7436 (= $x7172 $x8377)))
 (let (($x1886 (forall ((n (_ BitVec 6)) )(let ((?x2305 (stack_t w_1 w_2 1 n)))
 (let ((?x11273 (stack_s w_1 w_2 4 n)))
 (let (($x7230 (= ?x11273 ?x2305)))
 (let ((?x7154 (sc_t 1)))
 (let (($x6622 (bvsle ?x7154 n)))
 (or $x6622 $x7230)))))))
 ))
 (let ((?x7154 (sc_t 1)))
 (let ((?x4305 (sc_s 4)))
 (let (($x4015 (= ?x4305 ?x7154)))
 (let ((?x8528 (used_gas_t w_1 w_2 0)))
 (let ((?x10498 (used_gas_s w_1 w_2 0)))
 (let (($x976 (= ?x10498 ?x8528)))
 (let (($x9575 (forall ((w (_ BitVec 256)) )(let ((?x6290 (storage_t w_1 w_2 0 w)))
 (let ((?x10658 (storage_s w_1 w_2 0 w)))
 (= ?x10658 ?x6290))))
 ))
 (let (($x56 (exc_halt_t 0)))
 (let (($x57 (exc_halt_s 0)))
 (let (($x58 (= $x57 $x56)))
 (let (($x9014 (forall ((n (_ BitVec 6)) )(let ((?x999 (stack_t w_1 w_2 0 n)))
 (let ((?x4770 (stack_s w_1 w_2 0 n)))
 (let (($x5344 (= ?x4770 ?x999)))
 (let ((?x63 (sc_t 0)))
 (let (($x4813 (bvsle ?x63 n)))
 (or $x4813 $x5344)))))))
 ))
 (let ((?x63 (sc_t 0)))
 (let ((?x72 (sc_s 0)))
 (let (($x73 (= ?x72 ?x63)))
 (let (($x3613 (= $x8377 (or $x56 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x63))) (_ bv0 1)))))))
 (let (($x2589 (forall ((w (_ BitVec 256)) )(let ((?x6290 (storage_t w_1 w_2 0 w)))
 (let ((?x6910 (storage_t w_1 w_2 1 w)))
 (= ?x6910 ?x6290))))
 ))
 (let (($x6712 (forall ((n (_ BitVec 6)) )(let ((?x63 (sc_t 0)))
 (let (($x4813 (bvsle ?x63 n)))
 (or $x4813 (= (stack_t w_1 w_2 1 n) (stack_t w_1 w_2 0 n))))))
 ))
 (let (($x471 (= ?x7154 (bvadd (_ bv1 6) ?x63))))
 (let (($x7463 (= $x7172 (or (exc_halt_s 3) (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_s 3))))))))
 (let (($x11122 (forall ((w (_ BitVec 256)) )(let ((?x10867 (storage_s w_1 w_2 3 w)))
 (let ((?x6398 (storage_s w_1 w_2 4 w)))
 (= ?x6398 ?x10867))))
 ))
 (let (($x633 (forall ((n (_ BitVec 6)) )(or (bvsle (bvadd (_ bv63 6) (sc_s 3)) n) (= (stack_s w_1 w_2 4 n) (stack_s w_1 w_2 3 n))))
 ))
 (let (($x9416 (= (used_gas_s w_1 w_2 4) (+ 2 (used_gas_s w_1 w_2 3)))))
 (let (($x292 (exc_halt_s 3)))
 (let (($x2300 (= $x292 (or (exc_halt_s 2) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 2))))))))
 (let (($x8849 (forall ((w (_ BitVec 256)) )(let ((?x11139 (storage_s w_1 w_2 2 w)))
 (let ((?x10867 (storage_s w_1 w_2 3 w)))
 (= ?x10867 ?x11139))))
 ))
 (let (($x7159 (forall ((n (_ BitVec 6)) )(or (bvsle (bvadd (_ bv62 6) (sc_s 2)) n) (= (stack_s w_1 w_2 3 n) (stack_s w_1 w_2 2 n))))
 ))
 (let ((?x892 (used_gas_s w_1 w_2 3)))
 (let (($x1295 (= (stack_s w_1 w_2 3 (bvadd (_ bv62 6) (sc_s 3))) (stack_s w_1 w_2 2 (bvadd (_ bv63 6) (sc_s 2))))))
 (let (($x4533 (= (stack_s w_1 w_2 3 (bvadd (_ bv63 6) (sc_s 3))) (stack_s w_1 w_2 2 (bvadd (_ bv62 6) (sc_s 2))))))
 (let (($x189 (exc_halt_s 1)))
 (let (($x589 (or $x189 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 1)))) (_ bv0 1))))))
 (let (($x247 (exc_halt_s 2)))
 (let (($x2037 (forall ((w (_ BitVec 256)) )(let ((?x5899 (storage_s w_1 w_2 1 w)))
 (let ((?x11139 (storage_s w_1 w_2 2 w)))
 (= ?x11139 ?x5899))))
 ))
 (let (($x7462 (forall ((n (_ BitVec 6)) )(or (= (stack_s w_1 w_2 2 n) (stack_s w_1 w_2 1 n)) (bvsle (sc_s 1) n)))
 ))
 (let ((?x377 (used_gas_s w_1 w_2 2)))
 (let (($x8008 (= $x189 (or $x57 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x72))) (_ bv0 1)))))))
 (let (($x4638 (forall ((w (_ BitVec 256)) )(let ((?x10658 (storage_s w_1 w_2 0 w)))
 (let ((?x5899 (storage_s w_1 w_2 1 w)))
 (= ?x5899 ?x10658))))
 ))
 (let (($x27 (forall ((n (_ BitVec 6)) )(let ((?x72 (sc_s 0)))
 (let (($x10187 (bvsle ?x72 n)))
 (or $x10187 (= (stack_s w_1 w_2 1 n) (stack_s w_1 w_2 0 n))))))
 ))
 (let ((?x154 (sc_s 1)))
 (let (($x2373 (= ?x154 (bvadd (_ bv1 6) ?x72))))
 (let (($x2913 (forall ((w (_ BitVec 256)) )(let ((?x10658 (storage_s w_1 w_2 0 w)))
 (= ?x10658 (_ bv0 256))))
 ))
 (let (($x11669 (= ?x10498 0)))
 (let (($x5107 (not $x57)))
 (let (($x1074 (= ?x72 (_ bv0 6))))
 (and $x1074 $x5107 $x11669 $x2913 (= (stack_s w_1 w_2 1 ?x72) w_1) (= (used_gas_s w_1 w_2 1) (+ 3 ?x10498)) $x2373 $x27 $x4638 $x8008 (= (stack_s w_1 w_2 2 ?x154) w_2) (= ?x377 (+ 3 (used_gas_s w_1 w_2 1))) (= (sc_s 2) (bvadd (_ bv1 6) ?x154)) $x7462 $x2037 (= $x247 $x589) $x4533 $x1295 (= ?x892 (+ 3 ?x377)) (= (sc_s 3) (sc_s 2)) $x7159 $x8849 $x2300 $x9416 (= ?x4305 (bvadd (_ bv63 6) (sc_s 3))) $x633 $x11122 $x7463 (= (stack_t w_1 w_2 1 ?x63) w_2) (= (used_gas_t w_1 w_2 1) (+ 3 ?x8528)) $x471 $x6712 $x2589 $x3613 $x73 $x9014 $x58 $x9575 $x976 (not (and $x4015 $x1886 $x7436 $x2582)))))))))))))))))))))))))))))))))))))))))))))))))))))
(check-sat)
(get-proof)