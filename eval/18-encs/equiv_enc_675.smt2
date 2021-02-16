; CALLER PUSH cw_1 SWAP1 => PUSH cw_1 CALLER
(set-option :produce-proofs true)
; 
(set-info :status unknown)
(declare-fun storage_t ((_ BitVec 256) (_ BitVec 256) Int (_ BitVec 256)) (_ BitVec 256))
(declare-fun w_1 () (_ BitVec 256))
(declare-fun storage_s ((_ BitVec 256) (_ BitVec 256) Int (_ BitVec 256)) (_ BitVec 256))
(declare-fun exc_halt_t (Int) Bool)
(declare-fun exc_halt_s (Int) Bool)
(declare-fun sc_t (Int) (_ BitVec 6))
(declare-fun stack_t ((_ BitVec 256) (_ BitVec 256) Int (_ BitVec 6)) (_ BitVec 256))
(declare-fun stack_s ((_ BitVec 256) (_ BitVec 256) Int (_ BitVec 6)) (_ BitVec 256))
(declare-fun sc_s (Int) (_ BitVec 6))
(declare-fun used_gas_t ((_ BitVec 256) (_ BitVec 256) Int) Int)
(declare-fun used_gas_s ((_ BitVec 256) (_ BitVec 256) Int) Int)
(assert
 (exists ((x_CALLER (_ BitVec 256)) )(let (($x3687 (forall ((w (_ BitVec 256)) )(let ((?x10380 (storage_t w_1 x_CALLER 2 w)))
 (let ((?x4599 (storage_s w_1 x_CALLER 3 w)))
 (= ?x4599 ?x10380))))
 ))
 (let (($x3470 (exc_halt_t 2)))
 (let (($x292 (exc_halt_s 3)))
 (let (($x10289 (= $x292 $x3470)))
 (let (($x7139 (forall ((n (_ BitVec 6)) )(let ((?x6158 (sc_t 2)))
 (let (($x5803 (bvsle ?x6158 n)))
 (let ((?x934 (stack_t w_1 x_CALLER 2 n)))
 (let ((?x7083 (stack_s w_1 x_CALLER 3 n)))
 (let (($x3410 (= ?x7083 ?x934)))
 (or $x3410 $x5803)))))))
 ))
 (let ((?x6158 (sc_t 2)))
 (let ((?x275 (sc_s 3)))
 (let (($x5371 (= ?x275 ?x6158)))
 (let ((?x5904 (used_gas_t w_1 x_CALLER 0)))
 (let ((?x1399 (used_gas_s w_1 x_CALLER 0)))
 (let (($x1356 (= ?x1399 ?x5904)))
 (let (($x1442 (forall ((w (_ BitVec 256)) )(let ((?x11349 (storage_t w_1 x_CALLER 0 w)))
 (let ((?x2528 (storage_s w_1 x_CALLER 0 w)))
 (= ?x2528 ?x11349))))
 ))
 (let (($x56 (exc_halt_t 0)))
 (let (($x57 (exc_halt_s 0)))
 (let (($x58 (= $x57 $x56)))
 (let (($x9519 (forall ((n (_ BitVec 6)) )(let ((?x63 (sc_t 0)))
 (let (($x7673 (bvsle ?x63 n)))
 (let ((?x7622 (stack_t w_1 x_CALLER 0 n)))
 (let ((?x9920 (stack_s w_1 x_CALLER 0 n)))
 (let (($x4437 (= ?x9920 ?x7622)))
 (or $x4437 $x7673)))))))
 ))
 (let ((?x63 (sc_t 0)))
 (let ((?x72 (sc_s 0)))
 (let (($x73 (= ?x72 ?x63)))
 (let (($x4195 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_t 1)))) (_ bv0 1)))))
 (let (($x8377 (exc_halt_t 1)))
 (let (($x10932 (= $x3470 (or $x8377 $x4195))))
 (let (($x2959 (forall ((w (_ BitVec 256)) )(let ((?x6674 (storage_t w_1 x_CALLER 1 w)))
 (let ((?x10380 (storage_t w_1 x_CALLER 2 w)))
 (= ?x10380 ?x6674))))
 ))
 (let (($x11104 (forall ((n (_ BitVec 6)) )(let ((?x7154 (sc_t 1)))
 (let (($x3413 (bvsle ?x7154 n)))
 (or $x3413 (= (stack_t w_1 x_CALLER 2 n) (stack_t w_1 x_CALLER 1 n))))))
 ))
 (let (($x2783 (= ?x6158 (bvadd (_ bv1 6) (sc_t 1)))))
 (let (($x39 (= (used_gas_t w_1 x_CALLER 2) (+ 2 (used_gas_t w_1 x_CALLER 1)))))
 (let (($x4983 (= $x8377 (or $x56 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x63))) (_ bv0 1)))))))
 (let (($x5012 (forall ((w (_ BitVec 256)) )(let ((?x11349 (storage_t w_1 x_CALLER 0 w)))
 (let ((?x6674 (storage_t w_1 x_CALLER 1 w)))
 (= ?x6674 ?x11349))))
 ))
 (let (($x10360 (forall ((n (_ BitVec 6)) )(let ((?x63 (sc_t 0)))
 (let (($x7673 (bvsle ?x63 n)))
 (or (= (stack_t w_1 x_CALLER 1 n) (stack_t w_1 x_CALLER 0 n)) $x7673))))
 ))
 (let ((?x7154 (sc_t 1)))
 (let (($x3228 (= ?x7154 (bvadd (_ bv1 6) ?x63))))
 (let (($x7648 (= $x292 (or (exc_halt_s 2) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 2))))))))
 (let (($x6636 (forall ((w (_ BitVec 256)) )(let ((?x11759 (storage_s w_1 x_CALLER 2 w)))
 (let ((?x4599 (storage_s w_1 x_CALLER 3 w)))
 (= ?x4599 ?x11759))))
 ))
 (let (($x3670 (forall ((n (_ BitVec 6)) )(let ((?x218 (sc_s 2)))
 (let ((?x6941 (bvadd (_ bv62 6) ?x218)))
 (let (($x11374 (bvsle ?x6941 n)))
 (or (= (stack_s w_1 x_CALLER 3 n) (stack_s w_1 x_CALLER 2 n)) $x11374)))))
 ))
 (let ((?x218 (sc_s 2)))
 (let (($x8219 (= ?x275 ?x218)))
 (let (($x8757 (= (used_gas_s w_1 x_CALLER 3) (+ 3 (used_gas_s w_1 x_CALLER 2)))))
 (let (($x129 (= (stack_s w_1 x_CALLER 3 (bvadd (_ bv62 6) ?x275)) (stack_s w_1 x_CALLER 2 (bvadd (_ bv63 6) ?x218)))))
 (let (($x5965 (= (stack_s w_1 x_CALLER 3 (bvadd (_ bv63 6) ?x275)) (stack_s w_1 x_CALLER 2 (bvadd (_ bv62 6) ?x218)))))
 (let (($x6759 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 1)))) (_ bv0 1)))))
 (let (($x189 (exc_halt_s 1)))
 (let (($x247 (exc_halt_s 2)))
 (let (($x1333 (= $x247 (or $x189 $x6759))))
 (let (($x2938 (forall ((w (_ BitVec 256)) )(let ((?x10609 (storage_s w_1 x_CALLER 1 w)))
 (let ((?x11759 (storage_s w_1 x_CALLER 2 w)))
 (= ?x11759 ?x10609))))
 ))
 (let (($x10767 (forall ((n (_ BitVec 6)) )(let ((?x154 (sc_s 1)))
 (let (($x9237 (bvsle ?x154 n)))
 (or $x9237 (= (stack_s w_1 x_CALLER 2 n) (stack_s w_1 x_CALLER 1 n))))))
 ))
 (let (($x6266 (= ?x218 (bvadd (_ bv1 6) (sc_s 1)))))
 (let ((?x1523 (used_gas_s w_1 x_CALLER 2)))
 (let (($x10804 (= $x189 (or $x57 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x72))) (_ bv0 1)))))))
 (let (($x1726 (forall ((w (_ BitVec 256)) )(let ((?x2528 (storage_s w_1 x_CALLER 0 w)))
 (let ((?x10609 (storage_s w_1 x_CALLER 1 w)))
 (= ?x10609 ?x2528))))
 ))
 (let (($x1152 (forall ((n (_ BitVec 6)) )(let ((?x72 (sc_s 0)))
 (let (($x3419 (bvsle ?x72 n)))
 (or (= (stack_s w_1 x_CALLER 1 n) (stack_s w_1 x_CALLER 0 n)) $x3419))))
 ))
 (let ((?x154 (sc_s 1)))
 (let (($x8655 (= ?x154 (bvadd (_ bv1 6) ?x72))))
 (let (($x3571 (forall ((w (_ BitVec 256)) )(let ((?x2528 (storage_s w_1 x_CALLER 0 w)))
 (= ?x2528 (_ bv0 256))))
 ))
 (let (($x2265 (= ?x1399 0)))
 (let (($x7303 (not $x57)))
 (let (($x8325 (= ?x72 (_ bv0 6))))
 (and $x8325 $x7303 $x2265 $x3571 (= (stack_s w_1 x_CALLER 1 ?x72) x_CALLER) (= (used_gas_s w_1 x_CALLER 1) (+ 2 ?x1399)) $x8655 $x1152 $x1726 $x10804 (= (stack_s w_1 x_CALLER 2 ?x154) w_1) (= ?x1523 (+ 3 (used_gas_s w_1 x_CALLER 1))) $x6266 $x10767 $x2938 $x1333 $x5965 $x129 $x8757 $x8219 $x3670 $x6636 $x7648 (= (stack_t w_1 x_CALLER 1 ?x63) w_1) (= (used_gas_t w_1 x_CALLER 1) (+ 3 ?x5904)) $x3228 $x10360 $x5012 $x4983 (= (stack_t w_1 x_CALLER 2 ?x7154) x_CALLER) $x39 $x2783 $x11104 $x2959 $x10932 $x73 $x9519 $x58 $x1442 $x1356 (not (and $x5371 $x7139 $x10289 $x3687))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
 )
(check-sat)
(get-proof)