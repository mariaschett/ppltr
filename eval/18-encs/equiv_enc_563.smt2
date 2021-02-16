; PUSH cw_5 PUSH cw_5 PUSH cw_3 POP => PUSH cw_5 DUP1
(set-option :produce-proofs true)
; 
(set-info :status unknown)
(declare-fun storage_t ((_ BitVec 256) (_ BitVec 256) Int (_ BitVec 256)) (_ BitVec 256))
(declare-fun w_3 () (_ BitVec 256))
(declare-fun w_5 () (_ BitVec 256))
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
 (let (($x11939 (forall ((w (_ BitVec 256)) )(let ((?x5033 (storage_t w_5 w_3 2 w)))
 (let ((?x11855 (storage_s w_5 w_3 4 w)))
 (= ?x11855 ?x5033))))
 ))
 (let (($x2163 (exc_halt_t 2)))
 (let (($x7172 (exc_halt_s 4)))
 (let (($x2772 (= $x7172 $x2163)))
 (let (($x10846 (forall ((n (_ BitVec 6)) )(let ((?x11953 (stack_t w_5 w_3 2 n)))
 (let ((?x4951 (stack_s w_5 w_3 4 n)))
 (let (($x5235 (= ?x4951 ?x11953)))
 (let ((?x2714 (sc_t 2)))
 (let (($x1155 (bvsle ?x2714 n)))
 (or $x1155 $x5235)))))))
 ))
 (let ((?x2714 (sc_t 2)))
 (let ((?x4305 (sc_s 4)))
 (let (($x7874 (= ?x4305 ?x2714)))
 (let ((?x11968 (used_gas_t w_5 w_3 0)))
 (let ((?x11264 (used_gas_s w_5 w_3 0)))
 (let (($x11977 (= ?x11264 ?x11968)))
 (let (($x2383 (forall ((w (_ BitVec 256)) )(let ((?x47 (storage_t w_5 w_3 0 w)))
 (let ((?x10872 (storage_s w_5 w_3 0 w)))
 (= ?x10872 ?x47))))
 ))
 (let (($x56 (exc_halt_t 0)))
 (let (($x57 (exc_halt_s 0)))
 (let (($x58 (= $x57 $x56)))
 (let (($x9743 (forall ((n (_ BitVec 6)) )(let ((?x63 (sc_t 0)))
 (let (($x2617 (bvsle ?x63 n)))
 (let ((?x8005 (stack_t w_5 w_3 0 n)))
 (let ((?x2746 (stack_s w_5 w_3 0 n)))
 (let (($x113 (= ?x2746 ?x8005)))
 (or $x113 $x2617)))))))
 ))
 (let ((?x63 (sc_t 0)))
 (let ((?x72 (sc_s 0)))
 (let (($x73 (= ?x72 ?x63)))
 (let (($x9864 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_t 1)))) (_ bv0 1)))))
 (let (($x8377 (exc_halt_t 1)))
 (let (($x2507 (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_t 1))))))
 (let (($x9752 (= $x2163 (or $x2507 $x8377 $x9864))))
 (let (($x3832 (forall ((w (_ BitVec 256)) )(let ((?x11979 (storage_t w_5 w_3 1 w)))
 (let ((?x5033 (storage_t w_5 w_3 2 w)))
 (= ?x5033 ?x11979))))
 ))
 (let (($x2754 (forall ((n (_ BitVec 6)) )(let ((?x7154 (sc_t 1)))
 (let ((?x8231 (bvadd (_ bv63 6) ?x7154)))
 (let (($x8114 (bvsle ?x8231 n)))
 (or $x8114 (= (stack_t w_5 w_3 2 n) (stack_t w_5 w_3 1 n)))))))
 ))
 (let (($x3007 (= ?x2714 (bvadd (_ bv1 6) (sc_t 1)))))
 (let (($x5086 (= (used_gas_t w_5 w_3 2) (+ 3 (used_gas_t w_5 w_3 1)))))
 (let ((?x7154 (sc_t 1)))
 (let ((?x8231 (bvadd (_ bv63 6) ?x7154)))
 (let ((?x4081 (stack_t w_5 w_3 1 ?x8231)))
 (let (($x10110 (= $x8377 (or $x56 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x63))) (_ bv0 1)))))))
 (let (($x669 (forall ((w (_ BitVec 256)) )(let ((?x47 (storage_t w_5 w_3 0 w)))
 (let ((?x11979 (storage_t w_5 w_3 1 w)))
 (= ?x11979 ?x47))))
 ))
 (let (($x4771 (forall ((n (_ BitVec 6)) )(let ((?x63 (sc_t 0)))
 (let (($x2617 (bvsle ?x63 n)))
 (or $x2617 (= (stack_t w_5 w_3 1 n) (stack_t w_5 w_3 0 n))))))
 ))
 (let (($x1385 (= ?x7154 (bvadd (_ bv1 6) ?x63))))
 (let (($x7523 (= $x7172 (or (exc_halt_s 3) (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_s 3))))))))
 (let (($x6969 (forall ((w (_ BitVec 256)) )(let ((?x4810 (storage_s w_5 w_3 3 w)))
 (let ((?x11855 (storage_s w_5 w_3 4 w)))
 (= ?x11855 ?x4810))))
 ))
 (let (($x11616 (forall ((n (_ BitVec 6)) )(let ((?x275 (sc_s 3)))
 (let ((?x413 (bvadd (_ bv63 6) ?x275)))
 (let (($x7464 (bvsle ?x413 n)))
 (or (= (stack_s w_5 w_3 4 n) (stack_s w_5 w_3 3 n)) $x7464)))))
 ))
 (let ((?x275 (sc_s 3)))
 (let ((?x413 (bvadd (_ bv63 6) ?x275)))
 (let (($x9738 (= ?x4305 ?x413)))
 (let (($x524 (= (used_gas_s w_5 w_3 4) (+ 2 (used_gas_s w_5 w_3 3)))))
 (let (($x4387 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 2)))) (_ bv0 1)))))
 (let (($x247 (exc_halt_s 2)))
 (let (($x292 (exc_halt_s 3)))
 (let (($x10157 (= $x292 (or $x247 $x4387))))
 (let (($x5947 (forall ((w (_ BitVec 256)) )(let ((?x6439 (storage_s w_5 w_3 2 w)))
 (let ((?x4810 (storage_s w_5 w_3 3 w)))
 (= ?x4810 ?x6439))))
 ))
 (let (($x7510 (forall ((n (_ BitVec 6)) )(let ((?x218 (sc_s 2)))
 (let (($x9956 (bvsle ?x218 n)))
 (or (= (stack_s w_5 w_3 3 n) (stack_s w_5 w_3 2 n)) $x9956))))
 ))
 (let (($x5768 (= ?x275 (bvadd (_ bv1 6) (sc_s 2)))))
 (let ((?x5191 (used_gas_s w_5 w_3 3)))
 (let (($x1503 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 1)))) (_ bv0 1)))))
 (let (($x189 (exc_halt_s 1)))
 (let (($x5052 (= $x247 (or $x189 $x1503))))
 (let (($x7980 (forall ((w (_ BitVec 256)) )(let ((?x8921 (storage_s w_5 w_3 1 w)))
 (let ((?x6439 (storage_s w_5 w_3 2 w)))
 (= ?x6439 ?x8921))))
 ))
 (let (($x7025 (forall ((n (_ BitVec 6)) )(let ((?x154 (sc_s 1)))
 (let (($x1475 (bvsle ?x154 n)))
 (or $x1475 (= (stack_s w_5 w_3 2 n) (stack_s w_5 w_3 1 n))))))
 ))
 (let ((?x218 (sc_s 2)))
 (let (($x3363 (= ?x218 (bvadd (_ bv1 6) (sc_s 1)))))
 (let ((?x10945 (used_gas_s w_5 w_3 2)))
 (let (($x5377 (= $x189 (or $x57 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x72))) (_ bv0 1)))))))
 (let (($x1716 (forall ((w (_ BitVec 256)) )(let ((?x10872 (storage_s w_5 w_3 0 w)))
 (let ((?x8921 (storage_s w_5 w_3 1 w)))
 (= ?x8921 ?x10872))))
 ))
 (let (($x6364 (forall ((n (_ BitVec 6)) )(let ((?x72 (sc_s 0)))
 (let (($x5566 (bvsle ?x72 n)))
 (or $x5566 (= (stack_s w_5 w_3 1 n) (stack_s w_5 w_3 0 n))))))
 ))
 (let ((?x154 (sc_s 1)))
 (let (($x9024 (= ?x154 (bvadd (_ bv1 6) ?x72))))
 (let (($x7110 (forall ((w (_ BitVec 256)) )(let ((?x10872 (storage_s w_5 w_3 0 w)))
 (= ?x10872 (_ bv0 256))))
 ))
 (let (($x1462 (= ?x11264 0)))
 (let (($x7461 (not $x57)))
 (let (($x1074 (= ?x72 (_ bv0 6))))
 (and $x1074 $x7461 $x1462 $x7110 (= (stack_s w_5 w_3 1 ?x72) w_5) (= (used_gas_s w_5 w_3 1) (+ 3 ?x11264)) $x9024 $x6364 $x1716 $x5377 (= (stack_s w_5 w_3 2 ?x154) w_5) (= ?x10945 (+ 3 (used_gas_s w_5 w_3 1))) $x3363 $x7025 $x7980 $x5052 (= (stack_s w_5 w_3 3 ?x218) w_3) (= ?x5191 (+ 3 ?x10945)) $x5768 $x7510 $x5947 $x10157 $x524 $x9738 $x11616 $x6969 $x7523 (= (stack_t w_5 w_3 1 ?x63) w_5) (= (used_gas_t w_5 w_3 1) (+ 3 ?x11968)) $x1385 $x4771 $x669 $x10110 (= (stack_t w_5 w_3 2 (bvadd (_ bv63 6) ?x2714)) ?x4081) (= (stack_t w_5 w_3 2 ?x8231) ?x4081) $x5086 $x3007 $x2754 $x3832 $x9752 $x73 $x9743 $x58 $x2383 $x11977 (not (and $x7874 $x10846 $x2772 $x11939))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
(check-sat)
(get-proof)