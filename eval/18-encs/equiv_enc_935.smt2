; PUSH 0x00 DUP5 LT ISZERO => PUSH 0x01
(set-option :produce-proofs true)
; 
(set-info :status unknown)
(declare-fun storage_t ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 256)) (_ BitVec 256))
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
 (exists ((x_0 (_ BitVec 256)) (x_1 (_ BitVec 256)) (x_2 (_ BitVec 256)) (x_3 (_ BitVec 256)) )(let (($x7803 (forall ((w (_ BitVec 256)) )(let ((?x2814 (storage_t x_0 x_1 x_2 x_3 1 w)))
 (let ((?x3589 (storage_s x_0 x_1 x_2 x_3 4 w)))
 (= ?x3589 ?x2814))))
 ))
 (let (($x3194 (exc_halt_t 1)))
 (let (($x9175 (exc_halt_s 4)))
 (let (($x1530 (= $x9175 $x3194)))
 (let (($x9956 (forall ((n (_ BitVec 6)) )(let ((?x6855 (sc_t 1)))
 (let (($x9995 (bvsle ?x6855 n)))
 (let ((?x649 (stack_t x_0 x_1 x_2 x_3 1 n)))
 (let ((?x9252 (stack_s x_0 x_1 x_2 x_3 4 n)))
 (let (($x5077 (= ?x9252 ?x649)))
 (or $x5077 $x9995)))))))
 ))
 (let ((?x6855 (sc_t 1)))
 (let ((?x9433 (sc_s 4)))
 (let (($x1027 (= ?x9433 ?x6855)))
 (let ((?x7781 (used_gas_t x_0 x_1 x_2 x_3 0)))
 (let ((?x1306 (used_gas_s x_0 x_1 x_2 x_3 0)))
 (let (($x8762 (= ?x1306 ?x7781)))
 (let (($x8459 (forall ((w (_ BitVec 256)) )(let ((?x6044 (storage_t x_0 x_1 x_2 x_3 0 w)))
 (let ((?x8303 (storage_s x_0 x_1 x_2 x_3 0 w)))
 (= ?x8303 ?x6044))))
 ))
 (let (($x56 (exc_halt_t 0)))
 (let (($x57 (exc_halt_s 0)))
 (let (($x58 (= $x57 $x56)))
 (let (($x7601 (forall ((n (_ BitVec 6)) )(let ((?x7703 (stack_t x_0 x_1 x_2 x_3 0 n)))
 (let ((?x6760 (stack_s x_0 x_1 x_2 x_3 0 n)))
 (let (($x8812 (= ?x6760 ?x7703)))
 (let ((?x63 (sc_t 0)))
 (let (($x5202 (bvsle ?x63 n)))
 (or $x5202 $x8812)))))))
 ))
 (let ((?x63 (sc_t 0)))
 (let ((?x72 (sc_s 0)))
 (let (($x73 (= ?x72 ?x63)))
 (let (($x6965 (= $x3194 (or $x56 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x63))) (_ bv0 1)))))))
 (let (($x3069 (forall ((w (_ BitVec 256)) )(let ((?x6044 (storage_t x_0 x_1 x_2 x_3 0 w)))
 (let ((?x2814 (storage_t x_0 x_1 x_2 x_3 1 w)))
 (= ?x2814 ?x6044))))
 ))
 (let (($x1284 (forall ((n (_ BitVec 6)) )(let ((?x7703 (stack_t x_0 x_1 x_2 x_3 0 n)))
 (let ((?x649 (stack_t x_0 x_1 x_2 x_3 1 n)))
 (let ((?x63 (sc_t 0)))
 (let (($x5202 (bvsle ?x63 n)))
 (or $x5202 (= ?x649 ?x7703)))))))
 ))
 (let (($x3505 (= ?x6855 (bvadd (_ bv1 6) ?x63))))
 (let (($x7082 (= $x9175 (or (exc_halt_s 3) (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_s 3))))))))
 (let (($x139 (forall ((w (_ BitVec 256)) )(let ((?x5861 (storage_s x_0 x_1 x_2 x_3 3 w)))
 (let ((?x3589 (storage_s x_0 x_1 x_2 x_3 4 w)))
 (= ?x3589 ?x5861))))
 ))
 (let (($x9534 (forall ((n (_ BitVec 6)) )(let ((?x10677 (stack_s x_0 x_1 x_2 x_3 3 n)))
 (let ((?x9252 (stack_s x_0 x_1 x_2 x_3 4 n)))
 (or (= ?x9252 ?x10677) (bvsle (bvadd (_ bv63 6) (sc_s 3)) n)))))
 ))
 (let ((?x3851 (sc_s 3)))
 (let (($x10759 (= ?x9433 ?x3851)))
 (let (($x2432 (= (used_gas_s x_0 x_1 x_2 x_3 4) (+ 3 (used_gas_s x_0 x_1 x_2 x_3 3)))))
 (let ((?x11656 (ite (= (stack_s x_0 x_1 x_2 x_3 3 (bvadd (_ bv63 6) ?x3851)) (_ bv0 256)) (_ bv1 256) (_ bv0 256))))
 (let (($x8103 (exc_halt_s 3)))
 (let (($x12003 (= $x8103 (or (exc_halt_s 2) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 2))))))))
 (let (($x3999 (forall ((w (_ BitVec 256)) )(let ((?x3068 (storage_s x_0 x_1 x_2 x_3 2 w)))
 (let ((?x5861 (storage_s x_0 x_1 x_2 x_3 3 w)))
 (= ?x5861 ?x3068))))
 ))
 (let (($x2983 (forall ((n (_ BitVec 6)) )(let ((?x3909 (stack_s x_0 x_1 x_2 x_3 2 n)))
 (let ((?x10677 (stack_s x_0 x_1 x_2 x_3 3 n)))
 (let ((?x2272 (sc_s 2)))
 (let ((?x10094 (bvadd (_ bv62 6) ?x2272)))
 (let (($x3308 (bvsle ?x10094 n)))
 (or $x3308 (= ?x10677 ?x3909))))))))
 ))
 (let ((?x8292 (used_gas_s x_0 x_1 x_2 x_3 3)))
 (let ((?x2272 (sc_s 2)))
 (let ((?x11939 (bvadd (_ bv63 6) ?x2272)))
 (let ((?x3418 (stack_s x_0 x_1 x_2 x_3 2 ?x11939)))
 (let ((?x9836 (ite (bvule (stack_s x_0 x_1 x_2 x_3 2 (bvadd (_ bv62 6) ?x2272)) ?x3418) (_ bv0 256) (_ bv1 256))))
 (let ((?x6454 (bvadd (_ bv63 6) ?x3851)))
 (let ((?x9027 (stack_s x_0 x_1 x_2 x_3 3 ?x6454)))
 (let (($x8780 (exc_halt_s 1)))
 (let (($x7458 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 1)))) (_ bv0 1)))))
 (let (($x10052 (exc_halt_s 2)))
 (let (($x7668 (forall ((w (_ BitVec 256)) )(let ((?x1734 (storage_s x_0 x_1 x_2 x_3 1 w)))
 (let ((?x3068 (storage_s x_0 x_1 x_2 x_3 2 w)))
 (= ?x3068 ?x1734))))
 ))
 (let (($x3193 (forall ((n (_ BitVec 6)) )(let ((?x7747 (stack_s x_0 x_1 x_2 x_3 1 n)))
 (let ((?x3909 (stack_s x_0 x_1 x_2 x_3 2 n)))
 (or (bvsle (bvadd (_ bv59 6) (sc_s 1)) n) (= ?x3909 ?x7747)))))
 ))
 (let (($x773 (= ?x2272 (bvadd (_ bv1 6) (sc_s 1)))))
 (let ((?x3291 (used_gas_s x_0 x_1 x_2 x_3 2)))
 (let (($x8131 (= (stack_s x_0 x_1 x_2 x_3 2 (bvadd (_ bv63 6) (sc_s 1))) (stack_s x_0 x_1 x_2 x_3 1 (bvadd (_ bv63 6) (sc_s 1))))))
 (let (($x10528 (= (stack_s x_0 x_1 x_2 x_3 2 (bvadd (_ bv62 6) (sc_s 1))) (stack_s x_0 x_1 x_2 x_3 1 (bvadd (_ bv62 6) (sc_s 1))))))
 (let (($x9261 (= (stack_s x_0 x_1 x_2 x_3 2 (bvadd (_ bv61 6) (sc_s 1))) (stack_s x_0 x_1 x_2 x_3 1 (bvadd (_ bv61 6) (sc_s 1))))))
 (let (($x9224 (= (stack_s x_0 x_1 x_2 x_3 2 (bvadd (_ bv60 6) (sc_s 1))) (stack_s x_0 x_1 x_2 x_3 1 (bvadd (_ bv60 6) (sc_s 1))))))
 (let ((?x154 (sc_s 1)))
 (let ((?x3714 (bvadd (_ bv59 6) ?x154)))
 (let ((?x3873 (stack_s x_0 x_1 x_2 x_3 1 ?x3714)))
 (let (($x3070 (= $x8780 (or $x57 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x72))) (_ bv0 1)))))))
 (let (($x422 (forall ((w (_ BitVec 256)) )(let ((?x8303 (storage_s x_0 x_1 x_2 x_3 0 w)))
 (let ((?x1734 (storage_s x_0 x_1 x_2 x_3 1 w)))
 (= ?x1734 ?x8303))))
 ))
 (let (($x7726 (forall ((n (_ BitVec 6)) )(let ((?x6760 (stack_s x_0 x_1 x_2 x_3 0 n)))
 (let ((?x7747 (stack_s x_0 x_1 x_2 x_3 1 n)))
 (or (= ?x7747 ?x6760) (bvsle (sc_s 0) n)))))
 ))
 (let (($x2608 (= ?x154 (bvadd (_ bv1 6) ?x72))))
 (let (($x10191 (forall ((w (_ BitVec 256)) )(let ((?x8303 (storage_s x_0 x_1 x_2 x_3 0 w)))
 (= ?x8303 (_ bv0 256))))
 ))
 (let (($x6661 (= ?x1306 0)))
 (let (($x3623 (not $x57)))
 (let (($x10353 (= (stack_s x_0 x_1 x_2 x_3 0 (_ bv3 6)) x_3)))
 (let (($x771 (= (stack_s x_0 x_1 x_2 x_3 0 (_ bv2 6)) x_2)))
 (let (($x1099 (= (stack_s x_0 x_1 x_2 x_3 0 (_ bv1 6)) x_1)))
 (let (($x1415 (= (stack_s x_0 x_1 x_2 x_3 0 (_ bv0 6)) x_0)))
 (let (($x7141 (= ?x72 (_ bv4 6))))
 (and $x7141 $x1415 $x1099 $x771 $x10353 $x3623 $x6661 $x10191 (= (stack_s x_0 x_1 x_2 x_3 1 ?x72) (_ bv0 256)) (= (used_gas_s x_0 x_1 x_2 x_3 1) (+ 3 ?x1306)) $x2608 $x7726 $x422 $x3070 (= ?x3418 ?x3873) (= (stack_s x_0 x_1 x_2 x_3 2 ?x3714) ?x3873) $x9224 $x9261 $x10528 $x8131 (= ?x3291 (+ 3 (used_gas_s x_0 x_1 x_2 x_3 1))) $x773 $x3193 $x7668 (= $x10052 (or (not (bvsle (_ bv0 6) ?x3714)) $x7458 $x8780)) (= ?x9027 ?x9836) (= ?x8292 (+ 3 ?x3291)) (= ?x3851 ?x11939) $x2983 $x3999 $x12003 (= (stack_s x_0 x_1 x_2 x_3 4 (bvadd (_ bv63 6) ?x9433)) ?x11656) $x2432 $x10759 $x9534 $x139 $x7082 (= (stack_t x_0 x_1 x_2 x_3 1 ?x63) (_ bv1 256)) (= (used_gas_t x_0 x_1 x_2 x_3 1) (+ 3 ?x7781)) $x3505 $x1284 $x3069 $x6965 $x73 $x7601 $x58 $x8459 $x8762 (not (and $x1027 $x9956 $x1530 $x7803)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
 )
(check-sat)
(get-proof)