; PUSH 0x00 DUP2 LT => PUSH 0x00
(set-option :produce-proofs true)
; 
(set-info :status unknown)
(declare-fun storage_t ((_ BitVec 256) Int (_ BitVec 256)) (_ BitVec 256))
(declare-fun storage_s ((_ BitVec 256) Int (_ BitVec 256)) (_ BitVec 256))
(declare-fun exc_halt_t (Int) Bool)
(declare-fun exc_halt_s (Int) Bool)
(declare-fun sc_t (Int) (_ BitVec 6))
(declare-fun stack_t ((_ BitVec 256) Int (_ BitVec 6)) (_ BitVec 256))
(declare-fun stack_s ((_ BitVec 256) Int (_ BitVec 6)) (_ BitVec 256))
(declare-fun sc_s (Int) (_ BitVec 6))
(declare-fun used_gas_t ((_ BitVec 256) Int) Int)
(declare-fun used_gas_s ((_ BitVec 256) Int) Int)
(assert
 (exists ((x_0 (_ BitVec 256)) )(let (($x9870 (forall ((w (_ BitVec 256)) )(let ((?x4688 (storage_t x_0 1 w)))
 (let ((?x3333 (storage_s x_0 3 w)))
 (= ?x3333 ?x4688))))
 ))
 (let (($x3751 (exc_halt_t 1)))
 (let (($x8103 (exc_halt_s 3)))
 (let (($x2610 (= $x8103 $x3751)))
 (let (($x10190 (forall ((n (_ BitVec 6)) )(let ((?x2459 (sc_t 1)))
 (let (($x5085 (bvsle ?x2459 n)))
 (let ((?x11847 (stack_t x_0 1 n)))
 (let ((?x769 (stack_s x_0 3 n)))
 (let (($x2662 (= ?x769 ?x11847)))
 (or $x2662 $x5085)))))))
 ))
 (let ((?x2459 (sc_t 1)))
 (let ((?x3851 (sc_s 3)))
 (let (($x3197 (= ?x3851 ?x2459)))
 (let (($x11546 (not (and $x3197 $x10190 $x2610 $x9870))))
 (let ((?x5056 (used_gas_t x_0 0)))
 (let ((?x8540 (used_gas_s x_0 0)))
 (let (($x5711 (= ?x8540 ?x5056)))
 (let (($x3357 (forall ((w (_ BitVec 256)) )(let ((?x3355 (storage_t x_0 0 w)))
 (let ((?x10955 (storage_s x_0 0 w)))
 (= ?x10955 ?x3355))))
 ))
 (let (($x56 (exc_halt_t 0)))
 (let (($x57 (exc_halt_s 0)))
 (let (($x58 (= $x57 $x56)))
 (let (($x7418 (forall ((n (_ BitVec 6)) )(let ((?x7908 (stack_t x_0 0 n)))
 (let ((?x8152 (stack_s x_0 0 n)))
 (let (($x8902 (= ?x8152 ?x7908)))
 (let ((?x63 (sc_t 0)))
 (let (($x6494 (bvsle ?x63 n)))
 (or $x6494 $x8902)))))))
 ))
 (let ((?x63 (sc_t 0)))
 (let ((?x72 (sc_s 0)))
 (let (($x73 (= ?x72 ?x63)))
 (let (($x5133 (= $x3751 (or $x56 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x63))) (_ bv0 1)))))))
 (let (($x1171 (forall ((w (_ BitVec 256)) )(let ((?x3355 (storage_t x_0 0 w)))
 (let ((?x4688 (storage_t x_0 1 w)))
 (= ?x4688 ?x3355))))
 ))
 (let (($x7593 (forall ((n (_ BitVec 6)) )(let ((?x7908 (stack_t x_0 0 n)))
 (let ((?x11847 (stack_t x_0 1 n)))
 (let (($x8560 (= ?x11847 ?x7908)))
 (let ((?x63 (sc_t 0)))
 (let (($x6494 (bvsle ?x63 n)))
 (or $x6494 $x8560)))))))
 ))
 (let (($x3412 (= ?x2459 (bvadd (_ bv1 6) ?x63))))
 (let (($x2060 (= (used_gas_t x_0 1) (+ 3 ?x5056))))
 (let (($x3894 (= $x8103 (or (exc_halt_s 2) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 2))))))))
 (let (($x8746 (forall ((w (_ BitVec 256)) )(let ((?x11501 (storage_s x_0 2 w)))
 (let ((?x3333 (storage_s x_0 3 w)))
 (= ?x3333 ?x11501))))
 ))
 (let (($x2568 (forall ((n (_ BitVec 6)) )(let ((?x2272 (sc_s 2)))
 (let ((?x681 (bvadd (_ bv62 6) ?x2272)))
 (let (($x11843 (bvsle ?x681 n)))
 (let ((?x6611 (stack_s x_0 2 n)))
 (let ((?x769 (stack_s x_0 3 n)))
 (let (($x10371 (= ?x769 ?x6611)))
 (or $x10371 $x11843))))))))
 ))
 (let ((?x2272 (sc_s 2)))
 (let ((?x8323 (bvadd (_ bv63 6) ?x2272)))
 (let (($x5042 (= ?x3851 ?x8323)))
 (let (($x1908 (= (used_gas_s x_0 3) (+ 3 (used_gas_s x_0 2)))))
 (let ((?x2184 (stack_s x_0 2 ?x8323)))
 (let ((?x2271 (bvadd (_ bv63 6) ?x3851)))
 (let ((?x9741 (stack_s x_0 3 ?x2271)))
 (let (($x1828 (= ?x9741 (ite (bvule (stack_s x_0 2 (bvadd (_ bv62 6) ?x2272)) ?x2184) (_ bv0 256) (_ bv1 256)))))
 (let (($x8831 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 1)))) (_ bv0 1)))))
 (let (($x8780 (exc_halt_s 1)))
 (let (($x10052 (exc_halt_s 2)))
 (let (($x3767 (= $x10052 (or (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 1)))) $x8780 $x8831))))
 (let (($x3165 (forall ((w (_ BitVec 256)) )(let ((?x9310 (storage_s x_0 1 w)))
 (let ((?x11501 (storage_s x_0 2 w)))
 (= ?x11501 ?x9310))))
 ))
 (let (($x9935 (forall ((n (_ BitVec 6)) )(let ((?x8818 (stack_s x_0 1 n)))
 (let ((?x6611 (stack_s x_0 2 n)))
 (let (($x9702 (= ?x6611 ?x8818)))
 (let ((?x154 (sc_s 1)))
 (let ((?x5528 (bvadd (_ bv62 6) ?x154)))
 (let (($x5245 (bvsle ?x5528 n)))
 (or $x5245 $x9702))))))))
 ))
 (let (($x8006 (= ?x2272 (bvadd (_ bv1 6) (sc_s 1)))))
 (let ((?x1082 (used_gas_s x_0 2)))
 (let (($x3271 (= ?x1082 (+ 3 (used_gas_s x_0 1)))))
 (let ((?x154 (sc_s 1)))
 (let ((?x3331 (bvadd (_ bv63 6) ?x154)))
 (let ((?x3981 (stack_s x_0 1 ?x3331)))
 (let ((?x5528 (bvadd (_ bv62 6) ?x154)))
 (let ((?x1133 (stack_s x_0 1 ?x5528)))
 (let (($x856 (= $x8780 (or $x57 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x72))) (_ bv0 1)))))))
 (let (($x2774 (forall ((w (_ BitVec 256)) )(let ((?x10955 (storage_s x_0 0 w)))
 (let ((?x9310 (storage_s x_0 1 w)))
 (= ?x9310 ?x10955))))
 ))
 (let (($x7512 (forall ((n (_ BitVec 6)) )(let ((?x72 (sc_s 0)))
 (let (($x8233 (bvsle ?x72 n)))
 (let ((?x8152 (stack_s x_0 0 n)))
 (let ((?x8818 (stack_s x_0 1 n)))
 (let (($x5946 (= ?x8818 ?x8152)))
 (or $x5946 $x8233)))))))
 ))
 (let (($x11039 (= ?x154 (bvadd (_ bv1 6) ?x72))))
 (let ((?x2458 (used_gas_s x_0 1)))
 (let (($x7644 (= ?x2458 (+ 3 ?x8540))))
 (let (($x9330 (forall ((w (_ BitVec 256)) )(let ((?x10955 (storage_s x_0 0 w)))
 (= ?x10955 (_ bv0 256))))
 ))
 (let (($x10383 (= ?x8540 0)))
 (let (($x8061 (not $x57)))
 (let (($x8127 (= (stack_s x_0 0 (_ bv0 6)) x_0)))
 (let (($x7461 (= ?x72 (_ bv1 6))))
 (and $x7461 $x8127 $x8061 $x10383 $x9330 (= (stack_s x_0 1 ?x72) (_ bv0 256)) $x7644 $x11039 $x7512 $x2774 $x856 (= ?x2184 ?x1133) (= (stack_s x_0 2 ?x5528) ?x1133) (= (stack_s x_0 2 ?x3331) ?x3981) $x3271 $x8006 $x9935 $x3165 $x3767 $x1828 $x1908 $x5042 $x2568 $x8746 $x3894 (= (stack_t x_0 1 ?x63) (_ bv0 256)) $x2060 $x3412 $x7593 $x1171 $x5133 $x73 $x7418 $x58 $x3357 $x5711 $x11546)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
 )
(check-sat)
(get-proof)