; DUP1 PUSH cw_1 SWAP1 => PUSH cw_1 DUP2
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
 (exists ((x_0 (_ BitVec 256)) )(let (($x2582 (forall ((w (_ BitVec 256)) )(let ((?x4653 (storage_t x_0 w_1 2 w)))
 (let ((?x5514 (storage_s x_0 w_1 3 w)))
 (= ?x5514 ?x4653))))
 ))
 (let (($x10380 (exc_halt_t 2)))
 (let (($x8103 (exc_halt_s 3)))
 (let (($x4034 (= $x8103 $x10380)))
 (let (($x9508 (forall ((n (_ BitVec 6)) )(let ((?x9193 (sc_t 2)))
 (let (($x2627 (bvsle ?x9193 n)))
 (let ((?x11673 (stack_t x_0 w_1 2 n)))
 (let ((?x11547 (stack_s x_0 w_1 3 n)))
 (let (($x2151 (= ?x11547 ?x11673)))
 (or $x2151 $x2627)))))))
 ))
 (let ((?x9193 (sc_t 2)))
 (let ((?x3851 (sc_s 3)))
 (let (($x9033 (= ?x3851 ?x9193)))
 (let (($x6827 (not (and $x9033 $x9508 $x4034 $x2582))))
 (let ((?x10575 (used_gas_t x_0 w_1 0)))
 (let ((?x10462 (used_gas_s x_0 w_1 0)))
 (let (($x9782 (= ?x10462 ?x10575)))
 (let (($x5451 (forall ((w (_ BitVec 256)) )(let ((?x10650 (storage_t x_0 w_1 0 w)))
 (let ((?x10881 (storage_s x_0 w_1 0 w)))
 (= ?x10881 ?x10650))))
 ))
 (let (($x56 (exc_halt_t 0)))
 (let (($x57 (exc_halt_s 0)))
 (let (($x58 (= $x57 $x56)))
 (let (($x10612 (forall ((n (_ BitVec 6)) )(let ((?x63 (sc_t 0)))
 (let (($x11418 (bvsle ?x63 n)))
 (let ((?x4552 (stack_t x_0 w_1 0 n)))
 (let ((?x11906 (stack_s x_0 w_1 0 n)))
 (let (($x10763 (= ?x11906 ?x4552)))
 (or $x10763 $x11418)))))))
 ))
 (let ((?x63 (sc_t 0)))
 (let ((?x72 (sc_s 0)))
 (let (($x73 (= ?x72 ?x63)))
 (let (($x11933 (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_t 1))))))
 (let (($x11710 (exc_halt_t 1)))
 (let (($x7822 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_t 1)))) (_ bv0 1)))))
 (let (($x5722 (= $x10380 (or $x7822 $x11710 $x11933))))
 (let (($x7863 (forall ((w (_ BitVec 256)) )(let ((?x9287 (storage_t x_0 w_1 1 w)))
 (let ((?x4653 (storage_t x_0 w_1 2 w)))
 (= ?x4653 ?x9287))))
 ))
 (let (($x6245 (forall ((n (_ BitVec 6)) )(let ((?x10366 (stack_t x_0 w_1 1 n)))
 (let ((?x11673 (stack_t x_0 w_1 2 n)))
 (let (($x11662 (= ?x11673 ?x10366)))
 (let ((?x8615 (sc_t 1)))
 (let ((?x10630 (bvadd (_ bv62 6) ?x8615)))
 (let (($x4504 (bvsle ?x10630 n)))
 (or $x4504 $x11662))))))))
 ))
 (let (($x9971 (= ?x9193 (bvadd (_ bv1 6) (sc_t 1)))))
 (let (($x11838 (= (used_gas_t x_0 w_1 2) (+ 3 (used_gas_t x_0 w_1 1)))))
 (let (($x8608 (= (stack_t x_0 w_1 2 (bvadd (_ bv63 6) (sc_t 1))) (stack_t x_0 w_1 1 (bvadd (_ bv63 6) (sc_t 1))))))
 (let ((?x8615 (sc_t 1)))
 (let ((?x10630 (bvadd (_ bv62 6) ?x8615)))
 (let ((?x7083 (stack_t x_0 w_1 1 ?x10630)))
 (let (($x9870 (= $x11710 (or $x56 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x63))) (_ bv0 1)))))))
 (let (($x1591 (forall ((w (_ BitVec 256)) )(let ((?x10650 (storage_t x_0 w_1 0 w)))
 (let ((?x9287 (storage_t x_0 w_1 1 w)))
 (= ?x9287 ?x10650))))
 ))
 (let (($x407 (forall ((n (_ BitVec 6)) )(let ((?x63 (sc_t 0)))
 (let (($x11418 (bvsle ?x63 n)))
 (let ((?x4552 (stack_t x_0 w_1 0 n)))
 (let ((?x10366 (stack_t x_0 w_1 1 n)))
 (let (($x6351 (= ?x10366 ?x4552)))
 (or $x6351 $x11418)))))))
 ))
 (let (($x2614 (= ?x8615 (bvadd (_ bv1 6) ?x63))))
 (let (($x8560 (= $x8103 (or (exc_halt_s 2) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 2))))))))
 (let (($x3652 (forall ((w (_ BitVec 256)) )(let ((?x8525 (storage_s x_0 w_1 2 w)))
 (let ((?x5514 (storage_s x_0 w_1 3 w)))
 (= ?x5514 ?x8525))))
 ))
 (let (($x8252 (forall ((n (_ BitVec 6)) )(let ((?x2272 (sc_s 2)))
 (let ((?x9521 (bvadd (_ bv62 6) ?x2272)))
 (let (($x4857 (bvsle ?x9521 n)))
 (let ((?x10431 (stack_s x_0 w_1 2 n)))
 (let ((?x11547 (stack_s x_0 w_1 3 n)))
 (let (($x4452 (= ?x11547 ?x10431)))
 (or $x4452 $x4857))))))))
 ))
 (let ((?x2272 (sc_s 2)))
 (let (($x5056 (= ?x3851 ?x2272)))
 (let ((?x5067 (used_gas_s x_0 w_1 3)))
 (let ((?x4124 (bvadd (_ bv63 6) ?x2272)))
 (let ((?x10669 (stack_s x_0 w_1 2 ?x4124)))
 (let ((?x9521 (bvadd (_ bv62 6) ?x2272)))
 (let ((?x11278 (stack_s x_0 w_1 2 ?x9521)))
 (let (($x8780 (exc_halt_s 1)))
 (let (($x8009 (or $x8780 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 1)))) (_ bv0 1))))))
 (let (($x10052 (exc_halt_s 2)))
 (let (($x6952 (= $x10052 $x8009)))
 (let (($x889 (forall ((w (_ BitVec 256)) )(let ((?x529 (storage_s x_0 w_1 1 w)))
 (let ((?x8525 (storage_s x_0 w_1 2 w)))
 (= ?x8525 ?x529))))
 ))
 (let (($x7161 (forall ((n (_ BitVec 6)) )(let ((?x154 (sc_s 1)))
 (let (($x5530 (bvsle ?x154 n)))
 (let ((?x2986 (stack_s x_0 w_1 1 n)))
 (let ((?x10431 (stack_s x_0 w_1 2 n)))
 (let (($x4514 (= ?x10431 ?x2986)))
 (or $x4514 $x5530)))))))
 ))
 (let (($x8831 (= ?x2272 (bvadd (_ bv1 6) (sc_s 1)))))
 (let ((?x7260 (used_gas_s x_0 w_1 2)))
 (let (($x3823 (= ?x7260 (+ 3 (used_gas_s x_0 w_1 1)))))
 (let (($x9100 (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) ?x72)))))
 (let (($x967 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x72))) (_ bv0 1)))))
 (let (($x1866 (= $x8780 (or $x57 $x967 $x9100))))
 (let (($x7342 (forall ((w (_ BitVec 256)) )(let ((?x10881 (storage_s x_0 w_1 0 w)))
 (let ((?x529 (storage_s x_0 w_1 1 w)))
 (= ?x529 ?x10881))))
 ))
 (let (($x11283 (forall ((n (_ BitVec 6)) )(let ((?x11906 (stack_s x_0 w_1 0 n)))
 (let ((?x2986 (stack_s x_0 w_1 1 n)))
 (let (($x11380 (= ?x2986 ?x11906)))
 (let ((?x72 (sc_s 0)))
 (let ((?x9431 (bvadd (_ bv63 6) ?x72)))
 (let (($x8622 (bvsle ?x9431 n)))
 (or $x8622 $x11380))))))))
 ))
 (let ((?x154 (sc_s 1)))
 (let (($x10962 (= ?x154 (bvadd (_ bv1 6) ?x72))))
 (let ((?x11080 (used_gas_s x_0 w_1 1)))
 (let (($x1338 (= ?x11080 (+ 3 ?x10462))))
 (let ((?x9431 (bvadd (_ bv63 6) ?x72)))
 (let ((?x9307 (stack_s x_0 w_1 0 ?x9431)))
 (let (($x6780 (forall ((w (_ BitVec 256)) )(let ((?x10881 (storage_s x_0 w_1 0 w)))
 (= ?x10881 (_ bv0 256))))
 ))
 (let (($x9895 (= ?x10462 0)))
 (let (($x10706 (not $x57)))
 (let (($x7185 (= (stack_s x_0 w_1 0 (_ bv0 6)) x_0)))
 (let (($x10788 (= ?x72 (_ bv1 6))))
 (and $x10788 $x7185 $x10706 $x9895 $x6780 (= (stack_s x_0 w_1 1 (bvadd (_ bv63 6) ?x154)) ?x9307) (= (stack_s x_0 w_1 1 ?x9431) ?x9307) $x1338 $x10962 $x11283 $x7342 $x1866 (= (stack_s x_0 w_1 2 ?x154) w_1) $x3823 $x8831 $x7161 $x889 $x6952 (= (stack_s x_0 w_1 3 (bvadd (_ bv63 6) ?x3851)) ?x11278) (= (stack_s x_0 w_1 3 (bvadd (_ bv62 6) ?x3851)) ?x10669) (= ?x5067 (+ 3 ?x7260)) $x5056 $x8252 $x3652 $x8560 (= (stack_t x_0 w_1 1 ?x63) w_1) (= (used_gas_t x_0 w_1 1) (+ 3 ?x10575)) $x2614 $x407 $x1591 $x9870 (= (stack_t x_0 w_1 2 (bvadd (_ bv63 6) ?x9193)) ?x7083) (= (stack_t x_0 w_1 2 ?x10630) ?x7083) $x8608 $x11838 $x9971 $x6245 $x7863 $x5722 $x73 $x10612 $x58 $x5451 $x9782 $x6827)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
 )
(check-sat)
(get-proof)
