; PUSH cw_2 SWAP2 POP MLOAD SWAP1 => MLOAD SWAP1 POP PUSH cw_2
(set-option :produce-proofs true)
; 
(set-info :status unknown)
(declare-fun storage_t ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 256)) (_ BitVec 256))
(declare-fun w_2 () (_ BitVec 256))
(declare-fun storage_s ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 256)) (_ BitVec 256))
(declare-fun exc_halt_t (Int) Bool)
(declare-fun exc_halt_s (Int) Bool)
(declare-fun stack_t ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 6)) (_ BitVec 256))
(declare-fun stack_s ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 6)) (_ BitVec 256))
(declare-fun sc_t (Int) (_ BitVec 6))
(declare-fun sc_s (Int) (_ BitVec 6))
(declare-fun used_gas_t ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int) Int)
(declare-fun used_gas_s ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int) Int)
(declare-fun f_MLOAD ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256)) (_ BitVec 256))
(assert
 (exists ((x_0 (_ BitVec 256)) (x_1 (_ BitVec 256)) (x_MLOAD_0 (_ BitVec 256)) )(let (($x3011 (forall ((w (_ BitVec 256)) )(let ((?x7867 (storage_t x_0 x_1 w_2 x_MLOAD_0 4 w)))
 (let ((?x7868 (storage_s x_0 x_1 w_2 x_MLOAD_0 5 w)))
 (= ?x7868 ?x7867))))
 ))
 (let (($x9842 (exc_halt_t 4)))
 (let (($x3979 (exc_halt_s 5)))
 (let (($x9426 (= $x3979 $x9842)))
 (let (($x8572 (forall ((n (_ BitVec 6)) )(let ((?x3023 (stack_t x_0 x_1 w_2 x_MLOAD_0 4 n)))
 (let ((?x3024 (stack_s x_0 x_1 w_2 x_MLOAD_0 5 n)))
 (let (($x3043 (= ?x3024 ?x3023)))
 (or (bvsle (sc_t 4) n) $x3043)))))
 ))
 (let ((?x9923 (sc_t 4)))
 (let ((?x805 (sc_s 5)))
 (let (($x1444 (= ?x805 ?x9923)))
 (let ((?x8873 (used_gas_t x_0 x_1 w_2 x_MLOAD_0 0)))
 (let ((?x7892 (used_gas_s x_0 x_1 w_2 x_MLOAD_0 0)))
 (let (($x8874 (= ?x7892 ?x8873)))
 (let (($x2388 (forall ((w (_ BitVec 256)) )(let ((?x3045 (storage_t x_0 x_1 w_2 x_MLOAD_0 0 w)))
 (let ((?x2396 (storage_s x_0 x_1 w_2 x_MLOAD_0 0 w)))
 (= ?x2396 ?x3045))))
 ))
 (let (($x56 (exc_halt_t 0)))
 (let (($x57 (exc_halt_s 0)))
 (let (($x58 (= $x57 $x56)))
 (let (($x8569 (forall ((n (_ BitVec 6)) )(let ((?x63 (sc_t 0)))
 (let (($x3168 (bvsle ?x63 n)))
 (let ((?x3052 (stack_t x_0 x_1 w_2 x_MLOAD_0 0 n)))
 (let ((?x2381 (stack_s x_0 x_1 w_2 x_MLOAD_0 0 n)))
 (let (($x2955 (= ?x2381 ?x3052)))
 (or $x2955 $x3168)))))))
 ))
 (let ((?x63 (sc_t 0)))
 (let ((?x72 (sc_s 0)))
 (let (($x73 (= ?x72 ?x63)))
 (let (($x9995 (exc_halt_t 3)))
 (let (($x9529 (or $x9995 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_t 3)))) (_ bv0 1))))))
 (let (($x3761 (= $x9842 $x9529)))
 (let (($x446 (forall ((w (_ BitVec 256)) )(let ((?x2308 (storage_t x_0 x_1 w_2 x_MLOAD_0 3 w)))
 (let ((?x7867 (storage_t x_0 x_1 w_2 x_MLOAD_0 4 w)))
 (= ?x7867 ?x2308))))
 ))
 (let (($x8562 (forall ((n (_ BitVec 6)) )(let ((?x2012 (sc_t 3)))
 (let (($x9347 (bvsle ?x2012 n)))
 (let ((?x2421 (stack_t x_0 x_1 w_2 x_MLOAD_0 3 n)))
 (let ((?x3023 (stack_t x_0 x_1 w_2 x_MLOAD_0 4 n)))
 (or (= ?x3023 ?x2421) $x9347))))))
 ))
 (let (($x9346 (= ?x9923 (bvadd (_ bv1 6) (sc_t 3)))))
 (let (($x8560 (= (used_gas_t x_0 x_1 w_2 x_MLOAD_0 4) (+ 3 (used_gas_t x_0 x_1 w_2 x_MLOAD_0 3)))))
 (let (($x8557 (= $x9995 (or (exc_halt_t 2) (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_t 2))))))))
 (let (($x8554 (forall ((w (_ BitVec 256)) )(let ((?x3085 (storage_t x_0 x_1 w_2 x_MLOAD_0 2 w)))
 (let ((?x2308 (storage_t x_0 x_1 w_2 x_MLOAD_0 3 w)))
 (= ?x2308 ?x3085))))
 ))
 (let (($x8531 (forall ((n (_ BitVec 6)) )(let ((?x3070 (stack_t x_0 x_1 w_2 x_MLOAD_0 2 n)))
 (let ((?x2421 (stack_t x_0 x_1 w_2 x_MLOAD_0 3 n)))
 (or (bvsle (bvadd (_ bv63 6) (sc_t 2)) n) (= ?x2421 ?x3070)))))
 ))
 (let ((?x2414 (used_gas_t x_0 x_1 w_2 x_MLOAD_0 3)))
 (let (($x903 (exc_halt_t 2)))
 (let (($x8537 (= $x903 (or (exc_halt_t 1) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_t 1))))))))
 (let (($x7697 (forall ((w (_ BitVec 256)) )(let ((?x8909 (storage_t x_0 x_1 w_2 x_MLOAD_0 1 w)))
 (let ((?x3085 (storage_t x_0 x_1 w_2 x_MLOAD_0 2 w)))
 (= ?x3085 ?x8909))))
 ))
 (let (($x8641 (forall ((n (_ BitVec 6)) )(let ((?x3018 (stack_t x_0 x_1 w_2 x_MLOAD_0 1 n)))
 (let ((?x3070 (stack_t x_0 x_1 w_2 x_MLOAD_0 2 n)))
 (or (bvsle (bvadd (_ bv62 6) (sc_t 1)) n) (= ?x3070 ?x3018)))))
 ))
 (let ((?x3074 (used_gas_t x_0 x_1 w_2 x_MLOAD_0 2)))
 (let ((?x8611 (stack_t x_0 x_1 w_2 x_MLOAD_0 1 (bvadd (_ bv63 6) (sc_t 1)))))
 (let (($x7735 (= (stack_t x_0 x_1 w_2 x_MLOAD_0 2 (bvadd (_ bv62 6) (sc_t 2))) ?x8611)))
 (let (($x7766 (= (stack_t x_0 x_1 w_2 x_MLOAD_0 2 (bvadd (_ bv63 6) (sc_t 2))) (stack_t x_0 x_1 w_2 x_MLOAD_0 1 (bvadd (_ bv62 6) (sc_t 1))))))
 (let (($x7810 (forall ((w (_ BitVec 256)) )(let ((?x3045 (storage_t x_0 x_1 w_2 x_MLOAD_0 0 w)))
 (let ((?x8909 (storage_t x_0 x_1 w_2 x_MLOAD_0 1 w)))
 (= ?x8909 ?x3045))))
 ))
 (let (($x7808 (forall ((n (_ BitVec 6)) )(let ((?x3052 (stack_t x_0 x_1 w_2 x_MLOAD_0 0 n)))
 (let ((?x3018 (stack_t x_0 x_1 w_2 x_MLOAD_0 1 n)))
 (or (bvsle (bvadd (_ bv63 6) (sc_t 0)) n) (= ?x3018 ?x3052)))))
 ))
 (let ((?x8789 (f_MLOAD x_0 x_1 w_2 x_MLOAD_0 (stack_t x_0 x_1 w_2 x_MLOAD_0 0 (bvadd (_ bv63 6) ?x63)))))
 (let (($x8589 (= $x3979 (or (exc_halt_s 4) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 4))))))))
 (let (($x7693 (forall ((w (_ BitVec 256)) )(let ((?x3012 (storage_s x_0 x_1 w_2 x_MLOAD_0 4 w)))
 (let ((?x7868 (storage_s x_0 x_1 w_2 x_MLOAD_0 5 w)))
 (= ?x7868 ?x3012))))
 ))
 (let (($x8679 (forall ((n (_ BitVec 6)) )(let ((?x7863 (stack_s x_0 x_1 w_2 x_MLOAD_0 4 n)))
 (let ((?x3024 (stack_s x_0 x_1 w_2 x_MLOAD_0 5 n)))
 (or (= ?x3024 ?x7863) (bvsle (bvadd (_ bv62 6) (sc_s 4)) n)))))
 ))
 (let (($x7711 (= (used_gas_s x_0 x_1 w_2 x_MLOAD_0 5) (+ 3 (used_gas_s x_0 x_1 w_2 x_MLOAD_0 4)))))
 (let ((?x8772 (stack_s x_0 x_1 w_2 x_MLOAD_0 4 (bvadd (_ bv63 6) (sc_s 4)))))
 (let (($x7731 (= (stack_s x_0 x_1 w_2 x_MLOAD_0 5 (bvadd (_ bv63 6) ?x805)) (stack_s x_0 x_1 w_2 x_MLOAD_0 4 (bvadd (_ bv62 6) (sc_s 4))))))
 (let (($x64 (exc_halt_s 4)))
 (let (($x8800 (= $x64 (or (exc_halt_s 3) (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_s 3))))))))
 (let (($x8787 (forall ((w (_ BitVec 256)) )(let ((?x7853 (storage_s x_0 x_1 w_2 x_MLOAD_0 3 w)))
 (let ((?x3012 (storage_s x_0 x_1 w_2 x_MLOAD_0 4 w)))
 (= ?x3012 ?x7853))))
 ))
 (let (($x7689 (forall ((n (_ BitVec 6)) )(let ((?x8848 (stack_s x_0 x_1 w_2 x_MLOAD_0 3 n)))
 (let ((?x7863 (stack_s x_0 x_1 w_2 x_MLOAD_0 4 n)))
 (or (bvsle (bvadd (_ bv63 6) (sc_s 3)) n) (= ?x7863 ?x8848)))))
 ))
 (let ((?x2980 (used_gas_s x_0 x_1 w_2 x_MLOAD_0 4)))
 (let ((?x7706 (f_MLOAD x_0 x_1 w_2 x_MLOAD_0 (stack_s x_0 x_1 w_2 x_MLOAD_0 3 (bvadd (_ bv63 6) (sc_s 3))))))
 (let (($x292 (exc_halt_s 3)))
 (let (($x8763 (= $x292 (or (exc_halt_s 2) (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_s 2))))))))
 (let (($x8771 (forall ((w (_ BitVec 256)) )(let ((?x8843 (storage_s x_0 x_1 w_2 x_MLOAD_0 2 w)))
 (let ((?x7853 (storage_s x_0 x_1 w_2 x_MLOAD_0 3 w)))
 (= ?x7853 ?x8843))))
 ))
 (let (($x8770 (forall ((n (_ BitVec 6)) )(let ((?x2962 (stack_s x_0 x_1 w_2 x_MLOAD_0 2 n)))
 (let ((?x8848 (stack_s x_0 x_1 w_2 x_MLOAD_0 3 n)))
 (or (bvsle (bvadd (_ bv63 6) (sc_s 2)) n) (= ?x8848 ?x2962)))))
 ))
 (let ((?x8841 (used_gas_s x_0 x_1 w_2 x_MLOAD_0 3)))
 (let (($x247 (exc_halt_s 2)))
 (let (($x8757 (= $x247 (or (exc_halt_s 1) (not (bvsle (_ bv0 6) (bvadd (_ bv61 6) (sc_s 1))))))))
 (let (($x7804 (forall ((w (_ BitVec 256)) )(let ((?x2844 (storage_s x_0 x_1 w_2 x_MLOAD_0 1 w)))
 (let ((?x8843 (storage_s x_0 x_1 w_2 x_MLOAD_0 2 w)))
 (= ?x8843 ?x2844))))
 ))
 (let (($x7805 (forall ((n (_ BitVec 6)) )(let ((?x8828 (stack_s x_0 x_1 w_2 x_MLOAD_0 1 n)))
 (let ((?x2962 (stack_s x_0 x_1 w_2 x_MLOAD_0 2 n)))
 (or (bvsle (bvadd (_ bv61 6) (sc_s 1)) n) (= ?x2962 ?x8828)))))
 ))
 (let ((?x154 (sc_s 1)))
 (let ((?x218 (sc_s 2)))
 (let (($x9488 (= ?x218 ?x154)))
 (let ((?x8835 (used_gas_s x_0 x_1 w_2 x_MLOAD_0 2)))
 (let (($x7798 (= (stack_s x_0 x_1 w_2 x_MLOAD_0 2 (bvadd (_ bv62 6) ?x218)) (stack_s x_0 x_1 w_2 x_MLOAD_0 1 (bvadd (_ bv62 6) ?x154)))))
 (let (($x7786 (= (stack_s x_0 x_1 w_2 x_MLOAD_0 2 (bvadd (_ bv61 6) ?x218)) (stack_s x_0 x_1 w_2 x_MLOAD_0 1 (bvadd (_ bv63 6) ?x154)))))
 (let (($x8740 (= (stack_s x_0 x_1 w_2 x_MLOAD_0 2 (bvadd (_ bv63 6) ?x218)) (stack_s x_0 x_1 w_2 x_MLOAD_0 1 (bvadd (_ bv61 6) ?x154)))))
 (let (($x189 (exc_halt_s 1)))
 (let (($x9504 (= $x189 (or $x57 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x72))) (_ bv0 1)))))))
 (let (($x7779 (forall ((w (_ BitVec 256)) )(let ((?x2396 (storage_s x_0 x_1 w_2 x_MLOAD_0 0 w)))
 (let ((?x2844 (storage_s x_0 x_1 w_2 x_MLOAD_0 1 w)))
 (= ?x2844 ?x2396))))
 ))
 (let (($x7770 (forall ((n (_ BitVec 6)) )(let ((?x72 (sc_s 0)))
 (let (($x9266 (bvsle ?x72 n)))
 (let ((?x2381 (stack_s x_0 x_1 w_2 x_MLOAD_0 0 n)))
 (let ((?x8828 (stack_s x_0 x_1 w_2 x_MLOAD_0 1 n)))
 (or (= ?x8828 ?x2381) $x9266))))))
 ))
 (let (($x8922 (= ?x154 (bvadd (_ bv1 6) ?x72))))
 (let (($x7728 (forall ((w0 (_ BitVec 256)) )(let (($x7763 (= (stack_s x_0 x_1 w_2 x_MLOAD_0 3 (bvadd (_ bv63 6) (sc_s 3))) w0)))
 (let ((?x8862 (f_MLOAD x_0 x_1 w_2 x_MLOAD_0 w0)))
 (= ?x8862 (ite $x7763 x_MLOAD_0 (_ bv0 256))))))
 ))
 (let (($x7745 (forall ((w (_ BitVec 256)) )(let ((?x2396 (storage_s x_0 x_1 w_2 x_MLOAD_0 0 w)))
 (= ?x2396 (_ bv0 256))))
 ))
 (let (($x7746 (= ?x7892 0)))
 (let (($x5037 (not $x57)))
 (let (($x7738 (= (stack_s x_0 x_1 w_2 x_MLOAD_0 0 (_ bv1 6)) x_1)))
 (let (($x7736 (= (stack_s x_0 x_1 w_2 x_MLOAD_0 0 (_ bv0 6)) x_0)))
 (let (($x8956 (= ?x72 (_ bv2 6))))
 (and $x8956 $x7736 $x7738 $x5037 $x7746 $x7745 $x7728 (= (stack_s x_0 x_1 w_2 x_MLOAD_0 1 ?x72) w_2) (= (used_gas_s x_0 x_1 w_2 x_MLOAD_0 1) (+ 3 ?x7892)) $x8922 $x7770 $x7779 $x9504 $x8740 $x7786 $x7798 (= ?x8835 (+ 3 (used_gas_s x_0 x_1 w_2 x_MLOAD_0 1))) $x9488 $x7805 $x7804 $x8757 (= ?x8841 (+ 2 ?x8835)) (= (sc_s 3) (bvadd (_ bv63 6) ?x218)) $x8770 $x8771 $x8763 (= ?x8772 ?x7706) (= ?x2980 (+ 3 ?x8841)) (= (sc_s 4) (sc_s 3)) $x7689 $x8787 $x8800 $x7731 (= (stack_s x_0 x_1 w_2 x_MLOAD_0 5 (bvadd (_ bv62 6) ?x805)) ?x8772) $x7711 (= ?x805 (sc_s 4)) $x8679 $x7693 $x8589 (= ?x8611 ?x8789) (= (used_gas_t x_0 x_1 w_2 x_MLOAD_0 1) (+ 3 ?x8873)) (= (sc_t 1) ?x63) $x7808 $x7810 (= (exc_halt_t 1) (or $x56 (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) ?x63))))) $x7766 $x7735 (= ?x3074 (+ 3 (used_gas_t x_0 x_1 w_2 x_MLOAD_0 1))) (= (sc_t 2) (sc_t 1)) $x8641 $x7697 $x8537 (= ?x2414 (+ 2 ?x3074)) (= (sc_t 3) (bvadd (_ bv63 6) (sc_t 2))) $x8531 $x8554 $x8557 (= (stack_t x_0 x_1 w_2 x_MLOAD_0 4 (sc_t 3)) w_2) $x8560 $x9346 $x8562 $x446 $x3761 $x73 $x8569 $x58 $x2388 $x8874 (not (and $x1444 $x8572 $x9426 $x3011)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
 )
(check-sat)
(get-proof)