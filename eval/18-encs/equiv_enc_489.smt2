; PUSH cw_2 DUP1 MLOAD SWAP2 SWAP1 => PUSH cw_2 MLOAD SWAP1 PUSH cw_2
(set-option :produce-proofs true)
; 
(set-info :status unknown)
(declare-fun storage_t ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 256)) (_ BitVec 256))
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
(declare-fun f_MLOAD ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256)) (_ BitVec 256))
(assert
 (exists ((x_0 (_ BitVec 256)) (x_MLOAD_0 (_ BitVec 256)) )(let (($x10704 (forall ((w (_ BitVec 256)) )(let ((?x9521 (storage_t x_0 w_2 x_MLOAD_0 4 w)))
 (let ((?x8138 (storage_s x_0 w_2 x_MLOAD_0 5 w)))
 (= ?x8138 ?x9521))))
 ))
 (let (($x7722 (exc_halt_t 4)))
 (let (($x11317 (exc_halt_s 5)))
 (let (($x4034 (= $x11317 $x7722)))
 (let (($x253 (forall ((n (_ BitVec 6)) )(let ((?x4081 (stack_t x_0 w_2 x_MLOAD_0 4 n)))
 (let ((?x3600 (stack_s x_0 w_2 x_MLOAD_0 5 n)))
 (let (($x9928 (= ?x3600 ?x4081)))
 (let ((?x11631 (sc_t 4)))
 (let (($x4767 (bvsle ?x11631 n)))
 (or $x4767 $x9928)))))))
 ))
 (let ((?x11631 (sc_t 4)))
 (let ((?x4319 (sc_s 5)))
 (let (($x5651 (= ?x4319 ?x11631)))
 (let ((?x11789 (used_gas_t x_0 w_2 x_MLOAD_0 0)))
 (let ((?x7447 (used_gas_s x_0 w_2 x_MLOAD_0 0)))
 (let (($x7238 (= ?x7447 ?x11789)))
 (let (($x10363 (forall ((w (_ BitVec 256)) )(let ((?x9776 (storage_t x_0 w_2 x_MLOAD_0 0 w)))
 (let ((?x4406 (storage_s x_0 w_2 x_MLOAD_0 0 w)))
 (= ?x4406 ?x9776))))
 ))
 (let (($x56 (exc_halt_t 0)))
 (let (($x57 (exc_halt_s 0)))
 (let (($x58 (= $x57 $x56)))
 (let (($x8729 (forall ((n (_ BitVec 6)) )(let ((?x10212 (stack_t x_0 w_2 x_MLOAD_0 0 n)))
 (let ((?x8700 (stack_s x_0 w_2 x_MLOAD_0 0 n)))
 (let (($x11751 (= ?x8700 ?x10212)))
 (let ((?x63 (sc_t 0)))
 (let (($x4790 (bvsle ?x63 n)))
 (or $x4790 $x11751)))))))
 ))
 (let ((?x63 (sc_t 0)))
 (let ((?x72 (sc_s 0)))
 (let (($x73 (= ?x72 ?x63)))
 (let (($x1866 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_t 3)))) (_ bv0 1)))))
 (let (($x6783 (exc_halt_t 3)))
 (let (($x6714 (forall ((w (_ BitVec 256)) )(let ((?x3790 (storage_t x_0 w_2 x_MLOAD_0 3 w)))
 (let ((?x9521 (storage_t x_0 w_2 x_MLOAD_0 4 w)))
 (= ?x9521 ?x3790))))
 ))
 (let (($x4093 (forall ((n (_ BitVec 6)) )(let ((?x6438 (sc_t 3)))
 (let (($x10190 (bvsle ?x6438 n)))
 (let ((?x10567 (stack_t x_0 w_2 x_MLOAD_0 3 n)))
 (let ((?x4081 (stack_t x_0 w_2 x_MLOAD_0 4 n)))
 (or (= ?x4081 ?x10567) $x10190))))))
 ))
 (let (($x6249 (= ?x11631 (bvadd (_ bv1 6) (sc_t 3)))))
 (let (($x449 (= (used_gas_t x_0 w_2 x_MLOAD_0 4) (+ 3 (used_gas_t x_0 w_2 x_MLOAD_0 3)))))
 (let (($x657 (= $x6783 (or (exc_halt_t 2) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_t 2))))))))
 (let (($x2305 (forall ((w (_ BitVec 256)) )(let ((?x1392 (storage_t x_0 w_2 x_MLOAD_0 2 w)))
 (let ((?x3790 (storage_t x_0 w_2 x_MLOAD_0 3 w)))
 (= ?x3790 ?x1392))))
 ))
 (let (($x9032 (forall ((n (_ BitVec 6)) )(let ((?x2714 (sc_t 2)))
 (let ((?x3686 (bvadd (_ bv62 6) ?x2714)))
 (let (($x6499 (bvsle ?x3686 n)))
 (let ((?x8174 (stack_t x_0 w_2 x_MLOAD_0 2 n)))
 (let ((?x10567 (stack_t x_0 w_2 x_MLOAD_0 3 n)))
 (or (= ?x10567 ?x8174) $x6499)))))))
 ))
 (let ((?x2714 (sc_t 2)))
 (let ((?x6438 (sc_t 3)))
 (let (($x9951 (= ?x6438 ?x2714)))
 (let ((?x2492 (used_gas_t x_0 w_2 x_MLOAD_0 3)))
 (let ((?x4833 (bvadd (_ bv63 6) ?x2714)))
 (let ((?x8693 (stack_t x_0 w_2 x_MLOAD_0 2 ?x4833)))
 (let (($x6852 (= (stack_t x_0 w_2 x_MLOAD_0 3 (bvadd (_ bv63 6) ?x6438)) (stack_t x_0 w_2 x_MLOAD_0 2 (bvadd (_ bv62 6) ?x2714)))))
 (let (($x2163 (exc_halt_t 2)))
 (let (($x4332 (= $x2163 (or (exc_halt_t 1) (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_t 1))))))))
 (let (($x11285 (forall ((w (_ BitVec 256)) )(let ((?x2752 (storage_t x_0 w_2 x_MLOAD_0 1 w)))
 (let ((?x1392 (storage_t x_0 w_2 x_MLOAD_0 2 w)))
 (= ?x1392 ?x2752))))
 ))
 (let (($x5667 (forall ((n (_ BitVec 6)) )(let ((?x10647 (stack_t x_0 w_2 x_MLOAD_0 1 n)))
 (let ((?x8174 (stack_t x_0 w_2 x_MLOAD_0 2 n)))
 (let ((?x8347 (sc_t 1)))
 (let ((?x9514 (bvadd (_ bv63 6) ?x8347)))
 (let (($x11167 (bvsle ?x9514 n)))
 (or $x11167 (= ?x8174 ?x10647))))))))
 ))
 (let ((?x8347 (sc_t 1)))
 (let (($x5788 (= ?x2714 ?x8347)))
 (let ((?x8974 (used_gas_t x_0 w_2 x_MLOAD_0 2)))
 (let ((?x4565 (f_MLOAD x_0 w_2 x_MLOAD_0 (stack_t x_0 w_2 x_MLOAD_0 1 (bvadd (_ bv63 6) ?x8347)))))
 (let (($x3508 (exc_halt_t 1)))
 (let (($x2506 (= $x3508 (or $x56 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x63))) (_ bv0 1)))))))
 (let (($x6654 (forall ((w (_ BitVec 256)) )(let ((?x9776 (storage_t x_0 w_2 x_MLOAD_0 0 w)))
 (let ((?x2752 (storage_t x_0 w_2 x_MLOAD_0 1 w)))
 (= ?x2752 ?x9776))))
 ))
 (let (($x1785 (forall ((n (_ BitVec 6)) )(let ((?x10212 (stack_t x_0 w_2 x_MLOAD_0 0 n)))
 (let ((?x10647 (stack_t x_0 w_2 x_MLOAD_0 1 n)))
 (let ((?x63 (sc_t 0)))
 (let (($x4790 (bvsle ?x63 n)))
 (or $x4790 (= ?x10647 ?x10212)))))))
 ))
 (let (($x7449 (= ?x8347 (bvadd (_ bv1 6) ?x63))))
 (let (($x108 (= $x11317 (or (exc_halt_s 4) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 4))))))))
 (let (($x2587 (forall ((w (_ BitVec 256)) )(let ((?x9935 (storage_s x_0 w_2 x_MLOAD_0 4 w)))
 (let ((?x8138 (storage_s x_0 w_2 x_MLOAD_0 5 w)))
 (= ?x8138 ?x9935))))
 ))
 (let (($x10130 (forall ((n (_ BitVec 6)) )(let ((?x731 (stack_s x_0 w_2 x_MLOAD_0 4 n)))
 (let ((?x3600 (stack_s x_0 w_2 x_MLOAD_0 5 n)))
 (let ((?x4305 (sc_s 4)))
 (let ((?x11568 (bvadd (_ bv62 6) ?x4305)))
 (let (($x573 (bvsle ?x11568 n)))
 (or $x573 (= ?x3600 ?x731))))))))
 ))
 (let ((?x4305 (sc_s 4)))
 (let (($x10545 (= ?x4319 ?x4305)))
 (let (($x6104 (= (used_gas_s x_0 w_2 x_MLOAD_0 5) (+ 3 (used_gas_s x_0 w_2 x_MLOAD_0 4)))))
 (let ((?x10159 (bvadd (_ bv63 6) ?x4305)))
 (let ((?x848 (stack_s x_0 w_2 x_MLOAD_0 4 ?x10159)))
 (let ((?x11568 (bvadd (_ bv62 6) ?x4305)))
 (let ((?x11316 (stack_s x_0 w_2 x_MLOAD_0 4 ?x11568)))
 (let (($x7172 (exc_halt_s 4)))
 (let (($x7612 (= $x7172 (or (exc_halt_s 3) (not (bvsle (_ bv0 6) (bvadd (_ bv61 6) (sc_s 3))))))))
 (let (($x942 (forall ((w (_ BitVec 256)) )(let ((?x10951 (storage_s x_0 w_2 x_MLOAD_0 3 w)))
 (let ((?x9935 (storage_s x_0 w_2 x_MLOAD_0 4 w)))
 (= ?x9935 ?x10951))))
 ))
 (let (($x2778 (forall ((n (_ BitVec 6)) )(let ((?x3908 (stack_s x_0 w_2 x_MLOAD_0 3 n)))
 (let ((?x731 (stack_s x_0 w_2 x_MLOAD_0 4 n)))
 (let ((?x275 (sc_s 3)))
 (let ((?x3025 (bvadd (_ bv61 6) ?x275)))
 (let (($x7609 (bvsle ?x3025 n)))
 (or $x7609 (= ?x731 ?x3908))))))))
 ))
 (let ((?x275 (sc_s 3)))
 (let (($x3822 (= ?x4305 ?x275)))
 (let ((?x474 (used_gas_s x_0 w_2 x_MLOAD_0 4)))
 (let ((?x9670 (bvadd (_ bv63 6) ?x275)))
 (let ((?x5906 (stack_s x_0 w_2 x_MLOAD_0 3 ?x9670)))
 (let (($x292 (exc_halt_s 3)))
 (let (($x2995 (= $x292 (or (exc_halt_s 2) (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_s 2))))))))
 (let (($x9200 (forall ((w (_ BitVec 256)) )(let ((?x2126 (storage_s x_0 w_2 x_MLOAD_0 2 w)))
 (let ((?x10951 (storage_s x_0 w_2 x_MLOAD_0 3 w)))
 (= ?x10951 ?x2126))))
 ))
 (let (($x9771 (forall ((n (_ BitVec 6)) )(let ((?x9655 (stack_s x_0 w_2 x_MLOAD_0 2 n)))
 (let ((?x3908 (stack_s x_0 w_2 x_MLOAD_0 3 n)))
 (let ((?x218 (sc_s 2)))
 (let ((?x7280 (bvadd (_ bv63 6) ?x218)))
 (let (($x1255 (bvsle ?x7280 n)))
 (or $x1255 (= ?x3908 ?x9655))))))))
 ))
 (let ((?x218 (sc_s 2)))
 (let (($x2729 (= ?x275 ?x218)))
 (let ((?x10394 (used_gas_s x_0 w_2 x_MLOAD_0 3)))
 (let ((?x7280 (bvadd (_ bv63 6) ?x218)))
 (let ((?x2446 (stack_s x_0 w_2 x_MLOAD_0 2 ?x7280)))
 (let (($x9132 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 1)))) (_ bv0 1)))))
 (let (($x5176 (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_s 1))))))
 (let (($x189 (exc_halt_s 1)))
 (let (($x247 (exc_halt_s 2)))
 (let (($x2434 (forall ((w (_ BitVec 256)) )(let ((?x10113 (storage_s x_0 w_2 x_MLOAD_0 1 w)))
 (let ((?x2126 (storage_s x_0 w_2 x_MLOAD_0 2 w)))
 (= ?x2126 ?x10113))))
 ))
 (let (($x2280 (forall ((n (_ BitVec 6)) )(let ((?x9680 (stack_s x_0 w_2 x_MLOAD_0 1 n)))
 (let ((?x9655 (stack_s x_0 w_2 x_MLOAD_0 2 n)))
 (let ((?x154 (sc_s 1)))
 (let ((?x634 (bvadd (_ bv63 6) ?x154)))
 (let (($x7540 (bvsle ?x634 n)))
 (or $x7540 (= ?x9655 ?x9680))))))))
 ))
 (let (($x1347 (= ?x218 (bvadd (_ bv1 6) (sc_s 1)))))
 (let ((?x6440 (used_gas_s x_0 w_2 x_MLOAD_0 2)))
 (let ((?x154 (sc_s 1)))
 (let ((?x634 (bvadd (_ bv63 6) ?x154)))
 (let ((?x2829 (stack_s x_0 w_2 x_MLOAD_0 1 ?x634)))
 (let (($x3774 (= $x189 (or $x57 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x72))) (_ bv0 1)))))))
 (let (($x3045 (forall ((w (_ BitVec 256)) )(let ((?x4406 (storage_s x_0 w_2 x_MLOAD_0 0 w)))
 (let ((?x10113 (storage_s x_0 w_2 x_MLOAD_0 1 w)))
 (= ?x10113 ?x4406))))
 ))
 (let (($x8903 (forall ((n (_ BitVec 6)) )(let ((?x8700 (stack_s x_0 w_2 x_MLOAD_0 0 n)))
 (let ((?x9680 (stack_s x_0 w_2 x_MLOAD_0 1 n)))
 (let ((?x72 (sc_s 0)))
 (let (($x6179 (bvsle ?x72 n)))
 (or $x6179 (= ?x9680 ?x8700)))))))
 ))
 (let (($x11645 (= ?x154 (bvadd (_ bv1 6) ?x72))))
 (let (($x8509 (forall ((w0 (_ BitVec 256)) )(let ((?x10669 (ite (= (stack_s x_0 w_2 x_MLOAD_0 2 (bvadd (_ bv63 6) (sc_s 2))) w0) x_MLOAD_0 (_ bv0 256))))
 (let ((?x8205 (f_MLOAD x_0 w_2 x_MLOAD_0 w0)))
 (= ?x8205 ?x10669))))
 ))
 (let (($x698 (forall ((w (_ BitVec 256)) )(let ((?x4406 (storage_s x_0 w_2 x_MLOAD_0 0 w)))
 (= ?x4406 (_ bv0 256))))
 ))
 (let (($x9921 (= ?x7447 0)))
 (let (($x11450 (not $x57)))
 (let (($x9446 (= (stack_s x_0 w_2 x_MLOAD_0 0 (_ bv0 6)) x_0)))
 (let (($x6981 (= ?x72 (_ bv1 6))))
 (and $x6981 $x9446 $x11450 $x9921 $x698 $x8509 (= (stack_s x_0 w_2 x_MLOAD_0 1 ?x72) w_2) (= (used_gas_s x_0 w_2 x_MLOAD_0 1) (+ 3 ?x7447)) $x11645 $x8903 $x3045 $x3774 (= ?x2446 ?x2829) (= (stack_s x_0 w_2 x_MLOAD_0 2 ?x634) ?x2829) (= ?x6440 (+ 3 (used_gas_s x_0 w_2 x_MLOAD_0 1))) $x1347 $x2280 $x2434 (= $x247 (or $x189 $x5176 $x9132)) (= ?x5906 (f_MLOAD x_0 w_2 x_MLOAD_0 ?x2446)) (= ?x10394 (+ 3 ?x6440)) $x2729 $x9771 $x9200 $x2995 (= ?x848 (stack_s x_0 w_2 x_MLOAD_0 3 (bvadd (_ bv61 6) ?x275))) (= (stack_s x_0 w_2 x_MLOAD_0 4 (bvadd (_ bv61 6) ?x4305)) ?x5906) (= ?x11316 (stack_s x_0 w_2 x_MLOAD_0 3 (bvadd (_ bv62 6) ?x275))) (= ?x474 (+ 3 ?x10394)) $x3822 $x2778 $x942 $x7612 (= (stack_s x_0 w_2 x_MLOAD_0 5 (bvadd (_ bv63 6) ?x4319)) ?x11316) (= (stack_s x_0 w_2 x_MLOAD_0 5 (bvadd (_ bv62 6) ?x4319)) ?x848) $x6104 $x10545 $x10130 $x2587 $x108 (= (stack_t x_0 w_2 x_MLOAD_0 1 ?x63) w_2) (= (used_gas_t x_0 w_2 x_MLOAD_0 1) (+ 3 ?x11789)) $x7449 $x1785 $x6654 $x2506 (= ?x8693 ?x4565) (= ?x8974 (+ 3 (used_gas_t x_0 w_2 x_MLOAD_0 1))) $x5788 $x5667 $x11285 $x4332 $x6852 (= (stack_t x_0 w_2 x_MLOAD_0 3 (bvadd (_ bv62 6) ?x6438)) ?x8693) (= ?x2492 (+ 3 ?x8974)) $x9951 $x9032 $x2305 $x657 (= (stack_t x_0 w_2 x_MLOAD_0 4 ?x6438) w_2) $x449 $x6249 $x4093 $x6714 (= $x7722 (or $x6783 $x1866)) $x73 $x8729 $x58 $x10363 $x7238 (not (and $x5651 $x253 $x4034 $x10704)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
 )
(check-sat)
(get-proof)
