; PUSH cw_3 DUP1 CALLDATALOAD SWAP1 PUSH cw_5 DUP1 => PUSH cw_3 CALLDATALOAD PUSH cw_3 PUSH cw_5 PUSH cw_5
(set-option :produce-proofs true)
; 
(set-info :status unknown)
(declare-fun storage_t ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 256)) (_ BitVec 256))
(declare-fun w_5 () (_ BitVec 256))
(declare-fun w_3 () (_ BitVec 256))
(declare-fun storage_s ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 256)) (_ BitVec 256))
(declare-fun exc_halt_t (Int) Bool)
(declare-fun exc_halt_s (Int) Bool)
(declare-fun sc_t (Int) (_ BitVec 6))
(declare-fun stack_t ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 6)) (_ BitVec 256))
(declare-fun stack_s ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 6)) (_ BitVec 256))
(declare-fun sc_s (Int) (_ BitVec 6))
(declare-fun used_gas_t ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int) Int)
(declare-fun used_gas_s ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int) Int)
(declare-fun f_CALLDATALOAD ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256)) (_ BitVec 256))
(assert
 (exists ((x_CALLDATALOAD_0 (_ BitVec 256)) )(let (($x286 (forall ((w (_ BitVec 256)) )(let ((?x4785 (storage_t w_3 w_5 x_CALLDATALOAD_0 5 w)))
 (let ((?x10832 (storage_s w_3 w_5 x_CALLDATALOAD_0 6 w)))
 (= ?x10832 ?x4785))))
 ))
 (let (($x1885 (exc_halt_t 5)))
 (let (($x772 (exc_halt_s 6)))
 (let (($x11108 (= $x772 $x1885)))
 (let (($x5105 (forall ((n (_ BitVec 6)) )(let ((?x8961 (sc_t 5)))
 (let (($x9127 (bvsle ?x8961 n)))
 (let ((?x2238 (stack_t w_3 w_5 x_CALLDATALOAD_0 5 n)))
 (let ((?x8282 (stack_s w_3 w_5 x_CALLDATALOAD_0 6 n)))
 (let (($x4460 (= ?x8282 ?x2238)))
 (or $x4460 $x9127)))))))
 ))
 (let ((?x8961 (sc_t 5)))
 (let ((?x926 (sc_s 6)))
 (let (($x5889 (= ?x926 ?x8961)))
 (let ((?x1879 (used_gas_t w_3 w_5 x_CALLDATALOAD_0 0)))
 (let ((?x2753 (used_gas_s w_3 w_5 x_CALLDATALOAD_0 0)))
 (let (($x2850 (= ?x2753 ?x1879)))
 (let (($x11403 (forall ((w (_ BitVec 256)) )(let ((?x11428 (storage_t w_3 w_5 x_CALLDATALOAD_0 0 w)))
 (let ((?x4241 (storage_s w_3 w_5 x_CALLDATALOAD_0 0 w)))
 (= ?x4241 ?x11428))))
 ))
 (let (($x56 (exc_halt_t 0)))
 (let (($x57 (exc_halt_s 0)))
 (let (($x58 (= $x57 $x56)))
 (let (($x2412 (forall ((n (_ BitVec 6)) )(let ((?x1076 (stack_t w_3 w_5 x_CALLDATALOAD_0 0 n)))
 (let ((?x1333 (stack_s w_3 w_5 x_CALLDATALOAD_0 0 n)))
 (let (($x11716 (= ?x1333 ?x1076)))
 (let ((?x63 (sc_t 0)))
 (let (($x2617 (bvsle ?x63 n)))
 (or $x2617 $x11716)))))))
 ))
 (let ((?x63 (sc_t 0)))
 (let ((?x72 (sc_s 0)))
 (let (($x73 (= ?x72 ?x63)))
 (let (($x7854 (exc_halt_t 4)))
 (let (($x3654 (or $x7854 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_t 4)))) (_ bv0 1))))))
 (let (($x1601 (forall ((w (_ BitVec 256)) )(let ((?x9498 (storage_t w_3 w_5 x_CALLDATALOAD_0 4 w)))
 (let ((?x4785 (storage_t w_3 w_5 x_CALLDATALOAD_0 5 w)))
 (= ?x4785 ?x9498))))
 ))
 (let (($x11100 (forall ((n (_ BitVec 6)) )(let ((?x4818 (sc_t 4)))
 (let (($x11294 (bvsle ?x4818 n)))
 (let ((?x1174 (stack_t w_3 w_5 x_CALLDATALOAD_0 4 n)))
 (let ((?x2238 (stack_t w_3 w_5 x_CALLDATALOAD_0 5 n)))
 (or (= ?x2238 ?x1174) $x11294))))))
 ))
 (let (($x6914 (= (used_gas_t w_3 w_5 x_CALLDATALOAD_0 5) (+ 3 (used_gas_t w_3 w_5 x_CALLDATALOAD_0 4)))))
 (let (($x4755 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_t 3)))) (_ bv0 1)))))
 (let (($x6783 (exc_halt_t 3)))
 (let (($x7342 (= $x7854 (or $x6783 $x4755))))
 (let (($x11188 (forall ((w (_ BitVec 256)) )(let ((?x3880 (storage_t w_3 w_5 x_CALLDATALOAD_0 3 w)))
 (let ((?x9498 (storage_t w_3 w_5 x_CALLDATALOAD_0 4 w)))
 (= ?x9498 ?x3880))))
 ))
 (let (($x4996 (forall ((n (_ BitVec 6)) )(let ((?x11072 (stack_t w_3 w_5 x_CALLDATALOAD_0 3 n)))
 (let ((?x1174 (stack_t w_3 w_5 x_CALLDATALOAD_0 4 n)))
 (let ((?x6438 (sc_t 3)))
 (let (($x5278 (bvsle ?x6438 n)))
 (or $x5278 (= ?x1174 ?x11072)))))))
 ))
 (let ((?x4818 (sc_t 4)))
 (let (($x6479 (= ?x4818 (bvadd (_ bv1 6) (sc_t 3)))))
 (let ((?x9443 (used_gas_t w_3 w_5 x_CALLDATALOAD_0 4)))
 (let (($x367 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_t 2)))) (_ bv0 1)))))
 (let (($x2163 (exc_halt_t 2)))
 (let (($x6807 (= $x6783 (or $x2163 $x367))))
 (let (($x625 (forall ((w (_ BitVec 256)) )(let ((?x1946 (storage_t w_3 w_5 x_CALLDATALOAD_0 2 w)))
 (let ((?x3880 (storage_t w_3 w_5 x_CALLDATALOAD_0 3 w)))
 (= ?x3880 ?x1946))))
 ))
 (let (($x2296 (forall ((n (_ BitVec 6)) )(let ((?x2714 (sc_t 2)))
 (let (($x1155 (bvsle ?x2714 n)))
 (let ((?x4772 (stack_t w_3 w_5 x_CALLDATALOAD_0 2 n)))
 (let ((?x11072 (stack_t w_3 w_5 x_CALLDATALOAD_0 3 n)))
 (or (= ?x11072 ?x4772) $x1155))))))
 ))
 (let ((?x6438 (sc_t 3)))
 (let (($x1004 (= ?x6438 (bvadd (_ bv1 6) (sc_t 2)))))
 (let ((?x5368 (used_gas_t w_3 w_5 x_CALLDATALOAD_0 3)))
 (let (($x4401 (= $x2163 (or (exc_halt_t 1) (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_t 1))))))))
 (let (($x10963 (forall ((w (_ BitVec 256)) )(let ((?x3450 (storage_t w_3 w_5 x_CALLDATALOAD_0 1 w)))
 (let ((?x1946 (storage_t w_3 w_5 x_CALLDATALOAD_0 2 w)))
 (= ?x1946 ?x3450))))
 ))
 (let (($x11151 (forall ((n (_ BitVec 6)) )(let ((?x7154 (sc_t 1)))
 (let ((?x8231 (bvadd (_ bv63 6) ?x7154)))
 (let (($x8114 (bvsle ?x8231 n)))
 (let ((?x11744 (stack_t w_3 w_5 x_CALLDATALOAD_0 1 n)))
 (let ((?x4772 (stack_t w_3 w_5 x_CALLDATALOAD_0 2 n)))
 (or (= ?x4772 ?x11744) $x8114)))))))
 ))
 (let ((?x9918 (used_gas_t w_3 w_5 x_CALLDATALOAD_0 2)))
 (let ((?x6469 (f_CALLDATALOAD w_3 w_5 x_CALLDATALOAD_0 (stack_t w_3 w_5 x_CALLDATALOAD_0 1 (bvadd (_ bv63 6) (sc_t 1))))))
 (let (($x8377 (exc_halt_t 1)))
 (let (($x10110 (= $x8377 (or $x56 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x63))) (_ bv0 1)))))))
 (let (($x7774 (forall ((w (_ BitVec 256)) )(let ((?x11428 (storage_t w_3 w_5 x_CALLDATALOAD_0 0 w)))
 (let ((?x3450 (storage_t w_3 w_5 x_CALLDATALOAD_0 1 w)))
 (= ?x3450 ?x11428))))
 ))
 (let (($x5444 (forall ((n (_ BitVec 6)) )(let ((?x1076 (stack_t w_3 w_5 x_CALLDATALOAD_0 0 n)))
 (let ((?x11744 (stack_t w_3 w_5 x_CALLDATALOAD_0 1 n)))
 (let ((?x63 (sc_t 0)))
 (let (($x2617 (bvsle ?x63 n)))
 (or $x2617 (= ?x11744 ?x1076)))))))
 ))
 (let ((?x7154 (sc_t 1)))
 (let (($x1385 (= ?x7154 (bvadd (_ bv1 6) ?x63))))
 (let (($x11317 (exc_halt_s 5)))
 (let (($x1954 (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_s 5))))))
 (let (($x10709 (or $x1954 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 5)))) (_ bv0 1))) $x11317)))
 (let (($x5885 (forall ((w (_ BitVec 256)) )(let ((?x9393 (storage_s w_3 w_5 x_CALLDATALOAD_0 5 w)))
 (let ((?x10832 (storage_s w_3 w_5 x_CALLDATALOAD_0 6 w)))
 (= ?x10832 ?x9393))))
 ))
 (let (($x9543 (forall ((n (_ BitVec 6)) )(let ((?x4319 (sc_s 5)))
 (let ((?x11455 (bvadd (_ bv63 6) ?x4319)))
 (let (($x5981 (bvsle ?x11455 n)))
 (let ((?x1059 (stack_s w_3 w_5 x_CALLDATALOAD_0 5 n)))
 (let ((?x8282 (stack_s w_3 w_5 x_CALLDATALOAD_0 6 n)))
 (or (= ?x8282 ?x1059) $x5981)))))))
 ))
 (let (($x3097 (= (used_gas_s w_3 w_5 x_CALLDATALOAD_0 6) (+ 3 (used_gas_s w_3 w_5 x_CALLDATALOAD_0 5)))))
 (let ((?x4319 (sc_s 5)))
 (let ((?x11455 (bvadd (_ bv63 6) ?x4319)))
 (let ((?x402 (stack_s w_3 w_5 x_CALLDATALOAD_0 5 ?x11455)))
 (let (($x7172 (exc_halt_s 4)))
 (let (($x4746 (or $x7172 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 4)))) (_ bv0 1))))))
 (let (($x7390 (= $x11317 $x4746)))
 (let (($x4889 (forall ((w (_ BitVec 256)) )(let ((?x8182 (storage_s w_3 w_5 x_CALLDATALOAD_0 4 w)))
 (let ((?x9393 (storage_s w_3 w_5 x_CALLDATALOAD_0 5 w)))
 (= ?x9393 ?x8182))))
 ))
 (let (($x10951 (forall ((n (_ BitVec 6)) )(let ((?x8375 (stack_s w_3 w_5 x_CALLDATALOAD_0 4 n)))
 (let ((?x1059 (stack_s w_3 w_5 x_CALLDATALOAD_0 5 n)))
 (let ((?x4305 (sc_s 4)))
 (let (($x4047 (bvsle ?x4305 n)))
 (or $x4047 (= ?x1059 ?x8375)))))))
 ))
 (let (($x9414 (= ?x4319 (bvadd (_ bv1 6) (sc_s 4)))))
 (let ((?x1570 (used_gas_s w_3 w_5 x_CALLDATALOAD_0 5)))
 (let (($x1854 (= $x7172 (or (exc_halt_s 3) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 3))))))))
 (let (($x11390 (forall ((w (_ BitVec 256)) )(let ((?x2219 (storage_s w_3 w_5 x_CALLDATALOAD_0 3 w)))
 (let ((?x8182 (storage_s w_3 w_5 x_CALLDATALOAD_0 4 w)))
 (= ?x8182 ?x2219))))
 ))
 (let (($x11672 (forall ((n (_ BitVec 6)) )(let ((?x275 (sc_s 3)))
 (let ((?x10223 (bvadd (_ bv62 6) ?x275)))
 (let (($x6192 (bvsle ?x10223 n)))
 (let ((?x11225 (stack_s w_3 w_5 x_CALLDATALOAD_0 3 n)))
 (let ((?x8375 (stack_s w_3 w_5 x_CALLDATALOAD_0 4 n)))
 (or (= ?x8375 ?x11225) $x6192)))))))
 ))
 (let ((?x275 (sc_s 3)))
 (let ((?x4305 (sc_s 4)))
 (let (($x5735 (= ?x4305 ?x275)))
 (let ((?x8942 (used_gas_s w_3 w_5 x_CALLDATALOAD_0 4)))
 (let ((?x413 (bvadd (_ bv63 6) ?x275)))
 (let ((?x7311 (stack_s w_3 w_5 x_CALLDATALOAD_0 3 ?x413)))
 (let (($x957 (= (stack_s w_3 w_5 x_CALLDATALOAD_0 4 (bvadd (_ bv63 6) ?x4305)) (stack_s w_3 w_5 x_CALLDATALOAD_0 3 (bvadd (_ bv62 6) ?x275)))))
 (let (($x292 (exc_halt_s 3)))
 (let (($x3232 (= $x292 (or (exc_halt_s 2) (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_s 2))))))))
 (let (($x1849 (forall ((w (_ BitVec 256)) )(let ((?x11087 (storage_s w_3 w_5 x_CALLDATALOAD_0 2 w)))
 (let ((?x2219 (storage_s w_3 w_5 x_CALLDATALOAD_0 3 w)))
 (= ?x2219 ?x11087))))
 ))
 (let (($x254 (forall ((n (_ BitVec 6)) )(let ((?x218 (sc_s 2)))
 (let ((?x10607 (bvadd (_ bv63 6) ?x218)))
 (let (($x8922 (bvsle ?x10607 n)))
 (let ((?x8197 (stack_s w_3 w_5 x_CALLDATALOAD_0 2 n)))
 (let ((?x11225 (stack_s w_3 w_5 x_CALLDATALOAD_0 3 n)))
 (or (= ?x11225 ?x8197) $x8922)))))))
 ))
 (let ((?x4805 (used_gas_s w_3 w_5 x_CALLDATALOAD_0 3)))
 (let ((?x218 (sc_s 2)))
 (let ((?x10607 (bvadd (_ bv63 6) ?x218)))
 (let ((?x6653 (stack_s w_3 w_5 x_CALLDATALOAD_0 2 ?x10607)))
 (let (($x5753 (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_s 1))))))
 (let (($x189 (exc_halt_s 1)))
 (let (($x2585 (or $x189 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 1)))) (_ bv0 1))) $x5753)))
 (let (($x247 (exc_halt_s 2)))
 (let (($x4374 (= $x247 $x2585)))
 (let (($x9415 (forall ((w (_ BitVec 256)) )(let ((?x2045 (storage_s w_3 w_5 x_CALLDATALOAD_0 1 w)))
 (let ((?x11087 (storage_s w_3 w_5 x_CALLDATALOAD_0 2 w)))
 (= ?x11087 ?x2045))))
 ))
 (let (($x5078 (forall ((n (_ BitVec 6)) )(let ((?x154 (sc_s 1)))
 (let ((?x4271 (bvadd (_ bv63 6) ?x154)))
 (let (($x3365 (bvsle ?x4271 n)))
 (let ((?x1914 (stack_s w_3 w_5 x_CALLDATALOAD_0 1 n)))
 (let ((?x8197 (stack_s w_3 w_5 x_CALLDATALOAD_0 2 n)))
 (or (= ?x8197 ?x1914) $x3365)))))))
 ))
 (let (($x3363 (= ?x218 (bvadd (_ bv1 6) (sc_s 1)))))
 (let ((?x2486 (used_gas_s w_3 w_5 x_CALLDATALOAD_0 2)))
 (let ((?x154 (sc_s 1)))
 (let ((?x4271 (bvadd (_ bv63 6) ?x154)))
 (let ((?x8912 (stack_s w_3 w_5 x_CALLDATALOAD_0 1 ?x4271)))
 (let (($x5377 (= $x189 (or $x57 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x72))) (_ bv0 1)))))))
 (let (($x8679 (forall ((w (_ BitVec 256)) )(let ((?x4241 (storage_s w_3 w_5 x_CALLDATALOAD_0 0 w)))
 (let ((?x2045 (storage_s w_3 w_5 x_CALLDATALOAD_0 1 w)))
 (= ?x2045 ?x4241))))
 ))
 (let (($x6319 (forall ((n (_ BitVec 6)) )(let ((?x1333 (stack_s w_3 w_5 x_CALLDATALOAD_0 0 n)))
 (let ((?x1914 (stack_s w_3 w_5 x_CALLDATALOAD_0 1 n)))
 (let ((?x72 (sc_s 0)))
 (let (($x5566 (bvsle ?x72 n)))
 (or $x5566 (= ?x1914 ?x1333)))))))
 ))
 (let (($x9024 (= ?x154 (bvadd (_ bv1 6) ?x72))))
 (let (($x5285 (forall ((w0 (_ BitVec 256)) )(let ((?x1515 (ite (= (stack_s w_3 w_5 x_CALLDATALOAD_0 2 (bvadd (_ bv63 6) (sc_s 2))) w0) x_CALLDATALOAD_0 (_ bv0 256))))
 (let ((?x3808 (f_CALLDATALOAD w_3 w_5 x_CALLDATALOAD_0 w0)))
 (= ?x3808 ?x1515))))
 ))
 (let (($x6243 (forall ((w (_ BitVec 256)) )(let ((?x4241 (storage_s w_3 w_5 x_CALLDATALOAD_0 0 w)))
 (= ?x4241 (_ bv0 256))))
 ))
 (let (($x2869 (= ?x2753 0)))
 (let (($x7461 (not $x57)))
 (let (($x1074 (= ?x72 (_ bv0 6))))
 (and $x1074 $x7461 $x2869 $x6243 $x5285 (= (stack_s w_3 w_5 x_CALLDATALOAD_0 1 ?x72) w_3) (= (used_gas_s w_3 w_5 x_CALLDATALOAD_0 1) (+ 3 ?x2753)) $x9024 $x6319 $x8679 $x5377 (= ?x6653 ?x8912) (= (stack_s w_3 w_5 x_CALLDATALOAD_0 2 ?x4271) ?x8912) (= ?x2486 (+ 3 (used_gas_s w_3 w_5 x_CALLDATALOAD_0 1))) $x3363 $x5078 $x9415 $x4374 (= ?x7311 (f_CALLDATALOAD w_3 w_5 x_CALLDATALOAD_0 ?x6653)) (= ?x4805 (+ 3 ?x2486)) (= ?x275 ?x218) $x254 $x1849 $x3232 $x957 (= (stack_s w_3 w_5 x_CALLDATALOAD_0 4 (bvadd (_ bv62 6) ?x4305)) ?x7311) (= ?x8942 (+ 3 ?x4805)) $x5735 $x11672 $x11390 $x1854 (= (stack_s w_3 w_5 x_CALLDATALOAD_0 5 ?x4305) w_5) (= ?x1570 (+ 3 ?x8942)) $x9414 $x10951 $x4889 $x7390 (= (stack_s w_3 w_5 x_CALLDATALOAD_0 6 (bvadd (_ bv63 6) ?x926)) ?x402) (= (stack_s w_3 w_5 x_CALLDATALOAD_0 6 ?x11455) ?x402) $x3097 (= ?x926 (bvadd (_ bv1 6) ?x4319)) $x9543 $x5885 (= $x772 $x10709) (= (stack_t w_3 w_5 x_CALLDATALOAD_0 1 ?x63) w_3) (= (used_gas_t w_3 w_5 x_CALLDATALOAD_0 1) (+ 3 ?x1879)) $x1385 $x5444 $x7774 $x10110 (= (stack_t w_3 w_5 x_CALLDATALOAD_0 2 (bvadd (_ bv63 6) (sc_t 2))) ?x6469) (= ?x9918 (+ 3 (used_gas_t w_3 w_5 x_CALLDATALOAD_0 1))) (= (sc_t 2) ?x7154) $x11151 $x10963 $x4401 (= (stack_t w_3 w_5 x_CALLDATALOAD_0 3 (sc_t 2)) w_3) (= ?x5368 (+ 3 ?x9918)) $x1004 $x2296 $x625 $x6807 (= (stack_t w_3 w_5 x_CALLDATALOAD_0 4 ?x6438) w_5) (= ?x9443 (+ 3 ?x5368)) $x6479 $x4996 $x11188 $x7342 (= (stack_t w_3 w_5 x_CALLDATALOAD_0 5 ?x4818) w_5) $x6914 (= ?x8961 (bvadd (_ bv1 6) ?x4818)) $x11100 $x1601 (= $x1885 $x3654) $x73 $x2412 $x58 $x11403 $x2850 (not (and $x5889 $x5105 $x11108 $x286))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
 )
(check-sat)
(get-proof)