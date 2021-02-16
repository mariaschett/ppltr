; SWAP2 SWAP1 SWAP2 ADD => SWAP1 SWAP2 ADD
(set-option :produce-proofs true)
; 
(set-info :status unknown)
(declare-fun storage_t ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 256)) (_ BitVec 256))
(declare-fun storage_s ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 256)) (_ BitVec 256))
(declare-fun exc_halt_t (Int) Bool)
(declare-fun exc_halt_s (Int) Bool)
(declare-fun sc_t (Int) (_ BitVec 6))
(declare-fun stack_t ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 6)) (_ BitVec 256))
(declare-fun stack_s ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 6)) (_ BitVec 256))
(declare-fun sc_s (Int) (_ BitVec 6))
(declare-fun used_gas_t ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int) Int)
(declare-fun used_gas_s ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int) Int)
(assert
 (exists ((x_0 (_ BitVec 256)) (x_1 (_ BitVec 256)) (x_2 (_ BitVec 256)) )(let (($x3378 (forall ((w (_ BitVec 256)) )(let ((?x3210 (storage_t x_0 x_1 x_2 3 w)))
 (let ((?x11485 (storage_s x_0 x_1 x_2 4 w)))
 (= ?x11485 ?x3210))))
 ))
 (let (($x6783 (exc_halt_t 3)))
 (let (($x7172 (exc_halt_s 4)))
 (let (($x7017 (= $x7172 $x6783)))
 (let (($x6422 (forall ((n (_ BitVec 6)) )(let ((?x6438 (sc_t 3)))
 (let (($x10190 (bvsle ?x6438 n)))
 (let ((?x10327 (stack_t x_0 x_1 x_2 3 n)))
 (let ((?x9904 (stack_s x_0 x_1 x_2 4 n)))
 (let (($x5570 (= ?x9904 ?x10327)))
 (or $x5570 $x10190)))))))
 ))
 (let ((?x6438 (sc_t 3)))
 (let ((?x4305 (sc_s 4)))
 (let (($x923 (= ?x4305 ?x6438)))
 (let ((?x8732 (used_gas_t x_0 x_1 x_2 0)))
 (let ((?x9728 (used_gas_s x_0 x_1 x_2 0)))
 (let (($x3540 (= ?x9728 ?x8732)))
 (let (($x10969 (forall ((w (_ BitVec 256)) )(let ((?x9012 (storage_t x_0 x_1 x_2 0 w)))
 (let ((?x8071 (storage_s x_0 x_1 x_2 0 w)))
 (= ?x8071 ?x9012))))
 ))
 (let (($x56 (exc_halt_t 0)))
 (let (($x57 (exc_halt_s 0)))
 (let (($x58 (= $x57 $x56)))
 (let (($x3344 (forall ((n (_ BitVec 6)) )(let ((?x6681 (stack_t x_0 x_1 x_2 0 n)))
 (let ((?x6493 (stack_s x_0 x_1 x_2 0 n)))
 (let (($x5540 (= ?x6493 ?x6681)))
 (let ((?x63 (sc_t 0)))
 (let (($x4790 (bvsle ?x63 n)))
 (or $x4790 $x5540)))))))
 ))
 (let ((?x63 (sc_t 0)))
 (let ((?x72 (sc_s 0)))
 (let (($x73 (= ?x72 ?x63)))
 (let (($x657 (= $x6783 (or (exc_halt_t 2) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_t 2))))))))
 (let (($x4168 (forall ((w (_ BitVec 256)) )(let ((?x11545 (storage_t x_0 x_1 x_2 2 w)))
 (let ((?x3210 (storage_t x_0 x_1 x_2 3 w)))
 (= ?x3210 ?x11545))))
 ))
 (let (($x1600 (forall ((n (_ BitVec 6)) )(let ((?x2714 (sc_t 2)))
 (let ((?x3686 (bvadd (_ bv62 6) ?x2714)))
 (let (($x6499 (bvsle ?x3686 n)))
 (let ((?x10330 (stack_t x_0 x_1 x_2 2 n)))
 (let ((?x10327 (stack_t x_0 x_1 x_2 3 n)))
 (let (($x3072 (= ?x10327 ?x10330)))
 (or $x3072 $x6499))))))))
 ))
 (let ((?x2714 (sc_t 2)))
 (let ((?x4833 (bvadd (_ bv63 6) ?x2714)))
 (let (($x10533 (= ?x6438 ?x4833)))
 (let ((?x686 (used_gas_t x_0 x_1 x_2 3)))
 (let (($x1409 (= ?x686 (+ 3 (used_gas_t x_0 x_1 x_2 2)))))
 (let ((?x3686 (bvadd (_ bv62 6) ?x2714)))
 (let ((?x10412 (stack_t x_0 x_1 x_2 2 ?x3686)))
 (let ((?x7350 (stack_t x_0 x_1 x_2 2 ?x4833)))
 (let ((?x6568 (bvadd (_ bv63 6) ?x6438)))
 (let ((?x810 (stack_t x_0 x_1 x_2 3 ?x6568)))
 (let (($x2163 (exc_halt_t 2)))
 (let (($x5465 (= $x2163 (or (exc_halt_t 1) (not (bvsle (_ bv0 6) (bvadd (_ bv61 6) (sc_t 1))))))))
 (let (($x10695 (forall ((w (_ BitVec 256)) )(let ((?x9726 (storage_t x_0 x_1 x_2 1 w)))
 (let ((?x11545 (storage_t x_0 x_1 x_2 2 w)))
 (= ?x11545 ?x9726))))
 ))
 (let (($x10437 (forall ((n (_ BitVec 6)) )(let ((?x11679 (stack_t x_0 x_1 x_2 1 n)))
 (let ((?x10330 (stack_t x_0 x_1 x_2 2 n)))
 (let (($x8025 (= ?x10330 ?x11679)))
 (let ((?x8347 (sc_t 1)))
 (let ((?x3171 (bvadd (_ bv61 6) ?x8347)))
 (let (($x6358 (bvsle ?x3171 n)))
 (or $x6358 $x8025))))))))
 ))
 (let ((?x8347 (sc_t 1)))
 (let (($x5788 (= ?x2714 ?x8347)))
 (let ((?x11178 (used_gas_t x_0 x_1 x_2 2)))
 (let (($x5095 (= ?x11178 (+ 3 (used_gas_t x_0 x_1 x_2 1)))))
 (let ((?x4898 (bvadd (_ bv62 6) ?x8347)))
 (let ((?x3319 (stack_t x_0 x_1 x_2 1 ?x4898)))
 (let (($x2354 (= ?x10412 ?x3319)))
 (let ((?x9514 (bvadd (_ bv63 6) ?x8347)))
 (let ((?x7485 (stack_t x_0 x_1 x_2 1 ?x9514)))
 (let ((?x2973 (bvadd (_ bv61 6) ?x2714)))
 (let ((?x7849 (stack_t x_0 x_1 x_2 2 ?x2973)))
 (let (($x7395 (forall ((w (_ BitVec 256)) )(let ((?x9012 (storage_t x_0 x_1 x_2 0 w)))
 (let ((?x9726 (storage_t x_0 x_1 x_2 1 w)))
 (= ?x9726 ?x9012))))
 ))
 (let (($x859 (forall ((n (_ BitVec 6)) )(let ((?x6681 (stack_t x_0 x_1 x_2 0 n)))
 (let ((?x11679 (stack_t x_0 x_1 x_2 1 n)))
 (let ((?x63 (sc_t 0)))
 (let ((?x9097 (bvadd (_ bv62 6) ?x63)))
 (let (($x6887 (bvsle ?x9097 n)))
 (or $x6887 (= ?x11679 ?x6681))))))))
 ))
 (let (($x5230 (= ?x8347 ?x63)))
 (let ((?x4399 (used_gas_t x_0 x_1 x_2 1)))
 (let (($x10008 (= ?x4399 (+ 3 ?x8732))))
 (let ((?x9097 (bvadd (_ bv62 6) ?x63)))
 (let ((?x1517 (stack_t x_0 x_1 x_2 0 ?x9097)))
 (let (($x6993 (= ?x7485 ?x1517)))
 (let (($x3674 (= $x7172 (or (exc_halt_s 3) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 3))))))))
 (let (($x6369 (forall ((w (_ BitVec 256)) )(let ((?x11791 (storage_s x_0 x_1 x_2 3 w)))
 (let ((?x11485 (storage_s x_0 x_1 x_2 4 w)))
 (= ?x11485 ?x11791))))
 ))
 (let (($x11193 (forall ((n (_ BitVec 6)) )(let ((?x5874 (stack_s x_0 x_1 x_2 3 n)))
 (let ((?x9904 (stack_s x_0 x_1 x_2 4 n)))
 (let ((?x275 (sc_s 3)))
 (let ((?x10426 (bvadd (_ bv62 6) ?x275)))
 (let (($x5065 (bvsle ?x10426 n)))
 (or $x5065 (= ?x9904 ?x5874))))))))
 ))
 (let ((?x275 (sc_s 3)))
 (let ((?x9670 (bvadd (_ bv63 6) ?x275)))
 (let (($x5727 (= ?x4305 ?x9670)))
 (let ((?x11540 (used_gas_s x_0 x_1 x_2 4)))
 (let (($x9842 (= ?x11540 (+ 3 (used_gas_s x_0 x_1 x_2 3)))))
 (let ((?x10426 (bvadd (_ bv62 6) ?x275)))
 (let ((?x4441 (stack_s x_0 x_1 x_2 3 ?x10426)))
 (let ((?x3706 (stack_s x_0 x_1 x_2 3 ?x9670)))
 (let ((?x10159 (bvadd (_ bv63 6) ?x4305)))
 (let ((?x6023 (stack_s x_0 x_1 x_2 4 ?x10159)))
 (let (($x292 (exc_halt_s 3)))
 (let (($x5149 (= $x292 (or (exc_halt_s 2) (not (bvsle (_ bv0 6) (bvadd (_ bv61 6) (sc_s 2))))))))
 (let (($x5138 (forall ((w (_ BitVec 256)) )(let ((?x952 (storage_s x_0 x_1 x_2 2 w)))
 (let ((?x11791 (storage_s x_0 x_1 x_2 3 w)))
 (= ?x11791 ?x952))))
 ))
 (let (($x2427 (forall ((n (_ BitVec 6)) )(let ((?x11269 (stack_s x_0 x_1 x_2 2 n)))
 (let ((?x5874 (stack_s x_0 x_1 x_2 3 n)))
 (or (bvsle (bvadd (_ bv61 6) (sc_s 2)) n) (= ?x5874 ?x11269)))))
 ))
 (let ((?x218 (sc_s 2)))
 (let (($x2729 (= ?x275 ?x218)))
 (let ((?x10272 (used_gas_s x_0 x_1 x_2 3)))
 (let (($x6866 (= ?x10272 (+ 3 (used_gas_s x_0 x_1 x_2 2)))))
 (let ((?x6909 (bvadd (_ bv62 6) ?x218)))
 (let ((?x327 (stack_s x_0 x_1 x_2 2 ?x6909)))
 (let (($x2358 (= ?x4441 ?x327)))
 (let ((?x7280 (bvadd (_ bv63 6) ?x218)))
 (let ((?x8309 (stack_s x_0 x_1 x_2 2 ?x7280)))
 (let (($x6962 (= (stack_s x_0 x_1 x_2 3 (bvadd (_ bv61 6) ?x275)) ?x8309)))
 (let (($x5424 (= ?x3706 (stack_s x_0 x_1 x_2 2 (bvadd (_ bv61 6) ?x218)))))
 (let (($x247 (exc_halt_s 2)))
 (let (($x8330 (= $x247 (or (exc_halt_s 1) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 1))))))))
 (let (($x6810 (forall ((w (_ BitVec 256)) )(let ((?x2430 (storage_s x_0 x_1 x_2 1 w)))
 (let ((?x952 (storage_s x_0 x_1 x_2 2 w)))
 (= ?x952 ?x2430))))
 ))
 (let (($x6617 (forall ((n (_ BitVec 6)) )(let ((?x154 (sc_s 1)))
 (let ((?x945 (bvadd (_ bv62 6) ?x154)))
 (let (($x7118 (bvsle ?x945 n)))
 (let ((?x8976 (stack_s x_0 x_1 x_2 1 n)))
 (let ((?x11269 (stack_s x_0 x_1 x_2 2 n)))
 (or (= ?x11269 ?x8976) $x7118)))))))
 ))
 (let ((?x154 (sc_s 1)))
 (let (($x1181 (= ?x218 ?x154)))
 (let ((?x7088 (used_gas_s x_0 x_1 x_2 2)))
 (let (($x199 (= ?x7088 (+ 3 (used_gas_s x_0 x_1 x_2 1)))))
 (let (($x6669 (forall ((w (_ BitVec 256)) )(let ((?x8071 (storage_s x_0 x_1 x_2 0 w)))
 (let ((?x2430 (storage_s x_0 x_1 x_2 1 w)))
 (= ?x2430 ?x8071))))
 ))
 (let (($x2377 (forall ((n (_ BitVec 6)) )(let ((?x6493 (stack_s x_0 x_1 x_2 0 n)))
 (let ((?x8976 (stack_s x_0 x_1 x_2 1 n)))
 (let (($x4307 (= ?x8976 ?x6493)))
 (let ((?x72 (sc_s 0)))
 (let ((?x1396 (bvadd (_ bv61 6) ?x72)))
 (let (($x282 (bvsle ?x1396 n)))
 (or $x282 $x4307))))))))
 ))
 (let (($x4079 (= ?x154 ?x72)))
 (let ((?x1987 (used_gas_s x_0 x_1 x_2 1)))
 (let (($x1912 (= ?x1987 (+ 3 ?x9728))))
 (let ((?x7914 (bvadd (_ bv62 6) ?x72)))
 (let ((?x7705 (stack_s x_0 x_1 x_2 0 ?x7914)))
 (let ((?x945 (bvadd (_ bv62 6) ?x154)))
 (let ((?x9727 (stack_s x_0 x_1 x_2 1 ?x945)))
 (let ((?x10931 (bvadd (_ bv63 6) ?x72)))
 (let ((?x5160 (stack_s x_0 x_1 x_2 0 ?x10931)))
 (let ((?x634 (bvadd (_ bv63 6) ?x154)))
 (let ((?x1862 (stack_s x_0 x_1 x_2 1 ?x634)))
 (let (($x414 (forall ((w (_ BitVec 256)) )(let ((?x8071 (storage_s x_0 x_1 x_2 0 w)))
 (= ?x8071 (_ bv0 256))))
 ))
 (let (($x8935 (= ?x9728 0)))
 (let (($x11450 (not $x57)))
 (let (($x9894 (= (stack_s x_0 x_1 x_2 0 (_ bv2 6)) x_2)))
 (let (($x6245 (= (stack_s x_0 x_1 x_2 0 (_ bv1 6)) x_1)))
 (let (($x4529 (= (stack_s x_0 x_1 x_2 0 (_ bv0 6)) x_0)))
 (let (($x5315 (= ?x72 (_ bv3 6))))
 (and $x5315 $x4529 $x6245 $x9894 $x11450 $x8935 $x414 (= ?x1862 (stack_s x_0 x_1 x_2 0 (bvadd (_ bv61 6) ?x72))) (= (stack_s x_0 x_1 x_2 1 (bvadd (_ bv61 6) ?x154)) ?x5160) (= ?x9727 ?x7705) $x1912 $x4079 $x2377 $x6669 (= (exc_halt_s 1) (or $x57 (not (bvsle (_ bv0 6) (bvadd (_ bv61 6) ?x72))))) (= ?x8309 ?x9727) (= ?x327 ?x1862) $x199 $x1181 $x6617 $x6810 $x8330 $x5424 $x6962 $x2358 $x6866 $x2729 $x2427 $x5138 $x5149 (= ?x6023 (bvadd ?x3706 ?x4441)) $x9842 $x5727 $x11193 $x6369 $x3674 $x6993 (= ?x3319 (stack_t x_0 x_1 x_2 0 (bvadd (_ bv63 6) ?x63))) $x10008 $x5230 $x859 $x7395 (= (exc_halt_t 1) (or $x56 (not (bvsle (_ bv0 6) ?x9097)))) (= ?x7350 (stack_t x_0 x_1 x_2 1 (bvadd (_ bv61 6) ?x8347))) (= ?x7849 ?x7485) $x2354 $x5095 $x5788 $x10437 $x10695 $x5465 (= ?x810 (bvadd ?x7350 ?x10412)) $x1409 $x10533 $x1600 $x4168 $x657 $x73 $x3344 $x58 $x10969 $x3540 (not (and $x923 $x6422 $x7017 $x3378)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
 )
(check-sat)
(get-proof)