; SWAP1 DUP2 SWAP1 TIMESTAMP SWAP1 => TIMESTAMP DUP2 SWAP3
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
 (exists ((x_0 (_ BitVec 256)) (x_1 (_ BitVec 256)) (x_TIMESTAMP (_ BitVec 256)) )(let (($x8515 (forall ((w (_ BitVec 256)) )(let ((?x3210 (storage_t x_0 x_1 x_TIMESTAMP 3 w)))
 (let ((?x3136 (storage_s x_0 x_1 x_TIMESTAMP 5 w)))
 (= ?x3136 ?x3210))))
 ))
 (let (($x6783 (exc_halt_t 3)))
 (let (($x11317 (exc_halt_s 5)))
 (let (($x11613 (= $x11317 $x6783)))
 (let (($x6280 (forall ((n (_ BitVec 6)) )(let ((?x6438 (sc_t 3)))
 (let (($x10190 (bvsle ?x6438 n)))
 (let ((?x10327 (stack_t x_0 x_1 x_TIMESTAMP 3 n)))
 (let ((?x6609 (stack_s x_0 x_1 x_TIMESTAMP 5 n)))
 (let (($x354 (= ?x6609 ?x10327)))
 (or $x354 $x10190)))))))
 ))
 (let ((?x6438 (sc_t 3)))
 (let ((?x4319 (sc_s 5)))
 (let (($x7281 (= ?x4319 ?x6438)))
 (let ((?x8732 (used_gas_t x_0 x_1 x_TIMESTAMP 0)))
 (let ((?x9728 (used_gas_s x_0 x_1 x_TIMESTAMP 0)))
 (let (($x3540 (= ?x9728 ?x8732)))
 (let (($x10969 (forall ((w (_ BitVec 256)) )(let ((?x9012 (storage_t x_0 x_1 x_TIMESTAMP 0 w)))
 (let ((?x8071 (storage_s x_0 x_1 x_TIMESTAMP 0 w)))
 (= ?x8071 ?x9012))))
 ))
 (let (($x56 (exc_halt_t 0)))
 (let (($x57 (exc_halt_s 0)))
 (let (($x58 (= $x57 $x56)))
 (let (($x3344 (forall ((n (_ BitVec 6)) )(let ((?x6681 (stack_t x_0 x_1 x_TIMESTAMP 0 n)))
 (let ((?x6493 (stack_s x_0 x_1 x_TIMESTAMP 0 n)))
 (let (($x5540 (= ?x6493 ?x6681)))
 (let ((?x63 (sc_t 0)))
 (let (($x4790 (bvsle ?x63 n)))
 (or $x4790 $x5540)))))))
 ))
 (let ((?x63 (sc_t 0)))
 (let ((?x72 (sc_s 0)))
 (let (($x73 (= ?x72 ?x63)))
 (let (($x992 (= $x6783 (or (exc_halt_t 2) (not (bvsle (_ bv0 6) (bvadd (_ bv60 6) (sc_t 2))))))))
 (let (($x4168 (forall ((w (_ BitVec 256)) )(let ((?x11545 (storage_t x_0 x_1 x_TIMESTAMP 2 w)))
 (let ((?x3210 (storage_t x_0 x_1 x_TIMESTAMP 3 w)))
 (= ?x3210 ?x11545))))
 ))
 (let (($x3441 (forall ((n (_ BitVec 6)) )(let ((?x10330 (stack_t x_0 x_1 x_TIMESTAMP 2 n)))
 (let ((?x10327 (stack_t x_0 x_1 x_TIMESTAMP 3 n)))
 (let (($x3072 (= ?x10327 ?x10330)))
 (or (bvsle (bvadd (_ bv60 6) (sc_t 2)) n) $x3072)))))
 ))
 (let ((?x2714 (sc_t 2)))
 (let (($x9951 (= ?x6438 ?x2714)))
 (let ((?x686 (used_gas_t x_0 x_1 x_TIMESTAMP 3)))
 (let (($x1409 (= ?x686 (+ 3 (used_gas_t x_0 x_1 x_TIMESTAMP 2)))))
 (let ((?x3686 (bvadd (_ bv62 6) ?x2714)))
 (let ((?x10412 (stack_t x_0 x_1 x_TIMESTAMP 2 ?x3686)))
 (let ((?x7878 (bvadd (_ bv62 6) ?x6438)))
 (let ((?x11644 (stack_t x_0 x_1 x_TIMESTAMP 3 ?x7878)))
 (let (($x10236 (= ?x11644 ?x10412)))
 (let ((?x2973 (bvadd (_ bv61 6) ?x2714)))
 (let ((?x7849 (stack_t x_0 x_1 x_TIMESTAMP 2 ?x2973)))
 (let ((?x1345 (bvadd (_ bv61 6) ?x6438)))
 (let ((?x6838 (stack_t x_0 x_1 x_TIMESTAMP 3 ?x1345)))
 (let ((?x4833 (bvadd (_ bv63 6) ?x2714)))
 (let ((?x7350 (stack_t x_0 x_1 x_TIMESTAMP 2 ?x4833)))
 (let ((?x6284 (bvadd (_ bv60 6) ?x2714)))
 (let ((?x4733 (stack_t x_0 x_1 x_TIMESTAMP 2 ?x6284)))
 (let ((?x6568 (bvadd (_ bv63 6) ?x6438)))
 (let ((?x810 (stack_t x_0 x_1 x_TIMESTAMP 3 ?x6568)))
 (let (($x6781 (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_t 1))))))
 (let (($x3854 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_t 1)))) (_ bv0 1)))))
 (let (($x3508 (exc_halt_t 1)))
 (let (($x2163 (exc_halt_t 2)))
 (let (($x10695 (forall ((w (_ BitVec 256)) )(let ((?x9726 (storage_t x_0 x_1 x_TIMESTAMP 1 w)))
 (let ((?x11545 (storage_t x_0 x_1 x_TIMESTAMP 2 w)))
 (= ?x11545 ?x9726))))
 ))
 (let (($x8917 (forall ((n (_ BitVec 6)) )(let ((?x11679 (stack_t x_0 x_1 x_TIMESTAMP 1 n)))
 (let ((?x10330 (stack_t x_0 x_1 x_TIMESTAMP 2 n)))
 (let (($x8025 (= ?x10330 ?x11679)))
 (let ((?x8347 (sc_t 1)))
 (let ((?x4898 (bvadd (_ bv62 6) ?x8347)))
 (let (($x4429 (bvsle ?x4898 n)))
 (or $x4429 $x8025))))))))
 ))
 (let (($x1950 (= ?x2714 (bvadd (_ bv1 6) (sc_t 1)))))
 (let ((?x11178 (used_gas_t x_0 x_1 x_TIMESTAMP 2)))
 (let (($x5095 (= ?x11178 (+ 3 (used_gas_t x_0 x_1 x_TIMESTAMP 1)))))
 (let ((?x8347 (sc_t 1)))
 (let ((?x9514 (bvadd (_ bv63 6) ?x8347)))
 (let ((?x7485 (stack_t x_0 x_1 x_TIMESTAMP 1 ?x9514)))
 (let ((?x4898 (bvadd (_ bv62 6) ?x8347)))
 (let ((?x3319 (stack_t x_0 x_1 x_TIMESTAMP 1 ?x4898)))
 (let (($x2506 (= $x3508 (or $x56 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x63))) (_ bv0 1)))))))
 (let (($x7395 (forall ((w (_ BitVec 256)) )(let ((?x9012 (storage_t x_0 x_1 x_TIMESTAMP 0 w)))
 (let ((?x9726 (storage_t x_0 x_1 x_TIMESTAMP 1 w)))
 (= ?x9726 ?x9012))))
 ))
 (let (($x6735 (forall ((n (_ BitVec 6)) )(let ((?x6681 (stack_t x_0 x_1 x_TIMESTAMP 0 n)))
 (let ((?x11679 (stack_t x_0 x_1 x_TIMESTAMP 1 n)))
 (let (($x9827 (= ?x11679 ?x6681)))
 (let ((?x63 (sc_t 0)))
 (let (($x4790 (bvsle ?x63 n)))
 (or $x4790 $x9827)))))))
 ))
 (let (($x7449 (= ?x8347 (bvadd (_ bv1 6) ?x63))))
 (let (($x108 (= $x11317 (or (exc_halt_s 4) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 4))))))))
 (let (($x7199 (forall ((w (_ BitVec 256)) )(let ((?x11485 (storage_s x_0 x_1 x_TIMESTAMP 4 w)))
 (let ((?x3136 (storage_s x_0 x_1 x_TIMESTAMP 5 w)))
 (= ?x3136 ?x11485))))
 ))
 (let (($x11206 (forall ((n (_ BitVec 6)) )(let ((?x9904 (stack_s x_0 x_1 x_TIMESTAMP 4 n)))
 (let ((?x6609 (stack_s x_0 x_1 x_TIMESTAMP 5 n)))
 (let (($x4401 (= ?x6609 ?x9904)))
 (let ((?x4305 (sc_s 4)))
 (let ((?x11568 (bvadd (_ bv62 6) ?x4305)))
 (let (($x573 (bvsle ?x11568 n)))
 (or $x573 $x4401))))))))
 ))
 (let ((?x4305 (sc_s 4)))
 (let (($x10545 (= ?x4319 ?x4305)))
 (let (($x2082 (= (used_gas_s x_0 x_1 x_TIMESTAMP 5) (+ 3 (used_gas_s x_0 x_1 x_TIMESTAMP 4)))))
 (let ((?x10159 (bvadd (_ bv63 6) ?x4305)))
 (let ((?x6023 (stack_s x_0 x_1 x_TIMESTAMP 4 ?x10159)))
 (let ((?x3094 (bvadd (_ bv62 6) ?x4319)))
 (let ((?x4452 (stack_s x_0 x_1 x_TIMESTAMP 5 ?x3094)))
 (let ((?x11568 (bvadd (_ bv62 6) ?x4305)))
 (let ((?x6487 (stack_s x_0 x_1 x_TIMESTAMP 4 ?x11568)))
 (let ((?x3388 (bvadd (_ bv63 6) ?x4319)))
 (let ((?x7425 (stack_s x_0 x_1 x_TIMESTAMP 5 ?x3388)))
 (let (($x908 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 3)))) (_ bv0 1)))))
 (let (($x292 (exc_halt_s 3)))
 (let (($x7172 (exc_halt_s 4)))
 (let (($x1232 (= $x7172 (or $x292 $x908))))
 (let (($x6369 (forall ((w (_ BitVec 256)) )(let ((?x11791 (storage_s x_0 x_1 x_TIMESTAMP 3 w)))
 (let ((?x11485 (storage_s x_0 x_1 x_TIMESTAMP 4 w)))
 (= ?x11485 ?x11791))))
 ))
 (let (($x7659 (forall ((n (_ BitVec 6)) )(let ((?x5874 (stack_s x_0 x_1 x_TIMESTAMP 3 n)))
 (let ((?x9904 (stack_s x_0 x_1 x_TIMESTAMP 4 n)))
 (let (($x8394 (= ?x9904 ?x5874)))
 (let ((?x275 (sc_s 3)))
 (let (($x5576 (bvsle ?x275 n)))
 (or $x5576 $x8394)))))))
 ))
 (let (($x10718 (= ?x4305 (bvadd (_ bv1 6) (sc_s 3)))))
 (let ((?x11540 (used_gas_s x_0 x_1 x_TIMESTAMP 4)))
 (let (($x3705 (= $x292 (or (exc_halt_s 2) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 2))))))))
 (let (($x5138 (forall ((w (_ BitVec 256)) )(let ((?x952 (storage_s x_0 x_1 x_TIMESTAMP 2 w)))
 (let ((?x11791 (storage_s x_0 x_1 x_TIMESTAMP 3 w)))
 (= ?x11791 ?x952))))
 ))
 (let (($x9519 (forall ((n (_ BitVec 6)) )(let ((?x218 (sc_s 2)))
 (let ((?x6909 (bvadd (_ bv62 6) ?x218)))
 (let (($x11825 (bvsle ?x6909 n)))
 (let ((?x11269 (stack_s x_0 x_1 x_TIMESTAMP 2 n)))
 (let ((?x5874 (stack_s x_0 x_1 x_TIMESTAMP 3 n)))
 (let (($x8810 (= ?x5874 ?x11269)))
 (or $x8810 $x11825))))))))
 ))
 (let ((?x218 (sc_s 2)))
 (let ((?x275 (sc_s 3)))
 (let (($x2729 (= ?x275 ?x218)))
 (let ((?x10272 (used_gas_s x_0 x_1 x_TIMESTAMP 3)))
 (let (($x6866 (= ?x10272 (+ 3 (used_gas_s x_0 x_1 x_TIMESTAMP 2)))))
 (let ((?x7280 (bvadd (_ bv63 6) ?x218)))
 (let ((?x8309 (stack_s x_0 x_1 x_TIMESTAMP 2 ?x7280)))
 (let ((?x10426 (bvadd (_ bv62 6) ?x275)))
 (let ((?x4441 (stack_s x_0 x_1 x_TIMESTAMP 3 ?x10426)))
 (let ((?x6909 (bvadd (_ bv62 6) ?x218)))
 (let ((?x327 (stack_s x_0 x_1 x_TIMESTAMP 2 ?x6909)))
 (let ((?x9670 (bvadd (_ bv63 6) ?x275)))
 (let ((?x3706 (stack_s x_0 x_1 x_TIMESTAMP 3 ?x9670)))
 (let (($x11555 (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 1))))))
 (let (($x9132 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 1)))) (_ bv0 1)))))
 (let (($x189 (exc_halt_s 1)))
 (let (($x247 (exc_halt_s 2)))
 (let (($x6810 (forall ((w (_ BitVec 256)) )(let ((?x2430 (storage_s x_0 x_1 x_TIMESTAMP 1 w)))
 (let ((?x952 (storage_s x_0 x_1 x_TIMESTAMP 2 w)))
 (= ?x952 ?x2430))))
 ))
 (let (($x6617 (forall ((n (_ BitVec 6)) )(let ((?x154 (sc_s 1)))
 (let ((?x945 (bvadd (_ bv62 6) ?x154)))
 (let (($x7118 (bvsle ?x945 n)))
 (let ((?x8976 (stack_s x_0 x_1 x_TIMESTAMP 1 n)))
 (let ((?x11269 (stack_s x_0 x_1 x_TIMESTAMP 2 n)))
 (or (= ?x11269 ?x8976) $x7118)))))))
 ))
 (let (($x1347 (= ?x218 (bvadd (_ bv1 6) (sc_s 1)))))
 (let ((?x7088 (used_gas_s x_0 x_1 x_TIMESTAMP 2)))
 (let (($x199 (= ?x7088 (+ 3 (used_gas_s x_0 x_1 x_TIMESTAMP 1)))))
 (let ((?x154 (sc_s 1)))
 (let ((?x634 (bvadd (_ bv63 6) ?x154)))
 (let ((?x1862 (stack_s x_0 x_1 x_TIMESTAMP 1 ?x634)))
 (let ((?x945 (bvadd (_ bv62 6) ?x154)))
 (let ((?x9727 (stack_s x_0 x_1 x_TIMESTAMP 1 ?x945)))
 (let (($x5416 (= ?x8309 ?x9727)))
 (let (($x6669 (forall ((w (_ BitVec 256)) )(let ((?x8071 (storage_s x_0 x_1 x_TIMESTAMP 0 w)))
 (let ((?x2430 (storage_s x_0 x_1 x_TIMESTAMP 1 w)))
 (= ?x2430 ?x8071))))
 ))
 (let (($x11143 (forall ((n (_ BitVec 6)) )(let ((?x6493 (stack_s x_0 x_1 x_TIMESTAMP 0 n)))
 (let ((?x8976 (stack_s x_0 x_1 x_TIMESTAMP 1 n)))
 (let (($x4307 (= ?x8976 ?x6493)))
 (let ((?x72 (sc_s 0)))
 (let ((?x7914 (bvadd (_ bv62 6) ?x72)))
 (let (($x3855 (bvsle ?x7914 n)))
 (or $x3855 $x4307))))))))
 ))
 (let (($x4079 (= ?x154 ?x72)))
 (let ((?x1987 (used_gas_s x_0 x_1 x_TIMESTAMP 1)))
 (let (($x1912 (= ?x1987 (+ 3 ?x9728))))
 (let ((?x7914 (bvadd (_ bv62 6) ?x72)))
 (let ((?x7705 (stack_s x_0 x_1 x_TIMESTAMP 0 ?x7914)))
 (let (($x5691 (= ?x1862 ?x7705)))
 (let (($x414 (forall ((w (_ BitVec 256)) )(let ((?x8071 (storage_s x_0 x_1 x_TIMESTAMP 0 w)))
 (= ?x8071 (_ bv0 256))))
 ))
 (let (($x8935 (= ?x9728 0)))
 (let (($x11450 (not $x57)))
 (let (($x6245 (= (stack_s x_0 x_1 x_TIMESTAMP 0 (_ bv1 6)) x_1)))
 (let (($x4529 (= (stack_s x_0 x_1 x_TIMESTAMP 0 (_ bv0 6)) x_0)))
 (let (($x11128 (= ?x72 (_ bv2 6))))
 (and $x11128 $x4529 $x6245 $x11450 $x8935 $x414 $x5691 (= ?x9727 (stack_s x_0 x_1 x_TIMESTAMP 0 (bvadd (_ bv63 6) ?x72))) $x1912 $x4079 $x11143 $x6669 (= $x189 (or $x57 (not (bvsle (_ bv0 6) ?x7914)))) $x5416 (= (stack_s x_0 x_1 x_TIMESTAMP 2 ?x945) ?x9727) (= (stack_s x_0 x_1 x_TIMESTAMP 2 ?x634) ?x1862) $x199 $x1347 $x6617 $x6810 (= $x247 (or $x189 $x9132 $x11555)) (= ?x3706 ?x327) (= ?x4441 ?x8309) $x6866 $x2729 $x9519 $x5138 $x3705 (= (stack_s x_0 x_1 x_TIMESTAMP 4 ?x275) x_TIMESTAMP) (= ?x11540 (+ 2 ?x10272)) $x10718 $x7659 $x6369 $x1232 (= ?x7425 ?x6487) (= ?x4452 ?x6023) $x2082 $x10545 $x11206 $x7199 $x108 (= (stack_t x_0 x_1 x_TIMESTAMP 1 ?x63) x_TIMESTAMP) (= (used_gas_t x_0 x_1 x_TIMESTAMP 1) (+ 2 ?x8732)) $x7449 $x6735 $x7395 $x2506 (= ?x7350 ?x3319) (= (stack_t x_0 x_1 x_TIMESTAMP 2 ?x4898) ?x3319) (= (stack_t x_0 x_1 x_TIMESTAMP 2 ?x9514) ?x7485) $x5095 $x1950 $x8917 $x10695 (= $x2163 (or $x3508 $x3854 $x6781)) (= ?x810 ?x4733) (= (stack_t x_0 x_1 x_TIMESTAMP 3 (bvadd (_ bv60 6) ?x6438)) ?x7350) (= ?x6838 ?x7849) $x10236 $x1409 $x9951 $x3441 $x4168 $x992 $x73 $x3344 $x58 $x10969 $x3540 (not (and $x7281 $x6280 $x11613 $x8515))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
 )
(check-sat)
(get-proof)
