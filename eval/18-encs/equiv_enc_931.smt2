; DUP4 SWAP1 PUSH cw_1 SWAP1 DUP4 SWAP1 => PUSH cw_1 DUP3 DUP6 SWAP3
(set-option :produce-proofs true)
; 
(set-info :status unknown)
(declare-fun storage_t ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 256)) (_ BitVec 256))
(declare-fun w_1 () (_ BitVec 256))
(declare-fun storage_s ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 256)) (_ BitVec 256))
(declare-fun exc_halt_t (Int) Bool)
(declare-fun exc_halt_s (Int) Bool)
(declare-fun sc_t (Int) (_ BitVec 6))
(declare-fun stack_t ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 6)) (_ BitVec 256))
(declare-fun stack_s ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 6)) (_ BitVec 256))
(declare-fun sc_s (Int) (_ BitVec 6))
(declare-fun used_gas_t ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int) Int)
(declare-fun used_gas_s ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int) Int)
(assert
 (exists ((x_0 (_ BitVec 256)) (x_1 (_ BitVec 256)) (x_2 (_ BitVec 256)) (x_3 (_ BitVec 256)) )(let (($x2933 (forall ((w (_ BitVec 256)) )(let ((?x830 (storage_t x_0 x_1 x_2 x_3 w_1 4 w)))
 (let ((?x3120 (storage_s x_0 x_1 x_2 x_3 w_1 6 w)))
 (= ?x3120 ?x830))))
 ))
 (let (($x7854 (exc_halt_t 4)))
 (let (($x7121 (exc_halt_s 6)))
 (let (($x10095 (= $x7121 $x7854)))
 (let (($x9738 (forall ((n (_ BitVec 6)) )(let ((?x7495 (sc_t 4)))
 (let (($x7980 (bvsle ?x7495 n)))
 (let ((?x463 (stack_t x_0 x_1 x_2 x_3 w_1 4 n)))
 (let ((?x7808 (stack_s x_0 x_1 x_2 x_3 w_1 6 n)))
 (let (($x5054 (= ?x7808 ?x463)))
 (or $x5054 $x7980)))))))
 ))
 (let ((?x7495 (sc_t 4)))
 (let ((?x9114 (sc_s 6)))
 (let (($x2362 (= ?x9114 ?x7495)))
 (let (($x3011 (not (and $x2362 $x9738 $x10095 $x2933))))
 (let ((?x4742 (used_gas_t x_0 x_1 x_2 x_3 w_1 0)))
 (let ((?x7669 (used_gas_s x_0 x_1 x_2 x_3 w_1 0)))
 (let (($x10981 (= ?x7669 ?x4742)))
 (let (($x3036 (forall ((w (_ BitVec 256)) )(let ((?x9944 (storage_t x_0 x_1 x_2 x_3 w_1 0 w)))
 (let ((?x11678 (storage_s x_0 x_1 x_2 x_3 w_1 0 w)))
 (= ?x11678 ?x9944))))
 ))
 (let (($x56 (exc_halt_t 0)))
 (let (($x57 (exc_halt_s 0)))
 (let (($x58 (= $x57 $x56)))
 (let (($x6323 (forall ((n (_ BitVec 6)) )(let ((?x63 (sc_t 0)))
 (let (($x4809 (bvsle ?x63 n)))
 (let ((?x6054 (stack_t x_0 x_1 x_2 x_3 w_1 0 n)))
 (let ((?x2712 (stack_s x_0 x_1 x_2 x_3 w_1 0 n)))
 (let (($x464 (= ?x2712 ?x6054)))
 (or $x464 $x4809)))))))
 ))
 (let ((?x63 (sc_t 0)))
 (let ((?x72 (sc_s 0)))
 (let (($x73 (= ?x72 ?x63)))
 (let (($x5180 (= $x7854 (or (exc_halt_t 3) (not (bvsle (_ bv0 6) (bvadd (_ bv60 6) (sc_t 3))))))))
 (let (($x489 (forall ((w (_ BitVec 256)) )(let ((?x9303 (storage_t x_0 x_1 x_2 x_3 w_1 3 w)))
 (let ((?x830 (storage_t x_0 x_1 x_2 x_3 w_1 4 w)))
 (= ?x830 ?x9303))))
 ))
 (let (($x7283 (forall ((n (_ BitVec 6)) )(let ((?x10013 (sc_t 3)))
 (let ((?x10619 (bvadd (_ bv60 6) ?x10013)))
 (let (($x7920 (bvsle ?x10619 n)))
 (let ((?x5248 (stack_t x_0 x_1 x_2 x_3 w_1 3 n)))
 (let ((?x463 (stack_t x_0 x_1 x_2 x_3 w_1 4 n)))
 (let (($x4974 (= ?x463 ?x5248)))
 (or $x4974 $x7920))))))))
 ))
 (let ((?x10013 (sc_t 3)))
 (let (($x845 (= ?x7495 ?x10013)))
 (let (($x6088 (= (used_gas_t x_0 x_1 x_2 x_3 w_1 4) (+ 3 (used_gas_t x_0 x_1 x_2 x_3 w_1 3)))))
 (let ((?x11918 (bvadd (_ bv62 6) ?x10013)))
 (let ((?x5400 (stack_t x_0 x_1 x_2 x_3 w_1 3 ?x11918)))
 (let ((?x2186 (bvadd (_ bv61 6) ?x10013)))
 (let ((?x3674 (stack_t x_0 x_1 x_2 x_3 w_1 3 ?x2186)))
 (let ((?x9905 (bvadd (_ bv63 6) ?x10013)))
 (let ((?x10868 (stack_t x_0 x_1 x_2 x_3 w_1 3 ?x9905)))
 (let ((?x10619 (bvadd (_ bv60 6) ?x10013)))
 (let ((?x7358 (stack_t x_0 x_1 x_2 x_3 w_1 3 ?x10619)))
 (let (($x8194 (exc_halt_t 2)))
 (let (($x2370 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_t 2)))) (_ bv0 1)))))
 (let (($x9131 (exc_halt_t 3)))
 (let (($x7044 (forall ((w (_ BitVec 256)) )(let ((?x3672 (storage_t x_0 x_1 x_2 x_3 w_1 2 w)))
 (let ((?x9303 (storage_t x_0 x_1 x_2 x_3 w_1 3 w)))
 (= ?x9303 ?x3672))))
 ))
 (let (($x896 (forall ((n (_ BitVec 6)) )(let ((?x5407 (stack_t x_0 x_1 x_2 x_3 w_1 2 n)))
 (let ((?x5248 (stack_t x_0 x_1 x_2 x_3 w_1 3 n)))
 (let (($x5906 (= ?x5248 ?x5407)))
 (or (bvsle (bvadd (_ bv58 6) (sc_t 2)) n) $x5906)))))
 ))
 (let (($x985 (= ?x10013 (bvadd (_ bv1 6) (sc_t 2)))))
 (let ((?x9268 (used_gas_t x_0 x_1 x_2 x_3 w_1 3)))
 (let (($x5834 (= ?x9268 (+ 3 (used_gas_t x_0 x_1 x_2 x_3 w_1 2)))))
 (let ((?x6535 (sc_t 2)))
 (let ((?x7497 (bvadd (_ bv63 6) ?x6535)))
 (let ((?x4378 (stack_t x_0 x_1 x_2 x_3 w_1 2 ?x7497)))
 (let ((?x4179 (bvadd (_ bv62 6) ?x6535)))
 (let ((?x8716 (stack_t x_0 x_1 x_2 x_3 w_1 2 ?x4179)))
 (let ((?x4004 (bvadd (_ bv61 6) ?x6535)))
 (let ((?x7879 (stack_t x_0 x_1 x_2 x_3 w_1 2 ?x4004)))
 (let ((?x9689 (bvadd (_ bv60 6) ?x6535)))
 (let ((?x1877 (stack_t x_0 x_1 x_2 x_3 w_1 2 ?x9689)))
 (let ((?x10757 (bvadd (_ bv59 6) ?x6535)))
 (let ((?x4411 (stack_t x_0 x_1 x_2 x_3 w_1 2 ?x10757)))
 (let ((?x6984 (bvadd (_ bv58 6) ?x6535)))
 (let ((?x9291 (stack_t x_0 x_1 x_2 x_3 w_1 2 ?x6984)))
 (let (($x11832 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_t 1)))) (_ bv0 1)))))
 (let (($x7008 (exc_halt_t 1)))
 (let (($x4173 (not (bvsle (_ bv0 6) (bvadd (_ bv61 6) (sc_t 1))))))
 (let (($x11704 (forall ((w (_ BitVec 256)) )(let ((?x4986 (storage_t x_0 x_1 x_2 x_3 w_1 1 w)))
 (let ((?x3672 (storage_t x_0 x_1 x_2 x_3 w_1 2 w)))
 (= ?x3672 ?x4986))))
 ))
 (let (($x679 (forall ((n (_ BitVec 6)) )(let ((?x1740 (stack_t x_0 x_1 x_2 x_3 w_1 1 n)))
 (let ((?x5407 (stack_t x_0 x_1 x_2 x_3 w_1 2 n)))
 (let (($x11754 (= ?x5407 ?x1740)))
 (let ((?x6360 (sc_t 1)))
 (let ((?x2287 (bvadd (_ bv61 6) ?x6360)))
 (let (($x4154 (bvsle ?x2287 n)))
 (or $x4154 $x11754))))))))
 ))
 (let (($x7414 (= ?x6535 (bvadd (_ bv1 6) (sc_t 1)))))
 (let ((?x9599 (used_gas_t x_0 x_1 x_2 x_3 w_1 2)))
 (let (($x10444 (= ?x9599 (+ 3 (used_gas_t x_0 x_1 x_2 x_3 w_1 1)))))
 (let ((?x6360 (sc_t 1)))
 (let ((?x3316 (bvadd (_ bv63 6) ?x6360)))
 (let ((?x10067 (stack_t x_0 x_1 x_2 x_3 w_1 1 ?x3316)))
 (let (($x8692 (= (stack_t x_0 x_1 x_2 x_3 w_1 2 ?x3316) ?x10067)))
 (let ((?x8888 (bvadd (_ bv62 6) ?x6360)))
 (let ((?x8522 (stack_t x_0 x_1 x_2 x_3 w_1 1 ?x8888)))
 (let (($x9375 (= (stack_t x_0 x_1 x_2 x_3 w_1 2 ?x8888) ?x8522)))
 (let ((?x2287 (bvadd (_ bv61 6) ?x6360)))
 (let ((?x11460 (stack_t x_0 x_1 x_2 x_3 w_1 1 ?x2287)))
 (let (($x11941 (= (stack_t x_0 x_1 x_2 x_3 w_1 2 ?x2287) ?x11460)))
 (let (($x8634 (= ?x4378 ?x11460)))
 (let (($x2089 (= $x7008 (or $x56 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x63))) (_ bv0 1)))))))
 (let (($x653 (forall ((w (_ BitVec 256)) )(let ((?x9944 (storage_t x_0 x_1 x_2 x_3 w_1 0 w)))
 (let ((?x4986 (storage_t x_0 x_1 x_2 x_3 w_1 1 w)))
 (= ?x4986 ?x9944))))
 ))
 (let (($x3827 (forall ((n (_ BitVec 6)) )(let ((?x63 (sc_t 0)))
 (let (($x4809 (bvsle ?x63 n)))
 (let ((?x6054 (stack_t x_0 x_1 x_2 x_3 w_1 0 n)))
 (let ((?x1740 (stack_t x_0 x_1 x_2 x_3 w_1 1 n)))
 (let (($x3227 (= ?x1740 ?x6054)))
 (or $x3227 $x4809)))))))
 ))
 (let (($x6856 (= ?x6360 (bvadd (_ bv1 6) ?x63))))
 (let ((?x5113 (used_gas_t x_0 x_1 x_2 x_3 w_1 1)))
 (let (($x3692 (= ?x5113 (+ 3 ?x4742))))
 (let (($x8786 (= $x7121 (or (exc_halt_s 5) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 5))))))))
 (let (($x7582 (forall ((w (_ BitVec 256)) )(let ((?x3850 (storage_s x_0 x_1 x_2 x_3 w_1 5 w)))
 (let ((?x3120 (storage_s x_0 x_1 x_2 x_3 w_1 6 w)))
 (= ?x3120 ?x3850))))
 ))
 (let (($x8919 (forall ((n (_ BitVec 6)) )(let ((?x4319 (sc_s 5)))
 (let ((?x10374 (bvadd (_ bv62 6) ?x4319)))
 (let (($x8274 (bvsle ?x10374 n)))
 (let ((?x3532 (stack_s x_0 x_1 x_2 x_3 w_1 5 n)))
 (let ((?x7808 (stack_s x_0 x_1 x_2 x_3 w_1 6 n)))
 (or (= ?x7808 ?x3532) $x8274)))))))
 ))
 (let ((?x4319 (sc_s 5)))
 (let (($x9879 (= ?x9114 ?x4319)))
 (let (($x2080 (= (used_gas_s x_0 x_1 x_2 x_3 w_1 6) (+ 3 (used_gas_s x_0 x_1 x_2 x_3 w_1 5)))))
 (let ((?x262 (bvadd (_ bv63 6) ?x4319)))
 (let ((?x7752 (stack_s x_0 x_1 x_2 x_3 w_1 5 ?x262)))
 (let (($x10778 (= (stack_s x_0 x_1 x_2 x_3 w_1 6 (bvadd (_ bv62 6) ?x9114)) ?x7752)))
 (let ((?x10374 (bvadd (_ bv62 6) ?x4319)))
 (let ((?x3269 (stack_s x_0 x_1 x_2 x_3 w_1 5 ?x10374)))
 (let (($x1659 (= (stack_s x_0 x_1 x_2 x_3 w_1 6 (bvadd (_ bv63 6) ?x9114)) ?x3269)))
 (let (($x9175 (exc_halt_s 4)))
 (let (($x7724 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 4)))) (_ bv0 1)))))
 (let (($x7625 (not (bvsle (_ bv0 6) (bvadd (_ bv60 6) (sc_s 4))))))
 (let (($x1862 (exc_halt_s 5)))
 (let (($x6459 (forall ((w (_ BitVec 256)) )(let ((?x8385 (storage_s x_0 x_1 x_2 x_3 w_1 4 w)))
 (let ((?x3850 (storage_s x_0 x_1 x_2 x_3 w_1 5 w)))
 (= ?x3850 ?x8385))))
 ))
 (let (($x11351 (forall ((n (_ BitVec 6)) )(let ((?x7568 (stack_s x_0 x_1 x_2 x_3 w_1 4 n)))
 (let ((?x3532 (stack_s x_0 x_1 x_2 x_3 w_1 5 n)))
 (let (($x3182 (= ?x3532 ?x7568)))
 (let ((?x9433 (sc_s 4)))
 (let ((?x218 (bvadd (_ bv60 6) ?x9433)))
 (let (($x2238 (bvsle ?x218 n)))
 (or $x2238 $x3182))))))))
 ))
 (let (($x7352 (= ?x4319 (bvadd (_ bv1 6) (sc_s 4)))))
 (let ((?x6362 (used_gas_s x_0 x_1 x_2 x_3 w_1 5)))
 (let (($x1838 (= ?x6362 (+ 3 (used_gas_s x_0 x_1 x_2 x_3 w_1 4)))))
 (let ((?x9433 (sc_s 4)))
 (let ((?x2904 (bvadd (_ bv63 6) ?x9433)))
 (let ((?x1138 (stack_s x_0 x_1 x_2 x_3 w_1 4 ?x2904)))
 (let ((?x9158 (bvadd (_ bv62 6) ?x9433)))
 (let ((?x5312 (stack_s x_0 x_1 x_2 x_3 w_1 4 ?x9158)))
 (let ((?x1705 (bvadd (_ bv61 6) ?x9433)))
 (let ((?x3719 (stack_s x_0 x_1 x_2 x_3 w_1 4 ?x1705)))
 (let ((?x218 (bvadd (_ bv60 6) ?x9433)))
 (let ((?x1845 (stack_s x_0 x_1 x_2 x_3 w_1 4 ?x218)))
 (let (($x874 (= $x9175 (or (exc_halt_s 3) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 3))))))))
 (let (($x2766 (forall ((w (_ BitVec 256)) )(let ((?x8230 (storage_s x_0 x_1 x_2 x_3 w_1 3 w)))
 (let ((?x8385 (storage_s x_0 x_1 x_2 x_3 w_1 4 w)))
 (= ?x8385 ?x8230))))
 ))
 (let (($x4130 (forall ((n (_ BitVec 6)) )(let ((?x4446 (stack_s x_0 x_1 x_2 x_3 w_1 3 n)))
 (let ((?x7568 (stack_s x_0 x_1 x_2 x_3 w_1 4 n)))
 (let (($x5875 (= ?x7568 ?x4446)))
 (let ((?x3851 (sc_s 3)))
 (let ((?x2944 (bvadd (_ bv62 6) ?x3851)))
 (let (($x2023 (bvsle ?x2944 n)))
 (or $x2023 $x5875))))))))
 ))
 (let ((?x3851 (sc_s 3)))
 (let (($x10493 (= ?x9433 ?x3851)))
 (let ((?x2165 (used_gas_s x_0 x_1 x_2 x_3 w_1 4)))
 (let (($x7574 (= ?x2165 (+ 3 (used_gas_s x_0 x_1 x_2 x_3 w_1 3)))))
 (let ((?x8926 (bvadd (_ bv63 6) ?x3851)))
 (let ((?x11732 (stack_s x_0 x_1 x_2 x_3 w_1 3 ?x8926)))
 (let (($x10784 (= ?x5312 ?x11732)))
 (let ((?x2944 (bvadd (_ bv62 6) ?x3851)))
 (let ((?x7894 (stack_s x_0 x_1 x_2 x_3 w_1 3 ?x2944)))
 (let (($x2634 (= ?x1138 ?x7894)))
 (let (($x388 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 2)))) (_ bv0 1)))))
 (let (($x10052 (exc_halt_s 2)))
 (let (($x8103 (exc_halt_s 3)))
 (let (($x2169 (= $x8103 (or $x10052 $x388))))
 (let (($x536 (forall ((w (_ BitVec 256)) )(let ((?x11628 (storage_s x_0 x_1 x_2 x_3 w_1 2 w)))
 (let ((?x8230 (storage_s x_0 x_1 x_2 x_3 w_1 3 w)))
 (= ?x8230 ?x11628))))
 ))
 (let (($x11320 (forall ((n (_ BitVec 6)) )(let ((?x2272 (sc_s 2)))
 (let (($x8837 (bvsle ?x2272 n)))
 (let ((?x11484 (stack_s x_0 x_1 x_2 x_3 w_1 2 n)))
 (let ((?x4446 (stack_s x_0 x_1 x_2 x_3 w_1 3 n)))
 (let (($x1066 (= ?x4446 ?x11484)))
 (or $x1066 $x8837)))))))
 ))
 (let (($x1249 (= ?x3851 (bvadd (_ bv1 6) (sc_s 2)))))
 (let ((?x86 (used_gas_s x_0 x_1 x_2 x_3 w_1 3)))
 (let (($x11329 (= ?x86 (+ 3 (used_gas_s x_0 x_1 x_2 x_3 w_1 2)))))
 (let (($x11062 (= (stack_s x_0 x_1 x_2 x_3 w_1 3 (sc_s 2)) w_1)))
 (let (($x4287 (= $x10052 (or (exc_halt_s 1) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 1))))))))
 (let (($x10077 (forall ((w (_ BitVec 256)) )(let ((?x3797 (storage_s x_0 x_1 x_2 x_3 w_1 1 w)))
 (let ((?x11628 (storage_s x_0 x_1 x_2 x_3 w_1 2 w)))
 (= ?x11628 ?x3797))))
 ))
 (let (($x71 (forall ((n (_ BitVec 6)) )(let ((?x154 (sc_s 1)))
 (let ((?x10016 (bvadd (_ bv62 6) ?x154)))
 (let (($x5870 (bvsle ?x10016 n)))
 (let ((?x6653 (stack_s x_0 x_1 x_2 x_3 w_1 1 n)))
 (let ((?x11484 (stack_s x_0 x_1 x_2 x_3 w_1 2 n)))
 (let (($x3263 (= ?x11484 ?x6653)))
 (or $x3263 $x5870))))))))
 ))
 (let ((?x154 (sc_s 1)))
 (let ((?x2272 (sc_s 2)))
 (let (($x4922 (= ?x2272 ?x154)))
 (let ((?x103 (used_gas_s x_0 x_1 x_2 x_3 w_1 2)))
 (let (($x3012 (= ?x103 (+ 3 (used_gas_s x_0 x_1 x_2 x_3 w_1 1)))))
 (let ((?x8257 (bvadd (_ bv63 6) ?x154)))
 (let ((?x6813 (stack_s x_0 x_1 x_2 x_3 w_1 1 ?x8257)))
 (let ((?x6884 (bvadd (_ bv62 6) ?x2272)))
 (let ((?x1090 (stack_s x_0 x_1 x_2 x_3 w_1 2 ?x6884)))
 (let (($x3675 (= ?x1090 ?x6813)))
 (let ((?x10016 (bvadd (_ bv62 6) ?x154)))
 (let ((?x6287 (stack_s x_0 x_1 x_2 x_3 w_1 1 ?x10016)))
 (let ((?x4463 (bvadd (_ bv63 6) ?x2272)))
 (let ((?x7234 (stack_s x_0 x_1 x_2 x_3 w_1 2 ?x4463)))
 (let (($x323 (= ?x7234 ?x6287)))
 (let (($x7592 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x72))) (_ bv0 1)))))
 (let (($x8780 (exc_halt_s 1)))
 (let (($x10319 (forall ((w (_ BitVec 256)) )(let ((?x11678 (storage_s x_0 x_1 x_2 x_3 w_1 0 w)))
 (let ((?x3797 (storage_s x_0 x_1 x_2 x_3 w_1 1 w)))
 (= ?x3797 ?x11678))))
 ))
 (let (($x11713 (forall ((n (_ BitVec 6)) )(let ((?x2712 (stack_s x_0 x_1 x_2 x_3 w_1 0 n)))
 (let ((?x6653 (stack_s x_0 x_1 x_2 x_3 w_1 1 n)))
 (let (($x11959 (= ?x6653 ?x2712)))
 (or (bvsle (bvadd (_ bv60 6) (sc_s 0)) n) $x11959)))))
 ))
 (let (($x10114 (= ?x154 (bvadd (_ bv1 6) ?x72))))
 (let ((?x10732 (used_gas_s x_0 x_1 x_2 x_3 w_1 1)))
 (let (($x5031 (= ?x10732 (+ 3 ?x7669))))
 (let (($x1614 (= (stack_s x_0 x_1 x_2 x_3 w_1 1 (bvadd (_ bv63 6) ?x72)) (stack_s x_0 x_1 x_2 x_3 w_1 0 (bvadd (_ bv63 6) ?x72)))))
 (let (($x2455 (= (stack_s x_0 x_1 x_2 x_3 w_1 1 (bvadd (_ bv62 6) ?x72)) (stack_s x_0 x_1 x_2 x_3 w_1 0 (bvadd (_ bv62 6) ?x72)))))
 (let ((?x11884 (bvadd (_ bv61 6) ?x72)))
 (let ((?x6198 (stack_s x_0 x_1 x_2 x_3 w_1 0 ?x11884)))
 (let (($x405 (= (stack_s x_0 x_1 x_2 x_3 w_1 1 ?x11884) ?x6198)))
 (let ((?x11309 (bvadd (_ bv60 6) ?x72)))
 (let ((?x8191 (stack_s x_0 x_1 x_2 x_3 w_1 0 ?x11309)))
 (let (($x9384 (forall ((w (_ BitVec 256)) )(let ((?x11678 (storage_s x_0 x_1 x_2 x_3 w_1 0 w)))
 (= ?x11678 (_ bv0 256))))
 ))
 (let (($x6250 (= ?x7669 0)))
 (let (($x7519 (not $x57)))
 (let (($x2311 (= (stack_s x_0 x_1 x_2 x_3 w_1 0 (_ bv3 6)) x_3)))
 (let (($x2676 (= (stack_s x_0 x_1 x_2 x_3 w_1 0 (_ bv2 6)) x_2)))
 (let (($x10933 (= (stack_s x_0 x_1 x_2 x_3 w_1 0 (_ bv1 6)) x_1)))
 (let (($x9666 (= (stack_s x_0 x_1 x_2 x_3 w_1 0 (_ bv0 6)) x_0)))
 (let (($x10832 (= ?x72 (_ bv4 6))))
 (and $x10832 $x9666 $x10933 $x2676 $x2311 $x7519 $x6250 $x9384 (= ?x6813 ?x8191) (= (stack_s x_0 x_1 x_2 x_3 w_1 1 ?x11309) ?x8191) $x405 $x2455 $x1614 $x5031 $x10114 $x11713 $x10319 (= $x8780 (or $x57 (not (bvsle (_ bv0 6) ?x11309)) $x7592)) $x323 $x3675 $x3012 $x4922 $x71 $x10077 $x4287 $x11062 $x11329 $x1249 $x11320 $x536 $x2169 $x2634 $x10784 $x7574 $x10493 $x4130 $x2766 $x874 (= ?x7752 ?x1845) (= (stack_s x_0 x_1 x_2 x_3 w_1 5 ?x218) ?x1845) (= (stack_s x_0 x_1 x_2 x_3 w_1 5 ?x1705) ?x3719) (= (stack_s x_0 x_1 x_2 x_3 w_1 5 ?x9158) ?x5312) (= (stack_s x_0 x_1 x_2 x_3 w_1 5 ?x2904) ?x1138) $x1838 $x7352 $x11351 $x6459 (= $x1862 (or $x7625 $x7724 $x9175)) $x1659 $x10778 $x2080 $x9879 $x8919 $x7582 $x8786 (= (stack_t x_0 x_1 x_2 x_3 w_1 1 ?x63) w_1) $x3692 $x6856 $x3827 $x653 $x2089 $x8634 $x11941 $x9375 $x8692 $x10444 $x7414 $x679 $x11704 (= $x8194 (or $x4173 $x7008 $x11832)) (= ?x10868 ?x9291) (= (stack_t x_0 x_1 x_2 x_3 w_1 3 ?x6984) ?x9291) (= (stack_t x_0 x_1 x_2 x_3 w_1 3 ?x10757) ?x4411) (= (stack_t x_0 x_1 x_2 x_3 w_1 3 ?x9689) ?x1877) (= (stack_t x_0 x_1 x_2 x_3 w_1 3 ?x4004) ?x7879) (= (stack_t x_0 x_1 x_2 x_3 w_1 3 ?x4179) ?x8716) (= (stack_t x_0 x_1 x_2 x_3 w_1 3 ?x7497) ?x4378) $x5834 $x985 $x896 $x7044 (= $x9131 (or $x2370 (not (bvsle (_ bv0 6) ?x6984)) $x8194)) (= (stack_t x_0 x_1 x_2 x_3 w_1 4 (bvadd (_ bv63 6) ?x7495)) ?x7358) (= (stack_t x_0 x_1 x_2 x_3 w_1 4 (bvadd (_ bv60 6) ?x7495)) ?x10868) (= (stack_t x_0 x_1 x_2 x_3 w_1 4 (bvadd (_ bv61 6) ?x7495)) ?x3674) (= (stack_t x_0 x_1 x_2 x_3 w_1 4 (bvadd (_ bv62 6) ?x7495)) ?x5400) $x6088 $x845 $x7283 $x489 $x5180 $x73 $x6323 $x58 $x3036 $x10981 $x3011)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
 )
(check-sat)
(get-proof)
