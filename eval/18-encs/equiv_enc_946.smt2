; SWAP1 SWAP2 PUSH cw_1 SWAP2 SWAP1 => PUSH cw_1 SWAP2 SWAP3
(set-option :produce-proofs true)
; 
(set-info :status unknown)
(declare-fun storage_t ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 256)) (_ BitVec 256))
(declare-fun w_1 () (_ BitVec 256))
(declare-fun storage_s ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 256)) (_ BitVec 256))
(declare-fun exc_halt_t (Int) Bool)
(declare-fun exc_halt_s (Int) Bool)
(declare-fun stack_t ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 6)) (_ BitVec 256))
(declare-fun stack_s ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 6)) (_ BitVec 256))
(declare-fun sc_t (Int) (_ BitVec 6))
(declare-fun sc_s (Int) (_ BitVec 6))
(declare-fun used_gas_t ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int) Int)
(declare-fun used_gas_s ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int) Int)
(assert
 (exists ((x_0 (_ BitVec 256)) (x_1 (_ BitVec 256)) (x_2 (_ BitVec 256)) )(let (($x11915 (forall ((w (_ BitVec 256)) )(let ((?x582 (storage_t x_0 x_1 x_2 w_1 3 w)))
 (let ((?x10724 (storage_s x_0 x_1 x_2 w_1 5 w)))
 (= ?x10724 ?x582))))
 ))
 (let (($x9131 (exc_halt_t 3)))
 (let (($x1862 (exc_halt_s 5)))
 (let (($x6090 (= $x1862 $x9131)))
 (let (($x2541 (forall ((n (_ BitVec 6)) )(let ((?x2080 (stack_t x_0 x_1 x_2 w_1 3 n)))
 (let ((?x7352 (stack_s x_0 x_1 x_2 w_1 5 n)))
 (let (($x11014 (= ?x7352 ?x2080)))
 (let ((?x10013 (sc_t 3)))
 (let (($x9581 (bvsle ?x10013 n)))
 (or $x9581 $x11014)))))))
 ))
 (let ((?x10013 (sc_t 3)))
 (let ((?x4319 (sc_s 5)))
 (let (($x3447 (= ?x4319 ?x10013)))
 (let ((?x1974 (used_gas_t x_0 x_1 x_2 w_1 0)))
 (let ((?x6459 (used_gas_s x_0 x_1 x_2 w_1 0)))
 (let (($x8294 (= ?x6459 ?x1974)))
 (let (($x5499 (forall ((w (_ BitVec 256)) )(let ((?x685 (storage_t x_0 x_1 x_2 w_1 0 w)))
 (let ((?x6823 (storage_s x_0 x_1 x_2 w_1 0 w)))
 (= ?x6823 ?x685))))
 ))
 (let (($x56 (exc_halt_t 0)))
 (let (($x57 (exc_halt_s 0)))
 (let (($x58 (= $x57 $x56)))
 (let (($x7871 (forall ((n (_ BitVec 6)) )(let ((?x3687 (stack_t x_0 x_1 x_2 w_1 0 n)))
 (let ((?x11774 (stack_s x_0 x_1 x_2 w_1 0 n)))
 (let (($x11764 (= ?x11774 ?x3687)))
 (let ((?x63 (sc_t 0)))
 (let (($x5202 (bvsle ?x63 n)))
 (or $x5202 $x11764)))))))
 ))
 (let ((?x63 (sc_t 0)))
 (let ((?x72 (sc_s 0)))
 (let (($x73 (= ?x72 ?x63)))
 (let (($x1782 (= $x9131 (or (exc_halt_t 2) (not (bvsle (_ bv0 6) (bvadd (_ bv60 6) (sc_t 2))))))))
 (let (($x7616 (forall ((w (_ BitVec 256)) )(let ((?x4986 (storage_t x_0 x_1 x_2 w_1 2 w)))
 (let ((?x582 (storage_t x_0 x_1 x_2 w_1 3 w)))
 (= ?x582 ?x4986))))
 ))
 (let (($x11031 (forall ((n (_ BitVec 6)) )(let ((?x7008 (stack_t x_0 x_1 x_2 w_1 2 n)))
 (let ((?x2080 (stack_t x_0 x_1 x_2 w_1 3 n)))
 (let (($x8075 (= ?x2080 ?x7008)))
 (or (bvsle (bvadd (_ bv60 6) (sc_t 2)) n) $x8075)))))
 ))
 (let ((?x6198 (sc_t 2)))
 (let (($x6694 (= ?x10013 ?x6198)))
 (let ((?x1089 (used_gas_t x_0 x_1 x_2 w_1 3)))
 (let (($x7930 (= ?x1089 (+ 3 (used_gas_t x_0 x_1 x_2 w_1 2)))))
 (let ((?x9571 (bvadd (_ bv62 6) ?x6198)))
 (let ((?x684 (stack_t x_0 x_1 x_2 w_1 2 ?x9571)))
 (let ((?x1211 (bvadd (_ bv62 6) ?x10013)))
 (let ((?x4109 (stack_t x_0 x_1 x_2 w_1 3 ?x1211)))
 (let (($x9874 (= ?x4109 ?x684)))
 (let ((?x8119 (bvadd (_ bv61 6) ?x6198)))
 (let ((?x2228 (stack_t x_0 x_1 x_2 w_1 2 ?x8119)))
 (let ((?x142 (bvadd (_ bv61 6) ?x10013)))
 (let ((?x3668 (stack_t x_0 x_1 x_2 w_1 3 ?x142)))
 (let ((?x332 (bvadd (_ bv63 6) ?x6198)))
 (let ((?x1901 (stack_t x_0 x_1 x_2 w_1 2 ?x332)))
 (let ((?x10229 (bvadd (_ bv60 6) ?x10013)))
 (let ((?x5808 (stack_t x_0 x_1 x_2 w_1 3 ?x10229)))
 (let ((?x5197 (bvadd (_ bv60 6) ?x6198)))
 (let ((?x7510 (stack_t x_0 x_1 x_2 w_1 2 ?x5197)))
 (let ((?x6860 (bvadd (_ bv63 6) ?x10013)))
 (let ((?x3252 (stack_t x_0 x_1 x_2 w_1 3 ?x6860)))
 (let (($x10349 (exc_halt_t 2)))
 (let (($x2498 (= $x10349 (or (exc_halt_t 1) (not (bvsle (_ bv0 6) (bvadd (_ bv61 6) (sc_t 1))))))))
 (let (($x3944 (forall ((w (_ BitVec 256)) )(let ((?x579 (storage_t x_0 x_1 x_2 w_1 1 w)))
 (let ((?x4986 (storage_t x_0 x_1 x_2 w_1 2 w)))
 (= ?x4986 ?x579))))
 ))
 (let (($x4874 (forall ((n (_ BitVec 6)) )(let ((?x4154 (stack_t x_0 x_1 x_2 w_1 1 n)))
 (let ((?x7008 (stack_t x_0 x_1 x_2 w_1 2 n)))
 (let (($x8394 (= ?x7008 ?x4154)))
 (let ((?x6855 (sc_t 1)))
 (let ((?x3223 (bvadd (_ bv61 6) ?x6855)))
 (let (($x855 (bvsle ?x3223 n)))
 (or $x855 $x8394))))))))
 ))
 (let ((?x6855 (sc_t 1)))
 (let (($x8216 (= ?x6198 ?x6855)))
 (let ((?x6535 (used_gas_t x_0 x_1 x_2 w_1 2)))
 (let (($x1572 (= ?x6535 (+ 3 (used_gas_t x_0 x_1 x_2 w_1 1)))))
 (let ((?x9525 (bvadd (_ bv62 6) ?x6855)))
 (let ((?x6637 (stack_t x_0 x_1 x_2 w_1 1 ?x9525)))
 (let (($x6348 (= ?x684 ?x6637)))
 (let ((?x3223 (bvadd (_ bv61 6) ?x6855)))
 (let ((?x3139 (stack_t x_0 x_1 x_2 w_1 1 ?x3223)))
 (let (($x10936 (= ?x1901 ?x3139)))
 (let (($x3194 (exc_halt_t 1)))
 (let (($x6965 (= $x3194 (or $x56 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x63))) (_ bv0 1)))))))
 (let (($x5931 (forall ((w (_ BitVec 256)) )(let ((?x685 (storage_t x_0 x_1 x_2 w_1 0 w)))
 (let ((?x579 (storage_t x_0 x_1 x_2 w_1 1 w)))
 (= ?x579 ?x685))))
 ))
 (let (($x3903 (forall ((n (_ BitVec 6)) )(let ((?x3687 (stack_t x_0 x_1 x_2 w_1 0 n)))
 (let ((?x4154 (stack_t x_0 x_1 x_2 w_1 1 n)))
 (let (($x5539 (= ?x4154 ?x3687)))
 (let ((?x63 (sc_t 0)))
 (let (($x5202 (bvsle ?x63 n)))
 (or $x5202 $x5539)))))))
 ))
 (let (($x3505 (= ?x6855 (bvadd (_ bv1 6) ?x63))))
 (let ((?x8194 (used_gas_t x_0 x_1 x_2 w_1 1)))
 (let (($x1382 (= ?x8194 (+ 3 ?x1974))))
 (let (($x10695 (= $x1862 (or (exc_halt_s 4) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 4))))))))
 (let (($x6920 (forall ((w (_ BitVec 256)) )(let ((?x11439 (storage_s x_0 x_1 x_2 w_1 4 w)))
 (let ((?x10724 (storage_s x_0 x_1 x_2 w_1 5 w)))
 (= ?x10724 ?x11439))))
 ))
 (let (($x24 (forall ((n (_ BitVec 6)) )(let ((?x373 (stack_s x_0 x_1 x_2 w_1 4 n)))
 (let ((?x7352 (stack_s x_0 x_1 x_2 w_1 5 n)))
 (let (($x10591 (= ?x7352 ?x373)))
 (let ((?x9433 (sc_s 4)))
 (let ((?x3745 (bvadd (_ bv62 6) ?x9433)))
 (let (($x7729 (bvsle ?x3745 n)))
 (or $x7729 $x10591))))))))
 ))
 (let ((?x9433 (sc_s 4)))
 (let (($x5582 (= ?x4319 ?x9433)))
 (let ((?x2767 (used_gas_s x_0 x_1 x_2 w_1 5)))
 (let (($x11018 (= ?x2767 (+ 3 (used_gas_s x_0 x_1 x_2 w_1 4)))))
 (let ((?x11350 (bvadd (_ bv63 6) ?x9433)))
 (let ((?x6153 (stack_s x_0 x_1 x_2 w_1 4 ?x11350)))
 (let ((?x9678 (bvadd (_ bv62 6) ?x4319)))
 (let ((?x7239 (stack_s x_0 x_1 x_2 w_1 5 ?x9678)))
 (let (($x9106 (= ?x7239 ?x6153)))
 (let ((?x3745 (bvadd (_ bv62 6) ?x9433)))
 (let ((?x11041 (stack_s x_0 x_1 x_2 w_1 4 ?x3745)))
 (let ((?x8095 (bvadd (_ bv63 6) ?x4319)))
 (let ((?x10727 (stack_s x_0 x_1 x_2 w_1 5 ?x8095)))
 (let (($x3995 (= ?x10727 ?x11041)))
 (let (($x9175 (exc_halt_s 4)))
 (let (($x7302 (= $x9175 (or (exc_halt_s 3) (not (bvsle (_ bv0 6) (bvadd (_ bv61 6) (sc_s 3))))))))
 (let (($x3951 (forall ((w (_ BitVec 256)) )(let ((?x7358 (storage_s x_0 x_1 x_2 w_1 3 w)))
 (let ((?x11439 (storage_s x_0 x_1 x_2 w_1 4 w)))
 (= ?x11439 ?x7358))))
 ))
 (let (($x6720 (forall ((n (_ BitVec 6)) )(let ((?x2197 (stack_s x_0 x_1 x_2 w_1 3 n)))
 (let ((?x373 (stack_s x_0 x_1 x_2 w_1 4 n)))
 (let (($x4815 (= ?x373 ?x2197)))
 (or (bvsle (bvadd (_ bv61 6) (sc_s 3)) n) $x4815)))))
 ))
 (let ((?x3851 (sc_s 3)))
 (let (($x10759 (= ?x9433 ?x3851)))
 (let ((?x5515 (used_gas_s x_0 x_1 x_2 w_1 4)))
 (let (($x7487 (= ?x5515 (+ 3 (used_gas_s x_0 x_1 x_2 w_1 3)))))
 (let ((?x6454 (bvadd (_ bv63 6) ?x3851)))
 (let ((?x5577 (stack_s x_0 x_1 x_2 w_1 3 ?x6454)))
 (let ((?x8890 (bvadd (_ bv61 6) ?x9433)))
 (let ((?x8922 (stack_s x_0 x_1 x_2 w_1 4 ?x8890)))
 (let (($x11851 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 2)))) (_ bv0 1)))))
 (let (($x10052 (exc_halt_s 2)))
 (let (($x8103 (exc_halt_s 3)))
 (let (($x7448 (= $x8103 (or $x10052 $x11851))))
 (let (($x1914 (forall ((w (_ BitVec 256)) )(let ((?x4896 (storage_s x_0 x_1 x_2 w_1 2 w)))
 (let ((?x7358 (storage_s x_0 x_1 x_2 w_1 3 w)))
 (= ?x7358 ?x4896))))
 ))
 (let (($x10635 (forall ((n (_ BitVec 6)) )(let ((?x7980 (stack_s x_0 x_1 x_2 w_1 2 n)))
 (let ((?x2197 (stack_s x_0 x_1 x_2 w_1 3 n)))
 (let (($x6393 (= ?x2197 ?x7980)))
 (or (bvsle (sc_s 2) n) $x6393)))))
 ))
 (let (($x4308 (= ?x3851 (bvadd (_ bv1 6) (sc_s 2)))))
 (let ((?x11918 (used_gas_s x_0 x_1 x_2 w_1 3)))
 (let (($x11347 (= ?x11918 (+ 3 (used_gas_s x_0 x_1 x_2 w_1 2)))))
 (let (($x7357 (= (stack_s x_0 x_1 x_2 w_1 3 (sc_s 2)) w_1)))
 (let (($x4245 (= $x10052 (or (exc_halt_s 1) (not (bvsle (_ bv0 6) (bvadd (_ bv61 6) (sc_s 1))))))))
 (let (($x5688 (forall ((w (_ BitVec 256)) )(let ((?x1002 (storage_s x_0 x_1 x_2 w_1 1 w)))
 (let ((?x4896 (storage_s x_0 x_1 x_2 w_1 2 w)))
 (= ?x4896 ?x1002))))
 ))
 (let (($x4337 (forall ((n (_ BitVec 6)) )(let ((?x6937 (stack_s x_0 x_1 x_2 w_1 1 n)))
 (let ((?x7980 (stack_s x_0 x_1 x_2 w_1 2 n)))
 (let (($x10513 (= ?x7980 ?x6937)))
 (or (bvsle (bvadd (_ bv61 6) (sc_s 1)) n) $x10513)))))
 ))
 (let ((?x154 (sc_s 1)))
 (let ((?x2272 (sc_s 2)))
 (let (($x151 (= ?x2272 ?x154)))
 (let ((?x9360 (used_gas_s x_0 x_1 x_2 w_1 2)))
 (let (($x5977 (= ?x9360 (+ 3 (used_gas_s x_0 x_1 x_2 w_1 1)))))
 (let ((?x10814 (bvadd (_ bv62 6) ?x154)))
 (let ((?x10050 (stack_s x_0 x_1 x_2 w_1 1 ?x10814)))
 (let ((?x10094 (bvadd (_ bv62 6) ?x2272)))
 (let ((?x3023 (stack_s x_0 x_1 x_2 w_1 2 ?x10094)))
 (let ((?x7515 (bvadd (_ bv63 6) ?x154)))
 (let ((?x3839 (stack_s x_0 x_1 x_2 w_1 1 ?x7515)))
 (let ((?x10655 (bvadd (_ bv61 6) ?x2272)))
 (let ((?x2819 (stack_s x_0 x_1 x_2 w_1 2 ?x10655)))
 (let ((?x5658 (bvadd (_ bv61 6) ?x154)))
 (let ((?x8340 (stack_s x_0 x_1 x_2 w_1 1 ?x5658)))
 (let ((?x11939 (bvadd (_ bv63 6) ?x2272)))
 (let ((?x10172 (stack_s x_0 x_1 x_2 w_1 2 ?x11939)))
 (let (($x8780 (exc_halt_s 1)))
 (let (($x4613 (= $x8780 (or $x57 (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) ?x72)))))))
 (let (($x3657 (forall ((w (_ BitVec 256)) )(let ((?x6823 (storage_s x_0 x_1 x_2 w_1 0 w)))
 (let ((?x1002 (storage_s x_0 x_1 x_2 w_1 1 w)))
 (= ?x1002 ?x6823))))
 ))
 (let (($x2273 (forall ((n (_ BitVec 6)) )(let ((?x72 (sc_s 0)))
 (let ((?x7101 (bvadd (_ bv62 6) ?x72)))
 (let (($x9570 (bvsle ?x7101 n)))
 (let ((?x11774 (stack_s x_0 x_1 x_2 w_1 0 n)))
 (let ((?x6937 (stack_s x_0 x_1 x_2 w_1 1 n)))
 (let (($x3213 (= ?x6937 ?x11774)))
 (or $x3213 $x9570))))))))
 ))
 (let (($x6924 (= ?x154 ?x72)))
 (let ((?x1668 (used_gas_s x_0 x_1 x_2 w_1 1)))
 (let (($x1050 (= ?x1668 (+ 3 ?x6459))))
 (let ((?x9361 (bvadd (_ bv63 6) ?x72)))
 (let ((?x5225 (stack_s x_0 x_1 x_2 w_1 0 ?x9361)))
 (let (($x4604 (= ?x10050 ?x5225)))
 (let ((?x7101 (bvadd (_ bv62 6) ?x72)))
 (let ((?x10478 (stack_s x_0 x_1 x_2 w_1 0 ?x7101)))
 (let (($x6303 (= ?x3839 ?x10478)))
 (let (($x6797 (forall ((w (_ BitVec 256)) )(let ((?x6823 (storage_s x_0 x_1 x_2 w_1 0 w)))
 (= ?x6823 (_ bv0 256))))
 ))
 (let (($x8927 (= ?x6459 0)))
 (let (($x3623 (not $x57)))
 (let (($x5433 (= (stack_s x_0 x_1 x_2 w_1 0 (_ bv2 6)) x_2)))
 (let (($x7395 (= (stack_s x_0 x_1 x_2 w_1 0 (_ bv1 6)) x_1)))
 (let (($x11455 (= (stack_s x_0 x_1 x_2 w_1 0 (_ bv0 6)) x_0)))
 (let (($x9108 (= ?x72 (_ bv3 6))))
 (and $x9108 $x11455 $x7395 $x5433 $x3623 $x8927 $x6797 $x6303 $x4604 $x1050 $x6924 $x2273 $x3657 $x4613 (= ?x10172 ?x8340) (= ?x2819 ?x3839) (= ?x3023 ?x10050) $x5977 $x151 $x4337 $x5688 $x4245 $x7357 $x11347 $x4308 $x10635 $x1914 $x7448 (= ?x6153 (stack_s x_0 x_1 x_2 w_1 3 (bvadd (_ bv61 6) ?x3851))) (= ?x8922 ?x5577) (= ?x11041 (stack_s x_0 x_1 x_2 w_1 3 (bvadd (_ bv62 6) ?x3851))) $x7487 $x10759 $x6720 $x3951 $x7302 $x3995 $x9106 $x11018 $x5582 $x24 $x6920 $x10695 (= (stack_t x_0 x_1 x_2 w_1 1 ?x63) w_1) $x1382 $x3505 $x3903 $x5931 $x6965 $x10936 (= ?x2228 (stack_t x_0 x_1 x_2 w_1 1 (bvadd (_ bv63 6) ?x6855))) $x6348 $x1572 $x8216 $x4874 $x3944 $x2498 (= ?x3252 ?x7510) (= ?x5808 ?x1901) (= ?x3668 ?x2228) $x9874 $x7930 $x6694 $x11031 $x7616 $x1782 $x73 $x7871 $x58 $x5499 $x8294 (not (and $x3447 $x2541 $x6090 $x11915)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
 )
(check-sat)
(get-proof)