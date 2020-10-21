; DUP1 POP SWAP3 SWAP2 POP POP => SWAP2 SUB POP SWAP1
(set-option :produce-proofs true)
; 
(set-info :status unknown)
(declare-fun storage_t ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 256)) (_ BitVec 256))
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
 (exists ((x_0 (_ BitVec 256)) (x_1 (_ BitVec 256)) (x_2 (_ BitVec 256)) (x_3 (_ BitVec 256)) )(let (($x8398 (forall ((w (_ BitVec 256)) )(let ((?x2499 (storage_t x_0 x_1 x_2 x_3 4 w)))
 (let ((?x9878 (storage_s x_0 x_1 x_2 x_3 6 w)))
 (= ?x9878 ?x2499))))
 ))
 (let (($x3723 (exc_halt_t 4)))
 (let (($x772 (exc_halt_s 6)))
 (let (($x2931 (= $x772 $x3723)))
 (let (($x10429 (forall ((n (_ BitVec 6)) )(let ((?x8165 (stack_t x_0 x_1 x_2 x_3 4 n)))
 (let ((?x7288 (stack_s x_0 x_1 x_2 x_3 6 n)))
 (let (($x11250 (= ?x7288 ?x8165)))
 (let ((?x3757 (sc_t 4)))
 (let (($x5280 (bvsle ?x3757 n)))
 (or $x5280 $x11250)))))))
 ))
 (let ((?x3757 (sc_t 4)))
 (let ((?x926 (sc_s 6)))
 (let (($x6504 (= ?x926 ?x3757)))
 (let (($x7795 (not (and $x6504 $x10429 $x2931 $x8398))))
 (let ((?x8018 (used_gas_t x_0 x_1 x_2 x_3 0)))
 (let ((?x7983 (used_gas_s x_0 x_1 x_2 x_3 0)))
 (let (($x2987 (= ?x7983 ?x8018)))
 (let (($x541 (forall ((w (_ BitVec 256)) )(let ((?x8824 (storage_t x_0 x_1 x_2 x_3 0 w)))
 (let ((?x4950 (storage_s x_0 x_1 x_2 x_3 0 w)))
 (= ?x4950 ?x8824))))
 ))
 (let (($x56 (exc_halt_t 0)))
 (let (($x57 (exc_halt_s 0)))
 (let (($x58 (= $x57 $x56)))
 (let (($x6925 (forall ((n (_ BitVec 6)) )(let ((?x63 (sc_t 0)))
 (let (($x11524 (bvsle ?x63 n)))
 (let ((?x5487 (stack_t x_0 x_1 x_2 x_3 0 n)))
 (let ((?x8010 (stack_s x_0 x_1 x_2 x_3 0 n)))
 (let (($x5906 (= ?x8010 ?x5487)))
 (or $x5906 $x11524)))))))
 ))
 (let ((?x63 (sc_t 0)))
 (let ((?x72 (sc_s 0)))
 (let (($x73 (= ?x72 ?x63)))
 (let (($x9828 (= $x3723 (or (exc_halt_t 3) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_t 3))))))))
 (let (($x4777 (forall ((w (_ BitVec 256)) )(let ((?x3005 (storage_t x_0 x_1 x_2 x_3 3 w)))
 (let ((?x2499 (storage_t x_0 x_1 x_2 x_3 4 w)))
 (= ?x2499 ?x3005))))
 ))
 (let (($x3469 (forall ((n (_ BitVec 6)) )(let ((?x8181 (stack_t x_0 x_1 x_2 x_3 3 n)))
 (let ((?x8165 (stack_t x_0 x_1 x_2 x_3 4 n)))
 (let (($x3213 (= ?x8165 ?x8181)))
 (or $x3213 (bvsle (bvadd (_ bv62 6) (sc_t 3)) n))))))
 ))
 (let ((?x11842 (sc_t 3)))
 (let (($x9721 (= ?x3757 ?x11842)))
 (let ((?x8325 (used_gas_t x_0 x_1 x_2 x_3 4)))
 (let (($x2958 (= ?x8325 (+ 3 (used_gas_t x_0 x_1 x_2 x_3 3)))))
 (let ((?x11478 (bvadd (_ bv63 6) ?x11842)))
 (let ((?x9153 (stack_t x_0 x_1 x_2 x_3 3 ?x11478)))
 (let ((?x11265 (bvadd (_ bv62 6) ?x11842)))
 (let ((?x6357 (stack_t x_0 x_1 x_2 x_3 3 ?x11265)))
 (let ((?x4511 (bvadd (_ bv63 6) ?x3757)))
 (let ((?x5407 (stack_t x_0 x_1 x_2 x_3 4 ?x4511)))
 (let (($x4755 (exc_halt_t 3)))
 (let (($x509 (= $x4755 (or (exc_halt_t 2) (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_t 2))))))))
 (let (($x5299 (forall ((w (_ BitVec 256)) )(let ((?x3848 (storage_t x_0 x_1 x_2 x_3 2 w)))
 (let ((?x3005 (storage_t x_0 x_1 x_2 x_3 3 w)))
 (= ?x3005 ?x3848))))
 ))
 (let (($x7098 (forall ((n (_ BitVec 6)) )(let ((?x4056 (sc_t 2)))
 (let ((?x180 (bvadd (_ bv63 6) ?x4056)))
 (let (($x528 (bvsle ?x180 n)))
 (let ((?x4620 (stack_t x_0 x_1 x_2 x_3 2 n)))
 (let ((?x8181 (stack_t x_0 x_1 x_2 x_3 3 n)))
 (let (($x164 (= ?x8181 ?x4620)))
 (or $x164 $x528))))))))
 ))
 (let ((?x7568 (used_gas_t x_0 x_1 x_2 x_3 3)))
 (let (($x903 (exc_halt_t 2)))
 (let (($x6259 (= $x903 (or (exc_halt_t 1) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_t 1))))))))
 (let (($x6953 (forall ((w (_ BitVec 256)) )(let ((?x7984 (storage_t x_0 x_1 x_2 x_3 1 w)))
 (let ((?x3848 (storage_t x_0 x_1 x_2 x_3 2 w)))
 (= ?x3848 ?x7984))))
 ))
 (let (($x6757 (forall ((n (_ BitVec 6)) )(let ((?x10097 (stack_t x_0 x_1 x_2 x_3 1 n)))
 (let ((?x4620 (stack_t x_0 x_1 x_2 x_3 2 n)))
 (let (($x9382 (= ?x4620 ?x10097)))
 (let ((?x4023 (sc_t 1)))
 (let ((?x6304 (bvadd (_ bv62 6) ?x4023)))
 (let (($x573 (bvsle ?x6304 n)))
 (or $x573 $x9382))))))))
 ))
 (let ((?x9329 (used_gas_t x_0 x_1 x_2 x_3 2)))
 (let (($x7888 (= ?x9329 (+ 3 (used_gas_t x_0 x_1 x_2 x_3 1)))))
 (let ((?x4023 (sc_t 1)))
 (let ((?x6304 (bvadd (_ bv62 6) ?x4023)))
 (let ((?x6850 (stack_t x_0 x_1 x_2 x_3 1 ?x6304)))
 (let ((?x4690 (bvadd (_ bv63 6) ?x4023)))
 (let ((?x8164 (stack_t x_0 x_1 x_2 x_3 1 ?x4690)))
 (let ((?x4056 (sc_t 2)))
 (let ((?x180 (bvadd (_ bv63 6) ?x4056)))
 (let ((?x8277 (stack_t x_0 x_1 x_2 x_3 2 ?x180)))
 (let (($x3064 (forall ((w (_ BitVec 256)) )(let ((?x8824 (storage_t x_0 x_1 x_2 x_3 0 w)))
 (let ((?x7984 (storage_t x_0 x_1 x_2 x_3 1 w)))
 (= ?x7984 ?x8824))))
 ))
 (let (($x9709 (forall ((n (_ BitVec 6)) )(let ((?x5487 (stack_t x_0 x_1 x_2 x_3 0 n)))
 (let ((?x10097 (stack_t x_0 x_1 x_2 x_3 1 n)))
 (let (($x4521 (= ?x10097 ?x5487)))
 (or $x4521 (bvsle (bvadd (_ bv61 6) (sc_t 0)) n))))))
 ))
 (let (($x9875 (= ?x4023 ?x63)))
 (let ((?x2902 (used_gas_t x_0 x_1 x_2 x_3 1)))
 (let (($x4745 (= ?x2902 (+ 3 ?x8018))))
 (let ((?x8045 (bvadd (_ bv63 6) ?x63)))
 (let ((?x7836 (stack_t x_0 x_1 x_2 x_3 0 ?x8045)))
 (let ((?x8725 (bvadd (_ bv61 6) ?x63)))
 (let ((?x2918 (stack_t x_0 x_1 x_2 x_3 0 ?x8725)))
 (let (($x8358 (= ?x8164 ?x2918)))
 (let (($x5057 (= $x772 (or (exc_halt_s 5) (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_s 5))))))))
 (let (($x10595 (forall ((w (_ BitVec 256)) )(let ((?x3927 (storage_s x_0 x_1 x_2 x_3 5 w)))
 (let ((?x9878 (storage_s x_0 x_1 x_2 x_3 6 w)))
 (= ?x9878 ?x3927))))
 ))
 (let (($x4706 (forall ((n (_ BitVec 6)) )(let ((?x1144 (stack_s x_0 x_1 x_2 x_3 5 n)))
 (let ((?x7288 (stack_s x_0 x_1 x_2 x_3 6 n)))
 (let (($x6392 (= ?x7288 ?x1144)))
 (or (bvsle (bvadd (_ bv63 6) (sc_s 5)) n) $x6392)))))
 ))
 (let ((?x8964 (used_gas_s x_0 x_1 x_2 x_3 6)))
 (let (($x3979 (exc_halt_s 5)))
 (let (($x5870 (= $x3979 (or (exc_halt_s 4) (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_s 4))))))))
 (let (($x3553 (forall ((w (_ BitVec 256)) )(let ((?x9678 (storage_s x_0 x_1 x_2 x_3 4 w)))
 (let ((?x3927 (storage_s x_0 x_1 x_2 x_3 5 w)))
 (= ?x3927 ?x9678))))
 ))
 (let (($x8687 (forall ((n (_ BitVec 6)) )(let ((?x7612 (stack_s x_0 x_1 x_2 x_3 4 n)))
 (let ((?x1144 (stack_s x_0 x_1 x_2 x_3 5 n)))
 (let (($x9079 (= ?x1144 ?x7612)))
 (or (bvsle (bvadd (_ bv63 6) (sc_s 4)) n) $x9079)))))
 ))
 (let ((?x4305 (sc_s 4)))
 (let ((?x129 (bvadd (_ bv63 6) ?x4305)))
 (let ((?x805 (sc_s 5)))
 (let (($x8607 (= ?x805 ?x129)))
 (let ((?x1720 (used_gas_s x_0 x_1 x_2 x_3 5)))
 (let (($x10301 (= ?x1720 (+ 2 (used_gas_s x_0 x_1 x_2 x_3 4)))))
 (let (($x64 (exc_halt_s 4)))
 (let (($x9881 (= $x64 (or (exc_halt_s 3) (not (bvsle (_ bv0 6) (bvadd (_ bv61 6) (sc_s 3))))))))
 (let (($x5859 (forall ((w (_ BitVec 256)) )(let ((?x8688 (storage_s x_0 x_1 x_2 x_3 3 w)))
 (let ((?x9678 (storage_s x_0 x_1 x_2 x_3 4 w)))
 (= ?x9678 ?x8688))))
 ))
 (let (($x8269 (forall ((n (_ BitVec 6)) )(let ((?x8758 (stack_s x_0 x_1 x_2 x_3 3 n)))
 (let ((?x7612 (stack_s x_0 x_1 x_2 x_3 4 n)))
 (let (($x2318 (= ?x7612 ?x8758)))
 (or $x2318 (bvsle (bvadd (_ bv61 6) (sc_s 3)) n))))))
 ))
 (let ((?x275 (sc_s 3)))
 (let (($x3747 (= ?x4305 ?x275)))
 (let ((?x6197 (used_gas_s x_0 x_1 x_2 x_3 4)))
 (let (($x6108 (= ?x6197 (+ 3 (used_gas_s x_0 x_1 x_2 x_3 3)))))
 (let ((?x11507 (bvadd (_ bv62 6) ?x275)))
 (let ((?x8067 (stack_s x_0 x_1 x_2 x_3 3 ?x11507)))
 (let ((?x3190 (bvadd (_ bv62 6) ?x4305)))
 (let ((?x1439 (stack_s x_0 x_1 x_2 x_3 4 ?x3190)))
 (let ((?x3429 (bvadd (_ bv63 6) ?x275)))
 (let ((?x4281 (stack_s x_0 x_1 x_2 x_3 3 ?x3429)))
 (let ((?x5862 (bvadd (_ bv61 6) ?x4305)))
 (let ((?x3900 (stack_s x_0 x_1 x_2 x_3 4 ?x5862)))
 (let ((?x11531 (bvadd (_ bv61 6) ?x275)))
 (let ((?x9936 (stack_s x_0 x_1 x_2 x_3 3 ?x11531)))
 (let ((?x3821 (stack_s x_0 x_1 x_2 x_3 4 ?x129)))
 (let (($x292 (exc_halt_s 3)))
 (let (($x6329 (= $x292 (or (exc_halt_s 2) (not (bvsle (_ bv0 6) (bvadd (_ bv60 6) (sc_s 2))))))))
 (let (($x3942 (forall ((w (_ BitVec 256)) )(let ((?x5827 (storage_s x_0 x_1 x_2 x_3 2 w)))
 (let ((?x8688 (storage_s x_0 x_1 x_2 x_3 3 w)))
 (= ?x8688 ?x5827))))
 ))
 (let (($x6893 (forall ((n (_ BitVec 6)) )(let ((?x3867 (stack_s x_0 x_1 x_2 x_3 2 n)))
 (let ((?x8758 (stack_s x_0 x_1 x_2 x_3 3 n)))
 (let (($x8552 (= ?x8758 ?x3867)))
 (or (bvsle (bvadd (_ bv60 6) (sc_s 2)) n) $x8552)))))
 ))
 (let ((?x218 (sc_s 2)))
 (let (($x4482 (= ?x275 ?x218)))
 (let ((?x6636 (used_gas_s x_0 x_1 x_2 x_3 3)))
 (let (($x2766 (= ?x6636 (+ 3 (used_gas_s x_0 x_1 x_2 x_3 2)))))
 (let ((?x500 (bvadd (_ bv63 6) ?x218)))
 (let ((?x7214 (stack_s x_0 x_1 x_2 x_3 2 ?x500)))
 (let ((?x7564 (bvadd (_ bv60 6) ?x218)))
 (let ((?x7969 (stack_s x_0 x_1 x_2 x_3 2 ?x7564)))
 (let (($x2506 (= ?x4281 ?x7969)))
 (let (($x247 (exc_halt_s 2)))
 (let (($x3121 (= $x247 (or (exc_halt_s 1) (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_s 1))))))))
 (let (($x1881 (forall ((w (_ BitVec 256)) )(let ((?x7465 (storage_s x_0 x_1 x_2 x_3 1 w)))
 (let ((?x5827 (storage_s x_0 x_1 x_2 x_3 2 w)))
 (= ?x5827 ?x7465))))
 ))
 (let (($x5841 (forall ((n (_ BitVec 6)) )(let ((?x2184 (stack_s x_0 x_1 x_2 x_3 1 n)))
 (let ((?x3867 (stack_s x_0 x_1 x_2 x_3 2 n)))
 (let (($x8746 (= ?x3867 ?x2184)))
 (let ((?x154 (sc_s 1)))
 (let ((?x11806 (bvadd (_ bv63 6) ?x154)))
 (let (($x2160 (bvsle ?x11806 n)))
 (or $x2160 $x8746))))))))
 ))
 (let ((?x154 (sc_s 1)))
 (let ((?x11806 (bvadd (_ bv63 6) ?x154)))
 (let (($x10109 (= ?x218 ?x11806)))
 (let ((?x7909 (used_gas_s x_0 x_1 x_2 x_3 2)))
 (let (($x11798 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x72))) (_ bv0 1)))))
 (let (($x2995 (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) ?x72)))))
 (let (($x189 (exc_halt_s 1)))
 (let (($x4357 (= $x189 (or $x57 $x2995 $x11798))))
 (let (($x6880 (forall ((w (_ BitVec 256)) )(let ((?x4950 (storage_s x_0 x_1 x_2 x_3 0 w)))
 (let ((?x7465 (storage_s x_0 x_1 x_2 x_3 1 w)))
 (= ?x7465 ?x4950))))
 ))
 (let (($x4263 (forall ((n (_ BitVec 6)) )(let ((?x72 (sc_s 0)))
 (let ((?x2312 (bvadd (_ bv63 6) ?x72)))
 (let (($x8297 (bvsle ?x2312 n)))
 (let ((?x8010 (stack_s x_0 x_1 x_2 x_3 0 n)))
 (let ((?x2184 (stack_s x_0 x_1 x_2 x_3 1 n)))
 (let (($x2967 (= ?x2184 ?x8010)))
 (or $x2967 $x8297))))))))
 ))
 (let (($x11593 (= ?x154 (bvadd (_ bv1 6) ?x72))))
 (let ((?x2965 (used_gas_s x_0 x_1 x_2 x_3 1)))
 (let (($x8105 (= ?x2965 (+ 3 ?x7983))))
 (let ((?x2312 (bvadd (_ bv63 6) ?x72)))
 (let ((?x1590 (stack_s x_0 x_1 x_2 x_3 0 ?x2312)))
 (let (($x1133 (= (stack_s x_0 x_1 x_2 x_3 1 ?x2312) ?x1590)))
 (let (($x9248 (forall ((w (_ BitVec 256)) )(let ((?x4950 (storage_s x_0 x_1 x_2 x_3 0 w)))
 (= ?x4950 (_ bv0 256))))
 ))
 (let (($x2473 (= ?x7983 0)))
 (let (($x11787 (not $x57)))
 (let (($x10013 (= (stack_s x_0 x_1 x_2 x_3 0 (_ bv3 6)) x_3)))
 (let (($x7937 (= (stack_s x_0 x_1 x_2 x_3 0 (_ bv2 6)) x_2)))
 (let (($x311 (= (stack_s x_0 x_1 x_2 x_3 0 (_ bv1 6)) x_1)))
 (let (($x8107 (= (stack_s x_0 x_1 x_2 x_3 0 (_ bv0 6)) x_0)))
 (let (($x9861 (= ?x72 (_ bv4 6))))
 (and $x9861 $x8107 $x311 $x7937 $x10013 $x11787 $x2473 $x9248 (= (stack_s x_0 x_1 x_2 x_3 1 ?x11806) ?x1590) $x1133 $x8105 $x11593 $x4263 $x6880 $x4357 (= ?x7909 (+ 2 ?x2965)) $x10109 $x5841 $x1881 $x3121 $x2506 (= (stack_s x_0 x_1 x_2 x_3 3 (bvadd (_ bv60 6) ?x275)) ?x7214) (= ?x9936 (stack_s x_0 x_1 x_2 x_3 2 (bvadd (_ bv61 6) ?x218))) (= ?x8067 (stack_s x_0 x_1 x_2 x_3 2 (bvadd (_ bv62 6) ?x218))) $x2766 $x4482 $x6893 $x3942 $x6329 (= ?x3821 ?x9936) (= ?x3900 ?x4281) (= ?x1439 ?x8067) $x6108 $x3747 $x8269 $x5859 $x9881 $x10301 $x8607 $x8687 $x3553 $x5870 (= ?x8964 (+ 2 ?x1720)) (= ?x926 (bvadd (_ bv63 6) ?x805)) $x4706 $x10595 $x5057 $x8358 (= (stack_t x_0 x_1 x_2 x_3 1 (bvadd (_ bv61 6) ?x4023)) ?x7836) (= ?x6850 (stack_t x_0 x_1 x_2 x_3 0 (bvadd (_ bv62 6) ?x63))) $x4745 $x9875 $x9709 $x3064 (= (exc_halt_t 1) (or $x56 (not (bvsle (_ bv0 6) ?x8725)))) (= ?x8277 (bvadd ?x8164 (bvmul (_ bv115792089237316195423570985008687907853269984665640564039457584007913129639935 256) ?x6850))) $x7888 (= ?x4056 ?x4690) $x6757 $x6953 $x6259 (= ?x7568 (+ 2 ?x9329)) (= ?x11842 ?x180) $x7098 $x5299 $x509 (= ?x5407 ?x6357) (= (stack_t x_0 x_1 x_2 x_3 4 (bvadd (_ bv62 6) ?x3757)) ?x9153) $x2958 $x9721 $x3469 $x4777 $x9828 $x73 $x6925 $x58 $x541 $x2987 $x7795))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
 )
(check-sat)
(get-proof)
