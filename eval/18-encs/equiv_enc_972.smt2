; SWAP1 DUP2 SWAP1 SHA3 SWAP2 SWAP1 => DUP1 SWAP3 SWAP2 SHA3 SWAP2
(set-option :produce-proofs true)
; 
(set-info :status unknown)
(declare-fun storage_t ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 256)) (_ BitVec 256))
(declare-fun storage_s ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 256)) (_ BitVec 256))
(declare-fun exc_halt_t (Int) Bool)
(declare-fun exc_halt_s (Int) Bool)
(declare-fun sc_t (Int) (_ BitVec 6))
(declare-fun stack_t ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 6)) (_ BitVec 256))
(declare-fun stack_s ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 6)) (_ BitVec 256))
(declare-fun sc_s (Int) (_ BitVec 6))
(declare-fun used_gas_t ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int) Int)
(declare-fun used_gas_s ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int) Int)
(declare-fun f_SHA3 ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256)) (_ BitVec 256))
(assert
 (exists ((x_0 (_ BitVec 256)) (x_1 (_ BitVec 256)) (x_2 (_ BitVec 256)) (x_SHA3_0 (_ BitVec 256)) )(let (($x7558 (forall ((w (_ BitVec 256)) )(let ((?x42 (storage_t x_0 x_1 x_2 x_SHA3_0 5 w)))
 (let ((?x2007 (storage_s x_0 x_1 x_2 x_SHA3_0 6 w)))
 (= ?x2007 ?x42))))
 ))
 (let (($x10311 (exc_halt_t 5)))
 (let (($x7121 (exc_halt_s 6)))
 (let (($x4790 (= $x7121 $x10311)))
 (let (($x6204 (forall ((n (_ BitVec 6)) )(let ((?x10036 (sc_t 5)))
 (let (($x4214 (bvsle ?x10036 n)))
 (let ((?x569 (stack_t x_0 x_1 x_2 x_SHA3_0 5 n)))
 (let ((?x5601 (stack_s x_0 x_1 x_2 x_SHA3_0 6 n)))
 (let (($x2296 (= ?x5601 ?x569)))
 (or $x2296 $x4214)))))))
 ))
 (let ((?x10036 (sc_t 5)))
 (let ((?x9114 (sc_s 6)))
 (let (($x6242 (= ?x9114 ?x10036)))
 (let ((?x11593 (used_gas_t x_0 x_1 x_2 x_SHA3_0 0)))
 (let ((?x4303 (used_gas_s x_0 x_1 x_2 x_SHA3_0 0)))
 (let (($x3707 (= ?x4303 ?x11593)))
 (let (($x1470 (forall ((w (_ BitVec 256)) )(let ((?x2103 (storage_t x_0 x_1 x_2 x_SHA3_0 0 w)))
 (let ((?x6743 (storage_s x_0 x_1 x_2 x_SHA3_0 0 w)))
 (= ?x6743 ?x2103))))
 ))
 (let (($x56 (exc_halt_t 0)))
 (let (($x57 (exc_halt_s 0)))
 (let (($x58 (= $x57 $x56)))
 (let (($x11377 (forall ((n (_ BitVec 6)) )(let ((?x63 (sc_t 0)))
 (let (($x11318 (bvsle ?x63 n)))
 (let ((?x3962 (stack_t x_0 x_1 x_2 x_SHA3_0 0 n)))
 (let ((?x7340 (stack_s x_0 x_1 x_2 x_SHA3_0 0 n)))
 (let (($x11107 (= ?x7340 ?x3962)))
 (or $x11107 $x11318)))))))
 ))
 (let ((?x63 (sc_t 0)))
 (let ((?x72 (sc_s 0)))
 (let (($x73 (= ?x72 ?x63)))
 (let (($x9941 (= $x10311 (or (exc_halt_t 4) (not (bvsle (_ bv0 6) (bvadd (_ bv61 6) (sc_t 4))))))))
 (let (($x8355 (forall ((w (_ BitVec 256)) )(let ((?x822 (storage_t x_0 x_1 x_2 x_SHA3_0 4 w)))
 (let ((?x42 (storage_t x_0 x_1 x_2 x_SHA3_0 5 w)))
 (= ?x42 ?x822))))
 ))
 (let (($x6548 (forall ((n (_ BitVec 6)) )(let ((?x9497 (stack_t x_0 x_1 x_2 x_SHA3_0 4 n)))
 (let ((?x569 (stack_t x_0 x_1 x_2 x_SHA3_0 5 n)))
 (or (= ?x569 ?x9497) (bvsle (bvadd (_ bv61 6) (sc_t 4)) n)))))
 ))
 (let (($x834 (= (used_gas_t x_0 x_1 x_2 x_SHA3_0 5) (+ 3 (used_gas_t x_0 x_1 x_2 x_SHA3_0 4)))))
 (let (($x185 (= (stack_t x_0 x_1 x_2 x_SHA3_0 5 (bvadd (_ bv62 6) ?x10036)) (stack_t x_0 x_1 x_2 x_SHA3_0 4 (bvadd (_ bv62 6) (sc_t 4))))))
 (let ((?x7495 (sc_t 4)))
 (let ((?x8167 (bvadd (_ bv63 6) ?x7495)))
 (let ((?x4385 (stack_t x_0 x_1 x_2 x_SHA3_0 4 ?x8167)))
 (let (($x3112 (= (stack_t x_0 x_1 x_2 x_SHA3_0 5 (bvadd (_ bv63 6) ?x10036)) (stack_t x_0 x_1 x_2 x_SHA3_0 4 (bvadd (_ bv61 6) ?x7495)))))
 (let (($x7854 (exc_halt_t 4)))
 (let (($x3047 (= $x7854 (or (exc_halt_t 3) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_t 3))))))))
 (let (($x7833 (forall ((w (_ BitVec 256)) )(let ((?x5991 (storage_t x_0 x_1 x_2 x_SHA3_0 3 w)))
 (let ((?x822 (storage_t x_0 x_1 x_2 x_SHA3_0 4 w)))
 (= ?x822 ?x5991))))
 ))
 (let (($x8985 (forall ((n (_ BitVec 6)) )(let ((?x10526 (stack_t x_0 x_1 x_2 x_SHA3_0 3 n)))
 (let ((?x9497 (stack_t x_0 x_1 x_2 x_SHA3_0 4 n)))
 (let ((?x7621 (sc_t 3)))
 (let ((?x150 (bvadd (_ bv62 6) ?x7621)))
 (let (($x5841 (bvsle ?x150 n)))
 (or $x5841 (= ?x9497 ?x10526))))))))
 ))
 (let ((?x7621 (sc_t 3)))
 (let ((?x9251 (bvadd (_ bv63 6) ?x7621)))
 (let (($x5099 (= ?x7495 ?x9251)))
 (let ((?x6469 (used_gas_t x_0 x_1 x_2 x_SHA3_0 4)))
 (let (($x9964 (= ?x6469 (+ 30 (used_gas_t x_0 x_1 x_2 x_SHA3_0 3)))))
 (let ((?x150 (bvadd (_ bv62 6) ?x7621)))
 (let ((?x10881 (stack_t x_0 x_1 x_2 x_SHA3_0 3 ?x150)))
 (let ((?x4849 (stack_t x_0 x_1 x_2 x_SHA3_0 3 ?x9251)))
 (let (($x5543 (= ?x4385 (f_SHA3 x_0 x_1 x_2 x_SHA3_0 ?x4849 ?x10881))))
 (let (($x5429 (exc_halt_t 3)))
 (let (($x4234 (= $x5429 (or (exc_halt_t 2) (not (bvsle (_ bv0 6) (bvadd (_ bv61 6) (sc_t 2))))))))
 (let (($x2986 (forall ((w (_ BitVec 256)) )(let ((?x4361 (storage_t x_0 x_1 x_2 x_SHA3_0 2 w)))
 (let ((?x5991 (storage_t x_0 x_1 x_2 x_SHA3_0 3 w)))
 (= ?x5991 ?x4361))))
 ))
 (let (($x11426 (forall ((n (_ BitVec 6)) )(let ((?x10979 (stack_t x_0 x_1 x_2 x_SHA3_0 2 n)))
 (let ((?x10526 (stack_t x_0 x_1 x_2 x_SHA3_0 3 n)))
 (let (($x11080 (= ?x10526 ?x10979)))
 (or (bvsle (bvadd (_ bv61 6) (sc_t 2)) n) $x11080)))))
 ))
 (let ((?x8890 (sc_t 2)))
 (let (($x5385 (= ?x7621 ?x8890)))
 (let ((?x9307 (used_gas_t x_0 x_1 x_2 x_SHA3_0 3)))
 (let (($x7262 (= ?x9307 (+ 3 (used_gas_t x_0 x_1 x_2 x_SHA3_0 2)))))
 (let ((?x4746 (bvadd (_ bv62 6) ?x8890)))
 (let ((?x6780 (stack_t x_0 x_1 x_2 x_SHA3_0 2 ?x4746)))
 (let (($x8308 (= ?x10881 ?x6780)))
 (let ((?x6760 (bvadd (_ bv63 6) ?x8890)))
 (let ((?x11948 (stack_t x_0 x_1 x_2 x_SHA3_0 2 ?x6760)))
 (let ((?x11507 (bvadd (_ bv61 6) ?x7621)))
 (let ((?x461 (stack_t x_0 x_1 x_2 x_SHA3_0 3 ?x11507)))
 (let (($x7505 (exc_halt_t 2)))
 (let (($x5110 (= $x7505 (or (exc_halt_t 1) (not (bvsle (_ bv0 6) (bvadd (_ bv60 6) (sc_t 1))))))))
 (let (($x10211 (forall ((w (_ BitVec 256)) )(let ((?x548 (storage_t x_0 x_1 x_2 x_SHA3_0 1 w)))
 (let ((?x4361 (storage_t x_0 x_1 x_2 x_SHA3_0 2 w)))
 (= ?x4361 ?x548))))
 ))
 (let (($x8170 (forall ((n (_ BitVec 6)) )(let ((?x1280 (stack_t x_0 x_1 x_2 x_SHA3_0 1 n)))
 (let ((?x10979 (stack_t x_0 x_1 x_2 x_SHA3_0 2 n)))
 (let (($x10875 (= ?x10979 ?x1280)))
 (or $x10875 (bvsle (bvadd (_ bv60 6) (sc_t 1)) n))))))
 ))
 (let ((?x4554 (sc_t 1)))
 (let (($x6914 (= ?x8890 ?x4554)))
 (let ((?x10707 (used_gas_t x_0 x_1 x_2 x_SHA3_0 2)))
 (let (($x2064 (= ?x10707 (+ 3 (used_gas_t x_0 x_1 x_2 x_SHA3_0 1)))))
 (let ((?x4371 (bvadd (_ bv61 6) ?x8890)))
 (let ((?x7185 (stack_t x_0 x_1 x_2 x_SHA3_0 2 ?x4371)))
 (let ((?x6802 (bvadd (_ bv63 6) ?x4554)))
 (let ((?x11471 (stack_t x_0 x_1 x_2 x_SHA3_0 1 ?x6802)))
 (let ((?x5041 (bvadd (_ bv60 6) ?x8890)))
 (let ((?x3520 (stack_t x_0 x_1 x_2 x_SHA3_0 2 ?x5041)))
 (let (($x921 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x63))) (_ bv0 1)))))
 (let (($x11985 (exc_halt_t 1)))
 (let (($x2703 (forall ((w (_ BitVec 256)) )(let ((?x2103 (storage_t x_0 x_1 x_2 x_SHA3_0 0 w)))
 (let ((?x548 (storage_t x_0 x_1 x_2 x_SHA3_0 1 w)))
 (= ?x548 ?x2103))))
 ))
 (let (($x7893 (forall ((n (_ BitVec 6)) )(let ((?x3962 (stack_t x_0 x_1 x_2 x_SHA3_0 0 n)))
 (let ((?x1280 (stack_t x_0 x_1 x_2 x_SHA3_0 1 n)))
 (let (($x2920 (= ?x1280 ?x3962)))
 (or $x2920 (bvsle (bvadd (_ bv63 6) (sc_t 0)) n))))))
 ))
 (let (($x2096 (= ?x4554 (bvadd (_ bv1 6) ?x63))))
 (let ((?x7996 (used_gas_t x_0 x_1 x_2 x_SHA3_0 1)))
 (let (($x3111 (= ?x7996 (+ 3 ?x11593))))
 (let ((?x3574 (bvadd (_ bv63 6) ?x63)))
 (let ((?x10845 (stack_t x_0 x_1 x_2 x_SHA3_0 0 ?x3574)))
 (let (($x4296 (= (stack_t x_0 x_1 x_2 x_SHA3_0 1 ?x3574) ?x10845)))
 (let (($x11199 (= $x7121 (or (exc_halt_s 5) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 5))))))))
 (let (($x6265 (forall ((w (_ BitVec 256)) )(let ((?x3900 (storage_s x_0 x_1 x_2 x_SHA3_0 5 w)))
 (let ((?x2007 (storage_s x_0 x_1 x_2 x_SHA3_0 6 w)))
 (= ?x2007 ?x3900))))
 ))
 (let (($x931 (forall ((n (_ BitVec 6)) )(let ((?x9017 (stack_s x_0 x_1 x_2 x_SHA3_0 5 n)))
 (let ((?x5601 (stack_s x_0 x_1 x_2 x_SHA3_0 6 n)))
 (let (($x11541 (= ?x5601 ?x9017)))
 (let ((?x4319 (sc_s 5)))
 (let ((?x3570 (bvadd (_ bv62 6) ?x4319)))
 (let (($x4799 (bvsle ?x3570 n)))
 (or $x4799 $x11541))))))))
 ))
 (let ((?x4319 (sc_s 5)))
 (let (($x4694 (= ?x9114 ?x4319)))
 (let (($x6182 (= (used_gas_s x_0 x_1 x_2 x_SHA3_0 6) (+ 3 (used_gas_s x_0 x_1 x_2 x_SHA3_0 5)))))
 (let ((?x9002 (bvadd (_ bv63 6) ?x4319)))
 (let ((?x3918 (stack_s x_0 x_1 x_2 x_SHA3_0 5 ?x9002)))
 (let ((?x8375 (stack_s x_0 x_1 x_2 x_SHA3_0 6 (bvadd (_ bv62 6) ?x9114))))
 (let ((?x3570 (bvadd (_ bv62 6) ?x4319)))
 (let ((?x3994 (stack_s x_0 x_1 x_2 x_SHA3_0 5 ?x3570)))
 (let ((?x7027 (bvadd (_ bv63 6) ?x9114)))
 (let ((?x10034 (stack_s x_0 x_1 x_2 x_SHA3_0 6 ?x7027)))
 (let (($x1862 (exc_halt_s 5)))
 (let (($x1481 (= $x1862 (or (exc_halt_s 4) (not (bvsle (_ bv0 6) (bvadd (_ bv61 6) (sc_s 4))))))))
 (let (($x4902 (forall ((w (_ BitVec 256)) )(let ((?x9821 (storage_s x_0 x_1 x_2 x_SHA3_0 4 w)))
 (let ((?x3900 (storage_s x_0 x_1 x_2 x_SHA3_0 5 w)))
 (= ?x3900 ?x9821))))
 ))
 (let (($x4028 (forall ((n (_ BitVec 6)) )(let ((?x9550 (stack_s x_0 x_1 x_2 x_SHA3_0 4 n)))
 (let ((?x9017 (stack_s x_0 x_1 x_2 x_SHA3_0 5 n)))
 (let (($x10835 (= ?x9017 ?x9550)))
 (or (bvsle (bvadd (_ bv61 6) (sc_s 4)) n) $x10835)))))
 ))
 (let ((?x9433 (sc_s 4)))
 (let (($x1752 (= ?x4319 ?x9433)))
 (let ((?x3383 (used_gas_s x_0 x_1 x_2 x_SHA3_0 5)))
 (let (($x2218 (= ?x3383 (+ 3 (used_gas_s x_0 x_1 x_2 x_SHA3_0 4)))))
 (let ((?x3395 (bvadd (_ bv63 6) ?x9433)))
 (let ((?x11385 (stack_s x_0 x_1 x_2 x_SHA3_0 4 ?x3395)))
 (let ((?x7985 (bvadd (_ bv61 6) ?x4319)))
 (let ((?x1625 (stack_s x_0 x_1 x_2 x_SHA3_0 5 ?x7985)))
 (let (($x9175 (exc_halt_s 4)))
 (let (($x1274 (= $x9175 (or (exc_halt_s 3) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 3))))))))
 (let (($x400 (forall ((w (_ BitVec 256)) )(let ((?x10424 (storage_s x_0 x_1 x_2 x_SHA3_0 3 w)))
 (let ((?x9821 (storage_s x_0 x_1 x_2 x_SHA3_0 4 w)))
 (= ?x9821 ?x10424))))
 ))
 (let (($x11501 (forall ((n (_ BitVec 6)) )(let ((?x3575 (stack_s x_0 x_1 x_2 x_SHA3_0 3 n)))
 (let ((?x9550 (stack_s x_0 x_1 x_2 x_SHA3_0 4 n)))
 (let (($x10474 (= ?x9550 ?x3575)))
 (let ((?x3851 (sc_s 3)))
 (let ((?x8089 (bvadd (_ bv62 6) ?x3851)))
 (let (($x1497 (bvsle ?x8089 n)))
 (or $x1497 $x10474))))))))
 ))
 (let ((?x7118 (used_gas_s x_0 x_1 x_2 x_SHA3_0 4)))
 (let ((?x3851 (sc_s 3)))
 (let ((?x8089 (bvadd (_ bv62 6) ?x3851)))
 (let ((?x11659 (stack_s x_0 x_1 x_2 x_SHA3_0 3 ?x8089)))
 (let ((?x1738 (bvadd (_ bv63 6) ?x3851)))
 (let ((?x7409 (stack_s x_0 x_1 x_2 x_SHA3_0 3 ?x1738)))
 (let (($x8103 (exc_halt_s 3)))
 (let (($x4380 (= $x8103 (or (exc_halt_s 2) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 2))))))))
 (let (($x5352 (forall ((w (_ BitVec 256)) )(let ((?x1967 (storage_s x_0 x_1 x_2 x_SHA3_0 2 w)))
 (let ((?x10424 (storage_s x_0 x_1 x_2 x_SHA3_0 3 w)))
 (= ?x10424 ?x1967))))
 ))
 (let (($x5195 (forall ((n (_ BitVec 6)) )(let ((?x2272 (sc_s 2)))
 (let ((?x7947 (bvadd (_ bv62 6) ?x2272)))
 (let (($x7062 (bvsle ?x7947 n)))
 (let ((?x5322 (stack_s x_0 x_1 x_2 x_SHA3_0 2 n)))
 (let ((?x3575 (stack_s x_0 x_1 x_2 x_SHA3_0 3 n)))
 (or (= ?x3575 ?x5322) $x7062)))))))
 ))
 (let ((?x2272 (sc_s 2)))
 (let (($x5381 (= ?x3851 ?x2272)))
 (let ((?x10754 (used_gas_s x_0 x_1 x_2 x_SHA3_0 3)))
 (let (($x2862 (= ?x10754 (+ 3 (used_gas_s x_0 x_1 x_2 x_SHA3_0 2)))))
 (let ((?x9000 (bvadd (_ bv63 6) ?x2272)))
 (let ((?x10558 (stack_s x_0 x_1 x_2 x_SHA3_0 2 ?x9000)))
 (let (($x1673 (= ?x11659 ?x10558)))
 (let ((?x7947 (bvadd (_ bv62 6) ?x2272)))
 (let ((?x260 (stack_s x_0 x_1 x_2 x_SHA3_0 2 ?x7947)))
 (let (($x10169 (= ?x7409 ?x260)))
 (let (($x8780 (exc_halt_s 1)))
 (let (($x4393 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 1)))) (_ bv0 1)))))
 (let (($x10052 (exc_halt_s 2)))
 (let (($x5382 (= $x10052 (or (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 1)))) $x4393 $x8780))))
 (let (($x7373 (forall ((w (_ BitVec 256)) )(let ((?x4803 (storage_s x_0 x_1 x_2 x_SHA3_0 1 w)))
 (let ((?x1967 (storage_s x_0 x_1 x_2 x_SHA3_0 2 w)))
 (= ?x1967 ?x4803))))
 ))
 (let (($x3048 (forall ((n (_ BitVec 6)) )(let ((?x154 (sc_s 1)))
 (let ((?x8244 (bvadd (_ bv62 6) ?x154)))
 (let (($x6954 (bvsle ?x8244 n)))
 (let ((?x6880 (stack_s x_0 x_1 x_2 x_SHA3_0 1 n)))
 (let ((?x5322 (stack_s x_0 x_1 x_2 x_SHA3_0 2 n)))
 (let (($x960 (= ?x5322 ?x6880)))
 (or $x960 $x6954))))))))
 ))
 (let (($x9417 (= ?x2272 (bvadd (_ bv1 6) (sc_s 1)))))
 (let ((?x2941 (used_gas_s x_0 x_1 x_2 x_SHA3_0 2)))
 (let (($x8521 (= ?x2941 (+ 3 (used_gas_s x_0 x_1 x_2 x_SHA3_0 1)))))
 (let ((?x154 (sc_s 1)))
 (let ((?x1791 (bvadd (_ bv63 6) ?x154)))
 (let ((?x11275 (stack_s x_0 x_1 x_2 x_SHA3_0 1 ?x1791)))
 (let (($x1240 (= (stack_s x_0 x_1 x_2 x_SHA3_0 2 ?x1791) ?x11275)))
 (let ((?x8244 (bvadd (_ bv62 6) ?x154)))
 (let ((?x1790 (stack_s x_0 x_1 x_2 x_SHA3_0 1 ?x8244)))
 (let (($x987 (= (stack_s x_0 x_1 x_2 x_SHA3_0 2 ?x8244) ?x1790)))
 (let (($x8708 (= ?x10558 ?x1790)))
 (let (($x7140 (= $x8780 (or $x57 (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) ?x72)))))))
 (let (($x1118 (forall ((w (_ BitVec 256)) )(let ((?x6743 (storage_s x_0 x_1 x_2 x_SHA3_0 0 w)))
 (let ((?x4803 (storage_s x_0 x_1 x_2 x_SHA3_0 1 w)))
 (= ?x4803 ?x6743))))
 ))
 (let (($x7278 (forall ((n (_ BitVec 6)) )(let ((?x7340 (stack_s x_0 x_1 x_2 x_SHA3_0 0 n)))
 (let ((?x6880 (stack_s x_0 x_1 x_2 x_SHA3_0 1 n)))
 (let (($x7806 (= ?x6880 ?x7340)))
 (let ((?x72 (sc_s 0)))
 (let ((?x10468 (bvadd (_ bv62 6) ?x72)))
 (let (($x2129 (bvsle ?x10468 n)))
 (or $x2129 $x7806))))))))
 ))
 (let (($x9780 (= ?x154 ?x72)))
 (let ((?x2119 (used_gas_s x_0 x_1 x_2 x_SHA3_0 1)))
 (let (($x396 (= ?x2119 (+ 3 ?x4303))))
 (let ((?x10288 (bvadd (_ bv63 6) ?x72)))
 (let ((?x11518 (stack_s x_0 x_1 x_2 x_SHA3_0 0 ?x10288)))
 (let (($x10772 (= ?x1790 ?x11518)))
 (let (($x9327 (= ?x11275 (stack_s x_0 x_1 x_2 x_SHA3_0 0 (bvadd (_ bv62 6) ?x72)))))
 (let (($x3088 (forall ((w0 (_ BitVec 256)) (w1 (_ BitVec 256)) )(let (($x3119 (= (stack_s x_0 x_1 x_2 x_SHA3_0 3 (bvadd (_ bv62 6) (sc_s 3))) w1)))
 (let (($x9869 (= (stack_s x_0 x_1 x_2 x_SHA3_0 3 (bvadd (_ bv63 6) (sc_s 3))) w0)))
 (let ((?x6076 (f_SHA3 x_0 x_1 x_2 x_SHA3_0 w0 w1)))
 (= ?x6076 (ite (and $x9869 $x3119) x_SHA3_0 (_ bv0 256)))))))
 ))
 (let (($x2229 (forall ((w (_ BitVec 256)) )(let ((?x6743 (storage_s x_0 x_1 x_2 x_SHA3_0 0 w)))
 (= ?x6743 (_ bv0 256))))
 ))
 (let (($x1717 (= ?x4303 0)))
 (let (($x3971 (not $x57)))
 (let (($x11059 (= (stack_s x_0 x_1 x_2 x_SHA3_0 0 (_ bv2 6)) x_2)))
 (let (($x9341 (= (stack_s x_0 x_1 x_2 x_SHA3_0 0 (_ bv1 6)) x_1)))
 (let (($x10662 (= (stack_s x_0 x_1 x_2 x_SHA3_0 0 (_ bv0 6)) x_0)))
 (let (($x9108 (= ?x72 (_ bv3 6))))
 (and $x9108 $x10662 $x9341 $x11059 $x3971 $x1717 $x2229 $x3088 $x9327 $x10772 $x396 $x9780 $x7278 $x1118 $x7140 $x8708 $x987 $x1240 $x8521 $x9417 $x3048 $x7373 $x5382 $x10169 $x1673 $x2862 $x5381 $x5195 $x5352 $x4380 (= ?x11385 (f_SHA3 x_0 x_1 x_2 x_SHA3_0 ?x7409 ?x11659)) (= ?x7118 (+ 30 ?x10754)) (= ?x9433 ?x1738) $x11501 $x400 $x1274 (= ?x3918 (stack_s x_0 x_1 x_2 x_SHA3_0 4 (bvadd (_ bv61 6) ?x9433))) (= ?x1625 ?x11385) (= ?x3994 (stack_s x_0 x_1 x_2 x_SHA3_0 4 (bvadd (_ bv62 6) ?x9433))) $x2218 $x1752 $x4028 $x4902 $x1481 (= ?x10034 ?x3994) (= ?x8375 ?x3918) $x6182 $x4694 $x931 $x6265 $x11199 (= ?x11471 ?x10845) $x4296 $x3111 $x2096 $x7893 $x2703 (= $x11985 (or $x56 $x921 (not (bvsle (_ bv0 6) ?x3574)))) (= ?x11948 (stack_t x_0 x_1 x_2 x_SHA3_0 1 (bvadd (_ bv60 6) ?x4554))) (= ?x3520 ?x11471) (= ?x7185 (stack_t x_0 x_1 x_2 x_SHA3_0 1 (bvadd (_ bv61 6) ?x4554))) (= ?x6780 (stack_t x_0 x_1 x_2 x_SHA3_0 1 (bvadd (_ bv62 6) ?x4554))) $x2064 $x6914 $x8170 $x10211 $x5110 (= ?x4849 ?x7185) (= ?x461 ?x11948) $x8308 $x7262 $x5385 $x11426 $x2986 $x4234 $x5543 $x9964 $x5099 $x8985 $x7833 $x3047 $x3112 (= (stack_t x_0 x_1 x_2 x_SHA3_0 5 (bvadd (_ bv61 6) ?x10036)) ?x4385) $x185 $x834 (= ?x10036 ?x7495) $x6548 $x8355 $x9941 $x73 $x11377 $x58 $x1470 $x3707 (not (and $x6242 $x6204 $x4790 $x7558))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
 )
(check-sat)
(get-proof)
