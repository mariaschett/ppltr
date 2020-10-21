; DUP2 DUP2 ADD DUP4 SWAP1 => DUP3 DUP2 DUP4 ADD
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
 (exists ((x_0 (_ BitVec 256)) (x_1 (_ BitVec 256)) (x_2 (_ BitVec 256)) )(let (($x3726 (forall ((w (_ BitVec 256)) )(let ((?x2734 (storage_t x_0 x_1 x_2 4 w)))
 (let ((?x9755 (storage_s x_0 x_1 x_2 5 w)))
 (= ?x9755 ?x2734))))
 ))
 (let (($x7854 (exc_halt_t 4)))
 (let (($x11317 (exc_halt_s 5)))
 (let (($x3163 (= $x11317 $x7854)))
 (let (($x232 (forall ((n (_ BitVec 6)) )(let ((?x4818 (sc_t 4)))
 (let (($x1999 (bvsle ?x4818 n)))
 (let ((?x5485 (stack_t x_0 x_1 x_2 4 n)))
 (let ((?x5218 (stack_s x_0 x_1 x_2 5 n)))
 (let (($x764 (= ?x5218 ?x5485)))
 (or $x764 $x1999)))))))
 ))
 (let ((?x4818 (sc_t 4)))
 (let ((?x4319 (sc_s 5)))
 (let (($x7147 (= ?x4319 ?x4818)))
 (let ((?x6423 (used_gas_t x_0 x_1 x_2 0)))
 (let ((?x5033 (used_gas_s x_0 x_1 x_2 0)))
 (let (($x8373 (= ?x5033 ?x6423)))
 (let (($x1120 (forall ((w (_ BitVec 256)) )(let ((?x10985 (storage_t x_0 x_1 x_2 0 w)))
 (let ((?x29 (storage_s x_0 x_1 x_2 0 w)))
 (= ?x29 ?x10985))))
 ))
 (let (($x56 (exc_halt_t 0)))
 (let (($x57 (exc_halt_s 0)))
 (let (($x58 (= $x57 $x56)))
 (let (($x7461 (forall ((n (_ BitVec 6)) )(let ((?x6698 (stack_t x_0 x_1 x_2 0 n)))
 (let ((?x3994 (stack_s x_0 x_1 x_2 0 n)))
 (let (($x11185 (= ?x3994 ?x6698)))
 (let ((?x63 (sc_t 0)))
 (let (($x1710 (bvsle ?x63 n)))
 (or $x1710 $x11185)))))))
 ))
 (let ((?x63 (sc_t 0)))
 (let ((?x72 (sc_s 0)))
 (let (($x73 (= ?x72 ?x63)))
 (let (($x7977 (= $x7854 (or (exc_halt_t 3) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_t 3))))))))
 (let (($x7027 (forall ((w (_ BitVec 256)) )(let ((?x4036 (storage_t x_0 x_1 x_2 3 w)))
 (let ((?x2734 (storage_t x_0 x_1 x_2 4 w)))
 (= ?x2734 ?x4036))))
 ))
 (let (($x7374 (forall ((n (_ BitVec 6)) )(let ((?x6438 (sc_t 3)))
 (let ((?x10981 (bvadd (_ bv62 6) ?x6438)))
 (let (($x1812 (bvsle ?x10981 n)))
 (let ((?x4337 (stack_t x_0 x_1 x_2 3 n)))
 (let ((?x5485 (stack_t x_0 x_1 x_2 4 n)))
 (or (= ?x5485 ?x4337) $x1812)))))))
 ))
 (let ((?x6438 (sc_t 3)))
 (let ((?x9047 (bvadd (_ bv63 6) ?x6438)))
 (let (($x4948 (= ?x4818 ?x9047)))
 (let (($x5911 (= (used_gas_t x_0 x_1 x_2 4) (+ 3 (used_gas_t x_0 x_1 x_2 3)))))
 (let ((?x2819 (stack_t x_0 x_1 x_2 3 ?x9047)))
 (let (($x9392 (= (stack_t x_0 x_1 x_2 4 (bvadd (_ bv63 6) ?x4818)) (bvadd ?x2819 (stack_t x_0 x_1 x_2 3 (bvadd (_ bv62 6) ?x6438))))))
 (let (($x9682 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_t 2)))) (_ bv0 1)))))
 (let (($x2163 (exc_halt_t 2)))
 (let (($x6783 (exc_halt_t 3)))
 (let (($x5518 (= $x6783 (or $x2163 (not (bvsle (_ bv0 6) (bvadd (_ bv60 6) (sc_t 2)))) $x9682))))
 (let (($x4905 (forall ((w (_ BitVec 256)) )(let ((?x835 (storage_t x_0 x_1 x_2 2 w)))
 (let ((?x4036 (storage_t x_0 x_1 x_2 3 w)))
 (= ?x4036 ?x835))))
 ))
 (let (($x1167 (forall ((n (_ BitVec 6)) )(let ((?x9364 (stack_t x_0 x_1 x_2 2 n)))
 (let ((?x4337 (stack_t x_0 x_1 x_2 3 n)))
 (let ((?x2714 (sc_t 2)))
 (let ((?x8658 (bvadd (_ bv60 6) ?x2714)))
 (let (($x2201 (bvsle ?x8658 n)))
 (or $x2201 (= ?x4337 ?x9364))))))))
 ))
 (let (($x11685 (= ?x6438 (bvadd (_ bv1 6) (sc_t 2)))))
 (let ((?x6346 (used_gas_t x_0 x_1 x_2 3)))
 (let ((?x2714 (sc_t 2)))
 (let ((?x6840 (bvadd (_ bv63 6) ?x2714)))
 (let ((?x4437 (stack_t x_0 x_1 x_2 2 ?x6840)))
 (let (($x4033 (= (stack_t x_0 x_1 x_2 3 (bvadd (_ bv62 6) ?x2714)) (stack_t x_0 x_1 x_2 2 (bvadd (_ bv62 6) ?x2714)))))
 (let (($x11833 (= (stack_t x_0 x_1 x_2 3 (bvadd (_ bv61 6) ?x2714)) (stack_t x_0 x_1 x_2 2 (bvadd (_ bv61 6) ?x2714)))))
 (let ((?x8658 (bvadd (_ bv60 6) ?x2714)))
 (let ((?x6616 (stack_t x_0 x_1 x_2 2 ?x8658)))
 (let (($x8377 (exc_halt_t 1)))
 (let (($x7439 (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_t 1))))))
 (let (($x3121 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_t 1)))) (_ bv0 1)))))
 (let (($x7376 (forall ((w (_ BitVec 256)) )(let ((?x2272 (storage_t x_0 x_1 x_2 1 w)))
 (let ((?x835 (storage_t x_0 x_1 x_2 2 w)))
 (= ?x835 ?x2272))))
 ))
 (let (($x7830 (forall ((n (_ BitVec 6)) )(let ((?x4208 (stack_t x_0 x_1 x_2 1 n)))
 (let ((?x9364 (stack_t x_0 x_1 x_2 2 n)))
 (let (($x7535 (= ?x9364 ?x4208)))
 (let ((?x7154 (sc_t 1)))
 (let ((?x10607 (bvadd (_ bv62 6) ?x7154)))
 (let (($x5860 (bvsle ?x10607 n)))
 (or $x5860 $x7535))))))))
 ))
 (let (($x1589 (= ?x2714 (bvadd (_ bv1 6) (sc_t 1)))))
 (let ((?x11061 (used_gas_t x_0 x_1 x_2 2)))
 (let ((?x7154 (sc_t 1)))
 (let ((?x2642 (bvadd (_ bv63 6) ?x7154)))
 (let ((?x8535 (stack_t x_0 x_1 x_2 1 ?x2642)))
 (let ((?x10607 (bvadd (_ bv62 6) ?x7154)))
 (let ((?x3751 (stack_t x_0 x_1 x_2 1 ?x10607)))
 (let (($x6063 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x63))) (_ bv0 1)))))
 (let (($x4780 (not (bvsle (_ bv0 6) (bvadd (_ bv61 6) ?x63)))))
 (let (($x10152 (forall ((w (_ BitVec 256)) )(let ((?x10985 (storage_t x_0 x_1 x_2 0 w)))
 (let ((?x2272 (storage_t x_0 x_1 x_2 1 w)))
 (= ?x2272 ?x10985))))
 ))
 (let (($x8780 (forall ((n (_ BitVec 6)) )(let ((?x63 (sc_t 0)))
 (let ((?x11452 (bvadd (_ bv61 6) ?x63)))
 (let (($x11541 (bvsle ?x11452 n)))
 (let ((?x6698 (stack_t x_0 x_1 x_2 0 n)))
 (let ((?x4208 (stack_t x_0 x_1 x_2 1 n)))
 (let (($x3943 (= ?x4208 ?x6698)))
 (or $x3943 $x11541))))))))
 ))
 (let (($x5986 (= ?x7154 (bvadd (_ bv1 6) ?x63))))
 (let ((?x4619 (used_gas_t x_0 x_1 x_2 1)))
 (let (($x10415 (= ?x4619 (+ 3 ?x6423))))
 (let ((?x4642 (bvadd (_ bv63 6) ?x63)))
 (let ((?x929 (stack_t x_0 x_1 x_2 0 ?x4642)))
 (let (($x3927 (= (stack_t x_0 x_1 x_2 1 ?x4642) ?x929)))
 (let ((?x11161 (bvadd (_ bv62 6) ?x63)))
 (let ((?x6936 (stack_t x_0 x_1 x_2 0 ?x11161)))
 (let (($x6748 (= (stack_t x_0 x_1 x_2 1 ?x11161) ?x6936)))
 (let ((?x11452 (bvadd (_ bv61 6) ?x63)))
 (let ((?x8376 (stack_t x_0 x_1 x_2 0 ?x11452)))
 (let (($x8712 (= $x11317 (or (exc_halt_s 4) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 4))))))))
 (let (($x7203 (forall ((w (_ BitVec 256)) )(let ((?x11584 (storage_s x_0 x_1 x_2 4 w)))
 (let ((?x9755 (storage_s x_0 x_1 x_2 5 w)))
 (= ?x9755 ?x11584))))
 ))
 (let (($x1421 (forall ((n (_ BitVec 6)) )(let ((?x11410 (stack_s x_0 x_1 x_2 4 n)))
 (let ((?x5218 (stack_s x_0 x_1 x_2 5 n)))
 (let (($x7073 (= ?x5218 ?x11410)))
 (let ((?x4305 (sc_s 4)))
 (let ((?x4222 (bvadd (_ bv62 6) ?x4305)))
 (let (($x5750 (bvsle ?x4222 n)))
 (or $x5750 $x7073))))))))
 ))
 (let ((?x4305 (sc_s 4)))
 (let (($x10003 (= ?x4319 ?x4305)))
 (let ((?x74 (used_gas_s x_0 x_1 x_2 5)))
 (let ((?x5156 (bvadd (_ bv63 6) ?x4305)))
 (let ((?x3333 (stack_s x_0 x_1 x_2 4 ?x5156)))
 (let ((?x4222 (bvadd (_ bv62 6) ?x4305)))
 (let ((?x5399 (stack_s x_0 x_1 x_2 4 ?x4222)))
 (let (($x10211 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 3)))) (_ bv0 1)))))
 (let (($x292 (exc_halt_s 3)))
 (let (($x7172 (exc_halt_s 4)))
 (let (($x10046 (forall ((w (_ BitVec 256)) )(let ((?x8583 (storage_s x_0 x_1 x_2 3 w)))
 (let ((?x11584 (storage_s x_0 x_1 x_2 4 w)))
 (= ?x11584 ?x8583))))
 ))
 (let (($x715 (forall ((n (_ BitVec 6)) )(let ((?x10494 (stack_s x_0 x_1 x_2 3 n)))
 (let ((?x11410 (stack_s x_0 x_1 x_2 4 n)))
 (let (($x2582 (= ?x11410 ?x10494)))
 (or (bvsle (bvadd (_ bv60 6) (sc_s 3)) n) $x2582)))))
 ))
 (let (($x4030 (= ?x4305 (bvadd (_ bv1 6) (sc_s 3)))))
 (let ((?x3426 (used_gas_s x_0 x_1 x_2 4)))
 (let (($x8332 (= ?x3426 (+ 3 (used_gas_s x_0 x_1 x_2 3)))))
 (let ((?x275 (sc_s 3)))
 (let ((?x4768 (bvadd (_ bv63 6) ?x275)))
 (let ((?x5446 (stack_s x_0 x_1 x_2 3 ?x4768)))
 (let ((?x3946 (bvadd (_ bv62 6) ?x275)))
 (let ((?x10101 (stack_s x_0 x_1 x_2 3 ?x3946)))
 (let (($x6920 (= (stack_s x_0 x_1 x_2 4 (bvadd (_ bv61 6) ?x275)) (stack_s x_0 x_1 x_2 3 (bvadd (_ bv61 6) ?x275)))))
 (let ((?x855 (bvadd (_ bv60 6) ?x275)))
 (let ((?x8070 (stack_s x_0 x_1 x_2 3 ?x855)))
 (let (($x9813 (= $x292 (or (exc_halt_s 2) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 2))))))))
 (let (($x740 (forall ((w (_ BitVec 256)) )(let ((?x5355 (storage_s x_0 x_1 x_2 2 w)))
 (let ((?x8583 (storage_s x_0 x_1 x_2 3 w)))
 (= ?x8583 ?x5355))))
 ))
 (let (($x5338 (forall ((n (_ BitVec 6)) )(let ((?x6852 (stack_s x_0 x_1 x_2 2 n)))
 (let ((?x10494 (stack_s x_0 x_1 x_2 3 n)))
 (let (($x586 (= ?x10494 ?x6852)))
 (or $x586 (bvsle (bvadd (_ bv62 6) (sc_s 2)) n))))))
 ))
 (let ((?x218 (sc_s 2)))
 (let ((?x9888 (bvadd (_ bv63 6) ?x218)))
 (let (($x2591 (= ?x275 ?x9888)))
 (let ((?x11474 (used_gas_s x_0 x_1 x_2 3)))
 (let (($x6823 (= ?x11474 (+ 3 (used_gas_s x_0 x_1 x_2 2)))))
 (let ((?x9614 (bvadd (_ bv62 6) ?x218)))
 (let ((?x9979 (stack_s x_0 x_1 x_2 2 ?x9614)))
 (let ((?x1623 (stack_s x_0 x_1 x_2 2 ?x9888)))
 (let (($x6006 (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 1))))))
 (let (($x3244 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 1)))) (_ bv0 1)))))
 (let (($x189 (exc_halt_s 1)))
 (let (($x247 (exc_halt_s 2)))
 (let (($x8365 (forall ((w (_ BitVec 256)) )(let ((?x8792 (storage_s x_0 x_1 x_2 1 w)))
 (let ((?x5355 (storage_s x_0 x_1 x_2 2 w)))
 (= ?x5355 ?x8792))))
 ))
 (let (($x10297 (forall ((n (_ BitVec 6)) )(let ((?x154 (sc_s 1)))
 (let ((?x6225 (bvadd (_ bv62 6) ?x154)))
 (let (($x6726 (bvsle ?x6225 n)))
 (let ((?x5906 (stack_s x_0 x_1 x_2 1 n)))
 (let ((?x6852 (stack_s x_0 x_1 x_2 2 n)))
 (let (($x4270 (= ?x6852 ?x5906)))
 (or $x4270 $x6726))))))))
 ))
 (let (($x2280 (= ?x218 (bvadd (_ bv1 6) (sc_s 1)))))
 (let ((?x4309 (used_gas_s x_0 x_1 x_2 2)))
 (let (($x11401 (= ?x4309 (+ 3 (used_gas_s x_0 x_1 x_2 1)))))
 (let ((?x154 (sc_s 1)))
 (let ((?x3500 (bvadd (_ bv63 6) ?x154)))
 (let ((?x11389 (stack_s x_0 x_1 x_2 1 ?x3500)))
 (let (($x7767 (= (stack_s x_0 x_1 x_2 2 ?x3500) ?x11389)))
 (let ((?x6225 (bvadd (_ bv62 6) ?x154)))
 (let ((?x8020 (stack_s x_0 x_1 x_2 1 ?x6225)))
 (let (($x4836 (= (stack_s x_0 x_1 x_2 2 ?x6225) ?x8020)))
 (let (($x9241 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x72))) (_ bv0 1)))))
 (let (($x7609 (forall ((w (_ BitVec 256)) )(let ((?x29 (storage_s x_0 x_1 x_2 0 w)))
 (let ((?x8792 (storage_s x_0 x_1 x_2 1 w)))
 (= ?x8792 ?x29))))
 ))
 (let (($x2695 (forall ((n (_ BitVec 6)) )(let ((?x3994 (stack_s x_0 x_1 x_2 0 n)))
 (let ((?x5906 (stack_s x_0 x_1 x_2 1 n)))
 (let (($x6745 (= ?x5906 ?x3994)))
 (or (bvsle (bvadd (_ bv62 6) (sc_s 0)) n) $x6745)))))
 ))
 (let (($x7319 (= ?x154 (bvadd (_ bv1 6) ?x72))))
 (let ((?x3857 (used_gas_s x_0 x_1 x_2 1)))
 (let (($x9988 (= ?x3857 (+ 3 ?x5033))))
 (let ((?x4777 (bvadd (_ bv63 6) ?x72)))
 (let ((?x5235 (stack_s x_0 x_1 x_2 0 ?x4777)))
 (let (($x3151 (= (stack_s x_0 x_1 x_2 1 ?x4777) ?x5235)))
 (let ((?x3397 (bvadd (_ bv62 6) ?x72)))
 (let ((?x6622 (stack_s x_0 x_1 x_2 0 ?x3397)))
 (let (($x7050 (forall ((w (_ BitVec 256)) )(let ((?x29 (storage_s x_0 x_1 x_2 0 w)))
 (= ?x29 (_ bv0 256))))
 ))
 (let (($x6256 (= ?x5033 0)))
 (let (($x1892 (not $x57)))
 (let (($x7810 (= (stack_s x_0 x_1 x_2 0 (_ bv2 6)) x_2)))
 (let (($x2828 (= (stack_s x_0 x_1 x_2 0 (_ bv1 6)) x_1)))
 (let (($x4499 (= (stack_s x_0 x_1 x_2 0 (_ bv0 6)) x_0)))
 (let (($x8477 (= ?x72 (_ bv3 6))))
 (and $x8477 $x4499 $x2828 $x7810 $x1892 $x6256 $x7050 (= ?x11389 ?x6622) (= (stack_s x_0 x_1 x_2 1 ?x3397) ?x6622) $x3151 $x9988 $x7319 $x2695 $x7609 (= $x189 (or $x57 (not (bvsle (_ bv0 6) ?x3397)) $x9241)) (= ?x1623 ?x8020) $x4836 $x7767 $x11401 $x2280 $x10297 $x8365 (= $x247 (or $x189 $x3244 $x6006)) (= ?x5446 (bvadd ?x1623 ?x9979)) $x6823 $x2591 $x5338 $x740 $x9813 (= ?x3333 ?x8070) (= (stack_s x_0 x_1 x_2 4 ?x855) ?x8070) $x6920 (= (stack_s x_0 x_1 x_2 4 ?x3946) ?x10101) (= (stack_s x_0 x_1 x_2 4 ?x4768) ?x5446) $x8332 $x4030 $x715 $x10046 (= $x7172 (or $x292 $x10211 (not (bvsle (_ bv0 6) ?x855)))) (= (stack_s x_0 x_1 x_2 5 (bvadd (_ bv63 6) ?x4319)) ?x5399) (= (stack_s x_0 x_1 x_2 5 (bvadd (_ bv62 6) ?x4319)) ?x3333) (= ?x74 (+ 3 ?x3426)) $x10003 $x1421 $x7203 $x8712 (= ?x8535 ?x8376) (= (stack_t x_0 x_1 x_2 1 ?x11452) ?x8376) $x6748 $x3927 $x10415 $x5986 $x8780 $x10152 (= $x8377 (or $x56 $x4780 $x6063)) (= ?x4437 ?x3751) (= (stack_t x_0 x_1 x_2 2 ?x10607) ?x3751) (= (stack_t x_0 x_1 x_2 2 ?x2642) ?x8535) (= ?x11061 (+ 3 ?x4619)) $x1589 $x7830 $x7376 (= $x2163 (or $x3121 $x7439 $x8377)) (= ?x2819 ?x6616) (= (stack_t x_0 x_1 x_2 3 ?x8658) ?x6616) $x11833 $x4033 (= (stack_t x_0 x_1 x_2 3 ?x6840) ?x4437) (= ?x6346 (+ 3 ?x11061)) $x11685 $x1167 $x4905 $x5518 $x9392 $x5911 $x4948 $x7374 $x7027 $x7977 $x73 $x7461 $x58 $x1120 $x8373 (not (and $x7147 $x232 $x3163 $x3726)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
 )
(check-sat)
(get-proof)
