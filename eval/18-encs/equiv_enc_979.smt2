; SWAP1 DUP2 SWAP1 DUP2 SWAP1 => DUP1 DUP1 SWAP3
(set-option :produce-proofs true)
; 
(set-info :status unknown)
(declare-fun storage_t ((_ BitVec 256) (_ BitVec 256) Int (_ BitVec 256)) (_ BitVec 256))
(declare-fun storage_s ((_ BitVec 256) (_ BitVec 256) Int (_ BitVec 256)) (_ BitVec 256))
(declare-fun exc_halt_t (Int) Bool)
(declare-fun exc_halt_s (Int) Bool)
(declare-fun stack_t ((_ BitVec 256) (_ BitVec 256) Int (_ BitVec 6)) (_ BitVec 256))
(declare-fun stack_s ((_ BitVec 256) (_ BitVec 256) Int (_ BitVec 6)) (_ BitVec 256))
(declare-fun sc_t (Int) (_ BitVec 6))
(declare-fun sc_s (Int) (_ BitVec 6))
(declare-fun used_gas_t ((_ BitVec 256) (_ BitVec 256) Int) Int)
(declare-fun used_gas_s ((_ BitVec 256) (_ BitVec 256) Int) Int)
(assert
 (exists ((x_0 (_ BitVec 256)) (x_1 (_ BitVec 256)) )(let (($x5244 (forall ((w (_ BitVec 256)) )(let ((?x1188 (storage_t x_0 x_1 3 w)))
 (let ((?x7645 (storage_s x_0 x_1 5 w)))
 (= ?x7645 ?x1188))))
 ))
 (let (($x5429 (exc_halt_t 3)))
 (let (($x1862 (exc_halt_s 5)))
 (let (($x3022 (= $x1862 $x5429)))
 (let (($x8233 (forall ((n (_ BitVec 6)) )(let ((?x11431 (stack_t x_0 x_1 3 n)))
 (let ((?x3753 (stack_s x_0 x_1 5 n)))
 (let (($x2851 (= ?x3753 ?x11431)))
 (let ((?x7621 (sc_t 3)))
 (let (($x438 (bvsle ?x7621 n)))
 (or $x438 $x2851)))))))
 ))
 (let ((?x7621 (sc_t 3)))
 (let ((?x4319 (sc_s 5)))
 (let (($x3981 (= ?x4319 ?x7621)))
 (let ((?x2063 (used_gas_t x_0 x_1 0)))
 (let ((?x3254 (used_gas_s x_0 x_1 0)))
 (let (($x10567 (= ?x3254 ?x2063)))
 (let (($x2518 (forall ((w (_ BitVec 256)) )(let ((?x6649 (storage_t x_0 x_1 0 w)))
 (let ((?x6970 (storage_s x_0 x_1 0 w)))
 (= ?x6970 ?x6649))))
 ))
 (let (($x56 (exc_halt_t 0)))
 (let (($x57 (exc_halt_s 0)))
 (let (($x58 (= $x57 $x56)))
 (let (($x9019 (forall ((n (_ BitVec 6)) )(let ((?x63 (sc_t 0)))
 (let (($x11318 (bvsle ?x63 n)))
 (let ((?x406 (stack_t x_0 x_1 0 n)))
 (let ((?x5147 (stack_s x_0 x_1 0 n)))
 (let (($x7562 (= ?x5147 ?x406)))
 (or $x7562 $x11318)))))))
 ))
 (let ((?x63 (sc_t 0)))
 (let ((?x72 (sc_s 0)))
 (let (($x73 (= ?x72 ?x63)))
 (let (($x5589 (= $x5429 (or (exc_halt_t 2) (not (bvsle (_ bv0 6) (bvadd (_ bv60 6) (sc_t 2))))))))
 (let (($x2623 (forall ((w (_ BitVec 256)) )(let ((?x9567 (storage_t x_0 x_1 2 w)))
 (let ((?x1188 (storage_t x_0 x_1 3 w)))
 (= ?x1188 ?x9567))))
 ))
 (let (($x5653 (forall ((n (_ BitVec 6)) )(let ((?x8890 (sc_t 2)))
 (let ((?x5041 (bvadd (_ bv60 6) ?x8890)))
 (let (($x6626 (bvsle ?x5041 n)))
 (or (= (stack_t x_0 x_1 3 n) (stack_t x_0 x_1 2 n)) $x6626)))))
 ))
 (let ((?x8890 (sc_t 2)))
 (let (($x5385 (= ?x7621 ?x8890)))
 (let (($x11554 (= (used_gas_t x_0 x_1 3) (+ 3 (used_gas_t x_0 x_1 2)))))
 (let (($x10025 (= (stack_t x_0 x_1 3 (bvadd (_ bv62 6) ?x7621)) (stack_t x_0 x_1 2 (bvadd (_ bv62 6) ?x8890)))))
 (let (($x10525 (= (stack_t x_0 x_1 3 (bvadd (_ bv61 6) ?x7621)) (stack_t x_0 x_1 2 (bvadd (_ bv61 6) ?x8890)))))
 (let ((?x6760 (bvadd (_ bv63 6) ?x8890)))
 (let ((?x3998 (stack_t x_0 x_1 2 ?x6760)))
 (let (($x10579 (= (stack_t x_0 x_1 3 (bvadd (_ bv63 6) ?x7621)) (stack_t x_0 x_1 2 (bvadd (_ bv60 6) ?x8890)))))
 (let (($x11985 (exc_halt_t 1)))
 (let (($x9667 (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_t 1))))))
 (let (($x808 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_t 1)))) (_ bv0 1)))))
 (let (($x7505 (exc_halt_t 2)))
 (let (($x871 (= $x7505 (or $x808 $x9667 $x11985))))
 (let (($x4390 (forall ((w (_ BitVec 256)) )(let ((?x2904 (storage_t x_0 x_1 1 w)))
 (let ((?x9567 (storage_t x_0 x_1 2 w)))
 (= ?x9567 ?x2904))))
 ))
 (let (($x2795 (forall ((n (_ BitVec 6)) )(let ((?x4554 (sc_t 1)))
 (let ((?x6802 (bvadd (_ bv63 6) ?x4554)))
 (let (($x293 (bvsle ?x6802 n)))
 (or $x293 (= (stack_t x_0 x_1 2 n) (stack_t x_0 x_1 1 n)))))))
 ))
 (let (($x422 (= ?x8890 (bvadd (_ bv1 6) (sc_t 1)))))
 (let ((?x1875 (used_gas_t x_0 x_1 2)))
 (let ((?x4554 (sc_t 1)))
 (let ((?x6802 (bvadd (_ bv63 6) ?x4554)))
 (let ((?x9959 (stack_t x_0 x_1 1 ?x6802)))
 (let (($x10245 (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) ?x63)))))
 (let (($x921 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x63))) (_ bv0 1)))))
 (let (($x5716 (= $x11985 (or $x56 $x921 $x10245))))
 (let (($x9378 (forall ((w (_ BitVec 256)) )(let ((?x6649 (storage_t x_0 x_1 0 w)))
 (let ((?x2904 (storage_t x_0 x_1 1 w)))
 (= ?x2904 ?x6649))))
 ))
 (let (($x8247 (forall ((n (_ BitVec 6)) )(let ((?x63 (sc_t 0)))
 (let ((?x3574 (bvadd (_ bv63 6) ?x63)))
 (let (($x6844 (bvsle ?x3574 n)))
 (or (= (stack_t x_0 x_1 1 n) (stack_t x_0 x_1 0 n)) $x6844)))))
 ))
 (let (($x2096 (= ?x4554 (bvadd (_ bv1 6) ?x63))))
 (let ((?x3574 (bvadd (_ bv63 6) ?x63)))
 (let ((?x4647 (stack_t x_0 x_1 0 ?x3574)))
 (let (($x9525 (= $x1862 (or (exc_halt_s 4) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 4))))))))
 (let (($x8501 (forall ((w (_ BitVec 256)) )(let ((?x10565 (storage_s x_0 x_1 4 w)))
 (let ((?x7645 (storage_s x_0 x_1 5 w)))
 (= ?x7645 ?x10565))))
 ))
 (let (($x3540 (forall ((n (_ BitVec 6)) )(let ((?x4287 (stack_s x_0 x_1 4 n)))
 (let ((?x3753 (stack_s x_0 x_1 5 n)))
 (let (($x9091 (= ?x3753 ?x4287)))
 (let ((?x9433 (sc_s 4)))
 (let ((?x11364 (bvadd (_ bv62 6) ?x9433)))
 (let (($x6445 (bvsle ?x11364 n)))
 (or $x6445 $x9091))))))))
 ))
 (let ((?x9433 (sc_s 4)))
 (let (($x1752 (= ?x4319 ?x9433)))
 (let ((?x2407 (used_gas_s x_0 x_1 5)))
 (let ((?x3395 (bvadd (_ bv63 6) ?x9433)))
 (let ((?x11629 (stack_s x_0 x_1 4 ?x3395)))
 (let (($x2680 (= (stack_s x_0 x_1 5 (bvadd (_ bv63 6) ?x4319)) (stack_s x_0 x_1 4 (bvadd (_ bv62 6) ?x9433)))))
 (let (($x10681 (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 3))))))
 (let (($x8103 (exc_halt_s 3)))
 (let (($x1963 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 3)))) (_ bv0 1)))))
 (let (($x9175 (exc_halt_s 4)))
 (let (($x10066 (= $x9175 (or $x1963 $x8103 $x10681))))
 (let (($x2088 (forall ((w (_ BitVec 256)) )(let ((?x1287 (storage_s x_0 x_1 3 w)))
 (let ((?x10565 (storage_s x_0 x_1 4 w)))
 (= ?x10565 ?x1287))))
 ))
 (let (($x1031 (forall ((n (_ BitVec 6)) )(let ((?x1994 (stack_s x_0 x_1 3 n)))
 (let ((?x4287 (stack_s x_0 x_1 4 n)))
 (let (($x3074 (= ?x4287 ?x1994)))
 (let ((?x3851 (sc_s 3)))
 (let ((?x8089 (bvadd (_ bv62 6) ?x3851)))
 (let (($x1497 (bvsle ?x8089 n)))
 (or $x1497 $x3074))))))))
 ))
 (let (($x2843 (= ?x9433 (bvadd (_ bv1 6) (sc_s 3)))))
 (let ((?x1542 (used_gas_s x_0 x_1 4)))
 (let ((?x3851 (sc_s 3)))
 (let ((?x1738 (bvadd (_ bv63 6) ?x3851)))
 (let ((?x7493 (stack_s x_0 x_1 3 ?x1738)))
 (let ((?x8089 (bvadd (_ bv62 6) ?x3851)))
 (let ((?x5933 (stack_s x_0 x_1 3 ?x8089)))
 (let (($x4380 (= $x8103 (or (exc_halt_s 2) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 2))))))))
 (let (($x6555 (forall ((w (_ BitVec 256)) )(let ((?x1853 (storage_s x_0 x_1 2 w)))
 (let ((?x1287 (storage_s x_0 x_1 3 w)))
 (= ?x1287 ?x1853))))
 ))
 (let (($x11574 (forall ((n (_ BitVec 6)) )(let ((?x2272 (sc_s 2)))
 (let ((?x7947 (bvadd (_ bv62 6) ?x2272)))
 (let (($x7062 (bvsle ?x7947 n)))
 (or (= (stack_s x_0 x_1 3 n) (stack_s x_0 x_1 2 n)) $x7062)))))
 ))
 (let ((?x2272 (sc_s 2)))
 (let (($x5381 (= ?x3851 ?x2272)))
 (let ((?x2169 (used_gas_s x_0 x_1 3)))
 (let (($x613 (= ?x2169 (+ 3 (used_gas_s x_0 x_1 2)))))
 (let ((?x9000 (bvadd (_ bv63 6) ?x2272)))
 (let ((?x7282 (stack_s x_0 x_1 2 ?x9000)))
 (let (($x5592 (= ?x5933 ?x7282)))
 (let (($x2837 (= ?x7493 (stack_s x_0 x_1 2 (bvadd (_ bv62 6) ?x2272)))))
 (let (($x8780 (exc_halt_s 1)))
 (let (($x4393 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 1)))) (_ bv0 1)))))
 (let (($x1302 (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 1))))))
 (let (($x10052 (exc_halt_s 2)))
 (let (($x5382 (= $x10052 (or $x1302 $x4393 $x8780))))
 (let (($x11427 (forall ((w (_ BitVec 256)) )(let ((?x9110 (storage_s x_0 x_1 1 w)))
 (let ((?x1853 (storage_s x_0 x_1 2 w)))
 (= ?x1853 ?x9110))))
 ))
 (let (($x304 (forall ((n (_ BitVec 6)) )(let ((?x7753 (stack_s x_0 x_1 1 n)))
 (let ((?x1144 (stack_s x_0 x_1 2 n)))
 (let (($x8670 (= ?x1144 ?x7753)))
 (let ((?x154 (sc_s 1)))
 (let ((?x8244 (bvadd (_ bv62 6) ?x154)))
 (let (($x6954 (bvsle ?x8244 n)))
 (or $x6954 $x8670))))))))
 ))
 (let (($x9417 (= ?x2272 (bvadd (_ bv1 6) (sc_s 1)))))
 (let ((?x8361 (used_gas_s x_0 x_1 2)))
 (let (($x6519 (= ?x8361 (+ 3 (used_gas_s x_0 x_1 1)))))
 (let ((?x154 (sc_s 1)))
 (let ((?x1791 (bvadd (_ bv63 6) ?x154)))
 (let ((?x403 (stack_s x_0 x_1 1 ?x1791)))
 (let ((?x8244 (bvadd (_ bv62 6) ?x154)))
 (let ((?x2455 (stack_s x_0 x_1 1 ?x8244)))
 (let (($x7140 (= $x8780 (or $x57 (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) ?x72)))))))
 (let (($x7785 (forall ((w (_ BitVec 256)) )(let ((?x6970 (storage_s x_0 x_1 0 w)))
 (let ((?x9110 (storage_s x_0 x_1 1 w)))
 (= ?x9110 ?x6970))))
 ))
 (let (($x8330 (forall ((n (_ BitVec 6)) )(let ((?x5147 (stack_s x_0 x_1 0 n)))
 (let ((?x7753 (stack_s x_0 x_1 1 n)))
 (let (($x8018 (= ?x7753 ?x5147)))
 (let ((?x72 (sc_s 0)))
 (let ((?x10468 (bvadd (_ bv62 6) ?x72)))
 (let (($x2129 (bvsle ?x10468 n)))
 (or $x2129 $x8018))))))))
 ))
 (let (($x9780 (= ?x154 ?x72)))
 (let ((?x3292 (used_gas_s x_0 x_1 1)))
 (let (($x7969 (= ?x3292 (+ 3 ?x3254))))
 (let (($x6704 (forall ((w (_ BitVec 256)) )(let ((?x6970 (storage_s x_0 x_1 0 w)))
 (= ?x6970 (_ bv0 256))))
 ))
 (let (($x11019 (= ?x3254 0)))
 (let (($x3971 (not $x57)))
 (let (($x4491 (= (stack_s x_0 x_1 0 (_ bv1 6)) x_1)))
 (let (($x520 (= (stack_s x_0 x_1 0 (_ bv0 6)) x_0)))
 (let (($x2075 (= ?x72 (_ bv2 6))))
 (and $x2075 $x520 $x4491 $x3971 $x11019 $x6704 (= ?x403 (stack_s x_0 x_1 0 (bvadd (_ bv62 6) ?x72))) (= ?x2455 (stack_s x_0 x_1 0 (bvadd (_ bv63 6) ?x72))) $x7969 $x9780 $x8330 $x7785 $x7140 (= ?x7282 ?x2455) (= (stack_s x_0 x_1 2 ?x8244) ?x2455) (= (stack_s x_0 x_1 2 ?x1791) ?x403) $x6519 $x9417 $x304 $x11427 $x5382 $x2837 $x5592 $x613 $x5381 $x11574 $x6555 $x4380 (= ?x11629 ?x5933) (= (stack_s x_0 x_1 4 ?x8089) ?x5933) (= (stack_s x_0 x_1 4 ?x1738) ?x7493) (= ?x1542 (+ 3 ?x2169)) $x2843 $x1031 $x2088 $x10066 $x2680 (= (stack_s x_0 x_1 5 (bvadd (_ bv62 6) ?x4319)) ?x11629) (= ?x2407 (+ 3 ?x1542)) $x1752 $x3540 $x8501 $x9525 (= ?x9959 ?x4647) (= (stack_t x_0 x_1 1 ?x3574) ?x4647) (= (used_gas_t x_0 x_1 1) (+ 3 ?x2063)) $x2096 $x8247 $x9378 $x5716 (= ?x3998 ?x9959) (= (stack_t x_0 x_1 2 ?x6802) ?x9959) (= ?x1875 (+ 3 (used_gas_t x_0 x_1 1))) $x422 $x2795 $x4390 $x871 $x10579 (= (stack_t x_0 x_1 3 (bvadd (_ bv60 6) ?x7621)) ?x3998) $x10525 $x10025 $x11554 $x5385 $x5653 $x2623 $x5589 $x73 $x9019 $x58 $x2518 $x10567 (not (and $x3981 $x8233 $x3022 $x5244)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
 )
(check-sat)
(get-proof)
