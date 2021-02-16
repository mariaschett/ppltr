; NOT NOT EQ SWAP2 POP POP => EQ SWAP2 SLT POP
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
 (exists ((x_0 (_ BitVec 256)) (x_1 (_ BitVec 256)) (x_2 (_ BitVec 256)) (x_3 (_ BitVec 256)) )(let (($x9017 (forall ((w (_ BitVec 256)) )(let ((?x6105 (storage_t x_0 x_1 x_2 x_3 4 w)))
 (let ((?x6364 (storage_s x_0 x_1 x_2 x_3 6 w)))
 (= ?x6364 ?x6105))))
 ))
 (let (($x8747 (exc_halt_t 4)))
 (let (($x772 (exc_halt_s 6)))
 (let (($x3489 (= $x772 $x8747)))
 (let (($x3496 (forall ((n (_ BitVec 6)) )(let ((?x6088 (stack_t x_0 x_1 x_2 x_3 4 n)))
 (let ((?x6282 (stack_s x_0 x_1 x_2 x_3 6 n)))
 (let (($x9030 (= ?x6282 ?x6088)))
 (let ((?x9469 (sc_t 4)))
 (let (($x3598 (bvsle ?x9469 n)))
 (or $x3598 $x9030)))))))
 ))
 (let ((?x9469 (sc_t 4)))
 (let ((?x926 (sc_s 6)))
 (let (($x3536 (= ?x926 ?x9469)))
 (let ((?x6269 (used_gas_t x_0 x_1 x_2 x_3 0)))
 (let ((?x6947 (used_gas_s x_0 x_1 x_2 x_3 0)))
 (let (($x6909 (= ?x6947 ?x6269)))
 (let (($x6911 (forall ((w (_ BitVec 256)) )(let ((?x6254 (storage_t x_0 x_1 x_2 x_3 0 w)))
 (let ((?x6786 (storage_s x_0 x_1 x_2 x_3 0 w)))
 (= ?x6786 ?x6254))))
 ))
 (let (($x56 (exc_halt_t 0)))
 (let (($x57 (exc_halt_s 0)))
 (let (($x58 (= $x57 $x56)))
 (let (($x6971 (forall ((n (_ BitVec 6)) )(let ((?x6215 (stack_t x_0 x_1 x_2 x_3 0 n)))
 (let ((?x8632 (stack_s x_0 x_1 x_2 x_3 0 n)))
 (let (($x6216 (= ?x8632 ?x6215)))
 (let ((?x63 (sc_t 0)))
 (let (($x3675 (bvsle ?x63 n)))
 (or $x3675 $x6216)))))))
 ))
 (let ((?x63 (sc_t 0)))
 (let ((?x72 (sc_s 0)))
 (let (($x73 (= ?x72 ?x63)))
 (let (($x3274 (= $x8747 (or (exc_halt_t 3) (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_t 3))))))))
 (let (($x6560 (forall ((w (_ BitVec 256)) )(let ((?x6161 (storage_t x_0 x_1 x_2 x_3 3 w)))
 (let ((?x6105 (storage_t x_0 x_1 x_2 x_3 4 w)))
 (= ?x6105 ?x6161))))
 ))
 (let (($x1994 (forall ((n (_ BitVec 6)) )(let ((?x6030 (stack_t x_0 x_1 x_2 x_3 3 n)))
 (let ((?x6088 (stack_t x_0 x_1 x_2 x_3 4 n)))
 (let (($x6580 (= ?x6088 ?x6030)))
 (let ((?x2012 (sc_t 3)))
 (let ((?x3673 (bvadd (_ bv63 6) ?x2012)))
 (let (($x3245 (bvsle ?x3673 n)))
 (or $x3245 $x6580))))))))
 ))
 (let ((?x2012 (sc_t 3)))
 (let ((?x3673 (bvadd (_ bv63 6) ?x2012)))
 (let (($x8028 (= ?x9469 ?x3673)))
 (let ((?x6101 (used_gas_t x_0 x_1 x_2 x_3 4)))
 (let (($x8030 (exc_halt_t 3)))
 (let (($x8029 (= $x8030 (or (exc_halt_t 2) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_t 2))))))))
 (let (($x6595 (forall ((w (_ BitVec 256)) )(let ((?x5977 (storage_t x_0 x_1 x_2 x_3 2 w)))
 (let ((?x6161 (storage_t x_0 x_1 x_2 x_3 3 w)))
 (= ?x6161 ?x5977))))
 ))
 (let (($x5573 (forall ((n (_ BitVec 6)) )(let ((?x4056 (sc_t 2)))
 (let ((?x2229 (bvadd (_ bv62 6) ?x4056)))
 (let (($x8323 (bvsle ?x2229 n)))
 (let ((?x5662 (stack_t x_0 x_1 x_2 x_3 2 n)))
 (let ((?x6030 (stack_t x_0 x_1 x_2 x_3 3 n)))
 (let (($x6614 (= ?x6030 ?x5662)))
 (or $x6614 $x8323))))))))
 ))
 (let ((?x4056 (sc_t 2)))
 (let ((?x7929 (bvadd (_ bv63 6) ?x4056)))
 (let (($x5770 (= ?x2012 ?x7929)))
 (let ((?x5887 (used_gas_t x_0 x_1 x_2 x_3 3)))
 (let (($x6603 (= ?x5887 (+ 3 (used_gas_t x_0 x_1 x_2 x_3 2)))))
 (let ((?x6642 (stack_t x_0 x_1 x_2 x_3 2 ?x7929)))
 (let ((?x2229 (bvadd (_ bv62 6) ?x4056)))
 (let ((?x6653 (stack_t x_0 x_1 x_2 x_3 2 ?x2229)))
 (let ((?x6623 (stack_t x_0 x_1 x_2 x_3 3 ?x3673)))
 (let (($x903 (exc_halt_t 2)))
 (let (($x9151 (= $x903 (or (exc_halt_t 1) (not (bvsle (_ bv0 6) (bvadd (_ bv61 6) (sc_t 1))))))))
 (let (($x6630 (forall ((w (_ BitVec 256)) )(let ((?x5466 (storage_t x_0 x_1 x_2 x_3 1 w)))
 (let ((?x5977 (storage_t x_0 x_1 x_2 x_3 2 w)))
 (= ?x5977 ?x5466))))
 ))
 (let (($x9384 (forall ((n (_ BitVec 6)) )(let ((?x5554 (stack_t x_0 x_1 x_2 x_3 1 n)))
 (let ((?x5662 (stack_t x_0 x_1 x_2 x_3 2 n)))
 (let (($x6635 (= ?x5662 ?x5554)))
 (or $x6635 (bvsle (bvadd (_ bv61 6) (sc_t 1)) n))))))
 ))
 (let ((?x4023 (sc_t 1)))
 (let (($x2318 (= ?x4056 ?x4023)))
 (let ((?x5661 (used_gas_t x_0 x_1 x_2 x_3 2)))
 (let (($x6648 (= ?x5661 (+ 3 (used_gas_t x_0 x_1 x_2 x_3 1)))))
 (let ((?x4102 (bvadd (_ bv62 6) ?x4023)))
 (let ((?x6641 (stack_t x_0 x_1 x_2 x_3 1 ?x4102)))
 (let (($x7811 (= ?x6653 ?x6641)))
 (let ((?x2639 (bvadd (_ bv63 6) ?x4023)))
 (let ((?x6700 (stack_t x_0 x_1 x_2 x_3 1 ?x2639)))
 (let ((?x2159 (bvadd (_ bv61 6) ?x4056)))
 (let ((?x6619 (stack_t x_0 x_1 x_2 x_3 2 ?x2159)))
 (let (($x6676 (forall ((w (_ BitVec 256)) )(let ((?x6254 (storage_t x_0 x_1 x_2 x_3 0 w)))
 (let ((?x5466 (storage_t x_0 x_1 x_2 x_3 1 w)))
 (= ?x5466 ?x6254))))
 ))
 (let (($x6679 (forall ((n (_ BitVec 6)) )(let ((?x6215 (stack_t x_0 x_1 x_2 x_3 0 n)))
 (let ((?x5554 (stack_t x_0 x_1 x_2 x_3 1 n)))
 (let (($x6667 (= ?x5554 ?x6215)))
 (or $x6667 (bvsle (bvadd (_ bv62 6) (sc_t 0)) n))))))
 ))
 (let ((?x6829 (used_gas_t x_0 x_1 x_2 x_3 1)))
 (let (($x6663 (= ?x6829 (+ 3 ?x6269))))
 (let ((?x1133 (bvadd (_ bv62 6) ?x63)))
 (let ((?x6692 (stack_t x_0 x_1 x_2 x_3 0 ?x1133)))
 (let ((?x129 (bvadd (_ bv63 6) ?x63)))
 (let ((?x6669 (stack_t x_0 x_1 x_2 x_3 0 ?x129)))
 (let (($x6729 (= $x772 (or (exc_halt_s 5) (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_s 5))))))))
 (let (($x6683 (forall ((w (_ BitVec 256)) )(let ((?x6350 (storage_s x_0 x_1 x_2 x_3 5 w)))
 (let ((?x6364 (storage_s x_0 x_1 x_2 x_3 6 w)))
 (= ?x6364 ?x6350))))
 ))
 (let (($x6689 (forall ((n (_ BitVec 6)) )(let ((?x6343 (stack_s x_0 x_1 x_2 x_3 5 n)))
 (let ((?x6282 (stack_s x_0 x_1 x_2 x_3 6 n)))
 (or (= ?x6282 ?x6343) (bvsle (bvadd (_ bv63 6) (sc_s 5)) n)))))
 ))
 (let ((?x805 (sc_s 5)))
 (let ((?x930 (bvadd (_ bv63 6) ?x805)))
 (let (($x1341 (= ?x926 ?x930)))
 (let ((?x6709 (used_gas_s x_0 x_1 x_2 x_3 6)))
 (let (($x3979 (exc_halt_s 5)))
 (let (($x2296 (= $x3979 (or (exc_halt_s 4) (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_s 4))))))))
 (let (($x6716 (forall ((w (_ BitVec 256)) )(let ((?x6325 (storage_s x_0 x_1 x_2 x_3 4 w)))
 (let ((?x6350 (storage_s x_0 x_1 x_2 x_3 5 w)))
 (= ?x6350 ?x6325))))
 ))
 (let (($x6728 (forall ((n (_ BitVec 6)) )(let ((?x6314 (stack_s x_0 x_1 x_2 x_3 4 n)))
 (let ((?x6343 (stack_s x_0 x_1 x_2 x_3 5 n)))
 (let (($x6743 (= ?x6343 ?x6314)))
 (let ((?x4305 (sc_s 4)))
 (let ((?x374 (bvadd (_ bv63 6) ?x4305)))
 (let (($x2204 (bvsle ?x374 n)))
 (or $x2204 $x6743))))))))
 ))
 (let ((?x4305 (sc_s 4)))
 (let ((?x374 (bvadd (_ bv63 6) ?x4305)))
 (let (($x2186 (= ?x805 ?x374)))
 (let ((?x7057 (used_gas_s x_0 x_1 x_2 x_3 5)))
 (let (($x64 (exc_halt_s 4)))
 (let (($x9400 (= $x64 (or (exc_halt_s 3) (not (bvsle (_ bv0 6) (bvadd (_ bv61 6) (sc_s 3))))))))
 (let (($x6730 (forall ((w (_ BitVec 256)) )(let ((?x7177 (storage_s x_0 x_1 x_2 x_3 3 w)))
 (let ((?x6325 (storage_s x_0 x_1 x_2 x_3 4 w)))
 (= ?x6325 ?x7177))))
 ))
 (let (($x3183 (forall ((n (_ BitVec 6)) )(let ((?x6308 (stack_s x_0 x_1 x_2 x_3 3 n)))
 (let ((?x6314 (stack_s x_0 x_1 x_2 x_3 4 n)))
 (let (($x6736 (= ?x6314 ?x6308)))
 (or $x6736 (bvsle (bvadd (_ bv61 6) (sc_s 3)) n))))))
 ))
 (let ((?x275 (sc_s 3)))
 (let (($x1228 (= ?x4305 ?x275)))
 (let ((?x6298 (used_gas_s x_0 x_1 x_2 x_3 4)))
 (let (($x6747 (= ?x6298 (+ 3 (used_gas_s x_0 x_1 x_2 x_3 3)))))
 (let ((?x2605 (bvadd (_ bv62 6) ?x275)))
 (let ((?x7350 (stack_s x_0 x_1 x_2 x_3 3 ?x2605)))
 (let ((?x3773 (bvadd (_ bv62 6) ?x4305)))
 (let ((?x7468 (stack_s x_0 x_1 x_2 x_3 4 ?x3773)))
 (let ((?x3193 (bvadd (_ bv63 6) ?x275)))
 (let ((?x1944 (stack_s x_0 x_1 x_2 x_3 3 ?x3193)))
 (let ((?x10003 (bvadd (_ bv61 6) ?x4305)))
 (let ((?x7488 (stack_s x_0 x_1 x_2 x_3 4 ?x10003)))
 (let ((?x223 (bvadd (_ bv61 6) ?x275)))
 (let ((?x6780 (stack_s x_0 x_1 x_2 x_3 3 ?x223)))
 (let ((?x6740 (stack_s x_0 x_1 x_2 x_3 4 ?x374)))
 (let (($x7022 (= ?x6740 ?x6780)))
 (let (($x292 (exc_halt_s 3)))
 (let (($x1141 (= $x292 (or (exc_halt_s 2) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 2))))))))
 (let (($x6757 (forall ((w (_ BitVec 256)) )(let ((?x6287 (storage_s x_0 x_1 x_2 x_3 2 w)))
 (let ((?x7177 (storage_s x_0 x_1 x_2 x_3 3 w)))
 (= ?x7177 ?x6287))))
 ))
 (let (($x7023 (forall ((n (_ BitVec 6)) )(let ((?x6271 (stack_s x_0 x_1 x_2 x_3 2 n)))
 (let ((?x6308 (stack_s x_0 x_1 x_2 x_3 3 n)))
 (let (($x6438 (= ?x6308 ?x6271)))
 (let ((?x218 (sc_s 2)))
 (let ((?x3182 (bvadd (_ bv62 6) ?x218)))
 (let (($x706 (bvsle ?x3182 n)))
 (or $x706 $x6438))))))))
 ))
 (let ((?x218 (sc_s 2)))
 (let ((?x3831 (bvadd (_ bv63 6) ?x218)))
 (let (($x7401 (= ?x275 ?x3831)))
 (let ((?x6293 (used_gas_s x_0 x_1 x_2 x_3 3)))
 (let (($x7005 (= ?x6293 (+ 3 (used_gas_s x_0 x_1 x_2 x_3 2)))))
 (let ((?x3182 (bvadd (_ bv62 6) ?x218)))
 (let ((?x2470 (stack_s x_0 x_1 x_2 x_3 2 ?x3182)))
 (let ((?x6982 (stack_s x_0 x_1 x_2 x_3 2 ?x3831)))
 (let (($x247 (exc_halt_s 2)))
 (let (($x3844 (= $x247 (or (exc_halt_s 1) (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_s 1))))))))
 (let (($x6996 (forall ((w (_ BitVec 256)) )(let ((?x6240 (storage_s x_0 x_1 x_2 x_3 1 w)))
 (let ((?x6287 (storage_s x_0 x_1 x_2 x_3 2 w)))
 (= ?x6287 ?x6240))))
 ))
 (let (($x9053 (forall ((n (_ BitVec 6)) )(let ((?x6229 (stack_s x_0 x_1 x_2 x_3 1 n)))
 (let ((?x6271 (stack_s x_0 x_1 x_2 x_3 2 n)))
 (let (($x6994 (= ?x6271 ?x6229)))
 (let ((?x154 (sc_s 1)))
 (let ((?x3842 (bvadd (_ bv63 6) ?x154)))
 (let (($x2311 (bvsle ?x3842 n)))
 (or $x2311 $x6994))))))))
 ))
 (let ((?x154 (sc_s 1)))
 (let (($x3835 (= ?x218 ?x154)))
 (let ((?x7201 (used_gas_s x_0 x_1 x_2 x_3 2)))
 (let (($x6991 (= ?x7201 (+ 3 (used_gas_s x_0 x_1 x_2 x_3 1)))))
 (let ((?x3842 (bvadd (_ bv63 6) ?x154)))
 (let ((?x6134 (stack_s x_0 x_1 x_2 x_3 1 ?x3842)))
 (let ((?x7860 (bvnot ?x6134)))
 (let (($x6988 (forall ((w (_ BitVec 256)) )(let ((?x6786 (storage_s x_0 x_1 x_2 x_3 0 w)))
 (let ((?x6240 (storage_s x_0 x_1 x_2 x_3 1 w)))
 (= ?x6240 ?x6786))))
 ))
 (let (($x3206 (forall ((n (_ BitVec 6)) )(let ((?x8632 (stack_s x_0 x_1 x_2 x_3 0 n)))
 (let ((?x6229 (stack_s x_0 x_1 x_2 x_3 1 n)))
 (let (($x6985 (= ?x6229 ?x8632)))
 (let ((?x72 (sc_s 0)))
 (let ((?x3338 (bvadd (_ bv63 6) ?x72)))
 (let (($x3207 (bvsle ?x3338 n)))
 (or $x3207 $x6985))))))))
 ))
 (let (($x6404 (= ?x154 ?x72)))
 (let ((?x6228 (used_gas_s x_0 x_1 x_2 x_3 1)))
 (let (($x6406 (= ?x6228 (+ 3 ?x6947))))
 (let (($x9446 (= ?x6134 (bvnot (stack_s x_0 x_1 x_2 x_3 0 (bvadd (_ bv63 6) ?x72))))))
 (let (($x883 (forall ((w (_ BitVec 256)) )(let ((?x6786 (storage_s x_0 x_1 x_2 x_3 0 w)))
 (= ?x6786 (_ bv0 256))))
 ))
 (let (($x9996 (= ?x6947 0)))
 (let (($x3853 (not $x57)))
 (let (($x7465 (= (stack_s x_0 x_1 x_2 x_3 0 (_ bv3 6)) x_3)))
 (let (($x6778 (= (stack_s x_0 x_1 x_2 x_3 0 (_ bv2 6)) x_2)))
 (let (($x6163 (= (stack_s x_0 x_1 x_2 x_3 0 (_ bv1 6)) x_1)))
 (let (($x6196 (= (stack_s x_0 x_1 x_2 x_3 0 (_ bv0 6)) x_0)))
 (let (($x3266 (= ?x72 (_ bv4 6))))
 (and $x3266 $x6196 $x6163 $x6778 $x7465 $x3853 $x9996 $x883 $x9446 $x6406 $x6404 $x3206 $x6988 (= (exc_halt_s 1) (or $x57 (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) ?x72))))) (= ?x6982 ?x7860) $x6991 $x3835 $x9053 $x6996 $x3844 (= ?x1944 (ite (= ?x6982 ?x2470) (_ bv1 256) (_ bv0 256))) $x7005 $x7401 $x7023 $x6757 $x1141 $x7022 (= ?x7488 ?x1944) (= ?x7468 ?x7350) $x6747 $x1228 $x3183 $x6730 $x9400 (= ?x7057 (+ 2 ?x6298)) $x2186 $x6728 $x6716 $x2296 (= ?x6709 (+ 2 ?x7057)) $x1341 $x6689 $x6683 $x6729 (= ?x6700 (ite (= ?x6669 ?x6692) (_ bv1 256) (_ bv0 256))) $x6663 (= ?x4023 ?x129) $x6679 $x6676 (= (exc_halt_t 1) (or $x56 (not (bvsle (_ bv0 6) ?x1133)))) (= ?x6642 (stack_t x_0 x_1 x_2 x_3 1 (bvadd (_ bv61 6) ?x4023))) (= ?x6619 ?x6700) $x7811 $x6648 $x2318 $x9384 $x6630 $x9151 (= ?x6623 (ite (bvsle ?x6653 ?x6642) (_ bv0 256) (_ bv1 256))) $x6603 $x5770 $x5573 $x6595 $x8029 (= ?x6101 (+ 2 ?x5887)) $x8028 $x1994 $x6560 $x3274 $x73 $x6971 $x58 $x6911 $x6909 (not (and $x3536 $x3496 $x3489 $x9017))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
 )
(check-sat)
(get-proof)