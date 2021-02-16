; PUSH cw_1 DUP2 SHA3 SWAP2 POP POP => SWAP1 POP PUSH cw_1 SWAP1 SHA3
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
(declare-fun f_SHA3 ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256)) (_ BitVec 256))
(assert
 (exists ((x_0 (_ BitVec 256)) (x_1 (_ BitVec 256)) (x_SHA3_0 (_ BitVec 256)) )(let (($x1439 (forall ((w (_ BitVec 256)) )(let ((?x2098 (storage_t x_0 x_1 w_1 x_SHA3_0 5 w)))
 (let ((?x2215 (storage_s x_0 x_1 w_1 x_SHA3_0 6 w)))
 (= ?x2215 ?x2098))))
 ))
 (let (($x886 (exc_halt_t 5)))
 (let (($x772 (exc_halt_s 6)))
 (let (($x889 (= $x772 $x886)))
 (let (($x2117 (forall ((n (_ BitVec 6)) )(let ((?x7282 (stack_t x_0 x_1 w_1 x_SHA3_0 5 n)))
 (let ((?x9145 (stack_s x_0 x_1 w_1 x_SHA3_0 6 n)))
 (let (($x7873 (= ?x9145 ?x7282)))
 (or (bvsle (sc_t 5) n) $x7873)))))
 ))
 (let ((?x919 (sc_t 5)))
 (let ((?x926 (sc_s 6)))
 (let (($x929 (= ?x926 ?x919)))
 (let ((?x1306 (used_gas_t x_0 x_1 w_1 x_SHA3_0 0)))
 (let ((?x1170 (used_gas_s x_0 x_1 w_1 x_SHA3_0 0)))
 (let (($x3460 (= ?x1170 ?x1306)))
 (let (($x5597 (forall ((w (_ BitVec 256)) )(let ((?x5421 (storage_t x_0 x_1 w_1 x_SHA3_0 0 w)))
 (let ((?x8045 (storage_s x_0 x_1 w_1 x_SHA3_0 0 w)))
 (= ?x8045 ?x5421))))
 ))
 (let (($x56 (exc_halt_t 0)))
 (let (($x57 (exc_halt_s 0)))
 (let (($x58 (= $x57 $x56)))
 (let (($x335 (forall ((n (_ BitVec 6)) )(let ((?x63 (sc_t 0)))
 (let (($x2733 (bvsle ?x63 n)))
 (let ((?x6712 (stack_t x_0 x_1 w_1 x_SHA3_0 0 n)))
 (let ((?x9055 (stack_s x_0 x_1 w_1 x_SHA3_0 0 n)))
 (let (($x2082 (= ?x9055 ?x6712)))
 (or $x2082 $x2733)))))))
 ))
 (let ((?x63 (sc_t 0)))
 (let ((?x72 (sc_s 0)))
 (let (($x73 (= ?x72 ?x63)))
 (let (($x2890 (= $x886 (or (exc_halt_t 4) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_t 4))))))))
 (let (($x1673 (forall ((w (_ BitVec 256)) )(let ((?x1526 (storage_t x_0 x_1 w_1 x_SHA3_0 4 w)))
 (let ((?x2098 (storage_t x_0 x_1 w_1 x_SHA3_0 5 w)))
 (= ?x2098 ?x1526))))
 ))
 (let (($x1717 (forall ((n (_ BitVec 6)) )(let ((?x609 (stack_t x_0 x_1 w_1 x_SHA3_0 4 n)))
 (let ((?x7282 (stack_t x_0 x_1 w_1 x_SHA3_0 5 n)))
 (or (= ?x7282 ?x609) (bvsle (bvadd (_ bv62 6) (sc_t 4)) n)))))
 ))
 (let (($x9297 (= (used_gas_t x_0 x_1 w_1 x_SHA3_0 5) (+ 30 (used_gas_t x_0 x_1 w_1 x_SHA3_0 4)))))
 (let ((?x3757 (sc_t 4)))
 (let ((?x8912 (bvadd (_ bv62 6) ?x3757)))
 (let ((?x5308 (stack_t x_0 x_1 w_1 x_SHA3_0 4 ?x8912)))
 (let ((?x143 (bvadd (_ bv63 6) ?x3757)))
 (let ((?x955 (stack_t x_0 x_1 w_1 x_SHA3_0 4 ?x143)))
 (let (($x5771 (= (stack_t x_0 x_1 w_1 x_SHA3_0 5 (bvadd (_ bv63 6) ?x919)) (f_SHA3 x_0 x_1 w_1 x_SHA3_0 ?x955 ?x5308))))
 (let (($x3723 (exc_halt_t 4)))
 (let (($x5439 (= $x3723 (or (exc_halt_t 3) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_t 3))))))))
 (let (($x2229 (forall ((w (_ BitVec 256)) )(let ((?x5402 (storage_t x_0 x_1 w_1 x_SHA3_0 3 w)))
 (let ((?x1526 (storage_t x_0 x_1 w_1 x_SHA3_0 4 w)))
 (= ?x1526 ?x5402))))
 ))
 (let (($x7892 (forall ((n (_ BitVec 6)) )(let ((?x6194 (stack_t x_0 x_1 w_1 x_SHA3_0 3 n)))
 (let ((?x609 (stack_t x_0 x_1 w_1 x_SHA3_0 4 n)))
 (or (bvsle (bvadd (_ bv62 6) (sc_t 3)) n) (= ?x609 ?x6194)))))
 ))
 (let ((?x8231 (used_gas_t x_0 x_1 w_1 x_SHA3_0 4)))
 (let (($x1393 (= ?x5308 (stack_t x_0 x_1 w_1 x_SHA3_0 3 (bvadd (_ bv63 6) (sc_t 3))))))
 (let (($x7414 (= ?x955 (stack_t x_0 x_1 w_1 x_SHA3_0 3 (bvadd (_ bv62 6) (sc_t 3))))))
 (let (($x8512 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_t 2)))) (_ bv0 1)))))
 (let (($x903 (exc_halt_t 2)))
 (let (($x10336 (exc_halt_t 3)))
 (let (($x4986 (= $x10336 (or $x903 $x8512))))
 (let (($x5708 (forall ((w (_ BitVec 256)) )(let ((?x5883 (storage_t x_0 x_1 w_1 x_SHA3_0 2 w)))
 (let ((?x5402 (storage_t x_0 x_1 w_1 x_SHA3_0 3 w)))
 (= ?x5402 ?x5883))))
 ))
 (let (($x5753 (forall ((n (_ BitVec 6)) )(let ((?x2498 (stack_t x_0 x_1 w_1 x_SHA3_0 2 n)))
 (let ((?x6194 (stack_t x_0 x_1 w_1 x_SHA3_0 3 n)))
 (let ((?x4056 (sc_t 2)))
 (let (($x8328 (bvsle ?x4056 n)))
 (or $x8328 (= ?x6194 ?x2498)))))))
 ))
 (let ((?x2012 (sc_t 3)))
 (let (($x5064 (= ?x2012 (bvadd (_ bv1 6) (sc_t 2)))))
 (let ((?x10423 (used_gas_t x_0 x_1 w_1 x_SHA3_0 3)))
 (let (($x2625 (= $x903 (or (exc_halt_t 1) (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_t 1))))))))
 (let (($x1788 (forall ((w (_ BitVec 256)) )(let ((?x9245 (storage_t x_0 x_1 w_1 x_SHA3_0 1 w)))
 (let ((?x5883 (storage_t x_0 x_1 w_1 x_SHA3_0 2 w)))
 (= ?x5883 ?x9245))))
 ))
 (let (($x7498 (forall ((n (_ BitVec 6)) )(let ((?x4023 (sc_t 1)))
 (let ((?x6958 (bvadd (_ bv63 6) ?x4023)))
 (let (($x2645 (bvsle ?x6958 n)))
 (let ((?x8959 (stack_t x_0 x_1 w_1 x_SHA3_0 1 n)))
 (let ((?x2498 (stack_t x_0 x_1 w_1 x_SHA3_0 2 n)))
 (or (= ?x2498 ?x8959) $x2645)))))))
 ))
 (let ((?x4023 (sc_t 1)))
 (let ((?x6958 (bvadd (_ bv63 6) ?x4023)))
 (let ((?x4056 (sc_t 2)))
 (let (($x7803 (= ?x4056 ?x6958)))
 (let ((?x1962 (used_gas_t x_0 x_1 w_1 x_SHA3_0 2)))
 (let (($x1920 (exc_halt_t 1)))
 (let (($x2703 (= $x1920 (or $x56 (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) ?x63)))))))
 (let (($x7927 (forall ((w (_ BitVec 256)) )(let ((?x5421 (storage_t x_0 x_1 w_1 x_SHA3_0 0 w)))
 (let ((?x9245 (storage_t x_0 x_1 w_1 x_SHA3_0 1 w)))
 (= ?x9245 ?x5421))))
 ))
 (let (($x5931 (forall ((n (_ BitVec 6)) )(let ((?x63 (sc_t 0)))
 (let ((?x10561 (bvadd (_ bv62 6) ?x63)))
 (let (($x10587 (bvsle ?x10561 n)))
 (let ((?x6712 (stack_t x_0 x_1 w_1 x_SHA3_0 0 n)))
 (let ((?x8959 (stack_t x_0 x_1 w_1 x_SHA3_0 1 n)))
 (or (= ?x8959 ?x6712) $x10587)))))))
 ))
 (let (($x4686 (= ?x4023 ?x63)))
 (let (($x4332 (= (stack_t x_0 x_1 w_1 x_SHA3_0 1 (bvadd (_ bv62 6) ?x4023)) (stack_t x_0 x_1 w_1 x_SHA3_0 0 (bvadd (_ bv63 6) ?x63)))))
 (let (($x3254 (= (stack_t x_0 x_1 w_1 x_SHA3_0 1 ?x6958) (stack_t x_0 x_1 w_1 x_SHA3_0 0 (bvadd (_ bv62 6) ?x63)))))
 (let (($x3773 (= $x772 (or (exc_halt_s 5) (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_s 5))))))))
 (let (($x8020 (forall ((w (_ BitVec 256)) )(let ((?x6723 (storage_s x_0 x_1 w_1 x_SHA3_0 5 w)))
 (let ((?x2215 (storage_s x_0 x_1 w_1 x_SHA3_0 6 w)))
 (= ?x2215 ?x6723))))
 ))
 (let (($x1964 (forall ((n (_ BitVec 6)) )(let ((?x805 (sc_s 5)))
 (let ((?x8839 (bvadd (_ bv63 6) ?x805)))
 (let (($x7467 (bvsle ?x8839 n)))
 (let ((?x9304 (stack_s x_0 x_1 w_1 x_SHA3_0 5 n)))
 (let ((?x9145 (stack_s x_0 x_1 w_1 x_SHA3_0 6 n)))
 (or (= ?x9145 ?x9304) $x7467)))))))
 ))
 (let ((?x805 (sc_s 5)))
 (let ((?x8839 (bvadd (_ bv63 6) ?x805)))
 (let (($x3408 (= ?x926 ?x8839)))
 (let (($x7484 (= (used_gas_s x_0 x_1 w_1 x_SHA3_0 6) (+ 2 (used_gas_s x_0 x_1 w_1 x_SHA3_0 5)))))
 (let (($x3979 (exc_halt_s 5)))
 (let (($x7980 (= $x3979 (or (exc_halt_s 4) (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_s 4))))))))
 (let (($x2683 (forall ((w (_ BitVec 256)) )(let ((?x1912 (storage_s x_0 x_1 w_1 x_SHA3_0 4 w)))
 (let ((?x6723 (storage_s x_0 x_1 w_1 x_SHA3_0 5 w)))
 (= ?x6723 ?x1912))))
 ))
 (let (($x1792 (forall ((n (_ BitVec 6)) )(let ((?x4305 (sc_s 4)))
 (let ((?x10083 (bvadd (_ bv63 6) ?x4305)))
 (let (($x6463 (bvsle ?x10083 n)))
 (let ((?x2029 (stack_s x_0 x_1 w_1 x_SHA3_0 4 n)))
 (let ((?x9304 (stack_s x_0 x_1 w_1 x_SHA3_0 5 n)))
 (or (= ?x9304 ?x2029) $x6463)))))))
 ))
 (let ((?x4305 (sc_s 4)))
 (let ((?x10083 (bvadd (_ bv63 6) ?x4305)))
 (let (($x6538 (= ?x805 ?x10083)))
 (let ((?x7814 (used_gas_s x_0 x_1 w_1 x_SHA3_0 5)))
 (let (($x64 (exc_halt_s 4)))
 (let (($x5911 (= $x64 (or (exc_halt_s 3) (not (bvsle (_ bv0 6) (bvadd (_ bv61 6) (sc_s 3))))))))
 (let (($x2272 (forall ((w (_ BitVec 256)) )(let ((?x7174 (storage_s x_0 x_1 w_1 x_SHA3_0 3 w)))
 (let ((?x1912 (storage_s x_0 x_1 w_1 x_SHA3_0 4 w)))
 (= ?x1912 ?x7174))))
 ))
 (let (($x1094 (forall ((n (_ BitVec 6)) )(let ((?x275 (sc_s 3)))
 (let ((?x11015 (bvadd (_ bv61 6) ?x275)))
 (let (($x8466 (bvsle ?x11015 n)))
 (let ((?x7176 (stack_s x_0 x_1 w_1 x_SHA3_0 3 n)))
 (let ((?x2029 (stack_s x_0 x_1 w_1 x_SHA3_0 4 n)))
 (or (= ?x2029 ?x7176) $x8466)))))))
 ))
 (let ((?x275 (sc_s 3)))
 (let (($x5353 (= ?x4305 ?x275)))
 (let ((?x5654 (used_gas_s x_0 x_1 w_1 x_SHA3_0 4)))
 (let (($x1330 (= (stack_s x_0 x_1 w_1 x_SHA3_0 4 (bvadd (_ bv62 6) ?x4305)) (stack_s x_0 x_1 w_1 x_SHA3_0 3 (bvadd (_ bv62 6) ?x275)))))
 (let ((?x2861 (bvadd (_ bv63 6) ?x275)))
 (let ((?x725 (stack_s x_0 x_1 w_1 x_SHA3_0 3 ?x2861)))
 (let (($x1680 (= (stack_s x_0 x_1 w_1 x_SHA3_0 4 ?x10083) (stack_s x_0 x_1 w_1 x_SHA3_0 3 (bvadd (_ bv61 6) ?x275)))))
 (let (($x292 (exc_halt_s 3)))
 (let (($x9263 (= $x292 (or (exc_halt_s 2) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 2))))))))
 (let (($x1740 (forall ((w (_ BitVec 256)) )(let ((?x7480 (storage_s x_0 x_1 w_1 x_SHA3_0 2 w)))
 (let ((?x7174 (storage_s x_0 x_1 w_1 x_SHA3_0 3 w)))
 (= ?x7174 ?x7480))))
 ))
 (let (($x1659 (forall ((n (_ BitVec 6)) )(let ((?x218 (sc_s 2)))
 (let ((?x1386 (bvadd (_ bv62 6) ?x218)))
 (let (($x10275 (bvsle ?x1386 n)))
 (let ((?x3675 (stack_s x_0 x_1 w_1 x_SHA3_0 2 n)))
 (let ((?x7176 (stack_s x_0 x_1 w_1 x_SHA3_0 3 n)))
 (or (= ?x7176 ?x3675) $x10275)))))))
 ))
 (let ((?x218 (sc_s 2)))
 (let ((?x6327 (bvadd (_ bv63 6) ?x218)))
 (let (($x3662 (= ?x275 ?x6327)))
 (let ((?x1821 (used_gas_s x_0 x_1 w_1 x_SHA3_0 3)))
 (let ((?x2076 (stack_s x_0 x_1 w_1 x_SHA3_0 2 ?x6327)))
 (let ((?x6720 (f_SHA3 x_0 x_1 w_1 x_SHA3_0 ?x2076 (stack_s x_0 x_1 w_1 x_SHA3_0 2 (bvadd (_ bv62 6) ?x218)))))
 (let (($x3417 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 1)))) (_ bv0 1)))))
 (let (($x189 (exc_halt_s 1)))
 (let (($x247 (exc_halt_s 2)))
 (let (($x5374 (= $x247 (or $x189 $x3417 (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 1))))))))
 (let (($x1696 (forall ((w (_ BitVec 256)) )(let ((?x6381 (storage_s x_0 x_1 w_1 x_SHA3_0 1 w)))
 (let ((?x7480 (storage_s x_0 x_1 w_1 x_SHA3_0 2 w)))
 (= ?x7480 ?x6381))))
 ))
 (let (($x2168 (forall ((n (_ BitVec 6)) )(let ((?x154 (sc_s 1)))
 (let ((?x237 (bvadd (_ bv62 6) ?x154)))
 (let (($x1333 (bvsle ?x237 n)))
 (let ((?x5490 (stack_s x_0 x_1 w_1 x_SHA3_0 1 n)))
 (let ((?x3675 (stack_s x_0 x_1 w_1 x_SHA3_0 2 n)))
 (or (= ?x3675 ?x5490) $x1333)))))))
 ))
 (let (($x3670 (= ?x218 (bvadd (_ bv1 6) (sc_s 1)))))
 (let ((?x6658 (used_gas_s x_0 x_1 w_1 x_SHA3_0 2)))
 (let (($x8275 (= (stack_s x_0 x_1 w_1 x_SHA3_0 2 (bvadd (_ bv63 6) (sc_s 1))) (stack_s x_0 x_1 w_1 x_SHA3_0 1 (bvadd (_ bv63 6) (sc_s 1))))))
 (let ((?x154 (sc_s 1)))
 (let ((?x237 (bvadd (_ bv62 6) ?x154)))
 (let ((?x6457 (stack_s x_0 x_1 w_1 x_SHA3_0 1 ?x237)))
 (let (($x4891 (= $x189 (or $x57 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x72))) (_ bv0 1)))))))
 (let (($x5252 (forall ((w (_ BitVec 256)) )(let ((?x8045 (storage_s x_0 x_1 w_1 x_SHA3_0 0 w)))
 (let ((?x6381 (storage_s x_0 x_1 w_1 x_SHA3_0 1 w)))
 (= ?x6381 ?x8045))))
 ))
 (let (($x2085 (forall ((n (_ BitVec 6)) )(let ((?x72 (sc_s 0)))
 (let (($x1656 (bvsle ?x72 n)))
 (let ((?x9055 (stack_s x_0 x_1 w_1 x_SHA3_0 0 n)))
 (let ((?x5490 (stack_s x_0 x_1 w_1 x_SHA3_0 1 n)))
 (or (= ?x5490 ?x9055) $x1656))))))
 ))
 (let (($x10428 (= ?x154 (bvadd (_ bv1 6) ?x72))))
 (let (($x8791 (forall ((w0 (_ BitVec 256)) (w1 (_ BitVec 256)) )(let (($x7704 (= (stack_s x_0 x_1 w_1 x_SHA3_0 2 (bvadd (_ bv62 6) (sc_s 2))) w1)))
 (let (($x5409 (= (stack_s x_0 x_1 w_1 x_SHA3_0 2 (bvadd (_ bv63 6) (sc_s 2))) w0)))
 (let ((?x1388 (f_SHA3 x_0 x_1 w_1 x_SHA3_0 w0 w1)))
 (= ?x1388 (ite (and $x5409 $x7704) x_SHA3_0 (_ bv0 256)))))))
 ))
 (let (($x5755 (forall ((w (_ BitVec 256)) )(let ((?x8045 (storage_s x_0 x_1 w_1 x_SHA3_0 0 w)))
 (= ?x8045 (_ bv0 256))))
 ))
 (let (($x2189 (= ?x1170 0)))
 (let (($x3117 (not $x57)))
 (let (($x5761 (= (stack_s x_0 x_1 w_1 x_SHA3_0 0 (_ bv1 6)) x_1)))
 (let (($x1011 (= (stack_s x_0 x_1 w_1 x_SHA3_0 0 (_ bv0 6)) x_0)))
 (let (($x3159 (= ?x72 (_ bv2 6))))
 (and $x3159 $x1011 $x5761 $x3117 $x2189 $x5755 $x8791 (= (stack_s x_0 x_1 w_1 x_SHA3_0 1 ?x72) w_1) (= (used_gas_s x_0 x_1 w_1 x_SHA3_0 1) (+ 3 ?x1170)) $x10428 $x2085 $x5252 $x4891 (= ?x2076 ?x6457) (= (stack_s x_0 x_1 w_1 x_SHA3_0 2 ?x237) ?x6457) $x8275 (= ?x6658 (+ 3 (used_gas_s x_0 x_1 w_1 x_SHA3_0 1))) $x3670 $x2168 $x1696 $x5374 (= ?x725 ?x6720) (= ?x1821 (+ 30 ?x6658)) $x3662 $x1659 $x1740 $x9263 $x1680 (= (stack_s x_0 x_1 w_1 x_SHA3_0 4 (bvadd (_ bv61 6) ?x4305)) ?x725) $x1330 (= ?x5654 (+ 3 ?x1821)) $x5353 $x1094 $x2272 $x5911 (= ?x7814 (+ 2 ?x5654)) $x6538 $x1792 $x2683 $x7980 $x7484 $x3408 $x1964 $x8020 $x3773 $x3254 $x4332 (= (used_gas_t x_0 x_1 w_1 x_SHA3_0 1) (+ 3 ?x1306)) $x4686 $x5931 $x7927 $x2703 (= ?x1962 (+ 2 (used_gas_t x_0 x_1 w_1 x_SHA3_0 1))) $x7803 $x7498 $x1788 $x2625 (= (stack_t x_0 x_1 w_1 x_SHA3_0 3 ?x4056) w_1) (= ?x10423 (+ 3 ?x1962)) $x5064 $x5753 $x5708 $x4986 $x7414 $x1393 (= ?x8231 (+ 3 ?x10423)) (= ?x3757 ?x2012) $x7892 $x2229 $x5439 $x5771 $x9297 (= ?x919 ?x143) $x1717 $x1673 $x2890 $x73 $x335 $x58 $x5597 $x3460 (not (and $x929 $x2117 $x889 $x1439)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
 )
(check-sat)
(get-proof)