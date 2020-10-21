; PUSH 0x00 ADD PUSH cw_2 SWAP1 SLOAD SWAP1 => SLOAD PUSH cw_2
(set-option :produce-proofs true)
; 
(set-info :status unknown)
(declare-fun storage_t ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 256)) (_ BitVec 256))
(declare-fun w_2 () (_ BitVec 256))
(declare-fun storage_s ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 256)) (_ BitVec 256))
(declare-fun exc_halt_t (Int) Bool)
(declare-fun exc_halt_s (Int) Bool)
(declare-fun stack_t ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 6)) (_ BitVec 256))
(declare-fun stack_s ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 6)) (_ BitVec 256))
(declare-fun sc_t (Int) (_ BitVec 6))
(declare-fun sc_s (Int) (_ BitVec 6))
(declare-fun used_gas_t ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int) Int)
(declare-fun used_gas_s ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int) Int)
(assert
 (exists ((x_0 (_ BitVec 256)) (x_SLOAD_0 (_ BitVec 256)) )(let (($x6248 (forall ((w (_ BitVec 256)) )(let ((?x4214 (storage_t x_0 x_SLOAD_0 w_2 2 w)))
 (let ((?x7401 (storage_s x_0 x_SLOAD_0 w_2 6 w)))
 (= ?x7401 ?x4214))))
 ))
 (let (($x5252 (exc_halt_t 2)))
 (let (($x772 (exc_halt_s 6)))
 (let (($x735 (= $x772 $x5252)))
 (let (($x10606 (forall ((n (_ BitVec 6)) )(let ((?x6046 (stack_t x_0 x_SLOAD_0 w_2 2 n)))
 (let ((?x5976 (stack_s x_0 x_SLOAD_0 w_2 6 n)))
 (let (($x8701 (= ?x5976 ?x6046)))
 (let ((?x2714 (sc_t 2)))
 (let (($x6869 (bvsle ?x2714 n)))
 (or $x6869 $x8701)))))))
 ))
 (let ((?x2714 (sc_t 2)))
 (let ((?x926 (sc_s 6)))
 (let (($x3808 (= ?x926 ?x2714)))
 (let ((?x8807 (used_gas_t x_0 x_SLOAD_0 w_2 0)))
 (let ((?x9169 (used_gas_s x_0 x_SLOAD_0 w_2 0)))
 (let (($x6083 (= ?x9169 ?x8807)))
 (let (($x7371 (forall ((w (_ BitVec 256)) )(let ((?x5125 (storage_t x_0 x_SLOAD_0 w_2 0 w)))
 (let ((?x8621 (storage_s x_0 x_SLOAD_0 w_2 0 w)))
 (= ?x8621 ?x5125))))
 ))
 (let (($x56 (exc_halt_t 0)))
 (let (($x57 (exc_halt_s 0)))
 (let (($x58 (= $x57 $x56)))
 (let (($x9336 (forall ((n (_ BitVec 6)) )(let ((?x2082 (stack_t x_0 x_SLOAD_0 w_2 0 n)))
 (let ((?x4269 (stack_s x_0 x_SLOAD_0 w_2 0 n)))
 (let (($x5344 (= ?x4269 ?x2082)))
 (let ((?x63 (sc_t 0)))
 (let (($x3267 (bvsle ?x63 n)))
 (or $x3267 $x5344)))))))
 ))
 (let ((?x63 (sc_t 0)))
 (let ((?x72 (sc_s 0)))
 (let (($x73 (= ?x72 ?x63)))
 (let (($x1972 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_t 1)))) (_ bv0 1)))))
 (let (($x3508 (exc_halt_t 1)))
 (let (($x8317 (= $x5252 (or $x3508 $x1972))))
 (let (($x11560 (forall ((w (_ BitVec 256)) )(let ((?x7149 (storage_t x_0 x_SLOAD_0 w_2 1 w)))
 (let ((?x4214 (storage_t x_0 x_SLOAD_0 w_2 2 w)))
 (= ?x4214 ?x7149))))
 ))
 (let (($x8266 (forall ((n (_ BitVec 6)) )(let ((?x8801 (stack_t x_0 x_SLOAD_0 w_2 1 n)))
 (let ((?x6046 (stack_t x_0 x_SLOAD_0 w_2 2 n)))
 (let (($x11435 (= ?x6046 ?x8801)))
 (let ((?x8347 (sc_t 1)))
 (let (($x6165 (bvsle ?x8347 n)))
 (or $x6165 $x11435)))))))
 ))
 (let (($x1064 (= ?x2714 (bvadd (_ bv1 6) (sc_t 1)))))
 (let ((?x9024 (used_gas_t x_0 x_SLOAD_0 w_2 2)))
 (let (($x11606 (= ?x9024 (+ 3 (used_gas_t x_0 x_SLOAD_0 w_2 1)))))
 (let (($x7158 (= (stack_t x_0 x_SLOAD_0 w_2 2 (sc_t 1)) w_2)))
 (let (($x8090 (= $x3508 (or $x56 (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) ?x63)))))))
 (let (($x6793 (forall ((w (_ BitVec 256)) )(let ((?x5125 (storage_t x_0 x_SLOAD_0 w_2 0 w)))
 (let ((?x7149 (storage_t x_0 x_SLOAD_0 w_2 1 w)))
 (= ?x7149 ?x5125))))
 ))
 (let (($x6407 (forall ((n (_ BitVec 6)) )(let ((?x63 (sc_t 0)))
 (let ((?x3364 (bvadd (_ bv63 6) ?x63)))
 (let (($x6829 (bvsle ?x3364 n)))
 (let ((?x2082 (stack_t x_0 x_SLOAD_0 w_2 0 n)))
 (let ((?x8801 (stack_t x_0 x_SLOAD_0 w_2 1 n)))
 (let (($x4353 (= ?x8801 ?x2082)))
 (or $x4353 $x6829))))))))
 ))
 (let ((?x8347 (sc_t 1)))
 (let (($x2771 (= ?x8347 ?x63)))
 (let ((?x3364 (bvadd (_ bv63 6) ?x63)))
 (let ((?x7084 (stack_t x_0 x_SLOAD_0 w_2 0 ?x3364)))
 (let ((?x401 (bvadd (_ bv63 6) ?x8347)))
 (let ((?x7455 (stack_t x_0 x_SLOAD_0 w_2 1 ?x401)))
 (let (($x904 (= $x772 (or (exc_halt_s 5) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 5))))))))
 (let (($x8147 (forall ((w (_ BitVec 256)) )(let ((?x8157 (storage_s x_0 x_SLOAD_0 w_2 5 w)))
 (let ((?x7401 (storage_s x_0 x_SLOAD_0 w_2 6 w)))
 (= ?x7401 ?x8157))))
 ))
 (let (($x122 (forall ((n (_ BitVec 6)) )(let ((?x1485 (stack_s x_0 x_SLOAD_0 w_2 5 n)))
 (let ((?x5976 (stack_s x_0 x_SLOAD_0 w_2 6 n)))
 (or (bvsle (bvadd (_ bv62 6) (sc_s 5)) n) (= ?x5976 ?x1485)))))
 ))
 (let ((?x4319 (sc_s 5)))
 (let (($x2373 (= ?x926 ?x4319)))
 (let (($x580 (= (used_gas_s x_0 x_SLOAD_0 w_2 6) (+ 3 (used_gas_s x_0 x_SLOAD_0 w_2 5)))))
 (let ((?x7318 (bvadd (_ bv63 6) ?x4319)))
 (let ((?x4105 (stack_s x_0 x_SLOAD_0 w_2 5 ?x7318)))
 (let (($x1748 (= (stack_s x_0 x_SLOAD_0 w_2 6 (bvadd (_ bv62 6) ?x926)) ?x4105)))
 (let ((?x4449 (bvadd (_ bv62 6) ?x4319)))
 (let ((?x5682 (stack_s x_0 x_SLOAD_0 w_2 5 ?x4449)))
 (let (($x7002 (= (stack_s x_0 x_SLOAD_0 w_2 6 (bvadd (_ bv63 6) ?x926)) ?x5682)))
 (let (($x11317 (exc_halt_s 5)))
 (let (($x10749 (= $x11317 (or (exc_halt_s 4) (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_s 4))))))))
 (let (($x7540 (forall ((w (_ BitVec 256)) )(let ((?x7861 (storage_s x_0 x_SLOAD_0 w_2 4 w)))
 (let ((?x8157 (storage_s x_0 x_SLOAD_0 w_2 5 w)))
 (= ?x8157 ?x7861))))
 ))
 (let (($x9279 (forall ((n (_ BitVec 6)) )(let ((?x5686 (stack_s x_0 x_SLOAD_0 w_2 4 n)))
 (let ((?x1485 (stack_s x_0 x_SLOAD_0 w_2 5 n)))
 (let (($x9501 (= ?x1485 ?x5686)))
 (let ((?x4305 (sc_s 4)))
 (let ((?x4062 (bvadd (_ bv63 6) ?x4305)))
 (let (($x1621 (bvsle ?x4062 n)))
 (or $x1621 $x9501))))))))
 ))
 (let ((?x4305 (sc_s 4)))
 (let (($x1334 (= ?x4319 ?x4305)))
 (let ((?x313 (used_gas_s x_0 x_SLOAD_0 w_2 5)))
 (let ((?x4062 (bvadd (_ bv63 6) ?x4305)))
 (let ((?x3018 (stack_s x_0 x_SLOAD_0 w_2 4 ?x4062)))
 (let (($x7172 (exc_halt_s 4)))
 (let (($x4030 (= $x7172 (or (exc_halt_s 3) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 3))))))))
 (let (($x8249 (forall ((w (_ BitVec 256)) )(let ((?x6289 (storage_s x_0 x_SLOAD_0 w_2 3 w)))
 (let ((?x7861 (storage_s x_0 x_SLOAD_0 w_2 4 w)))
 (= ?x7861 ?x6289))))
 ))
 (let (($x1060 (forall ((n (_ BitVec 6)) )(let ((?x275 (sc_s 3)))
 (let ((?x10732 (bvadd (_ bv62 6) ?x275)))
 (let (($x7977 (bvsle ?x10732 n)))
 (let ((?x1040 (stack_s x_0 x_SLOAD_0 w_2 3 n)))
 (let ((?x5686 (stack_s x_0 x_SLOAD_0 w_2 4 n)))
 (or (= ?x5686 ?x1040) $x7977)))))))
 ))
 (let ((?x275 (sc_s 3)))
 (let (($x2388 (= ?x4305 ?x275)))
 (let ((?x9241 (used_gas_s x_0 x_SLOAD_0 w_2 4)))
 (let (($x7242 (= ?x9241 (+ 3 (used_gas_s x_0 x_SLOAD_0 w_2 3)))))
 (let ((?x6354 (bvadd (_ bv63 6) ?x275)))
 (let ((?x3334 (stack_s x_0 x_SLOAD_0 w_2 3 ?x6354)))
 (let ((?x10436 (bvadd (_ bv62 6) ?x4305)))
 (let ((?x413 (stack_s x_0 x_SLOAD_0 w_2 4 ?x10436)))
 (let ((?x10732 (bvadd (_ bv62 6) ?x275)))
 (let ((?x10211 (stack_s x_0 x_SLOAD_0 w_2 3 ?x10732)))
 (let (($x1201 (= ?x3018 ?x10211)))
 (let (($x6651 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 2)))) (_ bv0 1)))))
 (let (($x247 (exc_halt_s 2)))
 (let (($x292 (exc_halt_s 3)))
 (let (($x11313 (forall ((w (_ BitVec 256)) )(let ((?x6080 (storage_s x_0 x_SLOAD_0 w_2 2 w)))
 (let ((?x6289 (storage_s x_0 x_SLOAD_0 w_2 3 w)))
 (= ?x6289 ?x6080))))
 ))
 (let (($x6825 (forall ((n (_ BitVec 6)) )(let ((?x9811 (stack_s x_0 x_SLOAD_0 w_2 2 n)))
 (let ((?x1040 (stack_s x_0 x_SLOAD_0 w_2 3 n)))
 (let (($x11437 (= ?x1040 ?x9811)))
 (or (bvsle (sc_s 2) n) $x11437)))))
 ))
 (let (($x2920 (= ?x275 (bvadd (_ bv1 6) (sc_s 2)))))
 (let ((?x8963 (used_gas_s x_0 x_SLOAD_0 w_2 3)))
 (let (($x9251 (= ?x8963 (+ 3 (used_gas_s x_0 x_SLOAD_0 w_2 2)))))
 (let (($x7256 (= $x247 (or (exc_halt_s 1) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 1))))))))
 (let (($x8770 (forall ((w (_ BitVec 256)) )(let ((?x4528 (storage_s x_0 x_SLOAD_0 w_2 1 w)))
 (let ((?x6080 (storage_s x_0 x_SLOAD_0 w_2 2 w)))
 (= ?x6080 ?x4528))))
 ))
 (let (($x888 (forall ((n (_ BitVec 6)) )(let ((?x154 (sc_s 1)))
 (let ((?x3833 (bvadd (_ bv62 6) ?x154)))
 (let (($x5468 (bvsle ?x3833 n)))
 (let ((?x11082 (stack_s x_0 x_SLOAD_0 w_2 1 n)))
 (let ((?x9811 (stack_s x_0 x_SLOAD_0 w_2 2 n)))
 (let (($x4370 (= ?x9811 ?x11082)))
 (or $x4370 $x5468))))))))
 ))
 (let ((?x10949 (used_gas_s x_0 x_SLOAD_0 w_2 2)))
 (let (($x9458 (= ?x10949 (+ 3 (used_gas_s x_0 x_SLOAD_0 w_2 1)))))
 (let ((?x154 (sc_s 1)))
 (let ((?x3833 (bvadd (_ bv62 6) ?x154)))
 (let ((?x9496 (stack_s x_0 x_SLOAD_0 w_2 1 ?x3833)))
 (let ((?x7858 (bvadd (_ bv63 6) ?x154)))
 (let ((?x417 (stack_s x_0 x_SLOAD_0 w_2 1 ?x7858)))
 (let ((?x218 (sc_s 2)))
 (let ((?x4506 (bvadd (_ bv63 6) ?x218)))
 (let ((?x8051 (stack_s x_0 x_SLOAD_0 w_2 2 ?x4506)))
 (let (($x189 (exc_halt_s 1)))
 (let (($x722 (= $x189 (or $x57 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x72))) (_ bv0 1)))))))
 (let (($x3874 (forall ((w (_ BitVec 256)) )(let ((?x8621 (storage_s x_0 x_SLOAD_0 w_2 0 w)))
 (let ((?x4528 (storage_s x_0 x_SLOAD_0 w_2 1 w)))
 (= ?x4528 ?x8621))))
 ))
 (let (($x2757 (forall ((n (_ BitVec 6)) )(let ((?x72 (sc_s 0)))
 (let (($x9281 (bvsle ?x72 n)))
 (let ((?x4269 (stack_s x_0 x_SLOAD_0 w_2 0 n)))
 (let ((?x11082 (stack_s x_0 x_SLOAD_0 w_2 1 n)))
 (let (($x3438 (= ?x11082 ?x4269)))
 (or $x3438 $x9281)))))))
 ))
 (let (($x7884 (= ?x154 (bvadd (_ bv1 6) ?x72))))
 (let ((?x10234 (used_gas_s x_0 x_SLOAD_0 w_2 1)))
 (let (($x8257 (= ?x10234 (+ 3 ?x9169))))
 (let (($x10021 (forall ((w (_ BitVec 256)) )(let ((?x10757 (ite (= w (stack_s x_0 x_SLOAD_0 w_2 4 (bvadd (_ bv63 6) (sc_s 4)))) x_SLOAD_0 (_ bv0 256))))
 (let ((?x8621 (storage_s x_0 x_SLOAD_0 w_2 0 w)))
 (= ?x8621 ?x10757))))
 ))
 (let (($x6714 (= ?x9169 0)))
 (let (($x2751 (not $x57)))
 (let (($x4129 (= (stack_s x_0 x_SLOAD_0 w_2 0 (_ bv0 6)) x_0)))
 (let (($x8685 (= ?x72 (_ bv1 6))))
 (and $x8685 $x4129 $x2751 $x6714 $x10021 (= (stack_s x_0 x_SLOAD_0 w_2 1 ?x72) (_ bv0 256)) $x8257 $x7884 $x2757 $x3874 $x722 (= ?x8051 (bvadd ?x417 ?x9496)) $x9458 (= ?x218 ?x7858) $x888 $x8770 $x7256 (= (stack_s x_0 x_SLOAD_0 w_2 3 ?x218) w_2) $x9251 $x2920 $x6825 $x11313 (= $x292 (or $x247 $x6651)) $x1201 (= ?x413 ?x3334) $x7242 $x2388 $x1060 $x8249 $x4030 (= ?x4105 (storage_s x_0 x_SLOAD_0 w_2 4 ?x3018)) (= ?x313 (+ 200 ?x9241)) $x1334 $x9279 $x7540 $x10749 $x7002 $x1748 $x580 $x2373 $x122 $x8147 $x904 (= ?x7455 (storage_t x_0 x_SLOAD_0 w_2 0 ?x7084)) (= (used_gas_t x_0 x_SLOAD_0 w_2 1) (+ 200 ?x8807)) $x2771 $x6407 $x6793 $x8090 $x7158 $x11606 $x1064 $x8266 $x11560 $x8317 $x73 $x9336 $x58 $x7371 $x6083 (not (and $x3808 $x10606 $x735 $x6248))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
 )
(check-sat)
(get-proof)
