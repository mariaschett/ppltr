; PUSH cw_2 DUP3 PUSH cw_3 SWAP1 SWAP2 SWAP1 => DUP2 PUSH cw_2 PUSH cw_3
(set-option :produce-proofs true)
; 
(set-info :status unknown)
(declare-fun storage_t ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 256)) (_ BitVec 256))
(declare-fun w_3 () (_ BitVec 256))
(declare-fun w_2 () (_ BitVec 256))
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
 (exists ((x_0 (_ BitVec 256)) (x_1 (_ BitVec 256)) )(let (($x7058 (forall ((w (_ BitVec 256)) )(let ((?x4675 (storage_t x_0 x_1 w_2 w_3 3 w)))
 (let ((?x10280 (storage_s x_0 x_1 w_2 w_3 6 w)))
 (= ?x10280 ?x4675))))
 ))
 (let (($x6783 (exc_halt_t 3)))
 (let (($x772 (exc_halt_s 6)))
 (let (($x11249 (= $x772 $x6783)))
 (let (($x10336 (forall ((n (_ BitVec 6)) )(let ((?x7916 (stack_t x_0 x_1 w_2 w_3 3 n)))
 (let ((?x180 (stack_s x_0 x_1 w_2 w_3 6 n)))
 (let (($x6192 (= ?x180 ?x7916)))
 (let ((?x6438 (sc_t 3)))
 (let (($x1180 (bvsle ?x6438 n)))
 (or $x1180 $x6192)))))))
 ))
 (let ((?x6438 (sc_t 3)))
 (let ((?x926 (sc_s 6)))
 (let (($x2909 (= ?x926 ?x6438)))
 (let ((?x10911 (used_gas_t x_0 x_1 w_2 w_3 0)))
 (let ((?x8199 (used_gas_s x_0 x_1 w_2 w_3 0)))
 (let (($x1193 (= ?x8199 ?x10911)))
 (let (($x10677 (forall ((w (_ BitVec 256)) )(let ((?x678 (storage_t x_0 x_1 w_2 w_3 0 w)))
 (let ((?x1934 (storage_s x_0 x_1 w_2 w_3 0 w)))
 (= ?x1934 ?x678))))
 ))
 (let (($x56 (exc_halt_t 0)))
 (let (($x57 (exc_halt_s 0)))
 (let (($x58 (= $x57 $x56)))
 (let (($x8488 (forall ((n (_ BitVec 6)) )(let ((?x4700 (stack_t x_0 x_1 w_2 w_3 0 n)))
 (let ((?x1898 (stack_s x_0 x_1 w_2 w_3 0 n)))
 (let (($x11359 (= ?x1898 ?x4700)))
 (let ((?x63 (sc_t 0)))
 (let (($x2855 (bvsle ?x63 n)))
 (or $x2855 $x11359)))))))
 ))
 (let ((?x63 (sc_t 0)))
 (let ((?x72 (sc_s 0)))
 (let (($x73 (= ?x72 ?x63)))
 (let (($x2163 (exc_halt_t 2)))
 (let (($x494 (or $x2163 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_t 2)))) (_ bv0 1))))))
 (let (($x2133 (= $x6783 $x494)))
 (let (($x6366 (forall ((w (_ BitVec 256)) )(let ((?x3487 (storage_t x_0 x_1 w_2 w_3 2 w)))
 (let ((?x4675 (storage_t x_0 x_1 w_2 w_3 3 w)))
 (= ?x4675 ?x3487))))
 ))
 (let (($x1418 (forall ((n (_ BitVec 6)) )(let ((?x10493 (stack_t x_0 x_1 w_2 w_3 2 n)))
 (let ((?x7916 (stack_t x_0 x_1 w_2 w_3 3 n)))
 (let ((?x2714 (sc_t 2)))
 (let (($x1932 (bvsle ?x2714 n)))
 (or $x1932 (= ?x7916 ?x10493)))))))
 ))
 (let (($x10335 (= ?x6438 (bvadd (_ bv1 6) (sc_t 2)))))
 (let (($x7838 (= (used_gas_t x_0 x_1 w_2 w_3 3) (+ 3 (used_gas_t x_0 x_1 w_2 w_3 2)))))
 (let (($x6916 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_t 1)))) (_ bv0 1)))))
 (let (($x8377 (exc_halt_t 1)))
 (let (($x11314 (= $x2163 (or $x8377 $x6916))))
 (let (($x8172 (forall ((w (_ BitVec 256)) )(let ((?x7182 (storage_t x_0 x_1 w_2 w_3 1 w)))
 (let ((?x3487 (storage_t x_0 x_1 w_2 w_3 2 w)))
 (= ?x3487 ?x7182))))
 ))
 (let (($x6144 (forall ((n (_ BitVec 6)) )(let ((?x6245 (stack_t x_0 x_1 w_2 w_3 1 n)))
 (let ((?x10493 (stack_t x_0 x_1 w_2 w_3 2 n)))
 (let ((?x7154 (sc_t 1)))
 (let (($x1020 (bvsle ?x7154 n)))
 (or $x1020 (= ?x10493 ?x6245)))))))
 ))
 (let ((?x2714 (sc_t 2)))
 (let (($x135 (= ?x2714 (bvadd (_ bv1 6) (sc_t 1)))))
 (let ((?x8283 (used_gas_t x_0 x_1 w_2 w_3 2)))
 (let (($x5235 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x63))) (_ bv0 1)))))
 (let (($x11306 (forall ((w (_ BitVec 256)) )(let ((?x678 (storage_t x_0 x_1 w_2 w_3 0 w)))
 (let ((?x7182 (storage_t x_0 x_1 w_2 w_3 1 w)))
 (= ?x7182 ?x678))))
 ))
 (let (($x9148 (forall ((n (_ BitVec 6)) )(let ((?x4700 (stack_t x_0 x_1 w_2 w_3 0 n)))
 (let ((?x6245 (stack_t x_0 x_1 w_2 w_3 1 n)))
 (or (= ?x6245 ?x4700) (bvsle (bvadd (_ bv62 6) (sc_t 0)) n)))))
 ))
 (let ((?x7154 (sc_t 1)))
 (let (($x6089 (= ?x7154 (bvadd (_ bv1 6) ?x63))))
 (let (($x9318 (= (stack_t x_0 x_1 w_2 w_3 1 (bvadd (_ bv63 6) ?x63)) (stack_t x_0 x_1 w_2 w_3 0 (bvadd (_ bv63 6) ?x63)))))
 (let ((?x3346 (bvadd (_ bv62 6) ?x63)))
 (let ((?x6888 (stack_t x_0 x_1 w_2 w_3 0 ?x3346)))
 (let (($x6274 (= $x772 (or (exc_halt_s 5) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 5))))))))
 (let (($x8685 (forall ((w (_ BitVec 256)) )(let ((?x5476 (storage_s x_0 x_1 w_2 w_3 5 w)))
 (let ((?x10280 (storage_s x_0 x_1 w_2 w_3 6 w)))
 (= ?x10280 ?x5476))))
 ))
 (let (($x3861 (forall ((n (_ BitVec 6)) )(let ((?x4319 (sc_s 5)))
 (let ((?x4138 (bvadd (_ bv62 6) ?x4319)))
 (let (($x7050 (bvsle ?x4138 n)))
 (let ((?x952 (stack_s x_0 x_1 w_2 w_3 5 n)))
 (let ((?x180 (stack_s x_0 x_1 w_2 w_3 6 n)))
 (or (= ?x180 ?x952) $x7050)))))))
 ))
 (let ((?x4319 (sc_s 5)))
 (let (($x3654 (= ?x926 ?x4319)))
 (let (($x9922 (= (used_gas_s x_0 x_1 w_2 w_3 6) (+ 3 (used_gas_s x_0 x_1 w_2 w_3 5)))))
 (let ((?x8324 (bvadd (_ bv63 6) ?x4319)))
 (let ((?x11473 (stack_s x_0 x_1 w_2 w_3 5 ?x8324)))
 (let ((?x4138 (bvadd (_ bv62 6) ?x4319)))
 (let ((?x2420 (stack_s x_0 x_1 w_2 w_3 5 ?x4138)))
 (let (($x11317 (exc_halt_s 5)))
 (let (($x9037 (= $x11317 (or (exc_halt_s 4) (not (bvsle (_ bv0 6) (bvadd (_ bv61 6) (sc_s 4))))))))
 (let (($x8186 (forall ((w (_ BitVec 256)) )(let ((?x10087 (storage_s x_0 x_1 w_2 w_3 4 w)))
 (let ((?x5476 (storage_s x_0 x_1 w_2 w_3 5 w)))
 (= ?x5476 ?x10087))))
 ))
 (let (($x9619 (forall ((n (_ BitVec 6)) )(let ((?x6478 (stack_s x_0 x_1 w_2 w_3 4 n)))
 (let ((?x952 (stack_s x_0 x_1 w_2 w_3 5 n)))
 (or (= ?x952 ?x6478) (bvsle (bvadd (_ bv61 6) (sc_s 4)) n)))))
 ))
 (let ((?x6909 (used_gas_s x_0 x_1 w_2 w_3 5)))
 (let ((?x4305 (sc_s 4)))
 (let ((?x5394 (bvadd (_ bv62 6) ?x4305)))
 (let ((?x3416 (stack_s x_0 x_1 w_2 w_3 4 ?x5394)))
 (let ((?x4388 (bvadd (_ bv63 6) ?x4305)))
 (let ((?x9153 (stack_s x_0 x_1 w_2 w_3 4 ?x4388)))
 (let (($x7172 (exc_halt_s 4)))
 (let (($x2571 (= $x7172 (or (exc_halt_s 3) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 3))))))))
 (let (($x4682 (forall ((w (_ BitVec 256)) )(let ((?x2082 (storage_s x_0 x_1 w_2 w_3 3 w)))
 (let ((?x10087 (storage_s x_0 x_1 w_2 w_3 4 w)))
 (= ?x10087 ?x2082))))
 ))
 (let (($x4483 (forall ((n (_ BitVec 6)) )(let ((?x275 (sc_s 3)))
 (let ((?x1134 (bvadd (_ bv62 6) ?x275)))
 (let (($x7946 (bvsle ?x1134 n)))
 (let ((?x11206 (stack_s x_0 x_1 w_2 w_3 3 n)))
 (let ((?x6478 (stack_s x_0 x_1 w_2 w_3 4 n)))
 (or (= ?x6478 ?x11206) $x7946)))))))
 ))
 (let ((?x275 (sc_s 3)))
 (let (($x2594 (= ?x4305 ?x275)))
 (let ((?x9904 (used_gas_s x_0 x_1 w_2 w_3 4)))
 (let (($x9685 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 2)))) (_ bv0 1)))))
 (let (($x247 (exc_halt_s 2)))
 (let (($x292 (exc_halt_s 3)))
 (let (($x590 (= $x292 (or $x247 $x9685))))
 (let (($x1088 (forall ((w (_ BitVec 256)) )(let ((?x10762 (storage_s x_0 x_1 w_2 w_3 2 w)))
 (let ((?x2082 (storage_s x_0 x_1 w_2 w_3 3 w)))
 (= ?x2082 ?x10762))))
 ))
 (let (($x3587 (forall ((n (_ BitVec 6)) )(let ((?x218 (sc_s 2)))
 (let (($x11461 (bvsle ?x218 n)))
 (let ((?x724 (stack_s x_0 x_1 w_2 w_3 2 n)))
 (let ((?x11206 (stack_s x_0 x_1 w_2 w_3 3 n)))
 (or (= ?x11206 ?x724) $x11461))))))
 ))
 (let (($x2016 (= ?x275 (bvadd (_ bv1 6) (sc_s 2)))))
 (let ((?x1471 (used_gas_s x_0 x_1 w_2 w_3 3)))
 (let (($x1803 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 1)))) (_ bv0 1)))))
 (let (($x189 (exc_halt_s 1)))
 (let (($x11218 (forall ((w (_ BitVec 256)) )(let ((?x810 (storage_s x_0 x_1 w_2 w_3 1 w)))
 (let ((?x10762 (storage_s x_0 x_1 w_2 w_3 2 w)))
 (= ?x10762 ?x810))))
 ))
 (let (($x9560 (forall ((n (_ BitVec 6)) )(let ((?x1406 (stack_s x_0 x_1 w_2 w_3 1 n)))
 (let ((?x724 (stack_s x_0 x_1 w_2 w_3 2 n)))
 (or (= ?x724 ?x1406) (bvsle (bvadd (_ bv61 6) (sc_s 1)) n)))))
 ))
 (let ((?x218 (sc_s 2)))
 (let (($x11672 (= ?x218 (bvadd (_ bv1 6) (sc_s 1)))))
 (let ((?x8917 (used_gas_s x_0 x_1 w_2 w_3 2)))
 (let (($x5810 (= (stack_s x_0 x_1 w_2 w_3 2 (bvadd (_ bv63 6) (sc_s 1))) (stack_s x_0 x_1 w_2 w_3 1 (bvadd (_ bv63 6) (sc_s 1))))))
 (let (($x7136 (= (stack_s x_0 x_1 w_2 w_3 2 (bvadd (_ bv62 6) (sc_s 1))) (stack_s x_0 x_1 w_2 w_3 1 (bvadd (_ bv62 6) (sc_s 1))))))
 (let ((?x154 (sc_s 1)))
 (let ((?x7028 (bvadd (_ bv61 6) ?x154)))
 (let ((?x1929 (stack_s x_0 x_1 w_2 w_3 1 ?x7028)))
 (let (($x9208 (= $x189 (or $x57 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x72))) (_ bv0 1)))))))
 (let (($x585 (forall ((w (_ BitVec 256)) )(let ((?x1934 (storage_s x_0 x_1 w_2 w_3 0 w)))
 (let ((?x810 (storage_s x_0 x_1 w_2 w_3 1 w)))
 (= ?x810 ?x1934))))
 ))
 (let (($x316 (forall ((n (_ BitVec 6)) )(let ((?x72 (sc_s 0)))
 (let (($x10612 (bvsle ?x72 n)))
 (let ((?x1898 (stack_s x_0 x_1 w_2 w_3 0 n)))
 (let ((?x1406 (stack_s x_0 x_1 w_2 w_3 1 n)))
 (or (= ?x1406 ?x1898) $x10612))))))
 ))
 (let (($x6169 (= ?x154 (bvadd (_ bv1 6) ?x72))))
 (let (($x1871 (forall ((w (_ BitVec 256)) )(let ((?x1934 (storage_s x_0 x_1 w_2 w_3 0 w)))
 (= ?x1934 (_ bv0 256))))
 ))
 (let (($x6252 (= ?x8199 0)))
 (let (($x10766 (not $x57)))
 (let (($x10822 (= (stack_s x_0 x_1 w_2 w_3 0 (_ bv1 6)) x_1)))
 (let (($x4068 (= (stack_s x_0 x_1 w_2 w_3 0 (_ bv0 6)) x_0)))
 (let (($x11128 (= ?x72 (_ bv2 6))))
 (and $x11128 $x4068 $x10822 $x10766 $x6252 $x1871 (= (stack_s x_0 x_1 w_2 w_3 1 ?x72) w_2) (= (used_gas_s x_0 x_1 w_2 w_3 1) (+ 3 ?x8199)) $x6169 $x316 $x585 $x9208 (= (stack_s x_0 x_1 w_2 w_3 2 (bvadd (_ bv63 6) ?x218)) ?x1929) (= (stack_s x_0 x_1 w_2 w_3 2 ?x7028) ?x1929) $x7136 $x5810 (= ?x8917 (+ 3 (used_gas_s x_0 x_1 w_2 w_3 1))) $x11672 $x9560 $x11218 (= $x247 (or $x189 $x1803 (not (bvsle (_ bv0 6) ?x7028)))) (= (stack_s x_0 x_1 w_2 w_3 3 ?x218) w_3) (= ?x1471 (+ 3 ?x8917)) $x2016 $x3587 $x1088 $x590 (= ?x9153 (stack_s x_0 x_1 w_2 w_3 3 (bvadd (_ bv62 6) ?x275))) (= ?x3416 (stack_s x_0 x_1 w_2 w_3 3 (bvadd (_ bv63 6) ?x275))) (= ?x9904 (+ 3 ?x1471)) $x2594 $x4483 $x4682 $x2571 (= ?x11473 (stack_s x_0 x_1 w_2 w_3 4 (bvadd (_ bv61 6) ?x4305))) (= (stack_s x_0 x_1 w_2 w_3 5 (bvadd (_ bv61 6) ?x4319)) ?x9153) (= ?x2420 ?x3416) (= ?x6909 (+ 3 ?x9904)) (= ?x4319 ?x4305) $x9619 $x8186 $x9037 (= (stack_s x_0 x_1 w_2 w_3 6 (bvadd (_ bv63 6) ?x926)) ?x2420) (= (stack_s x_0 x_1 w_2 w_3 6 (bvadd (_ bv62 6) ?x926)) ?x11473) $x9922 $x3654 $x3861 $x8685 $x6274 (= (stack_t x_0 x_1 w_2 w_3 1 (bvadd (_ bv63 6) ?x7154)) ?x6888) (= (stack_t x_0 x_1 w_2 w_3 1 ?x3346) ?x6888) $x9318 (= (used_gas_t x_0 x_1 w_2 w_3 1) (+ 3 ?x10911)) $x6089 $x9148 $x11306 (= $x8377 (or $x56 $x5235 (not (bvsle (_ bv0 6) ?x3346)))) (= (stack_t x_0 x_1 w_2 w_3 2 ?x7154) w_2) (= ?x8283 (+ 3 (used_gas_t x_0 x_1 w_2 w_3 1))) $x135 $x6144 $x8172 $x11314 (= (stack_t x_0 x_1 w_2 w_3 3 ?x2714) w_3) $x7838 $x10335 $x1418 $x6366 $x2133 $x73 $x8488 $x58 $x10677 $x1193 (not (and $x2909 $x10336 $x11249 $x7058)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
 )
(check-sat)
(get-proof)
