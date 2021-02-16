; PUSH cw_1 SWAP4 SWAP1 SWAP4 => SWAP3 PUSH cw_1
(set-option :produce-proofs true)
; 
(set-info :status unknown)
(declare-fun storage_t ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 256)) (_ BitVec 256))
(declare-fun w_1 () (_ BitVec 256))
(declare-fun storage_s ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 256)) (_ BitVec 256))
(declare-fun exc_halt_t (Int) Bool)
(declare-fun exc_halt_s (Int) Bool)
(declare-fun stack_t ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 6)) (_ BitVec 256))
(declare-fun stack_s ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 6)) (_ BitVec 256))
(declare-fun sc_t (Int) (_ BitVec 6))
(declare-fun sc_s (Int) (_ BitVec 6))
(declare-fun used_gas_t ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int) Int)
(declare-fun used_gas_s ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int) Int)
(assert
 (exists ((x_0 (_ BitVec 256)) (x_1 (_ BitVec 256)) (x_2 (_ BitVec 256)) (x_3 (_ BitVec 256)) )(let (($x10617 (forall ((w (_ BitVec 256)) )(let ((?x4223 (storage_t x_0 x_1 x_2 x_3 w_1 2 w)))
 (let ((?x2862 (storage_s x_0 x_1 x_2 x_3 w_1 4 w)))
 (= ?x2862 ?x4223))))
 ))
 (let (($x903 (exc_halt_t 2)))
 (let (($x64 (exc_halt_s 4)))
 (let (($x488 (= $x64 $x903)))
 (let (($x7821 (forall ((n (_ BitVec 6)) )(let ((?x11423 (stack_t x_0 x_1 x_2 x_3 w_1 2 n)))
 (let ((?x1676 (stack_s x_0 x_1 x_2 x_3 w_1 4 n)))
 (let (($x7071 (= ?x1676 ?x11423)))
 (let ((?x4056 (sc_t 2)))
 (let (($x1865 (bvsle ?x4056 n)))
 (or $x1865 $x7071)))))))
 ))
 (let ((?x4056 (sc_t 2)))
 (let ((?x4305 (sc_s 4)))
 (let (($x1495 (= ?x4305 ?x4056)))
 (let ((?x1808 (used_gas_t x_0 x_1 x_2 x_3 w_1 0)))
 (let ((?x6089 (used_gas_s x_0 x_1 x_2 x_3 w_1 0)))
 (let (($x3125 (= ?x6089 ?x1808)))
 (let (($x2994 (forall ((w (_ BitVec 256)) )(let ((?x7748 (storage_t x_0 x_1 x_2 x_3 w_1 0 w)))
 (let ((?x39 (storage_s x_0 x_1 x_2 x_3 w_1 0 w)))
 (= ?x39 ?x7748))))
 ))
 (let (($x56 (exc_halt_t 0)))
 (let (($x57 (exc_halt_s 0)))
 (let (($x58 (= $x57 $x56)))
 (let (($x5849 (forall ((n (_ BitVec 6)) )(let ((?x8862 (stack_t x_0 x_1 x_2 x_3 w_1 0 n)))
 (let ((?x8539 (stack_s x_0 x_1 x_2 x_3 w_1 0 n)))
 (let (($x9902 (= ?x8539 ?x8862)))
 (let ((?x63 (sc_t 0)))
 (let (($x4036 (bvsle ?x63 n)))
 (or $x4036 $x9902)))))))
 ))
 (let ((?x63 (sc_t 0)))
 (let ((?x72 (sc_s 0)))
 (let (($x73 (= ?x72 ?x63)))
 (let (($x10206 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_t 1)))) (_ bv0 1)))))
 (let (($x1920 (exc_halt_t 1)))
 (let (($x8504 (= $x903 (or $x1920 $x10206))))
 (let (($x10307 (forall ((w (_ BitVec 256)) )(let ((?x10478 (storage_t x_0 x_1 x_2 x_3 w_1 1 w)))
 (let ((?x4223 (storage_t x_0 x_1 x_2 x_3 w_1 2 w)))
 (= ?x4223 ?x10478))))
 ))
 (let (($x11694 (forall ((n (_ BitVec 6)) )(let ((?x8963 (stack_t x_0 x_1 x_2 x_3 w_1 1 n)))
 (let ((?x11423 (stack_t x_0 x_1 x_2 x_3 w_1 2 n)))
 (let ((?x4023 (sc_t 1)))
 (let (($x10796 (bvsle ?x4023 n)))
 (or $x10796 (= ?x11423 ?x8963)))))))
 ))
 (let (($x7668 (= ?x4056 (bvadd (_ bv1 6) (sc_t 1)))))
 (let (($x3258 (= (used_gas_t x_0 x_1 x_2 x_3 w_1 2) (+ 3 (used_gas_t x_0 x_1 x_2 x_3 w_1 1)))))
 (let (($x9645 (forall ((w (_ BitVec 256)) )(let ((?x7748 (storage_t x_0 x_1 x_2 x_3 w_1 0 w)))
 (let ((?x10478 (storage_t x_0 x_1 x_2 x_3 w_1 1 w)))
 (= ?x10478 ?x7748))))
 ))
 (let (($x2659 (forall ((n (_ BitVec 6)) )(let ((?x8862 (stack_t x_0 x_1 x_2 x_3 w_1 0 n)))
 (let ((?x8963 (stack_t x_0 x_1 x_2 x_3 w_1 1 n)))
 (or (bvsle (bvadd (_ bv60 6) (sc_t 0)) n) (= ?x8963 ?x8862)))))
 ))
 (let (($x4169 (= (stack_t x_0 x_1 x_2 x_3 w_1 1 (bvadd (_ bv62 6) (sc_t 1))) (stack_t x_0 x_1 x_2 x_3 w_1 0 (bvadd (_ bv62 6) ?x63)))))
 (let (($x6035 (= (stack_t x_0 x_1 x_2 x_3 w_1 1 (bvadd (_ bv61 6) (sc_t 1))) (stack_t x_0 x_1 x_2 x_3 w_1 0 (bvadd (_ bv61 6) ?x63)))))
 (let (($x7264 (= (stack_t x_0 x_1 x_2 x_3 w_1 1 (bvadd (_ bv60 6) (sc_t 1))) (stack_t x_0 x_1 x_2 x_3 w_1 0 (bvadd (_ bv63 6) ?x63)))))
 (let (($x10193 (= (stack_t x_0 x_1 x_2 x_3 w_1 1 (bvadd (_ bv63 6) (sc_t 1))) (stack_t x_0 x_1 x_2 x_3 w_1 0 (bvadd (_ bv60 6) ?x63)))))
 (let (($x8134 (= $x64 (or (exc_halt_s 3) (not (bvsle (_ bv0 6) (bvadd (_ bv59 6) (sc_s 3))))))))
 (let (($x4805 (forall ((w (_ BitVec 256)) )(let ((?x2547 (storage_s x_0 x_1 x_2 x_3 w_1 3 w)))
 (let ((?x2862 (storage_s x_0 x_1 x_2 x_3 w_1 4 w)))
 (= ?x2862 ?x2547))))
 ))
 (let (($x115 (forall ((n (_ BitVec 6)) )(let ((?x7359 (stack_s x_0 x_1 x_2 x_3 w_1 3 n)))
 (let ((?x1676 (stack_s x_0 x_1 x_2 x_3 w_1 4 n)))
 (or (= ?x1676 ?x7359) (bvsle (bvadd (_ bv59 6) (sc_s 3)) n)))))
 ))
 (let ((?x275 (sc_s 3)))
 (let (($x2249 (= ?x4305 ?x275)))
 (let (($x9736 (= (used_gas_s x_0 x_1 x_2 x_3 w_1 4) (+ 3 (used_gas_s x_0 x_1 x_2 x_3 w_1 3)))))
 (let ((?x10770 (bvadd (_ bv62 6) ?x275)))
 (let ((?x1016 (stack_s x_0 x_1 x_2 x_3 w_1 3 ?x10770)))
 (let (($x6600 (= (stack_s x_0 x_1 x_2 x_3 w_1 4 (bvadd (_ bv61 6) ?x4305)) (stack_s x_0 x_1 x_2 x_3 w_1 3 (bvadd (_ bv61 6) ?x275)))))
 (let (($x764 (= (stack_s x_0 x_1 x_2 x_3 w_1 4 (bvadd (_ bv60 6) ?x4305)) (stack_s x_0 x_1 x_2 x_3 w_1 3 (bvadd (_ bv60 6) ?x275)))))
 (let ((?x7617 (bvadd (_ bv63 6) ?x275)))
 (let ((?x999 (stack_s x_0 x_1 x_2 x_3 w_1 3 ?x7617)))
 (let (($x1121 (= (stack_s x_0 x_1 x_2 x_3 w_1 4 (bvadd (_ bv63 6) ?x4305)) (stack_s x_0 x_1 x_2 x_3 w_1 3 (bvadd (_ bv59 6) ?x275)))))
 (let (($x292 (exc_halt_s 3)))
 (let (($x1454 (= $x292 (or (exc_halt_s 2) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 2))))))))
 (let (($x9972 (forall ((w (_ BitVec 256)) )(let ((?x11792 (storage_s x_0 x_1 x_2 x_3 w_1 2 w)))
 (let ((?x2547 (storage_s x_0 x_1 x_2 x_3 w_1 3 w)))
 (= ?x2547 ?x11792))))
 ))
 (let (($x8543 (forall ((n (_ BitVec 6)) )(let ((?x218 (sc_s 2)))
 (let ((?x2028 (bvadd (_ bv62 6) ?x218)))
 (let (($x9852 (bvsle ?x2028 n)))
 (let ((?x834 (stack_s x_0 x_1 x_2 x_3 w_1 2 n)))
 (let ((?x7359 (stack_s x_0 x_1 x_2 x_3 w_1 3 n)))
 (or (= ?x7359 ?x834) $x9852)))))))
 ))
 (let ((?x559 (used_gas_s x_0 x_1 x_2 x_3 w_1 3)))
 (let ((?x218 (sc_s 2)))
 (let ((?x3337 (bvadd (_ bv63 6) ?x218)))
 (let ((?x1484 (stack_s x_0 x_1 x_2 x_3 w_1 2 ?x3337)))
 (let (($x247 (exc_halt_s 2)))
 (let (($x10843 (= $x247 (or (exc_halt_s 1) (not (bvsle (_ bv0 6) (bvadd (_ bv59 6) (sc_s 1))))))))
 (let (($x4130 (forall ((w (_ BitVec 256)) )(let ((?x10511 (storage_s x_0 x_1 x_2 x_3 w_1 1 w)))
 (let ((?x11792 (storage_s x_0 x_1 x_2 x_3 w_1 2 w)))
 (= ?x11792 ?x10511))))
 ))
 (let (($x9569 (forall ((n (_ BitVec 6)) )(let ((?x8771 (stack_s x_0 x_1 x_2 x_3 w_1 1 n)))
 (let ((?x834 (stack_s x_0 x_1 x_2 x_3 w_1 2 n)))
 (or (= ?x834 ?x8771) (bvsle (bvadd (_ bv59 6) (sc_s 1)) n)))))
 ))
 (let ((?x154 (sc_s 1)))
 (let (($x2333 (= ?x218 ?x154)))
 (let ((?x291 (used_gas_s x_0 x_1 x_2 x_3 w_1 2)))
 (let ((?x2028 (bvadd (_ bv62 6) ?x218)))
 (let ((?x6458 (stack_s x_0 x_1 x_2 x_3 w_1 2 ?x2028)))
 (let (($x3676 (= (stack_s x_0 x_1 x_2 x_3 w_1 2 (bvadd (_ bv61 6) ?x218)) (stack_s x_0 x_1 x_2 x_3 w_1 1 (bvadd (_ bv61 6) ?x154)))))
 (let (($x3080 (= (stack_s x_0 x_1 x_2 x_3 w_1 2 (bvadd (_ bv60 6) ?x218)) (stack_s x_0 x_1 x_2 x_3 w_1 1 (bvadd (_ bv60 6) ?x154)))))
 (let (($x9342 (= (stack_s x_0 x_1 x_2 x_3 w_1 2 (bvadd (_ bv59 6) ?x218)) (stack_s x_0 x_1 x_2 x_3 w_1 1 (bvadd (_ bv63 6) ?x154)))))
 (let (($x189 (exc_halt_s 1)))
 (let (($x4103 (= $x189 (or $x57 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x72))) (_ bv0 1)))))))
 (let (($x322 (forall ((w (_ BitVec 256)) )(let ((?x39 (storage_s x_0 x_1 x_2 x_3 w_1 0 w)))
 (let ((?x10511 (storage_s x_0 x_1 x_2 x_3 w_1 1 w)))
 (= ?x10511 ?x39))))
 ))
 (let (($x3026 (forall ((n (_ BitVec 6)) )(let ((?x8539 (stack_s x_0 x_1 x_2 x_3 w_1 0 n)))
 (let ((?x8771 (stack_s x_0 x_1 x_2 x_3 w_1 1 n)))
 (let ((?x72 (sc_s 0)))
 (let (($x2948 (bvsle ?x72 n)))
 (or $x2948 (= ?x8771 ?x8539)))))))
 ))
 (let (($x6822 (= ?x154 (bvadd (_ bv1 6) ?x72))))
 (let (($x7475 (forall ((w (_ BitVec 256)) )(let ((?x39 (storage_s x_0 x_1 x_2 x_3 w_1 0 w)))
 (= ?x39 (_ bv0 256))))
 ))
 (let (($x1540 (= ?x6089 0)))
 (let (($x4562 (not $x57)))
 (let (($x6363 (= (stack_s x_0 x_1 x_2 x_3 w_1 0 (_ bv3 6)) x_3)))
 (let (($x4640 (= (stack_s x_0 x_1 x_2 x_3 w_1 0 (_ bv2 6)) x_2)))
 (let (($x10704 (= (stack_s x_0 x_1 x_2 x_3 w_1 0 (_ bv1 6)) x_1)))
 (let (($x4110 (= (stack_s x_0 x_1 x_2 x_3 w_1 0 (_ bv0 6)) x_0)))
 (let (($x1399 (= ?x72 (_ bv4 6))))
 (and $x1399 $x4110 $x10704 $x4640 $x6363 $x4562 $x1540 $x7475 (= (stack_s x_0 x_1 x_2 x_3 w_1 1 ?x72) w_1) (= (used_gas_s x_0 x_1 x_2 x_3 w_1 1) (+ 3 ?x6089)) $x6822 $x3026 $x322 $x4103 (= ?x1484 (stack_s x_0 x_1 x_2 x_3 w_1 1 (bvadd (_ bv59 6) ?x154))) $x9342 $x3080 $x3676 (= ?x6458 (stack_s x_0 x_1 x_2 x_3 w_1 1 (bvadd (_ bv62 6) ?x154))) (= ?x291 (+ 3 (used_gas_s x_0 x_1 x_2 x_3 w_1 1))) $x2333 $x9569 $x4130 $x10843 (= ?x999 ?x6458) (= ?x1016 ?x1484) (= ?x559 (+ 3 ?x291)) (= ?x275 ?x218) $x8543 $x9972 $x1454 $x1121 (= (stack_s x_0 x_1 x_2 x_3 w_1 4 (bvadd (_ bv59 6) ?x4305)) ?x999) $x764 $x6600 (= (stack_s x_0 x_1 x_2 x_3 w_1 4 (bvadd (_ bv62 6) ?x4305)) ?x1016) $x9736 $x2249 $x115 $x4805 $x8134 $x10193 $x7264 $x6035 $x4169 (= (used_gas_t x_0 x_1 x_2 x_3 w_1 1) (+ 3 ?x1808)) (= (sc_t 1) ?x63) $x2659 $x9645 (= $x1920 (or $x56 (not (bvsle (_ bv0 6) (bvadd (_ bv60 6) ?x63))))) (= (stack_t x_0 x_1 x_2 x_3 w_1 2 (sc_t 1)) w_1) $x3258 $x7668 $x11694 $x10307 $x8504 $x73 $x5849 $x58 $x2994 $x3125 (not (and $x1495 $x7821 $x488 $x10617))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
 )
(check-sat)
(get-proof)