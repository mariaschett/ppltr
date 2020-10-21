; SWAP3 AND SWAP1 SWAP2 OR => SWAP2 SWAP3 AND OR
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
(assert
 (exists ((x_0 (_ BitVec 256)) (x_1 (_ BitVec 256)) (x_2 (_ BitVec 256)) (x_3 (_ BitVec 256)) )(let (($x6566 (forall ((w (_ BitVec 256)) )(let ((?x10325 (storage_t x_0 x_1 x_2 x_3 4 w)))
 (let ((?x9813 (storage_s x_0 x_1 x_2 x_3 5 w)))
 (= ?x9813 ?x10325))))
 ))
 (let (($x566 (exc_halt_t 4)))
 (let (($x3979 (exc_halt_s 5)))
 (let (($x10168 (= $x3979 $x566)))
 (let (($x6081 (forall ((n (_ BitVec 6)) )(let ((?x518 (sc_t 4)))
 (let (($x9265 (bvsle ?x518 n)))
 (let ((?x10326 (stack_t x_0 x_1 x_2 x_3 4 n)))
 (let ((?x9781 (stack_s x_0 x_1 x_2 x_3 5 n)))
 (let (($x6301 (= ?x9781 ?x10326)))
 (or $x6301 $x9265)))))))
 ))
 (let ((?x518 (sc_t 4)))
 (let ((?x805 (sc_s 5)))
 (let (($x5034 (= ?x805 ?x518)))
 (let ((?x10316 (used_gas_t x_0 x_1 x_2 x_3 0)))
 (let ((?x10320 (used_gas_s x_0 x_1 x_2 x_3 0)))
 (let (($x10317 (= ?x10320 ?x10316)))
 (let (($x10314 (forall ((w (_ BitVec 256)) )(let ((?x10321 (storage_t x_0 x_1 x_2 x_3 0 w)))
 (let ((?x10313 (storage_s x_0 x_1 x_2 x_3 0 w)))
 (= ?x10313 ?x10321))))
 ))
 (let (($x56 (exc_halt_t 0)))
 (let (($x57 (exc_halt_s 0)))
 (let (($x58 (= $x57 $x56)))
 (let (($x657 (forall ((n (_ BitVec 6)) )(let ((?x10305 (stack_t x_0 x_1 x_2 x_3 0 n)))
 (let ((?x10306 (stack_s x_0 x_1 x_2 x_3 0 n)))
 (let (($x10307 (= ?x10306 ?x10305)))
 (or (bvsle (sc_t 0) n) $x10307)))))
 ))
 (let ((?x63 (sc_t 0)))
 (let ((?x72 (sc_s 0)))
 (let (($x73 (= ?x72 ?x63)))
 (let (($x9039 (= $x566 (or (exc_halt_t 3) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_t 3))))))))
 (let (($x8610 (forall ((w (_ BitVec 256)) )(let ((?x3064 (storage_t x_0 x_1 x_2 x_3 3 w)))
 (let ((?x10325 (storage_t x_0 x_1 x_2 x_3 4 w)))
 (= ?x10325 ?x3064))))
 ))
 (let (($x6033 (forall ((n (_ BitVec 6)) )(let ((?x10310 (stack_t x_0 x_1 x_2 x_3 3 n)))
 (let ((?x10326 (stack_t x_0 x_1 x_2 x_3 4 n)))
 (let (($x3348 (= ?x10326 ?x10310)))
 (or $x3348 (bvsle (bvadd (_ bv62 6) (sc_t 3)) n))))))
 ))
 (let ((?x2012 (sc_t 3)))
 (let ((?x9681 (bvadd (_ bv63 6) ?x2012)))
 (let (($x9587 (= ?x518 ?x9681)))
 (let ((?x9584 (used_gas_t x_0 x_1 x_2 x_3 4)))
 (let ((?x9698 (bvadd (_ bv62 6) ?x2012)))
 (let ((?x9549 (stack_t x_0 x_1 x_2 x_3 3 ?x9698)))
 (let ((?x10167 (stack_t x_0 x_1 x_2 x_3 3 ?x9681)))
 (let (($x6045 (= (stack_t x_0 x_1 x_2 x_3 4 (bvadd (_ bv63 6) ?x518)) (bvor ?x10167 ?x9549))))
 (let (($x10336 (exc_halt_t 3)))
 (let (($x9411 (= $x10336 (or (exc_halt_t 2) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_t 2))))))))
 (let (($x9575 (forall ((w (_ BitVec 256)) )(let ((?x10294 (storage_t x_0 x_1 x_2 x_3 2 w)))
 (let ((?x3064 (storage_t x_0 x_1 x_2 x_3 3 w)))
 (= ?x3064 ?x10294))))
 ))
 (let (($x9570 (forall ((n (_ BitVec 6)) )(let ((?x10284 (stack_t x_0 x_1 x_2 x_3 2 n)))
 (let ((?x10310 (stack_t x_0 x_1 x_2 x_3 3 n)))
 (or (bvsle (bvadd (_ bv62 6) (sc_t 2)) n) (= ?x10310 ?x10284)))))
 ))
 (let ((?x10296 (used_gas_t x_0 x_1 x_2 x_3 3)))
 (let (($x9548 (= ?x10296 (+ 3 (used_gas_t x_0 x_1 x_2 x_3 2)))))
 (let ((?x6005 (bvor (bvnot (stack_t x_0 x_1 x_2 x_3 2 (bvadd (_ bv63 6) (sc_t 2)))) (bvnot (stack_t x_0 x_1 x_2 x_3 2 (bvadd (_ bv62 6) (sc_t 2)))))))
 (let (($x903 (exc_halt_t 2)))
 (let (($x7754 (= $x903 (or (exc_halt_t 1) (not (bvsle (_ bv0 6) (bvadd (_ bv60 6) (sc_t 1))))))))
 (let (($x9666 (forall ((w (_ BitVec 256)) )(let ((?x4087 (storage_t x_0 x_1 x_2 x_3 1 w)))
 (let ((?x10294 (storage_t x_0 x_1 x_2 x_3 2 w)))
 (= ?x10294 ?x4087))))
 ))
 (let (($x6469 (forall ((n (_ BitVec 6)) )(let ((?x4801 (stack_t x_0 x_1 x_2 x_3 1 n)))
 (let ((?x10284 (stack_t x_0 x_1 x_2 x_3 2 n)))
 (let (($x3995 (= ?x10284 ?x4801)))
 (or $x3995 (bvsle (bvadd (_ bv60 6) (sc_t 1)) n))))))
 ))
 (let ((?x4023 (sc_t 1)))
 (let ((?x4056 (sc_t 2)))
 (let (($x9387 (= ?x4056 ?x4023)))
 (let ((?x8899 (used_gas_t x_0 x_1 x_2 x_3 2)))
 (let (($x9751 (= ?x8899 (+ 3 (used_gas_t x_0 x_1 x_2 x_3 1)))))
 (let ((?x6517 (stack_t x_0 x_1 x_2 x_3 1 (bvadd (_ bv62 6) ?x4023))))
 (let ((?x10177 (bvadd (_ bv62 6) ?x4056)))
 (let ((?x9516 (stack_t x_0 x_1 x_2 x_3 2 ?x10177)))
 (let ((?x9009 (stack_t x_0 x_1 x_2 x_3 1 (bvadd (_ bv61 6) ?x4023))))
 (let ((?x9435 (bvadd (_ bv63 6) ?x4023)))
 (let ((?x9461 (stack_t x_0 x_1 x_2 x_3 1 ?x9435)))
 (let ((?x9462 (bvadd (_ bv63 6) ?x4056)))
 (let ((?x4404 (stack_t x_0 x_1 x_2 x_3 2 ?x9462)))
 (let (($x853 (forall ((w (_ BitVec 256)) )(let ((?x10321 (storage_t x_0 x_1 x_2 x_3 0 w)))
 (let ((?x4087 (storage_t x_0 x_1 x_2 x_3 1 w)))
 (= ?x4087 ?x10321))))
 ))
 (let (($x6522 (forall ((n (_ BitVec 6)) )(let ((?x10305 (stack_t x_0 x_1 x_2 x_3 0 n)))
 (let ((?x4801 (stack_t x_0 x_1 x_2 x_3 1 n)))
 (let (($x851 (= ?x4801 ?x10305)))
 (or $x851 (bvsle (bvadd (_ bv61 6) (sc_t 0)) n))))))
 ))
 (let (($x3942 (= $x3979 (or (exc_halt_s 4) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 4))))))))
 (let (($x9463 (forall ((w (_ BitVec 256)) )(let ((?x9870 (storage_s x_0 x_1 x_2 x_3 4 w)))
 (let ((?x9813 (storage_s x_0 x_1 x_2 x_3 5 w)))
 (= ?x9813 ?x9870))))
 ))
 (let (($x9550 (forall ((n (_ BitVec 6)) )(let ((?x9439 (stack_s x_0 x_1 x_2 x_3 4 n)))
 (let ((?x9781 (stack_s x_0 x_1 x_2 x_3 5 n)))
 (or (= ?x9781 ?x9439) (bvsle (bvadd (_ bv62 6) (sc_s 4)) n)))))
 ))
 (let ((?x4305 (sc_s 4)))
 (let ((?x9580 (bvadd (_ bv63 6) ?x4305)))
 (let (($x9278 (= ?x805 ?x9580)))
 (let ((?x1248 (used_gas_s x_0 x_1 x_2 x_3 5)))
 (let (($x3902 (= ?x1248 (+ 3 (used_gas_s x_0 x_1 x_2 x_3 4)))))
 (let ((?x8894 (bvadd (_ bv62 6) ?x4305)))
 (let ((?x9432 (stack_s x_0 x_1 x_2 x_3 4 ?x8894)))
 (let ((?x9582 (stack_s x_0 x_1 x_2 x_3 4 ?x9580)))
 (let ((?x9564 (bvadd (_ bv63 6) ?x805)))
 (let ((?x3522 (stack_s x_0 x_1 x_2 x_3 5 ?x9564)))
 (let (($x64 (exc_halt_s 4)))
 (let (($x3680 (= $x64 (or (exc_halt_s 3) (not (bvsle (_ bv0 6) (bvadd (_ bv61 6) (sc_s 3))))))))
 (let (($x9540 (forall ((w (_ BitVec 256)) )(let ((?x9915 (storage_s x_0 x_1 x_2 x_3 3 w)))
 (let ((?x9870 (storage_s x_0 x_1 x_2 x_3 4 w)))
 (= ?x9870 ?x9915))))
 ))
 (let (($x9547 (forall ((n (_ BitVec 6)) )(let ((?x10448 (stack_s x_0 x_1 x_2 x_3 3 n)))
 (let ((?x9439 (stack_s x_0 x_1 x_2 x_3 4 n)))
 (or (bvsle (bvadd (_ bv61 6) (sc_s 3)) n) (= ?x9439 ?x10448)))))
 ))
 (let ((?x275 (sc_s 3)))
 (let (($x3560 (= ?x4305 ?x275)))
 (let ((?x2806 (used_gas_s x_0 x_1 x_2 x_3 4)))
 (let (($x633 (= ?x2806 (+ 3 (used_gas_s x_0 x_1 x_2 x_3 3)))))
 (let ((?x9694 (stack_s x_0 x_1 x_2 x_3 3 (bvadd (_ bv62 6) ?x275))))
 (let (($x3508 (= ?x9432 ?x9694)))
 (let ((?x9612 (stack_s x_0 x_1 x_2 x_3 3 (bvadd (_ bv63 6) ?x275))))
 (let (($x9577 (= (stack_s x_0 x_1 x_2 x_3 4 (bvadd (_ bv61 6) ?x4305)) ?x9612)))
 (let (($x3565 (= ?x9582 (stack_s x_0 x_1 x_2 x_3 3 (bvadd (_ bv61 6) ?x275)))))
 (let (($x292 (exc_halt_s 3)))
 (let (($x8035 (= $x292 (or (exc_halt_s 2) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 2))))))))
 (let (($x9615 (forall ((w (_ BitVec 256)) )(let ((?x901 (storage_s x_0 x_1 x_2 x_3 2 w)))
 (let ((?x9915 (storage_s x_0 x_1 x_2 x_3 3 w)))
 (= ?x9915 ?x901))))
 ))
 (let (($x4080 (forall ((n (_ BitVec 6)) )(let ((?x9409 (stack_s x_0 x_1 x_2 x_3 2 n)))
 (let ((?x10448 (stack_s x_0 x_1 x_2 x_3 3 n)))
 (let (($x9598 (= ?x10448 ?x9409)))
 (or (bvsle (bvadd (_ bv62 6) (sc_s 2)) n) $x9598)))))
 ))
 (let ((?x218 (sc_s 2)))
 (let (($x9592 (= ?x275 ?x218)))
 (let ((?x875 (used_gas_s x_0 x_1 x_2 x_3 3)))
 (let (($x9588 (= ?x875 (+ 3 (used_gas_s x_0 x_1 x_2 x_3 2)))))
 (let (($x247 (exc_halt_s 2)))
 (let (($x9594 (= $x247 (or (exc_halt_s 1) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 1))))))))
 (let (($x9270 (forall ((w (_ BitVec 256)) )(let ((?x2132 (storage_s x_0 x_1 x_2 x_3 1 w)))
 (let ((?x901 (storage_s x_0 x_1 x_2 x_3 2 w)))
 (= ?x901 ?x2132))))
 ))
 (let (($x4957 (forall ((n (_ BitVec 6)) )(let ((?x792 (stack_s x_0 x_1 x_2 x_3 1 n)))
 (let ((?x9409 (stack_s x_0 x_1 x_2 x_3 2 n)))
 (or (= ?x9409 ?x792) (bvsle (bvadd (_ bv62 6) (sc_s 1)) n)))))
 ))
 (let ((?x9512 (used_gas_s x_0 x_1 x_2 x_3 2)))
 (let (($x8903 (= ?x9512 (+ 3 (used_gas_s x_0 x_1 x_2 x_3 1)))))
 (let ((?x8965 (bvor (bvnot (stack_s x_0 x_1 x_2 x_3 1 (bvadd (_ bv63 6) (sc_s 1)))) (bvnot (stack_s x_0 x_1 x_2 x_3 1 (bvadd (_ bv62 6) (sc_s 1)))))))
 (let ((?x9655 (bvadd (_ bv63 6) ?x218)))
 (let ((?x9621 (stack_s x_0 x_1 x_2 x_3 2 ?x9655)))
 (let (($x9619 (forall ((w (_ BitVec 256)) )(let ((?x10313 (storage_s x_0 x_1 x_2 x_3 0 w)))
 (let ((?x2132 (storage_s x_0 x_1 x_2 x_3 1 w)))
 (= ?x2132 ?x10313))))
 ))
 (let (($x354 (forall ((n (_ BitVec 6)) )(let ((?x10306 (stack_s x_0 x_1 x_2 x_3 0 n)))
 (let ((?x792 (stack_s x_0 x_1 x_2 x_3 1 n)))
 (let (($x211 (= ?x792 ?x10306)))
 (or $x211 (bvsle (bvadd (_ bv60 6) (sc_s 0)) n))))))
 ))
 (let ((?x154 (sc_s 1)))
 (let (($x703 (= ?x154 ?x72)))
 (let ((?x934 (used_gas_s x_0 x_1 x_2 x_3 1)))
 (let (($x768 (= ?x934 (+ 3 ?x10320))))
 (let ((?x9695 (bvadd (_ bv62 6) ?x154)))
 (let ((?x9630 (stack_s x_0 x_1 x_2 x_3 1 ?x9695)))
 (let (($x9627 (= ?x9630 (stack_s x_0 x_1 x_2 x_3 0 (bvadd (_ bv62 6) ?x72)))))
 (let ((?x9634 (bvadd (_ bv61 6) ?x72)))
 (let ((?x3469 (stack_s x_0 x_1 x_2 x_3 0 ?x9634)))
 (let ((?x9643 (stack_s x_0 x_1 x_2 x_3 1 (bvadd (_ bv61 6) ?x154))))
 (let ((?x9641 (stack_s x_0 x_1 x_2 x_3 0 (bvadd (_ bv63 6) ?x72))))
 (let ((?x9649 (bvadd (_ bv63 6) ?x154)))
 (let ((?x9651 (stack_s x_0 x_1 x_2 x_3 1 ?x9649)))
 (let (($x3920 (forall ((w (_ BitVec 256)) )(let ((?x10313 (storage_s x_0 x_1 x_2 x_3 0 w)))
 (= ?x10313 (_ bv0 256))))
 ))
 (let (($x553 (= ?x10320 0)))
 (let (($x5028 (not $x57)))
 (let (($x6090 (= (stack_s x_0 x_1 x_2 x_3 0 (_ bv3 6)) x_3)))
 (let (($x9685 (= (stack_s x_0 x_1 x_2 x_3 0 (_ bv2 6)) x_2)))
 (let (($x9677 (= (stack_s x_0 x_1 x_2 x_3 0 (_ bv1 6)) x_1)))
 (let (($x9678 (= (stack_s x_0 x_1 x_2 x_3 0 (_ bv0 6)) x_0)))
 (let (($x950 (= ?x72 (_ bv4 6))))
 (and $x950 $x9678 $x9677 $x9685 $x6090 $x5028 $x553 $x3920 (= ?x9651 (stack_s x_0 x_1 x_2 x_3 0 (bvadd (_ bv60 6) ?x72))) (= (stack_s x_0 x_1 x_2 x_3 1 (bvadd (_ bv60 6) ?x154)) ?x9641) (= ?x9643 ?x3469) $x9627 $x768 $x703 $x354 $x9619 (= (exc_halt_s 1) (or $x57 (not (bvsle (_ bv0 6) (bvadd (_ bv60 6) ?x72))))) (= ?x9621 (bvnot ?x8965)) $x8903 (= ?x218 ?x9649) $x4957 $x9270 $x9594 (= ?x9612 (stack_s x_0 x_1 x_2 x_3 2 (bvadd (_ bv62 6) ?x218))) (= ?x9694 ?x9621) $x9588 $x9592 $x4080 $x9615 $x8035 $x3565 $x9577 $x3508 $x633 $x3560 $x9547 $x9540 $x3680 (= ?x3522 (bvor ?x9582 ?x9432)) $x3902 $x9278 $x9550 $x9463 $x3942 (= ?x9461 (stack_t x_0 x_1 x_2 x_3 0 (bvadd (_ bv61 6) ?x63))) (= ?x9009 (stack_t x_0 x_1 x_2 x_3 0 (bvadd (_ bv63 6) ?x63))) (= ?x6517 (stack_t x_0 x_1 x_2 x_3 0 (bvadd (_ bv62 6) ?x63))) (= (used_gas_t x_0 x_1 x_2 x_3 1) (+ 3 ?x10316)) (= ?x4023 ?x63) $x6522 $x853 (= (exc_halt_t 1) (or $x56 (not (bvsle (_ bv0 6) (bvadd (_ bv61 6) ?x63))))) (= ?x4404 (stack_t x_0 x_1 x_2 x_3 1 (bvadd (_ bv60 6) ?x4023))) (= (stack_t x_0 x_1 x_2 x_3 2 (bvadd (_ bv60 6) ?x4056)) ?x9461) (= (stack_t x_0 x_1 x_2 x_3 2 (bvadd (_ bv61 6) ?x4056)) ?x9009) (= ?x9516 ?x6517) $x9751 $x9387 $x6469 $x9666 $x7754 (= ?x10167 (bvnot ?x6005)) $x9548 (= ?x2012 ?x9462) $x9570 $x9575 $x9411 $x6045 (= ?x9584 (+ 3 ?x10296)) $x9587 $x6033 $x8610 $x9039 $x73 $x657 $x58 $x10314 $x10317 (not (and $x5034 $x6081 $x10168 $x6566))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
 )
(check-sat)
(get-proof)
