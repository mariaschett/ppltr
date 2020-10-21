; SWAP1 SWAP3 AND SWAP2 => DUP4 AND SWAP3 POP
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
 (exists ((x_0 (_ BitVec 256)) (x_1 (_ BitVec 256)) (x_2 (_ BitVec 256)) (x_3 (_ BitVec 256)) )(let (($x9267 (forall ((w (_ BitVec 256)) )(let ((?x9264 (storage_t x_0 x_1 x_2 x_3 4 w)))
 (let ((?x9265 (storage_s x_0 x_1 x_2 x_3 4 w)))
 (= ?x9265 ?x9264))))
 ))
 (let (($x3900 (exc_halt_t 4)))
 (let (($x64 (exc_halt_s 4)))
 (let (($x8922 (= $x64 $x3900)))
 (let (($x9703 (forall ((n (_ BitVec 6)) )(let ((?x9269 (stack_t x_0 x_1 x_2 x_3 4 n)))
 (let ((?x9270 (stack_s x_0 x_1 x_2 x_3 4 n)))
 (let (($x9271 (= ?x9270 ?x9269)))
 (let ((?x3920 (sc_t 4)))
 (let (($x53 (bvsle ?x3920 n)))
 (or $x53 $x9271)))))))
 ))
 (let ((?x3920 (sc_t 4)))
 (let ((?x4305 (sc_s 4)))
 (let (($x8875 (= ?x4305 ?x3920)))
 (let ((?x9277 (used_gas_t x_0 x_1 x_2 x_3 0)))
 (let ((?x9278 (used_gas_s x_0 x_1 x_2 x_3 0)))
 (let (($x9279 (= ?x9278 ?x9277)))
 (let (($x9284 (forall ((w (_ BitVec 256)) )(let ((?x9281 (storage_t x_0 x_1 x_2 x_3 0 w)))
 (let ((?x9282 (storage_s x_0 x_1 x_2 x_3 0 w)))
 (= ?x9282 ?x9281))))
 ))
 (let (($x56 (exc_halt_t 0)))
 (let (($x57 (exc_halt_s 0)))
 (let (($x58 (= $x57 $x56)))
 (let (($x9701 (forall ((n (_ BitVec 6)) )(let ((?x9286 (stack_t x_0 x_1 x_2 x_3 0 n)))
 (let ((?x9287 (stack_s x_0 x_1 x_2 x_3 0 n)))
 (let (($x9288 (= ?x9287 ?x9286)))
 (let ((?x63 (sc_t 0)))
 (let (($x3470 (bvsle ?x63 n)))
 (or $x3470 $x9288)))))))
 ))
 (let ((?x63 (sc_t 0)))
 (let ((?x72 (sc_s 0)))
 (let (($x73 (= ?x72 ?x63)))
 (let (($x5037 (= $x3900 (or (exc_halt_t 3) (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_t 3))))))))
 (let (($x9698 (forall ((w (_ BitVec 256)) )(let ((?x9294 (storage_t x_0 x_1 x_2 x_3 3 w)))
 (let ((?x9264 (storage_t x_0 x_1 x_2 x_3 4 w)))
 (= ?x9264 ?x9294))))
 ))
 (let (($x9697 (forall ((n (_ BitVec 6)) )(let ((?x9298 (stack_t x_0 x_1 x_2 x_3 3 n)))
 (let ((?x9269 (stack_t x_0 x_1 x_2 x_3 4 n)))
 (let ((?x2012 (sc_t 3)))
 (let ((?x3250 (bvadd (_ bv63 6) ?x2012)))
 (let (($x5028 (bvsle ?x3250 n)))
 (or $x5028 (= ?x9269 ?x9298))))))))
 ))
 (let ((?x2012 (sc_t 3)))
 (let ((?x3250 (bvadd (_ bv63 6) ?x2012)))
 (let (($x747 (= ?x3920 ?x3250)))
 (let (($x9693 (= (used_gas_t x_0 x_1 x_2 x_3 4) (+ 2 (used_gas_t x_0 x_1 x_2 x_3 3)))))
 (let (($x4125 (exc_halt_t 3)))
 (let (($x516 (= $x4125 (or (exc_halt_t 2) (not (bvsle (_ bv0 6) (bvadd (_ bv60 6) (sc_t 2))))))))
 (let (($x9690 (forall ((w (_ BitVec 256)) )(let ((?x9312 (storage_t x_0 x_1 x_2 x_3 2 w)))
 (let ((?x9294 (storage_t x_0 x_1 x_2 x_3 3 w)))
 (= ?x9294 ?x9312))))
 ))
 (let (($x9689 (forall ((n (_ BitVec 6)) )(let ((?x9316 (stack_t x_0 x_1 x_2 x_3 2 n)))
 (let ((?x9298 (stack_t x_0 x_1 x_2 x_3 3 n)))
 (let ((?x4056 (sc_t 2)))
 (let ((?x556 (bvadd (_ bv60 6) ?x4056)))
 (let (($x553 (bvsle ?x556 n)))
 (or $x553 (= ?x9298 ?x9316))))))))
 ))
 (let ((?x4056 (sc_t 2)))
 (let (($x2216 (= ?x2012 ?x4056)))
 (let ((?x9306 (used_gas_t x_0 x_1 x_2 x_3 3)))
 (let (($x9683 (= (stack_t x_0 x_1 x_2 x_3 3 (bvadd (_ bv62 6) ?x2012)) (stack_t x_0 x_1 x_2 x_3 2 (bvadd (_ bv62 6) ?x4056)))))
 (let (($x9678 (= (stack_t x_0 x_1 x_2 x_3 3 (bvadd (_ bv61 6) ?x2012)) (stack_t x_0 x_1 x_2 x_3 2 (bvadd (_ bv61 6) ?x4056)))))
 (let ((?x2700 (bvadd (_ bv63 6) ?x4056)))
 (let ((?x9651 (stack_t x_0 x_1 x_2 x_3 2 ?x2700)))
 (let (($x9670 (= (stack_t x_0 x_1 x_2 x_3 3 ?x3250) (stack_t x_0 x_1 x_2 x_3 2 (bvadd (_ bv60 6) ?x4056)))))
 (let (($x903 (exc_halt_t 2)))
 (let (($x1620 (= $x903 (or (exc_halt_t 1) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_t 1))))))))
 (let (($x9665 (forall ((w (_ BitVec 256)) )(let ((?x9345 (storage_t x_0 x_1 x_2 x_3 1 w)))
 (let ((?x9312 (storage_t x_0 x_1 x_2 x_3 2 w)))
 (= ?x9312 ?x9345))))
 ))
 (let (($x9664 (forall ((n (_ BitVec 6)) )(let ((?x9349 (stack_t x_0 x_1 x_2 x_3 1 n)))
 (let ((?x9316 (stack_t x_0 x_1 x_2 x_3 2 n)))
 (let ((?x4023 (sc_t 1)))
 (let ((?x3017 (bvadd (_ bv62 6) ?x4023)))
 (let (($x1831 (bvsle ?x3017 n)))
 (or $x1831 (= ?x9316 ?x9349))))))))
 ))
 (let ((?x4023 (sc_t 1)))
 (let ((?x2367 (bvadd (_ bv63 6) ?x4023)))
 (let (($x2323 (= ?x4056 ?x2367)))
 (let ((?x9324 (used_gas_t x_0 x_1 x_2 x_3 2)))
 (let ((?x9656 (bvor (bvnot (stack_t x_0 x_1 x_2 x_3 1 ?x2367)) (bvnot (stack_t x_0 x_1 x_2 x_3 1 (bvadd (_ bv62 6) ?x4023))))))
 (let (($x3418 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x63))) (_ bv0 1)))))
 (let (($x1920 (exc_halt_t 1)))
 (let (($x9643 (forall ((w (_ BitVec 256)) )(let ((?x9281 (storage_t x_0 x_1 x_2 x_3 0 w)))
 (let ((?x9345 (storage_t x_0 x_1 x_2 x_3 1 w)))
 (= ?x9345 ?x9281))))
 ))
 (let (($x9642 (forall ((n (_ BitVec 6)) )(let ((?x9286 (stack_t x_0 x_1 x_2 x_3 0 n)))
 (let ((?x9349 (stack_t x_0 x_1 x_2 x_3 1 n)))
 (or (bvsle (bvadd (_ bv60 6) (sc_t 0)) n) (= ?x9349 ?x9286)))))
 ))
 (let (($x3469 (= ?x4023 (bvadd (_ bv1 6) ?x63))))
 (let (($x9635 (= (stack_t x_0 x_1 x_2 x_3 1 (bvadd (_ bv63 6) ?x63)) (stack_t x_0 x_1 x_2 x_3 0 (bvadd (_ bv63 6) ?x63)))))
 (let (($x9631 (= (stack_t x_0 x_1 x_2 x_3 1 (bvadd (_ bv62 6) ?x63)) (stack_t x_0 x_1 x_2 x_3 0 (bvadd (_ bv62 6) ?x63)))))
 (let (($x9627 (= (stack_t x_0 x_1 x_2 x_3 1 (bvadd (_ bv61 6) ?x63)) (stack_t x_0 x_1 x_2 x_3 0 (bvadd (_ bv61 6) ?x63)))))
 (let ((?x9619 (bvadd (_ bv60 6) ?x63)))
 (let ((?x9620 (stack_t x_0 x_1 x_2 x_3 0 ?x9619)))
 (let (($x7618 (= $x64 (or (exc_halt_s 3) (not (bvsle (_ bv0 6) (bvadd (_ bv61 6) (sc_s 3))))))))
 (let (($x9615 (forall ((w (_ BitVec 256)) )(let ((?x9399 (storage_s x_0 x_1 x_2 x_3 3 w)))
 (let ((?x9265 (storage_s x_0 x_1 x_2 x_3 4 w)))
 (= ?x9265 ?x9399))))
 ))
 (let (($x9614 (forall ((n (_ BitVec 6)) )(let ((?x9403 (stack_s x_0 x_1 x_2 x_3 3 n)))
 (let ((?x9270 (stack_s x_0 x_1 x_2 x_3 4 n)))
 (let ((?x275 (sc_s 3)))
 (let ((?x3081 (bvadd (_ bv61 6) ?x275)))
 (let (($x7609 (bvsle ?x3081 n)))
 (or $x7609 (= ?x9270 ?x9403))))))))
 ))
 (let ((?x275 (sc_s 3)))
 (let (($x4957 (= ?x4305 ?x275)))
 (let (($x9610 (= (used_gas_s x_0 x_1 x_2 x_3 4) (+ 3 (used_gas_s x_0 x_1 x_2 x_3 3)))))
 (let (($x9607 (= (stack_s x_0 x_1 x_2 x_3 4 (bvadd (_ bv62 6) ?x4305)) (stack_s x_0 x_1 x_2 x_3 3 (bvadd (_ bv62 6) ?x275)))))
 (let ((?x3627 (bvadd (_ bv63 6) ?x275)))
 (let ((?x9579 (stack_s x_0 x_1 x_2 x_3 3 ?x3627)))
 (let (($x9599 (= (stack_s x_0 x_1 x_2 x_3 4 (bvadd (_ bv63 6) ?x4305)) (stack_s x_0 x_1 x_2 x_3 3 (bvadd (_ bv61 6) ?x275)))))
 (let (($x292 (exc_halt_s 3)))
 (let (($x9594 (= $x292 (or (exc_halt_s 2) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 2))))))))
 (let (($x9592 (forall ((w (_ BitVec 256)) )(let ((?x9428 (storage_s x_0 x_1 x_2 x_3 2 w)))
 (let ((?x9399 (storage_s x_0 x_1 x_2 x_3 3 w)))
 (= ?x9399 ?x9428))))
 ))
 (let (($x9591 (forall ((n (_ BitVec 6)) )(let ((?x9432 (stack_s x_0 x_1 x_2 x_3 2 n)))
 (let ((?x9403 (stack_s x_0 x_1 x_2 x_3 3 n)))
 (let ((?x218 (sc_s 2)))
 (let ((?x3565 (bvadd (_ bv62 6) ?x218)))
 (let (($x8603 (bvsle ?x3565 n)))
 (or $x8603 (= ?x9403 ?x9432))))))))
 ))
 (let ((?x9411 (used_gas_s x_0 x_1 x_2 x_3 3)))
 (let ((?x9582 (bvor (bvnot (stack_s x_0 x_1 x_2 x_3 2 (bvadd (_ bv63 6) (sc_s 2)))) (bvnot (stack_s x_0 x_1 x_2 x_3 2 (bvadd (_ bv62 6) (sc_s 2)))))))
 (let (($x247 (exc_halt_s 2)))
 (let (($x8900 (= $x247 (or (exc_halt_s 1) (not (bvsle (_ bv0 6) (bvadd (_ bv60 6) (sc_s 1))))))))
 (let (($x9577 (forall ((w (_ BitVec 256)) )(let ((?x9451 (storage_s x_0 x_1 x_2 x_3 1 w)))
 (let ((?x9428 (storage_s x_0 x_1 x_2 x_3 2 w)))
 (= ?x9428 ?x9451))))
 ))
 (let (($x9576 (forall ((n (_ BitVec 6)) )(let ((?x9455 (stack_s x_0 x_1 x_2 x_3 1 n)))
 (let ((?x9432 (stack_s x_0 x_1 x_2 x_3 2 n)))
 (let ((?x154 (sc_s 1)))
 (let ((?x696 (bvadd (_ bv60 6) ?x154)))
 (let (($x8894 (bvsle ?x696 n)))
 (or $x8894 (= ?x9432 ?x9455))))))))
 ))
 (let ((?x154 (sc_s 1)))
 (let ((?x218 (sc_s 2)))
 (let (($x3567 (= ?x218 ?x154)))
 (let ((?x9440 (used_gas_s x_0 x_1 x_2 x_3 2)))
 (let ((?x3520 (bvadd (_ bv62 6) ?x154)))
 (let ((?x9542 (stack_s x_0 x_1 x_2 x_3 1 ?x3520)))
 (let ((?x3565 (bvadd (_ bv62 6) ?x218)))
 (let ((?x9569 (stack_s x_0 x_1 x_2 x_3 2 ?x3565)))
 (let (($x9567 (= (stack_s x_0 x_1 x_2 x_3 2 (bvadd (_ bv61 6) ?x218)) (stack_s x_0 x_1 x_2 x_3 1 (bvadd (_ bv61 6) ?x154)))))
 (let ((?x3496 (bvadd (_ bv63 6) ?x154)))
 (let ((?x9536 (stack_s x_0 x_1 x_2 x_3 1 ?x3496)))
 (let ((?x3560 (bvadd (_ bv63 6) ?x218)))
 (let ((?x9554 (stack_s x_0 x_1 x_2 x_3 2 ?x3560)))
 (let (($x189 (exc_halt_s 1)))
 (let (($x6799 (= $x189 (or $x57 (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) ?x72)))))))
 (let (($x9552 (forall ((w (_ BitVec 256)) )(let ((?x9282 (storage_s x_0 x_1 x_2 x_3 0 w)))
 (let ((?x9451 (storage_s x_0 x_1 x_2 x_3 1 w)))
 (= ?x9451 ?x9282))))
 ))
 (let (($x9551 (forall ((n (_ BitVec 6)) )(let ((?x9287 (stack_s x_0 x_1 x_2 x_3 0 n)))
 (let ((?x9455 (stack_s x_0 x_1 x_2 x_3 1 n)))
 (let ((?x72 (sc_s 0)))
 (let ((?x3525 (bvadd (_ bv62 6) ?x72)))
 (let (($x1720 (bvsle ?x3525 n)))
 (or $x1720 (= ?x9455 ?x9287))))))))
 ))
 (let (($x3552 (= ?x154 ?x72)))
 (let (($x9508 (forall ((w (_ BitVec 256)) )(let ((?x9282 (storage_s x_0 x_1 x_2 x_3 0 w)))
 (= ?x9282 (_ bv0 256))))
 ))
 (let (($x9509 (= ?x9278 0)))
 (let (($x3503 (not $x57)))
 (let (($x9513 (= (stack_s x_0 x_1 x_2 x_3 0 (_ bv3 6)) x_3)))
 (let (($x9515 (= (stack_s x_0 x_1 x_2 x_3 0 (_ bv2 6)) x_2)))
 (let (($x9517 (= (stack_s x_0 x_1 x_2 x_3 0 (_ bv1 6)) x_1)))
 (let (($x9519 (= (stack_s x_0 x_1 x_2 x_3 0 (_ bv0 6)) x_0)))
 (let (($x8964 (= ?x72 (_ bv4 6))))
 (and $x8964 $x9519 $x9517 $x9515 $x9513 $x3503 $x9509 $x9508 (= ?x9536 (stack_s x_0 x_1 x_2 x_3 0 (bvadd (_ bv62 6) ?x72))) (= ?x9542 (stack_s x_0 x_1 x_2 x_3 0 (bvadd (_ bv63 6) ?x72))) (= (used_gas_s x_0 x_1 x_2 x_3 1) (+ 3 ?x9278)) $x3552 $x9551 $x9552 $x6799 (= ?x9554 (stack_s x_0 x_1 x_2 x_3 1 (bvadd (_ bv60 6) ?x154))) (= (stack_s x_0 x_1 x_2 x_3 2 (bvadd (_ bv60 6) ?x218)) ?x9536) $x9567 (= ?x9569 ?x9542) (= ?x9440 (+ 3 (used_gas_s x_0 x_1 x_2 x_3 1))) $x3567 $x9576 $x9577 $x8900 (= ?x9579 (bvnot ?x9582)) (= ?x9411 (+ 3 ?x9440)) (= ?x275 ?x3560) $x9591 $x9592 $x9594 $x9599 (= (stack_s x_0 x_1 x_2 x_3 4 (bvadd (_ bv61 6) ?x4305)) ?x9579) $x9607 $x9610 $x4957 $x9614 $x9615 $x7618 (= (stack_t x_0 x_1 x_2 x_3 1 ?x2367) ?x9620) (= (stack_t x_0 x_1 x_2 x_3 1 ?x9619) ?x9620) $x9627 $x9631 $x9635 (= (used_gas_t x_0 x_1 x_2 x_3 1) (+ 3 ?x9277)) $x3469 $x9642 $x9643 (= $x1920 (or $x56 $x3418 (not (bvsle (_ bv0 6) ?x9619)))) (= ?x9651 (bvnot ?x9656)) (= ?x9324 (+ 3 (used_gas_t x_0 x_1 x_2 x_3 1))) $x2323 $x9664 $x9665 $x1620 $x9670 (= (stack_t x_0 x_1 x_2 x_3 3 (bvadd (_ bv60 6) ?x2012)) ?x9651) $x9678 $x9683 (= ?x9306 (+ 3 ?x9324)) $x2216 $x9689 $x9690 $x516 $x9693 $x747 $x9697 $x9698 $x5037 $x73 $x9701 $x58 $x9284 $x9279 (not (and $x8875 $x9703 $x8922 $x9267)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
 )
(check-sat)
(get-proof)
