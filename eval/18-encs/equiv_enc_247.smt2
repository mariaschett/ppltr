; DUP1 SWAP2 SWAP1 => SWAP1 DUP2
(set-option :produce-proofs true)
; 
(set-info :status unknown)
(declare-fun storage_t ((_ BitVec 256) (_ BitVec 256) Int (_ BitVec 256)) (_ BitVec 256))
(declare-fun storage_s ((_ BitVec 256) (_ BitVec 256) Int (_ BitVec 256)) (_ BitVec 256))
(declare-fun exc_halt_t (Int) Bool)
(declare-fun exc_halt_s (Int) Bool)
(declare-fun sc_t (Int) (_ BitVec 6))
(declare-fun stack_t ((_ BitVec 256) (_ BitVec 256) Int (_ BitVec 6)) (_ BitVec 256))
(declare-fun stack_s ((_ BitVec 256) (_ BitVec 256) Int (_ BitVec 6)) (_ BitVec 256))
(declare-fun sc_s (Int) (_ BitVec 6))
(declare-fun used_gas_t ((_ BitVec 256) (_ BitVec 256) Int) Int)
(declare-fun used_gas_s ((_ BitVec 256) (_ BitVec 256) Int) Int)
(assert
 (exists ((x_0 (_ BitVec 256)) (x_1 (_ BitVec 256)) )(let (($x7416 (forall ((w (_ BitVec 256)) )(let ((?x9001 (storage_t x_0 x_1 2 w)))
 (let ((?x739 (storage_s x_0 x_1 3 w)))
 (= ?x739 ?x9001))))
 ))
 (let (($x903 (exc_halt_t 2)))
 (let (($x292 (exc_halt_s 3)))
 (let (($x3603 (= $x292 $x903)))
 (let (($x4121 (forall ((n (_ BitVec 6)) )(let ((?x4056 (sc_t 2)))
 (let (($x2976 (bvsle ?x4056 n)))
 (let ((?x9750 (stack_t x_0 x_1 2 n)))
 (let ((?x8196 (stack_s x_0 x_1 3 n)))
 (let (($x2530 (= ?x8196 ?x9750)))
 (or $x2530 $x2976)))))))
 ))
 (let ((?x4056 (sc_t 2)))
 (let ((?x275 (sc_s 3)))
 (let (($x10344 (= ?x275 ?x4056)))
 (let ((?x9876 (used_gas_t x_0 x_1 0)))
 (let ((?x3004 (used_gas_s x_0 x_1 0)))
 (let (($x610 (= ?x3004 ?x9876)))
 (let (($x3428 (forall ((w (_ BitVec 256)) )(let ((?x8712 (storage_t x_0 x_1 0 w)))
 (let ((?x7672 (storage_s x_0 x_1 0 w)))
 (= ?x7672 ?x8712))))
 ))
 (let (($x56 (exc_halt_t 0)))
 (let (($x57 (exc_halt_s 0)))
 (let (($x58 (= $x57 $x56)))
 (let (($x6907 (forall ((n (_ BitVec 6)) )(let ((?x63 (sc_t 0)))
 (let (($x10780 (bvsle ?x63 n)))
 (let ((?x4671 (stack_t x_0 x_1 0 n)))
 (let ((?x10861 (stack_s x_0 x_1 0 n)))
 (let (($x9719 (= ?x10861 ?x4671)))
 (or $x9719 $x10780)))))))
 ))
 (let ((?x63 (sc_t 0)))
 (let ((?x72 (sc_s 0)))
 (let (($x73 (= ?x72 ?x63)))
 (let (($x7658 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_t 1)))) (_ bv0 1)))))
 (let (($x1920 (exc_halt_t 1)))
 (let (($x1468 (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_t 1))))))
 (let (($x2597 (= $x903 (or $x1468 $x1920 $x7658))))
 (let (($x1894 (forall ((w (_ BitVec 256)) )(let ((?x2143 (storage_t x_0 x_1 1 w)))
 (let ((?x9001 (storage_t x_0 x_1 2 w)))
 (= ?x9001 ?x2143))))
 ))
 (let (($x8267 (forall ((n (_ BitVec 6)) )(let ((?x4023 (sc_t 1)))
 (let ((?x4968 (bvadd (_ bv62 6) ?x4023)))
 (let (($x5510 (bvsle ?x4968 n)))
 (or (= (stack_t x_0 x_1 2 n) (stack_t x_0 x_1 1 n)) $x5510)))))
 ))
 (let (($x1474 (= ?x4056 (bvadd (_ bv1 6) (sc_t 1)))))
 (let (($x824 (= (used_gas_t x_0 x_1 2) (+ 3 (used_gas_t x_0 x_1 1)))))
 (let ((?x4023 (sc_t 1)))
 (let ((?x166 (bvadd (_ bv63 6) ?x4023)))
 (let ((?x7506 (stack_t x_0 x_1 1 ?x166)))
 (let ((?x4968 (bvadd (_ bv62 6) ?x4023)))
 (let ((?x6677 (stack_t x_0 x_1 1 ?x4968)))
 (let (($x573 (= $x1920 (or $x56 (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) ?x63)))))))
 (let (($x11567 (forall ((w (_ BitVec 256)) )(let ((?x8712 (storage_t x_0 x_1 0 w)))
 (let ((?x2143 (storage_t x_0 x_1 1 w)))
 (= ?x2143 ?x8712))))
 ))
 (let (($x11312 (forall ((n (_ BitVec 6)) )(let ((?x63 (sc_t 0)))
 (let ((?x9795 (bvadd (_ bv62 6) ?x63)))
 (let (($x1720 (bvsle ?x9795 n)))
 (or $x1720 (= (stack_t x_0 x_1 1 n) (stack_t x_0 x_1 0 n)))))))
 ))
 (let (($x2379 (= ?x4023 ?x63)))
 (let (($x10091 (= $x292 (or (exc_halt_s 2) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 2))))))))
 (let (($x2382 (forall ((w (_ BitVec 256)) )(let ((?x9003 (storage_s x_0 x_1 2 w)))
 (let ((?x739 (storage_s x_0 x_1 3 w)))
 (= ?x739 ?x9003))))
 ))
 (let (($x9459 (forall ((n (_ BitVec 6)) )(let ((?x218 (sc_s 2)))
 (let ((?x10627 (bvadd (_ bv62 6) ?x218)))
 (let (($x2768 (bvsle ?x10627 n)))
 (or $x2768 (= (stack_s x_0 x_1 3 n) (stack_s x_0 x_1 2 n)))))))
 ))
 (let ((?x218 (sc_s 2)))
 (let (($x9676 (= ?x275 ?x218)))
 (let (($x7209 (= (used_gas_s x_0 x_1 3) (+ 3 (used_gas_s x_0 x_1 2)))))
 (let ((?x8896 (bvadd (_ bv63 6) ?x218)))
 (let ((?x7156 (stack_s x_0 x_1 2 ?x8896)))
 (let ((?x10627 (bvadd (_ bv62 6) ?x218)))
 (let ((?x7030 (stack_s x_0 x_1 2 ?x10627)))
 (let (($x247 (exc_halt_s 2)))
 (let (($x8652 (= $x247 (or (exc_halt_s 1) (not (bvsle (_ bv0 6) (bvadd (_ bv61 6) (sc_s 1))))))))
 (let (($x409 (forall ((w (_ BitVec 256)) )(let ((?x3380 (storage_s x_0 x_1 1 w)))
 (let ((?x9003 (storage_s x_0 x_1 2 w)))
 (= ?x9003 ?x3380))))
 ))
 (let (($x10398 (forall ((n (_ BitVec 6)) )(let ((?x154 (sc_s 1)))
 (let ((?x2212 (bvadd (_ bv61 6) ?x154)))
 (let (($x3746 (bvsle ?x2212 n)))
 (or $x3746 (= (stack_s x_0 x_1 2 n) (stack_s x_0 x_1 1 n)))))))
 ))
 (let ((?x154 (sc_s 1)))
 (let (($x1238 (= ?x218 ?x154)))
 (let ((?x4681 (used_gas_s x_0 x_1 2)))
 (let ((?x2267 (bvadd (_ bv63 6) ?x154)))
 (let ((?x630 (stack_s x_0 x_1 1 ?x2267)))
 (let (($x4570 (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) ?x72)))))
 (let (($x3372 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x72))) (_ bv0 1)))))
 (let (($x189 (exc_halt_s 1)))
 (let (($x5833 (= $x189 (or $x57 $x3372 $x4570))))
 (let (($x2581 (forall ((w (_ BitVec 256)) )(let ((?x7672 (storage_s x_0 x_1 0 w)))
 (let ((?x3380 (storage_s x_0 x_1 1 w)))
 (= ?x3380 ?x7672))))
 ))
 (let (($x9137 (forall ((n (_ BitVec 6)) )(let ((?x72 (sc_s 0)))
 (let ((?x8369 (bvadd (_ bv63 6) ?x72)))
 (let (($x3627 (bvsle ?x8369 n)))
 (or $x3627 (= (stack_s x_0 x_1 1 n) (stack_s x_0 x_1 0 n)))))))
 ))
 (let (($x5613 (= ?x154 (bvadd (_ bv1 6) ?x72))))
 (let ((?x8369 (bvadd (_ bv63 6) ?x72)))
 (let ((?x688 (stack_s x_0 x_1 0 ?x8369)))
 (let (($x9641 (forall ((w (_ BitVec 256)) )(let ((?x7672 (storage_s x_0 x_1 0 w)))
 (= ?x7672 (_ bv0 256))))
 ))
 (let (($x6471 (= ?x3004 0)))
 (let (($x9872 (not $x57)))
 (let (($x10313 (= (stack_s x_0 x_1 0 (_ bv1 6)) x_1)))
 (let (($x9420 (= (stack_s x_0 x_1 0 (_ bv0 6)) x_0)))
 (let (($x643 (= ?x72 (_ bv2 6))))
 (and $x643 $x9420 $x10313 $x9872 $x6471 $x9641 (= ?x630 ?x688) (= (stack_s x_0 x_1 1 ?x8369) ?x688) (= (used_gas_s x_0 x_1 1) (+ 3 ?x3004)) $x5613 $x9137 $x2581 $x5833 (= ?x7156 (stack_s x_0 x_1 1 (bvadd (_ bv61 6) ?x154))) (= (stack_s x_0 x_1 2 (bvadd (_ bv61 6) ?x218)) ?x630) (= ?x7030 (stack_s x_0 x_1 1 (bvadd (_ bv62 6) ?x154))) (= ?x4681 (+ 3 (used_gas_s x_0 x_1 1))) $x1238 $x10398 $x409 $x8652 (= (stack_s x_0 x_1 3 (bvadd (_ bv63 6) ?x275)) ?x7030) (= (stack_s x_0 x_1 3 (bvadd (_ bv62 6) ?x275)) ?x7156) $x7209 $x9676 $x9459 $x2382 $x10091 (= ?x7506 (stack_t x_0 x_1 0 (bvadd (_ bv62 6) ?x63))) (= ?x6677 (stack_t x_0 x_1 0 (bvadd (_ bv63 6) ?x63))) (= (used_gas_t x_0 x_1 1) (+ 3 ?x9876)) $x2379 $x11312 $x11567 $x573 (= (stack_t x_0 x_1 2 (bvadd (_ bv63 6) ?x4056)) ?x6677) (= (stack_t x_0 x_1 2 ?x4968) ?x6677) (= (stack_t x_0 x_1 2 ?x166) ?x7506) $x824 $x1474 $x8267 $x1894 $x2597 $x73 $x6907 $x58 $x3428 $x610 (not (and $x10344 $x4121 $x3603 $x7416))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
 )
(check-sat)
(get-proof)
