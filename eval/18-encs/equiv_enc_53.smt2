; PUSH cw_0 SWAP2 DUP2 SWAP3 POP POP => SWAP1 POP DUP1
(set-option :produce-proofs true)
; 
(set-info :status unknown)
(declare-fun storage_t ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 256)) (_ BitVec 256))
(declare-fun w_0 () (_ BitVec 256))
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
 (exists ((x_0 (_ BitVec 256)) (x_1 (_ BitVec 256)) )(let (($x6621 (forall ((w (_ BitVec 256)) )(let ((?x6645 (storage_t x_0 x_1 w_0 3 w)))
 (let ((?x6606 (storage_s x_0 x_1 w_0 6 w)))
 (= ?x6606 ?x6645))))
 ))
 (let (($x10336 (exc_halt_t 3)))
 (let (($x772 (exc_halt_s 6)))
 (let (($x2647 (= $x772 $x10336)))
 (let (($x5547 (forall ((n (_ BitVec 6)) )(let ((?x2012 (sc_t 3)))
 (let (($x7850 (bvsle ?x2012 n)))
 (let ((?x6624 (stack_t x_0 x_1 w_0 3 n)))
 (let ((?x6657 (stack_s x_0 x_1 w_0 6 n)))
 (let (($x6652 (= ?x6657 ?x6624)))
 (or $x6652 $x7850)))))))
 ))
 (let ((?x2012 (sc_t 3)))
 (let ((?x926 (sc_s 6)))
 (let (($x7824 (= ?x926 ?x2012)))
 (let ((?x6970 (used_gas_t x_0 x_1 w_0 0)))
 (let ((?x6434 (used_gas_s x_0 x_1 w_0 0)))
 (let (($x7003 (= ?x6434 ?x6970)))
 (let (($x5902 (forall ((w (_ BitVec 256)) )(let ((?x5903 (storage_t x_0 x_1 w_0 0 w)))
 (let ((?x5799 (storage_s x_0 x_1 w_0 0 w)))
 (= ?x5799 ?x5903))))
 ))
 (let (($x56 (exc_halt_t 0)))
 (let (($x57 (exc_halt_s 0)))
 (let (($x58 (= $x57 $x56)))
 (let (($x5179 (forall ((n (_ BitVec 6)) )(let ((?x63 (sc_t 0)))
 (let (($x7609 (bvsle ?x63 n)))
 (let ((?x5762 (stack_t x_0 x_1 w_0 0 n)))
 (let ((?x5820 (stack_s x_0 x_1 w_0 0 n)))
 (let (($x5577 (= ?x5820 ?x5762)))
 (or $x5577 $x7609)))))))
 ))
 (let ((?x63 (sc_t 0)))
 (let ((?x72 (sc_s 0)))
 (let (($x73 (= ?x72 ?x63)))
 (let (($x8818 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_t 2)))) (_ bv0 1)))))
 (let (($x7835 (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_t 2))))))
 (let (($x903 (exc_halt_t 2)))
 (let (($x5151 (forall ((w (_ BitVec 256)) )(let ((?x5914 (storage_t x_0 x_1 w_0 2 w)))
 (let ((?x6645 (storage_t x_0 x_1 w_0 3 w)))
 (= ?x6645 ?x5914))))
 ))
 (let (($x5540 (forall ((n (_ BitVec 6)) )(let ((?x4056 (sc_t 2)))
 (let ((?x9462 (bvadd (_ bv63 6) ?x4056)))
 (let (($x740 (bvsle ?x9462 n)))
 (let ((?x3072 (stack_t x_0 x_1 w_0 2 n)))
 (let ((?x6624 (stack_t x_0 x_1 w_0 3 n)))
 (or (= ?x6624 ?x3072) $x740)))))))
 ))
 (let (($x8822 (= ?x2012 (bvadd (_ bv1 6) (sc_t 2)))))
 (let (($x5546 (= (used_gas_t x_0 x_1 w_0 3) (+ 3 (used_gas_t x_0 x_1 w_0 2)))))
 (let ((?x4056 (sc_t 2)))
 (let ((?x9462 (bvadd (_ bv63 6) ?x4056)))
 (let ((?x5524 (stack_t x_0 x_1 w_0 2 ?x9462)))
 (let (($x9676 (= $x903 (or (exc_halt_t 1) (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_t 1))))))))
 (let (($x5417 (forall ((w (_ BitVec 256)) )(let ((?x5753 (storage_t x_0 x_1 w_0 1 w)))
 (let ((?x5914 (storage_t x_0 x_1 w_0 2 w)))
 (= ?x5914 ?x5753))))
 ))
 (let (($x7602 (forall ((n (_ BitVec 6)) )(let ((?x4023 (sc_t 1)))
 (let ((?x9435 (bvadd (_ bv63 6) ?x4023)))
 (let (($x9378 (bvsle ?x9435 n)))
 (let ((?x8579 (stack_t x_0 x_1 w_0 1 n)))
 (let ((?x3072 (stack_t x_0 x_1 w_0 2 n)))
 (or (= ?x3072 ?x8579) $x9378)))))))
 ))
 (let ((?x5894 (used_gas_t x_0 x_1 w_0 2)))
 (let (($x5162 (forall ((w (_ BitVec 256)) )(let ((?x5903 (storage_t x_0 x_1 w_0 0 w)))
 (let ((?x5753 (storage_t x_0 x_1 w_0 1 w)))
 (= ?x5753 ?x5903))))
 ))
 (let (($x5399 (forall ((n (_ BitVec 6)) )(let ((?x5762 (stack_t x_0 x_1 w_0 0 n)))
 (let ((?x8579 (stack_t x_0 x_1 w_0 1 n)))
 (or (= ?x8579 ?x5762) (bvsle (bvadd (_ bv62 6) (sc_t 0)) n)))))
 ))
 (let ((?x4023 (sc_t 1)))
 (let (($x6507 (= ?x4023 ?x63)))
 (let (($x5328 (= (stack_t x_0 x_1 w_0 1 (bvadd (_ bv62 6) ?x4023)) (stack_t x_0 x_1 w_0 0 (bvadd (_ bv63 6) ?x63)))))
 (let (($x5334 (= (stack_t x_0 x_1 w_0 1 (bvadd (_ bv63 6) ?x4023)) (stack_t x_0 x_1 w_0 0 (bvadd (_ bv62 6) ?x63)))))
 (let (($x10002 (= $x772 (or (exc_halt_s 5) (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_s 5))))))))
 (let (($x5726 (forall ((w (_ BitVec 256)) )(let ((?x3121 (storage_s x_0 x_1 w_0 5 w)))
 (let ((?x6606 (storage_s x_0 x_1 w_0 6 w)))
 (= ?x6606 ?x3121))))
 ))
 (let (($x5286 (forall ((n (_ BitVec 6)) )(let ((?x805 (sc_s 5)))
 (let ((?x9564 (bvadd (_ bv63 6) ?x805)))
 (let (($x9721 (bvsle ?x9564 n)))
 (let ((?x5683 (stack_s x_0 x_1 w_0 5 n)))
 (let ((?x6657 (stack_s x_0 x_1 w_0 6 n)))
 (or (= ?x6657 ?x5683) $x9721)))))))
 ))
 (let ((?x805 (sc_s 5)))
 (let ((?x9564 (bvadd (_ bv63 6) ?x805)))
 (let (($x9553 (= ?x926 ?x9564)))
 (let (($x5527 (= (used_gas_s x_0 x_1 w_0 6) (+ 2 (used_gas_s x_0 x_1 w_0 5)))))
 (let (($x3979 (exc_halt_s 5)))
 (let (($x5264 (= $x3979 (or (exc_halt_s 4) (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_s 4))))))))
 (let (($x5602 (forall ((w (_ BitVec 256)) )(let ((?x5723 (storage_s x_0 x_1 w_0 4 w)))
 (let ((?x3121 (storage_s x_0 x_1 w_0 5 w)))
 (= ?x3121 ?x5723))))
 ))
 (let (($x5581 (forall ((n (_ BitVec 6)) )(let ((?x3215 (stack_s x_0 x_1 w_0 4 n)))
 (let ((?x5683 (stack_s x_0 x_1 w_0 5 n)))
 (or (= ?x5683 ?x3215) (bvsle (bvadd (_ bv63 6) (sc_s 4)) n)))))
 ))
 (let ((?x4305 (sc_s 4)))
 (let ((?x9580 (bvadd (_ bv63 6) ?x4305)))
 (let (($x9278 (= ?x805 ?x9580)))
 (let ((?x5631 (used_gas_s x_0 x_1 w_0 5)))
 (let (($x64 (exc_halt_s 4)))
 (let (($x5224 (= $x64 (or (exc_halt_s 3) (not (bvsle (_ bv0 6) (bvadd (_ bv60 6) (sc_s 3))))))))
 (let (($x5812 (forall ((w (_ BitVec 256)) )(let ((?x3633 (storage_s x_0 x_1 w_0 3 w)))
 (let ((?x5723 (storage_s x_0 x_1 w_0 4 w)))
 (= ?x5723 ?x3633))))
 ))
 (let (($x5728 (forall ((n (_ BitVec 6)) )(let ((?x3801 (stack_s x_0 x_1 w_0 3 n)))
 (let ((?x3215 (stack_s x_0 x_1 w_0 4 n)))
 (or (= ?x3215 ?x3801) (bvsle (bvadd (_ bv60 6) (sc_s 3)) n)))))
 ))
 (let ((?x275 (sc_s 3)))
 (let (($x3560 (= ?x4305 ?x275)))
 (let ((?x5736 (used_gas_s x_0 x_1 w_0 4)))
 (let (($x3210 (= (stack_s x_0 x_1 w_0 4 (bvadd (_ bv62 6) ?x4305)) (stack_s x_0 x_1 w_0 3 (bvadd (_ bv62 6) ?x275)))))
 (let (($x5684 (= (stack_s x_0 x_1 w_0 4 (bvadd (_ bv61 6) ?x4305)) (stack_s x_0 x_1 w_0 3 (bvadd (_ bv61 6) ?x275)))))
 (let ((?x9399 (bvadd (_ bv63 6) ?x275)))
 (let ((?x5632 (stack_s x_0 x_1 w_0 3 ?x9399)))
 (let (($x5693 (= (stack_s x_0 x_1 w_0 4 ?x9580) (stack_s x_0 x_1 w_0 3 (bvadd (_ bv60 6) ?x275)))))
 (let (($x4090 (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 2))))))
 (let (($x2196 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 2)))) (_ bv0 1)))))
 (let (($x247 (exc_halt_s 2)))
 (let (($x292 (exc_halt_s 3)))
 (let (($x5668 (forall ((w (_ BitVec 256)) )(let ((?x5771 (storage_s x_0 x_1 w_0 2 w)))
 (let ((?x3633 (storage_s x_0 x_1 w_0 3 w)))
 (= ?x3633 ?x5771))))
 ))
 (let (($x5667 (forall ((n (_ BitVec 6)) )(let ((?x4332 (stack_s x_0 x_1 w_0 2 n)))
 (let ((?x3801 (stack_s x_0 x_1 w_0 3 n)))
 (let ((?x218 (sc_s 2)))
 (let ((?x7618 (bvadd (_ bv62 6) ?x218)))
 (let (($x6052 (bvsle ?x7618 n)))
 (or $x6052 (= ?x3801 ?x4332))))))))
 ))
 (let (($x8896 (= ?x275 (bvadd (_ bv1 6) (sc_s 2)))))
 (let ((?x2212 (used_gas_s x_0 x_1 w_0 3)))
 (let ((?x218 (sc_s 2)))
 (let ((?x9655 (bvadd (_ bv63 6) ?x218)))
 (let ((?x3649 (stack_s x_0 x_1 w_0 2 ?x9655)))
 (let ((?x7618 (bvadd (_ bv62 6) ?x218)))
 (let ((?x5609 (stack_s x_0 x_1 w_0 2 ?x7618)))
 (let (($x7678 (= $x247 (or (exc_halt_s 1) (not (bvsle (_ bv0 6) (bvadd (_ bv61 6) (sc_s 1))))))))
 (let (($x5645 (forall ((w (_ BitVec 256)) )(let ((?x5783 (storage_s x_0 x_1 w_0 1 w)))
 (let ((?x5771 (storage_s x_0 x_1 w_0 2 w)))
 (= ?x5771 ?x5783))))
 ))
 (let (($x5959 (forall ((n (_ BitVec 6)) )(let ((?x5791 (stack_s x_0 x_1 w_0 1 n)))
 (let ((?x4332 (stack_s x_0 x_1 w_0 2 n)))
 (or (bvsle (bvadd (_ bv61 6) (sc_s 1)) n) (= ?x4332 ?x5791)))))
 ))
 (let ((?x154 (sc_s 1)))
 (let (($x9614 (= ?x218 ?x154)))
 (let ((?x5368 (used_gas_s x_0 x_1 w_0 2)))
 (let (($x5614 (= (stack_s x_0 x_1 w_0 2 (bvadd (_ bv61 6) ?x218)) (stack_s x_0 x_1 w_0 1 (bvadd (_ bv63 6) ?x154)))))
 (let (($x189 (exc_halt_s 1)))
 (let (($x5618 (= $x189 (or $x57 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x72))) (_ bv0 1)))))))
 (let (($x5600 (forall ((w (_ BitVec 256)) )(let ((?x5799 (storage_s x_0 x_1 w_0 0 w)))
 (let ((?x5783 (storage_s x_0 x_1 w_0 1 w)))
 (= ?x5783 ?x5799))))
 ))
 (let (($x2037 (forall ((n (_ BitVec 6)) )(let ((?x5820 (stack_s x_0 x_1 w_0 0 n)))
 (let ((?x5791 (stack_s x_0 x_1 w_0 1 n)))
 (or (bvsle (sc_s 0) n) (= ?x5791 ?x5820)))))
 ))
 (let (($x9025 (= ?x154 (bvadd (_ bv1 6) ?x72))))
 (let (($x5633 (forall ((w (_ BitVec 256)) )(let ((?x5799 (storage_s x_0 x_1 w_0 0 w)))
 (= ?x5799 (_ bv0 256))))
 ))
 (let (($x5813 (= ?x6434 0)))
 (let (($x5028 (not $x57)))
 (let (($x5579 (= (stack_s x_0 x_1 w_0 0 (_ bv1 6)) x_1)))
 (let (($x5576 (= (stack_s x_0 x_1 w_0 0 (_ bv0 6)) x_0)))
 (let (($x8229 (= ?x72 (_ bv2 6))))
 (and $x8229 $x5576 $x5579 $x5028 $x5813 $x5633 (= (stack_s x_0 x_1 w_0 1 ?x72) w_0) (= (used_gas_s x_0 x_1 w_0 1) (+ 3 ?x6434)) $x9025 $x2037 $x5600 $x5618 (= ?x3649 (stack_s x_0 x_1 w_0 1 (bvadd (_ bv61 6) ?x154))) $x5614 (= ?x5609 (stack_s x_0 x_1 w_0 1 (bvadd (_ bv62 6) ?x154))) (= ?x5368 (+ 3 (used_gas_s x_0 x_1 w_0 1))) $x9614 $x5959 $x5645 $x7678 (= ?x5632 ?x5609) (= (stack_s x_0 x_1 w_0 3 ?x7618) ?x5609) (= (stack_s x_0 x_1 w_0 3 ?x9655) ?x3649) (= ?x2212 (+ 3 ?x5368)) $x8896 $x5667 $x5668 (= $x292 (or $x247 $x2196 $x4090)) $x5693 (= (stack_s x_0 x_1 w_0 4 (bvadd (_ bv60 6) ?x4305)) ?x5632) $x5684 $x3210 (= ?x5736 (+ 3 ?x2212)) $x3560 $x5728 $x5812 $x5224 (= ?x5631 (+ 2 ?x5736)) $x9278 $x5581 $x5602 $x5264 $x5527 $x9553 $x5286 $x5726 $x10002 $x5334 $x5328 (= (used_gas_t x_0 x_1 w_0 1) (+ 3 ?x6970)) $x6507 $x5399 $x5162 (= (exc_halt_t 1) (or $x56 (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) ?x63))))) (= ?x5894 (+ 2 (used_gas_t x_0 x_1 w_0 1))) (= ?x4056 (bvadd (_ bv63 6) ?x4023)) $x7602 $x5417 $x9676 (= (stack_t x_0 x_1 w_0 3 (bvadd (_ bv63 6) ?x2012)) ?x5524) (= (stack_t x_0 x_1 w_0 3 ?x9462) ?x5524) $x5546 $x8822 $x5540 $x5151 (= $x10336 (or $x903 $x7835 $x8818)) $x73 $x5179 $x58 $x5902 $x7003 (not (and $x7824 $x5547 $x2647 $x6621)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
 )
(check-sat)
(get-proof)
