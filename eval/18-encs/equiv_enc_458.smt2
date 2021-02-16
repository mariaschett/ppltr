; DUP1 PUSH cw_3 ADD PUSH cw_2 SWAP1 => PUSH cw_2 PUSH cw_3 DUP3 ADD
(set-option :produce-proofs true)
; 
(set-info :status unknown)
(declare-fun storage_t ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 256)) (_ BitVec 256))
(declare-fun w_2 () (_ BitVec 256))
(declare-fun w_3 () (_ BitVec 256))
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
 (exists ((x_0 (_ BitVec 256)) )(let (($x5326 (forall ((w (_ BitVec 256)) )(let ((?x5340 (storage_t x_0 w_3 w_2 4 w)))
 (let ((?x6017 (storage_s x_0 w_3 w_2 5 w)))
 (= ?x6017 ?x5340))))
 ))
 (let (($x7722 (exc_halt_t 4)))
 (let (($x11317 (exc_halt_s 5)))
 (let (($x10023 (= $x11317 $x7722)))
 (let (($x11797 (forall ((n (_ BitVec 6)) )(let ((?x3811 (stack_t x_0 w_3 w_2 4 n)))
 (let ((?x5810 (stack_s x_0 w_3 w_2 5 n)))
 (let (($x8536 (= ?x5810 ?x3811)))
 (let ((?x11631 (sc_t 4)))
 (let (($x4352 (bvsle ?x11631 n)))
 (or $x4352 $x8536)))))))
 ))
 (let ((?x11631 (sc_t 4)))
 (let ((?x4319 (sc_s 5)))
 (let (($x8727 (= ?x4319 ?x11631)))
 (let ((?x4965 (used_gas_t x_0 w_3 w_2 0)))
 (let ((?x9479 (used_gas_s x_0 w_3 w_2 0)))
 (let (($x7276 (= ?x9479 ?x4965)))
 (let (($x5546 (forall ((w (_ BitVec 256)) )(let ((?x11438 (storage_t x_0 w_3 w_2 0 w)))
 (let ((?x3281 (storage_s x_0 w_3 w_2 0 w)))
 (= ?x3281 ?x11438))))
 ))
 (let (($x56 (exc_halt_t 0)))
 (let (($x57 (exc_halt_s 0)))
 (let (($x58 (= $x57 $x56)))
 (let (($x9594 (forall ((n (_ BitVec 6)) )(let ((?x6622 (stack_t x_0 w_3 w_2 0 n)))
 (let ((?x4687 (stack_s x_0 w_3 w_2 0 n)))
 (let (($x4524 (= ?x4687 ?x6622)))
 (let ((?x63 (sc_t 0)))
 (let (($x368 (bvsle ?x63 n)))
 (or $x368 $x4524)))))))
 ))
 (let ((?x63 (sc_t 0)))
 (let ((?x72 (sc_s 0)))
 (let (($x73 (= ?x72 ?x63)))
 (let (($x9332 (= $x7722 (or (exc_halt_t 3) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_t 3))))))))
 (let (($x3260 (forall ((w (_ BitVec 256)) )(let ((?x584 (storage_t x_0 w_3 w_2 3 w)))
 (let ((?x5340 (storage_t x_0 w_3 w_2 4 w)))
 (= ?x5340 ?x584))))
 ))
 (let (($x5280 (forall ((n (_ BitVec 6)) )(let ((?x7638 (stack_t x_0 w_3 w_2 3 n)))
 (let ((?x3811 (stack_t x_0 w_3 w_2 4 n)))
 (let (($x8620 (= ?x3811 ?x7638)))
 (let ((?x6438 (sc_t 3)))
 (let ((?x4076 (bvadd (_ bv62 6) ?x6438)))
 (let (($x8388 (bvsle ?x4076 n)))
 (or $x8388 $x8620))))))))
 ))
 (let ((?x8323 (used_gas_t x_0 w_3 w_2 4)))
 (let (($x11379 (= ?x8323 (+ 3 (used_gas_t x_0 w_3 w_2 3)))))
 (let ((?x6438 (sc_t 3)))
 (let ((?x6883 (bvadd (_ bv63 6) ?x6438)))
 (let ((?x9698 (stack_t x_0 w_3 w_2 3 ?x6883)))
 (let ((?x9366 (bvadd (_ bv63 6) ?x11631)))
 (let ((?x3830 (stack_t x_0 w_3 w_2 4 ?x9366)))
 (let (($x7573 (= ?x3830 (bvadd ?x9698 (stack_t x_0 w_3 w_2 3 (bvadd (_ bv62 6) ?x6438))))))
 (let (($x10055 (exc_halt_t 2)))
 (let (($x6954 (not (bvsle (_ bv0 6) (bvadd (_ bv61 6) (sc_t 2))))))
 (let (($x742 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_t 2)))) (_ bv0 1)))))
 (let (($x6783 (exc_halt_t 3)))
 (let (($x7996 (forall ((w (_ BitVec 256)) )(let ((?x9629 (storage_t x_0 w_3 w_2 2 w)))
 (let ((?x584 (storage_t x_0 w_3 w_2 3 w)))
 (= ?x584 ?x9629))))
 ))
 (let (($x11555 (forall ((n (_ BitVec 6)) )(let ((?x2524 (stack_t x_0 w_3 w_2 2 n)))
 (let ((?x7638 (stack_t x_0 w_3 w_2 3 n)))
 (let (($x7840 (= ?x7638 ?x2524)))
 (let ((?x2714 (sc_t 2)))
 (let ((?x5805 (bvadd (_ bv61 6) ?x2714)))
 (let (($x7712 (bvsle ?x5805 n)))
 (or $x7712 $x7840))))))))
 ))
 (let (($x7112 (= ?x6438 (bvadd (_ bv1 6) (sc_t 2)))))
 (let ((?x10444 (used_gas_t x_0 w_3 w_2 3)))
 (let (($x2810 (= ?x10444 (+ 3 (used_gas_t x_0 w_3 w_2 2)))))
 (let (($x5482 (= (stack_t x_0 w_3 w_2 3 (bvadd (_ bv63 6) (sc_t 2))) (stack_t x_0 w_3 w_2 2 (bvadd (_ bv63 6) (sc_t 2))))))
 (let (($x7857 (= (stack_t x_0 w_3 w_2 3 (bvadd (_ bv62 6) (sc_t 2))) (stack_t x_0 w_3 w_2 2 (bvadd (_ bv62 6) (sc_t 2))))))
 (let ((?x2714 (sc_t 2)))
 (let ((?x5805 (bvadd (_ bv61 6) ?x2714)))
 (let ((?x2990 (stack_t x_0 w_3 w_2 2 ?x5805)))
 (let (($x9506 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_t 1)))) (_ bv0 1)))))
 (let (($x3508 (exc_halt_t 1)))
 (let (($x9455 (= $x10055 (or $x3508 $x9506))))
 (let (($x3951 (forall ((w (_ BitVec 256)) )(let ((?x2819 (storage_t x_0 w_3 w_2 1 w)))
 (let ((?x9629 (storage_t x_0 w_3 w_2 2 w)))
 (= ?x9629 ?x2819))))
 ))
 (let (($x8041 (forall ((n (_ BitVec 6)) )(let ((?x773 (stack_t x_0 w_3 w_2 1 n)))
 (let ((?x2524 (stack_t x_0 w_3 w_2 2 n)))
 (let ((?x8347 (sc_t 1)))
 (let (($x8474 (bvsle ?x8347 n)))
 (or $x8474 (= ?x2524 ?x773)))))))
 ))
 (let (($x3851 (= ?x2714 (bvadd (_ bv1 6) (sc_t 1)))))
 (let ((?x9081 (used_gas_t x_0 w_3 w_2 2)))
 (let (($x5124 (= ?x9081 (+ 3 (used_gas_t x_0 w_3 w_2 1)))))
 (let (($x8246 (= $x3508 (or $x56 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x63))) (_ bv0 1)))))))
 (let (($x8120 (forall ((w (_ BitVec 256)) )(let ((?x11438 (storage_t x_0 w_3 w_2 0 w)))
 (let ((?x2819 (storage_t x_0 w_3 w_2 1 w)))
 (= ?x2819 ?x11438))))
 ))
 (let (($x9926 (forall ((n (_ BitVec 6)) )(let ((?x6622 (stack_t x_0 w_3 w_2 0 n)))
 (let ((?x773 (stack_t x_0 w_3 w_2 1 n)))
 (let (($x7008 (= ?x773 ?x6622)))
 (let ((?x63 (sc_t 0)))
 (let (($x368 (bvsle ?x63 n)))
 (or $x368 $x7008)))))))
 ))
 (let ((?x8347 (sc_t 1)))
 (let (($x7773 (= ?x8347 (bvadd (_ bv1 6) ?x63))))
 (let (($x1419 (= $x11317 (or (exc_halt_s 4) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 4))))))))
 (let (($x9627 (forall ((w (_ BitVec 256)) )(let ((?x9636 (storage_s x_0 w_3 w_2 4 w)))
 (let ((?x6017 (storage_s x_0 w_3 w_2 5 w)))
 (= ?x6017 ?x9636))))
 ))
 (let (($x4413 (forall ((n (_ BitVec 6)) )(let ((?x704 (stack_s x_0 w_3 w_2 4 n)))
 (let ((?x5810 (stack_s x_0 w_3 w_2 5 n)))
 (let (($x6475 (= ?x5810 ?x704)))
 (let ((?x4305 (sc_s 4)))
 (let ((?x3448 (bvadd (_ bv62 6) ?x4305)))
 (let (($x3798 (bvsle ?x3448 n)))
 (or $x3798 $x6475))))))))
 ))
 (let ((?x4305 (sc_s 4)))
 (let (($x9388 (= ?x4319 ?x4305)))
 (let ((?x440 (used_gas_s x_0 w_3 w_2 5)))
 (let (($x8460 (= ?x440 (+ 3 (used_gas_s x_0 w_3 w_2 4)))))
 (let ((?x2253 (bvadd (_ bv63 6) ?x4305)))
 (let ((?x4368 (stack_s x_0 w_3 w_2 4 ?x2253)))
 (let ((?x1762 (bvadd (_ bv62 6) ?x4319)))
 (let ((?x6347 (stack_s x_0 w_3 w_2 5 ?x1762)))
 (let ((?x3448 (bvadd (_ bv62 6) ?x4305)))
 (let ((?x5047 (stack_s x_0 w_3 w_2 4 ?x3448)))
 (let ((?x3925 (bvadd (_ bv63 6) ?x4319)))
 (let ((?x1002 (stack_s x_0 w_3 w_2 5 ?x3925)))
 (let (($x9664 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 3)))) (_ bv0 1)))))
 (let (($x292 (exc_halt_s 3)))
 (let (($x7172 (exc_halt_s 4)))
 (let (($x4234 (= $x7172 (or $x292 $x9664))))
 (let (($x6105 (forall ((w (_ BitVec 256)) )(let ((?x4873 (storage_s x_0 w_3 w_2 3 w)))
 (let ((?x9636 (storage_s x_0 w_3 w_2 4 w)))
 (= ?x9636 ?x4873))))
 ))
 (let (($x6373 (forall ((n (_ BitVec 6)) )(let ((?x275 (sc_s 3)))
 (let (($x11727 (bvsle ?x275 n)))
 (let ((?x3219 (stack_s x_0 w_3 w_2 3 n)))
 (let ((?x704 (stack_s x_0 w_3 w_2 4 n)))
 (let (($x437 (= ?x704 ?x3219)))
 (or $x437 $x11727)))))))
 ))
 (let (($x11661 (= ?x4305 (bvadd (_ bv1 6) (sc_s 3)))))
 (let ((?x993 (used_gas_s x_0 w_3 w_2 4)))
 (let (($x4997 (= ?x993 (+ 3 (used_gas_s x_0 w_3 w_2 3)))))
 (let (($x1868 (= $x292 (or (exc_halt_s 2) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 2))))))))
 (let (($x11739 (forall ((w (_ BitVec 256)) )(let ((?x4952 (storage_s x_0 w_3 w_2 2 w)))
 (let ((?x4873 (storage_s x_0 w_3 w_2 3 w)))
 (= ?x4873 ?x4952))))
 ))
 (let (($x6935 (forall ((n (_ BitVec 6)) )(let ((?x3063 (stack_s x_0 w_3 w_2 2 n)))
 (let ((?x3219 (stack_s x_0 w_3 w_2 3 n)))
 (let (($x9816 (= ?x3219 ?x3063)))
 (let ((?x218 (sc_s 2)))
 (let ((?x7822 (bvadd (_ bv62 6) ?x218)))
 (let (($x3245 (bvsle ?x7822 n)))
 (or $x3245 $x9816))))))))
 ))
 (let ((?x1291 (used_gas_s x_0 w_3 w_2 3)))
 (let (($x813 (= ?x1291 (+ 3 (used_gas_s x_0 w_3 w_2 2)))))
 (let ((?x218 (sc_s 2)))
 (let ((?x6729 (bvadd (_ bv63 6) ?x218)))
 (let ((?x2197 (stack_s x_0 w_3 w_2 2 ?x6729)))
 (let ((?x275 (sc_s 3)))
 (let ((?x11810 (bvadd (_ bv63 6) ?x275)))
 (let ((?x8124 (stack_s x_0 w_3 w_2 3 ?x11810)))
 (let (($x5301 (= ?x8124 (bvadd ?x2197 (stack_s x_0 w_3 w_2 2 (bvadd (_ bv62 6) ?x218))))))
 (let (($x1680 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 1)))) (_ bv0 1)))))
 (let (($x189 (exc_halt_s 1)))
 (let (($x247 (exc_halt_s 2)))
 (let (($x3817 (forall ((w (_ BitVec 256)) )(let ((?x3921 (storage_s x_0 w_3 w_2 1 w)))
 (let ((?x4952 (storage_s x_0 w_3 w_2 2 w)))
 (= ?x4952 ?x3921))))
 ))
 (let (($x4954 (forall ((n (_ BitVec 6)) )(let ((?x11414 (stack_s x_0 w_3 w_2 1 n)))
 (let ((?x3063 (stack_s x_0 w_3 w_2 2 n)))
 (let (($x5453 (= ?x3063 ?x11414)))
 (or $x5453 (bvsle (sc_s 1) n))))))
 ))
 (let (($x430 (= ?x218 (bvadd (_ bv1 6) (sc_s 1)))))
 (let ((?x541 (used_gas_s x_0 w_3 w_2 2)))
 (let (($x9203 (= ?x541 (+ 3 (used_gas_s x_0 w_3 w_2 1)))))
 (let (($x674 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x72))) (_ bv0 1)))))
 (let (($x3775 (forall ((w (_ BitVec 256)) )(let ((?x3281 (storage_s x_0 w_3 w_2 0 w)))
 (let ((?x3921 (storage_s x_0 w_3 w_2 1 w)))
 (= ?x3921 ?x3281))))
 ))
 (let (($x8435 (forall ((n (_ BitVec 6)) )(let ((?x4687 (stack_s x_0 w_3 w_2 0 n)))
 (let ((?x11414 (stack_s x_0 w_3 w_2 1 n)))
 (let (($x6755 (= ?x11414 ?x4687)))
 (or (bvsle (bvadd (_ bv63 6) (sc_s 0)) n) $x6755)))))
 ))
 (let ((?x154 (sc_s 1)))
 (let (($x1515 (= ?x154 (bvadd (_ bv1 6) ?x72))))
 (let ((?x5723 (used_gas_s x_0 w_3 w_2 1)))
 (let (($x2561 (= ?x5723 (+ 3 ?x9479))))
 (let ((?x9903 (bvadd (_ bv63 6) ?x72)))
 (let ((?x9568 (stack_s x_0 w_3 w_2 0 ?x9903)))
 (let (($x8854 (forall ((w (_ BitVec 256)) )(let ((?x3281 (storage_s x_0 w_3 w_2 0 w)))
 (= ?x3281 (_ bv0 256))))
 ))
 (let (($x894 (= ?x9479 0)))
 (let (($x4102 (not $x57)))
 (let (($x10421 (= (stack_s x_0 w_3 w_2 0 (_ bv0 6)) x_0)))
 (let (($x8685 (= ?x72 (_ bv1 6))))
 (and $x8685 $x10421 $x4102 $x894 $x8854 (= (stack_s x_0 w_3 w_2 1 (bvadd (_ bv63 6) ?x154)) ?x9568) (= (stack_s x_0 w_3 w_2 1 ?x9903) ?x9568) $x2561 $x1515 $x8435 $x3775 (= $x189 (or $x57 $x674 (not (bvsle (_ bv0 6) ?x9903)))) (= (stack_s x_0 w_3 w_2 2 ?x154) w_3) $x9203 $x430 $x4954 $x3817 (= $x247 (or $x189 $x1680)) $x5301 $x813 (= ?x275 ?x6729) $x6935 $x11739 $x1868 (= (stack_s x_0 w_3 w_2 4 ?x275) w_2) $x4997 $x11661 $x6373 $x6105 $x4234 (= ?x1002 ?x5047) (= ?x6347 ?x4368) $x8460 $x9388 $x4413 $x9627 $x1419 (= (stack_t x_0 w_3 w_2 1 ?x63) w_2) (= (used_gas_t x_0 w_3 w_2 1) (+ 3 ?x4965)) $x7773 $x9926 $x8120 $x8246 (= (stack_t x_0 w_3 w_2 2 ?x8347) w_3) $x5124 $x3851 $x8041 $x3951 $x9455 (= ?x9698 ?x2990) (= (stack_t x_0 w_3 w_2 3 ?x5805) ?x2990) $x7857 $x5482 $x2810 $x7112 $x11555 $x7996 (= $x6783 (or $x742 $x6954 $x10055)) $x7573 $x11379 (= ?x11631 ?x6883) $x5280 $x3260 $x9332 $x73 $x9594 $x58 $x5546 $x7276 (not (and $x8727 $x11797 $x10023 $x5326)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
 )
(check-sat)
(get-proof)