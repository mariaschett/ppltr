; PUSH 0x00 DUP1 PUSH cw_5 ADD PUSH 0x00 => PUSH 0x00 PUSH cw_5 DUP2
(set-option :produce-proofs true)
; 
(set-info :status unknown)
(declare-fun storage_t ((_ BitVec 256) Int (_ BitVec 256)) (_ BitVec 256))
(declare-fun w_5 () (_ BitVec 256))
(declare-fun storage_s ((_ BitVec 256) Int (_ BitVec 256)) (_ BitVec 256))
(declare-fun exc_halt_t (Int) Bool)
(declare-fun exc_halt_s (Int) Bool)
(declare-fun stack_t ((_ BitVec 256) Int (_ BitVec 6)) (_ BitVec 256))
(declare-fun stack_s ((_ BitVec 256) Int (_ BitVec 6)) (_ BitVec 256))
(declare-fun sc_t (Int) (_ BitVec 6))
(declare-fun sc_s (Int) (_ BitVec 6))
(declare-fun used_gas_t ((_ BitVec 256) Int) Int)
(declare-fun used_gas_s ((_ BitVec 256) Int) Int)
(assert
 (let (($x7369 (forall ((w (_ BitVec 256)) )(let ((?x3717 (storage_t w_5 3 w)))
 (let ((?x1210 (storage_s w_5 5 w)))
 (= ?x1210 ?x3717))))
 ))
 (let (($x3614 (exc_halt_t 3)))
 (let (($x11317 (exc_halt_s 5)))
 (let (($x3494 (= $x11317 $x3614)))
 (let (($x9308 (forall ((n (_ BitVec 6)) )(let ((?x10492 (stack_t w_5 3 n)))
 (let ((?x3362 (stack_s w_5 5 n)))
 (let (($x11001 (= ?x3362 ?x10492)))
 (let ((?x11304 (sc_t 3)))
 (let (($x2198 (bvsle ?x11304 n)))
 (or $x2198 $x11001)))))))
 ))
 (let ((?x11304 (sc_t 3)))
 (let ((?x4319 (sc_s 5)))
 (let (($x1839 (= ?x4319 ?x11304)))
 (let ((?x9941 (used_gas_t w_5 0)))
 (let ((?x9666 (used_gas_s w_5 0)))
 (let (($x8225 (= ?x9666 ?x9941)))
 (let (($x3933 (forall ((w (_ BitVec 256)) )(let ((?x9274 (storage_t w_5 0 w)))
 (let ((?x4043 (storage_s w_5 0 w)))
 (= ?x4043 ?x9274))))
 ))
 (let (($x56 (exc_halt_t 0)))
 (let (($x57 (exc_halt_s 0)))
 (let (($x58 (= $x57 $x56)))
 (let (($x4336 (forall ((n (_ BitVec 6)) )(let ((?x6259 (stack_t w_5 0 n)))
 (let ((?x3165 (stack_s w_5 0 n)))
 (let (($x11692 (= ?x3165 ?x6259)))
 (let ((?x63 (sc_t 0)))
 (let (($x10756 (bvsle ?x63 n)))
 (or $x10756 $x11692)))))))
 ))
 (let ((?x63 (sc_t 0)))
 (let ((?x72 (sc_s 0)))
 (let (($x73 (= ?x72 ?x63)))
 (let (($x9682 (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_t 2))))))
 (let (($x4500 (exc_halt_t 2)))
 (let (($x1807 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_t 2)))) (_ bv0 1)))))
 (let (($x6255 (forall ((w (_ BitVec 256)) )(let ((?x11217 (storage_t w_5 2 w)))
 (let ((?x3717 (storage_t w_5 3 w)))
 (= ?x3717 ?x11217))))
 ))
 (let (($x6542 (forall ((n (_ BitVec 6)) )(let ((?x2714 (sc_t 2)))
 (let ((?x1705 (bvadd (_ bv62 6) ?x2714)))
 (let (($x10643 (bvsle ?x1705 n)))
 (or (= (stack_t w_5 3 n) (stack_t w_5 2 n)) $x10643)))))
 ))
 (let (($x8731 (= ?x11304 (bvadd (_ bv1 6) (sc_t 2)))))
 (let (($x4742 (= (stack_t w_5 3 (bvadd (_ bv63 6) (sc_t 2))) (stack_t w_5 2 (bvadd (_ bv63 6) (sc_t 2))))))
 (let ((?x2714 (sc_t 2)))
 (let ((?x1705 (bvadd (_ bv62 6) ?x2714)))
 (let ((?x9937 (stack_t w_5 2 ?x1705)))
 (let (($x8377 (exc_halt_t 1)))
 (let (($x6611 (or $x8377 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_t 1)))) (_ bv0 1))))))
 (let (($x1605 (= $x4500 $x6611)))
 (let (($x11267 (forall ((w (_ BitVec 256)) )(let ((?x5157 (storage_t w_5 1 w)))
 (let ((?x11217 (storage_t w_5 2 w)))
 (= ?x11217 ?x5157))))
 ))
 (let (($x469 (forall ((n (_ BitVec 6)) )(let ((?x7154 (sc_t 1)))
 (let (($x1943 (bvsle ?x7154 n)))
 (or $x1943 (= (stack_t w_5 2 n) (stack_t w_5 1 n))))))
 ))
 (let (($x10940 (= ?x2714 (bvadd (_ bv1 6) (sc_t 1)))))
 (let (($x8120 (= $x8377 (or $x56 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x63))) (_ bv0 1)))))))
 (let (($x2398 (forall ((w (_ BitVec 256)) )(let ((?x9274 (storage_t w_5 0 w)))
 (let ((?x5157 (storage_t w_5 1 w)))
 (= ?x5157 ?x9274))))
 ))
 (let (($x160 (forall ((n (_ BitVec 6)) )(let ((?x63 (sc_t 0)))
 (let (($x10756 (bvsle ?x63 n)))
 (or (= (stack_t w_5 1 n) (stack_t w_5 0 n)) $x10756))))
 ))
 (let ((?x7154 (sc_t 1)))
 (let (($x3866 (= ?x7154 (bvadd (_ bv1 6) ?x63))))
 (let (($x7172 (exc_halt_s 4)))
 (let (($x9629 (or $x7172 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 4)))) (_ bv0 1))))))
 (let (($x8321 (= $x11317 $x9629)))
 (let (($x6057 (forall ((w (_ BitVec 256)) )(let ((?x11714 (storage_s w_5 4 w)))
 (let ((?x1210 (storage_s w_5 5 w)))
 (= ?x1210 ?x11714))))
 ))
 (let (($x6479 (forall ((n (_ BitVec 6)) )(let ((?x4305 (sc_s 4)))
 (let (($x6840 (bvsle ?x4305 n)))
 (or $x6840 (= (stack_s w_5 5 n) (stack_s w_5 4 n))))))
 ))
 (let (($x10371 (= ?x4319 (bvadd (_ bv1 6) (sc_s 4)))))
 (let (($x2634 (= $x7172 (or (exc_halt_s 3) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 3))))))))
 (let (($x7181 (forall ((w (_ BitVec 256)) )(let ((?x2616 (storage_s w_5 3 w)))
 (let ((?x11714 (storage_s w_5 4 w)))
 (= ?x11714 ?x2616))))
 ))
 (let (($x7592 (forall ((n (_ BitVec 6)) )(let ((?x275 (sc_s 3)))
 (let ((?x9733 (bvadd (_ bv62 6) ?x275)))
 (let (($x8371 (bvsle ?x9733 n)))
 (or (= (stack_s w_5 4 n) (stack_s w_5 3 n)) $x8371)))))
 ))
 (let ((?x275 (sc_s 3)))
 (let ((?x397 (bvadd (_ bv63 6) ?x275)))
 (let ((?x4305 (sc_s 4)))
 (let (($x974 (= ?x4305 ?x397)))
 (let ((?x4457 (bvadd (stack_s w_5 3 ?x397) (stack_s w_5 3 (bvadd (_ bv62 6) ?x275)))))
 (let (($x7298 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 2)))) (_ bv0 1)))))
 (let (($x247 (exc_halt_s 2)))
 (let (($x292 (exc_halt_s 3)))
 (let (($x6062 (forall ((w (_ BitVec 256)) )(let ((?x8004 (storage_s w_5 2 w)))
 (let ((?x2616 (storage_s w_5 3 w)))
 (= ?x2616 ?x8004))))
 ))
 (let (($x1788 (forall ((n (_ BitVec 6)) )(or (= (stack_s w_5 3 n) (stack_s w_5 2 n)) (bvsle (sc_s 2) n)))
 ))
 (let (($x7996 (= ?x275 (bvadd (_ bv1 6) (sc_s 2)))))
 (let (($x10616 (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_s 1))))))
 (let (($x4355 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 1)))) (_ bv0 1)))))
 (let (($x189 (exc_halt_s 1)))
 (let (($x10057 (= $x247 (or $x189 $x4355 $x10616))))
 (let (($x6519 (forall ((w (_ BitVec 256)) )(let ((?x2776 (storage_s w_5 1 w)))
 (let ((?x8004 (storage_s w_5 2 w)))
 (= ?x8004 ?x2776))))
 ))
 (let (($x11806 (forall ((n (_ BitVec 6)) )(let ((?x154 (sc_s 1)))
 (let ((?x1744 (bvadd (_ bv63 6) ?x154)))
 (let (($x4872 (bvsle ?x1744 n)))
 (or (= (stack_s w_5 2 n) (stack_s w_5 1 n)) $x4872)))))
 ))
 (let ((?x218 (sc_s 2)))
 (let (($x4558 (= ?x218 (bvadd (_ bv1 6) (sc_s 1)))))
 (let ((?x154 (sc_s 1)))
 (let ((?x1744 (bvadd (_ bv63 6) ?x154)))
 (let ((?x6876 (stack_s w_5 1 ?x1744)))
 (let (($x9742 (= $x189 (or $x57 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x72))) (_ bv0 1)))))))
 (let (($x9921 (forall ((w (_ BitVec 256)) )(let ((?x4043 (storage_s w_5 0 w)))
 (let ((?x2776 (storage_s w_5 1 w)))
 (= ?x2776 ?x4043))))
 ))
 (let (($x3579 (forall ((n (_ BitVec 6)) )(let ((?x72 (sc_s 0)))
 (let (($x8583 (bvsle ?x72 n)))
 (or $x8583 (= (stack_s w_5 1 n) (stack_s w_5 0 n))))))
 ))
 (let (($x4777 (= ?x154 (bvadd (_ bv1 6) ?x72))))
 (let (($x3191 (forall ((w (_ BitVec 256)) )(let ((?x4043 (storage_s w_5 0 w)))
 (= ?x4043 (_ bv0 256))))
 ))
 (let (($x3352 (= ?x9666 0)))
 (let (($x11625 (not $x57)))
 (let (($x1074 (= ?x72 (_ bv0 6))))
 (and $x1074 $x11625 $x3352 $x3191 (= (stack_s w_5 1 ?x72) (_ bv0 256)) (= (used_gas_s w_5 1) (+ 3 ?x9666)) $x4777 $x3579 $x9921 $x9742 (= (stack_s w_5 2 (bvadd (_ bv63 6) ?x218)) ?x6876) (= (stack_s w_5 2 ?x1744) ?x6876) (= (used_gas_s w_5 2) (+ 3 (used_gas_s w_5 1))) $x4558 $x11806 $x6519 $x10057 (= (stack_s w_5 3 ?x218) w_5) (= (used_gas_s w_5 3) (+ 3 (used_gas_s w_5 2))) $x7996 $x1788 $x6062 (= $x292 (or $x247 $x7298)) (= (stack_s w_5 4 (bvadd (_ bv63 6) ?x4305)) ?x4457) (= (used_gas_s w_5 4) (+ 3 (used_gas_s w_5 3))) $x974 $x7592 $x7181 $x2634 (= (stack_s w_5 5 ?x4305) (_ bv0 256)) (= (used_gas_s w_5 5) (+ 3 (used_gas_s w_5 4))) $x10371 $x6479 $x6057 $x8321 (= (stack_t w_5 1 ?x63) (_ bv0 256)) (= (used_gas_t w_5 1) (+ 3 ?x9941)) $x3866 $x160 $x2398 $x8120 (= (stack_t w_5 2 ?x7154) w_5) (= (used_gas_t w_5 2) (+ 3 (used_gas_t w_5 1))) $x10940 $x469 $x11267 $x1605 (= (stack_t w_5 3 (bvadd (_ bv63 6) ?x11304)) ?x9937) (= (stack_t w_5 3 ?x1705) ?x9937) $x4742 (= (used_gas_t w_5 3) (+ 3 (used_gas_t w_5 2))) $x8731 $x6542 $x6255 (= $x3614 (or $x1807 $x4500 $x9682)) $x73 $x4336 $x58 $x3933 $x8225 (not (and $x1839 $x9308 $x3494 $x7369)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
(check-sat)
(get-proof)