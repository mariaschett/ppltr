; DUP1 PUSH 0x00 ADD PUSH cw_2 SWAP1 => PUSH cw_2 DUP2
(set-option :produce-proofs true)
; 
(set-info :status unknown)
(declare-fun storage_t ((_ BitVec 256) (_ BitVec 256) Int (_ BitVec 256)) (_ BitVec 256))
(declare-fun w_2 () (_ BitVec 256))
(declare-fun storage_s ((_ BitVec 256) (_ BitVec 256) Int (_ BitVec 256)) (_ BitVec 256))
(declare-fun exc_halt_t (Int) Bool)
(declare-fun exc_halt_s (Int) Bool)
(declare-fun stack_t ((_ BitVec 256) (_ BitVec 256) Int (_ BitVec 6)) (_ BitVec 256))
(declare-fun stack_s ((_ BitVec 256) (_ BitVec 256) Int (_ BitVec 6)) (_ BitVec 256))
(declare-fun sc_t (Int) (_ BitVec 6))
(declare-fun sc_s (Int) (_ BitVec 6))
(declare-fun used_gas_t ((_ BitVec 256) (_ BitVec 256) Int) Int)
(declare-fun used_gas_s ((_ BitVec 256) (_ BitVec 256) Int) Int)
(assert
 (exists ((x_0 (_ BitVec 256)) )(let (($x5989 (forall ((w (_ BitVec 256)) )(let ((?x723 (storage_t x_0 w_2 2 w)))
 (let ((?x5687 (storage_s x_0 w_2 5 w)))
 (= ?x5687 ?x723))))
 ))
 (let (($x2163 (exc_halt_t 2)))
 (let (($x11317 (exc_halt_s 5)))
 (let (($x6773 (= $x11317 $x2163)))
 (let (($x6180 (forall ((n (_ BitVec 6)) )(let ((?x1440 (stack_t x_0 w_2 2 n)))
 (let ((?x5105 (stack_s x_0 w_2 5 n)))
 (let (($x9383 (= ?x5105 ?x1440)))
 (or (bvsle (sc_t 2) n) $x9383)))))
 ))
 (let ((?x2714 (sc_t 2)))
 (let ((?x4319 (sc_s 5)))
 (let (($x10201 (= ?x4319 ?x2714)))
 (let ((?x2515 (used_gas_t x_0 w_2 0)))
 (let ((?x9834 (used_gas_s x_0 w_2 0)))
 (let (($x783 (= ?x9834 ?x2515)))
 (let (($x9874 (forall ((w (_ BitVec 256)) )(let ((?x6646 (storage_t x_0 w_2 0 w)))
 (let ((?x351 (storage_s x_0 w_2 0 w)))
 (= ?x351 ?x6646))))
 ))
 (let (($x56 (exc_halt_t 0)))
 (let (($x57 (exc_halt_s 0)))
 (let (($x58 (= $x57 $x56)))
 (let (($x4515 (forall ((n (_ BitVec 6)) )(let ((?x63 (sc_t 0)))
 (let (($x5499 (bvsle ?x63 n)))
 (let ((?x11649 (stack_t x_0 w_2 0 n)))
 (let ((?x1474 (stack_s x_0 w_2 0 n)))
 (let (($x425 (= ?x1474 ?x11649)))
 (or $x425 $x5499)))))))
 ))
 (let ((?x63 (sc_t 0)))
 (let ((?x72 (sc_s 0)))
 (let (($x73 (= ?x72 ?x63)))
 (let (($x3508 (exc_halt_t 1)))
 (let (($x5462 (or $x3508 (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_t 1)))) (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_t 1)))) (_ bv0 1))))))
 (let (($x6523 (forall ((w (_ BitVec 256)) )(let ((?x5767 (storage_t x_0 w_2 1 w)))
 (let ((?x723 (storage_t x_0 w_2 2 w)))
 (= ?x723 ?x5767))))
 ))
 (let (($x10655 (forall ((n (_ BitVec 6)) )(or (= (stack_t x_0 w_2 2 n) (stack_t x_0 w_2 1 n)) (bvsle (bvadd (_ bv62 6) (sc_t 1)) n)))
 ))
 (let (($x8277 (= (used_gas_t x_0 w_2 2) (+ 3 (used_gas_t x_0 w_2 1)))))
 (let (($x11328 (= (stack_t x_0 w_2 2 (bvadd (_ bv63 6) (sc_t 1))) (stack_t x_0 w_2 1 (bvadd (_ bv63 6) (sc_t 1))))))
 (let ((?x8347 (sc_t 1)))
 (let ((?x11486 (bvadd (_ bv62 6) ?x8347)))
 (let ((?x2989 (stack_t x_0 w_2 1 ?x11486)))
 (let (($x1609 (= $x3508 (or $x56 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x63))) (_ bv0 1)))))))
 (let (($x9355 (forall ((w (_ BitVec 256)) )(let ((?x6646 (storage_t x_0 w_2 0 w)))
 (let ((?x5767 (storage_t x_0 w_2 1 w)))
 (= ?x5767 ?x6646))))
 ))
 (let (($x8466 (forall ((n (_ BitVec 6)) )(let ((?x63 (sc_t 0)))
 (let (($x5499 (bvsle ?x63 n)))
 (or $x5499 (= (stack_t x_0 w_2 1 n) (stack_t x_0 w_2 0 n))))))
 ))
 (let (($x705 (= $x11317 (or (exc_halt_s 4) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 4))))))))
 (let (($x4365 (forall ((w (_ BitVec 256)) )(let ((?x10569 (storage_s x_0 w_2 4 w)))
 (let ((?x5687 (storage_s x_0 w_2 5 w)))
 (= ?x5687 ?x10569))))
 ))
 (let (($x1311 (forall ((n (_ BitVec 6)) )(or (= (stack_s x_0 w_2 5 n) (stack_s x_0 w_2 4 n)) (bvsle (bvadd (_ bv62 6) (sc_s 4)) n)))
 ))
 (let (($x3316 (= (used_gas_s x_0 w_2 5) (+ 3 (used_gas_s x_0 w_2 4)))))
 (let (($x7507 (= (stack_s x_0 w_2 5 (bvadd (_ bv62 6) ?x4319)) (stack_s x_0 w_2 4 (bvadd (_ bv63 6) (sc_s 4))))))
 (let (($x3958 (= (stack_s x_0 w_2 5 (bvadd (_ bv63 6) ?x4319)) (stack_s x_0 w_2 4 (bvadd (_ bv62 6) (sc_s 4))))))
 (let (($x292 (exc_halt_s 3)))
 (let (($x4228 (or $x292 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 3)))) (_ bv0 1))))))
 (let (($x7172 (exc_halt_s 4)))
 (let (($x2007 (forall ((w (_ BitVec 256)) )(let ((?x4586 (storage_s x_0 w_2 3 w)))
 (let ((?x10569 (storage_s x_0 w_2 4 w)))
 (= ?x10569 ?x4586))))
 ))
 (let (($x5786 (forall ((n (_ BitVec 6)) )(or (bvsle (sc_s 3) n) (= (stack_s x_0 w_2 4 n) (stack_s x_0 w_2 3 n))))
 ))
 (let ((?x8593 (used_gas_s x_0 w_2 4)))
 (let (($x7282 (= $x292 (or (exc_halt_s 2) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 2))))))))
 (let (($x4299 (forall ((w (_ BitVec 256)) )(let ((?x115 (storage_s x_0 w_2 2 w)))
 (let ((?x4586 (storage_s x_0 w_2 3 w)))
 (= ?x4586 ?x115))))
 ))
 (let (($x5145 (forall ((n (_ BitVec 6)) )(or (= (stack_s x_0 w_2 3 n) (stack_s x_0 w_2 2 n)) (bvsle (bvadd (_ bv62 6) (sc_s 2)) n)))
 ))
 (let ((?x5397 (used_gas_s x_0 w_2 3)))
 (let ((?x6549 (bvadd (stack_s x_0 w_2 2 (bvadd (_ bv63 6) (sc_s 2))) (stack_s x_0 w_2 2 (bvadd (_ bv62 6) (sc_s 2))))))
 (let (($x189 (exc_halt_s 1)))
 (let (($x2739 (or $x189 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 1)))) (_ bv0 1))))))
 (let (($x247 (exc_halt_s 2)))
 (let (($x371 (forall ((w (_ BitVec 256)) )(let ((?x3476 (storage_s x_0 w_2 1 w)))
 (let ((?x115 (storage_s x_0 w_2 2 w)))
 (= ?x115 ?x3476))))
 ))
 (let (($x7125 (forall ((n (_ BitVec 6)) )(or (= (stack_s x_0 w_2 2 n) (stack_s x_0 w_2 1 n)) (bvsle (sc_s 1) n)))
 ))
 (let ((?x5859 (used_gas_s x_0 w_2 2)))
 (let (($x3536 (or $x57 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x72))) (_ bv0 1))) (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) ?x72))))))
 (let (($x7098 (forall ((w (_ BitVec 256)) )(let ((?x351 (storage_s x_0 w_2 0 w)))
 (let ((?x3476 (storage_s x_0 w_2 1 w)))
 (= ?x3476 ?x351))))
 ))
 (let (($x5309 (forall ((n (_ BitVec 6)) )(or (= (stack_s x_0 w_2 1 n) (stack_s x_0 w_2 0 n)) (bvsle (bvadd (_ bv63 6) (sc_s 0)) n)))
 ))
 (let ((?x4397 (bvadd (_ bv63 6) ?x72)))
 (let ((?x9429 (stack_s x_0 w_2 0 ?x4397)))
 (let (($x6990 (forall ((w (_ BitVec 256)) )(let ((?x351 (storage_s x_0 w_2 0 w)))
 (= ?x351 (_ bv0 256))))
 ))
 (let (($x3493 (= ?x9834 0)))
 (let (($x4843 (= (stack_s x_0 w_2 0 (_ bv0 6)) x_0)))
 (let (($x8685 (= ?x72 (_ bv1 6))))
 (and $x8685 $x4843 (not $x57) $x3493 $x6990 (= (stack_s x_0 w_2 1 (bvadd (_ bv63 6) (sc_s 1))) ?x9429) (= (stack_s x_0 w_2 1 ?x4397) ?x9429) (= (used_gas_s x_0 w_2 1) (+ 3 ?x9834)) (= (sc_s 1) (bvadd (_ bv1 6) ?x72)) $x5309 $x7098 (= $x189 $x3536) (= (stack_s x_0 w_2 2 (sc_s 1)) (_ bv0 256)) (= ?x5859 (+ 3 (used_gas_s x_0 w_2 1))) (= (sc_s 2) (bvadd (_ bv1 6) (sc_s 1))) $x7125 $x371 (= $x247 $x2739) (= (stack_s x_0 w_2 3 (bvadd (_ bv63 6) (sc_s 3))) ?x6549) (= ?x5397 (+ 3 ?x5859)) (= (sc_s 3) (bvadd (_ bv63 6) (sc_s 2))) $x5145 $x4299 $x7282 (= (stack_s x_0 w_2 4 (sc_s 3)) w_2) (= ?x8593 (+ 3 ?x5397)) (= (sc_s 4) (bvadd (_ bv1 6) (sc_s 3))) $x5786 $x2007 (= $x7172 $x4228) $x3958 $x7507 $x3316 (= ?x4319 (sc_s 4)) $x1311 $x4365 $x705 (= (stack_t x_0 w_2 1 ?x63) w_2) (= (used_gas_t x_0 w_2 1) (+ 3 ?x2515)) (= ?x8347 (bvadd (_ bv1 6) ?x63)) $x8466 $x9355 $x1609 (= (stack_t x_0 w_2 2 (bvadd (_ bv63 6) ?x2714)) ?x2989) (= (stack_t x_0 w_2 2 ?x11486) ?x2989) $x11328 $x8277 (= ?x2714 (bvadd (_ bv1 6) ?x8347)) $x10655 $x6523 (= $x2163 $x5462) $x73 $x4515 $x58 $x9874 $x783 (not (and $x10201 $x6180 $x6773 $x5989)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
 )
(check-sat)
(get-proof)
