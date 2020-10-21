; DUP2 ADD SWAP1 POP => ADD
(set-option :produce-proofs true)
; 
(set-info :status unknown)
(declare-fun storage_t ((_ BitVec 256) (_ BitVec 256) Int (_ BitVec 256)) (_ BitVec 256))
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
 (exists ((x_0 (_ BitVec 256)) (x_1 (_ BitVec 256)) )(let (($x9677 (forall ((w (_ BitVec 256)) )(let ((?x5584 (storage_t x_0 x_1 1 w)))
 (let ((?x9393 (storage_s x_0 x_1 4 w)))
 (= ?x9393 ?x5584))))
 ))
 (let (($x1920 (exc_halt_t 1)))
 (let (($x64 (exc_halt_s 4)))
 (let (($x8700 (= $x64 $x1920)))
 (let (($x2192 (forall ((n (_ BitVec 6)) )(let ((?x7879 (stack_t x_0 x_1 1 n)))
 (let ((?x3071 (stack_s x_0 x_1 4 n)))
 (let (($x5679 (= ?x3071 ?x7879)))
 (let ((?x4023 (sc_t 1)))
 (let (($x3531 (bvsle ?x4023 n)))
 (or $x3531 $x5679)))))))
 ))
 (let ((?x4023 (sc_t 1)))
 (let ((?x4305 (sc_s 4)))
 (let (($x2857 (= ?x4305 ?x4023)))
 (let ((?x6109 (used_gas_t x_0 x_1 0)))
 (let ((?x1357 (used_gas_s x_0 x_1 0)))
 (let (($x7533 (= ?x1357 ?x6109)))
 (let (($x10530 (forall ((w (_ BitVec 256)) )(let ((?x7998 (storage_t x_0 x_1 0 w)))
 (let ((?x1246 (storage_s x_0 x_1 0 w)))
 (= ?x1246 ?x7998))))
 ))
 (let (($x56 (exc_halt_t 0)))
 (let (($x57 (exc_halt_s 0)))
 (let (($x58 (= $x57 $x56)))
 (let (($x3198 (forall ((n (_ BitVec 6)) )(let ((?x63 (sc_t 0)))
 (let (($x4861 (bvsle ?x63 n)))
 (let ((?x11362 (stack_t x_0 x_1 0 n)))
 (let ((?x412 (stack_s x_0 x_1 0 n)))
 (let (($x2492 (= ?x412 ?x11362)))
 (or $x2492 $x4861)))))))
 ))
 (let ((?x63 (sc_t 0)))
 (let ((?x72 (sc_s 0)))
 (let (($x73 (= ?x72 ?x63)))
 (let (($x10916 (forall ((w (_ BitVec 256)) )(let ((?x7998 (storage_t x_0 x_1 0 w)))
 (let ((?x5584 (storage_t x_0 x_1 1 w)))
 (= ?x5584 ?x7998))))
 ))
 (let (($x2793 (forall ((n (_ BitVec 6)) )(let ((?x63 (sc_t 0)))
 (let ((?x8305 (bvadd (_ bv62 6) ?x63)))
 (let (($x9662 (bvsle ?x8305 n)))
 (or (= (stack_t x_0 x_1 1 n) (stack_t x_0 x_1 0 n)) $x9662)))))
 ))
 (let ((?x4060 (bvadd (stack_t x_0 x_1 0 (bvadd (_ bv63 6) ?x63)) (stack_t x_0 x_1 0 (bvadd (_ bv62 6) ?x63)))))
 (let (($x2205 (= $x64 (or (exc_halt_s 3) (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_s 3))))))))
 (let (($x3097 (forall ((w (_ BitVec 256)) )(let ((?x1522 (storage_s x_0 x_1 3 w)))
 (let ((?x9393 (storage_s x_0 x_1 4 w)))
 (= ?x9393 ?x1522))))
 ))
 (let (($x4748 (forall ((n (_ BitVec 6)) )(let ((?x275 (sc_s 3)))
 (let ((?x7846 (bvadd (_ bv63 6) ?x275)))
 (let (($x3202 (bvsle ?x7846 n)))
 (or $x3202 (= (stack_s x_0 x_1 4 n) (stack_s x_0 x_1 3 n)))))))
 ))
 (let ((?x275 (sc_s 3)))
 (let ((?x7846 (bvadd (_ bv63 6) ?x275)))
 (let (($x7554 (= ?x4305 ?x7846)))
 (let (($x2210 (= (used_gas_s x_0 x_1 4) (+ 2 (used_gas_s x_0 x_1 3)))))
 (let (($x292 (exc_halt_s 3)))
 (let (($x7717 (= $x292 (or (exc_halt_s 2) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 2))))))))
 (let (($x5099 (forall ((w (_ BitVec 256)) )(let ((?x10298 (storage_s x_0 x_1 2 w)))
 (let ((?x1522 (storage_s x_0 x_1 3 w)))
 (= ?x1522 ?x10298))))
 ))
 (let (($x2175 (forall ((n (_ BitVec 6)) )(let ((?x218 (sc_s 2)))
 (let ((?x4288 (bvadd (_ bv62 6) ?x218)))
 (let (($x769 (bvsle ?x4288 n)))
 (or $x769 (= (stack_s x_0 x_1 3 n) (stack_s x_0 x_1 2 n)))))))
 ))
 (let ((?x218 (sc_s 2)))
 (let (($x1939 (= ?x275 ?x218)))
 (let ((?x10268 (used_gas_s x_0 x_1 3)))
 (let ((?x9274 (bvadd (_ bv63 6) ?x218)))
 (let ((?x7230 (stack_s x_0 x_1 2 ?x9274)))
 (let (($x3755 (= (stack_s x_0 x_1 3 ?x7846) (stack_s x_0 x_1 2 (bvadd (_ bv62 6) ?x218)))))
 (let (($x247 (exc_halt_s 2)))
 (let (($x9930 (= $x247 (or (exc_halt_s 1) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 1))))))))
 (let (($x1370 (forall ((w (_ BitVec 256)) )(let ((?x8325 (storage_s x_0 x_1 1 w)))
 (let ((?x10298 (storage_s x_0 x_1 2 w)))
 (= ?x10298 ?x8325))))
 ))
 (let (($x10626 (forall ((n (_ BitVec 6)) )(let ((?x154 (sc_s 1)))
 (let ((?x11305 (bvadd (_ bv62 6) ?x154)))
 (let (($x9046 (bvsle ?x11305 n)))
 (or (= (stack_s x_0 x_1 2 n) (stack_s x_0 x_1 1 n)) $x9046)))))
 ))
 (let ((?x154 (sc_s 1)))
 (let ((?x2046 (bvadd (_ bv63 6) ?x154)))
 (let (($x9202 (= ?x218 ?x2046)))
 (let ((?x729 (used_gas_s x_0 x_1 2)))
 (let ((?x2560 (stack_s x_0 x_1 1 ?x2046)))
 (let (($x7701 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x72))) (_ bv0 1)))))
 (let (($x4183 (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) ?x72)))))
 (let (($x189 (exc_halt_s 1)))
 (let (($x3652 (= $x189 (or $x57 $x4183 $x7701))))
 (let (($x6954 (forall ((w (_ BitVec 256)) )(let ((?x1246 (storage_s x_0 x_1 0 w)))
 (let ((?x8325 (storage_s x_0 x_1 1 w)))
 (= ?x8325 ?x1246))))
 ))
 (let (($x10420 (forall ((n (_ BitVec 6)) )(let ((?x72 (sc_s 0)))
 (let ((?x7740 (bvadd (_ bv62 6) ?x72)))
 (let (($x10958 (bvsle ?x7740 n)))
 (or (= (stack_s x_0 x_1 1 n) (stack_s x_0 x_1 0 n)) $x10958)))))
 ))
 (let (($x7777 (= ?x154 (bvadd (_ bv1 6) ?x72))))
 (let (($x11411 (= (stack_s x_0 x_1 1 (bvadd (_ bv63 6) ?x72)) (stack_s x_0 x_1 0 (bvadd (_ bv63 6) ?x72)))))
 (let ((?x7740 (bvadd (_ bv62 6) ?x72)))
 (let ((?x10653 (stack_s x_0 x_1 0 ?x7740)))
 (let (($x5834 (forall ((w (_ BitVec 256)) )(let ((?x1246 (storage_s x_0 x_1 0 w)))
 (= ?x1246 (_ bv0 256))))
 ))
 (let (($x7020 (= ?x1357 0)))
 (let (($x5326 (not $x57)))
 (let (($x3184 (= (stack_s x_0 x_1 0 (_ bv1 6)) x_1)))
 (let (($x8398 (= (stack_s x_0 x_1 0 (_ bv0 6)) x_0)))
 (let (($x9078 (= ?x72 (_ bv2 6))))
 (and $x9078 $x8398 $x3184 $x5326 $x7020 $x5834 (= ?x2560 ?x10653) (= (stack_s x_0 x_1 1 ?x7740) ?x10653) $x11411 (= (used_gas_s x_0 x_1 1) (+ 3 ?x1357)) $x7777 $x10420 $x6954 $x3652 (= ?x7230 (bvadd ?x2560 (stack_s x_0 x_1 1 (bvadd (_ bv62 6) ?x154)))) (= ?x729 (+ 3 (used_gas_s x_0 x_1 1))) $x9202 $x10626 $x1370 $x9930 $x3755 (= (stack_s x_0 x_1 3 (bvadd (_ bv62 6) ?x275)) ?x7230) (= ?x10268 (+ 3 ?x729)) $x1939 $x2175 $x5099 $x7717 $x2210 $x7554 $x4748 $x3097 $x2205 (= (stack_t x_0 x_1 1 (bvadd (_ bv63 6) ?x4023)) ?x4060) (= (used_gas_t x_0 x_1 1) (+ 3 ?x6109)) (= ?x4023 (bvadd (_ bv63 6) ?x63)) $x2793 $x10916 (= $x1920 (or $x56 (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) ?x63))))) $x73 $x3198 $x58 $x10530 $x7533 (not (and $x2857 $x2192 $x8700 $x9677))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
 )
(check-sat)
(get-proof)
