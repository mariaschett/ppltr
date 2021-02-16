; DUP3 SWAP3 POP => 
(set-option :produce-proofs true)
; 
(set-info :status unknown)
(declare-fun storage_t ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 256)) (_ BitVec 256))
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
 (exists ((x_0 (_ BitVec 256)) (x_1 (_ BitVec 256)) (x_2 (_ BitVec 256)) )(let (($x11241 (forall ((w (_ BitVec 256)) )(let ((?x1976 (storage_t x_0 x_1 x_2 0 w)))
 (let ((?x8054 (storage_s x_0 x_1 x_2 3 w)))
 (= ?x8054 ?x1976))))
 ))
 (let (($x56 (exc_halt_t 0)))
 (let (($x292 (exc_halt_s 3)))
 (let (($x10246 (= $x292 $x56)))
 (let (($x11106 (forall ((n (_ BitVec 6)) )(let ((?x63 (sc_t 0)))
 (let (($x7673 (bvsle ?x63 n)))
 (let ((?x8040 (stack_t x_0 x_1 x_2 0 n)))
 (let ((?x6734 (stack_s x_0 x_1 x_2 3 n)))
 (let (($x5047 (= ?x6734 ?x8040)))
 (or $x5047 $x7673)))))))
 ))
 (let ((?x63 (sc_t 0)))
 (let ((?x275 (sc_s 3)))
 (let (($x5655 (= ?x275 ?x63)))
 (let ((?x6693 (used_gas_s x_0 x_1 x_2 0)))
 (let (($x9266 (= ?x6693 (used_gas_t x_0 x_1 x_2 0))))
 (let (($x192 (forall ((w (_ BitVec 256)) )(let ((?x1976 (storage_t x_0 x_1 x_2 0 w)))
 (let ((?x2672 (storage_s x_0 x_1 x_2 0 w)))
 (= ?x2672 ?x1976))))
 ))
 (let (($x57 (exc_halt_s 0)))
 (let (($x58 (= $x57 $x56)))
 (let (($x80 (forall ((n (_ BitVec 6)) )(let ((?x63 (sc_t 0)))
 (let (($x7673 (bvsle ?x63 n)))
 (let ((?x8040 (stack_t x_0 x_1 x_2 0 n)))
 (let ((?x4465 (stack_s x_0 x_1 x_2 0 n)))
 (let (($x330 (= ?x4465 ?x8040)))
 (or $x330 $x7673)))))))
 ))
 (let ((?x72 (sc_s 0)))
 (let (($x73 (= ?x72 ?x63)))
 (let (($x3936 (= $x292 (or (exc_halt_s 2) (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_s 2))))))))
 (let (($x705 (forall ((w (_ BitVec 256)) )(let ((?x4839 (storage_s x_0 x_1 x_2 2 w)))
 (let ((?x8054 (storage_s x_0 x_1 x_2 3 w)))
 (= ?x8054 ?x4839))))
 ))
 (let (($x11032 (forall ((n (_ BitVec 6)) )(let ((?x3599 (stack_s x_0 x_1 x_2 2 n)))
 (let ((?x6734 (stack_s x_0 x_1 x_2 3 n)))
 (or (bvsle (bvadd (_ bv63 6) (sc_s 2)) n) (= ?x6734 ?x3599)))))
 ))
 (let (($x454 (= (used_gas_s x_0 x_1 x_2 3) (+ 2 (used_gas_s x_0 x_1 x_2 2)))))
 (let (($x247 (exc_halt_s 2)))
 (let (($x10387 (= $x247 (or (exc_halt_s 1) (not (bvsle (_ bv0 6) (bvadd (_ bv60 6) (sc_s 1))))))))
 (let (($x9964 (forall ((w (_ BitVec 256)) )(let ((?x7571 (storage_s x_0 x_1 x_2 1 w)))
 (let ((?x4839 (storage_s x_0 x_1 x_2 2 w)))
 (= ?x4839 ?x7571))))
 ))
 (let (($x4276 (forall ((n (_ BitVec 6)) )(let ((?x7675 (stack_s x_0 x_1 x_2 1 n)))
 (let ((?x3599 (stack_s x_0 x_1 x_2 2 n)))
 (or (bvsle (bvadd (_ bv60 6) (sc_s 1)) n) (= ?x3599 ?x7675)))))
 ))
 (let ((?x154 (sc_s 1)))
 (let ((?x218 (sc_s 2)))
 (let (($x3064 (= ?x218 ?x154)))
 (let ((?x9457 (used_gas_s x_0 x_1 x_2 2)))
 (let (($x5411 (= (stack_s x_0 x_1 x_2 2 (bvadd (_ bv62 6) ?x218)) (stack_s x_0 x_1 x_2 1 (bvadd (_ bv62 6) ?x154)))))
 (let (($x9018 (= (stack_s x_0 x_1 x_2 2 (bvadd (_ bv61 6) ?x218)) (stack_s x_0 x_1 x_2 1 (bvadd (_ bv61 6) ?x154)))))
 (let ((?x174 (bvadd (_ bv63 6) ?x154)))
 (let ((?x1665 (stack_s x_0 x_1 x_2 1 ?x174)))
 (let (($x6499 (= (stack_s x_0 x_1 x_2 2 (bvadd (_ bv63 6) ?x218)) (stack_s x_0 x_1 x_2 1 (bvadd (_ bv60 6) ?x154)))))
 (let (($x9839 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x72))) (_ bv0 1)))))
 (let (($x189 (exc_halt_s 1)))
 (let (($x759 (forall ((w (_ BitVec 256)) )(let ((?x2672 (storage_s x_0 x_1 x_2 0 w)))
 (let ((?x7571 (storage_s x_0 x_1 x_2 1 w)))
 (= ?x7571 ?x2672))))
 ))
 (let (($x11737 (forall ((n (_ BitVec 6)) )(let ((?x4465 (stack_s x_0 x_1 x_2 0 n)))
 (let ((?x7675 (stack_s x_0 x_1 x_2 1 n)))
 (or (= ?x7675 ?x4465) (bvsle (bvadd (_ bv61 6) (sc_s 0)) n)))))
 ))
 (let (($x8655 (= ?x154 (bvadd (_ bv1 6) ?x72))))
 (let (($x3519 (= (stack_s x_0 x_1 x_2 1 (bvadd (_ bv63 6) ?x72)) (stack_s x_0 x_1 x_2 0 (bvadd (_ bv63 6) ?x72)))))
 (let (($x2393 (= (stack_s x_0 x_1 x_2 1 (bvadd (_ bv62 6) ?x72)) (stack_s x_0 x_1 x_2 0 (bvadd (_ bv62 6) ?x72)))))
 (let ((?x4254 (bvadd (_ bv61 6) ?x72)))
 (let ((?x4410 (stack_s x_0 x_1 x_2 0 ?x4254)))
 (let (($x4876 (forall ((w (_ BitVec 256)) )(let ((?x2672 (storage_s x_0 x_1 x_2 0 w)))
 (= ?x2672 (_ bv0 256))))
 ))
 (let (($x8681 (= ?x6693 0)))
 (let (($x7303 (not $x57)))
 (let (($x1028 (= (stack_s x_0 x_1 x_2 0 (_ bv2 6)) x_2)))
 (let (($x8063 (= (stack_s x_0 x_1 x_2 0 (_ bv1 6)) x_1)))
 (let (($x8624 (= (stack_s x_0 x_1 x_2 0 (_ bv0 6)) x_0)))
 (let (($x3537 (= ?x72 (_ bv3 6))))
 (and $x3537 $x8624 $x8063 $x1028 $x7303 $x8681 $x4876 (= ?x1665 ?x4410) (= (stack_s x_0 x_1 x_2 1 ?x4254) ?x4410) $x2393 $x3519 (= (used_gas_s x_0 x_1 x_2 1) (+ 3 ?x6693)) $x8655 $x11737 $x759 (= $x189 (or $x57 (not (bvsle (_ bv0 6) ?x4254)) $x9839)) $x6499 (= (stack_s x_0 x_1 x_2 2 (bvadd (_ bv60 6) ?x218)) ?x1665) $x9018 $x5411 (= ?x9457 (+ 3 (used_gas_s x_0 x_1 x_2 1))) $x3064 $x4276 $x9964 $x10387 $x454 (= ?x275 (bvadd (_ bv63 6) ?x218)) $x11032 $x705 $x3936 $x73 $x80 $x58 $x192 $x9266 (not (and $x5655 $x11106 $x10246 $x11241)))))))))))))))))))))))))))))))))))))))))))))))))))))
 )
(check-sat)
(get-proof)