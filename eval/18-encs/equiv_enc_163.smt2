; LT POP => POP POP
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
 (exists ((x_0 (_ BitVec 256)) (x_1 (_ BitVec 256)) )(let (($x10830 (forall ((w (_ BitVec 256)) )(let ((?x9472 (storage_t x_0 x_1 2 w)))
 (let ((?x2874 (storage_s x_0 x_1 2 w)))
 (= ?x2874 ?x9472))))
 ))
 (let (($x903 (exc_halt_t 2)))
 (let (($x247 (exc_halt_s 2)))
 (let (($x2345 (= $x247 $x903)))
 (let (($x2860 (forall ((n (_ BitVec 6)) )(let ((?x10642 (stack_t x_0 x_1 2 n)))
 (let ((?x4120 (stack_s x_0 x_1 2 n)))
 (let (($x10813 (= ?x4120 ?x10642)))
 (let ((?x4056 (sc_t 2)))
 (let (($x7395 (bvsle ?x4056 n)))
 (or $x7395 $x10813)))))))
 ))
 (let ((?x4056 (sc_t 2)))
 (let ((?x218 (sc_s 2)))
 (let (($x9630 (= ?x218 ?x4056)))
 (let ((?x729 (used_gas_t x_0 x_1 0)))
 (let ((?x2258 (used_gas_s x_0 x_1 0)))
 (let (($x980 (= ?x2258 ?x729)))
 (let (($x2971 (forall ((w (_ BitVec 256)) )(let ((?x8368 (storage_t x_0 x_1 0 w)))
 (let ((?x4247 (storage_s x_0 x_1 0 w)))
 (= ?x4247 ?x8368))))
 ))
 (let (($x56 (exc_halt_t 0)))
 (let (($x57 (exc_halt_s 0)))
 (let (($x58 (= $x57 $x56)))
 (let (($x9399 (forall ((n (_ BitVec 6)) )(let ((?x63 (sc_t 0)))
 (let (($x2695 (bvsle ?x63 n)))
 (let ((?x3390 (stack_t x_0 x_1 0 n)))
 (let ((?x8465 (stack_s x_0 x_1 0 n)))
 (let (($x2342 (= ?x8465 ?x3390)))
 (or $x2342 $x2695)))))))
 ))
 (let ((?x63 (sc_t 0)))
 (let ((?x72 (sc_s 0)))
 (let (($x73 (= ?x72 ?x63)))
 (let (($x2468 (= $x903 (or (exc_halt_t 1) (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_t 1))))))))
 (let (($x2177 (forall ((w (_ BitVec 256)) )(let ((?x9477 (storage_t x_0 x_1 1 w)))
 (let ((?x9472 (storage_t x_0 x_1 2 w)))
 (= ?x9472 ?x9477))))
 ))
 (let (($x4853 (forall ((n (_ BitVec 6)) )(let ((?x4023 (sc_t 1)))
 (let ((?x5606 (bvadd (_ bv63 6) ?x4023)))
 (let (($x481 (bvsle ?x5606 n)))
 (or $x481 (= (stack_t x_0 x_1 2 n) (stack_t x_0 x_1 1 n)))))))
 ))
 (let (($x4669 (= (used_gas_t x_0 x_1 2) (+ 2 (used_gas_t x_0 x_1 1)))))
 (let (($x5074 (forall ((w (_ BitVec 256)) )(let ((?x8368 (storage_t x_0 x_1 0 w)))
 (let ((?x9477 (storage_t x_0 x_1 1 w)))
 (= ?x9477 ?x8368))))
 ))
 (let (($x9388 (forall ((n (_ BitVec 6)) )(or (= (stack_t x_0 x_1 1 n) (stack_t x_0 x_1 0 n)) (bvsle (bvadd (_ bv63 6) (sc_t 0)) n)))
 ))
 (let (($x4347 (= $x247 (or (exc_halt_s 1) (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_s 1))))))))
 (let (($x4444 (forall ((w (_ BitVec 256)) )(let ((?x4702 (storage_s x_0 x_1 1 w)))
 (let ((?x2874 (storage_s x_0 x_1 2 w)))
 (= ?x2874 ?x4702))))
 ))
 (let (($x4800 (forall ((n (_ BitVec 6)) )(let ((?x154 (sc_s 1)))
 (let ((?x1806 (bvadd (_ bv63 6) ?x154)))
 (let (($x10480 (bvsle ?x1806 n)))
 (or $x10480 (= (stack_s x_0 x_1 2 n) (stack_s x_0 x_1 1 n)))))))
 ))
 (let (($x700 (= (used_gas_s x_0 x_1 2) (+ 2 (used_gas_s x_0 x_1 1)))))
 (let (($x909 (forall ((w (_ BitVec 256)) )(let ((?x4247 (storage_s x_0 x_1 0 w)))
 (let ((?x4702 (storage_s x_0 x_1 1 w)))
 (= ?x4702 ?x4247))))
 ))
 (let (($x4623 (forall ((n (_ BitVec 6)) )(let ((?x72 (sc_s 0)))
 (let ((?x1088 (bvadd (_ bv62 6) ?x72)))
 (let (($x1032 (bvsle ?x1088 n)))
 (or $x1032 (= (stack_s x_0 x_1 1 n) (stack_s x_0 x_1 0 n)))))))
 ))
 (let (($x8764 (bvule (stack_s x_0 x_1 0 (bvadd (_ bv62 6) ?x72)) (stack_s x_0 x_1 0 (bvadd (_ bv63 6) ?x72)))))
 (let (($x8526 (= (stack_s x_0 x_1 1 (bvadd (_ bv63 6) (sc_s 1))) (ite $x8764 (_ bv0 256) (_ bv1 256)))))
 (let (($x3881 (forall ((w (_ BitVec 256)) )(let ((?x4247 (storage_s x_0 x_1 0 w)))
 (= ?x4247 (_ bv0 256))))
 ))
 (let (($x1865 (= ?x2258 0)))
 (let (($x4490 (not $x57)))
 (let (($x9506 (= (stack_s x_0 x_1 0 (_ bv1 6)) x_1)))
 (let (($x10853 (= (stack_s x_0 x_1 0 (_ bv0 6)) x_0)))
 (let (($x3184 (= ?x72 (_ bv2 6))))
 (and $x3184 $x10853 $x9506 $x4490 $x1865 $x3881 $x8526 (= (used_gas_s x_0 x_1 1) (+ 3 ?x2258)) (= (sc_s 1) (bvadd (_ bv63 6) ?x72)) $x4623 $x909 (= (exc_halt_s 1) (or $x57 (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) ?x72))))) $x700 (= ?x218 (bvadd (_ bv63 6) (sc_s 1))) $x4800 $x4444 $x4347 (= (used_gas_t x_0 x_1 1) (+ 2 ?x729)) (= (sc_t 1) (bvadd (_ bv63 6) ?x63)) $x9388 $x5074 (= (exc_halt_t 1) (or $x56 (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) ?x63))))) $x4669 (= ?x4056 (bvadd (_ bv63 6) (sc_t 1))) $x4853 $x2177 $x2468 $x73 $x9399 $x58 $x2971 $x980 (not (and $x9630 $x2860 $x2345 $x10830)))))))))))))))))))))))))))))))))))))))))))
 )
(check-sat)
(get-proof)
