; PUSH 0x00 EQ => ISZERO
(set-option :produce-proofs true)
; 
(set-info :status unknown)
(declare-fun storage_t ((_ BitVec 256) Int (_ BitVec 256)) (_ BitVec 256))
(declare-fun storage_s ((_ BitVec 256) Int (_ BitVec 256)) (_ BitVec 256))
(declare-fun exc_halt_t (Int) Bool)
(declare-fun exc_halt_s (Int) Bool)
(declare-fun sc_t (Int) (_ BitVec 6))
(declare-fun stack_t ((_ BitVec 256) Int (_ BitVec 6)) (_ BitVec 256))
(declare-fun stack_s ((_ BitVec 256) Int (_ BitVec 6)) (_ BitVec 256))
(declare-fun sc_s (Int) (_ BitVec 6))
(declare-fun used_gas_t ((_ BitVec 256) Int) Int)
(declare-fun used_gas_s ((_ BitVec 256) Int) Int)
(assert
 (exists ((x_0 (_ BitVec 256)) )(let (($x293 (forall ((w (_ BitVec 256)) )(let ((?x599 (storage_t x_0 1 w)))
 (let ((?x6039 (storage_s x_0 2 w)))
 (= ?x6039 ?x599))))
 ))
 (let (($x8377 (exc_halt_t 1)))
 (let (($x247 (exc_halt_s 2)))
 (let (($x1281 (= $x247 $x8377)))
 (let (($x3307 (forall ((n (_ BitVec 6)) )(let ((?x7154 (sc_t 1)))
 (let (($x6622 (bvsle ?x7154 n)))
 (let ((?x1837 (stack_t x_0 1 n)))
 (let ((?x3140 (stack_s x_0 2 n)))
 (let (($x2692 (= ?x3140 ?x1837)))
 (or $x2692 $x6622)))))))
 ))
 (let ((?x7154 (sc_t 1)))
 (let ((?x218 (sc_s 2)))
 (let (($x2042 (= ?x218 ?x7154)))
 (let ((?x9854 (used_gas_t x_0 0)))
 (let ((?x10825 (used_gas_s x_0 0)))
 (let (($x3438 (= ?x10825 ?x9854)))
 (let (($x6262 (forall ((w (_ BitVec 256)) )(let ((?x7942 (storage_t x_0 0 w)))
 (let ((?x8113 (storage_s x_0 0 w)))
 (= ?x8113 ?x7942))))
 ))
 (let (($x56 (exc_halt_t 0)))
 (let (($x57 (exc_halt_s 0)))
 (let (($x58 (= $x57 $x56)))
 (let (($x1702 (forall ((n (_ BitVec 6)) )(let ((?x1924 (stack_t x_0 0 n)))
 (let ((?x1079 (stack_s x_0 0 n)))
 (let (($x5891 (= ?x1079 ?x1924)))
 (let ((?x63 (sc_t 0)))
 (let (($x4813 (bvsle ?x63 n)))
 (or $x4813 $x5891)))))))
 ))
 (let ((?x63 (sc_t 0)))
 (let ((?x72 (sc_s 0)))
 (let (($x73 (= ?x72 ?x63)))
 (let (($x8324 (= $x8377 (or $x56 (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) ?x63)))))))
 (let (($x5742 (forall ((w (_ BitVec 256)) )(let ((?x7942 (storage_t x_0 0 w)))
 (let ((?x599 (storage_t x_0 1 w)))
 (= ?x599 ?x7942))))
 ))
 (let (($x9074 (forall ((n (_ BitVec 6)) )(let ((?x63 (sc_t 0)))
 (let ((?x7312 (bvadd (_ bv63 6) ?x63)))
 (let (($x8332 (bvsle ?x7312 n)))
 (or (= (stack_t x_0 1 n) (stack_t x_0 0 n)) $x8332)))))
 ))
 (let (($x4006 (= (stack_t x_0 1 (bvadd (_ bv63 6) ?x7154)) (ite (= (stack_t x_0 0 (bvadd (_ bv63 6) ?x63)) (_ bv0 256)) (_ bv1 256) (_ bv0 256)))))
 (let (($x9849 (= $x247 (or (exc_halt_s 1) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 1))))))))
 (let (($x1861 (forall ((w (_ BitVec 256)) )(let ((?x6934 (storage_s x_0 1 w)))
 (let ((?x6039 (storage_s x_0 2 w)))
 (= ?x6039 ?x6934))))
 ))
 (let (($x853 (forall ((n (_ BitVec 6)) )(let ((?x154 (sc_s 1)))
 (let ((?x1237 (bvadd (_ bv62 6) ?x154)))
 (let (($x3989 (bvsle ?x1237 n)))
 (or $x3989 (= (stack_s x_0 2 n) (stack_s x_0 1 n)))))))
 ))
 (let (($x1357 (= (stack_s x_0 1 (bvadd (_ bv63 6) (sc_s 1))) (stack_s x_0 1 (bvadd (_ bv62 6) (sc_s 1))))))
 (let (($x189 (exc_halt_s 1)))
 (let (($x8008 (= $x189 (or $x57 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x72))) (_ bv0 1)))))))
 (let (($x9763 (forall ((w (_ BitVec 256)) )(let ((?x8113 (storage_s x_0 0 w)))
 (let ((?x6934 (storage_s x_0 1 w)))
 (= ?x6934 ?x8113))))
 ))
 (let (($x6204 (forall ((n (_ BitVec 6)) )(let ((?x72 (sc_s 0)))
 (let (($x10187 (bvsle ?x72 n)))
 (or (= (stack_s x_0 1 n) (stack_s x_0 0 n)) $x10187))))
 ))
 (let ((?x154 (sc_s 1)))
 (let (($x2373 (= ?x154 (bvadd (_ bv1 6) ?x72))))
 (let (($x8999 (forall ((w (_ BitVec 256)) )(let ((?x8113 (storage_s x_0 0 w)))
 (= ?x8113 (_ bv0 256))))
 ))
 (let (($x7539 (= ?x10825 0)))
 (let (($x5107 (not $x57)))
 (let (($x4102 (= (stack_s x_0 0 (_ bv0 6)) x_0)))
 (let (($x7865 (= ?x72 (_ bv1 6))))
 (and $x7865 $x4102 $x5107 $x7539 $x8999 (= (stack_s x_0 1 ?x72) (_ bv0 256)) (= (used_gas_s x_0 1) (+ 3 ?x10825)) $x2373 $x6204 $x9763 $x8008 (= (stack_s x_0 2 (bvadd (_ bv63 6) ?x218)) (ite $x1357 (_ bv1 256) (_ bv0 256))) (= (used_gas_s x_0 2) (+ 3 (used_gas_s x_0 1))) (= ?x218 (bvadd (_ bv63 6) ?x154)) $x853 $x1861 $x9849 $x4006 (= (used_gas_t x_0 1) (+ 3 ?x9854)) (= ?x7154 ?x63) $x9074 $x5742 $x8324 $x73 $x1702 $x58 $x6262 $x3438 (not (and $x2042 $x3307 $x1281 $x293))))))))))))))))))))))))))))))))))))))))))
 )
(check-sat)
(get-proof)