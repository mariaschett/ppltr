; TIMESTAMP POP => 
(set-option :produce-proofs true)
; 
(set-info :status unknown)
(declare-fun storage_t ((_ BitVec 256) Int (_ BitVec 256)) (_ BitVec 256))
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
 (exists ((x_TIMESTAMP (_ BitVec 256)) )(let (($x5999 (forall ((w (_ BitVec 256)) )(let ((?x5033 (storage_t x_TIMESTAMP 0 w)))
 (let ((?x514 (storage_s x_TIMESTAMP 2 w)))
 (= ?x514 ?x5033))))
 ))
 (let (($x56 (exc_halt_t 0)))
 (let (($x247 (exc_halt_s 2)))
 (let (($x9812 (= $x247 $x56)))
 (let (($x6597 (forall ((n (_ BitVec 6)) )(let ((?x2507 (stack_t x_TIMESTAMP 0 n)))
 (let ((?x7325 (stack_s x_TIMESTAMP 2 n)))
 (let (($x9670 (= ?x7325 ?x2507)))
 (let ((?x63 (sc_t 0)))
 (let (($x4577 (bvsle ?x63 n)))
 (or $x4577 $x9670)))))))
 ))
 (let ((?x63 (sc_t 0)))
 (let ((?x218 (sc_s 2)))
 (let (($x8460 (= ?x218 ?x63)))
 (let ((?x7117 (used_gas_s x_TIMESTAMP 0)))
 (let (($x4319 (= ?x7117 (used_gas_t x_TIMESTAMP 0))))
 (let (($x2943 (forall ((w (_ BitVec 256)) )(let ((?x5033 (storage_t x_TIMESTAMP 0 w)))
 (let ((?x6670 (storage_s x_TIMESTAMP 0 w)))
 (= ?x6670 ?x5033))))
 ))
 (let (($x57 (exc_halt_s 0)))
 (let (($x58 (= $x57 $x56)))
 (let (($x2018 (forall ((n (_ BitVec 6)) )(let ((?x2507 (stack_t x_TIMESTAMP 0 n)))
 (let ((?x6320 (stack_s x_TIMESTAMP 0 n)))
 (let (($x5659 (= ?x6320 ?x2507)))
 (let ((?x63 (sc_t 0)))
 (let (($x4577 (bvsle ?x63 n)))
 (or $x4577 $x5659)))))))
 ))
 (let ((?x72 (sc_s 0)))
 (let (($x73 (= ?x72 ?x63)))
 (let (($x1521 (= $x247 (or (exc_halt_s 1) (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_s 1))))))))
 (let (($x7368 (forall ((w (_ BitVec 256)) )(let ((?x9000 (storage_s x_TIMESTAMP 1 w)))
 (let ((?x514 (storage_s x_TIMESTAMP 2 w)))
 (= ?x514 ?x9000))))
 ))
 (let (($x9804 (forall ((n (_ BitVec 6)) )(let ((?x154 (sc_s 1)))
 (let ((?x1435 (bvadd (_ bv63 6) ?x154)))
 (let (($x3626 (bvsle ?x1435 n)))
 (or $x3626 (= (stack_s x_TIMESTAMP 2 n) (stack_s x_TIMESTAMP 1 n)))))))
 ))
 (let (($x189 (exc_halt_s 1)))
 (let (($x4891 (= $x189 (or $x57 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x72))) (_ bv0 1)))))))
 (let (($x7649 (forall ((w (_ BitVec 256)) )(let ((?x6670 (storage_s x_TIMESTAMP 0 w)))
 (let ((?x9000 (storage_s x_TIMESTAMP 1 w)))
 (= ?x9000 ?x6670))))
 ))
 (let (($x8038 (forall ((n (_ BitVec 6)) )(let ((?x72 (sc_s 0)))
 (let (($x7832 (bvsle ?x72 n)))
 (or (= (stack_s x_TIMESTAMP 1 n) (stack_s x_TIMESTAMP 0 n)) $x7832))))
 ))
 (let ((?x154 (sc_s 1)))
 (let (($x3745 (= ?x154 (bvadd (_ bv1 6) ?x72))))
 (let (($x523 (forall ((w (_ BitVec 256)) )(let ((?x6670 (storage_s x_TIMESTAMP 0 w)))
 (= ?x6670 (_ bv0 256))))
 ))
 (let (($x7009 (= ?x7117 0)))
 (let (($x5252 (not $x57)))
 (let (($x136 (= ?x72 (_ bv0 6))))
 (and $x136 $x5252 $x7009 $x523 (= (stack_s x_TIMESTAMP 1 ?x72) x_TIMESTAMP) (= (used_gas_s x_TIMESTAMP 1) (+ 2 ?x7117)) $x3745 $x8038 $x7649 $x4891 (= (used_gas_s x_TIMESTAMP 2) (+ 2 (used_gas_s x_TIMESTAMP 1))) (= ?x218 (bvadd (_ bv63 6) ?x154)) $x9804 $x7368 $x1521 $x73 $x2018 $x58 $x2943 $x4319 (not (and $x8460 $x6597 $x9812 $x5999)))))))))))))))))))))))))))))))))
 )
(check-sat)
(get-proof)
