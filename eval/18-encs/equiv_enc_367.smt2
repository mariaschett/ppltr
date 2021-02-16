; PUSH 0x00 NOT AND => 
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
 (exists ((x_0 (_ BitVec 256)) )(let (($x6068 (forall ((w (_ BitVec 256)) )(let ((?x7943 (storage_t x_0 0 w)))
 (let ((?x3861 (storage_s x_0 3 w)))
 (= ?x3861 ?x7943))))
 ))
 (let (($x56 (exc_halt_t 0)))
 (let (($x292 (exc_halt_s 3)))
 (let (($x11387 (= $x292 $x56)))
 (let (($x1570 (forall ((n (_ BitVec 6)) )(let ((?x5707 (stack_t x_0 0 n)))
 (let ((?x10036 (stack_s x_0 3 n)))
 (let (($x8337 (= ?x10036 ?x5707)))
 (let ((?x63 (sc_t 0)))
 (let (($x4866 (bvsle ?x63 n)))
 (or $x4866 $x8337)))))))
 ))
 (let ((?x63 (sc_t 0)))
 (let ((?x275 (sc_s 3)))
 (let (($x2083 (= ?x275 ?x63)))
 (let ((?x7228 (used_gas_s x_0 0)))
 (let (($x9022 (= ?x7228 (used_gas_t x_0 0))))
 (let (($x10706 (forall ((w (_ BitVec 256)) )(let ((?x7943 (storage_t x_0 0 w)))
 (let ((?x4523 (storage_s x_0 0 w)))
 (= ?x4523 ?x7943))))
 ))
 (let (($x57 (exc_halt_s 0)))
 (let (($x58 (= $x57 $x56)))
 (let (($x5000 (forall ((n (_ BitVec 6)) )(let ((?x5707 (stack_t x_0 0 n)))
 (let ((?x10058 (stack_s x_0 0 n)))
 (let (($x6448 (= ?x10058 ?x5707)))
 (let ((?x63 (sc_t 0)))
 (let (($x4866 (bvsle ?x63 n)))
 (or $x4866 $x6448)))))))
 ))
 (let ((?x72 (sc_s 0)))
 (let (($x73 (= ?x72 ?x63)))
 (let (($x8083 (= $x292 (or (exc_halt_s 2) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 2))))))))
 (let (($x10300 (forall ((w (_ BitVec 256)) )(let ((?x9685 (storage_s x_0 2 w)))
 (let ((?x3861 (storage_s x_0 3 w)))
 (= ?x3861 ?x9685))))
 ))
 (let (($x3790 (forall ((n (_ BitVec 6)) )(let ((?x218 (sc_s 2)))
 (let ((?x3067 (bvadd (_ bv62 6) ?x218)))
 (let (($x5606 (bvsle ?x3067 n)))
 (or (= (stack_s x_0 3 n) (stack_s x_0 2 n)) $x5606)))))
 ))
 (let ((?x218 (sc_s 2)))
 (let ((?x5789 (bvadd (_ bv63 6) ?x218)))
 (let (($x4278 (= ?x275 ?x5789)))
 (let ((?x1795 (bvor (bvnot (stack_s x_0 2 ?x5789)) (bvnot (stack_s x_0 2 (bvadd (_ bv62 6) ?x218))))))
 (let (($x247 (exc_halt_s 2)))
 (let (($x4452 (= $x247 (or (exc_halt_s 1) (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_s 1))))))))
 (let (($x2017 (forall ((w (_ BitVec 256)) )(let ((?x9538 (storage_s x_0 1 w)))
 (let ((?x9685 (storage_s x_0 2 w)))
 (= ?x9685 ?x9538))))
 ))
 (let (($x11013 (forall ((n (_ BitVec 6)) )(let ((?x154 (sc_s 1)))
 (let ((?x9355 (bvadd (_ bv63 6) ?x154)))
 (let (($x11667 (bvsle ?x9355 n)))
 (or (= (stack_s x_0 2 n) (stack_s x_0 1 n)) $x11667)))))
 ))
 (let ((?x154 (sc_s 1)))
 (let (($x2182 (= ?x218 ?x154)))
 (let ((?x7478 (stack_s x_0 2 ?x5789)))
 (let (($x189 (exc_halt_s 1)))
 (let (($x9787 (= $x189 (or $x57 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x72))) (_ bv0 1)))))))
 (let (($x9846 (forall ((w (_ BitVec 256)) )(let ((?x4523 (storage_s x_0 0 w)))
 (let ((?x9538 (storage_s x_0 1 w)))
 (= ?x9538 ?x4523))))
 ))
 (let (($x1241 (forall ((n (_ BitVec 6)) )(let ((?x72 (sc_s 0)))
 (let (($x3549 (bvsle ?x72 n)))
 (or $x3549 (= (stack_s x_0 1 n) (stack_s x_0 0 n))))))
 ))
 (let (($x10046 (= ?x154 (bvadd (_ bv1 6) ?x72))))
 (let (($x11601 (forall ((w (_ BitVec 256)) )(let ((?x4523 (storage_s x_0 0 w)))
 (= ?x4523 (_ bv0 256))))
 ))
 (let (($x645 (= ?x7228 0)))
 (let (($x1248 (not $x57)))
 (let (($x8290 (= (stack_s x_0 0 (_ bv0 6)) x_0)))
 (let (($x8685 (= ?x72 (_ bv1 6))))
 (and $x8685 $x8290 $x1248 $x645 $x11601 (= (stack_s x_0 1 ?x72) (_ bv0 256)) (= (used_gas_s x_0 1) (+ 3 ?x7228)) $x10046 $x1241 $x9846 $x9787 (= ?x7478 (bvnot (stack_s x_0 1 (bvadd (_ bv63 6) ?x154)))) (= (used_gas_s x_0 2) (+ 3 (used_gas_s x_0 1))) $x2182 $x11013 $x2017 $x4452 (= (stack_s x_0 3 (bvadd (_ bv63 6) ?x275)) (bvnot ?x1795)) (= (used_gas_s x_0 3) (+ 3 (used_gas_s x_0 2))) $x4278 $x3790 $x10300 $x8083 $x73 $x5000 $x58 $x10706 $x9022 (not (and $x2083 $x1570 $x11387 $x6068))))))))))))))))))))))))))))))))))))))))))))
 )
(check-sat)
(get-proof)