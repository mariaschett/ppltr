; PUSH 0x00 DUP3 ISZERO ISZERO EQ => DUP2 ISZERO
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
 (exists ((x_0 (_ BitVec 256)) (x_1 (_ BitVec 256)) )(let (($x8358 (forall ((w (_ BitVec 256)) )(let ((?x8904 (storage_t x_0 x_1 2 w)))
 (let ((?x8519 (storage_s x_0 x_1 5 w)))
 (= ?x8519 ?x8904))))
 ))
 (let (($x903 (exc_halt_t 2)))
 (let (($x3979 (exc_halt_s 5)))
 (let (($x2918 (= $x3979 $x903)))
 (let (($x6043 (forall ((n (_ BitVec 6)) )(let ((?x7710 (stack_t x_0 x_1 2 n)))
 (let ((?x8688 (stack_s x_0 x_1 5 n)))
 (let (($x6646 (= ?x8688 ?x7710)))
 (let ((?x4056 (sc_t 2)))
 (let (($x4538 (bvsle ?x4056 n)))
 (or $x4538 $x6646)))))))
 ))
 (let ((?x4056 (sc_t 2)))
 (let ((?x805 (sc_s 5)))
 (let (($x6736 (= ?x805 ?x4056)))
 (let ((?x920 (used_gas_t x_0 x_1 0)))
 (let ((?x2422 (used_gas_s x_0 x_1 0)))
 (let (($x979 (= ?x2422 ?x920)))
 (let (($x7931 (forall ((w (_ BitVec 256)) )(let ((?x4140 (storage_t x_0 x_1 0 w)))
 (let ((?x3927 (storage_s x_0 x_1 0 w)))
 (= ?x3927 ?x4140))))
 ))
 (let (($x56 (exc_halt_t 0)))
 (let (($x57 (exc_halt_s 0)))
 (let (($x58 (= $x57 $x56)))
 (let (($x2630 (forall ((n (_ BitVec 6)) )(let ((?x5624 (stack_t x_0 x_1 0 n)))
 (let ((?x9019 (stack_s x_0 x_1 0 n)))
 (let (($x5794 (= ?x9019 ?x5624)))
 (let ((?x63 (sc_t 0)))
 (let (($x1361 (bvsle ?x63 n)))
 (or $x1361 $x5794)))))))
 ))
 (let ((?x63 (sc_t 0)))
 (let ((?x72 (sc_s 0)))
 (let (($x73 (= ?x72 ?x63)))
 (let (($x2492 (= $x903 (or (exc_halt_t 1) (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_t 1))))))))
 (let (($x7909 (forall ((w (_ BitVec 256)) )(let ((?x8682 (storage_t x_0 x_1 1 w)))
 (let ((?x8904 (storage_t x_0 x_1 2 w)))
 (= ?x8904 ?x8682))))
 ))
 (let (($x9505 (forall ((n (_ BitVec 6)) )(let ((?x4023 (sc_t 1)))
 (let ((?x1843 (bvadd (_ bv63 6) ?x4023)))
 (let (($x10022 (bvsle ?x1843 n)))
 (or (= (stack_t x_0 x_1 2 n) (stack_t x_0 x_1 1 n)) $x10022)))))
 ))
 (let ((?x4023 (sc_t 1)))
 (let (($x6083 (= ?x4056 ?x4023)))
 (let (($x3424 (= (used_gas_t x_0 x_1 2) (+ 3 (used_gas_t x_0 x_1 1)))))
 (let (($x4816 (= (stack_t x_0 x_1 2 (bvadd (_ bv63 6) ?x4056)) (ite (= (stack_t x_0 x_1 1 (bvadd (_ bv63 6) ?x4023)) (_ bv0 256)) (_ bv1 256) (_ bv0 256)))))
 (let (($x6222 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x63))) (_ bv0 1)))))
 (let (($x1920 (exc_halt_t 1)))
 (let (($x5827 (forall ((w (_ BitVec 256)) )(let ((?x4140 (storage_t x_0 x_1 0 w)))
 (let ((?x8682 (storage_t x_0 x_1 1 w)))
 (= ?x8682 ?x4140))))
 ))
 (let (($x7836 (forall ((n (_ BitVec 6)) )(or (= (stack_t x_0 x_1 1 n) (stack_t x_0 x_1 0 n)) (bvsle (bvadd (_ bv62 6) (sc_t 0)) n)))
 ))
 (let (($x2669 (= ?x4023 (bvadd (_ bv1 6) ?x63))))
 (let (($x1341 (= (stack_t x_0 x_1 1 (bvadd (_ bv63 6) ?x63)) (stack_t x_0 x_1 0 (bvadd (_ bv63 6) ?x63)))))
 (let ((?x8041 (bvadd (_ bv62 6) ?x63)))
 (let ((?x7005 (stack_t x_0 x_1 0 ?x8041)))
 (let (($x2089 (= $x3979 (or (exc_halt_s 4) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 4))))))))
 (let (($x4917 (forall ((w (_ BitVec 256)) )(let ((?x9509 (storage_s x_0 x_1 4 w)))
 (let ((?x8519 (storage_s x_0 x_1 5 w)))
 (= ?x8519 ?x9509))))
 ))
 (let (($x828 (forall ((n (_ BitVec 6)) )(or (= (stack_s x_0 x_1 5 n) (stack_s x_0 x_1 4 n)) (bvsle (bvadd (_ bv62 6) (sc_s 4)) n)))
 ))
 (let ((?x4305 (sc_s 4)))
 (let ((?x3595 (bvadd (_ bv63 6) ?x4305)))
 (let (($x2634 (= ?x805 ?x3595)))
 (let (($x1966 (= (used_gas_s x_0 x_1 5) (+ 3 (used_gas_s x_0 x_1 4)))))
 (let ((?x1487 (stack_s x_0 x_1 4 ?x3595)))
 (let (($x6511 (= (stack_s x_0 x_1 5 (bvadd (_ bv63 6) ?x805)) (ite (= ?x1487 (stack_s x_0 x_1 4 (bvadd (_ bv62 6) ?x4305))) (_ bv1 256) (_ bv0 256)))))
 (let (($x64 (exc_halt_s 4)))
 (let (($x4521 (= $x64 (or (exc_halt_s 3) (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_s 3))))))))
 (let (($x9162 (forall ((w (_ BitVec 256)) )(let ((?x7278 (storage_s x_0 x_1 3 w)))
 (let ((?x9509 (storage_s x_0 x_1 4 w)))
 (= ?x9509 ?x7278))))
 ))
 (let (($x9197 (forall ((n (_ BitVec 6)) )(or (= (stack_s x_0 x_1 4 n) (stack_s x_0 x_1 3 n)) (bvsle (bvadd (_ bv63 6) (sc_s 3)) n)))
 ))
 (let ((?x275 (sc_s 3)))
 (let (($x936 (= ?x4305 ?x275)))
 (let ((?x3025 (used_gas_s x_0 x_1 4)))
 (let (($x9209 (= ?x1487 (ite (= (stack_s x_0 x_1 3 (bvadd (_ bv63 6) ?x275)) (_ bv0 256)) (_ bv1 256) (_ bv0 256)))))
 (let (($x292 (exc_halt_s 3)))
 (let (($x906 (= $x292 (or (exc_halt_s 2) (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_s 2))))))))
 (let (($x8237 (forall ((w (_ BitVec 256)) )(let ((?x7695 (storage_s x_0 x_1 2 w)))
 (let ((?x7278 (storage_s x_0 x_1 3 w)))
 (= ?x7278 ?x7695))))
 ))
 (let (($x6552 (forall ((n (_ BitVec 6)) )(or (bvsle (bvadd (_ bv63 6) (sc_s 2)) n) (= (stack_s x_0 x_1 3 n) (stack_s x_0 x_1 2 n))))
 ))
 (let ((?x6197 (used_gas_s x_0 x_1 3)))
 (let ((?x3691 (ite (= (stack_s x_0 x_1 2 (bvadd (_ bv63 6) (sc_s 2))) (_ bv0 256)) (_ bv1 256) (_ bv0 256))))
 (let ((?x9192 (bvadd (_ bv63 6) ?x275)))
 (let ((?x3920 (stack_s x_0 x_1 3 ?x9192)))
 (let (($x11391 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 1)))) (_ bv0 1)))))
 (let (($x6890 (not (bvsle (_ bv0 6) (bvadd (_ bv61 6) (sc_s 1))))))
 (let (($x189 (exc_halt_s 1)))
 (let (($x247 (exc_halt_s 2)))
 (let (($x1709 (forall ((w (_ BitVec 256)) )(let ((?x5860 (storage_s x_0 x_1 1 w)))
 (let ((?x7695 (storage_s x_0 x_1 2 w)))
 (= ?x7695 ?x5860))))
 ))
 (let (($x9665 (forall ((n (_ BitVec 6)) )(let ((?x154 (sc_s 1)))
 (let ((?x8011 (bvadd (_ bv61 6) ?x154)))
 (let (($x2449 (bvsle ?x8011 n)))
 (or (= (stack_s x_0 x_1 2 n) (stack_s x_0 x_1 1 n)) $x2449)))))
 ))
 (let ((?x218 (sc_s 2)))
 (let (($x11361 (= ?x218 (bvadd (_ bv1 6) (sc_s 1)))))
 (let ((?x3621 (used_gas_s x_0 x_1 2)))
 (let (($x8016 (= (stack_s x_0 x_1 2 (bvadd (_ bv63 6) (sc_s 1))) (stack_s x_0 x_1 1 (bvadd (_ bv63 6) (sc_s 1))))))
 (let (($x9547 (= (stack_s x_0 x_1 2 (bvadd (_ bv62 6) (sc_s 1))) (stack_s x_0 x_1 1 (bvadd (_ bv62 6) (sc_s 1))))))
 (let ((?x154 (sc_s 1)))
 (let ((?x8011 (bvadd (_ bv61 6) ?x154)))
 (let ((?x8592 (stack_s x_0 x_1 1 ?x8011)))
 (let (($x8948 (= $x189 (or $x57 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x72))) (_ bv0 1)))))))
 (let (($x6993 (forall ((w (_ BitVec 256)) )(let ((?x3927 (storage_s x_0 x_1 0 w)))
 (let ((?x5860 (storage_s x_0 x_1 1 w)))
 (= ?x5860 ?x3927))))
 ))
 (let (($x6850 (forall ((n (_ BitVec 6)) )(let ((?x72 (sc_s 0)))
 (let (($x11379 (bvsle ?x72 n)))
 (or (= (stack_s x_0 x_1 1 n) (stack_s x_0 x_1 0 n)) $x11379))))
 ))
 (let (($x1901 (= ?x154 (bvadd (_ bv1 6) ?x72))))
 (let (($x3635 (forall ((w (_ BitVec 256)) )(let ((?x3927 (storage_s x_0 x_1 0 w)))
 (= ?x3927 (_ bv0 256))))
 ))
 (let (($x1491 (= ?x2422 0)))
 (let (($x11411 (not $x57)))
 (let (($x8219 (= (stack_s x_0 x_1 0 (_ bv1 6)) x_1)))
 (let (($x8631 (= (stack_s x_0 x_1 0 (_ bv0 6)) x_0)))
 (let (($x3907 (= ?x72 (_ bv2 6))))
 (and $x3907 $x8631 $x8219 $x11411 $x1491 $x3635 (= (stack_s x_0 x_1 1 ?x72) (_ bv0 256)) (= (used_gas_s x_0 x_1 1) (+ 3 ?x2422)) $x1901 $x6850 $x6993 $x8948 (= (stack_s x_0 x_1 2 (bvadd (_ bv63 6) ?x218)) ?x8592) (= (stack_s x_0 x_1 2 ?x8011) ?x8592) $x9547 $x8016 (= ?x3621 (+ 3 (used_gas_s x_0 x_1 1))) $x11361 $x9665 $x1709 (= $x247 (or $x189 $x6890 $x11391)) (= ?x3920 ?x3691) (= ?x6197 (+ 3 ?x3621)) (= ?x275 ?x218) $x6552 $x8237 $x906 $x9209 (= ?x3025 (+ 3 ?x6197)) $x936 $x9197 $x9162 $x4521 $x6511 $x1966 $x2634 $x828 $x4917 $x2089 (= (stack_t x_0 x_1 1 (bvadd (_ bv63 6) ?x4023)) ?x7005) (= (stack_t x_0 x_1 1 ?x8041) ?x7005) $x1341 (= (used_gas_t x_0 x_1 1) (+ 3 ?x920)) $x2669 $x7836 $x5827 (= $x1920 (or $x56 (not (bvsle (_ bv0 6) ?x8041)) $x6222)) $x4816 $x3424 $x6083 $x9505 $x7909 $x2492 $x73 $x2630 $x58 $x7931 $x979 (not (and $x6736 $x6043 $x2918 $x8358)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
 )
(check-sat)
(get-proof)
