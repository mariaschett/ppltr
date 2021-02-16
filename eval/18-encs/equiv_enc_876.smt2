; DUP1 PUSH cw_3 SWAP1 POP => PUSH cw_3
(set-option :produce-proofs true)
; 
(set-info :status unknown)
(declare-fun storage_t ((_ BitVec 256) (_ BitVec 256) Int (_ BitVec 256)) (_ BitVec 256))
(declare-fun w_3 () (_ BitVec 256))
(declare-fun storage_s ((_ BitVec 256) (_ BitVec 256) Int (_ BitVec 256)) (_ BitVec 256))
(declare-fun exc_halt_t (Int) Bool)
(declare-fun exc_halt_s (Int) Bool)
(declare-fun sc_t (Int) (_ BitVec 6))
(declare-fun stack_t ((_ BitVec 256) (_ BitVec 256) Int (_ BitVec 6)) (_ BitVec 256))
(declare-fun stack_s ((_ BitVec 256) (_ BitVec 256) Int (_ BitVec 6)) (_ BitVec 256))
(declare-fun sc_s (Int) (_ BitVec 6))
(declare-fun used_gas_t ((_ BitVec 256) (_ BitVec 256) Int) Int)
(declare-fun used_gas_s ((_ BitVec 256) (_ BitVec 256) Int) Int)
(assert
 (exists ((x_0 (_ BitVec 256)) )(let (($x8437 (forall ((w (_ BitVec 256)) )(let ((?x2619 (storage_t x_0 w_3 1 w)))
 (let ((?x5455 (storage_s x_0 w_3 4 w)))
 (= ?x5455 ?x2619))))
 ))
 (let (($x4852 (exc_halt_t 1)))
 (let (($x9175 (exc_halt_s 4)))
 (let (($x6532 (= $x9175 $x4852)))
 (let (($x7482 (forall ((n (_ BitVec 6)) )(let ((?x9666 (sc_t 1)))
 (let (($x8463 (bvsle ?x9666 n)))
 (let ((?x3957 (stack_t x_0 w_3 1 n)))
 (let ((?x1826 (stack_s x_0 w_3 4 n)))
 (let (($x8071 (= ?x1826 ?x3957)))
 (or $x8071 $x8463)))))))
 ))
 (let ((?x9666 (sc_t 1)))
 (let ((?x9433 (sc_s 4)))
 (let (($x3801 (= ?x9433 ?x9666)))
 (let ((?x1552 (used_gas_t x_0 w_3 0)))
 (let ((?x8090 (used_gas_s x_0 w_3 0)))
 (let (($x3867 (= ?x8090 ?x1552)))
 (let (($x11995 (forall ((w (_ BitVec 256)) )(let ((?x5722 (storage_t x_0 w_3 0 w)))
 (let ((?x186 (storage_s x_0 w_3 0 w)))
 (= ?x186 ?x5722))))
 ))
 (let (($x56 (exc_halt_t 0)))
 (let (($x57 (exc_halt_s 0)))
 (let (($x58 (= $x57 $x56)))
 (let (($x3982 (forall ((n (_ BitVec 6)) )(let ((?x1820 (stack_t x_0 w_3 0 n)))
 (let ((?x11174 (stack_s x_0 w_3 0 n)))
 (let (($x4976 (= ?x11174 ?x1820)))
 (let ((?x63 (sc_t 0)))
 (let (($x4370 (bvsle ?x63 n)))
 (or $x4370 $x4976)))))))
 ))
 (let ((?x63 (sc_t 0)))
 (let ((?x72 (sc_s 0)))
 (let (($x73 (= ?x72 ?x63)))
 (let (($x71 (= $x4852 (or $x56 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x63))) (_ bv0 1)))))))
 (let (($x2737 (forall ((w (_ BitVec 256)) )(let ((?x5722 (storage_t x_0 w_3 0 w)))
 (let ((?x2619 (storage_t x_0 w_3 1 w)))
 (= ?x2619 ?x5722))))
 ))
 (let (($x3014 (forall ((n (_ BitVec 6)) )(let ((?x63 (sc_t 0)))
 (let (($x4370 (bvsle ?x63 n)))
 (or (= (stack_t x_0 w_3 1 n) (stack_t x_0 w_3 0 n)) $x4370))))
 ))
 (let (($x11820 (= ?x9666 (bvadd (_ bv1 6) ?x63))))
 (let (($x8774 (= $x9175 (or (exc_halt_s 3) (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_s 3))))))))
 (let (($x4215 (forall ((w (_ BitVec 256)) )(let ((?x10877 (storage_s x_0 w_3 3 w)))
 (let ((?x5455 (storage_s x_0 w_3 4 w)))
 (= ?x5455 ?x10877))))
 ))
 (let (($x11856 (forall ((n (_ BitVec 6)) )(let ((?x3851 (sc_s 3)))
 (let ((?x9751 (bvadd (_ bv63 6) ?x3851)))
 (let (($x662 (bvsle ?x9751 n)))
 (or $x662 (= (stack_s x_0 w_3 4 n) (stack_s x_0 w_3 3 n)))))))
 ))
 (let ((?x3851 (sc_s 3)))
 (let ((?x9751 (bvadd (_ bv63 6) ?x3851)))
 (let (($x2264 (= ?x9433 ?x9751)))
 (let (($x4569 (= (used_gas_s x_0 w_3 4) (+ 2 (used_gas_s x_0 w_3 3)))))
 (let (($x8103 (exc_halt_s 3)))
 (let (($x9505 (= $x8103 (or (exc_halt_s 2) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 2))))))))
 (let (($x8544 (forall ((w (_ BitVec 256)) )(let ((?x1807 (storage_s x_0 w_3 2 w)))
 (let ((?x10877 (storage_s x_0 w_3 3 w)))
 (= ?x10877 ?x1807))))
 ))
 (let (($x8704 (forall ((n (_ BitVec 6)) )(let ((?x2272 (sc_s 2)))
 (let ((?x2268 (bvadd (_ bv62 6) ?x2272)))
 (let (($x1979 (bvsle ?x2268 n)))
 (or $x1979 (= (stack_s x_0 w_3 3 n) (stack_s x_0 w_3 2 n)))))))
 ))
 (let ((?x2272 (sc_s 2)))
 (let (($x863 (= ?x3851 ?x2272)))
 (let ((?x1389 (used_gas_s x_0 w_3 3)))
 (let (($x7322 (= (stack_s x_0 w_3 3 (bvadd (_ bv62 6) ?x3851)) (stack_s x_0 w_3 2 (bvadd (_ bv63 6) ?x2272)))))
 (let (($x532 (= (stack_s x_0 w_3 3 ?x9751) (stack_s x_0 w_3 2 (bvadd (_ bv62 6) ?x2272)))))
 (let (($x2355 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 1)))) (_ bv0 1)))))
 (let (($x8780 (exc_halt_s 1)))
 (let (($x10052 (exc_halt_s 2)))
 (let (($x11443 (= $x10052 (or $x8780 $x2355))))
 (let (($x2491 (forall ((w (_ BitVec 256)) )(let ((?x3780 (storage_s x_0 w_3 1 w)))
 (let ((?x1807 (storage_s x_0 w_3 2 w)))
 (= ?x1807 ?x3780))))
 ))
 (let (($x8496 (forall ((n (_ BitVec 6)) )(let ((?x154 (sc_s 1)))
 (let (($x10420 (bvsle ?x154 n)))
 (or (= (stack_s x_0 w_3 2 n) (stack_s x_0 w_3 1 n)) $x10420))))
 ))
 (let (($x6608 (= ?x2272 (bvadd (_ bv1 6) (sc_s 1)))))
 (let ((?x1938 (used_gas_s x_0 w_3 2)))
 (let (($x6025 (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) ?x72)))))
 (let (($x3534 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x72))) (_ bv0 1)))))
 (let (($x11599 (= $x8780 (or $x57 $x3534 $x6025))))
 (let (($x5758 (forall ((w (_ BitVec 256)) )(let ((?x186 (storage_s x_0 w_3 0 w)))
 (let ((?x3780 (storage_s x_0 w_3 1 w)))
 (= ?x3780 ?x186))))
 ))
 (let (($x1832 (forall ((n (_ BitVec 6)) )(let ((?x72 (sc_s 0)))
 (let ((?x3268 (bvadd (_ bv63 6) ?x72)))
 (let (($x3916 (bvsle ?x3268 n)))
 (or (= (stack_s x_0 w_3 1 n) (stack_s x_0 w_3 0 n)) $x3916)))))
 ))
 (let ((?x154 (sc_s 1)))
 (let (($x8172 (= ?x154 (bvadd (_ bv1 6) ?x72))))
 (let ((?x3268 (bvadd (_ bv63 6) ?x72)))
 (let ((?x8447 (stack_s x_0 w_3 0 ?x3268)))
 (let (($x11360 (forall ((w (_ BitVec 256)) )(let ((?x186 (storage_s x_0 w_3 0 w)))
 (= ?x186 (_ bv0 256))))
 ))
 (let (($x7664 (= ?x8090 0)))
 (let (($x2248 (not $x57)))
 (let (($x11739 (= (stack_s x_0 w_3 0 (_ bv0 6)) x_0)))
 (let (($x7461 (= ?x72 (_ bv1 6))))
 (and $x7461 $x11739 $x2248 $x7664 $x11360 (= (stack_s x_0 w_3 1 (bvadd (_ bv63 6) ?x154)) ?x8447) (= (stack_s x_0 w_3 1 ?x3268) ?x8447) (= (used_gas_s x_0 w_3 1) (+ 3 ?x8090)) $x8172 $x1832 $x5758 $x11599 (= (stack_s x_0 w_3 2 ?x154) w_3) (= ?x1938 (+ 3 (used_gas_s x_0 w_3 1))) $x6608 $x8496 $x2491 $x11443 $x532 $x7322 (= ?x1389 (+ 3 ?x1938)) $x863 $x8704 $x8544 $x9505 $x4569 $x2264 $x11856 $x4215 $x8774 (= (stack_t x_0 w_3 1 ?x63) w_3) (= (used_gas_t x_0 w_3 1) (+ 3 ?x1552)) $x11820 $x3014 $x2737 $x71 $x73 $x3982 $x58 $x11995 $x3867 (not (and $x3801 $x7482 $x6532 $x8437)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
 )
(check-sat)
(get-proof)