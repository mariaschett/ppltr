; DUP1 MLOAD SWAP2 LT POP POP => POP POP
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
(declare-fun f_MLOAD ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256)) (_ BitVec 256))
(assert
 (exists ((x_0 (_ BitVec 256)) (x_1 (_ BitVec 256)) (x_MLOAD_0 (_ BitVec 256)) )(let (($x1938 (forall ((w (_ BitVec 256)) )(let ((?x1090 (storage_t x_0 x_1 x_MLOAD_0 2 w)))
 (let ((?x1200 (storage_s x_0 x_1 x_MLOAD_0 6 w)))
 (= ?x1200 ?x1090))))
 ))
 (let (($x903 (exc_halt_t 2)))
 (let (($x772 (exc_halt_s 6)))
 (let (($x7260 (= $x772 $x903)))
 (let (($x4646 (forall ((n (_ BitVec 6)) )(let ((?x4056 (sc_t 2)))
 (let (($x8328 (bvsle ?x4056 n)))
 (let ((?x1958 (stack_t x_0 x_1 x_MLOAD_0 2 n)))
 (let ((?x1503 (stack_s x_0 x_1 x_MLOAD_0 6 n)))
 (let (($x1715 (= ?x1503 ?x1958)))
 (or $x1715 $x8328)))))))
 ))
 (let ((?x4056 (sc_t 2)))
 (let ((?x926 (sc_s 6)))
 (let (($x8054 (= ?x926 ?x4056)))
 (let ((?x9007 (used_gas_t x_0 x_1 x_MLOAD_0 0)))
 (let ((?x2883 (used_gas_s x_0 x_1 x_MLOAD_0 0)))
 (let (($x7691 (= ?x2883 ?x9007)))
 (let (($x8879 (forall ((w (_ BitVec 256)) )(let ((?x1908 (storage_t x_0 x_1 x_MLOAD_0 0 w)))
 (let ((?x1764 (storage_s x_0 x_1 x_MLOAD_0 0 w)))
 (= ?x1764 ?x1908))))
 ))
 (let (($x56 (exc_halt_t 0)))
 (let (($x57 (exc_halt_s 0)))
 (let (($x58 (= $x57 $x56)))
 (let (($x5582 (forall ((n (_ BitVec 6)) )(let ((?x63 (sc_t 0)))
 (let (($x2733 (bvsle ?x63 n)))
 (let ((?x8936 (stack_t x_0 x_1 x_MLOAD_0 0 n)))
 (let ((?x1505 (stack_s x_0 x_1 x_MLOAD_0 0 n)))
 (let (($x1368 (= ?x1505 ?x8936)))
 (or $x1368 $x2733)))))))
 ))
 (let ((?x63 (sc_t 0)))
 (let ((?x72 (sc_s 0)))
 (let (($x73 (= ?x72 ?x63)))
 (let (($x2625 (= $x903 (or (exc_halt_t 1) (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_t 1))))))))
 (let (($x2881 (forall ((w (_ BitVec 256)) )(let ((?x7754 (storage_t x_0 x_1 x_MLOAD_0 1 w)))
 (let ((?x1090 (storage_t x_0 x_1 x_MLOAD_0 2 w)))
 (= ?x1090 ?x7754))))
 ))
 (let (($x5750 (forall ((n (_ BitVec 6)) )(let ((?x9560 (stack_t x_0 x_1 x_MLOAD_0 1 n)))
 (let ((?x1958 (stack_t x_0 x_1 x_MLOAD_0 2 n)))
 (let (($x9367 (= ?x1958 ?x9560)))
 (let ((?x4023 (sc_t 1)))
 (let ((?x6958 (bvadd (_ bv63 6) ?x4023)))
 (let (($x2645 (bvsle ?x6958 n)))
 (or $x2645 $x9367))))))))
 ))
 (let ((?x4023 (sc_t 1)))
 (let ((?x6958 (bvadd (_ bv63 6) ?x4023)))
 (let (($x7803 (= ?x4056 ?x6958)))
 (let ((?x6517 (used_gas_t x_0 x_1 x_MLOAD_0 2)))
 (let (($x1920 (exc_halt_t 1)))
 (let (($x9111 (= $x1920 (or $x56 (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) ?x63)))))))
 (let (($x1948 (forall ((w (_ BitVec 256)) )(let ((?x1908 (storage_t x_0 x_1 x_MLOAD_0 0 w)))
 (let ((?x7754 (storage_t x_0 x_1 x_MLOAD_0 1 w)))
 (= ?x7754 ?x1908))))
 ))
 (let (($x7187 (forall ((n (_ BitVec 6)) )(let ((?x63 (sc_t 0)))
 (let ((?x2567 (bvadd (_ bv63 6) ?x63)))
 (let (($x10009 (bvsle ?x2567 n)))
 (let ((?x8936 (stack_t x_0 x_1 x_MLOAD_0 0 n)))
 (let ((?x9560 (stack_t x_0 x_1 x_MLOAD_0 1 n)))
 (let (($x2909 (= ?x9560 ?x8936)))
 (or $x2909 $x10009))))))))
 ))
 (let ((?x2567 (bvadd (_ bv63 6) ?x63)))
 (let (($x7219 (= ?x4023 ?x2567)))
 (let (($x3773 (= $x772 (or (exc_halt_s 5) (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_s 5))))))))
 (let (($x4361 (forall ((w (_ BitVec 256)) )(let ((?x6638 (storage_s x_0 x_1 x_MLOAD_0 5 w)))
 (let ((?x1200 (storage_s x_0 x_1 x_MLOAD_0 6 w)))
 (= ?x1200 ?x6638))))
 ))
 (let (($x5046 (forall ((n (_ BitVec 6)) )(let ((?x805 (sc_s 5)))
 (let ((?x8839 (bvadd (_ bv63 6) ?x805)))
 (let (($x7467 (bvsle ?x8839 n)))
 (let ((?x4860 (stack_s x_0 x_1 x_MLOAD_0 5 n)))
 (let ((?x1503 (stack_s x_0 x_1 x_MLOAD_0 6 n)))
 (or (= ?x1503 ?x4860) $x7467)))))))
 ))
 (let ((?x805 (sc_s 5)))
 (let ((?x8839 (bvadd (_ bv63 6) ?x805)))
 (let (($x3408 (= ?x926 ?x8839)))
 (let (($x4540 (= (used_gas_s x_0 x_1 x_MLOAD_0 6) (+ 2 (used_gas_s x_0 x_1 x_MLOAD_0 5)))))
 (let (($x3979 (exc_halt_s 5)))
 (let (($x7980 (= $x3979 (or (exc_halt_s 4) (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_s 4))))))))
 (let (($x963 (forall ((w (_ BitVec 256)) )(let ((?x1201 (storage_s x_0 x_1 x_MLOAD_0 4 w)))
 (let ((?x6638 (storage_s x_0 x_1 x_MLOAD_0 5 w)))
 (= ?x6638 ?x1201))))
 ))
 (let (($x485 (forall ((n (_ BitVec 6)) )(let ((?x2064 (stack_s x_0 x_1 x_MLOAD_0 4 n)))
 (let ((?x4860 (stack_s x_0 x_1 x_MLOAD_0 5 n)))
 (let ((?x4305 (sc_s 4)))
 (let ((?x10083 (bvadd (_ bv63 6) ?x4305)))
 (let (($x6463 (bvsle ?x10083 n)))
 (or $x6463 (= ?x4860 ?x2064))))))))
 ))
 (let ((?x4305 (sc_s 4)))
 (let ((?x10083 (bvadd (_ bv63 6) ?x4305)))
 (let (($x6538 (= ?x805 ?x10083)))
 (let ((?x297 (used_gas_s x_0 x_1 x_MLOAD_0 5)))
 (let (($x64 (exc_halt_s 4)))
 (let (($x1351 (= $x64 (or (exc_halt_s 3) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 3))))))))
 (let (($x1441 (forall ((w (_ BitVec 256)) )(let ((?x1149 (storage_s x_0 x_1 x_MLOAD_0 3 w)))
 (let ((?x1201 (storage_s x_0 x_1 x_MLOAD_0 4 w)))
 (= ?x1201 ?x1149))))
 ))
 (let (($x9981 (forall ((n (_ BitVec 6)) )(let ((?x1685 (stack_s x_0 x_1 x_MLOAD_0 3 n)))
 (let ((?x2064 (stack_s x_0 x_1 x_MLOAD_0 4 n)))
 (let (($x10994 (= ?x2064 ?x1685)))
 (or (bvsle (bvadd (_ bv62 6) (sc_s 3)) n) $x10994)))))
 ))
 (let ((?x275 (sc_s 3)))
 (let ((?x2861 (bvadd (_ bv63 6) ?x275)))
 (let (($x8961 (= ?x4305 ?x2861)))
 (let ((?x8854 (used_gas_s x_0 x_1 x_MLOAD_0 4)))
 (let (($x621 (= ?x8854 (+ 3 (used_gas_s x_0 x_1 x_MLOAD_0 3)))))
 (let ((?x6780 (stack_s x_0 x_1 x_MLOAD_0 3 ?x2861)))
 (let ((?x8977 (bvadd (_ bv62 6) ?x275)))
 (let ((?x10408 (stack_s x_0 x_1 x_MLOAD_0 3 ?x8977)))
 (let ((?x6093 (stack_s x_0 x_1 x_MLOAD_0 4 ?x10083)))
 (let (($x292 (exc_halt_s 3)))
 (let (($x466 (= $x292 (or (exc_halt_s 2) (not (bvsle (_ bv0 6) (bvadd (_ bv61 6) (sc_s 2))))))))
 (let (($x1377 (forall ((w (_ BitVec 256)) )(let ((?x7002 (storage_s x_0 x_1 x_MLOAD_0 2 w)))
 (let ((?x1149 (storage_s x_0 x_1 x_MLOAD_0 3 w)))
 (= ?x1149 ?x7002))))
 ))
 (let (($x3627 (forall ((n (_ BitVec 6)) )(let ((?x1451 (stack_s x_0 x_1 x_MLOAD_0 2 n)))
 (let ((?x1685 (stack_s x_0 x_1 x_MLOAD_0 3 n)))
 (let (($x1261 (= ?x1685 ?x1451)))
 (or $x1261 (bvsle (bvadd (_ bv61 6) (sc_s 2)) n))))))
 ))
 (let ((?x218 (sc_s 2)))
 (let (($x9085 (= ?x275 ?x218)))
 (let ((?x6383 (used_gas_s x_0 x_1 x_MLOAD_0 3)))
 (let (($x10930 (= ?x6383 (+ 3 (used_gas_s x_0 x_1 x_MLOAD_0 2)))))
 (let (($x11040 (= ?x10408 (stack_s x_0 x_1 x_MLOAD_0 2 (bvadd (_ bv62 6) ?x218)))))
 (let ((?x6327 (bvadd (_ bv63 6) ?x218)))
 (let ((?x10916 (stack_s x_0 x_1 x_MLOAD_0 2 ?x6327)))
 (let ((?x11015 (bvadd (_ bv61 6) ?x275)))
 (let ((?x4398 (stack_s x_0 x_1 x_MLOAD_0 3 ?x11015)))
 (let (($x247 (exc_halt_s 2)))
 (let (($x9112 (= $x247 (or (exc_halt_s 1) (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_s 1))))))))
 (let (($x584 (forall ((w (_ BitVec 256)) )(let ((?x5103 (storage_s x_0 x_1 x_MLOAD_0 1 w)))
 (let ((?x7002 (storage_s x_0 x_1 x_MLOAD_0 2 w)))
 (= ?x7002 ?x5103))))
 ))
 (let (($x9945 (forall ((n (_ BitVec 6)) )(let ((?x332 (stack_s x_0 x_1 x_MLOAD_0 1 n)))
 (let ((?x1451 (stack_s x_0 x_1 x_MLOAD_0 2 n)))
 (let (($x10834 (= ?x1451 ?x332)))
 (let ((?x154 (sc_s 1)))
 (let ((?x5240 (bvadd (_ bv63 6) ?x154)))
 (let (($x2656 (bvsle ?x5240 n)))
 (or $x2656 $x10834))))))))
 ))
 (let ((?x1082 (used_gas_s x_0 x_1 x_MLOAD_0 2)))
 (let (($x4703 (= ?x1082 (+ 3 (used_gas_s x_0 x_1 x_MLOAD_0 1)))))
 (let ((?x154 (sc_s 1)))
 (let ((?x5240 (bvadd (_ bv63 6) ?x154)))
 (let ((?x5634 (stack_s x_0 x_1 x_MLOAD_0 1 ?x5240)))
 (let (($x9528 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x72))) (_ bv0 1)))))
 (let (($x189 (exc_halt_s 1)))
 (let (($x1103 (forall ((w (_ BitVec 256)) )(let ((?x1764 (storage_s x_0 x_1 x_MLOAD_0 0 w)))
 (let ((?x5103 (storage_s x_0 x_1 x_MLOAD_0 1 w)))
 (= ?x5103 ?x1764))))
 ))
 (let (($x404 (forall ((n (_ BitVec 6)) )(let ((?x1505 (stack_s x_0 x_1 x_MLOAD_0 0 n)))
 (let ((?x332 (stack_s x_0 x_1 x_MLOAD_0 1 n)))
 (let (($x49 (= ?x332 ?x1505)))
 (or $x49 (bvsle (bvadd (_ bv63 6) (sc_s 0)) n))))))
 ))
 (let (($x10428 (= ?x154 (bvadd (_ bv1 6) ?x72))))
 (let ((?x847 (used_gas_s x_0 x_1 x_MLOAD_0 1)))
 (let (($x1626 (= ?x847 (+ 3 ?x2883))))
 (let ((?x517 (bvadd (_ bv63 6) ?x72)))
 (let ((?x10926 (stack_s x_0 x_1 x_MLOAD_0 0 ?x517)))
 (let (($x4643 (forall ((w0 (_ BitVec 256)) )(let ((?x5140 (ite (= (stack_s x_0 x_1 x_MLOAD_0 1 (bvadd (_ bv63 6) (sc_s 1))) w0) x_MLOAD_0 (_ bv0 256))))
 (let ((?x6023 (f_MLOAD x_0 x_1 x_MLOAD_0 w0)))
 (= ?x6023 ?x5140))))
 ))
 (let (($x4065 (forall ((w (_ BitVec 256)) )(let ((?x1764 (storage_s x_0 x_1 x_MLOAD_0 0 w)))
 (= ?x1764 (_ bv0 256))))
 ))
 (let (($x8817 (= ?x2883 0)))
 (let (($x3117 (not $x57)))
 (let (($x9450 (= (stack_s x_0 x_1 x_MLOAD_0 0 (_ bv1 6)) x_1)))
 (let (($x5818 (= (stack_s x_0 x_1 x_MLOAD_0 0 (_ bv0 6)) x_0)))
 (let (($x3159 (= ?x72 (_ bv2 6))))
 (and $x3159 $x5818 $x9450 $x3117 $x8817 $x4065 $x4643 (= ?x5634 ?x10926) (= (stack_s x_0 x_1 x_MLOAD_0 1 ?x517) ?x10926) $x1626 $x10428 $x404 $x1103 (= $x189 (or $x57 (not (bvsle (_ bv0 6) ?x517)) $x9528)) (= ?x10916 (f_MLOAD x_0 x_1 x_MLOAD_0 ?x5634)) $x4703 (= ?x218 ?x154) $x9945 $x584 $x9112 (= ?x6780 (stack_s x_0 x_1 x_MLOAD_0 2 (bvadd (_ bv61 6) ?x218))) (= ?x4398 ?x10916) $x11040 $x10930 $x9085 $x3627 $x1377 $x466 (= ?x6093 (ite (bvule ?x10408 ?x6780) (_ bv0 256) (_ bv1 256))) $x621 $x8961 $x9981 $x1441 $x1351 (= ?x297 (+ 2 ?x8854)) $x6538 $x485 $x963 $x7980 $x4540 $x3408 $x5046 $x4361 $x3773 (= (used_gas_t x_0 x_1 x_MLOAD_0 1) (+ 2 ?x9007)) $x7219 $x7187 $x1948 $x9111 (= ?x6517 (+ 2 (used_gas_t x_0 x_1 x_MLOAD_0 1))) $x7803 $x5750 $x2881 $x2625 $x73 $x5582 $x58 $x8879 $x7691 (not (and $x8054 $x4646 $x7260 $x1938))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
 )
(check-sat)
(get-proof)
