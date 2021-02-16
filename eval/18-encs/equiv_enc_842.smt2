; CALLVALUE DUP2 SWAP1 => DUP1 CALLVALUE
(set-option :produce-proofs true)
; 
(set-info :status unknown)
(declare-fun storage_t ((_ BitVec 256) (_ BitVec 256) Int (_ BitVec 256)) (_ BitVec 256))
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
 (exists ((x_0 (_ BitVec 256)) (x_CALLVALUE (_ BitVec 256)) )(let (($x6891 (forall ((w (_ BitVec 256)) )(let ((?x4894 (storage_t x_0 x_CALLVALUE 2 w)))
 (let ((?x7546 (storage_s x_0 x_CALLVALUE 3 w)))
 (= ?x7546 ?x4894))))
 ))
 (let (($x4057 (exc_halt_t 2)))
 (let (($x8103 (exc_halt_s 3)))
 (let (($x4469 (= $x8103 $x4057)))
 (let (($x10376 (forall ((n (_ BitVec 6)) )(let ((?x11248 (sc_t 2)))
 (let (($x4481 (bvsle ?x11248 n)))
 (let ((?x1212 (stack_t x_0 x_CALLVALUE 2 n)))
 (let ((?x2236 (stack_s x_0 x_CALLVALUE 3 n)))
 (let (($x884 (= ?x2236 ?x1212)))
 (or $x884 $x4481)))))))
 ))
 (let ((?x11248 (sc_t 2)))
 (let ((?x3851 (sc_s 3)))
 (let (($x5943 (= ?x3851 ?x11248)))
 (let ((?x7217 (used_gas_t x_0 x_CALLVALUE 0)))
 (let ((?x4574 (used_gas_s x_0 x_CALLVALUE 0)))
 (let (($x5739 (= ?x4574 ?x7217)))
 (let (($x8091 (forall ((w (_ BitVec 256)) )(let ((?x1834 (storage_t x_0 x_CALLVALUE 0 w)))
 (let ((?x1528 (storage_s x_0 x_CALLVALUE 0 w)))
 (= ?x1528 ?x1834))))
 ))
 (let (($x56 (exc_halt_t 0)))
 (let (($x57 (exc_halt_s 0)))
 (let (($x58 (= $x57 $x56)))
 (let (($x3985 (forall ((n (_ BitVec 6)) )(let ((?x7526 (stack_t x_0 x_CALLVALUE 0 n)))
 (let ((?x11516 (stack_s x_0 x_CALLVALUE 0 n)))
 (let (($x11363 (= ?x11516 ?x7526)))
 (let ((?x63 (sc_t 0)))
 (let (($x9079 (bvsle ?x63 n)))
 (or $x9079 $x11363)))))))
 ))
 (let ((?x63 (sc_t 0)))
 (let ((?x72 (sc_s 0)))
 (let (($x73 (= ?x72 ?x63)))
 (let (($x4852 (exc_halt_t 1)))
 (let (($x8809 (or $x4852 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_t 1)))) (_ bv0 1))))))
 (let (($x4116 (= $x4057 $x8809)))
 (let (($x6876 (forall ((w (_ BitVec 256)) )(let ((?x6130 (storage_t x_0 x_CALLVALUE 1 w)))
 (let ((?x4894 (storage_t x_0 x_CALLVALUE 2 w)))
 (= ?x4894 ?x6130))))
 ))
 (let (($x11187 (forall ((n (_ BitVec 6)) )(let ((?x9666 (sc_t 1)))
 (let (($x9963 (bvsle ?x9666 n)))
 (let ((?x7963 (stack_t x_0 x_CALLVALUE 1 n)))
 (let ((?x1212 (stack_t x_0 x_CALLVALUE 2 n)))
 (let (($x8776 (= ?x1212 ?x7963)))
 (or $x8776 $x9963)))))))
 ))
 (let (($x11433 (= ?x11248 (bvadd (_ bv1 6) (sc_t 1)))))
 (let ((?x3120 (used_gas_t x_0 x_CALLVALUE 2)))
 (let (($x9117 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x63))) (_ bv0 1)))))
 (let (($x1644 (= $x4852 (or $x56 (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) ?x63))) $x9117))))
 (let (($x10408 (forall ((w (_ BitVec 256)) )(let ((?x1834 (storage_t x_0 x_CALLVALUE 0 w)))
 (let ((?x6130 (storage_t x_0 x_CALLVALUE 1 w)))
 (= ?x6130 ?x1834))))
 ))
 (let (($x1198 (forall ((n (_ BitVec 6)) )(or (= (stack_t x_0 x_CALLVALUE 1 n) (stack_t x_0 x_CALLVALUE 0 n)) (bvsle (bvadd (_ bv63 6) (sc_t 0)) n)))
 ))
 (let ((?x9666 (sc_t 1)))
 (let (($x9898 (= ?x9666 (bvadd (_ bv1 6) ?x63))))
 (let ((?x5193 (used_gas_t x_0 x_CALLVALUE 1)))
 (let (($x4946 (= ?x5193 (+ 3 ?x7217))))
 (let ((?x1574 (bvadd (_ bv63 6) ?x63)))
 (let ((?x8063 (stack_t x_0 x_CALLVALUE 0 ?x1574)))
 (let (($x6305 (= (stack_t x_0 x_CALLVALUE 1 ?x1574) ?x8063)))
 (let ((?x3616 (bvadd (_ bv63 6) ?x9666)))
 (let ((?x204 (stack_t x_0 x_CALLVALUE 1 ?x3616)))
 (let (($x10307 (= ?x204 ?x8063)))
 (let (($x7113 (= $x8103 (or (exc_halt_s 2) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 2))))))))
 (let (($x9760 (forall ((w (_ BitVec 256)) )(let ((?x7624 (storage_s x_0 x_CALLVALUE 2 w)))
 (let ((?x7546 (storage_s x_0 x_CALLVALUE 3 w)))
 (= ?x7546 ?x7624))))
 ))
 (let (($x6908 (forall ((n (_ BitVec 6)) )(let ((?x2272 (sc_s 2)))
 (let ((?x9266 (bvadd (_ bv62 6) ?x2272)))
 (let (($x9051 (bvsle ?x9266 n)))
 (or (= (stack_s x_0 x_CALLVALUE 3 n) (stack_s x_0 x_CALLVALUE 2 n)) $x9051)))))
 ))
 (let ((?x2272 (sc_s 2)))
 (let (($x10118 (= ?x3851 ?x2272)))
 (let ((?x167 (used_gas_s x_0 x_CALLVALUE 3)))
 (let (($x10749 (= ?x167 (+ 3 (used_gas_s x_0 x_CALLVALUE 2)))))
 (let ((?x5528 (bvadd (_ bv63 6) ?x2272)))
 (let ((?x4150 (stack_s x_0 x_CALLVALUE 2 ?x5528)))
 (let ((?x1039 (bvadd (_ bv62 6) ?x3851)))
 (let ((?x1550 (stack_s x_0 x_CALLVALUE 3 ?x1039)))
 (let (($x1020 (= ?x1550 ?x4150)))
 (let ((?x998 (bvadd (_ bv63 6) ?x3851)))
 (let ((?x2090 (stack_s x_0 x_CALLVALUE 3 ?x998)))
 (let (($x2431 (= ?x2090 (stack_s x_0 x_CALLVALUE 2 (bvadd (_ bv62 6) ?x2272)))))
 (let (($x8780 (exc_halt_s 1)))
 (let (($x186 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 1)))) (_ bv0 1)))))
 (let (($x10052 (exc_halt_s 2)))
 (let (($x960 (= $x10052 (or $x186 (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 1)))) $x8780))))
 (let (($x1712 (forall ((w (_ BitVec 256)) )(let ((?x9964 (storage_s x_0 x_CALLVALUE 1 w)))
 (let ((?x7624 (storage_s x_0 x_CALLVALUE 2 w)))
 (= ?x7624 ?x9964))))
 ))
 (let (($x10519 (forall ((n (_ BitVec 6)) )(or (= (stack_s x_0 x_CALLVALUE 2 n) (stack_s x_0 x_CALLVALUE 1 n)) (bvsle (bvadd (_ bv62 6) (sc_s 1)) n)))
 ))
 (let (($x2595 (= ?x2272 (bvadd (_ bv1 6) (sc_s 1)))))
 (let ((?x9685 (used_gas_s x_0 x_CALLVALUE 2)))
 (let (($x5034 (= ?x9685 (+ 3 (used_gas_s x_0 x_CALLVALUE 1)))))
 (let ((?x154 (sc_s 1)))
 (let ((?x10565 (bvadd (_ bv63 6) ?x154)))
 (let ((?x3437 (stack_s x_0 x_CALLVALUE 1 ?x10565)))
 (let (($x1271 (= (stack_s x_0 x_CALLVALUE 2 ?x10565) ?x3437)))
 (let ((?x5195 (bvadd (_ bv62 6) ?x154)))
 (let ((?x5118 (stack_s x_0 x_CALLVALUE 1 ?x5195)))
 (let (($x8481 (= (stack_s x_0 x_CALLVALUE 2 ?x5195) ?x5118)))
 (let (($x9383 (= ?x4150 ?x5118)))
 (let (($x5465 (= $x8780 (or $x57 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x72))) (_ bv0 1)))))))
 (let (($x3599 (forall ((w (_ BitVec 256)) )(let ((?x1528 (storage_s x_0 x_CALLVALUE 0 w)))
 (let ((?x9964 (storage_s x_0 x_CALLVALUE 1 w)))
 (= ?x9964 ?x1528))))
 ))
 (let (($x10122 (forall ((n (_ BitVec 6)) )(let ((?x11516 (stack_s x_0 x_CALLVALUE 0 n)))
 (let ((?x55 (stack_s x_0 x_CALLVALUE 1 n)))
 (let (($x5569 (= ?x55 ?x11516)))
 (let ((?x72 (sc_s 0)))
 (let (($x4347 (bvsle ?x72 n)))
 (or $x4347 $x5569)))))))
 ))
 (let (($x10888 (= ?x154 (bvadd (_ bv1 6) ?x72))))
 (let (($x11525 (forall ((w (_ BitVec 256)) )(let ((?x1528 (storage_s x_0 x_CALLVALUE 0 w)))
 (= ?x1528 (_ bv0 256))))
 ))
 (let (($x3600 (= ?x4574 0)))
 (let (($x10995 (not $x57)))
 (let (($x8484 (= (stack_s x_0 x_CALLVALUE 0 (_ bv0 6)) x_0)))
 (let (($x7461 (= ?x72 (_ bv1 6))))
 (and $x7461 $x8484 $x10995 $x3600 $x11525 (= (stack_s x_0 x_CALLVALUE 1 ?x72) x_CALLVALUE) (= (used_gas_s x_0 x_CALLVALUE 1) (+ 2 ?x4574)) $x10888 $x10122 $x3599 $x5465 $x9383 $x8481 $x1271 $x5034 $x2595 $x10519 $x1712 $x960 $x2431 $x1020 $x10749 $x10118 $x6908 $x9760 $x7113 $x10307 $x6305 $x4946 $x9898 $x1198 $x10408 $x1644 (= (stack_t x_0 x_CALLVALUE 2 ?x9666) x_CALLVALUE) (= ?x3120 (+ 2 ?x5193)) $x11433 $x11187 $x6876 $x4116 $x73 $x3985 $x58 $x8091 $x5739 (not (and $x5943 $x10376 $x4469 $x6891)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
 )
(check-sat)
(get-proof)