; DUP1 SLOAD SWAP1 POP => SLOAD
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
 (exists ((x_0 (_ BitVec 256)) (x_SLOAD_0 (_ BitVec 256)) )(let (($x874 (forall ((w (_ BitVec 256)) )(let ((?x857 (storage_t x_0 x_SLOAD_0 1 w)))
 (let ((?x8703 (storage_s x_0 x_SLOAD_0 4 w)))
 (= ?x8703 ?x857))))
 ))
 (let (($x8390 (exc_halt_t 1)))
 (let (($x9175 (exc_halt_s 4)))
 (let (($x1671 (= $x9175 $x8390)))
 (let (($x10672 (forall ((n (_ BitVec 6)) )(let ((?x2547 (sc_t 1)))
 (let (($x8264 (bvsle ?x2547 n)))
 (let ((?x5434 (stack_t x_0 x_SLOAD_0 1 n)))
 (let ((?x186 (stack_s x_0 x_SLOAD_0 4 n)))
 (let (($x7836 (= ?x186 ?x5434)))
 (or $x7836 $x8264)))))))
 ))
 (let ((?x2547 (sc_t 1)))
 (let ((?x9433 (sc_s 4)))
 (let (($x3076 (= ?x9433 ?x2547)))
 (let ((?x11234 (used_gas_t x_0 x_SLOAD_0 0)))
 (let ((?x5134 (used_gas_s x_0 x_SLOAD_0 0)))
 (let (($x803 (= ?x5134 ?x11234)))
 (let (($x6263 (forall ((w (_ BitVec 256)) )(let ((?x10338 (storage_t x_0 x_SLOAD_0 0 w)))
 (let ((?x1174 (storage_s x_0 x_SLOAD_0 0 w)))
 (= ?x1174 ?x10338))))
 ))
 (let (($x56 (exc_halt_t 0)))
 (let (($x57 (exc_halt_s 0)))
 (let (($x58 (= $x57 $x56)))
 (let (($x10733 (forall ((n (_ BitVec 6)) )(let ((?x9279 (stack_t x_0 x_SLOAD_0 0 n)))
 (let ((?x10752 (stack_s x_0 x_SLOAD_0 0 n)))
 (let (($x6265 (= ?x10752 ?x9279)))
 (let ((?x63 (sc_t 0)))
 (let (($x5163 (bvsle ?x63 n)))
 (or $x5163 $x6265)))))))
 ))
 (let ((?x63 (sc_t 0)))
 (let ((?x72 (sc_s 0)))
 (let (($x73 (= ?x72 ?x63)))
 (let (($x7288 (forall ((w (_ BitVec 256)) )(let ((?x10338 (storage_t x_0 x_SLOAD_0 0 w)))
 (let ((?x857 (storage_t x_0 x_SLOAD_0 1 w)))
 (= ?x857 ?x10338))))
 ))
 (let (($x8536 (forall ((n (_ BitVec 6)) )(let ((?x63 (sc_t 0)))
 (let ((?x8237 (bvadd (_ bv63 6) ?x63)))
 (let (($x3984 (bvsle ?x8237 n)))
 (or $x3984 (= (stack_t x_0 x_SLOAD_0 1 n) (stack_t x_0 x_SLOAD_0 0 n)))))))
 ))
 (let (($x8940 (= ?x2547 ?x63)))
 (let ((?x1802 (storage_t x_0 x_SLOAD_0 0 (stack_t x_0 x_SLOAD_0 0 (bvadd (_ bv63 6) ?x63)))))
 (let (($x4742 (= $x9175 (or (exc_halt_s 3) (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_s 3))))))))
 (let (($x7856 (forall ((w (_ BitVec 256)) )(let ((?x5952 (storage_s x_0 x_SLOAD_0 3 w)))
 (let ((?x8703 (storage_s x_0 x_SLOAD_0 4 w)))
 (= ?x8703 ?x5952))))
 ))
 (let (($x3542 (forall ((n (_ BitVec 6)) )(let ((?x3851 (sc_s 3)))
 (let ((?x8535 (bvadd (_ bv63 6) ?x3851)))
 (let (($x2265 (bvsle ?x8535 n)))
 (or $x2265 (= (stack_s x_0 x_SLOAD_0 4 n) (stack_s x_0 x_SLOAD_0 3 n)))))))
 ))
 (let (($x7197 (= (used_gas_s x_0 x_SLOAD_0 4) (+ 2 (used_gas_s x_0 x_SLOAD_0 3)))))
 (let (($x8103 (exc_halt_s 3)))
 (let (($x3778 (= $x8103 (or (exc_halt_s 2) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 2))))))))
 (let (($x1469 (forall ((w (_ BitVec 256)) )(let ((?x4939 (storage_s x_0 x_SLOAD_0 2 w)))
 (let ((?x5952 (storage_s x_0 x_SLOAD_0 3 w)))
 (= ?x5952 ?x4939))))
 ))
 (let (($x3517 (forall ((n (_ BitVec 6)) )(let ((?x2272 (sc_s 2)))
 (let ((?x3112 (bvadd (_ bv62 6) ?x2272)))
 (let (($x8329 (bvsle ?x3112 n)))
 (or (= (stack_s x_0 x_SLOAD_0 3 n) (stack_s x_0 x_SLOAD_0 2 n)) $x8329)))))
 ))
 (let ((?x1216 (used_gas_s x_0 x_SLOAD_0 3)))
 (let ((?x2272 (sc_s 2)))
 (let ((?x5543 (bvadd (_ bv63 6) ?x2272)))
 (let ((?x8047 (stack_s x_0 x_SLOAD_0 2 ?x5543)))
 (let (($x9068 (= (stack_s x_0 x_SLOAD_0 3 (bvadd (_ bv63 6) (sc_s 3))) (stack_s x_0 x_SLOAD_0 2 (bvadd (_ bv62 6) ?x2272)))))
 (let (($x10052 (exc_halt_s 2)))
 (let (($x3898 (= $x10052 (or (exc_halt_s 1) (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_s 1))))))))
 (let (($x1736 (forall ((w (_ BitVec 256)) )(let ((?x10268 (storage_s x_0 x_SLOAD_0 1 w)))
 (let ((?x4939 (storage_s x_0 x_SLOAD_0 2 w)))
 (= ?x4939 ?x10268))))
 ))
 (let (($x11401 (forall ((n (_ BitVec 6)) )(let ((?x154 (sc_s 1)))
 (let ((?x5037 (bvadd (_ bv63 6) ?x154)))
 (let (($x11618 (bvsle ?x5037 n)))
 (or (= (stack_s x_0 x_SLOAD_0 2 n) (stack_s x_0 x_SLOAD_0 1 n)) $x11618)))))
 ))
 (let ((?x154 (sc_s 1)))
 (let (($x43 (= ?x2272 ?x154)))
 (let ((?x5691 (used_gas_s x_0 x_SLOAD_0 2)))
 (let ((?x5037 (bvadd (_ bv63 6) ?x154)))
 (let ((?x11610 (stack_s x_0 x_SLOAD_0 1 ?x5037)))
 (let (($x7461 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x72))) (_ bv0 1)))))
 (let (($x8780 (exc_halt_s 1)))
 (let (($x1826 (forall ((w (_ BitVec 256)) )(let ((?x1174 (storage_s x_0 x_SLOAD_0 0 w)))
 (let ((?x10268 (storage_s x_0 x_SLOAD_0 1 w)))
 (= ?x10268 ?x1174))))
 ))
 (let (($x4015 (forall ((n (_ BitVec 6)) )(or (= (stack_s x_0 x_SLOAD_0 1 n) (stack_s x_0 x_SLOAD_0 0 n)) (bvsle (bvadd (_ bv63 6) (sc_s 0)) n)))
 ))
 (let (($x185 (= ?x154 (bvadd (_ bv1 6) ?x72))))
 (let ((?x9462 (bvadd (_ bv63 6) ?x72)))
 (let ((?x10542 (stack_s x_0 x_SLOAD_0 0 ?x9462)))
 (let (($x4426 (forall ((w (_ BitVec 256)) )(let ((?x8721 (ite (= w (stack_s x_0 x_SLOAD_0 1 (bvadd (_ bv63 6) (sc_s 1)))) x_SLOAD_0 (_ bv0 256))))
 (let ((?x1174 (storage_s x_0 x_SLOAD_0 0 w)))
 (= ?x1174 ?x8721))))
 ))
 (let (($x2106 (= ?x5134 0)))
 (let (($x5099 (not $x57)))
 (let (($x6363 (= (stack_s x_0 x_SLOAD_0 0 (_ bv0 6)) x_0)))
 (let (($x8601 (= ?x72 (_ bv1 6))))
 (and $x8601 $x6363 $x5099 $x2106 $x4426 (= ?x11610 ?x10542) (= (stack_s x_0 x_SLOAD_0 1 ?x9462) ?x10542) (= (used_gas_s x_0 x_SLOAD_0 1) (+ 3 ?x5134)) $x185 $x4015 $x1826 (= $x8780 (or $x57 (not (bvsle (_ bv0 6) ?x9462)) $x7461)) (= ?x8047 (storage_s x_0 x_SLOAD_0 1 ?x11610)) (= ?x5691 (+ 200 (used_gas_s x_0 x_SLOAD_0 1))) $x43 $x11401 $x1736 $x3898 $x9068 (= (stack_s x_0 x_SLOAD_0 3 (bvadd (_ bv62 6) (sc_s 3))) ?x8047) (= ?x1216 (+ 3 ?x5691)) (= (sc_s 3) ?x2272) $x3517 $x1469 $x3778 $x7197 (= ?x9433 (bvadd (_ bv63 6) (sc_s 3))) $x3542 $x7856 $x4742 (= (stack_t x_0 x_SLOAD_0 1 (bvadd (_ bv63 6) ?x2547)) ?x1802) (= (used_gas_t x_0 x_SLOAD_0 1) (+ 200 ?x11234)) $x8940 $x8536 $x7288 (= $x8390 (or $x56 (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) ?x63))))) $x73 $x10733 $x58 $x6263 $x803 (not (and $x3076 $x10672 $x1671 $x874)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
 )
(check-sat)
(get-proof)