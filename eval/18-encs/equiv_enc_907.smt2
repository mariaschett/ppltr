; PUSH cw_3 PUSH cw_4 DUP2 SWAP1 => PUSH cw_3 PUSH cw_3 PUSH cw_4
(set-option :produce-proofs true)
; 
(set-info :status unknown)
(declare-fun storage_t ((_ BitVec 256) (_ BitVec 256) Int (_ BitVec 256)) (_ BitVec 256))
(declare-fun w_4 () (_ BitVec 256))
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
 (let (($x344 (forall ((w (_ BitVec 256)) )(let ((?x3301 (storage_t w_3 w_4 3 w)))
 (let ((?x1067 (storage_s w_3 w_4 4 w)))
 (= ?x1067 ?x3301))))
 ))
 (let (($x9131 (exc_halt_t 3)))
 (let (($x9175 (exc_halt_s 4)))
 (let (($x3002 (= $x9175 $x9131)))
 (let (($x10817 (forall ((n (_ BitVec 6)) )(let ((?x10013 (sc_t 3)))
 (let (($x11349 (bvsle ?x10013 n)))
 (let ((?x6061 (stack_t w_3 w_4 3 n)))
 (let ((?x5303 (stack_s w_3 w_4 4 n)))
 (let (($x5337 (= ?x5303 ?x6061)))
 (or $x5337 $x11349)))))))
 ))
 (let ((?x10013 (sc_t 3)))
 (let ((?x9433 (sc_s 4)))
 (let (($x6978 (= ?x9433 ?x10013)))
 (let ((?x1104 (used_gas_t w_3 w_4 0)))
 (let ((?x1881 (used_gas_s w_3 w_4 0)))
 (let (($x5032 (= ?x1881 ?x1104)))
 (let (($x1643 (forall ((w (_ BitVec 256)) )(let ((?x5513 (storage_t w_3 w_4 0 w)))
 (let ((?x360 (storage_s w_3 w_4 0 w)))
 (= ?x360 ?x5513))))
 ))
 (let (($x56 (exc_halt_t 0)))
 (let (($x57 (exc_halt_s 0)))
 (let (($x58 (= $x57 $x56)))
 (let (($x3126 (forall ((n (_ BitVec 6)) )(let ((?x10000 (stack_t w_3 w_4 0 n)))
 (let ((?x620 (stack_s w_3 w_4 0 n)))
 (let (($x7640 (= ?x620 ?x10000)))
 (let ((?x63 (sc_t 0)))
 (let (($x3394 (bvsle ?x63 n)))
 (or $x3394 $x7640)))))))
 ))
 (let ((?x63 (sc_t 0)))
 (let ((?x72 (sc_s 0)))
 (let (($x73 (= ?x72 ?x63)))
 (let (($x2189 (exc_halt_t 2)))
 (let (($x2693 (or $x2189 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_t 2)))) (_ bv0 1))))))
 (let (($x807 (= $x9131 $x2693)))
 (let (($x4887 (forall ((w (_ BitVec 256)) )(let ((?x6985 (storage_t w_3 w_4 2 w)))
 (let ((?x3301 (storage_t w_3 w_4 3 w)))
 (= ?x3301 ?x6985))))
 ))
 (let (($x1205 (forall ((n (_ BitVec 6)) )(let ((?x6718 (sc_t 2)))
 (let (($x4178 (bvsle ?x6718 n)))
 (or (= (stack_t w_3 w_4 3 n) (stack_t w_3 w_4 2 n)) $x4178))))
 ))
 (let (($x10225 (= ?x10013 (bvadd (_ bv1 6) (sc_t 2)))))
 (let (($x5574 (= (used_gas_t w_3 w_4 3) (+ 3 (used_gas_t w_3 w_4 2)))))
 (let (($x11058 (exc_halt_t 1)))
 (let (($x8257 (or $x11058 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_t 1)))) (_ bv0 1))))))
 (let (($x6995 (= $x2189 $x8257)))
 (let (($x4929 (forall ((w (_ BitVec 256)) )(let ((?x2065 (storage_t w_3 w_4 1 w)))
 (let ((?x6985 (storage_t w_3 w_4 2 w)))
 (= ?x6985 ?x2065))))
 ))
 (let (($x7751 (forall ((n (_ BitVec 6)) )(let ((?x2707 (sc_t 1)))
 (let (($x6519 (bvsle ?x2707 n)))
 (or (= (stack_t w_3 w_4 2 n) (stack_t w_3 w_4 1 n)) $x6519))))
 ))
 (let ((?x6718 (sc_t 2)))
 (let (($x7211 (= ?x6718 (bvadd (_ bv1 6) (sc_t 1)))))
 (let ((?x1770 (used_gas_t w_3 w_4 2)))
 (let (($x11104 (= $x11058 (or $x56 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x63))) (_ bv0 1)))))))
 (let (($x6729 (forall ((w (_ BitVec 256)) )(let ((?x5513 (storage_t w_3 w_4 0 w)))
 (let ((?x2065 (storage_t w_3 w_4 1 w)))
 (= ?x2065 ?x5513))))
 ))
 (let (($x9186 (forall ((n (_ BitVec 6)) )(let ((?x63 (sc_t 0)))
 (let (($x3394 (bvsle ?x63 n)))
 (or (= (stack_t w_3 w_4 1 n) (stack_t w_3 w_4 0 n)) $x3394))))
 ))
 (let ((?x2707 (sc_t 1)))
 (let (($x5757 (= ?x2707 (bvadd (_ bv1 6) ?x63))))
 (let (($x8838 (= $x9175 (or (exc_halt_s 3) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 3))))))))
 (let (($x7790 (forall ((w (_ BitVec 256)) )(let ((?x797 (storage_s w_3 w_4 3 w)))
 (let ((?x1067 (storage_s w_3 w_4 4 w)))
 (= ?x1067 ?x797))))
 ))
 (let (($x386 (forall ((n (_ BitVec 6)) )(let ((?x3851 (sc_s 3)))
 (let ((?x4510 (bvadd (_ bv62 6) ?x3851)))
 (let (($x3583 (bvsle ?x4510 n)))
 (or $x3583 (= (stack_s w_3 w_4 4 n) (stack_s w_3 w_4 3 n)))))))
 ))
 (let ((?x3851 (sc_s 3)))
 (let (($x9657 (= ?x9433 ?x3851)))
 (let (($x6216 (= (used_gas_s w_3 w_4 4) (+ 3 (used_gas_s w_3 w_4 3)))))
 (let ((?x9316 (bvadd (_ bv63 6) ?x3851)))
 (let ((?x9068 (stack_s w_3 w_4 3 ?x9316)))
 (let (($x4834 (= (stack_s w_3 w_4 4 (bvadd (_ bv63 6) ?x9433)) (stack_s w_3 w_4 3 (bvadd (_ bv62 6) ?x3851)))))
 (let (($x10052 (exc_halt_s 2)))
 (let (($x5143 (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 2))))))
 (let (($x4140 (or $x5143 $x10052 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 2)))) (_ bv0 1))))))
 (let (($x8103 (exc_halt_s 3)))
 (let (($x566 (= $x8103 $x4140)))
 (let (($x9602 (forall ((w (_ BitVec 256)) )(let ((?x10595 (storage_s w_3 w_4 2 w)))
 (let ((?x797 (storage_s w_3 w_4 3 w)))
 (= ?x797 ?x10595))))
 ))
 (let (($x1289 (forall ((n (_ BitVec 6)) )(let ((?x2272 (sc_s 2)))
 (let ((?x8832 (bvadd (_ bv62 6) ?x2272)))
 (let (($x7918 (bvsle ?x8832 n)))
 (or $x7918 (= (stack_s w_3 w_4 3 n) (stack_s w_3 w_4 2 n)))))))
 ))
 (let (($x10411 (= ?x3851 (bvadd (_ bv1 6) (sc_s 2)))))
 (let ((?x8811 (used_gas_s w_3 w_4 3)))
 (let (($x8952 (= (stack_s w_3 w_4 3 (bvadd (_ bv63 6) (sc_s 2))) (stack_s w_3 w_4 2 (bvadd (_ bv63 6) (sc_s 2))))))
 (let ((?x2272 (sc_s 2)))
 (let ((?x8832 (bvadd (_ bv62 6) ?x2272)))
 (let ((?x5351 (stack_s w_3 w_4 2 ?x8832)))
 (let (($x8780 (exc_halt_s 1)))
 (let (($x2261 (or $x8780 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 1)))) (_ bv0 1))))))
 (let (($x8670 (= $x10052 $x2261)))
 (let (($x251 (forall ((w (_ BitVec 256)) )(let ((?x1659 (storage_s w_3 w_4 1 w)))
 (let ((?x10595 (storage_s w_3 w_4 2 w)))
 (= ?x10595 ?x1659))))
 ))
 (let (($x7582 (forall ((n (_ BitVec 6)) )(let ((?x154 (sc_s 1)))
 (let (($x2938 (bvsle ?x154 n)))
 (or $x2938 (= (stack_s w_3 w_4 2 n) (stack_s w_3 w_4 1 n))))))
 ))
 (let (($x7159 (= ?x2272 (bvadd (_ bv1 6) (sc_s 1)))))
 (let ((?x6706 (used_gas_s w_3 w_4 2)))
 (let (($x426 (= $x8780 (or $x57 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x72))) (_ bv0 1)))))))
 (let (($x582 (forall ((w (_ BitVec 256)) )(let ((?x360 (storage_s w_3 w_4 0 w)))
 (let ((?x1659 (storage_s w_3 w_4 1 w)))
 (= ?x1659 ?x360))))
 ))
 (let (($x600 (forall ((n (_ BitVec 6)) )(let ((?x72 (sc_s 0)))
 (let (($x1781 (bvsle ?x72 n)))
 (or $x1781 (= (stack_s w_3 w_4 1 n) (stack_s w_3 w_4 0 n))))))
 ))
 (let ((?x154 (sc_s 1)))
 (let (($x4355 (= ?x154 (bvadd (_ bv1 6) ?x72))))
 (let (($x2393 (forall ((w (_ BitVec 256)) )(let ((?x360 (storage_s w_3 w_4 0 w)))
 (= ?x360 (_ bv0 256))))
 ))
 (let (($x7846 (= ?x1881 0)))
 (let (($x7103 (not $x57)))
 (let (($x4825 (= ?x72 (_ bv0 6))))
 (and $x4825 $x7103 $x7846 $x2393 (= (stack_s w_3 w_4 1 ?x72) w_3) (= (used_gas_s w_3 w_4 1) (+ 3 ?x1881)) $x4355 $x600 $x582 $x426 (= (stack_s w_3 w_4 2 ?x154) w_4) (= ?x6706 (+ 3 (used_gas_s w_3 w_4 1))) $x7159 $x7582 $x251 $x8670 (= ?x9068 ?x5351) (= (stack_s w_3 w_4 3 ?x8832) ?x5351) $x8952 (= ?x8811 (+ 3 ?x6706)) $x10411 $x1289 $x9602 $x566 $x4834 (= (stack_s w_3 w_4 4 (bvadd (_ bv62 6) ?x9433)) ?x9068) $x6216 $x9657 $x386 $x7790 $x8838 (= (stack_t w_3 w_4 1 ?x63) w_3) (= (used_gas_t w_3 w_4 1) (+ 3 ?x1104)) $x5757 $x9186 $x6729 $x11104 (= (stack_t w_3 w_4 2 ?x2707) w_3) (= ?x1770 (+ 3 (used_gas_t w_3 w_4 1))) $x7211 $x7751 $x4929 $x6995 (= (stack_t w_3 w_4 3 ?x6718) w_4) $x5574 $x10225 $x1205 $x4887 $x807 $x73 $x3126 $x58 $x1643 $x5032 (not (and $x6978 $x10817 $x3002 $x344)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
(check-sat)
(get-proof)