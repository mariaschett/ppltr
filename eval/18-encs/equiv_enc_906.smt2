; PUSH 0x00 CALLVALUE GT ISZERO => CALLVALUE ISZERO
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
 (exists ((x_CALLVALUE (_ BitVec 256)) )(let (($x11060 (forall ((w (_ BitVec 256)) )(let ((?x1295 (storage_t x_CALLVALUE 2 w)))
 (let ((?x10547 (storage_s x_CALLVALUE 4 w)))
 (= ?x10547 ?x1295))))
 ))
 (let (($x2189 (exc_halt_t 2)))
 (let (($x9175 (exc_halt_s 4)))
 (let (($x5384 (= $x9175 $x2189)))
 (let (($x11306 (forall ((n (_ BitVec 6)) )(let ((?x6718 (sc_t 2)))
 (let (($x4178 (bvsle ?x6718 n)))
 (let ((?x3620 (stack_t x_CALLVALUE 2 n)))
 (let ((?x7699 (stack_s x_CALLVALUE 4 n)))
 (let (($x3028 (= ?x7699 ?x3620)))
 (or $x3028 $x4178)))))))
 ))
 (let ((?x6718 (sc_t 2)))
 (let ((?x9433 (sc_s 4)))
 (let (($x4221 (= ?x9433 ?x6718)))
 (let ((?x2850 (used_gas_t x_CALLVALUE 0)))
 (let ((?x7930 (used_gas_s x_CALLVALUE 0)))
 (let (($x6992 (= ?x7930 ?x2850)))
 (let (($x939 (forall ((w (_ BitVec 256)) )(let ((?x3848 (storage_t x_CALLVALUE 0 w)))
 (let ((?x7772 (storage_s x_CALLVALUE 0 w)))
 (= ?x7772 ?x3848))))
 ))
 (let (($x56 (exc_halt_t 0)))
 (let (($x57 (exc_halt_s 0)))
 (let (($x58 (= $x57 $x56)))
 (let (($x6019 (forall ((n (_ BitVec 6)) )(let ((?x327 (stack_t x_CALLVALUE 0 n)))
 (let ((?x4016 (stack_s x_CALLVALUE 0 n)))
 (let (($x7606 (= ?x4016 ?x327)))
 (let ((?x63 (sc_t 0)))
 (let (($x3394 (bvsle ?x63 n)))
 (or $x3394 $x7606)))))))
 ))
 (let ((?x63 (sc_t 0)))
 (let ((?x72 (sc_s 0)))
 (let (($x73 (= ?x72 ?x63)))
 (let (($x2205 (= $x2189 (or (exc_halt_t 1) (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_t 1))))))))
 (let (($x2093 (forall ((w (_ BitVec 256)) )(let ((?x662 (storage_t x_CALLVALUE 1 w)))
 (let ((?x1295 (storage_t x_CALLVALUE 2 w)))
 (= ?x1295 ?x662))))
 ))
 (let (($x9073 (forall ((n (_ BitVec 6)) )(or (bvsle (bvadd (_ bv63 6) (sc_t 1)) n) (= (stack_t x_CALLVALUE 2 n) (stack_t x_CALLVALUE 1 n))))
 ))
 (let ((?x414 (ite (= (stack_t x_CALLVALUE 1 (bvadd (_ bv63 6) (sc_t 1))) (_ bv0 256)) (_ bv1 256) (_ bv0 256))))
 (let (($x11058 (exc_halt_t 1)))
 (let (($x11104 (= $x11058 (or $x56 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x63))) (_ bv0 1)))))))
 (let (($x6443 (forall ((w (_ BitVec 256)) )(let ((?x3848 (storage_t x_CALLVALUE 0 w)))
 (let ((?x662 (storage_t x_CALLVALUE 1 w)))
 (= ?x662 ?x3848))))
 ))
 (let (($x11273 (forall ((n (_ BitVec 6)) )(let ((?x63 (sc_t 0)))
 (let (($x3394 (bvsle ?x63 n)))
 (or $x3394 (= (stack_t x_CALLVALUE 1 n) (stack_t x_CALLVALUE 0 n))))))
 ))
 (let ((?x2707 (sc_t 1)))
 (let (($x5757 (= ?x2707 (bvadd (_ bv1 6) ?x63))))
 (let (($x10193 (= $x9175 (or (exc_halt_s 3) (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_s 3))))))))
 (let (($x645 (forall ((w (_ BitVec 256)) )(let ((?x8767 (storage_s x_CALLVALUE 3 w)))
 (let ((?x10547 (storage_s x_CALLVALUE 4 w)))
 (= ?x10547 ?x8767))))
 ))
 (let (($x4401 (forall ((n (_ BitVec 6)) )(or (= (stack_s x_CALLVALUE 4 n) (stack_s x_CALLVALUE 3 n)) (bvsle (bvadd (_ bv63 6) (sc_s 3)) n)))
 ))
 (let ((?x6203 (ite (= (stack_s x_CALLVALUE 3 (bvadd (_ bv63 6) (sc_s 3))) (_ bv0 256)) (_ bv1 256) (_ bv0 256))))
 (let (($x8103 (exc_halt_s 3)))
 (let (($x3392 (= $x8103 (or (exc_halt_s 2) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 2))))))))
 (let (($x10389 (forall ((w (_ BitVec 256)) )(let ((?x3343 (storage_s x_CALLVALUE 2 w)))
 (let ((?x8767 (storage_s x_CALLVALUE 3 w)))
 (= ?x8767 ?x3343))))
 ))
 (let (($x7117 (forall ((n (_ BitVec 6)) )(let ((?x2272 (sc_s 2)))
 (let ((?x8832 (bvadd (_ bv62 6) ?x2272)))
 (let (($x7918 (bvsle ?x8832 n)))
 (or (= (stack_s x_CALLVALUE 3 n) (stack_s x_CALLVALUE 2 n)) $x7918)))))
 ))
 (let (($x6619 (bvule (stack_s x_CALLVALUE 2 (bvadd (_ bv63 6) (sc_s 2))) (stack_s x_CALLVALUE 2 (bvadd (_ bv62 6) (sc_s 2))))))
 (let ((?x3851 (sc_s 3)))
 (let ((?x9316 (bvadd (_ bv63 6) ?x3851)))
 (let ((?x6052 (stack_s x_CALLVALUE 3 ?x9316)))
 (let (($x8780 (exc_halt_s 1)))
 (let (($x2261 (or $x8780 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 1)))) (_ bv0 1))))))
 (let (($x10052 (exc_halt_s 2)))
 (let (($x8670 (= $x10052 $x2261)))
 (let (($x6869 (forall ((w (_ BitVec 256)) )(let ((?x10032 (storage_s x_CALLVALUE 1 w)))
 (let ((?x3343 (storage_s x_CALLVALUE 2 w)))
 (= ?x3343 ?x10032))))
 ))
 (let (($x10047 (forall ((n (_ BitVec 6)) )(let ((?x154 (sc_s 1)))
 (let (($x2938 (bvsle ?x154 n)))
 (or $x2938 (= (stack_s x_CALLVALUE 2 n) (stack_s x_CALLVALUE 1 n))))))
 ))
 (let ((?x2272 (sc_s 2)))
 (let (($x7159 (= ?x2272 (bvadd (_ bv1 6) (sc_s 1)))))
 (let (($x426 (= $x8780 (or $x57 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x72))) (_ bv0 1)))))))
 (let (($x3280 (forall ((w (_ BitVec 256)) )(let ((?x7772 (storage_s x_CALLVALUE 0 w)))
 (let ((?x10032 (storage_s x_CALLVALUE 1 w)))
 (= ?x10032 ?x7772))))
 ))
 (let (($x9380 (forall ((n (_ BitVec 6)) )(let ((?x72 (sc_s 0)))
 (let (($x1781 (bvsle ?x72 n)))
 (or $x1781 (= (stack_s x_CALLVALUE 1 n) (stack_s x_CALLVALUE 0 n))))))
 ))
 (let ((?x154 (sc_s 1)))
 (let (($x4355 (= ?x154 (bvadd (_ bv1 6) ?x72))))
 (let (($x7383 (forall ((w (_ BitVec 256)) )(let ((?x7772 (storage_s x_CALLVALUE 0 w)))
 (= ?x7772 (_ bv0 256))))
 ))
 (let (($x10976 (= ?x7930 0)))
 (let (($x7103 (not $x57)))
 (let (($x4825 (= ?x72 (_ bv0 6))))
 (and $x4825 $x7103 $x10976 $x7383 (= (stack_s x_CALLVALUE 1 ?x72) (_ bv0 256)) (= (used_gas_s x_CALLVALUE 1) (+ 3 ?x7930)) $x4355 $x9380 $x3280 $x426 (= (stack_s x_CALLVALUE 2 ?x154) x_CALLVALUE) (= (used_gas_s x_CALLVALUE 2) (+ 2 (used_gas_s x_CALLVALUE 1))) $x7159 $x10047 $x6869 $x8670 (= ?x6052 (ite $x6619 (_ bv0 256) (_ bv1 256))) (= (used_gas_s x_CALLVALUE 3) (+ 3 (used_gas_s x_CALLVALUE 2))) (= ?x3851 (bvadd (_ bv63 6) ?x2272)) $x7117 $x10389 $x3392 (= (stack_s x_CALLVALUE 4 (bvadd (_ bv63 6) ?x9433)) ?x6203) (= (used_gas_s x_CALLVALUE 4) (+ 3 (used_gas_s x_CALLVALUE 3))) (= ?x9433 ?x3851) $x4401 $x645 $x10193 (= (stack_t x_CALLVALUE 1 ?x63) x_CALLVALUE) (= (used_gas_t x_CALLVALUE 1) (+ 2 ?x2850)) $x5757 $x11273 $x6443 $x11104 (= (stack_t x_CALLVALUE 2 (bvadd (_ bv63 6) ?x6718)) ?x414) (= (used_gas_t x_CALLVALUE 2) (+ 3 (used_gas_t x_CALLVALUE 1))) (= ?x6718 ?x2707) $x9073 $x2093 $x2205 $x73 $x6019 $x58 $x939 $x6992 (not (and $x4221 $x11306 $x5384 $x11060))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
 )
(check-sat)
(get-proof)