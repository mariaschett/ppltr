; DUP1 PUSH 0x01 ISZERO DUP2 => DUP1 PUSH 0x00 DUP3
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
 (exists ((x_0 (_ BitVec 256)) )(let (($x8447 (forall ((w (_ BitVec 256)) )(let ((?x8452 (storage_t x_0 3 w)))
 (let ((?x8446 (storage_s x_0 4 w)))
 (= ?x8446 ?x8452))))
 ))
 (let (($x9995 (exc_halt_t 3)))
 (let (($x64 (exc_halt_s 4)))
 (let (($x8592 (= $x64 $x9995)))
 (let (($x7924 (forall ((n (_ BitVec 6)) )(let ((?x2012 (sc_t 3)))
 (let (($x9347 (bvsle ?x2012 n)))
 (let ((?x8438 (stack_t x_0 3 n)))
 (let ((?x8439 (stack_s x_0 4 n)))
 (let (($x8440 (= ?x8439 ?x8438)))
 (or $x8440 $x9347)))))))
 ))
 (let ((?x2012 (sc_t 3)))
 (let ((?x4305 (sc_s 4)))
 (let (($x8579 (= ?x4305 ?x2012)))
 (let ((?x4406 (used_gas_t x_0 0)))
 (let ((?x4407 (used_gas_s x_0 0)))
 (let (($x8442 (= ?x4407 ?x4406)))
 (let (($x4247 (forall ((w (_ BitVec 256)) )(let ((?x8448 (storage_t x_0 0 w)))
 (let ((?x8453 (storage_s x_0 0 w)))
 (= ?x8453 ?x8448))))
 ))
 (let (($x56 (exc_halt_t 0)))
 (let (($x57 (exc_halt_s 0)))
 (let (($x58 (= $x57 $x56)))
 (let (($x8197 (forall ((n (_ BitVec 6)) )(let ((?x8427 (stack_t x_0 0 n)))
 (let ((?x8426 (stack_s x_0 0 n)))
 (let (($x8428 (= ?x8426 ?x8427)))
 (let ((?x63 (sc_t 0)))
 (let (($x3168 (bvsle ?x63 n)))
 (or $x3168 $x8428)))))))
 ))
 (let ((?x63 (sc_t 0)))
 (let ((?x72 (sc_s 0)))
 (let (($x73 (= ?x72 ?x63)))
 (let (($x9360 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_t 2)))) (_ bv0 1)))))
 (let (($x903 (exc_halt_t 2)))
 (let (($x8184 (forall ((w (_ BitVec 256)) )(let ((?x3191 (storage_t x_0 2 w)))
 (let ((?x8452 (storage_t x_0 3 w)))
 (= ?x8452 ?x3191))))
 ))
 (let (($x8179 (forall ((n (_ BitVec 6)) )(or (bvsle (bvadd (_ bv61 6) (sc_t 2)) n) (= (stack_t x_0 3 n) (stack_t x_0 2 n))))
 ))
 (let (($x9381 (= ?x2012 (bvadd (_ bv1 6) (sc_t 2)))))
 (let (($x8162 (= (stack_t x_0 3 (bvadd (_ bv63 6) (sc_t 2))) (stack_t x_0 2 (bvadd (_ bv63 6) (sc_t 2))))))
 (let (($x8171 (= (stack_t x_0 3 (bvadd (_ bv62 6) (sc_t 2))) (stack_t x_0 2 (bvadd (_ bv62 6) (sc_t 2))))))
 (let ((?x4056 (sc_t 2)))
 (let ((?x8144 (bvadd (_ bv61 6) ?x4056)))
 (let ((?x8142 (stack_t x_0 2 ?x8144)))
 (let (($x1920 (exc_halt_t 1)))
 (let (($x9375 (or $x1920 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_t 1)))) (_ bv0 1))))))
 (let (($x9136 (= $x903 $x9375)))
 (let (($x8136 (forall ((w (_ BitVec 256)) )(let ((?x8396 (storage_t x_0 1 w)))
 (let ((?x3191 (storage_t x_0 2 w)))
 (= ?x3191 ?x8396))))
 ))
 (let (($x8135 (forall ((n (_ BitVec 6)) )(let ((?x4023 (sc_t 1)))
 (let (($x9379 (bvsle ?x4023 n)))
 (or (= (stack_t x_0 2 n) (stack_t x_0 1 n)) $x9379))))
 ))
 (let (($x9397 (= ?x4056 (bvadd (_ bv1 6) (sc_t 1)))))
 (let (($x9390 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x63))) (_ bv0 1)))))
 (let (($x7802 (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) ?x63)))))
 (let (($x8114 (forall ((w (_ BitVec 256)) )(let ((?x8448 (storage_t x_0 0 w)))
 (let ((?x8396 (storage_t x_0 1 w)))
 (= ?x8396 ?x8448))))
 ))
 (let (($x8112 (forall ((n (_ BitVec 6)) )(let ((?x63 (sc_t 0)))
 (let ((?x8804 (bvadd (_ bv63 6) ?x63)))
 (let (($x7696 (bvsle ?x8804 n)))
 (or $x7696 (= (stack_t x_0 1 n) (stack_t x_0 0 n)))))))
 ))
 (let ((?x4023 (sc_t 1)))
 (let (($x9527 (= ?x4023 (bvadd (_ bv1 6) ?x63))))
 (let ((?x8804 (bvadd (_ bv63 6) ?x63)))
 (let ((?x8105 (stack_t x_0 0 ?x8804)))
 (let (($x9460 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 3)))) (_ bv0 1)))))
 (let (($x292 (exc_halt_s 3)))
 (let (($x8096 (forall ((w (_ BitVec 256)) )(let ((?x8377 (storage_s x_0 3 w)))
 (let ((?x8446 (storage_s x_0 4 w)))
 (= ?x8446 ?x8377))))
 ))
 (let (($x8095 (forall ((n (_ BitVec 6)) )(or (bvsle (bvadd (_ bv62 6) (sc_s 3)) n) (= (stack_s x_0 4 n) (stack_s x_0 3 n))))
 ))
 (let (($x9465 (= ?x4305 (bvadd (_ bv1 6) (sc_s 3)))))
 (let ((?x275 (sc_s 3)))
 (let ((?x8728 (bvadd (_ bv63 6) ?x275)))
 (let ((?x8057 (stack_s x_0 3 ?x8728)))
 (let ((?x8073 (bvadd (_ bv62 6) ?x275)))
 (let ((?x8074 (stack_s x_0 3 ?x8073)))
 (let (($x8763 (= $x292 (or (exc_halt_s 2) (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_s 2))))))))
 (let (($x8069 (forall ((w (_ BitVec 256)) )(let ((?x8342 (storage_s x_0 2 w)))
 (let ((?x8377 (storage_s x_0 3 w)))
 (= ?x8377 ?x8342))))
 ))
 (let (($x8066 (forall ((n (_ BitVec 6)) )(let ((?x218 (sc_s 2)))
 (let ((?x9500 (bvadd (_ bv63 6) ?x218)))
 (let (($x8761 (bvsle ?x9500 n)))
 (or (= (stack_s x_0 3 n) (stack_s x_0 2 n)) $x8761)))))
 ))
 (let ((?x8046 (ite (= (stack_s x_0 2 (bvadd (_ bv63 6) (sc_s 2))) (_ bv0 256)) (_ bv1 256) (_ bv0 256))))
 (let (($x189 (exc_halt_s 1)))
 (let (($x8038 (or $x189 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 1)))) (_ bv0 1))))))
 (let (($x247 (exc_halt_s 2)))
 (let (($x8026 (forall ((w (_ BitVec 256)) )(let ((?x8323 (storage_s x_0 1 w)))
 (let ((?x8342 (storage_s x_0 2 w)))
 (= ?x8342 ?x8323))))
 ))
 (let (($x8024 (forall ((n (_ BitVec 6)) )(or (= (stack_s x_0 2 n) (stack_s x_0 1 n)) (bvsle (sc_s 1) n)))
 ))
 (let (($x9503 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x72))) (_ bv0 1)))))
 (let (($x8017 (forall ((w (_ BitVec 256)) )(let ((?x8453 (storage_s x_0 0 w)))
 (let ((?x8323 (storage_s x_0 1 w)))
 (= ?x8323 ?x8453))))
 ))
 (let (($x8016 (forall ((n (_ BitVec 6)) )(or (bvsle (bvadd (_ bv63 6) (sc_s 0)) n) (= (stack_s x_0 1 n) (stack_s x_0 0 n))))
 ))
 (let ((?x154 (sc_s 1)))
 (let (($x8922 (= ?x154 (bvadd (_ bv1 6) ?x72))))
 (let ((?x7981 (bvadd (_ bv63 6) ?x72)))
 (let ((?x7983 (stack_s x_0 0 ?x7981)))
 (let (($x7934 (forall ((w (_ BitVec 256)) )(let ((?x8453 (storage_s x_0 0 w)))
 (= ?x8453 (_ bv0 256))))
 ))
 (let (($x7972 (= ?x4407 0)))
 (let (($x5037 (not $x57)))
 (let (($x7966 (= (stack_s x_0 0 (_ bv0 6)) x_0)))
 (let (($x3591 (= ?x72 (_ bv1 6))))
 (and $x3591 $x7966 $x5037 $x7972 $x7934 (= (stack_s x_0 1 (bvadd (_ bv63 6) ?x154)) ?x7983) (= (stack_s x_0 1 ?x7981) ?x7983) (= (used_gas_s x_0 1) (+ 3 ?x4407)) $x8922 $x8016 $x8017 (= $x189 (or $x57 (not (bvsle (_ bv0 6) ?x7981)) $x9503)) (= (stack_s x_0 2 ?x154) (_ bv1 256)) (= (used_gas_s x_0 2) (+ 3 (used_gas_s x_0 1))) (= (sc_s 2) (bvadd (_ bv1 6) ?x154)) $x8024 $x8026 (= $x247 $x8038) (= ?x8057 ?x8046) (= (used_gas_s x_0 3) (+ 3 (used_gas_s x_0 2))) (= ?x275 (sc_s 2)) $x8066 $x8069 $x8763 (= (stack_s x_0 4 (bvadd (_ bv63 6) ?x4305)) ?x8074) (= (stack_s x_0 4 ?x8073) ?x8074) (= (stack_s x_0 4 ?x8728) ?x8057) (= (used_gas_s x_0 4) (+ 3 (used_gas_s x_0 3))) $x9465 $x8095 $x8096 (= $x64 (or $x292 (not (bvsle (_ bv0 6) ?x8073)) $x9460)) (= (stack_t x_0 1 (bvadd (_ bv63 6) ?x4023)) ?x8105) (= (stack_t x_0 1 ?x8804) ?x8105) (= (used_gas_t x_0 1) (+ 3 ?x4406)) $x9527 $x8112 $x8114 (= $x1920 (or $x56 $x7802 $x9390)) (= (stack_t x_0 2 ?x4023) (_ bv0 256)) (= (used_gas_t x_0 2) (+ 3 (used_gas_t x_0 1))) $x9397 $x8135 $x8136 $x9136 (= (stack_t x_0 3 (bvadd (_ bv63 6) ?x2012)) ?x8142) (= (stack_t x_0 3 ?x8144) ?x8142) $x8171 $x8162 (= (used_gas_t x_0 3) (+ 3 (used_gas_t x_0 2))) $x9381 $x8179 $x8184 (= $x9995 (or $x903 (not (bvsle (_ bv0 6) ?x8144)) $x9360)) $x73 $x8197 $x58 $x4247 $x8442 (not (and $x8579 $x7924 $x8592 $x8447))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
 )
(check-sat)
(get-proof)
