; TIMESTAMP PUSH cw_2 DUP2 SWAP1 SSTORE => TIMESTAMP PUSH cw_2 SSTORE TIMESTAMP
(set-option :produce-proofs true)
; 
(set-info :status unknown)
(declare-fun storage_t ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 256)) (_ BitVec 256))
(declare-fun w_2 () (_ BitVec 256))
(declare-fun storage_s ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 256)) (_ BitVec 256))
(declare-fun exc_halt_t (Int) Bool)
(declare-fun exc_halt_s (Int) Bool)
(declare-fun sc_t (Int) (_ BitVec 6))
(declare-fun stack_t ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 6)) (_ BitVec 256))
(declare-fun stack_s ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 6)) (_ BitVec 256))
(declare-fun sc_s (Int) (_ BitVec 6))
(declare-fun used_gas_t ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int) Int)
(declare-fun used_gas_s ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int) Int)
(assert
 (exists ((x_SSTORE_0 (_ BitVec 256)) (x_TIMESTAMP (_ BitVec 256)) )(let (($x7279 (forall ((w (_ BitVec 256)) )(let ((?x11450 (storage_t x_SSTORE_0 w_2 x_TIMESTAMP 4 w)))
 (let ((?x5290 (storage_s x_SSTORE_0 w_2 x_TIMESTAMP 5 w)))
 (= ?x5290 ?x11450))))
 ))
 (let (($x7854 (exc_halt_t 4)))
 (let (($x11317 (exc_halt_s 5)))
 (let (($x6710 (= $x11317 $x7854)))
 (let (($x3714 (forall ((n (_ BitVec 6)) )(let ((?x4818 (sc_t 4)))
 (let (($x8053 (bvsle ?x4818 n)))
 (let ((?x5283 (stack_t x_SSTORE_0 w_2 x_TIMESTAMP 4 n)))
 (let ((?x3788 (stack_s x_SSTORE_0 w_2 x_TIMESTAMP 5 n)))
 (let (($x7271 (= ?x3788 ?x5283)))
 (or $x7271 $x8053)))))))
 ))
 (let ((?x4818 (sc_t 4)))
 (let ((?x4319 (sc_s 5)))
 (let (($x1673 (= ?x4319 ?x4818)))
 (let ((?x9025 (used_gas_t x_SSTORE_0 w_2 x_TIMESTAMP 0)))
 (let ((?x7560 (used_gas_s x_SSTORE_0 w_2 x_TIMESTAMP 0)))
 (let (($x8965 (= ?x7560 ?x9025)))
 (let (($x9116 (forall ((w (_ BitVec 256)) )(let ((?x10161 (storage_t x_SSTORE_0 w_2 x_TIMESTAMP 0 w)))
 (let ((?x8696 (storage_s x_SSTORE_0 w_2 x_TIMESTAMP 0 w)))
 (= ?x8696 ?x10161))))
 ))
 (let (($x56 (exc_halt_t 0)))
 (let (($x57 (exc_halt_s 0)))
 (let (($x58 (= $x57 $x56)))
 (let (($x10976 (forall ((n (_ BitVec 6)) )(let ((?x10377 (stack_t x_SSTORE_0 w_2 x_TIMESTAMP 0 n)))
 (let ((?x8949 (stack_s x_SSTORE_0 w_2 x_TIMESTAMP 0 n)))
 (let (($x5004 (= ?x8949 ?x10377)))
 (let ((?x63 (sc_t 0)))
 (let (($x1948 (bvsle ?x63 n)))
 (or $x1948 $x5004)))))))
 ))
 (let ((?x63 (sc_t 0)))
 (let ((?x72 (sc_s 0)))
 (let (($x73 (= ?x72 ?x63)))
 (let (($x6783 (exc_halt_t 3)))
 (let (($x4543 (or $x6783 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_t 3)))) (_ bv0 1))))))
 (let (($x2679 (= $x7854 $x4543)))
 (let (($x7717 (forall ((w (_ BitVec 256)) )(let ((?x3185 (storage_t x_SSTORE_0 w_2 x_TIMESTAMP 3 w)))
 (let ((?x11450 (storage_t x_SSTORE_0 w_2 x_TIMESTAMP 4 w)))
 (= ?x11450 ?x3185))))
 ))
 (let (($x5386 (forall ((n (_ BitVec 6)) )(let ((?x6438 (sc_t 3)))
 (let (($x3188 (bvsle ?x6438 n)))
 (let ((?x11044 (stack_t x_SSTORE_0 w_2 x_TIMESTAMP 3 n)))
 (let ((?x5283 (stack_t x_SSTORE_0 w_2 x_TIMESTAMP 4 n)))
 (or (= ?x5283 ?x11044) $x3188))))))
 ))
 (let (($x11075 (= ?x4818 (bvadd (_ bv1 6) (sc_t 3)))))
 (let (($x172 (= (used_gas_t x_SSTORE_0 w_2 x_TIMESTAMP 4) (+ 2 (used_gas_t x_SSTORE_0 w_2 x_TIMESTAMP 3)))))
 (let (($x4421 (= $x6783 (or (exc_halt_t 2) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_t 2))))))))
 (let (($x8595 (forall ((n (_ BitVec 6)) )(let ((?x2153 (stack_t x_SSTORE_0 w_2 x_TIMESTAMP 2 n)))
 (let ((?x11044 (stack_t x_SSTORE_0 w_2 x_TIMESTAMP 3 n)))
 (or (bvsle (bvadd (_ bv62 6) (sc_t 2)) n) (= ?x11044 ?x2153)))))
 ))
 (let (($x6564 (= (stack_t x_SSTORE_0 w_2 x_TIMESTAMP 2 (bvadd (_ bv62 6) (sc_t 2))) (_ bv0 256))))
 (let ((?x3163 (storage_t x_SSTORE_0 w_2 x_TIMESTAMP 2 (stack_t x_SSTORE_0 w_2 x_TIMESTAMP 2 (bvadd (_ bv63 6) (sc_t 2))))))
 (let ((?x2732 (used_gas_t x_SSTORE_0 w_2 x_TIMESTAMP 2)))
 (let ((?x10269 (+ ?x2732 (ite (= ?x3163 (_ bv0 256)) (ite $x6564 5000 20000) (ite $x6564 (- 10000) 5000)))))
 (let ((?x5056 (used_gas_t x_SSTORE_0 w_2 x_TIMESTAMP 3)))
 (let (($x9002 (forall ((w (_ BitVec 256)) )(let ((?x5190 (storage_t x_SSTORE_0 w_2 x_TIMESTAMP 2 w)))
 (let ((?x1955 (ite (= w (stack_t x_SSTORE_0 w_2 x_TIMESTAMP 2 (bvadd (_ bv63 6) (sc_t 2)))) (stack_t x_SSTORE_0 w_2 x_TIMESTAMP 2 (bvadd (_ bv62 6) (sc_t 2))) ?x5190)))
 (let ((?x3185 (storage_t x_SSTORE_0 w_2 x_TIMESTAMP 3 w)))
 (= ?x3185 ?x1955)))))
 ))
 (let (($x2331 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_t 1)))) (_ bv0 1)))))
 (let (($x8377 (exc_halt_t 1)))
 (let (($x2163 (exc_halt_t 2)))
 (let (($x3076 (= $x2163 (or $x8377 $x2331))))
 (let (($x3000 (forall ((w (_ BitVec 256)) )(let ((?x4385 (storage_t x_SSTORE_0 w_2 x_TIMESTAMP 1 w)))
 (let ((?x5190 (storage_t x_SSTORE_0 w_2 x_TIMESTAMP 2 w)))
 (= ?x5190 ?x4385))))
 ))
 (let (($x6548 (forall ((n (_ BitVec 6)) )(let ((?x1703 (stack_t x_SSTORE_0 w_2 x_TIMESTAMP 1 n)))
 (let ((?x2153 (stack_t x_SSTORE_0 w_2 x_TIMESTAMP 2 n)))
 (let ((?x7154 (sc_t 1)))
 (let (($x2342 (bvsle ?x7154 n)))
 (or $x2342 (= ?x2153 ?x1703)))))))
 ))
 (let ((?x2714 (sc_t 2)))
 (let (($x8154 (= ?x2714 (bvadd (_ bv1 6) (sc_t 1)))))
 (let (($x3863 (= $x8377 (or $x56 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x63))) (_ bv0 1)))))))
 (let (($x1851 (forall ((w (_ BitVec 256)) )(let ((?x10161 (storage_t x_SSTORE_0 w_2 x_TIMESTAMP 0 w)))
 (let ((?x4385 (storage_t x_SSTORE_0 w_2 x_TIMESTAMP 1 w)))
 (= ?x4385 ?x10161))))
 ))
 (let (($x10297 (forall ((n (_ BitVec 6)) )(let ((?x10377 (stack_t x_SSTORE_0 w_2 x_TIMESTAMP 0 n)))
 (let ((?x1703 (stack_t x_SSTORE_0 w_2 x_TIMESTAMP 1 n)))
 (let ((?x63 (sc_t 0)))
 (let (($x1948 (bvsle ?x63 n)))
 (or $x1948 (= ?x1703 ?x10377)))))))
 ))
 (let ((?x7154 (sc_t 1)))
 (let (($x10065 (= ?x7154 (bvadd (_ bv1 6) ?x63))))
 (let (($x3821 (= $x11317 (or (exc_halt_s 4) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 4))))))))
 (let (($x5578 (forall ((n (_ BitVec 6)) )(let ((?x1251 (stack_s x_SSTORE_0 w_2 x_TIMESTAMP 4 n)))
 (let ((?x3788 (stack_s x_SSTORE_0 w_2 x_TIMESTAMP 5 n)))
 (or (= ?x3788 ?x1251) (bvsle (bvadd (_ bv62 6) (sc_s 4)) n)))))
 ))
 (let ((?x4305 (sc_s 4)))
 (let ((?x2417 (bvadd (_ bv62 6) ?x4305)))
 (let ((?x6625 (stack_s x_SSTORE_0 w_2 x_TIMESTAMP 4 ?x2417)))
 (let (($x7345 (= ?x6625 (_ bv0 256))))
 (let ((?x6098 (bvadd (_ bv63 6) ?x4305)))
 (let ((?x129 (stack_s x_SSTORE_0 w_2 x_TIMESTAMP 4 ?x6098)))
 (let ((?x5195 (ite (= (storage_s x_SSTORE_0 w_2 x_TIMESTAMP 4 ?x129) (_ bv0 256)) (ite $x7345 5000 20000) (ite $x7345 (- 10000) 5000))))
 (let ((?x10155 (used_gas_s x_SSTORE_0 w_2 x_TIMESTAMP 4)))
 (let (($x7314 (forall ((w (_ BitVec 256)) )(let ((?x10000 (storage_s x_SSTORE_0 w_2 x_TIMESTAMP 4 w)))
 (let (($x1896 (= w (stack_s x_SSTORE_0 w_2 x_TIMESTAMP 4 (bvadd (_ bv63 6) (sc_s 4))))))
 (let ((?x9038 (ite $x1896 (stack_s x_SSTORE_0 w_2 x_TIMESTAMP 4 (bvadd (_ bv62 6) (sc_s 4))) ?x10000)))
 (let ((?x5290 (storage_s x_SSTORE_0 w_2 x_TIMESTAMP 5 w)))
 (= ?x5290 ?x9038))))))
 ))
 (let (($x7172 (exc_halt_s 4)))
 (let (($x9358 (= $x7172 (or (exc_halt_s 3) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 3))))))))
 (let (($x6499 (forall ((w (_ BitVec 256)) )(let ((?x798 (storage_s x_SSTORE_0 w_2 x_TIMESTAMP 3 w)))
 (let ((?x10000 (storage_s x_SSTORE_0 w_2 x_TIMESTAMP 4 w)))
 (= ?x10000 ?x798))))
 ))
 (let (($x4578 (forall ((n (_ BitVec 6)) )(let ((?x11386 (stack_s x_SSTORE_0 w_2 x_TIMESTAMP 3 n)))
 (let ((?x1251 (stack_s x_SSTORE_0 w_2 x_TIMESTAMP 4 n)))
 (let ((?x275 (sc_s 3)))
 (let ((?x2606 (bvadd (_ bv62 6) ?x275)))
 (let (($x4179 (bvsle ?x2606 n)))
 (or $x4179 (= ?x1251 ?x11386))))))))
 ))
 (let ((?x275 (sc_s 3)))
 (let (($x1480 (= ?x4305 ?x275)))
 (let (($x11463 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 2)))) (_ bv0 1)))))
 (let (($x247 (exc_halt_s 2)))
 (let (($x292 (exc_halt_s 3)))
 (let (($x1451 (forall ((w (_ BitVec 256)) )(let ((?x3256 (storage_s x_SSTORE_0 w_2 x_TIMESTAMP 2 w)))
 (let ((?x798 (storage_s x_SSTORE_0 w_2 x_TIMESTAMP 3 w)))
 (= ?x798 ?x3256))))
 ))
 (let (($x5502 (forall ((n (_ BitVec 6)) )(let ((?x3019 (stack_s x_SSTORE_0 w_2 x_TIMESTAMP 2 n)))
 (let ((?x11386 (stack_s x_SSTORE_0 w_2 x_TIMESTAMP 3 n)))
 (or (bvsle (bvadd (_ bv62 6) (sc_s 2)) n) (= ?x11386 ?x3019)))))
 ))
 (let (($x5593 (= ?x275 (bvadd (_ bv1 6) (sc_s 2)))))
 (let ((?x3277 (used_gas_s x_SSTORE_0 w_2 x_TIMESTAMP 3)))
 (let (($x6561 (= (stack_s x_SSTORE_0 w_2 x_TIMESTAMP 3 (bvadd (_ bv63 6) (sc_s 2))) (stack_s x_SSTORE_0 w_2 x_TIMESTAMP 2 (bvadd (_ bv63 6) (sc_s 2))))))
 (let ((?x218 (sc_s 2)))
 (let ((?x1112 (bvadd (_ bv62 6) ?x218)))
 (let ((?x8574 (stack_s x_SSTORE_0 w_2 x_TIMESTAMP 2 ?x1112)))
 (let (($x189 (exc_halt_s 1)))
 (let (($x793 (or $x189 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 1)))) (_ bv0 1))))))
 (let (($x1304 (= $x247 $x793)))
 (let (($x5617 (forall ((w (_ BitVec 256)) )(let ((?x2017 (storage_s x_SSTORE_0 w_2 x_TIMESTAMP 1 w)))
 (let ((?x3256 (storage_s x_SSTORE_0 w_2 x_TIMESTAMP 2 w)))
 (= ?x3256 ?x2017))))
 ))
 (let (($x8279 (forall ((n (_ BitVec 6)) )(let ((?x154 (sc_s 1)))
 (let (($x8231 (bvsle ?x154 n)))
 (let ((?x143 (stack_s x_SSTORE_0 w_2 x_TIMESTAMP 1 n)))
 (let ((?x3019 (stack_s x_SSTORE_0 w_2 x_TIMESTAMP 2 n)))
 (or (= ?x3019 ?x143) $x8231))))))
 ))
 (let (($x1061 (= ?x218 (bvadd (_ bv1 6) (sc_s 1)))))
 (let ((?x10521 (used_gas_s x_SSTORE_0 w_2 x_TIMESTAMP 2)))
 (let (($x2601 (= $x189 (or $x57 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x72))) (_ bv0 1)))))))
 (let (($x6738 (forall ((w (_ BitVec 256)) )(let ((?x8696 (storage_s x_SSTORE_0 w_2 x_TIMESTAMP 0 w)))
 (let ((?x2017 (storage_s x_SSTORE_0 w_2 x_TIMESTAMP 1 w)))
 (= ?x2017 ?x8696))))
 ))
 (let (($x4213 (forall ((n (_ BitVec 6)) )(let ((?x8949 (stack_s x_SSTORE_0 w_2 x_TIMESTAMP 0 n)))
 (let ((?x143 (stack_s x_SSTORE_0 w_2 x_TIMESTAMP 1 n)))
 (let ((?x72 (sc_s 0)))
 (let (($x6679 (bvsle ?x72 n)))
 (or $x6679 (= ?x143 ?x8949)))))))
 ))
 (let ((?x154 (sc_s 1)))
 (let (($x5696 (= ?x154 (bvadd (_ bv1 6) ?x72))))
 (let (($x6964 (forall ((w (_ BitVec 256)) )(let (($x1896 (= w (stack_s x_SSTORE_0 w_2 x_TIMESTAMP 4 (bvadd (_ bv63 6) (sc_s 4))))))
 (let ((?x8696 (storage_s x_SSTORE_0 w_2 x_TIMESTAMP 0 w)))
 (= ?x8696 (ite $x1896 x_SSTORE_0 (_ bv0 256))))))
 ))
 (let (($x7935 (= ?x7560 0)))
 (let (($x7631 (not $x57)))
 (let (($x1074 (= ?x72 (_ bv0 6))))
 (and $x1074 $x7631 $x7935 $x6964 (= (stack_s x_SSTORE_0 w_2 x_TIMESTAMP 1 ?x72) x_TIMESTAMP) (= (used_gas_s x_SSTORE_0 w_2 x_TIMESTAMP 1) (+ 2 ?x7560)) $x5696 $x4213 $x6738 $x2601 (= (stack_s x_SSTORE_0 w_2 x_TIMESTAMP 2 ?x154) w_2) (= ?x10521 (+ 3 (used_gas_s x_SSTORE_0 w_2 x_TIMESTAMP 1))) $x1061 $x8279 $x5617 $x1304 (= (stack_s x_SSTORE_0 w_2 x_TIMESTAMP 3 (bvadd (_ bv63 6) ?x275)) ?x8574) (= (stack_s x_SSTORE_0 w_2 x_TIMESTAMP 3 ?x1112) ?x8574) $x6561 (= ?x3277 (+ 3 ?x10521)) $x5593 $x5502 $x1451 (= $x292 (or $x247 (not (bvsle (_ bv0 6) ?x1112)) $x11463)) (= ?x129 (stack_s x_SSTORE_0 w_2 x_TIMESTAMP 3 (bvadd (_ bv62 6) ?x275))) (= ?x6625 (stack_s x_SSTORE_0 w_2 x_TIMESTAMP 3 (bvadd (_ bv63 6) ?x275))) (= ?x10155 (+ 3 ?x3277)) $x1480 $x4578 $x6499 $x9358 $x7314 (= (used_gas_s x_SSTORE_0 w_2 x_TIMESTAMP 5) (+ ?x10155 ?x5195)) (= ?x4319 ?x2417) $x5578 $x3821 (= (stack_t x_SSTORE_0 w_2 x_TIMESTAMP 1 ?x63) x_TIMESTAMP) (= (used_gas_t x_SSTORE_0 w_2 x_TIMESTAMP 1) (+ 2 ?x9025)) $x10065 $x10297 $x1851 $x3863 (= (stack_t x_SSTORE_0 w_2 x_TIMESTAMP 2 ?x7154) w_2) (= ?x2732 (+ 3 (used_gas_t x_SSTORE_0 w_2 x_TIMESTAMP 1))) $x8154 $x6548 $x3000 $x3076 $x9002 (= ?x5056 ?x10269) (= (sc_t 3) (bvadd (_ bv62 6) ?x2714)) $x8595 $x4421 (= (stack_t x_SSTORE_0 w_2 x_TIMESTAMP 4 (sc_t 3)) x_TIMESTAMP) $x172 $x11075 $x5386 $x7717 $x2679 $x73 $x10976 $x58 $x9116 $x8965 (not (and $x1673 $x3714 $x6710 $x7279)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
 )
(check-sat)
(get-proof)