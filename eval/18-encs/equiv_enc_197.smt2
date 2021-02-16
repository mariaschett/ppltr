; DUP2 DUP3 DUP4 SWAP3 DUP2 SWAP1 => DUP2 DUP1 DUP4 DUP5 SWAP4
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
 (exists ((x_0 (_ BitVec 256)) (x_1 (_ BitVec 256)) )(let (($x7245 (forall ((w (_ BitVec 256)) )(let ((?x6085 (storage_t x_0 x_1 5 w)))
 (let ((?x5501 (storage_s x_0 x_1 6 w)))
 (= ?x5501 ?x6085))))
 ))
 (let (($x886 (exc_halt_t 5)))
 (let (($x772 (exc_halt_s 6)))
 (let (($x889 (= $x772 $x886)))
 (let (($x5638 (forall ((n (_ BitVec 6)) )(let ((?x919 (sc_t 5)))
 (let (($x2999 (bvsle ?x919 n)))
 (let ((?x8180 (stack_t x_0 x_1 5 n)))
 (let ((?x3359 (stack_s x_0 x_1 6 n)))
 (let (($x2978 (= ?x3359 ?x8180)))
 (or $x2978 $x2999)))))))
 ))
 (let ((?x919 (sc_t 5)))
 (let ((?x926 (sc_s 6)))
 (let (($x929 (= ?x926 ?x919)))
 (let ((?x4567 (used_gas_t x_0 x_1 0)))
 (let ((?x8379 (used_gas_s x_0 x_1 0)))
 (let (($x6535 (= ?x8379 ?x4567)))
 (let (($x2531 (forall ((w (_ BitVec 256)) )(let ((?x7229 (storage_t x_0 x_1 0 w)))
 (let ((?x7018 (storage_s x_0 x_1 0 w)))
 (= ?x7018 ?x7229))))
 ))
 (let (($x56 (exc_halt_t 0)))
 (let (($x57 (exc_halt_s 0)))
 (let (($x58 (= $x57 $x56)))
 (let (($x8031 (forall ((n (_ BitVec 6)) )(let ((?x3731 (stack_t x_0 x_1 0 n)))
 (let ((?x194 (stack_s x_0 x_1 0 n)))
 (let (($x5924 (= ?x194 ?x3731)))
 (let ((?x63 (sc_t 0)))
 (let (($x4577 (bvsle ?x63 n)))
 (or $x4577 $x5924)))))))
 ))
 (let ((?x63 (sc_t 0)))
 (let ((?x72 (sc_s 0)))
 (let (($x73 (= ?x72 ?x63)))
 (let (($x10014 (= $x886 (or (exc_halt_t 4) (not (bvsle (_ bv0 6) (bvadd (_ bv59 6) (sc_t 4))))))))
 (let (($x6669 (forall ((w (_ BitVec 256)) )(let ((?x5901 (storage_t x_0 x_1 4 w)))
 (let ((?x6085 (storage_t x_0 x_1 5 w)))
 (= ?x6085 ?x5901))))
 ))
 (let (($x2839 (forall ((n (_ BitVec 6)) )(let ((?x3757 (sc_t 4)))
 (let ((?x1157 (bvadd (_ bv59 6) ?x3757)))
 (let (($x4172 (bvsle ?x1157 n)))
 (or $x4172 (= (stack_t x_0 x_1 5 n) (stack_t x_0 x_1 4 n)))))))
 ))
 (let ((?x3757 (sc_t 4)))
 (let (($x3153 (= ?x919 ?x3757)))
 (let (($x3353 (= (used_gas_t x_0 x_1 5) (+ 3 (used_gas_t x_0 x_1 4)))))
 (let (($x9681 (= (stack_t x_0 x_1 5 (bvadd (_ bv62 6) ?x919)) (stack_t x_0 x_1 4 (bvadd (_ bv62 6) ?x3757)))))
 (let (($x5643 (= (stack_t x_0 x_1 5 (bvadd (_ bv61 6) ?x919)) (stack_t x_0 x_1 4 (bvadd (_ bv61 6) ?x3757)))))
 (let (($x7037 (= (stack_t x_0 x_1 5 (bvadd (_ bv60 6) ?x919)) (stack_t x_0 x_1 4 (bvadd (_ bv60 6) ?x3757)))))
 (let ((?x3175 (bvadd (_ bv63 6) ?x3757)))
 (let ((?x8372 (stack_t x_0 x_1 4 ?x3175)))
 (let (($x6957 (= (stack_t x_0 x_1 5 (bvadd (_ bv63 6) ?x919)) (stack_t x_0 x_1 4 (bvadd (_ bv59 6) ?x3757)))))
 (let (($x10336 (exc_halt_t 3)))
 (let (($x5820 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_t 3)))) (_ bv0 1)))))
 (let (($x3723 (exc_halt_t 4)))
 (let (($x7281 (forall ((w (_ BitVec 256)) )(let ((?x6272 (storage_t x_0 x_1 3 w)))
 (let ((?x5901 (storage_t x_0 x_1 4 w)))
 (= ?x5901 ?x6272))))
 ))
 (let (($x5413 (forall ((n (_ BitVec 6)) )(or (= (stack_t x_0 x_1 4 n) (stack_t x_0 x_1 3 n)) (bvsle (bvadd (_ bv59 6) (sc_t 3)) n)))
 ))
 (let (($x9314 (= ?x3757 (bvadd (_ bv1 6) (sc_t 3)))))
 (let ((?x7619 (used_gas_t x_0 x_1 4)))
 (let ((?x2012 (sc_t 3)))
 (let ((?x6604 (bvadd (_ bv63 6) ?x2012)))
 (let ((?x8221 (stack_t x_0 x_1 3 ?x6604)))
 (let (($x7923 (= (stack_t x_0 x_1 4 (bvadd (_ bv62 6) ?x2012)) (stack_t x_0 x_1 3 (bvadd (_ bv62 6) ?x2012)))))
 (let (($x4738 (= (stack_t x_0 x_1 4 (bvadd (_ bv61 6) ?x2012)) (stack_t x_0 x_1 3 (bvadd (_ bv61 6) ?x2012)))))
 (let (($x5726 (= (stack_t x_0 x_1 4 (bvadd (_ bv60 6) ?x2012)) (stack_t x_0 x_1 3 (bvadd (_ bv60 6) ?x2012)))))
 (let ((?x1886 (bvadd (_ bv59 6) ?x2012)))
 (let ((?x8536 (stack_t x_0 x_1 3 ?x1886)))
 (let (($x2609 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_t 2)))) (_ bv0 1)))))
 (let (($x903 (exc_halt_t 2)))
 (let (($x3603 (forall ((w (_ BitVec 256)) )(let ((?x2396 (storage_t x_0 x_1 2 w)))
 (let ((?x6272 (storage_t x_0 x_1 3 w)))
 (= ?x6272 ?x2396))))
 ))
 (let (($x9254 (forall ((n (_ BitVec 6)) )(or (= (stack_t x_0 x_1 3 n) (stack_t x_0 x_1 2 n)) (bvsle (bvadd (_ bv60 6) (sc_t 2)) n)))
 ))
 (let (($x9278 (= ?x2012 (bvadd (_ bv1 6) (sc_t 2)))))
 (let ((?x9009 (used_gas_t x_0 x_1 3)))
 (let ((?x4056 (sc_t 2)))
 (let ((?x1385 (bvadd (_ bv63 6) ?x4056)))
 (let ((?x6943 (stack_t x_0 x_1 2 ?x1385)))
 (let (($x7887 (= (stack_t x_0 x_1 3 (bvadd (_ bv62 6) ?x4056)) (stack_t x_0 x_1 2 (bvadd (_ bv62 6) ?x4056)))))
 (let (($x7988 (= (stack_t x_0 x_1 3 (bvadd (_ bv61 6) ?x4056)) (stack_t x_0 x_1 2 (bvadd (_ bv61 6) ?x4056)))))
 (let ((?x8749 (bvadd (_ bv60 6) ?x4056)))
 (let ((?x6237 (stack_t x_0 x_1 2 ?x8749)))
 (let (($x5064 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_t 1)))) (_ bv0 1)))))
 (let (($x1920 (exc_halt_t 1)))
 (let (($x4889 (= $x903 (or $x1920 $x5064 (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_t 1))))))))
 (let (($x6610 (forall ((w (_ BitVec 256)) )(let ((?x5522 (storage_t x_0 x_1 1 w)))
 (let ((?x2396 (storage_t x_0 x_1 2 w)))
 (= ?x2396 ?x5522))))
 ))
 (let (($x10162 (forall ((n (_ BitVec 6)) )(let ((?x4023 (sc_t 1)))
 (let ((?x5283 (bvadd (_ bv63 6) ?x4023)))
 (let (($x9110 (bvsle ?x5283 n)))
 (or (= (stack_t x_0 x_1 2 n) (stack_t x_0 x_1 1 n)) $x9110)))))
 ))
 (let (($x8778 (= ?x4056 (bvadd (_ bv1 6) (sc_t 1)))))
 (let ((?x1036 (used_gas_t x_0 x_1 2)))
 (let ((?x4023 (sc_t 1)))
 (let ((?x5283 (bvadd (_ bv63 6) ?x4023)))
 (let ((?x6504 (stack_t x_0 x_1 1 ?x5283)))
 (let (($x5637 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x63))) (_ bv0 1)))))
 (let (($x7644 (= $x1920 (or $x56 $x5637 (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) ?x63)))))))
 (let (($x6082 (forall ((w (_ BitVec 256)) )(let ((?x7229 (storage_t x_0 x_1 0 w)))
 (let ((?x5522 (storage_t x_0 x_1 1 w)))
 (= ?x5522 ?x7229))))
 ))
 (let (($x4091 (forall ((n (_ BitVec 6)) )(let ((?x63 (sc_t 0)))
 (let ((?x71 (bvadd (_ bv62 6) ?x63)))
 (let (($x989 (bvsle ?x71 n)))
 (or $x989 (= (stack_t x_0 x_1 1 n) (stack_t x_0 x_1 0 n)))))))
 ))
 (let (($x245 (= ?x4023 (bvadd (_ bv1 6) ?x63))))
 (let (($x5205 (= (stack_t x_0 x_1 1 (bvadd (_ bv63 6) ?x63)) (stack_t x_0 x_1 0 (bvadd (_ bv63 6) ?x63)))))
 (let ((?x71 (bvadd (_ bv62 6) ?x63)))
 (let ((?x5162 (stack_t x_0 x_1 0 ?x71)))
 (let (($x1553 (= $x772 (or (exc_halt_s 5) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 5))))))))
 (let (($x1554 (forall ((w (_ BitVec 256)) )(let ((?x1953 (storage_s x_0 x_1 5 w)))
 (let ((?x5501 (storage_s x_0 x_1 6 w)))
 (= ?x5501 ?x1953))))
 ))
 (let (($x1988 (forall ((n (_ BitVec 6)) )(let ((?x805 (sc_s 5)))
 (let ((?x8008 (bvadd (_ bv62 6) ?x805)))
 (let (($x7353 (bvsle ?x8008 n)))
 (or (= (stack_s x_0 x_1 6 n) (stack_s x_0 x_1 5 n)) $x7353)))))
 ))
 (let ((?x805 (sc_s 5)))
 (let (($x8462 (= ?x926 ?x805)))
 (let (($x1455 (= (used_gas_s x_0 x_1 6) (+ 3 (used_gas_s x_0 x_1 5)))))
 (let ((?x1107 (bvadd (_ bv63 6) ?x805)))
 (let ((?x7440 (stack_s x_0 x_1 5 ?x1107)))
 (let (($x5304 (= (stack_s x_0 x_1 6 (bvadd (_ bv63 6) ?x926)) (stack_s x_0 x_1 5 (bvadd (_ bv62 6) ?x805)))))
 (let (($x5976 (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 4))))))
 (let (($x504 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 4)))) (_ bv0 1)))))
 (let (($x64 (exc_halt_s 4)))
 (let (($x3979 (exc_halt_s 5)))
 (let (($x2931 (forall ((w (_ BitVec 256)) )(let ((?x5862 (storage_s x_0 x_1 4 w)))
 (let ((?x1953 (storage_s x_0 x_1 5 w)))
 (= ?x1953 ?x5862))))
 ))
 (let (($x5711 (forall ((n (_ BitVec 6)) )(let ((?x4305 (sc_s 4)))
 (let ((?x7954 (bvadd (_ bv62 6) ?x4305)))
 (let (($x5184 (bvsle ?x7954 n)))
 (or $x5184 (= (stack_s x_0 x_1 5 n) (stack_s x_0 x_1 4 n)))))))
 ))
 (let (($x1347 (= ?x805 (bvadd (_ bv1 6) (sc_s 4)))))
 (let ((?x9120 (used_gas_s x_0 x_1 5)))
 (let ((?x4305 (sc_s 4)))
 (let ((?x1489 (bvadd (_ bv63 6) ?x4305)))
 (let ((?x941 (stack_s x_0 x_1 4 ?x1489)))
 (let ((?x7954 (bvadd (_ bv62 6) ?x4305)))
 (let ((?x10146 (stack_s x_0 x_1 4 ?x7954)))
 (let (($x2479 (= $x64 (or (exc_halt_s 3) (not (bvsle (_ bv0 6) (bvadd (_ bv60 6) (sc_s 3))))))))
 (let (($x7551 (forall ((w (_ BitVec 256)) )(let ((?x7945 (storage_s x_0 x_1 3 w)))
 (let ((?x5862 (storage_s x_0 x_1 4 w)))
 (= ?x5862 ?x7945))))
 ))
 (let (($x2322 (forall ((n (_ BitVec 6)) )(or (= (stack_s x_0 x_1 4 n) (stack_s x_0 x_1 3 n)) (bvsle (bvadd (_ bv60 6) (sc_s 3)) n)))
 ))
 (let ((?x275 (sc_s 3)))
 (let (($x180 (= ?x4305 ?x275)))
 (let ((?x5532 (used_gas_s x_0 x_1 4)))
 (let (($x6758 (= (stack_s x_0 x_1 4 (bvadd (_ bv61 6) ?x4305)) (stack_s x_0 x_1 3 (bvadd (_ bv61 6) ?x275)))))
 (let ((?x403 (bvadd (_ bv63 6) ?x275)))
 (let ((?x4441 (stack_s x_0 x_1 3 ?x403)))
 (let (($x1818 (not (bvsle (_ bv0 6) (bvadd (_ bv60 6) (sc_s 2))))))
 (let (($x608 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 2)))) (_ bv0 1)))))
 (let (($x247 (exc_halt_s 2)))
 (let (($x292 (exc_halt_s 3)))
 (let (($x1931 (= $x292 (or $x247 $x608 $x1818))))
 (let (($x1603 (forall ((w (_ BitVec 256)) )(let ((?x223 (storage_s x_0 x_1 2 w)))
 (let ((?x7945 (storage_s x_0 x_1 3 w)))
 (= ?x7945 ?x223))))
 ))
 (let (($x5927 (forall ((n (_ BitVec 6)) )(let ((?x218 (sc_s 2)))
 (let ((?x10975 (bvadd (_ bv60 6) ?x218)))
 (let (($x9456 (bvsle ?x10975 n)))
 (or (= (stack_s x_0 x_1 3 n) (stack_s x_0 x_1 2 n)) $x9456)))))
 ))
 (let (($x5948 (= ?x275 (bvadd (_ bv1 6) (sc_s 2)))))
 (let ((?x9174 (used_gas_s x_0 x_1 3)))
 (let ((?x218 (sc_s 2)))
 (let ((?x712 (bvadd (_ bv63 6) ?x218)))
 (let ((?x7347 (stack_s x_0 x_1 2 ?x712)))
 (let (($x5623 (= (stack_s x_0 x_1 3 (bvadd (_ bv62 6) ?x218)) (stack_s x_0 x_1 2 (bvadd (_ bv62 6) ?x218)))))
 (let (($x5428 (= (stack_s x_0 x_1 3 (bvadd (_ bv61 6) ?x218)) (stack_s x_0 x_1 2 (bvadd (_ bv61 6) ?x218)))))
 (let ((?x10975 (bvadd (_ bv60 6) ?x218)))
 (let ((?x7643 (stack_s x_0 x_1 2 ?x10975)))
 (let (($x6305 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 1)))) (_ bv0 1)))))
 (let (($x3953 (not (bvsle (_ bv0 6) (bvadd (_ bv61 6) (sc_s 1))))))
 (let (($x189 (exc_halt_s 1)))
 (let (($x5248 (forall ((w (_ BitVec 256)) )(let ((?x86 (storage_s x_0 x_1 1 w)))
 (let ((?x223 (storage_s x_0 x_1 2 w)))
 (= ?x223 ?x86))))
 ))
 (let (($x8297 (forall ((n (_ BitVec 6)) )(let ((?x154 (sc_s 1)))
 (let ((?x2430 (bvadd (_ bv61 6) ?x154)))
 (let (($x3822 (bvsle ?x2430 n)))
 (or $x3822 (= (stack_s x_0 x_1 2 n) (stack_s x_0 x_1 1 n)))))))
 ))
 (let (($x2153 (= ?x218 (bvadd (_ bv1 6) (sc_s 1)))))
 (let ((?x7935 (used_gas_s x_0 x_1 2)))
 (let ((?x154 (sc_s 1)))
 (let ((?x1435 (bvadd (_ bv63 6) ?x154)))
 (let ((?x6093 (stack_s x_0 x_1 1 ?x1435)))
 (let (($x891 (= (stack_s x_0 x_1 2 (bvadd (_ bv62 6) ?x154)) (stack_s x_0 x_1 1 (bvadd (_ bv62 6) ?x154)))))
 (let ((?x2430 (bvadd (_ bv61 6) ?x154)))
 (let ((?x9147 (stack_s x_0 x_1 1 ?x2430)))
 (let (($x8045 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x72))) (_ bv0 1)))))
 (let (($x2666 (forall ((w (_ BitVec 256)) )(let ((?x7018 (storage_s x_0 x_1 0 w)))
 (let ((?x86 (storage_s x_0 x_1 1 w)))
 (= ?x86 ?x7018))))
 ))
 (let (($x7591 (forall ((n (_ BitVec 6)) )(or (= (stack_s x_0 x_1 1 n) (stack_s x_0 x_1 0 n)) (bvsle (bvadd (_ bv62 6) (sc_s 0)) n)))
 ))
 (let (($x3745 (= ?x154 (bvadd (_ bv1 6) ?x72))))
 (let (($x6700 (= (stack_s x_0 x_1 1 (bvadd (_ bv63 6) ?x72)) (stack_s x_0 x_1 0 (bvadd (_ bv63 6) ?x72)))))
 (let ((?x1302 (bvadd (_ bv62 6) ?x72)))
 (let ((?x8714 (stack_s x_0 x_1 0 ?x1302)))
 (let (($x3179 (forall ((w (_ BitVec 256)) )(let ((?x7018 (storage_s x_0 x_1 0 w)))
 (= ?x7018 (_ bv0 256))))
 ))
 (let (($x5823 (= ?x8379 0)))
 (let (($x5252 (not $x57)))
 (let (($x5865 (= (stack_s x_0 x_1 0 (_ bv1 6)) x_1)))
 (let (($x9459 (= (stack_s x_0 x_1 0 (_ bv0 6)) x_0)))
 (let (($x2064 (= ?x72 (_ bv2 6))))
 (and $x2064 $x9459 $x5865 $x5252 $x5823 $x3179 (= ?x6093 ?x8714) (= (stack_s x_0 x_1 1 ?x1302) ?x8714) $x6700 (= (used_gas_s x_0 x_1 1) (+ 3 ?x8379)) $x3745 $x7591 $x2666 (= $x189 (or $x57 $x8045 (not (bvsle (_ bv0 6) ?x1302)))) (= ?x7347 ?x9147) (= (stack_s x_0 x_1 2 ?x2430) ?x9147) $x891 (= (stack_s x_0 x_1 2 ?x1435) ?x6093) (= ?x7935 (+ 3 (used_gas_s x_0 x_1 1))) $x2153 $x8297 $x5248 (= $x247 (or $x189 $x3953 $x6305)) (= ?x4441 ?x7643) (= (stack_s x_0 x_1 3 ?x10975) ?x7643) $x5428 $x5623 (= (stack_s x_0 x_1 3 ?x712) ?x7347) (= ?x9174 (+ 3 ?x7935)) $x5948 $x5927 $x1603 $x1931 (= ?x941 (stack_s x_0 x_1 3 (bvadd (_ bv60 6) ?x275))) (= (stack_s x_0 x_1 4 (bvadd (_ bv60 6) ?x4305)) ?x4441) $x6758 (= ?x10146 (stack_s x_0 x_1 3 (bvadd (_ bv62 6) ?x275))) (= ?x5532 (+ 3 ?x9174)) $x180 $x2322 $x7551 $x2479 (= ?x7440 ?x10146) (= (stack_s x_0 x_1 5 ?x7954) ?x10146) (= (stack_s x_0 x_1 5 ?x1489) ?x941) (= ?x9120 (+ 3 ?x5532)) $x1347 $x5711 $x2931 (= $x3979 (or $x64 $x504 $x5976)) $x5304 (= (stack_s x_0 x_1 6 (bvadd (_ bv62 6) ?x926)) ?x7440) $x1455 $x8462 $x1988 $x1554 $x1553 (= ?x6504 ?x5162) (= (stack_t x_0 x_1 1 ?x71) ?x5162) $x5205 (= (used_gas_t x_0 x_1 1) (+ 3 ?x4567)) $x245 $x4091 $x6082 $x7644 (= ?x6943 ?x6504) (= (stack_t x_0 x_1 2 ?x5283) ?x6504) (= ?x1036 (+ 3 (used_gas_t x_0 x_1 1))) $x8778 $x10162 $x6610 $x4889 (= ?x8221 ?x6237) (= (stack_t x_0 x_1 3 ?x8749) ?x6237) $x7988 $x7887 (= (stack_t x_0 x_1 3 ?x1385) ?x6943) (= ?x9009 (+ 3 ?x1036)) $x9278 $x9254 $x3603 (= $x10336 (or $x903 $x2609 (not (bvsle (_ bv0 6) ?x8749)))) (= ?x8372 ?x8536) (= (stack_t x_0 x_1 4 ?x1886) ?x8536) $x5726 $x4738 $x7923 (= (stack_t x_0 x_1 4 ?x6604) ?x8221) (= ?x7619 (+ 3 ?x9009)) $x9314 $x5413 $x7281 (= $x3723 (or (not (bvsle (_ bv0 6) ?x1886)) $x5820 $x10336)) $x6957 (= (stack_t x_0 x_1 5 (bvadd (_ bv59 6) ?x919)) ?x8372) $x7037 $x5643 $x9681 $x3353 $x3153 $x2839 $x6669 $x10014 $x73 $x8031 $x58 $x2531 $x6535 (not (and $x929 $x5638 $x889 $x7245))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
 )
(check-sat)
(get-proof)