; ISZERO ISZERO DUP2 ISZERO XOR ISZERO => ISZERO DUP2 ISZERO XOR
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
 (exists ((x_0 (_ BitVec 256)) (x_1 (_ BitVec 256)) )(let (($x9543 (forall ((w (_ BitVec 256)) )(let ((?x260 (storage_t x_0 x_1 4 w)))
 (let ((?x8326 (storage_s x_0 x_1 6 w)))
 (= ?x8326 ?x260))))
 ))
 (let (($x3723 (exc_halt_t 4)))
 (let (($x772 (exc_halt_s 6)))
 (let (($x4256 (= $x772 $x3723)))
 (let (($x7034 (forall ((n (_ BitVec 6)) )(let ((?x3757 (sc_t 4)))
 (let (($x7992 (bvsle ?x3757 n)))
 (let ((?x1174 (stack_t x_0 x_1 4 n)))
 (let ((?x2447 (stack_s x_0 x_1 6 n)))
 (let (($x7913 (= ?x2447 ?x1174)))
 (or $x7913 $x7992)))))))
 ))
 (let ((?x3757 (sc_t 4)))
 (let ((?x926 (sc_s 6)))
 (let (($x7941 (= ?x926 ?x3757)))
 (let ((?x10825 (used_gas_t x_0 x_1 0)))
 (let ((?x1753 (used_gas_s x_0 x_1 0)))
 (let (($x8355 (= ?x1753 ?x10825)))
 (let (($x6718 (forall ((w (_ BitVec 256)) )(let ((?x5106 (storage_t x_0 x_1 0 w)))
 (let ((?x5011 (storage_s x_0 x_1 0 w)))
 (= ?x5011 ?x5106))))
 ))
 (let (($x56 (exc_halt_t 0)))
 (let (($x57 (exc_halt_s 0)))
 (let (($x58 (= $x57 $x56)))
 (let (($x469 (forall ((n (_ BitVec 6)) )(let ((?x9058 (stack_t x_0 x_1 0 n)))
 (let ((?x8800 (stack_s x_0 x_1 0 n)))
 (let (($x8399 (= ?x8800 ?x9058)))
 (let ((?x63 (sc_t 0)))
 (let (($x4379 (bvsle ?x63 n)))
 (or $x4379 $x8399)))))))
 ))
 (let ((?x63 (sc_t 0)))
 (let ((?x72 (sc_s 0)))
 (let (($x73 (= ?x72 ?x63)))
 (let (($x7862 (= $x3723 (or (exc_halt_t 3) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_t 3))))))))
 (let (($x8522 (forall ((w (_ BitVec 256)) )(let ((?x8315 (storage_t x_0 x_1 3 w)))
 (let ((?x260 (storage_t x_0 x_1 4 w)))
 (= ?x260 ?x8315))))
 ))
 (let (($x2613 (forall ((n (_ BitVec 6)) )(or (= (stack_t x_0 x_1 4 n) (stack_t x_0 x_1 3 n)) (bvsle (bvadd (_ bv62 6) (sc_t 3)) n)))
 ))
 (let (($x3679 (= (used_gas_t x_0 x_1 4) (+ 3 (used_gas_t x_0 x_1 3)))))
 (let ((?x2012 (sc_t 3)))
 (let ((?x5914 (bvadd (_ bv63 6) ?x2012)))
 (let ((?x390 (stack_t x_0 x_1 3 ?x5914)))
 (let (($x9114 (= (stack_t x_0 x_1 4 (bvadd (_ bv63 6) ?x3757)) (bvxor ?x390 (stack_t x_0 x_1 3 (bvadd (_ bv62 6) ?x2012))))))
 (let (($x10336 (exc_halt_t 3)))
 (let (($x5923 (= $x10336 (or (exc_halt_t 2) (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_t 2))))))))
 (let (($x10884 (forall ((w (_ BitVec 256)) )(let ((?x7231 (storage_t x_0 x_1 2 w)))
 (let ((?x8315 (storage_t x_0 x_1 3 w)))
 (= ?x8315 ?x7231))))
 ))
 (let (($x7102 (forall ((n (_ BitVec 6)) )(let ((?x4056 (sc_t 2)))
 (let ((?x1464 (bvadd (_ bv63 6) ?x4056)))
 (let (($x5406 (bvsle ?x1464 n)))
 (or $x5406 (= (stack_t x_0 x_1 3 n) (stack_t x_0 x_1 2 n)))))))
 ))
 (let ((?x4056 (sc_t 2)))
 (let (($x1273 (= ?x2012 ?x4056)))
 (let ((?x8549 (used_gas_t x_0 x_1 3)))
 (let (($x1012 (= ?x390 (ite (= (stack_t x_0 x_1 2 (bvadd (_ bv63 6) ?x4056)) (_ bv0 256)) (_ bv1 256) (_ bv0 256)))))
 (let (($x5522 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_t 1)))) (_ bv0 1)))))
 (let (($x1920 (exc_halt_t 1)))
 (let (($x903 (exc_halt_t 2)))
 (let (($x7190 (forall ((w (_ BitVec 256)) )(let ((?x7875 (storage_t x_0 x_1 1 w)))
 (let ((?x7231 (storage_t x_0 x_1 2 w)))
 (= ?x7231 ?x7875))))
 ))
 (let (($x6805 (forall ((n (_ BitVec 6)) )(or (bvsle (bvadd (_ bv62 6) (sc_t 1)) n) (= (stack_t x_0 x_1 2 n) (stack_t x_0 x_1 1 n))))
 ))
 (let (($x5543 (= ?x4056 (bvadd (_ bv1 6) (sc_t 1)))))
 (let ((?x8430 (used_gas_t x_0 x_1 2)))
 (let ((?x4023 (sc_t 1)))
 (let ((?x1674 (bvadd (_ bv63 6) ?x4023)))
 (let ((?x47 (stack_t x_0 x_1 1 ?x1674)))
 (let ((?x1162 (bvadd (_ bv62 6) ?x4023)))
 (let ((?x70 (stack_t x_0 x_1 1 ?x1162)))
 (let (($x318 (= $x1920 (or $x56 (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) ?x63)))))))
 (let (($x8578 (forall ((w (_ BitVec 256)) )(let ((?x5106 (storage_t x_0 x_1 0 w)))
 (let ((?x7875 (storage_t x_0 x_1 1 w)))
 (= ?x7875 ?x5106))))
 ))
 (let (($x6163 (forall ((n (_ BitVec 6)) )(or (= (stack_t x_0 x_1 1 n) (stack_t x_0 x_1 0 n)) (bvsle (bvadd (_ bv63 6) (sc_t 0)) n)))
 ))
 (let (($x5384 (= ?x4023 ?x63)))
 (let (($x5709 (= ?x47 (ite (= (stack_t x_0 x_1 0 (bvadd (_ bv63 6) ?x63)) (_ bv0 256)) (_ bv1 256) (_ bv0 256)))))
 (let (($x1976 (= $x772 (or (exc_halt_s 5) (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_s 5))))))))
 (let (($x2277 (forall ((w (_ BitVec 256)) )(let ((?x5241 (storage_s x_0 x_1 5 w)))
 (let ((?x8326 (storage_s x_0 x_1 6 w)))
 (= ?x8326 ?x5241))))
 ))
 (let (($x7559 (forall ((n (_ BitVec 6)) )(let ((?x805 (sc_s 5)))
 (let ((?x8276 (bvadd (_ bv63 6) ?x805)))
 (let (($x7252 (bvsle ?x8276 n)))
 (or (= (stack_s x_0 x_1 6 n) (stack_s x_0 x_1 5 n)) $x7252)))))
 ))
 (let (($x3282 (= (used_gas_s x_0 x_1 6) (+ 3 (used_gas_s x_0 x_1 5)))))
 (let ((?x7928 (ite (= (stack_s x_0 x_1 5 (bvadd (_ bv63 6) (sc_s 5))) (_ bv0 256)) (_ bv1 256) (_ bv0 256))))
 (let (($x3979 (exc_halt_s 5)))
 (let (($x2352 (= $x3979 (or (exc_halt_s 4) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 4))))))))
 (let (($x8194 (forall ((w (_ BitVec 256)) )(let ((?x7725 (storage_s x_0 x_1 4 w)))
 (let ((?x5241 (storage_s x_0 x_1 5 w)))
 (= ?x5241 ?x7725))))
 ))
 (let (($x5188 (forall ((n (_ BitVec 6)) )(let ((?x4305 (sc_s 4)))
 (let ((?x5889 (bvadd (_ bv62 6) ?x4305)))
 (let (($x5669 (bvsle ?x5889 n)))
 (or (= (stack_s x_0 x_1 5 n) (stack_s x_0 x_1 4 n)) $x5669)))))
 ))
 (let ((?x4305 (sc_s 4)))
 (let ((?x1909 (bvadd (_ bv63 6) ?x4305)))
 (let ((?x805 (sc_s 5)))
 (let (($x1257 (= ?x805 ?x1909)))
 (let ((?x2411 (used_gas_s x_0 x_1 5)))
 (let ((?x8472 (stack_s x_0 x_1 4 ?x1909)))
 (let ((?x8276 (bvadd (_ bv63 6) ?x805)))
 (let ((?x3264 (stack_s x_0 x_1 5 ?x8276)))
 (let (($x64 (exc_halt_s 4)))
 (let (($x9181 (= $x64 (or (exc_halt_s 3) (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_s 3))))))))
 (let (($x6806 (forall ((w (_ BitVec 256)) )(let ((?x10467 (storage_s x_0 x_1 3 w)))
 (let ((?x7725 (storage_s x_0 x_1 4 w)))
 (= ?x7725 ?x10467))))
 ))
 (let (($x3205 (forall ((n (_ BitVec 6)) )(or (bvsle (bvadd (_ bv63 6) (sc_s 3)) n) (= (stack_s x_0 x_1 4 n) (stack_s x_0 x_1 3 n))))
 ))
 (let ((?x275 (sc_s 3)))
 (let (($x1346 (= ?x4305 ?x275)))
 (let ((?x2441 (used_gas_s x_0 x_1 4)))
 (let (($x8223 (= ?x8472 (ite (= (stack_s x_0 x_1 3 (bvadd (_ bv63 6) ?x275)) (_ bv0 256)) (_ bv1 256) (_ bv0 256)))))
 (let (($x8317 (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 2))))))
 (let (($x1387 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 2)))) (_ bv0 1)))))
 (let (($x247 (exc_halt_s 2)))
 (let (($x292 (exc_halt_s 3)))
 (let (($x9182 (forall ((w (_ BitVec 256)) )(let ((?x6788 (storage_s x_0 x_1 2 w)))
 (let ((?x10467 (storage_s x_0 x_1 3 w)))
 (= ?x10467 ?x6788))))
 ))
 (let (($x1319 (forall ((n (_ BitVec 6)) )(let ((?x218 (sc_s 2)))
 (let ((?x347 (bvadd (_ bv62 6) ?x218)))
 (let (($x3233 (bvsle ?x347 n)))
 (or (= (stack_s x_0 x_1 3 n) (stack_s x_0 x_1 2 n)) $x3233)))))
 ))
 (let (($x2890 (= ?x275 (bvadd (_ bv1 6) (sc_s 2)))))
 (let ((?x5219 (used_gas_s x_0 x_1 3)))
 (let ((?x218 (sc_s 2)))
 (let ((?x8036 (bvadd (_ bv63 6) ?x218)))
 (let ((?x1228 (stack_s x_0 x_1 2 ?x8036)))
 (let ((?x347 (bvadd (_ bv62 6) ?x218)))
 (let ((?x3112 (stack_s x_0 x_1 2 ?x347)))
 (let (($x4877 (= $x247 (or (exc_halt_s 1) (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_s 1))))))))
 (let (($x3430 (forall ((w (_ BitVec 256)) )(let ((?x8788 (storage_s x_0 x_1 1 w)))
 (let ((?x6788 (storage_s x_0 x_1 2 w)))
 (= ?x6788 ?x8788))))
 ))
 (let (($x1113 (forall ((n (_ BitVec 6)) )(let ((?x154 (sc_s 1)))
 (let ((?x2733 (bvadd (_ bv63 6) ?x154)))
 (let (($x9668 (bvsle ?x2733 n)))
 (or (= (stack_s x_0 x_1 2 n) (stack_s x_0 x_1 1 n)) $x9668)))))
 ))
 (let ((?x154 (sc_s 1)))
 (let (($x8328 (= ?x218 ?x154)))
 (let ((?x8723 (used_gas_s x_0 x_1 2)))
 (let (($x5600 (= ?x1228 (ite (= (stack_s x_0 x_1 1 (bvadd (_ bv63 6) ?x154)) (_ bv0 256)) (_ bv1 256) (_ bv0 256)))))
 (let (($x189 (exc_halt_s 1)))
 (let (($x1843 (= $x189 (or $x57 (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) ?x72)))))))
 (let (($x10098 (forall ((w (_ BitVec 256)) )(let ((?x5011 (storage_s x_0 x_1 0 w)))
 (let ((?x8788 (storage_s x_0 x_1 1 w)))
 (= ?x8788 ?x5011))))
 ))
 (let (($x7597 (forall ((n (_ BitVec 6)) )(let ((?x72 (sc_s 0)))
 (let ((?x10965 (bvadd (_ bv63 6) ?x72)))
 (let (($x2446 (bvsle ?x10965 n)))
 (or $x2446 (= (stack_s x_0 x_1 1 n) (stack_s x_0 x_1 0 n)))))))
 ))
 (let (($x1914 (= ?x154 ?x72)))
 (let ((?x7458 (used_gas_s x_0 x_1 1)))
 (let (($x9023 (= ?x7458 (+ 3 ?x1753))))
 (let ((?x2733 (bvadd (_ bv63 6) ?x154)))
 (let ((?x2344 (stack_s x_0 x_1 1 ?x2733)))
 (let (($x6036 (= ?x2344 (ite (= (stack_s x_0 x_1 0 (bvadd (_ bv63 6) ?x72)) (_ bv0 256)) (_ bv1 256) (_ bv0 256)))))
 (let (($x1661 (forall ((w (_ BitVec 256)) )(let ((?x5011 (storage_s x_0 x_1 0 w)))
 (= ?x5011 (_ bv0 256))))
 ))
 (let (($x3386 (= ?x1753 0)))
 (let (($x6677 (not $x57)))
 (let (($x1421 (= (stack_s x_0 x_1 0 (_ bv1 6)) x_1)))
 (let (($x9089 (= (stack_s x_0 x_1 0 (_ bv0 6)) x_0)))
 (let (($x1347 (= ?x72 (_ bv2 6))))
 (and $x1347 $x9089 $x1421 $x6677 $x3386 $x1661 $x6036 $x9023 $x1914 $x7597 $x10098 $x1843 $x5600 (= ?x8723 (+ 3 ?x7458)) $x8328 $x1113 $x3430 $x4877 (= (stack_s x_0 x_1 3 (bvadd (_ bv63 6) ?x275)) ?x3112) (= (stack_s x_0 x_1 3 ?x347) ?x3112) (= (stack_s x_0 x_1 3 ?x8036) ?x1228) (= ?x5219 (+ 3 ?x8723)) $x2890 $x1319 $x9182 (= $x292 (or $x247 $x1387 $x8317)) $x8223 (= ?x2441 (+ 3 ?x5219)) $x1346 $x3205 $x6806 $x9181 (= ?x3264 (bvxor ?x8472 (stack_s x_0 x_1 4 (bvadd (_ bv62 6) ?x4305)))) (= ?x2411 (+ 3 ?x2441)) $x1257 $x5188 $x8194 $x2352 (= (stack_s x_0 x_1 6 (bvadd (_ bv63 6) ?x926)) ?x7928) $x3282 (= ?x926 ?x805) $x7559 $x2277 $x1976 $x5709 (= (used_gas_t x_0 x_1 1) (+ 3 ?x10825)) $x5384 $x6163 $x8578 $x318 (= (stack_t x_0 x_1 2 (bvadd (_ bv63 6) ?x4056)) ?x70) (= (stack_t x_0 x_1 2 ?x1162) ?x70) (= (stack_t x_0 x_1 2 ?x1674) ?x47) (= ?x8430 (+ 3 (used_gas_t x_0 x_1 1))) $x5543 $x6805 $x7190 (= $x903 (or $x1920 (not (bvsle (_ bv0 6) ?x1162)) $x5522)) $x1012 (= ?x8549 (+ 3 ?x8430)) $x1273 $x7102 $x10884 $x5923 $x9114 $x3679 (= ?x3757 ?x5914) $x2613 $x8522 $x7862 $x73 $x469 $x58 $x6718 $x8355 (not (and $x7941 $x7034 $x4256 $x9543)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
 )
(check-sat)
(get-proof)
