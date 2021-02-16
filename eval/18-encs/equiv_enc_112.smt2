; DUP1 DUP1 CALLDATALOAD NOT NOT SWAP1 => DUP1 CALLDATALOAD DUP2
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
(declare-fun f_CALLDATALOAD ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256)) (_ BitVec 256))
(assert
 (exists ((x_0 (_ BitVec 256)) (x_CALLDATALOAD_0 (_ BitVec 256)) )(let (($x6535 (forall ((w (_ BitVec 256)) )(let ((?x7077 (storage_t x_0 x_CALLDATALOAD_0 3 w)))
 (let ((?x7433 (storage_s x_0 x_CALLDATALOAD_0 6 w)))
 (= ?x7433 ?x7077))))
 ))
 (let (($x10336 (exc_halt_t 3)))
 (let (($x772 (exc_halt_s 6)))
 (let (($x2647 (= $x772 $x10336)))
 (let (($x5949 (forall ((n (_ BitVec 6)) )(let ((?x4119 (stack_t x_0 x_CALLDATALOAD_0 3 n)))
 (let ((?x6859 (stack_s x_0 x_CALLDATALOAD_0 6 n)))
 (let (($x6071 (= ?x6859 ?x4119)))
 (or $x6071 (bvsle (sc_t 3) n))))))
 ))
 (let ((?x2012 (sc_t 3)))
 (let ((?x926 (sc_s 6)))
 (let (($x7824 (= ?x926 ?x2012)))
 (let ((?x5626 (used_gas_t x_0 x_CALLDATALOAD_0 0)))
 (let ((?x5368 (used_gas_s x_0 x_CALLDATALOAD_0 0)))
 (let (($x2873 (= ?x5368 ?x5626)))
 (let (($x5143 (forall ((w (_ BitVec 256)) )(let ((?x1841 (storage_t x_0 x_CALLDATALOAD_0 0 w)))
 (let ((?x5530 (storage_s x_0 x_CALLDATALOAD_0 0 w)))
 (= ?x5530 ?x1841))))
 ))
 (let (($x56 (exc_halt_t 0)))
 (let (($x57 (exc_halt_s 0)))
 (let (($x58 (= $x57 $x56)))
 (let (($x7960 (forall ((n (_ BitVec 6)) )(let ((?x5811 (stack_t x_0 x_CALLDATALOAD_0 0 n)))
 (let ((?x5571 (stack_s x_0 x_CALLDATALOAD_0 0 n)))
 (let (($x9745 (= ?x5571 ?x5811)))
 (let ((?x63 (sc_t 0)))
 (let (($x4561 (bvsle ?x63 n)))
 (or $x4561 $x9745)))))))
 ))
 (let ((?x63 (sc_t 0)))
 (let ((?x72 (sc_s 0)))
 (let (($x73 (= ?x72 ?x63)))
 (let (($x903 (exc_halt_t 2)))
 (let (($x7978 (or $x903 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_t 2)))) (_ bv0 1))) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_t 2)))))))
 (let (($x8201 (forall ((w (_ BitVec 256)) )(let ((?x5726 (storage_t x_0 x_CALLDATALOAD_0 2 w)))
 (let ((?x7077 (storage_t x_0 x_CALLDATALOAD_0 3 w)))
 (= ?x7077 ?x5726))))
 ))
 (let (($x6459 (forall ((n (_ BitVec 6)) )(or (bvsle (bvadd (_ bv62 6) (sc_t 2)) n) (= (stack_t x_0 x_CALLDATALOAD_0 3 n) (stack_t x_0 x_CALLDATALOAD_0 2 n))))
 ))
 (let (($x7963 (= (used_gas_t x_0 x_CALLDATALOAD_0 3) (+ 3 (used_gas_t x_0 x_CALLDATALOAD_0 2)))))
 (let ((?x4056 (sc_t 2)))
 (let ((?x6390 (bvadd (_ bv63 6) ?x4056)))
 (let ((?x7953 (stack_t x_0 x_CALLDATALOAD_0 2 ?x6390)))
 (let ((?x6845 (bvadd (_ bv62 6) ?x4056)))
 (let ((?x6046 (stack_t x_0 x_CALLDATALOAD_0 2 ?x6845)))
 (let (($x7852 (= $x903 (or (exc_halt_t 1) (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_t 1))))))))
 (let (($x8363 (forall ((w (_ BitVec 256)) )(let ((?x5358 (storage_t x_0 x_CALLDATALOAD_0 1 w)))
 (let ((?x5726 (storage_t x_0 x_CALLDATALOAD_0 2 w)))
 (= ?x5726 ?x5358))))
 ))
 (let (($x8856 (forall ((n (_ BitVec 6)) )(or (bvsle (bvadd (_ bv63 6) (sc_t 1)) n) (= (stack_t x_0 x_CALLDATALOAD_0 2 n) (stack_t x_0 x_CALLDATALOAD_0 1 n))))
 ))
 (let ((?x7250 (used_gas_t x_0 x_CALLDATALOAD_0 2)))
 (let ((?x4023 (sc_t 1)))
 (let ((?x7954 (bvadd (_ bv63 6) ?x4023)))
 (let ((?x8755 (stack_t x_0 x_CALLDATALOAD_0 1 ?x7954)))
 (let (($x8839 (or $x56 (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) ?x63))) (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x63))) (_ bv0 1))))))
 (let (($x1920 (exc_halt_t 1)))
 (let (($x7756 (forall ((w (_ BitVec 256)) )(let ((?x1841 (storage_t x_0 x_CALLDATALOAD_0 0 w)))
 (let ((?x5358 (storage_t x_0 x_CALLDATALOAD_0 1 w)))
 (= ?x5358 ?x1841))))
 ))
 (let (($x8957 (forall ((n (_ BitVec 6)) )(or (= (stack_t x_0 x_CALLDATALOAD_0 1 n) (stack_t x_0 x_CALLDATALOAD_0 0 n)) (bvsle (bvadd (_ bv63 6) (sc_t 0)) n)))
 ))
 (let ((?x741 (bvadd (_ bv63 6) ?x63)))
 (let ((?x7607 (stack_t x_0 x_CALLDATALOAD_0 0 ?x741)))
 (let (($x8797 (= $x772 (or (exc_halt_s 5) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 5))))))))
 (let (($x8866 (forall ((w (_ BitVec 256)) )(let ((?x8975 (storage_s x_0 x_CALLDATALOAD_0 5 w)))
 (let ((?x7433 (storage_s x_0 x_CALLDATALOAD_0 6 w)))
 (= ?x7433 ?x8975))))
 ))
 (let (($x7318 (forall ((n (_ BitVec 6)) )(or (bvsle (bvadd (_ bv62 6) (sc_s 5)) n) (= (stack_s x_0 x_CALLDATALOAD_0 6 n) (stack_s x_0 x_CALLDATALOAD_0 5 n))))
 ))
 (let ((?x805 (sc_s 5)))
 (let (($x3879 (= ?x926 ?x805)))
 (let (($x1410 (= (used_gas_s x_0 x_CALLDATALOAD_0 6) (+ 3 (used_gas_s x_0 x_CALLDATALOAD_0 5)))))
 (let ((?x10909 (bvadd (_ bv63 6) ?x805)))
 (let ((?x8270 (stack_s x_0 x_CALLDATALOAD_0 5 ?x10909)))
 (let (($x1035 (= (stack_s x_0 x_CALLDATALOAD_0 6 (bvadd (_ bv63 6) ?x926)) (stack_s x_0 x_CALLDATALOAD_0 5 (bvadd (_ bv62 6) ?x805)))))
 (let (($x3979 (exc_halt_s 5)))
 (let (($x7962 (= $x3979 (or (exc_halt_s 4) (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_s 4))))))))
 (let (($x3574 (forall ((w (_ BitVec 256)) )(let ((?x5320 (storage_s x_0 x_CALLDATALOAD_0 4 w)))
 (let ((?x8975 (storage_s x_0 x_CALLDATALOAD_0 5 w)))
 (= ?x8975 ?x5320))))
 ))
 (let (($x4104 (forall ((n (_ BitVec 6)) )(or (bvsle (bvadd (_ bv63 6) (sc_s 4)) n) (= (stack_s x_0 x_CALLDATALOAD_0 5 n) (stack_s x_0 x_CALLDATALOAD_0 4 n))))
 ))
 (let ((?x4305 (sc_s 4)))
 (let (($x3284 (= ?x805 ?x4305)))
 (let ((?x5363 (used_gas_s x_0 x_CALLDATALOAD_0 5)))
 (let (($x64 (exc_halt_s 4)))
 (let (($x8204 (= $x64 (or (exc_halt_s 3) (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_s 3))))))))
 (let (($x9179 (forall ((w (_ BitVec 256)) )(let ((?x9574 (storage_s x_0 x_CALLDATALOAD_0 3 w)))
 (let ((?x5320 (storage_s x_0 x_CALLDATALOAD_0 4 w)))
 (= ?x5320 ?x9574))))
 ))
 (let (($x7782 (forall ((n (_ BitVec 6)) )(or (bvsle (bvadd (_ bv63 6) (sc_s 3)) n) (= (stack_s x_0 x_CALLDATALOAD_0 4 n) (stack_s x_0 x_CALLDATALOAD_0 3 n))))
 ))
 (let ((?x5813 (used_gas_s x_0 x_CALLDATALOAD_0 4)))
 (let ((?x10988 (bvadd (_ bv63 6) ?x4305)))
 (let ((?x6622 (stack_s x_0 x_CALLDATALOAD_0 4 ?x10988)))
 (let (($x292 (exc_halt_s 3)))
 (let (($x5559 (= $x292 (or (exc_halt_s 2) (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_s 2))))))))
 (let (($x1596 (forall ((w (_ BitVec 256)) )(let ((?x2532 (storage_s x_0 x_CALLDATALOAD_0 2 w)))
 (let ((?x9574 (storage_s x_0 x_CALLDATALOAD_0 3 w)))
 (= ?x9574 ?x2532))))
 ))
 (let (($x5517 (forall ((n (_ BitVec 6)) )(or (bvsle (bvadd (_ bv63 6) (sc_s 2)) n) (= (stack_s x_0 x_CALLDATALOAD_0 3 n) (stack_s x_0 x_CALLDATALOAD_0 2 n))))
 ))
 (let ((?x218 (sc_s 2)))
 (let ((?x275 (sc_s 3)))
 (let (($x3833 (= ?x275 ?x218)))
 (let ((?x7375 (used_gas_s x_0 x_CALLDATALOAD_0 3)))
 (let ((?x11143 (bvadd (_ bv63 6) ?x275)))
 (let ((?x5326 (stack_s x_0 x_CALLDATALOAD_0 3 ?x11143)))
 (let (($x2535 (= ?x5326 (f_CALLDATALOAD x_0 x_CALLDATALOAD_0 (stack_s x_0 x_CALLDATALOAD_0 2 (bvadd (_ bv63 6) ?x218))))))
 (let (($x4770 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 1)))) (_ bv0 1)))))
 (let (($x189 (exc_halt_s 1)))
 (let (($x247 (exc_halt_s 2)))
 (let (($x5282 (forall ((w (_ BitVec 256)) )(let ((?x4859 (storage_s x_0 x_CALLDATALOAD_0 1 w)))
 (let ((?x2532 (storage_s x_0 x_CALLDATALOAD_0 2 w)))
 (= ?x2532 ?x4859))))
 ))
 (let (($x5255 (forall ((n (_ BitVec 6)) )(or (= (stack_s x_0 x_CALLDATALOAD_0 2 n) (stack_s x_0 x_CALLDATALOAD_0 1 n)) (bvsle (bvadd (_ bv63 6) (sc_s 1)) n)))
 ))
 (let (($x10803 (= ?x218 (bvadd (_ bv1 6) (sc_s 1)))))
 (let ((?x3121 (used_gas_s x_0 x_CALLDATALOAD_0 2)))
 (let ((?x154 (sc_s 1)))
 (let ((?x11178 (bvadd (_ bv63 6) ?x154)))
 (let ((?x6897 (stack_s x_0 x_CALLDATALOAD_0 1 ?x11178)))
 (let (($x7020 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x72))) (_ bv0 1)))))
 (let (($x5256 (forall ((w (_ BitVec 256)) )(let ((?x5530 (storage_s x_0 x_CALLDATALOAD_0 0 w)))
 (let ((?x4859 (storage_s x_0 x_CALLDATALOAD_0 1 w)))
 (= ?x4859 ?x5530))))
 ))
 (let (($x6168 (forall ((n (_ BitVec 6)) )(or (bvsle (bvadd (_ bv63 6) (sc_s 0)) n) (= (stack_s x_0 x_CALLDATALOAD_0 1 n) (stack_s x_0 x_CALLDATALOAD_0 0 n))))
 ))
 (let (($x10964 (= ?x154 (bvadd (_ bv1 6) ?x72))))
 (let ((?x10482 (bvadd (_ bv63 6) ?x72)))
 (let ((?x1621 (stack_s x_0 x_CALLDATALOAD_0 0 ?x10482)))
 (let (($x9134 (forall ((w0 (_ BitVec 256)) )(let ((?x6987 (ite (= (stack_s x_0 x_CALLDATALOAD_0 2 (bvadd (_ bv63 6) (sc_s 2))) w0) x_CALLDATALOAD_0 (_ bv0 256))))
 (let ((?x6943 (f_CALLDATALOAD x_0 x_CALLDATALOAD_0 w0)))
 (= ?x6943 ?x6987))))
 ))
 (let (($x7564 (forall ((w (_ BitVec 256)) )(let ((?x5530 (storage_s x_0 x_CALLDATALOAD_0 0 w)))
 (= ?x5530 (_ bv0 256))))
 ))
 (let (($x5532 (= ?x5368 0)))
 (let (($x11171 (not $x57)))
 (let (($x5155 (= (stack_s x_0 x_CALLDATALOAD_0 0 (_ bv0 6)) x_0)))
 (let (($x8685 (= ?x72 (_ bv1 6))))
 (and $x8685 $x5155 $x11171 $x5532 $x7564 $x9134 (= ?x6897 ?x1621) (= (stack_s x_0 x_CALLDATALOAD_0 1 ?x10482) ?x1621) (= (used_gas_s x_0 x_CALLDATALOAD_0 1) (+ 3 ?x5368)) $x10964 $x6168 $x5256 (= $x189 (or $x57 (not (bvsle (_ bv0 6) ?x10482)) $x7020)) (= (stack_s x_0 x_CALLDATALOAD_0 2 (bvadd (_ bv63 6) ?x218)) ?x6897) (= (stack_s x_0 x_CALLDATALOAD_0 2 ?x11178) ?x6897) (= ?x3121 (+ 3 (used_gas_s x_0 x_CALLDATALOAD_0 1))) $x10803 $x5255 $x5282 (= $x247 (or $x189 $x4770 (not (bvsle (_ bv0 6) ?x11178)))) $x2535 (= ?x7375 (+ 3 ?x3121)) $x3833 $x5517 $x1596 $x5559 (= ?x6622 (bvnot ?x5326)) (= ?x5813 (+ 3 ?x7375)) (= ?x4305 ?x275) $x7782 $x9179 $x8204 (= ?x8270 (bvnot ?x6622)) (= ?x5363 (+ 3 ?x5813)) $x3284 $x4104 $x3574 $x7962 $x1035 (= (stack_s x_0 x_CALLDATALOAD_0 6 (bvadd (_ bv62 6) ?x926)) ?x8270) $x1410 $x3879 $x7318 $x8866 $x8797 (= ?x8755 ?x7607) (= (stack_t x_0 x_CALLDATALOAD_0 1 ?x741) ?x7607) (= (used_gas_t x_0 x_CALLDATALOAD_0 1) (+ 3 ?x5626)) (= ?x4023 (bvadd (_ bv1 6) ?x63)) $x8957 $x7756 (= $x1920 $x8839) (= ?x7953 (f_CALLDATALOAD x_0 x_CALLDATALOAD_0 ?x8755)) (= ?x7250 (+ 3 (used_gas_t x_0 x_CALLDATALOAD_0 1))) (= ?x4056 ?x4023) $x8856 $x8363 $x7852 (= (stack_t x_0 x_CALLDATALOAD_0 3 (bvadd (_ bv63 6) ?x2012)) ?x6046) (= (stack_t x_0 x_CALLDATALOAD_0 3 ?x6845) ?x6046) (= (stack_t x_0 x_CALLDATALOAD_0 3 ?x6390) ?x7953) $x7963 (= ?x2012 (bvadd (_ bv1 6) ?x4056)) $x6459 $x8201 (= $x10336 $x7978) $x73 $x7960 $x58 $x5143 $x2873 (not (and $x7824 $x5949 $x2647 $x6535))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
 )
(check-sat)
(get-proof)