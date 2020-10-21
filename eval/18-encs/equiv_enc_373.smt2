; CALLER SWAP3 SWAP1 SWAP3 => SWAP2 CALLER
(set-option :produce-proofs true)
; 
(set-info :status unknown)
(declare-fun storage_t ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 256)) (_ BitVec 256))
(declare-fun storage_s ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 256)) (_ BitVec 256))
(declare-fun exc_halt_t (Int) Bool)
(declare-fun exc_halt_s (Int) Bool)
(declare-fun sc_t (Int) (_ BitVec 6))
(declare-fun stack_t ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 6)) (_ BitVec 256))
(declare-fun stack_s ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 6)) (_ BitVec 256))
(declare-fun sc_s (Int) (_ BitVec 6))
(declare-fun used_gas_t ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int) Int)
(declare-fun used_gas_s ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int) Int)
(assert
 (exists ((x_0 (_ BitVec 256)) (x_1 (_ BitVec 256)) (x_2 (_ BitVec 256)) (x_CALLER (_ BitVec 256)) )(let (($x2157 (forall ((w (_ BitVec 256)) )(let ((?x10711 (storage_t x_0 x_1 x_2 x_CALLER 2 w)))
 (let ((?x6619 (storage_s x_0 x_1 x_2 x_CALLER 4 w)))
 (= ?x6619 ?x10711))))
 ))
 (let (($x5252 (exc_halt_t 2)))
 (let (($x7172 (exc_halt_s 4)))
 (let (($x773 (= $x7172 $x5252)))
 (let (($x6386 (forall ((n (_ BitVec 6)) )(let ((?x2714 (sc_t 2)))
 (let (($x6039 (bvsle ?x2714 n)))
 (let ((?x10175 (stack_t x_0 x_1 x_2 x_CALLER 2 n)))
 (let ((?x4404 (stack_s x_0 x_1 x_2 x_CALLER 4 n)))
 (let (($x3821 (= ?x4404 ?x10175)))
 (or $x3821 $x6039)))))))
 ))
 (let ((?x2714 (sc_t 2)))
 (let ((?x4305 (sc_s 4)))
 (let (($x8664 (= ?x4305 ?x2714)))
 (let ((?x10988 (used_gas_t x_0 x_1 x_2 x_CALLER 0)))
 (let ((?x6387 (used_gas_s x_0 x_1 x_2 x_CALLER 0)))
 (let (($x2784 (= ?x6387 ?x10988)))
 (let (($x10844 (forall ((w (_ BitVec 256)) )(let ((?x8413 (storage_t x_0 x_1 x_2 x_CALLER 0 w)))
 (let ((?x6539 (storage_s x_0 x_1 x_2 x_CALLER 0 w)))
 (= ?x6539 ?x8413))))
 ))
 (let (($x56 (exc_halt_t 0)))
 (let (($x57 (exc_halt_s 0)))
 (let (($x58 (= $x57 $x56)))
 (let (($x5183 (forall ((n (_ BitVec 6)) )(let ((?x2456 (stack_t x_0 x_1 x_2 x_CALLER 0 n)))
 (let ((?x5579 (stack_s x_0 x_1 x_2 x_CALLER 0 n)))
 (let (($x6479 (= ?x5579 ?x2456)))
 (let ((?x63 (sc_t 0)))
 (let (($x4866 (bvsle ?x63 n)))
 (or $x4866 $x6479)))))))
 ))
 (let ((?x63 (sc_t 0)))
 (let ((?x72 (sc_s 0)))
 (let (($x73 (= ?x72 ?x63)))
 (let (($x6319 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_t 1)))) (_ bv0 1)))))
 (let (($x3508 (exc_halt_t 1)))
 (let (($x10693 (= $x5252 (or $x3508 $x6319))))
 (let (($x6811 (forall ((w (_ BitVec 256)) )(let ((?x1950 (storage_t x_0 x_1 x_2 x_CALLER 1 w)))
 (let ((?x10711 (storage_t x_0 x_1 x_2 x_CALLER 2 w)))
 (= ?x10711 ?x1950))))
 ))
 (let (($x7202 (forall ((n (_ BitVec 6)) )(let ((?x8347 (sc_t 1)))
 (let (($x7612 (bvsle ?x8347 n)))
 (let ((?x4934 (stack_t x_0 x_1 x_2 x_CALLER 1 n)))
 (let ((?x10175 (stack_t x_0 x_1 x_2 x_CALLER 2 n)))
 (let (($x7179 (= ?x10175 ?x4934)))
 (or $x7179 $x7612)))))))
 ))
 (let (($x6242 (= ?x2714 (bvadd (_ bv1 6) (sc_t 1)))))
 (let ((?x418 (used_gas_t x_0 x_1 x_2 x_CALLER 2)))
 (let (($x5489 (= $x3508 (or $x56 (not (bvsle (_ bv0 6) (bvadd (_ bv61 6) ?x63)))))))
 (let (($x10305 (forall ((w (_ BitVec 256)) )(let ((?x8413 (storage_t x_0 x_1 x_2 x_CALLER 0 w)))
 (let ((?x1950 (storage_t x_0 x_1 x_2 x_CALLER 1 w)))
 (= ?x1950 ?x8413))))
 ))
 (let (($x10038 (forall ((n (_ BitVec 6)) )(let ((?x63 (sc_t 0)))
 (let ((?x2232 (bvadd (_ bv61 6) ?x63)))
 (let (($x10055 (bvsle ?x2232 n)))
 (let ((?x2456 (stack_t x_0 x_1 x_2 x_CALLER 0 n)))
 (let ((?x4934 (stack_t x_0 x_1 x_2 x_CALLER 1 n)))
 (let (($x6020 (= ?x4934 ?x2456)))
 (or $x6020 $x10055))))))))
 ))
 (let ((?x8347 (sc_t 1)))
 (let (($x8981 (= ?x8347 ?x63)))
 (let ((?x7476 (used_gas_t x_0 x_1 x_2 x_CALLER 1)))
 (let (($x1969 (= ?x7476 (+ 3 ?x10988))))
 (let ((?x11005 (bvadd (_ bv62 6) ?x63)))
 (let ((?x5311 (stack_t x_0 x_1 x_2 x_CALLER 0 ?x11005)))
 (let ((?x10292 (bvadd (_ bv62 6) ?x8347)))
 (let ((?x8566 (stack_t x_0 x_1 x_2 x_CALLER 1 ?x10292)))
 (let ((?x2902 (bvadd (_ bv63 6) ?x63)))
 (let ((?x5914 (stack_t x_0 x_1 x_2 x_CALLER 0 ?x2902)))
 (let ((?x5839 (bvadd (_ bv61 6) ?x8347)))
 (let ((?x6050 (stack_t x_0 x_1 x_2 x_CALLER 1 ?x5839)))
 (let ((?x4288 (bvadd (_ bv63 6) ?x8347)))
 (let ((?x3981 (stack_t x_0 x_1 x_2 x_CALLER 1 ?x4288)))
 (let (($x2436 (= $x7172 (or (exc_halt_s 3) (not (bvsle (_ bv0 6) (bvadd (_ bv60 6) (sc_s 3))))))))
 (let (($x4877 (forall ((w (_ BitVec 256)) )(let ((?x10217 (storage_s x_0 x_1 x_2 x_CALLER 3 w)))
 (let ((?x6619 (storage_s x_0 x_1 x_2 x_CALLER 4 w)))
 (= ?x6619 ?x10217))))
 ))
 (let (($x8204 (forall ((n (_ BitVec 6)) )(let ((?x275 (sc_s 3)))
 (let ((?x6549 (bvadd (_ bv60 6) ?x275)))
 (let (($x6516 (bvsle ?x6549 n)))
 (let ((?x3542 (stack_s x_0 x_1 x_2 x_CALLER 3 n)))
 (let ((?x4404 (stack_s x_0 x_1 x_2 x_CALLER 4 n)))
 (let (($x1134 (= ?x4404 ?x3542)))
 (or $x1134 $x6516))))))))
 ))
 (let ((?x275 (sc_s 3)))
 (let (($x9884 (= ?x4305 ?x275)))
 (let ((?x6954 (used_gas_s x_0 x_1 x_2 x_CALLER 4)))
 (let (($x11467 (= ?x6954 (+ 3 (used_gas_s x_0 x_1 x_2 x_CALLER 3)))))
 (let ((?x4190 (bvadd (_ bv62 6) ?x275)))
 (let ((?x940 (stack_s x_0 x_1 x_2 x_CALLER 3 ?x4190)))
 (let ((?x9777 (bvadd (_ bv62 6) ?x4305)))
 (let ((?x5830 (stack_s x_0 x_1 x_2 x_CALLER 4 ?x9777)))
 (let ((?x6436 (bvadd (_ bv61 6) ?x275)))
 (let ((?x1563 (stack_s x_0 x_1 x_2 x_CALLER 3 ?x6436)))
 (let ((?x314 (bvadd (_ bv63 6) ?x275)))
 (let ((?x8306 (stack_s x_0 x_1 x_2 x_CALLER 3 ?x314)))
 (let ((?x6549 (bvadd (_ bv60 6) ?x275)))
 (let ((?x1140 (stack_s x_0 x_1 x_2 x_CALLER 3 ?x6549)))
 (let ((?x9783 (bvadd (_ bv63 6) ?x4305)))
 (let ((?x11322 (stack_s x_0 x_1 x_2 x_CALLER 4 ?x9783)))
 (let (($x292 (exc_halt_s 3)))
 (let (($x8083 (= $x292 (or (exc_halt_s 2) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 2))))))))
 (let (($x1121 (forall ((w (_ BitVec 256)) )(let ((?x1465 (storage_s x_0 x_1 x_2 x_CALLER 2 w)))
 (let ((?x10217 (storage_s x_0 x_1 x_2 x_CALLER 3 w)))
 (= ?x10217 ?x1465))))
 ))
 (let (($x4863 (forall ((n (_ BitVec 6)) )(let ((?x218 (sc_s 2)))
 (let ((?x3067 (bvadd (_ bv62 6) ?x218)))
 (let (($x5606 (bvsle ?x3067 n)))
 (let ((?x2688 (stack_s x_0 x_1 x_2 x_CALLER 2 n)))
 (let ((?x3542 (stack_s x_0 x_1 x_2 x_CALLER 3 n)))
 (let (($x3650 (= ?x3542 ?x2688)))
 (or $x3650 $x5606))))))))
 ))
 (let ((?x218 (sc_s 2)))
 (let (($x4933 (= ?x275 ?x218)))
 (let ((?x11784 (used_gas_s x_0 x_1 x_2 x_CALLER 3)))
 (let (($x11553 (= ?x11784 (+ 3 (used_gas_s x_0 x_1 x_2 x_CALLER 2)))))
 (let (($x247 (exc_halt_s 2)))
 (let (($x794 (= $x247 (or (exc_halt_s 1) (not (bvsle (_ bv0 6) (bvadd (_ bv60 6) (sc_s 1))))))))
 (let (($x4022 (forall ((w (_ BitVec 256)) )(let ((?x9566 (storage_s x_0 x_1 x_2 x_CALLER 1 w)))
 (let ((?x1465 (storage_s x_0 x_1 x_2 x_CALLER 2 w)))
 (= ?x1465 ?x9566))))
 ))
 (let (($x3605 (forall ((n (_ BitVec 6)) )(let ((?x1595 (stack_s x_0 x_1 x_2 x_CALLER 1 n)))
 (let ((?x2688 (stack_s x_0 x_1 x_2 x_CALLER 2 n)))
 (let (($x4357 (= ?x2688 ?x1595)))
 (or $x4357 (bvsle (bvadd (_ bv60 6) (sc_s 1)) n))))))
 ))
 (let ((?x154 (sc_s 1)))
 (let (($x2182 (= ?x218 ?x154)))
 (let ((?x437 (used_gas_s x_0 x_1 x_2 x_CALLER 2)))
 (let (($x3204 (= ?x437 (+ 3 (used_gas_s x_0 x_1 x_2 x_CALLER 1)))))
 (let ((?x9948 (bvadd (_ bv62 6) ?x154)))
 (let ((?x6371 (stack_s x_0 x_1 x_2 x_CALLER 1 ?x9948)))
 (let ((?x3067 (bvadd (_ bv62 6) ?x218)))
 (let ((?x6011 (stack_s x_0 x_1 x_2 x_CALLER 2 ?x3067)))
 (let (($x4428 (= ?x6011 ?x6371)))
 (let ((?x5690 (bvadd (_ bv61 6) ?x154)))
 (let ((?x4859 (stack_s x_0 x_1 x_2 x_CALLER 1 ?x5690)))
 (let ((?x10194 (bvadd (_ bv61 6) ?x218)))
 (let ((?x10185 (stack_s x_0 x_1 x_2 x_CALLER 2 ?x10194)))
 (let (($x6085 (= ?x10185 ?x4859)))
 (let ((?x9355 (bvadd (_ bv63 6) ?x154)))
 (let ((?x224 (stack_s x_0 x_1 x_2 x_CALLER 1 ?x9355)))
 (let ((?x6883 (bvadd (_ bv60 6) ?x218)))
 (let ((?x9506 (stack_s x_0 x_1 x_2 x_CALLER 2 ?x6883)))
 (let (($x3812 (= ?x9506 ?x224)))
 (let ((?x9779 (bvadd (_ bv60 6) ?x154)))
 (let ((?x10511 (stack_s x_0 x_1 x_2 x_CALLER 1 ?x9779)))
 (let ((?x5789 (bvadd (_ bv63 6) ?x218)))
 (let ((?x250 (stack_s x_0 x_1 x_2 x_CALLER 2 ?x5789)))
 (let (($x5527 (= ?x250 ?x10511)))
 (let (($x189 (exc_halt_s 1)))
 (let (($x9787 (= $x189 (or $x57 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x72))) (_ bv0 1)))))))
 (let (($x3763 (forall ((w (_ BitVec 256)) )(let ((?x6539 (storage_s x_0 x_1 x_2 x_CALLER 0 w)))
 (let ((?x9566 (storage_s x_0 x_1 x_2 x_CALLER 1 w)))
 (= ?x9566 ?x6539))))
 ))
 (let (($x6160 (forall ((n (_ BitVec 6)) )(let ((?x5579 (stack_s x_0 x_1 x_2 x_CALLER 0 n)))
 (let ((?x1595 (stack_s x_0 x_1 x_2 x_CALLER 1 n)))
 (let (($x10930 (= ?x1595 ?x5579)))
 (let ((?x72 (sc_s 0)))
 (let (($x3549 (bvsle ?x72 n)))
 (or $x3549 $x10930)))))))
 ))
 (let (($x10046 (= ?x154 (bvadd (_ bv1 6) ?x72))))
 (let (($x209 (forall ((w (_ BitVec 256)) )(let ((?x6539 (storage_s x_0 x_1 x_2 x_CALLER 0 w)))
 (= ?x6539 (_ bv0 256))))
 ))
 (let (($x4136 (= ?x6387 0)))
 (let (($x1248 (not $x57)))
 (let (($x1519 (= (stack_s x_0 x_1 x_2 x_CALLER 0 (_ bv2 6)) x_2)))
 (let (($x4087 (= (stack_s x_0 x_1 x_2 x_CALLER 0 (_ bv1 6)) x_1)))
 (let (($x786 (= (stack_s x_0 x_1 x_2 x_CALLER 0 (_ bv0 6)) x_0)))
 (let (($x5315 (= ?x72 (_ bv3 6))))
 (and $x5315 $x786 $x4087 $x1519 $x1248 $x4136 $x209 (= (stack_s x_0 x_1 x_2 x_CALLER 1 ?x72) x_CALLER) (= (used_gas_s x_0 x_1 x_2 x_CALLER 1) (+ 2 ?x6387)) $x10046 $x6160 $x3763 $x9787 $x5527 $x3812 $x6085 $x4428 $x3204 $x2182 $x3605 $x4022 $x794 (= ?x8306 ?x6011) (= ?x940 ?x250) $x11553 $x4933 $x4863 $x1121 $x8083 (= ?x11322 ?x1140) (= (stack_s x_0 x_1 x_2 x_CALLER 4 (bvadd (_ bv60 6) ?x4305)) ?x8306) (= (stack_s x_0 x_1 x_2 x_CALLER 4 (bvadd (_ bv61 6) ?x4305)) ?x1563) (= ?x5830 ?x940) $x11467 $x9884 $x8204 $x4877 $x2436 (= ?x3981 (stack_t x_0 x_1 x_2 x_CALLER 0 (bvadd (_ bv61 6) ?x63))) (= ?x6050 ?x5914) (= ?x8566 ?x5311) $x1969 $x8981 $x10038 $x10305 $x5489 (= (stack_t x_0 x_1 x_2 x_CALLER 2 ?x8347) x_CALLER) (= ?x418 (+ 2 ?x7476)) $x6242 $x7202 $x6811 $x10693 $x73 $x5183 $x58 $x10844 $x2784 (not (and $x8664 $x6386 $x773 $x2157))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
 )
(check-sat)
(get-proof)
