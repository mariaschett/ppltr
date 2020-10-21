; SWAP1 SWAP2 DUP4 SWAP2 SWAP1 => DUP4 SWAP2 SWAP3
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
 (exists ((x_0 (_ BitVec 256)) (x_1 (_ BitVec 256)) (x_2 (_ BitVec 256)) (x_3 (_ BitVec 256)) )(let (($x3321 (forall ((w (_ BitVec 256)) )(let ((?x6175 (storage_t x_0 x_1 x_2 x_3 3 w)))
 (let ((?x11274 (storage_s x_0 x_1 x_2 x_3 5 w)))
 (= ?x11274 ?x6175))))
 ))
 (let (($x4112 (exc_halt_t 3)))
 (let (($x1862 (exc_halt_s 5)))
 (let (($x34 (= $x1862 $x4112)))
 (let (($x5917 (forall ((n (_ BitVec 6)) )(let ((?x8728 (stack_t x_0 x_1 x_2 x_3 3 n)))
 (let ((?x6493 (stack_s x_0 x_1 x_2 x_3 5 n)))
 (let (($x3176 (= ?x6493 ?x8728)))
 (or $x3176 (bvsle (sc_t 3) n))))))
 ))
 (let ((?x11964 (sc_t 3)))
 (let ((?x4319 (sc_s 5)))
 (let (($x3659 (= ?x4319 ?x11964)))
 (let ((?x7911 (used_gas_t x_0 x_1 x_2 x_3 0)))
 (let ((?x7495 (used_gas_s x_0 x_1 x_2 x_3 0)))
 (let (($x2363 (= ?x7495 ?x7911)))
 (let (($x11213 (forall ((w (_ BitVec 256)) )(let ((?x4723 (storage_t x_0 x_1 x_2 x_3 0 w)))
 (let ((?x3318 (storage_s x_0 x_1 x_2 x_3 0 w)))
 (= ?x3318 ?x4723))))
 ))
 (let (($x56 (exc_halt_t 0)))
 (let (($x57 (exc_halt_s 0)))
 (let (($x58 (= $x57 $x56)))
 (let (($x3877 (forall ((n (_ BitVec 6)) )(let ((?x4436 (stack_t x_0 x_1 x_2 x_3 0 n)))
 (let ((?x4422 (stack_s x_0 x_1 x_2 x_3 0 n)))
 (let (($x11541 (= ?x4422 ?x4436)))
 (let ((?x63 (sc_t 0)))
 (let (($x632 (bvsle ?x63 n)))
 (or $x632 $x11541)))))))
 ))
 (let ((?x63 (sc_t 0)))
 (let ((?x72 (sc_s 0)))
 (let (($x73 (= ?x72 ?x63)))
 (let (($x7608 (= $x4112 (or (exc_halt_t 2) (not (bvsle (_ bv0 6) (bvadd (_ bv60 6) (sc_t 2))))))))
 (let (($x11505 (forall ((w (_ BitVec 256)) )(let ((?x9530 (storage_t x_0 x_1 x_2 x_3 2 w)))
 (let ((?x6175 (storage_t x_0 x_1 x_2 x_3 3 w)))
 (= ?x6175 ?x9530))))
 ))
 (let (($x9084 (forall ((n (_ BitVec 6)) )(let ((?x3258 (stack_t x_0 x_1 x_2 x_3 2 n)))
 (let ((?x8728 (stack_t x_0 x_1 x_2 x_3 3 n)))
 (or (bvsle (bvadd (_ bv60 6) (sc_t 2)) n) (= ?x8728 ?x3258)))))
 ))
 (let (($x7005 (= (used_gas_t x_0 x_1 x_2 x_3 3) (+ 3 (used_gas_t x_0 x_1 x_2 x_3 2)))))
 (let ((?x6602 (sc_t 2)))
 (let ((?x3220 (bvadd (_ bv62 6) ?x6602)))
 (let ((?x10805 (stack_t x_0 x_1 x_2 x_3 2 ?x3220)))
 (let ((?x5452 (stack_t x_0 x_1 x_2 x_3 2 (bvadd (_ bv61 6) ?x6602))))
 (let ((?x8103 (bvadd (_ bv63 6) ?x6602)))
 (let ((?x8353 (stack_t x_0 x_1 x_2 x_3 2 ?x8103)))
 (let (($x407 (= (stack_t x_0 x_1 x_2 x_3 3 (bvadd (_ bv63 6) ?x11964)) (stack_t x_0 x_1 x_2 x_3 2 (bvadd (_ bv60 6) ?x6602)))))
 (let (($x9181 (exc_halt_t 2)))
 (let (($x6724 (= $x9181 (or (exc_halt_t 1) (not (bvsle (_ bv0 6) (bvadd (_ bv61 6) (sc_t 1))))))))
 (let (($x3387 (forall ((w (_ BitVec 256)) )(let ((?x6713 (storage_t x_0 x_1 x_2 x_3 1 w)))
 (let ((?x9530 (storage_t x_0 x_1 x_2 x_3 2 w)))
 (= ?x9530 ?x6713))))
 ))
 (let (($x11269 (forall ((n (_ BitVec 6)) )(let ((?x8017 (stack_t x_0 x_1 x_2 x_3 1 n)))
 (let ((?x3258 (stack_t x_0 x_1 x_2 x_3 2 n)))
 (or (bvsle (bvadd (_ bv61 6) (sc_t 1)) n) (= ?x3258 ?x8017)))))
 ))
 (let ((?x6377 (sc_t 1)))
 (let (($x5631 (= ?x6602 ?x6377)))
 (let ((?x10183 (used_gas_t x_0 x_1 x_2 x_3 2)))
 (let (($x1117 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x63))) (_ bv0 1)))))
 (let (($x5011 (exc_halt_t 1)))
 (let (($x9165 (forall ((w (_ BitVec 256)) )(let ((?x4723 (storage_t x_0 x_1 x_2 x_3 0 w)))
 (let ((?x6713 (storage_t x_0 x_1 x_2 x_3 1 w)))
 (= ?x6713 ?x4723))))
 ))
 (let (($x6225 (forall ((n (_ BitVec 6)) )(let ((?x4436 (stack_t x_0 x_1 x_2 x_3 0 n)))
 (let ((?x8017 (stack_t x_0 x_1 x_2 x_3 1 n)))
 (or (= ?x8017 ?x4436) (bvsle (bvadd (_ bv60 6) (sc_t 0)) n)))))
 ))
 (let (($x11310 (= ?x6377 (bvadd (_ bv1 6) ?x63))))
 (let (($x5594 (= (stack_t x_0 x_1 x_2 x_3 1 (bvadd (_ bv63 6) ?x63)) (stack_t x_0 x_1 x_2 x_3 0 (bvadd (_ bv63 6) ?x63)))))
 (let (($x11723 (= (stack_t x_0 x_1 x_2 x_3 1 (bvadd (_ bv62 6) ?x63)) (stack_t x_0 x_1 x_2 x_3 0 (bvadd (_ bv62 6) ?x63)))))
 (let (($x1748 (= (stack_t x_0 x_1 x_2 x_3 1 (bvadd (_ bv61 6) ?x63)) (stack_t x_0 x_1 x_2 x_3 0 (bvadd (_ bv61 6) ?x63)))))
 (let ((?x9903 (bvadd (_ bv60 6) ?x63)))
 (let ((?x3979 (stack_t x_0 x_1 x_2 x_3 0 ?x9903)))
 (let (($x5223 (= $x1862 (or (exc_halt_s 4) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 4))))))))
 (let (($x4541 (forall ((w (_ BitVec 256)) )(let ((?x6954 (storage_s x_0 x_1 x_2 x_3 4 w)))
 (let ((?x11274 (storage_s x_0 x_1 x_2 x_3 5 w)))
 (= ?x11274 ?x6954))))
 ))
 (let (($x216 (forall ((n (_ BitVec 6)) )(let ((?x2920 (stack_s x_0 x_1 x_2 x_3 4 n)))
 (let ((?x6493 (stack_s x_0 x_1 x_2 x_3 5 n)))
 (let ((?x500 (sc_s 4)))
 (let ((?x7626 (bvadd (_ bv62 6) ?x500)))
 (let (($x2358 (bvsle ?x7626 n)))
 (or $x2358 (= ?x6493 ?x2920))))))))
 ))
 (let ((?x500 (sc_s 4)))
 (let (($x8952 (= ?x4319 ?x500)))
 (let (($x7106 (= (used_gas_s x_0 x_1 x_2 x_3 5) (+ 3 (used_gas_s x_0 x_1 x_2 x_3 4)))))
 (let ((?x4132 (bvadd (_ bv63 6) ?x500)))
 (let ((?x4629 (stack_s x_0 x_1 x_2 x_3 4 ?x4132)))
 (let ((?x7626 (bvadd (_ bv62 6) ?x500)))
 (let ((?x11784 (stack_s x_0 x_1 x_2 x_3 4 ?x7626)))
 (let (($x3743 (exc_halt_s 4)))
 (let (($x9223 (= $x3743 (or (exc_halt_s 3) (not (bvsle (_ bv0 6) (bvadd (_ bv61 6) (sc_s 3))))))))
 (let (($x5085 (forall ((w (_ BitVec 256)) )(let ((?x5627 (storage_s x_0 x_1 x_2 x_3 3 w)))
 (let ((?x6954 (storage_s x_0 x_1 x_2 x_3 4 w)))
 (= ?x6954 ?x5627))))
 ))
 (let (($x10296 (forall ((n (_ BitVec 6)) )(let ((?x11791 (stack_s x_0 x_1 x_2 x_3 3 n)))
 (let ((?x2920 (stack_s x_0 x_1 x_2 x_3 4 n)))
 (or (bvsle (bvadd (_ bv61 6) (sc_s 3)) n) (= ?x2920 ?x11791)))))
 ))
 (let ((?x1974 (sc_s 3)))
 (let (($x7752 (= ?x500 ?x1974)))
 (let ((?x9313 (used_gas_s x_0 x_1 x_2 x_3 4)))
 (let ((?x2640 (bvadd (_ bv63 6) ?x1974)))
 (let ((?x3182 (stack_s x_0 x_1 x_2 x_3 3 ?x2640)))
 (let (($x9761 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 2)))) (_ bv0 1)))))
 (let (($x3200 (exc_halt_s 2)))
 (let (($x8780 (exc_halt_s 3)))
 (let (($x3516 (forall ((w (_ BitVec 256)) )(let ((?x400 (storage_s x_0 x_1 x_2 x_3 2 w)))
 (let ((?x5627 (storage_s x_0 x_1 x_2 x_3 3 w)))
 (= ?x5627 ?x400))))
 ))
 (let (($x3306 (forall ((n (_ BitVec 6)) )(let ((?x8350 (stack_s x_0 x_1 x_2 x_3 2 n)))
 (let ((?x11791 (stack_s x_0 x_1 x_2 x_3 3 n)))
 (or (= ?x11791 ?x8350) (bvsle (bvadd (_ bv60 6) (sc_s 2)) n)))))
 ))
 (let (($x8893 (= ?x1974 (bvadd (_ bv1 6) (sc_s 2)))))
 (let ((?x5474 (used_gas_s x_0 x_1 x_2 x_3 3)))
 (let ((?x2620 (sc_s 2)))
 (let ((?x5957 (bvadd (_ bv63 6) ?x2620)))
 (let ((?x6064 (stack_s x_0 x_1 x_2 x_3 2 ?x5957)))
 (let ((?x11644 (bvadd (_ bv62 6) ?x2620)))
 (let ((?x10125 (stack_s x_0 x_1 x_2 x_3 2 ?x11644)))
 (let ((?x9396 (bvadd (_ bv61 6) ?x2620)))
 (let ((?x10012 (stack_s x_0 x_1 x_2 x_3 2 ?x9396)))
 (let ((?x1498 (bvadd (_ bv60 6) ?x2620)))
 (let ((?x10662 (stack_s x_0 x_1 x_2 x_3 2 ?x1498)))
 (let (($x1963 (= $x3200 (or (exc_halt_s 1) (not (bvsle (_ bv0 6) (bvadd (_ bv61 6) (sc_s 1))))))))
 (let (($x11175 (forall ((w (_ BitVec 256)) )(let ((?x2129 (storage_s x_0 x_1 x_2 x_3 1 w)))
 (let ((?x400 (storage_s x_0 x_1 x_2 x_3 2 w)))
 (= ?x400 ?x2129))))
 ))
 (let (($x10295 (forall ((n (_ BitVec 6)) )(let ((?x8960 (stack_s x_0 x_1 x_2 x_3 1 n)))
 (let ((?x8350 (stack_s x_0 x_1 x_2 x_3 2 n)))
 (or (= ?x8350 ?x8960) (bvsle (bvadd (_ bv61 6) (sc_s 1)) n)))))
 ))
 (let ((?x154 (sc_s 1)))
 (let (($x6125 (= ?x2620 ?x154)))
 (let ((?x7136 (used_gas_s x_0 x_1 x_2 x_3 2)))
 (let (($x10558 (exc_halt_s 1)))
 (let (($x199 (= $x10558 (or $x57 (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) ?x72)))))))
 (let (($x11521 (forall ((w (_ BitVec 256)) )(let ((?x3318 (storage_s x_0 x_1 x_2 x_3 0 w)))
 (let ((?x2129 (storage_s x_0 x_1 x_2 x_3 1 w)))
 (= ?x2129 ?x3318))))
 ))
 (let (($x4502 (forall ((n (_ BitVec 6)) )(let ((?x4422 (stack_s x_0 x_1 x_2 x_3 0 n)))
 (let ((?x8960 (stack_s x_0 x_1 x_2 x_3 1 n)))
 (let ((?x72 (sc_s 0)))
 (let ((?x3607 (bvadd (_ bv62 6) ?x72)))
 (let (($x824 (bvsle ?x3607 n)))
 (or $x824 (= ?x8960 ?x4422))))))))
 ))
 (let ((?x2147 (bvadd (_ bv62 6) ?x154)))
 (let ((?x11711 (stack_s x_0 x_1 x_2 x_3 1 ?x2147)))
 (let ((?x8586 (bvadd (_ bv63 6) ?x154)))
 (let ((?x4694 (stack_s x_0 x_1 x_2 x_3 1 ?x8586)))
 (let (($x10062 (forall ((w (_ BitVec 256)) )(let ((?x3318 (storage_s x_0 x_1 x_2 x_3 0 w)))
 (= ?x3318 (_ bv0 256))))
 ))
 (let (($x4099 (= ?x7495 0)))
 (let (($x10917 (not $x57)))
 (let (($x7758 (= (stack_s x_0 x_1 x_2 x_3 0 (_ bv3 6)) x_3)))
 (let (($x3410 (= (stack_s x_0 x_1 x_2 x_3 0 (_ bv2 6)) x_2)))
 (let (($x7646 (= (stack_s x_0 x_1 x_2 x_3 0 (_ bv1 6)) x_1)))
 (let (($x7951 (= (stack_s x_0 x_1 x_2 x_3 0 (_ bv0 6)) x_0)))
 (let (($x11199 (= ?x72 (_ bv4 6))))
 (and $x11199 $x7951 $x7646 $x3410 $x7758 $x10917 $x4099 $x10062 (= ?x4694 (stack_s x_0 x_1 x_2 x_3 0 (bvadd (_ bv62 6) ?x72))) (= ?x11711 (stack_s x_0 x_1 x_2 x_3 0 (bvadd (_ bv63 6) ?x72))) (= (used_gas_s x_0 x_1 x_2 x_3 1) (+ 3 ?x7495)) (= ?x154 ?x72) $x4502 $x11521 $x199 (= ?x6064 (stack_s x_0 x_1 x_2 x_3 1 (bvadd (_ bv61 6) ?x154))) (= ?x10012 ?x4694) (= ?x10125 ?x11711) (= ?x7136 (+ 3 (used_gas_s x_0 x_1 x_2 x_3 1))) $x6125 $x10295 $x11175 $x1963 (= ?x3182 ?x10662) (= (stack_s x_0 x_1 x_2 x_3 3 ?x1498) ?x10662) (= (stack_s x_0 x_1 x_2 x_3 3 ?x9396) ?x10012) (= (stack_s x_0 x_1 x_2 x_3 3 ?x11644) ?x10125) (= (stack_s x_0 x_1 x_2 x_3 3 ?x5957) ?x6064) (= ?x5474 (+ 3 ?x7136)) $x8893 $x3306 $x3516 (= $x8780 (or $x3200 $x9761 (not (bvsle (_ bv0 6) ?x1498)))) (= ?x4629 (stack_s x_0 x_1 x_2 x_3 3 (bvadd (_ bv61 6) ?x1974))) (= (stack_s x_0 x_1 x_2 x_3 4 (bvadd (_ bv61 6) ?x500)) ?x3182) (= ?x11784 (stack_s x_0 x_1 x_2 x_3 3 (bvadd (_ bv62 6) ?x1974))) (= ?x9313 (+ 3 ?x5474)) $x7752 $x10296 $x5085 $x9223 (= (stack_s x_0 x_1 x_2 x_3 5 (bvadd (_ bv63 6) ?x4319)) ?x11784) (= (stack_s x_0 x_1 x_2 x_3 5 (bvadd (_ bv62 6) ?x4319)) ?x4629) $x7106 $x8952 $x216 $x4541 $x5223 (= (stack_t x_0 x_1 x_2 x_3 1 (bvadd (_ bv63 6) ?x6377)) ?x3979) (= (stack_t x_0 x_1 x_2 x_3 1 ?x9903) ?x3979) $x1748 $x11723 $x5594 (= (used_gas_t x_0 x_1 x_2 x_3 1) (+ 3 ?x7911)) $x11310 $x6225 $x9165 (= $x5011 (or (not (bvsle (_ bv0 6) ?x9903)) $x56 $x1117)) (= ?x8353 (stack_t x_0 x_1 x_2 x_3 1 (bvadd (_ bv61 6) ?x6377))) (= ?x5452 (stack_t x_0 x_1 x_2 x_3 1 (bvadd (_ bv63 6) ?x6377))) (= ?x10805 (stack_t x_0 x_1 x_2 x_3 1 (bvadd (_ bv62 6) ?x6377))) (= ?x10183 (+ 3 (used_gas_t x_0 x_1 x_2 x_3 1))) $x5631 $x11269 $x3387 $x6724 $x407 (= (stack_t x_0 x_1 x_2 x_3 3 (bvadd (_ bv60 6) ?x11964)) ?x8353) (= (stack_t x_0 x_1 x_2 x_3 3 (bvadd (_ bv61 6) ?x11964)) ?x5452) (= (stack_t x_0 x_1 x_2 x_3 3 (bvadd (_ bv62 6) ?x11964)) ?x10805) $x7005 (= ?x11964 ?x6602) $x9084 $x11505 $x7608 $x73 $x3877 $x58 $x11213 $x2363 (not (and $x3659 $x5917 $x34 $x3321))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
 )
(check-sat)
(get-proof)
