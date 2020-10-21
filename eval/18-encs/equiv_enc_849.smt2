; PUSH cw_2 SWAP1 DUP2 SWAP1 SHA3 SWAP1 => PUSH cw_2 SWAP1 SHA3 PUSH cw_2
(set-option :produce-proofs true)
; 
(set-info :status unknown)
(declare-fun storage_t ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 256)) (_ BitVec 256))
(declare-fun w_2 () (_ BitVec 256))
(declare-fun storage_s ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 256)) (_ BitVec 256))
(declare-fun exc_halt_t (Int) Bool)
(declare-fun exc_halt_s (Int) Bool)
(declare-fun stack_t ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 6)) (_ BitVec 256))
(declare-fun stack_s ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 6)) (_ BitVec 256))
(declare-fun sc_t (Int) (_ BitVec 6))
(declare-fun sc_s (Int) (_ BitVec 6))
(declare-fun used_gas_t ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int) Int)
(declare-fun used_gas_s ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int) Int)
(declare-fun f_SHA3 ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256)) (_ BitVec 256))
(assert
 (exists ((x_0 (_ BitVec 256)) (x_SHA3_0 (_ BitVec 256)) )(let (($x9036 (forall ((w (_ BitVec 256)) )(let ((?x5329 (storage_t x_0 w_2 x_SHA3_0 4 w)))
 (let ((?x7976 (storage_s x_0 w_2 x_SHA3_0 6 w)))
 (= ?x7976 ?x5329))))
 ))
 (let (($x7854 (exc_halt_t 4)))
 (let (($x7121 (exc_halt_s 6)))
 (let (($x1652 (= $x7121 $x7854)))
 (let (($x6377 (forall ((n (_ BitVec 6)) )(let ((?x9075 (stack_t x_0 w_2 x_SHA3_0 4 n)))
 (let ((?x3001 (stack_s x_0 w_2 x_SHA3_0 6 n)))
 (let (($x4792 (= ?x3001 ?x9075)))
 (let ((?x7495 (sc_t 4)))
 (let (($x3509 (bvsle ?x7495 n)))
 (or $x3509 $x4792)))))))
 ))
 (let ((?x7495 (sc_t 4)))
 (let ((?x9114 (sc_s 6)))
 (let (($x9737 (= ?x9114 ?x7495)))
 (let ((?x5257 (used_gas_t x_0 w_2 x_SHA3_0 0)))
 (let ((?x1971 (used_gas_s x_0 w_2 x_SHA3_0 0)))
 (let (($x10179 (= ?x1971 ?x5257)))
 (let (($x1794 (forall ((w (_ BitVec 256)) )(let ((?x11518 (storage_t x_0 w_2 x_SHA3_0 0 w)))
 (let ((?x6603 (storage_s x_0 w_2 x_SHA3_0 0 w)))
 (= ?x6603 ?x11518))))
 ))
 (let (($x56 (exc_halt_t 0)))
 (let (($x57 (exc_halt_s 0)))
 (let (($x58 (= $x57 $x56)))
 (let (($x9165 (forall ((n (_ BitVec 6)) )(let ((?x10772 (stack_t x_0 w_2 x_SHA3_0 0 n)))
 (let ((?x4373 (stack_s x_0 w_2 x_SHA3_0 0 n)))
 (let (($x9222 (= ?x4373 ?x10772)))
 (let ((?x63 (sc_t 0)))
 (let (($x9079 (bvsle ?x63 n)))
 (or $x9079 $x9222)))))))
 ))
 (let ((?x63 (sc_t 0)))
 (let ((?x72 (sc_s 0)))
 (let (($x73 (= ?x72 ?x63)))
 (let (($x9131 (exc_halt_t 3)))
 (let (($x1565 (or $x9131 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_t 3)))) (_ bv0 1))))))
 (let (($x4304 (= $x7854 $x1565)))
 (let (($x6713 (forall ((w (_ BitVec 256)) )(let ((?x11703 (storage_t x_0 w_2 x_SHA3_0 3 w)))
 (let ((?x5329 (storage_t x_0 w_2 x_SHA3_0 4 w)))
 (= ?x5329 ?x11703))))
 ))
 (let (($x4847 (forall ((n (_ BitVec 6)) )(let ((?x279 (stack_t x_0 w_2 x_SHA3_0 3 n)))
 (let ((?x9075 (stack_t x_0 w_2 x_SHA3_0 4 n)))
 (let ((?x10013 (sc_t 3)))
 (let (($x2843 (bvsle ?x10013 n)))
 (or $x2843 (= ?x9075 ?x279)))))))
 ))
 (let (($x7253 (= ?x7495 (bvadd (_ bv1 6) (sc_t 3)))))
 (let (($x6088 (= (used_gas_t x_0 w_2 x_SHA3_0 4) (+ 3 (used_gas_t x_0 w_2 x_SHA3_0 3)))))
 (let (($x5844 (= $x9131 (or (exc_halt_t 2) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_t 2))))))))
 (let (($x11274 (forall ((w (_ BitVec 256)) )(let ((?x627 (storage_t x_0 w_2 x_SHA3_0 2 w)))
 (let ((?x11703 (storage_t x_0 w_2 x_SHA3_0 3 w)))
 (= ?x11703 ?x627))))
 ))
 (let (($x292 (forall ((n (_ BitVec 6)) )(let ((?x11248 (sc_t 2)))
 (let ((?x5841 (bvadd (_ bv62 6) ?x11248)))
 (let (($x7314 (bvsle ?x5841 n)))
 (let ((?x8876 (stack_t x_0 w_2 x_SHA3_0 2 n)))
 (let ((?x279 (stack_t x_0 w_2 x_SHA3_0 3 n)))
 (or (= ?x279 ?x8876) $x7314)))))))
 ))
 (let ((?x3950 (used_gas_t x_0 w_2 x_SHA3_0 3)))
 (let ((?x11248 (sc_t 2)))
 (let ((?x5841 (bvadd (_ bv62 6) ?x11248)))
 (let ((?x11791 (stack_t x_0 w_2 x_SHA3_0 2 ?x5841)))
 (let ((?x4064 (bvadd (_ bv63 6) ?x11248)))
 (let ((?x7771 (stack_t x_0 w_2 x_SHA3_0 2 ?x4064)))
 (let (($x3743 (= (stack_t x_0 w_2 x_SHA3_0 3 (bvadd (_ bv63 6) (sc_t 3))) (f_SHA3 x_0 w_2 x_SHA3_0 ?x7771 ?x11791))))
 (let (($x4057 (exc_halt_t 2)))
 (let (($x8101 (= $x4057 (or (exc_halt_t 1) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_t 1))))))))
 (let (($x7626 (forall ((w (_ BitVec 256)) )(let ((?x5993 (storage_t x_0 w_2 x_SHA3_0 1 w)))
 (let ((?x627 (storage_t x_0 w_2 x_SHA3_0 2 w)))
 (= ?x627 ?x5993))))
 ))
 (let (($x4629 (forall ((n (_ BitVec 6)) )(let ((?x8617 (stack_t x_0 w_2 x_SHA3_0 1 n)))
 (let ((?x8876 (stack_t x_0 w_2 x_SHA3_0 2 n)))
 (let ((?x9666 (sc_t 1)))
 (let ((?x6639 (bvadd (_ bv62 6) ?x9666)))
 (let (($x4902 (bvsle ?x6639 n)))
 (or $x4902 (= ?x8876 ?x8617))))))))
 ))
 (let ((?x9666 (sc_t 1)))
 (let (($x5665 (= ?x11248 ?x9666)))
 (let ((?x7571 (used_gas_t x_0 w_2 x_SHA3_0 2)))
 (let (($x4852 (exc_halt_t 1)))
 (let (($x3469 (= $x4852 (or $x56 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x63))) (_ bv0 1)))))))
 (let (($x3016 (forall ((w (_ BitVec 256)) )(let ((?x11518 (storage_t x_0 w_2 x_SHA3_0 0 w)))
 (let ((?x5993 (storage_t x_0 w_2 x_SHA3_0 1 w)))
 (= ?x5993 ?x11518))))
 ))
 (let (($x3962 (forall ((n (_ BitVec 6)) )(let ((?x63 (sc_t 0)))
 (let (($x9079 (bvsle ?x63 n)))
 (let ((?x10772 (stack_t x_0 w_2 x_SHA3_0 0 n)))
 (let ((?x8617 (stack_t x_0 w_2 x_SHA3_0 1 n)))
 (or (= ?x8617 ?x10772) $x9079))))))
 ))
 (let (($x9898 (= ?x9666 (bvadd (_ bv1 6) ?x63))))
 (let (($x6211 (= $x7121 (or (exc_halt_s 5) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 5))))))))
 (let (($x10662 (forall ((w (_ BitVec 256)) )(let ((?x897 (storage_s x_0 w_2 x_SHA3_0 5 w)))
 (let ((?x7976 (storage_s x_0 w_2 x_SHA3_0 6 w)))
 (= ?x7976 ?x897))))
 ))
 (let (($x1735 (forall ((n (_ BitVec 6)) )(let ((?x10014 (stack_s x_0 w_2 x_SHA3_0 5 n)))
 (let ((?x3001 (stack_s x_0 w_2 x_SHA3_0 6 n)))
 (let ((?x4319 (sc_s 5)))
 (let ((?x6485 (bvadd (_ bv62 6) ?x4319)))
 (let (($x82 (bvsle ?x6485 n)))
 (or $x82 (= ?x3001 ?x10014))))))))
 ))
 (let ((?x4319 (sc_s 5)))
 (let (($x5013 (= ?x9114 ?x4319)))
 (let (($x400 (= (used_gas_s x_0 w_2 x_SHA3_0 6) (+ 3 (used_gas_s x_0 w_2 x_SHA3_0 5)))))
 (let ((?x1065 (bvadd (_ bv63 6) ?x4319)))
 (let ((?x5957 (stack_s x_0 w_2 x_SHA3_0 5 ?x1065)))
 (let (($x7136 (= (stack_s x_0 w_2 x_SHA3_0 6 (bvadd (_ bv63 6) ?x9114)) (stack_s x_0 w_2 x_SHA3_0 5 (bvadd (_ bv62 6) ?x4319)))))
 (let (($x1862 (exc_halt_s 5)))
 (let (($x6195 (= $x1862 (or (exc_halt_s 4) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 4))))))))
 (let (($x316 (forall ((w (_ BitVec 256)) )(let ((?x5211 (storage_s x_0 w_2 x_SHA3_0 4 w)))
 (let ((?x897 (storage_s x_0 w_2 x_SHA3_0 5 w)))
 (= ?x897 ?x5211))))
 ))
 (let (($x9802 (forall ((n (_ BitVec 6)) )(let ((?x816 (stack_s x_0 w_2 x_SHA3_0 4 n)))
 (let ((?x10014 (stack_s x_0 w_2 x_SHA3_0 5 n)))
 (let ((?x9433 (sc_s 4)))
 (let ((?x9053 (bvadd (_ bv62 6) ?x9433)))
 (let (($x3536 (bvsle ?x9053 n)))
 (or $x3536 (= ?x10014 ?x816))))))))
 ))
 (let ((?x7141 (used_gas_s x_0 w_2 x_SHA3_0 5)))
 (let ((?x9433 (sc_s 4)))
 (let ((?x9053 (bvadd (_ bv62 6) ?x9433)))
 (let ((?x2512 (stack_s x_0 w_2 x_SHA3_0 4 ?x9053)))
 (let ((?x6083 (bvadd (_ bv63 6) ?x9433)))
 (let ((?x11758 (stack_s x_0 w_2 x_SHA3_0 4 ?x6083)))
 (let (($x9175 (exc_halt_s 4)))
 (let (($x1118 (= $x9175 (or (exc_halt_s 3) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 3))))))))
 (let (($x2797 (forall ((w (_ BitVec 256)) )(let ((?x8342 (storage_s x_0 w_2 x_SHA3_0 3 w)))
 (let ((?x5211 (storage_s x_0 w_2 x_SHA3_0 4 w)))
 (= ?x5211 ?x8342))))
 ))
 (let (($x2483 (forall ((n (_ BitVec 6)) )(let ((?x3851 (sc_s 3)))
 (let ((?x1039 (bvadd (_ bv62 6) ?x3851)))
 (let (($x11348 (bvsle ?x1039 n)))
 (let ((?x8006 (stack_s x_0 w_2 x_SHA3_0 3 n)))
 (let ((?x816 (stack_s x_0 w_2 x_SHA3_0 4 n)))
 (or (= ?x816 ?x8006) $x11348)))))))
 ))
 (let ((?x3851 (sc_s 3)))
 (let (($x9390 (= ?x9433 ?x3851)))
 (let ((?x5093 (used_gas_s x_0 w_2 x_SHA3_0 4)))
 (let (($x11311 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 2)))) (_ bv0 1)))))
 (let (($x10052 (exc_halt_s 2)))
 (let (($x7460 (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 2))))))
 (let (($x8103 (exc_halt_s 3)))
 (let (($x4226 (= $x8103 (or $x7460 $x10052 $x11311))))
 (let (($x2882 (forall ((w (_ BitVec 256)) )(let ((?x11686 (storage_s x_0 w_2 x_SHA3_0 2 w)))
 (let ((?x8342 (storage_s x_0 w_2 x_SHA3_0 3 w)))
 (= ?x8342 ?x11686))))
 ))
 (let (($x8547 (forall ((n (_ BitVec 6)) )(let ((?x2272 (sc_s 2)))
 (let ((?x9266 (bvadd (_ bv62 6) ?x2272)))
 (let (($x9051 (bvsle ?x9266 n)))
 (let ((?x3546 (stack_s x_0 w_2 x_SHA3_0 2 n)))
 (let ((?x8006 (stack_s x_0 w_2 x_SHA3_0 3 n)))
 (or (= ?x8006 ?x3546) $x9051)))))))
 ))
 (let (($x10998 (= ?x3851 (bvadd (_ bv1 6) (sc_s 2)))))
 (let ((?x8144 (used_gas_s x_0 w_2 x_SHA3_0 3)))
 (let ((?x2272 (sc_s 2)))
 (let ((?x5528 (bvadd (_ bv63 6) ?x2272)))
 (let ((?x6098 (stack_s x_0 w_2 x_SHA3_0 2 ?x5528)))
 (let ((?x9266 (bvadd (_ bv62 6) ?x2272)))
 (let ((?x2766 (stack_s x_0 w_2 x_SHA3_0 2 ?x9266)))
 (let (($x6775 (= $x10052 (or (exc_halt_s 1) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 1))))))))
 (let (($x3135 (forall ((w (_ BitVec 256)) )(let ((?x10442 (storage_s x_0 w_2 x_SHA3_0 1 w)))
 (let ((?x11686 (storage_s x_0 w_2 x_SHA3_0 2 w)))
 (= ?x11686 ?x10442))))
 ))
 (let (($x8513 (forall ((n (_ BitVec 6)) )(let ((?x154 (sc_s 1)))
 (let ((?x5195 (bvadd (_ bv62 6) ?x154)))
 (let (($x7363 (bvsle ?x5195 n)))
 (let ((?x11611 (stack_s x_0 w_2 x_SHA3_0 1 n)))
 (let ((?x3546 (stack_s x_0 w_2 x_SHA3_0 2 n)))
 (or (= ?x3546 ?x11611) $x7363)))))))
 ))
 (let ((?x154 (sc_s 1)))
 (let (($x2223 (= ?x2272 ?x154)))
 (let ((?x11404 (used_gas_s x_0 w_2 x_SHA3_0 2)))
 (let (($x8780 (exc_halt_s 1)))
 (let (($x5465 (= $x8780 (or $x57 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x72))) (_ bv0 1)))))))
 (let (($x3063 (forall ((w (_ BitVec 256)) )(let ((?x6603 (storage_s x_0 w_2 x_SHA3_0 0 w)))
 (let ((?x10442 (storage_s x_0 w_2 x_SHA3_0 1 w)))
 (= ?x10442 ?x6603))))
 ))
 (let (($x7347 (forall ((n (_ BitVec 6)) )(let ((?x72 (sc_s 0)))
 (let (($x4347 (bvsle ?x72 n)))
 (let ((?x4373 (stack_s x_0 w_2 x_SHA3_0 0 n)))
 (let ((?x11611 (stack_s x_0 w_2 x_SHA3_0 1 n)))
 (or (= ?x11611 ?x4373) $x4347))))))
 ))
 (let (($x10888 (= ?x154 (bvadd (_ bv1 6) ?x72))))
 (let (($x10169 (forall ((w0 (_ BitVec 256)) (w1 (_ BitVec 256)) )(let (($x3331 (and (= (stack_s x_0 w_2 x_SHA3_0 4 (bvadd (_ bv63 6) (sc_s 4))) w0) (= (stack_s x_0 w_2 x_SHA3_0 4 (bvadd (_ bv62 6) (sc_s 4))) w1))))
 (let ((?x7773 (f_SHA3 x_0 w_2 x_SHA3_0 w0 w1)))
 (= ?x7773 (ite $x3331 x_SHA3_0 (_ bv0 256))))))
 ))
 (let (($x8061 (forall ((w (_ BitVec 256)) )(let ((?x6603 (storage_s x_0 w_2 x_SHA3_0 0 w)))
 (= ?x6603 (_ bv0 256))))
 ))
 (let (($x7359 (= ?x1971 0)))
 (let (($x10995 (not $x57)))
 (let (($x6974 (= (stack_s x_0 w_2 x_SHA3_0 0 (_ bv0 6)) x_0)))
 (let (($x7461 (= ?x72 (_ bv1 6))))
 (and $x7461 $x6974 $x10995 $x7359 $x8061 $x10169 (= (stack_s x_0 w_2 x_SHA3_0 1 ?x72) w_2) (= (used_gas_s x_0 w_2 x_SHA3_0 1) (+ 3 ?x1971)) $x10888 $x7347 $x3063 $x5465 (= ?x6098 (stack_s x_0 w_2 x_SHA3_0 1 (bvadd (_ bv62 6) ?x154))) (= ?x2766 (stack_s x_0 w_2 x_SHA3_0 1 (bvadd (_ bv63 6) ?x154))) (= ?x11404 (+ 3 (used_gas_s x_0 w_2 x_SHA3_0 1))) $x2223 $x8513 $x3135 $x6775 (= (stack_s x_0 w_2 x_SHA3_0 3 (bvadd (_ bv63 6) ?x3851)) ?x2766) (= (stack_s x_0 w_2 x_SHA3_0 3 ?x9266) ?x2766) (= (stack_s x_0 w_2 x_SHA3_0 3 ?x5528) ?x6098) (= ?x8144 (+ 3 ?x11404)) $x10998 $x8547 $x2882 $x4226 (= ?x11758 (stack_s x_0 w_2 x_SHA3_0 3 (bvadd (_ bv62 6) ?x3851))) (= ?x2512 (stack_s x_0 w_2 x_SHA3_0 3 (bvadd (_ bv63 6) ?x3851))) (= ?x5093 (+ 3 ?x8144)) $x9390 $x2483 $x2797 $x1118 (= ?x5957 (f_SHA3 x_0 w_2 x_SHA3_0 ?x11758 ?x2512)) (= ?x7141 (+ 30 ?x5093)) (= ?x4319 ?x6083) $x9802 $x316 $x6195 $x7136 (= (stack_s x_0 w_2 x_SHA3_0 6 (bvadd (_ bv62 6) ?x9114)) ?x5957) $x400 $x5013 $x1735 $x10662 $x6211 (= (stack_t x_0 w_2 x_SHA3_0 1 ?x63) w_2) (= (used_gas_t x_0 w_2 x_SHA3_0 1) (+ 3 ?x5257)) $x9898 $x3962 $x3016 $x3469 (= ?x7771 (stack_t x_0 w_2 x_SHA3_0 1 (bvadd (_ bv62 6) ?x9666))) (= ?x11791 (stack_t x_0 w_2 x_SHA3_0 1 (bvadd (_ bv63 6) ?x9666))) (= ?x7571 (+ 3 (used_gas_t x_0 w_2 x_SHA3_0 1))) $x5665 $x4629 $x7626 $x8101 $x3743 (= ?x3950 (+ 30 ?x7571)) (= (sc_t 3) ?x4064) $x292 $x11274 $x5844 (= (stack_t x_0 w_2 x_SHA3_0 4 (sc_t 3)) w_2) $x6088 $x7253 $x4847 $x6713 $x4304 $x73 $x9165 $x58 $x1794 $x10179 (not (and $x9737 $x6377 $x1652 $x9036)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
 )
(check-sat)
(get-proof)
