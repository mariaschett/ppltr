; SWAP2 SWAP1 SWAP2 ADD SWAP1 PUSH cw_1 => PUSH cw_1 SWAP3 ADD SWAP2
(set-option :produce-proofs true)
; 
(set-info :status unknown)
(declare-fun storage_t ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 256)) (_ BitVec 256))
(declare-fun w_1 () (_ BitVec 256))
(declare-fun storage_s ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 256)) (_ BitVec 256))
(declare-fun exc_halt_t (Int) Bool)
(declare-fun exc_halt_s (Int) Bool)
(declare-fun stack_t ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 6)) (_ BitVec 256))
(declare-fun stack_s ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 6)) (_ BitVec 256))
(declare-fun sc_t (Int) (_ BitVec 6))
(declare-fun sc_s (Int) (_ BitVec 6))
(declare-fun used_gas_t ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int) Int)
(declare-fun used_gas_s ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int) Int)
(assert
 (exists ((x_0 (_ BitVec 256)) (x_1 (_ BitVec 256)) (x_2 (_ BitVec 256)) )(let (($x1447 (forall ((w (_ BitVec 256)) )(let ((?x5132 (storage_t x_0 x_1 x_2 w_1 4 w)))
 (let ((?x4307 (storage_s x_0 x_1 x_2 w_1 6 w)))
 (= ?x4307 ?x5132))))
 ))
 (let (($x3723 (exc_halt_t 4)))
 (let (($x772 (exc_halt_s 6)))
 (let (($x10342 (= $x772 $x3723)))
 (let (($x8603 (forall ((n (_ BitVec 6)) )(let ((?x506 (stack_t x_0 x_1 x_2 w_1 4 n)))
 (let ((?x648 (stack_s x_0 x_1 x_2 w_1 6 n)))
 (let (($x10606 (= ?x648 ?x506)))
 (or (bvsle (sc_t 4) n) $x10606)))))
 ))
 (let ((?x1098 (sc_t 4)))
 (let ((?x926 (sc_s 6)))
 (let (($x1988 (= ?x926 ?x1098)))
 (let ((?x6736 (used_gas_t x_0 x_1 x_2 w_1 0)))
 (let ((?x1165 (used_gas_s x_0 x_1 x_2 w_1 0)))
 (let (($x8446 (= ?x1165 ?x6736)))
 (let (($x4113 (forall ((w (_ BitVec 256)) )(let ((?x10161 (storage_t x_0 x_1 x_2 w_1 0 w)))
 (let ((?x1828 (storage_s x_0 x_1 x_2 w_1 0 w)))
 (= ?x1828 ?x10161))))
 ))
 (let (($x56 (exc_halt_t 0)))
 (let (($x57 (exc_halt_s 0)))
 (let (($x58 (= $x57 $x56)))
 (let (($x11624 (forall ((n (_ BitVec 6)) )(let ((?x63 (sc_t 0)))
 (let (($x9737 (bvsle ?x63 n)))
 (let ((?x11218 (stack_t x_0 x_1 x_2 w_1 0 n)))
 (let ((?x1585 (stack_s x_0 x_1 x_2 w_1 0 n)))
 (let (($x9493 (= ?x1585 ?x11218)))
 (or $x9493 $x9737)))))))
 ))
 (let ((?x63 (sc_t 0)))
 (let ((?x72 (sc_s 0)))
 (let (($x73 (= ?x72 ?x63)))
 (let (($x3767 (= $x3723 (or (exc_halt_t 3) (not (bvsle (_ bv0 6) (bvadd (_ bv61 6) (sc_t 3))))))))
 (let (($x7790 (forall ((w (_ BitVec 256)) )(let ((?x10461 (storage_t x_0 x_1 x_2 w_1 3 w)))
 (let ((?x5132 (storage_t x_0 x_1 x_2 w_1 4 w)))
 (= ?x5132 ?x10461))))
 ))
 (let (($x7850 (forall ((n (_ BitVec 6)) )(let ((?x7689 (stack_t x_0 x_1 x_2 w_1 3 n)))
 (let ((?x506 (stack_t x_0 x_1 x_2 w_1 4 n)))
 (or (= ?x506 ?x7689) (bvsle (bvadd (_ bv61 6) (sc_t 3)) n)))))
 ))
 (let ((?x6438 (sc_t 3)))
 (let (($x4743 (= ?x1098 ?x6438)))
 (let (($x5777 (= (used_gas_t x_0 x_1 x_2 w_1 4) (+ 3 (used_gas_t x_0 x_1 x_2 w_1 3)))))
 (let (($x2809 (= (stack_t x_0 x_1 x_2 w_1 4 (bvadd (_ bv62 6) ?x1098)) (stack_t x_0 x_1 x_2 w_1 3 (bvadd (_ bv62 6) ?x6438)))))
 (let ((?x6579 (bvadd (_ bv63 6) ?x6438)))
 (let ((?x7900 (stack_t x_0 x_1 x_2 w_1 3 ?x6579)))
 (let (($x8501 (= (stack_t x_0 x_1 x_2 w_1 4 (bvadd (_ bv63 6) ?x1098)) (stack_t x_0 x_1 x_2 w_1 3 (bvadd (_ bv61 6) ?x6438)))))
 (let (($x6783 (exc_halt_t 3)))
 (let (($x3918 (= $x6783 (or (exc_halt_t 2) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_t 2))))))))
 (let (($x11184 (forall ((w (_ BitVec 256)) )(let ((?x4155 (storage_t x_0 x_1 x_2 w_1 2 w)))
 (let ((?x10461 (storage_t x_0 x_1 x_2 w_1 3 w)))
 (= ?x10461 ?x4155))))
 ))
 (let (($x11126 (forall ((n (_ BitVec 6)) )(let ((?x893 (stack_t x_0 x_1 x_2 w_1 2 n)))
 (let ((?x7689 (stack_t x_0 x_1 x_2 w_1 3 n)))
 (let ((?x2714 (sc_t 2)))
 (let ((?x1614 (bvadd (_ bv62 6) ?x2714)))
 (let (($x6008 (bvsle ?x1614 n)))
 (or $x6008 (= ?x7689 ?x893))))))))
 ))
 (let ((?x2714 (sc_t 2)))
 (let ((?x11413 (bvadd (_ bv63 6) ?x2714)))
 (let (($x4652 (= ?x6438 ?x11413)))
 (let ((?x2763 (used_gas_t x_0 x_1 x_2 w_1 3)))
 (let ((?x1614 (bvadd (_ bv62 6) ?x2714)))
 (let ((?x8816 (stack_t x_0 x_1 x_2 w_1 2 ?x1614)))
 (let ((?x608 (stack_t x_0 x_1 x_2 w_1 2 ?x11413)))
 (let (($x5252 (exc_halt_t 2)))
 (let (($x10844 (= $x5252 (or (exc_halt_t 1) (not (bvsle (_ bv0 6) (bvadd (_ bv60 6) (sc_t 1))))))))
 (let (($x3442 (forall ((w (_ BitVec 256)) )(let ((?x7136 (storage_t x_0 x_1 x_2 w_1 1 w)))
 (let ((?x4155 (storage_t x_0 x_1 x_2 w_1 2 w)))
 (= ?x4155 ?x7136))))
 ))
 (let (($x4673 (forall ((n (_ BitVec 6)) )(let ((?x3014 (stack_t x_0 x_1 x_2 w_1 1 n)))
 (let ((?x893 (stack_t x_0 x_1 x_2 w_1 2 n)))
 (or (bvsle (bvadd (_ bv60 6) (sc_t 1)) n) (= ?x893 ?x3014)))))
 ))
 (let ((?x5151 (sc_t 1)))
 (let (($x7556 (= ?x2714 ?x5151)))
 (let ((?x3075 (used_gas_t x_0 x_1 x_2 w_1 2)))
 (let (($x10446 (= (stack_t x_0 x_1 x_2 w_1 2 (bvadd (_ bv61 6) ?x2714)) (stack_t x_0 x_1 x_2 w_1 1 (bvadd (_ bv61 6) ?x5151)))))
 (let (($x10025 (= (stack_t x_0 x_1 x_2 w_1 2 (bvadd (_ bv60 6) ?x2714)) (stack_t x_0 x_1 x_2 w_1 1 (bvadd (_ bv63 6) ?x5151)))))
 (let (($x6122 (exc_halt_t 1)))
 (let (($x1753 (= $x6122 (or $x56 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x63))) (_ bv0 1)))))))
 (let (($x285 (forall ((w (_ BitVec 256)) )(let ((?x10161 (storage_t x_0 x_1 x_2 w_1 0 w)))
 (let ((?x7136 (storage_t x_0 x_1 x_2 w_1 1 w)))
 (= ?x7136 ?x10161))))
 ))
 (let (($x7521 (forall ((n (_ BitVec 6)) )(let ((?x63 (sc_t 0)))
 (let (($x9737 (bvsle ?x63 n)))
 (let ((?x11218 (stack_t x_0 x_1 x_2 w_1 0 n)))
 (let ((?x3014 (stack_t x_0 x_1 x_2 w_1 1 n)))
 (or (= ?x3014 ?x11218) $x9737))))))
 ))
 (let (($x10883 (= ?x5151 (bvadd (_ bv1 6) ?x63))))
 (let (($x11317 (exc_halt_s 5)))
 (let (($x412 (or $x11317 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 5)))) (_ bv0 1))))))
 (let (($x5718 (forall ((w (_ BitVec 256)) )(let ((?x4792 (storage_s x_0 x_1 x_2 w_1 5 w)))
 (let ((?x4307 (storage_s x_0 x_1 x_2 w_1 6 w)))
 (= ?x4307 ?x4792))))
 ))
 (let (($x550 (forall ((n (_ BitVec 6)) )(let ((?x2653 (stack_s x_0 x_1 x_2 w_1 5 n)))
 (let ((?x648 (stack_s x_0 x_1 x_2 w_1 6 n)))
 (or (= ?x648 ?x2653) (bvsle (sc_s 5) n)))))
 ))
 (let (($x10661 (= (used_gas_s x_0 x_1 x_2 w_1 6) (+ 3 (used_gas_s x_0 x_1 x_2 w_1 5)))))
 (let (($x2852 (= $x11317 (or (exc_halt_s 4) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 4))))))))
 (let (($x8097 (forall ((w (_ BitVec 256)) )(let ((?x2881 (storage_s x_0 x_1 x_2 w_1 4 w)))
 (let ((?x4792 (storage_s x_0 x_1 x_2 w_1 5 w)))
 (= ?x4792 ?x2881))))
 ))
 (let (($x1184 (forall ((n (_ BitVec 6)) )(let ((?x4305 (sc_s 4)))
 (let ((?x9058 (bvadd (_ bv62 6) ?x4305)))
 (let (($x4752 (bvsle ?x9058 n)))
 (let ((?x4596 (stack_s x_0 x_1 x_2 w_1 4 n)))
 (let ((?x2653 (stack_s x_0 x_1 x_2 w_1 5 n)))
 (or (= ?x2653 ?x4596) $x4752)))))))
 ))
 (let ((?x8309 (used_gas_s x_0 x_1 x_2 w_1 5)))
 (let ((?x4305 (sc_s 4)))
 (let ((?x4445 (bvadd (_ bv63 6) ?x4305)))
 (let ((?x9110 (stack_s x_0 x_1 x_2 w_1 4 ?x4445)))
 (let (($x4253 (= (stack_s x_0 x_1 x_2 w_1 5 (bvadd (_ bv62 6) (sc_s 5))) ?x9110)))
 (let (($x4156 (= (stack_s x_0 x_1 x_2 w_1 5 (bvadd (_ bv63 6) (sc_s 5))) (stack_s x_0 x_1 x_2 w_1 4 (bvadd (_ bv62 6) ?x4305)))))
 (let (($x7172 (exc_halt_s 4)))
 (let (($x6996 (= $x7172 (or (exc_halt_s 3) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 3))))))))
 (let (($x8064 (forall ((w (_ BitVec 256)) )(let ((?x7192 (storage_s x_0 x_1 x_2 w_1 3 w)))
 (let ((?x2881 (storage_s x_0 x_1 x_2 w_1 4 w)))
 (= ?x2881 ?x7192))))
 ))
 (let (($x1987 (forall ((n (_ BitVec 6)) )(let ((?x6611 (stack_s x_0 x_1 x_2 w_1 3 n)))
 (let ((?x4596 (stack_s x_0 x_1 x_2 w_1 4 n)))
 (let ((?x275 (sc_s 3)))
 (let ((?x7426 (bvadd (_ bv62 6) ?x275)))
 (let (($x7529 (bvsle ?x7426 n)))
 (or $x7529 (= ?x4596 ?x6611))))))))
 ))
 (let ((?x275 (sc_s 3)))
 (let ((?x8928 (bvadd (_ bv63 6) ?x275)))
 (let (($x3092 (= ?x4305 ?x8928)))
 (let ((?x5401 (used_gas_s x_0 x_1 x_2 w_1 4)))
 (let ((?x7426 (bvadd (_ bv62 6) ?x275)))
 (let ((?x2155 (stack_s x_0 x_1 x_2 w_1 3 ?x7426)))
 (let ((?x6787 (stack_s x_0 x_1 x_2 w_1 3 ?x8928)))
 (let (($x292 (exc_halt_s 3)))
 (let (($x6958 (= $x292 (or (exc_halt_s 2) (not (bvsle (_ bv0 6) (bvadd (_ bv61 6) (sc_s 2))))))))
 (let (($x8424 (forall ((w (_ BitVec 256)) )(let ((?x6433 (storage_s x_0 x_1 x_2 w_1 2 w)))
 (let ((?x7192 (storage_s x_0 x_1 x_2 w_1 3 w)))
 (= ?x7192 ?x6433))))
 ))
 (let (($x3748 (forall ((n (_ BitVec 6)) )(let ((?x4511 (stack_s x_0 x_1 x_2 w_1 2 n)))
 (let ((?x6611 (stack_s x_0 x_1 x_2 w_1 3 n)))
 (or (bvsle (bvadd (_ bv61 6) (sc_s 2)) n) (= ?x6611 ?x4511)))))
 ))
 (let ((?x218 (sc_s 2)))
 (let (($x2859 (= ?x275 ?x218)))
 (let ((?x867 (used_gas_s x_0 x_1 x_2 w_1 3)))
 (let ((?x9157 (bvadd (_ bv63 6) ?x218)))
 (let ((?x5657 (stack_s x_0 x_1 x_2 w_1 2 ?x9157)))
 (let (($x247 (exc_halt_s 2)))
 (let (($x9404 (= $x247 (or (exc_halt_s 1) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 1))))))))
 (let (($x10778 (forall ((w (_ BitVec 256)) )(let ((?x8833 (storage_s x_0 x_1 x_2 w_1 1 w)))
 (let ((?x6433 (storage_s x_0 x_1 x_2 w_1 2 w)))
 (= ?x6433 ?x8833))))
 ))
 (let (($x7780 (forall ((n (_ BitVec 6)) )(let ((?x154 (sc_s 1)))
 (let ((?x11153 (bvadd (_ bv62 6) ?x154)))
 (let (($x1618 (bvsle ?x11153 n)))
 (let ((?x4721 (stack_s x_0 x_1 x_2 w_1 1 n)))
 (let ((?x4511 (stack_s x_0 x_1 x_2 w_1 2 n)))
 (or (= ?x4511 ?x4721) $x1618)))))))
 ))
 (let ((?x154 (sc_s 1)))
 (let (($x9895 (= ?x218 ?x154)))
 (let ((?x6255 (used_gas_s x_0 x_1 x_2 w_1 2)))
 (let ((?x93 (bvadd (_ bv63 6) ?x154)))
 (let ((?x8141 (stack_s x_0 x_1 x_2 w_1 1 ?x93)))
 (let ((?x4873 (bvadd (_ bv62 6) ?x218)))
 (let ((?x3445 (stack_s x_0 x_1 x_2 w_1 2 ?x4873)))
 (let (($x8009 (forall ((w (_ BitVec 256)) )(let ((?x1828 (storage_s x_0 x_1 x_2 w_1 0 w)))
 (let ((?x8833 (storage_s x_0 x_1 x_2 w_1 1 w)))
 (= ?x8833 ?x1828))))
 ))
 (let (($x10694 (forall ((n (_ BitVec 6)) )(let ((?x1585 (stack_s x_0 x_1 x_2 w_1 0 n)))
 (let ((?x4721 (stack_s x_0 x_1 x_2 w_1 1 n)))
 (or (bvsle (bvadd (_ bv61 6) (sc_s 0)) n) (= ?x4721 ?x1585)))))
 ))
 (let (($x11070 (= ?x154 ?x72)))
 (let ((?x11153 (bvadd (_ bv62 6) ?x154)))
 (let ((?x8140 (stack_s x_0 x_1 x_2 w_1 1 ?x11153)))
 (let (($x7029 (= (stack_s x_0 x_1 x_2 w_1 1 (bvadd (_ bv61 6) ?x154)) (stack_s x_0 x_1 x_2 w_1 0 (bvadd (_ bv63 6) ?x72)))))
 (let (($x3318 (forall ((w (_ BitVec 256)) )(let ((?x1828 (storage_s x_0 x_1 x_2 w_1 0 w)))
 (= ?x1828 (_ bv0 256))))
 ))
 (let (($x9334 (= ?x1165 0)))
 (let (($x3082 (not $x57)))
 (let (($x9752 (= (stack_s x_0 x_1 x_2 w_1 0 (_ bv2 6)) x_2)))
 (let (($x6537 (= (stack_s x_0 x_1 x_2 w_1 0 (_ bv1 6)) x_1)))
 (let (($x5599 (= (stack_s x_0 x_1 x_2 w_1 0 (_ bv0 6)) x_0)))
 (let (($x5315 (= ?x72 (_ bv3 6))))
 (and $x5315 $x5599 $x6537 $x9752 $x3082 $x9334 $x3318 (= ?x8141 (stack_s x_0 x_1 x_2 w_1 0 (bvadd (_ bv61 6) ?x72))) $x7029 (= ?x8140 (stack_s x_0 x_1 x_2 w_1 0 (bvadd (_ bv62 6) ?x72))) (= (used_gas_s x_0 x_1 x_2 w_1 1) (+ 3 ?x1165)) $x11070 $x10694 $x8009 (= (exc_halt_s 1) (or $x57 (not (bvsle (_ bv0 6) (bvadd (_ bv61 6) ?x72))))) (= ?x5657 ?x8140) (= ?x3445 ?x8141) (= ?x6255 (+ 3 (used_gas_s x_0 x_1 x_2 w_1 1))) $x9895 $x7780 $x10778 $x9404 (= ?x6787 (stack_s x_0 x_1 x_2 w_1 2 (bvadd (_ bv61 6) ?x218))) (= (stack_s x_0 x_1 x_2 w_1 3 (bvadd (_ bv61 6) ?x275)) ?x5657) (= ?x2155 ?x3445) (= ?x867 (+ 3 ?x6255)) $x2859 $x3748 $x8424 $x6958 (= ?x9110 (bvadd ?x6787 ?x2155)) (= ?x5401 (+ 3 ?x867)) $x3092 $x1987 $x8064 $x6996 $x4156 $x4253 (= ?x8309 (+ 3 ?x5401)) (= (sc_s 5) ?x4305) $x1184 $x8097 $x2852 (= (stack_s x_0 x_1 x_2 w_1 6 (sc_s 5)) w_1) $x10661 (= ?x926 (bvadd (_ bv1 6) (sc_s 5))) $x550 $x5718 (= $x772 $x412) (= (stack_t x_0 x_1 x_2 w_1 1 ?x63) w_1) (= (used_gas_t x_0 x_1 x_2 w_1 1) (+ 3 ?x6736)) $x10883 $x7521 $x285 $x1753 (= ?x608 (stack_t x_0 x_1 x_2 w_1 1 (bvadd (_ bv60 6) ?x5151))) $x10025 $x10446 (= ?x8816 (stack_t x_0 x_1 x_2 w_1 1 (bvadd (_ bv62 6) ?x5151))) (= ?x3075 (+ 3 (used_gas_t x_0 x_1 x_2 w_1 1))) $x7556 $x4673 $x3442 $x10844 (= ?x7900 (bvadd ?x608 ?x8816)) (= ?x2763 (+ 3 ?x3075)) $x4652 $x11126 $x11184 $x3918 $x8501 (= (stack_t x_0 x_1 x_2 w_1 4 (bvadd (_ bv61 6) ?x1098)) ?x7900) $x2809 $x5777 $x4743 $x7850 $x7790 $x3767 $x73 $x11624 $x58 $x4113 $x8446 (not (and $x1988 $x8603 $x10342 $x1447))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
 )
(check-sat)
(get-proof)
