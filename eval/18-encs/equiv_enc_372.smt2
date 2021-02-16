; PUSH cw_1 SWAP4 POP SWAP2 DUP3 SWAP1 => DUP1 SWAP3 PUSH cw_1 SWAP5 POP
(set-option :produce-proofs true)
; 
(set-info :status unknown)
(declare-fun storage_t ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 256)) (_ BitVec 256))
(declare-fun w_1 () (_ BitVec 256))
(declare-fun storage_s ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 256)) (_ BitVec 256))
(declare-fun exc_halt_t (Int) Bool)
(declare-fun exc_halt_s (Int) Bool)
(declare-fun stack_t ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 6)) (_ BitVec 256))
(declare-fun stack_s ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 6)) (_ BitVec 256))
(declare-fun sc_t (Int) (_ BitVec 6))
(declare-fun sc_s (Int) (_ BitVec 6))
(declare-fun used_gas_t ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int) Int)
(declare-fun used_gas_s ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int) Int)
(assert
 (exists ((x_0 (_ BitVec 256)) (x_1 (_ BitVec 256)) (x_2 (_ BitVec 256)) (x_3 (_ BitVec 256)) )(let (($x9689 (forall ((w (_ BitVec 256)) )(let ((?x9052 (storage_t x_0 x_1 x_2 x_3 w_1 5 w)))
 (let ((?x3553 (storage_s x_0 x_1 x_2 x_3 w_1 6 w)))
 (= ?x3553 ?x9052))))
 ))
 (let (($x886 (exc_halt_t 5)))
 (let (($x772 (exc_halt_s 6)))
 (let (($x889 (= $x772 $x886)))
 (let (($x7025 (forall ((n (_ BitVec 6)) )(let ((?x3353 (stack_t x_0 x_1 x_2 x_3 w_1 5 n)))
 (let ((?x7916 (stack_s x_0 x_1 x_2 x_3 w_1 6 n)))
 (let (($x10249 (= ?x7916 ?x3353)))
 (let ((?x919 (sc_t 5)))
 (let (($x7712 (bvsle ?x919 n)))
 (or $x7712 $x10249)))))))
 ))
 (let ((?x919 (sc_t 5)))
 (let ((?x926 (sc_s 6)))
 (let (($x929 (= ?x926 ?x919)))
 (let ((?x10718 (used_gas_t x_0 x_1 x_2 x_3 w_1 0)))
 (let ((?x5458 (used_gas_s x_0 x_1 x_2 x_3 w_1 0)))
 (let (($x3965 (= ?x5458 ?x10718)))
 (let (($x11448 (forall ((w (_ BitVec 256)) )(let ((?x1907 (storage_t x_0 x_1 x_2 x_3 w_1 0 w)))
 (let ((?x6035 (storage_s x_0 x_1 x_2 x_3 w_1 0 w)))
 (= ?x6035 ?x1907))))
 ))
 (let (($x56 (exc_halt_t 0)))
 (let (($x57 (exc_halt_s 0)))
 (let (($x58 (= $x57 $x56)))
 (let (($x5728 (forall ((n (_ BitVec 6)) )(let ((?x63 (sc_t 0)))
 (let (($x4866 (bvsle ?x63 n)))
 (let ((?x11627 (stack_t x_0 x_1 x_2 x_3 w_1 0 n)))
 (let ((?x6759 (stack_s x_0 x_1 x_2 x_3 w_1 0 n)))
 (let (($x2826 (= ?x6759 ?x11627)))
 (or $x2826 $x4866)))))))
 ))
 (let ((?x63 (sc_t 0)))
 (let ((?x72 (sc_s 0)))
 (let (($x73 (= ?x72 ?x63)))
 (let (($x3959 (= $x886 (or (exc_halt_t 4) (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_t 4))))))))
 (let (($x3920 (forall ((w (_ BitVec 256)) )(let ((?x6833 (storage_t x_0 x_1 x_2 x_3 w_1 4 w)))
 (let ((?x9052 (storage_t x_0 x_1 x_2 x_3 w_1 5 w)))
 (= ?x9052 ?x6833))))
 ))
 (let (($x3215 (forall ((n (_ BitVec 6)) )(let ((?x3784 (stack_t x_0 x_1 x_2 x_3 w_1 4 n)))
 (let ((?x3353 (stack_t x_0 x_1 x_2 x_3 w_1 5 n)))
 (or (= ?x3353 ?x3784) (bvsle (bvadd (_ bv63 6) (sc_t 4)) n)))))
 ))
 (let (($x2593 (= (used_gas_t x_0 x_1 x_2 x_3 w_1 5) (+ 2 (used_gas_t x_0 x_1 x_2 x_3 w_1 4)))))
 (let (($x3723 (exc_halt_t 4)))
 (let (($x2209 (= $x3723 (or (exc_halt_t 3) (not (bvsle (_ bv0 6) (bvadd (_ bv58 6) (sc_t 3))))))))
 (let (($x9823 (forall ((w (_ BitVec 256)) )(let ((?x10790 (storage_t x_0 x_1 x_2 x_3 w_1 3 w)))
 (let ((?x6833 (storage_t x_0 x_1 x_2 x_3 w_1 4 w)))
 (= ?x6833 ?x10790))))
 ))
 (let (($x37 (forall ((n (_ BitVec 6)) )(let ((?x1880 (stack_t x_0 x_1 x_2 x_3 w_1 3 n)))
 (let ((?x3784 (stack_t x_0 x_1 x_2 x_3 w_1 4 n)))
 (or (= ?x3784 ?x1880) (bvsle (bvadd (_ bv58 6) (sc_t 3)) n)))))
 ))
 (let ((?x6438 (sc_t 3)))
 (let ((?x1098 (sc_t 4)))
 (let (($x1038 (= ?x1098 ?x6438)))
 (let ((?x1643 (used_gas_t x_0 x_1 x_2 x_3 w_1 4)))
 (let (($x1928 (= (stack_t x_0 x_1 x_2 x_3 w_1 4 (bvadd (_ bv62 6) ?x1098)) (stack_t x_0 x_1 x_2 x_3 w_1 3 (bvadd (_ bv62 6) ?x6438)))))
 (let (($x3828 (= (stack_t x_0 x_1 x_2 x_3 w_1 4 (bvadd (_ bv61 6) ?x1098)) (stack_t x_0 x_1 x_2 x_3 w_1 3 (bvadd (_ bv61 6) ?x6438)))))
 (let (($x3585 (= (stack_t x_0 x_1 x_2 x_3 w_1 4 (bvadd (_ bv60 6) ?x1098)) (stack_t x_0 x_1 x_2 x_3 w_1 3 (bvadd (_ bv60 6) ?x6438)))))
 (let (($x23 (= (stack_t x_0 x_1 x_2 x_3 w_1 4 (bvadd (_ bv59 6) ?x1098)) (stack_t x_0 x_1 x_2 x_3 w_1 3 (bvadd (_ bv59 6) ?x6438)))))
 (let (($x3474 (= (stack_t x_0 x_1 x_2 x_3 w_1 4 (bvadd (_ bv58 6) ?x1098)) (stack_t x_0 x_1 x_2 x_3 w_1 3 (bvadd (_ bv63 6) ?x6438)))))
 (let (($x4551 (= (stack_t x_0 x_1 x_2 x_3 w_1 4 (bvadd (_ bv63 6) ?x1098)) (stack_t x_0 x_1 x_2 x_3 w_1 3 (bvadd (_ bv58 6) ?x6438)))))
 (let (($x5252 (exc_halt_t 2)))
 (let (($x8475 (or $x5252 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_t 2)))) (_ bv0 1))))))
 (let (($x6783 (exc_halt_t 3)))
 (let (($x10277 (= $x6783 $x8475)))
 (let (($x2558 (forall ((w (_ BitVec 256)) )(let ((?x8671 (storage_t x_0 x_1 x_2 x_3 w_1 2 w)))
 (let ((?x10790 (storage_t x_0 x_1 x_2 x_3 w_1 3 w)))
 (= ?x10790 ?x8671))))
 ))
 (let (($x11681 (forall ((n (_ BitVec 6)) )(let ((?x2726 (stack_t x_0 x_1 x_2 x_3 w_1 2 n)))
 (let ((?x1880 (stack_t x_0 x_1 x_2 x_3 w_1 3 n)))
 (let ((?x2714 (sc_t 2)))
 (let (($x6039 (bvsle ?x2714 n)))
 (or $x6039 (= ?x1880 ?x2726)))))))
 ))
 (let (($x1233 (= ?x6438 (bvadd (_ bv1 6) (sc_t 2)))))
 (let ((?x1368 (used_gas_t x_0 x_1 x_2 x_3 w_1 3)))
 (let (($x2918 (= $x5252 (or (exc_halt_t 1) (not (bvsle (_ bv0 6) (bvadd (_ bv60 6) (sc_t 1))))))))
 (let (($x11787 (forall ((w (_ BitVec 256)) )(let ((?x6843 (storage_t x_0 x_1 x_2 x_3 w_1 1 w)))
 (let ((?x8671 (storage_t x_0 x_1 x_2 x_3 w_1 2 w)))
 (= ?x8671 ?x6843))))
 ))
 (let (($x3265 (forall ((n (_ BitVec 6)) )(let ((?x8347 (sc_t 1)))
 (let ((?x8435 (bvadd (_ bv60 6) ?x8347)))
 (let (($x3728 (bvsle ?x8435 n)))
 (let ((?x9930 (stack_t x_0 x_1 x_2 x_3 w_1 1 n)))
 (let ((?x2726 (stack_t x_0 x_1 x_2 x_3 w_1 2 n)))
 (or (= ?x2726 ?x9930) $x3728)))))))
 ))
 (let ((?x8347 (sc_t 1)))
 (let ((?x2714 (sc_t 2)))
 (let (($x11805 (= ?x2714 ?x8347)))
 (let ((?x10750 (used_gas_t x_0 x_1 x_2 x_3 w_1 2)))
 (let (($x1040 (= (stack_t x_0 x_1 x_2 x_3 w_1 2 (bvadd (_ bv62 6) ?x2714)) (stack_t x_0 x_1 x_2 x_3 w_1 1 (bvadd (_ bv62 6) ?x8347)))))
 (let (($x9027 (= (stack_t x_0 x_1 x_2 x_3 w_1 2 (bvadd (_ bv61 6) ?x2714)) (stack_t x_0 x_1 x_2 x_3 w_1 1 (bvadd (_ bv61 6) ?x8347)))))
 (let ((?x4288 (bvadd (_ bv63 6) ?x8347)))
 (let ((?x3527 (stack_t x_0 x_1 x_2 x_3 w_1 1 ?x4288)))
 (let (($x4161 (= (stack_t x_0 x_1 x_2 x_3 w_1 2 (bvadd (_ bv63 6) ?x2714)) (stack_t x_0 x_1 x_2 x_3 w_1 1 (bvadd (_ bv60 6) ?x8347)))))
 (let (($x4779 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x63))) (_ bv0 1)))))
 (let (($x1247 (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) ?x63)))))
 (let (($x3508 (exc_halt_t 1)))
 (let (($x8450 (= $x3508 (or $x56 $x1247 $x4779))))
 (let (($x3050 (forall ((w (_ BitVec 256)) )(let ((?x1907 (storage_t x_0 x_1 x_2 x_3 w_1 0 w)))
 (let ((?x6843 (storage_t x_0 x_1 x_2 x_3 w_1 1 w)))
 (= ?x6843 ?x1907))))
 ))
 (let (($x7194 (forall ((n (_ BitVec 6)) )(let ((?x63 (sc_t 0)))
 (let ((?x2902 (bvadd (_ bv63 6) ?x63)))
 (let (($x5501 (bvsle ?x2902 n)))
 (let ((?x11627 (stack_t x_0 x_1 x_2 x_3 w_1 0 n)))
 (let ((?x9930 (stack_t x_0 x_1 x_2 x_3 w_1 1 n)))
 (or (= ?x9930 ?x11627) $x5501)))))))
 ))
 (let (($x9556 (= ?x8347 (bvadd (_ bv1 6) ?x63))))
 (let ((?x2902 (bvadd (_ bv63 6) ?x63)))
 (let ((?x7578 (stack_t x_0 x_1 x_2 x_3 w_1 0 ?x2902)))
 (let (($x7843 (= $x772 (or (exc_halt_s 5) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 5))))))))
 (let (($x10112 (forall ((w (_ BitVec 256)) )(let ((?x5724 (storage_s x_0 x_1 x_2 x_3 w_1 5 w)))
 (let ((?x3553 (storage_s x_0 x_1 x_2 x_3 w_1 6 w)))
 (= ?x3553 ?x5724))))
 ))
 (let (($x232 (forall ((n (_ BitVec 6)) )(let ((?x4319 (sc_s 5)))
 (let ((?x591 (bvadd (_ bv62 6) ?x4319)))
 (let (($x4237 (bvsle ?x591 n)))
 (let ((?x7253 (stack_s x_0 x_1 x_2 x_3 w_1 5 n)))
 (let ((?x7916 (stack_s x_0 x_1 x_2 x_3 w_1 6 n)))
 (or (= ?x7916 ?x7253) $x4237)))))))
 ))
 (let ((?x4319 (sc_s 5)))
 (let (($x9018 (= ?x926 ?x4319)))
 (let (($x4816 (= (used_gas_s x_0 x_1 x_2 x_3 w_1 6) (+ 3 (used_gas_s x_0 x_1 x_2 x_3 w_1 5)))))
 (let ((?x3898 (bvadd (_ bv63 6) ?x4319)))
 (let ((?x9753 (stack_s x_0 x_1 x_2 x_3 w_1 5 ?x3898)))
 (let (($x2995 (= (stack_s x_0 x_1 x_2 x_3 w_1 6 (bvadd (_ bv63 6) ?x926)) (stack_s x_0 x_1 x_2 x_3 w_1 5 (bvadd (_ bv62 6) ?x4319)))))
 (let (($x11222 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 4)))) (_ bv0 1)))))
 (let (($x8439 (not (bvsle (_ bv0 6) (bvadd (_ bv61 6) (sc_s 4))))))
 (let (($x7172 (exc_halt_s 4)))
 (let (($x11317 (exc_halt_s 5)))
 (let (($x6829 (forall ((w (_ BitVec 256)) )(let ((?x9553 (storage_s x_0 x_1 x_2 x_3 w_1 4 w)))
 (let ((?x5724 (storage_s x_0 x_1 x_2 x_3 w_1 5 w)))
 (= ?x5724 ?x9553))))
 ))
 (let (($x1126 (forall ((n (_ BitVec 6)) )(let ((?x4305 (sc_s 4)))
 (let ((?x1731 (bvadd (_ bv61 6) ?x4305)))
 (let (($x10480 (bvsle ?x1731 n)))
 (let ((?x3972 (stack_s x_0 x_1 x_2 x_3 w_1 4 n)))
 (let ((?x7253 (stack_s x_0 x_1 x_2 x_3 w_1 5 n)))
 (or (= ?x7253 ?x3972) $x10480)))))))
 ))
 (let (($x6140 (= ?x4319 (bvadd (_ bv1 6) (sc_s 4)))))
 (let ((?x9013 (used_gas_s x_0 x_1 x_2 x_3 w_1 5)))
 (let ((?x4305 (sc_s 4)))
 (let ((?x9783 (bvadd (_ bv63 6) ?x4305)))
 (let ((?x3051 (stack_s x_0 x_1 x_2 x_3 w_1 4 ?x9783)))
 (let ((?x9777 (bvadd (_ bv62 6) ?x4305)))
 (let ((?x5771 (stack_s x_0 x_1 x_2 x_3 w_1 4 ?x9777)))
 (let ((?x1731 (bvadd (_ bv61 6) ?x4305)))
 (let ((?x1259 (stack_s x_0 x_1 x_2 x_3 w_1 4 ?x1731)))
 (let (($x3658 (= $x7172 (or (exc_halt_s 3) (not (bvsle (_ bv0 6) (bvadd (_ bv61 6) (sc_s 3))))))))
 (let (($x4233 (forall ((w (_ BitVec 256)) )(let ((?x10576 (storage_s x_0 x_1 x_2 x_3 w_1 3 w)))
 (let ((?x9553 (storage_s x_0 x_1 x_2 x_3 w_1 4 w)))
 (= ?x9553 ?x10576))))
 ))
 (let (($x35 (forall ((n (_ BitVec 6)) )(let ((?x275 (sc_s 3)))
 (let ((?x6436 (bvadd (_ bv61 6) ?x275)))
 (let (($x8594 (bvsle ?x6436 n)))
 (let ((?x6639 (stack_s x_0 x_1 x_2 x_3 w_1 3 n)))
 (let ((?x3972 (stack_s x_0 x_1 x_2 x_3 w_1 4 n)))
 (or (= ?x3972 ?x6639) $x8594)))))))
 ))
 (let ((?x275 (sc_s 3)))
 (let (($x9884 (= ?x4305 ?x275)))
 (let ((?x6335 (used_gas_s x_0 x_1 x_2 x_3 w_1 4)))
 (let (($x292 (exc_halt_s 3)))
 (let (($x3804 (= $x292 (or (exc_halt_s 2) (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_s 2))))))))
 (let (($x9107 (forall ((w (_ BitVec 256)) )(let ((?x4494 (storage_s x_0 x_1 x_2 x_3 w_1 2 w)))
 (let ((?x10576 (storage_s x_0 x_1 x_2 x_3 w_1 3 w)))
 (= ?x10576 ?x4494))))
 ))
 (let (($x6091 (forall ((n (_ BitVec 6)) )(let ((?x218 (sc_s 2)))
 (let ((?x5789 (bvadd (_ bv63 6) ?x218)))
 (let (($x8451 (bvsle ?x5789 n)))
 (let ((?x9406 (stack_s x_0 x_1 x_2 x_3 w_1 2 n)))
 (let ((?x6639 (stack_s x_0 x_1 x_2 x_3 w_1 3 n)))
 (or (= ?x6639 ?x9406) $x8451)))))))
 ))
 (let ((?x218 (sc_s 2)))
 (let ((?x5789 (bvadd (_ bv63 6) ?x218)))
 (let (($x4278 (= ?x275 ?x5789)))
 (let ((?x1601 (used_gas_s x_0 x_1 x_2 x_3 w_1 3)))
 (let (($x247 (exc_halt_s 2)))
 (let (($x7395 (= $x247 (or (exc_halt_s 1) (not (bvsle (_ bv0 6) (bvadd (_ bv59 6) (sc_s 1))))))))
 (let (($x4815 (forall ((w (_ BitVec 256)) )(let ((?x3058 (storage_s x_0 x_1 x_2 x_3 w_1 1 w)))
 (let ((?x4494 (storage_s x_0 x_1 x_2 x_3 w_1 2 w)))
 (= ?x4494 ?x3058))))
 ))
 (let (($x2425 (forall ((n (_ BitVec 6)) )(let ((?x154 (sc_s 1)))
 (let ((?x1726 (bvadd (_ bv59 6) ?x154)))
 (let (($x6755 (bvsle ?x1726 n)))
 (let ((?x11167 (stack_s x_0 x_1 x_2 x_3 w_1 1 n)))
 (let ((?x9406 (stack_s x_0 x_1 x_2 x_3 w_1 2 n)))
 (or (= ?x9406 ?x11167) $x6755)))))))
 ))
 (let ((?x154 (sc_s 1)))
 (let (($x2182 (= ?x218 ?x154)))
 (let ((?x7907 (used_gas_s x_0 x_1 x_2 x_3 w_1 2)))
 (let (($x3703 (= (stack_s x_0 x_1 x_2 x_3 w_1 2 (bvadd (_ bv62 6) ?x218)) (stack_s x_0 x_1 x_2 x_3 w_1 1 (bvadd (_ bv62 6) ?x154)))))
 (let (($x2206 (= (stack_s x_0 x_1 x_2 x_3 w_1 2 (bvadd (_ bv61 6) ?x218)) (stack_s x_0 x_1 x_2 x_3 w_1 1 (bvadd (_ bv61 6) ?x154)))))
 (let (($x8441 (= (stack_s x_0 x_1 x_2 x_3 w_1 2 (bvadd (_ bv60 6) ?x218)) (stack_s x_0 x_1 x_2 x_3 w_1 1 (bvadd (_ bv60 6) ?x154)))))
 (let (($x10831 (= (stack_s x_0 x_1 x_2 x_3 w_1 2 (bvadd (_ bv59 6) ?x218)) (stack_s x_0 x_1 x_2 x_3 w_1 1 (bvadd (_ bv63 6) ?x154)))))
 (let (($x7183 (= (stack_s x_0 x_1 x_2 x_3 w_1 2 ?x5789) (stack_s x_0 x_1 x_2 x_3 w_1 1 (bvadd (_ bv59 6) ?x154)))))
 (let (($x189 (exc_halt_s 1)))
 (let (($x9787 (= $x189 (or $x57 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x72))) (_ bv0 1)))))))
 (let (($x2499 (forall ((w (_ BitVec 256)) )(let ((?x6035 (storage_s x_0 x_1 x_2 x_3 w_1 0 w)))
 (let ((?x3058 (storage_s x_0 x_1 x_2 x_3 w_1 1 w)))
 (= ?x3058 ?x6035))))
 ))
 (let (($x1041 (forall ((n (_ BitVec 6)) )(let ((?x6759 (stack_s x_0 x_1 x_2 x_3 w_1 0 n)))
 (let ((?x11167 (stack_s x_0 x_1 x_2 x_3 w_1 1 n)))
 (let ((?x72 (sc_s 0)))
 (let (($x3549 (bvsle ?x72 n)))
 (or $x3549 (= ?x11167 ?x6759)))))))
 ))
 (let (($x10046 (= ?x154 (bvadd (_ bv1 6) ?x72))))
 (let (($x2081 (forall ((w (_ BitVec 256)) )(let ((?x6035 (storage_s x_0 x_1 x_2 x_3 w_1 0 w)))
 (= ?x6035 (_ bv0 256))))
 ))
 (let (($x6064 (= ?x5458 0)))
 (let (($x1248 (not $x57)))
 (let (($x9269 (= (stack_s x_0 x_1 x_2 x_3 w_1 0 (_ bv3 6)) x_3)))
 (let (($x7597 (= (stack_s x_0 x_1 x_2 x_3 w_1 0 (_ bv2 6)) x_2)))
 (let (($x953 (= (stack_s x_0 x_1 x_2 x_3 w_1 0 (_ bv1 6)) x_1)))
 (let (($x9460 (= (stack_s x_0 x_1 x_2 x_3 w_1 0 (_ bv0 6)) x_0)))
 (let (($x289 (= ?x72 (_ bv4 6))))
 (and $x289 $x9460 $x953 $x7597 $x9269 $x1248 $x6064 $x2081 (= (stack_s x_0 x_1 x_2 x_3 w_1 1 ?x72) w_1) (= (used_gas_s x_0 x_1 x_2 x_3 w_1 1) (+ 3 ?x5458)) $x10046 $x1041 $x2499 $x9787 $x7183 $x10831 $x8441 $x2206 $x3703 (= ?x7907 (+ 3 (used_gas_s x_0 x_1 x_2 x_3 w_1 1))) $x2182 $x2425 $x4815 $x7395 (= ?x1601 (+ 2 ?x7907)) $x4278 $x6091 $x9107 $x3804 (= ?x3051 (stack_s x_0 x_1 x_2 x_3 w_1 3 (bvadd (_ bv61 6) ?x275))) (= ?x1259 (stack_s x_0 x_1 x_2 x_3 w_1 3 (bvadd (_ bv63 6) ?x275))) (= ?x5771 (stack_s x_0 x_1 x_2 x_3 w_1 3 (bvadd (_ bv62 6) ?x275))) (= ?x6335 (+ 3 ?x1601)) $x9884 $x35 $x4233 $x3658 (= ?x9753 ?x1259) (= (stack_s x_0 x_1 x_2 x_3 w_1 5 ?x1731) ?x1259) (= (stack_s x_0 x_1 x_2 x_3 w_1 5 ?x9777) ?x5771) (= (stack_s x_0 x_1 x_2 x_3 w_1 5 ?x9783) ?x3051) (= ?x9013 (+ 3 ?x6335)) $x6140 $x1126 $x6829 (= $x11317 (or $x7172 $x8439 $x11222)) $x2995 (= (stack_s x_0 x_1 x_2 x_3 w_1 6 (bvadd (_ bv62 6) ?x926)) ?x9753) $x4816 $x9018 $x232 $x10112 $x7843 (= ?x3527 ?x7578) (= (stack_t x_0 x_1 x_2 x_3 w_1 1 ?x2902) ?x7578) (= (used_gas_t x_0 x_1 x_2 x_3 w_1 1) (+ 3 ?x10718)) $x9556 $x7194 $x3050 $x8450 $x4161 (= (stack_t x_0 x_1 x_2 x_3 w_1 2 (bvadd (_ bv60 6) ?x2714)) ?x3527) $x9027 $x1040 (= ?x10750 (+ 3 (used_gas_t x_0 x_1 x_2 x_3 w_1 1))) $x11805 $x3265 $x11787 $x2918 (= (stack_t x_0 x_1 x_2 x_3 w_1 3 ?x2714) w_1) (= ?x1368 (+ 3 ?x10750)) $x1233 $x11681 $x2558 $x10277 $x4551 $x3474 $x23 $x3585 $x3828 $x1928 (= ?x1643 (+ 3 ?x1368)) $x1038 $x37 $x9823 $x2209 $x2593 (= ?x919 (bvadd (_ bv63 6) ?x1098)) $x3215 $x3920 $x3959 $x73 $x5728 $x58 $x11448 $x3965 (not (and $x929 $x7025 $x889 $x9689)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
 )
(check-sat)
(get-proof)