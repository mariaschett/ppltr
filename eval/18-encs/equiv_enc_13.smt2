; SWAP1 DUP2 PUSH cw_2 PUSH 0x00 DUP4 ADD => DUP1 PUSH cw_2 DUP4 SWAP3 SWAP4
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
(assert
 (exists ((x_0 (_ BitVec 256)) (x_1 (_ BitVec 256)) )(let (($x6434 (forall ((w (_ BitVec 256)) )(let ((?x6431 (storage_t x_0 x_1 w_2 5 w)))
 (let ((?x6432 (storage_s x_0 x_1 w_2 6 w)))
 (= ?x6432 ?x6431))))
 ))
 (let (($x886 (exc_halt_t 5)))
 (let (($x772 (exc_halt_s 6)))
 (let (($x889 (= $x772 $x886)))
 (let (($x7002 (forall ((n (_ BitVec 6)) )(let ((?x6436 (stack_t x_0 x_1 w_2 5 n)))
 (let ((?x6437 (stack_s x_0 x_1 w_2 6 n)))
 (let (($x6438 (= ?x6437 ?x6436)))
 (let ((?x919 (sc_t 5)))
 (let (($x3284 (bvsle ?x919 n)))
 (or $x3284 $x6438)))))))
 ))
 (let ((?x919 (sc_t 5)))
 (let ((?x926 (sc_s 6)))
 (let (($x929 (= ?x926 ?x919)))
 (let ((?x6444 (used_gas_t x_0 x_1 w_2 0)))
 (let ((?x6445 (used_gas_s x_0 x_1 w_2 0)))
 (let (($x6446 (= ?x6445 ?x6444)))
 (let (($x6451 (forall ((w (_ BitVec 256)) )(let ((?x6448 (storage_t x_0 x_1 w_2 0 w)))
 (let ((?x6449 (storage_s x_0 x_1 w_2 0 w)))
 (= ?x6449 ?x6448))))
 ))
 (let (($x56 (exc_halt_t 0)))
 (let (($x57 (exc_halt_s 0)))
 (let (($x58 (= $x57 $x56)))
 (let (($x7000 (forall ((n (_ BitVec 6)) )(let ((?x6453 (stack_t x_0 x_1 w_2 0 n)))
 (let ((?x6454 (stack_s x_0 x_1 w_2 0 n)))
 (let (($x6455 (= ?x6454 ?x6453)))
 (let ((?x63 (sc_t 0)))
 (let (($x3470 (bvsle ?x63 n)))
 (or $x3470 $x6455)))))))
 ))
 (let ((?x63 (sc_t 0)))
 (let ((?x72 (sc_s 0)))
 (let (($x73 (= ?x72 ?x63)))
 (let (($x1944 (= $x886 (or (exc_halt_t 4) (not (bvsle (_ bv0 6) (bvadd (_ bv59 6) (sc_t 4))))))))
 (let (($x6997 (forall ((w (_ BitVec 256)) )(let ((?x6461 (storage_t x_0 x_1 w_2 4 w)))
 (let ((?x6431 (storage_t x_0 x_1 w_2 5 w)))
 (= ?x6431 ?x6461))))
 ))
 (let (($x6996 (forall ((n (_ BitVec 6)) )(let ((?x6465 (stack_t x_0 x_1 w_2 4 n)))
 (let ((?x6436 (stack_t x_0 x_1 w_2 5 n)))
 (let ((?x3920 (sc_t 4)))
 (let ((?x2771 (bvadd (_ bv59 6) ?x3920)))
 (let (($x1934 (bvsle ?x2771 n)))
 (or $x1934 (= ?x6436 ?x6465))))))))
 ))
 (let ((?x3920 (sc_t 4)))
 (let (($x3278 (= ?x919 ?x3920)))
 (let (($x6992 (= (used_gas_t x_0 x_1 w_2 5) (+ 3 (used_gas_t x_0 x_1 w_2 4)))))
 (let ((?x2794 (bvadd (_ bv62 6) ?x3920)))
 (let ((?x6961 (stack_t x_0 x_1 w_2 4 ?x2794)))
 (let ((?x3246 (bvadd (_ bv61 6) ?x3920)))
 (let ((?x6956 (stack_t x_0 x_1 w_2 4 ?x3246)))
 (let ((?x2777 (bvadd (_ bv60 6) ?x3920)))
 (let ((?x6953 (stack_t x_0 x_1 w_2 4 ?x2777)))
 (let ((?x769 (bvadd (_ bv63 6) ?x3920)))
 (let ((?x6948 (stack_t x_0 x_1 w_2 4 ?x769)))
 (let (($x6977 (= (stack_t x_0 x_1 w_2 5 (bvadd (_ bv63 6) ?x919)) (stack_t x_0 x_1 w_2 4 (bvadd (_ bv59 6) ?x3920)))))
 (let (($x3900 (exc_halt_t 4)))
 (let (($x5970 (= $x3900 (or (exc_halt_t 3) (not (bvsle (_ bv0 6) (bvadd (_ bv60 6) (sc_t 3))))))))
 (let (($x6971 (forall ((w (_ BitVec 256)) )(let ((?x6497 (storage_t x_0 x_1 w_2 3 w)))
 (let ((?x6461 (storage_t x_0 x_1 w_2 4 w)))
 (= ?x6461 ?x6497))))
 ))
 (let (($x6970 (forall ((n (_ BitVec 6)) )(let ((?x6501 (stack_t x_0 x_1 w_2 3 n)))
 (let ((?x6465 (stack_t x_0 x_1 w_2 4 n)))
 (let ((?x2012 (sc_t 3)))
 (let ((?x3241 (bvadd (_ bv60 6) ?x2012)))
 (let (($x5961 (bvsle ?x3241 n)))
 (or $x5961 (= ?x6465 ?x6501))))))))
 ))
 (let ((?x2012 (sc_t 3)))
 (let (($x2799 (= ?x3920 ?x2012)))
 (let ((?x6473 (used_gas_t x_0 x_1 w_2 4)))
 (let (($x3232 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_t 2)))) (_ bv0 1)))))
 (let (($x903 (exc_halt_t 2)))
 (let (($x535 (not (bvsle (_ bv0 6) (bvadd (_ bv60 6) (sc_t 2))))))
 (let (($x4125 (exc_halt_t 3)))
 (let (($x6943 (forall ((w (_ BitVec 256)) )(let ((?x6530 (storage_t x_0 x_1 w_2 2 w)))
 (let ((?x6497 (storage_t x_0 x_1 w_2 3 w)))
 (= ?x6497 ?x6530))))
 ))
 (let (($x6942 (forall ((n (_ BitVec 6)) )(let ((?x6534 (stack_t x_0 x_1 w_2 2 n)))
 (let ((?x6501 (stack_t x_0 x_1 w_2 3 n)))
 (let ((?x4056 (sc_t 2)))
 (let ((?x556 (bvadd (_ bv60 6) ?x4056)))
 (let (($x553 (bvsle ?x556 n)))
 (or $x553 (= ?x6501 ?x6534))))))))
 ))
 (let (($x2730 (= ?x2012 (bvadd (_ bv1 6) (sc_t 2)))))
 (let ((?x6509 (used_gas_t x_0 x_1 w_2 3)))
 (let (($x6936 (= (stack_t x_0 x_1 w_2 3 (bvadd (_ bv63 6) (sc_t 2))) (stack_t x_0 x_1 w_2 2 (bvadd (_ bv63 6) (sc_t 2))))))
 (let (($x6932 (= (stack_t x_0 x_1 w_2 3 (bvadd (_ bv62 6) (sc_t 2))) (stack_t x_0 x_1 w_2 2 (bvadd (_ bv62 6) (sc_t 2))))))
 (let (($x6928 (= (stack_t x_0 x_1 w_2 3 (bvadd (_ bv61 6) (sc_t 2))) (stack_t x_0 x_1 w_2 2 (bvadd (_ bv61 6) (sc_t 2))))))
 (let ((?x4056 (sc_t 2)))
 (let ((?x556 (bvadd (_ bv60 6) ?x4056)))
 (let ((?x6921 (stack_t x_0 x_1 w_2 2 ?x556)))
 (let (($x3312 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_t 1)))) (_ bv0 1)))))
 (let (($x1920 (exc_halt_t 1)))
 (let (($x3333 (= $x903 (or $x1920 $x3312))))
 (let (($x6917 (forall ((w (_ BitVec 256)) )(let ((?x6564 (storage_t x_0 x_1 w_2 1 w)))
 (let ((?x6530 (storage_t x_0 x_1 w_2 2 w)))
 (= ?x6530 ?x6564))))
 ))
 (let (($x6916 (forall ((n (_ BitVec 6)) )(let ((?x6568 (stack_t x_0 x_1 w_2 1 n)))
 (let ((?x6534 (stack_t x_0 x_1 w_2 2 n)))
 (let ((?x4023 (sc_t 1)))
 (let (($x3393 (bvsle ?x4023 n)))
 (or $x3393 (= ?x6534 ?x6568)))))))
 ))
 (let (($x3391 (= ?x4056 (bvadd (_ bv1 6) (sc_t 1)))))
 (let ((?x6542 (used_gas_t x_0 x_1 w_2 2)))
 (let (($x3418 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x63))) (_ bv0 1)))))
 (let (($x6903 (forall ((w (_ BitVec 256)) )(let ((?x6448 (storage_t x_0 x_1 w_2 0 w)))
 (let ((?x6564 (storage_t x_0 x_1 w_2 1 w)))
 (= ?x6564 ?x6448))))
 ))
 (let (($x6902 (forall ((n (_ BitVec 6)) )(let ((?x6453 (stack_t x_0 x_1 w_2 0 n)))
 (let ((?x6568 (stack_t x_0 x_1 w_2 1 n)))
 (or (bvsle (bvadd (_ bv63 6) (sc_t 0)) n) (= ?x6568 ?x6453)))))
 ))
 (let ((?x4023 (sc_t 1)))
 (let (($x3469 (= ?x4023 (bvadd (_ bv1 6) ?x63))))
 (let ((?x3016 (bvadd (_ bv63 6) ?x63)))
 (let ((?x6892 (stack_t x_0 x_1 w_2 0 ?x3016)))
 (let (($x6888 (= $x772 (or (exc_halt_s 5) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 5))))))))
 (let (($x6884 (forall ((w (_ BitVec 256)) )(let ((?x6605 (storage_s x_0 x_1 w_2 5 w)))
 (let ((?x6432 (storage_s x_0 x_1 w_2 6 w)))
 (= ?x6432 ?x6605))))
 ))
 (let (($x6883 (forall ((n (_ BitVec 6)) )(let ((?x6609 (stack_s x_0 x_1 w_2 5 n)))
 (let ((?x6437 (stack_s x_0 x_1 w_2 6 n)))
 (or (bvsle (bvadd (_ bv62 6) (sc_s 5)) n) (= ?x6437 ?x6609)))))
 ))
 (let ((?x805 (sc_s 5)))
 (let ((?x3655 (bvadd (_ bv63 6) ?x805)))
 (let (($x1661 (= ?x926 ?x3655)))
 (let (($x6878 (= (used_gas_s x_0 x_1 w_2 6) (+ 3 (used_gas_s x_0 x_1 w_2 5)))))
 (let ((?x6837 (stack_s x_0 x_1 w_2 5 ?x3655)))
 (let (($x6875 (= (stack_s x_0 x_1 w_2 6 (bvadd (_ bv63 6) ?x926)) (bvadd ?x6837 (stack_s x_0 x_1 w_2 5 (bvadd (_ bv62 6) ?x805))))))
 (let (($x3671 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 4)))) (_ bv0 1)))))
 (let (($x64 (exc_halt_s 4)))
 (let (($x3979 (exc_halt_s 5)))
 (let (($x6864 (forall ((w (_ BitVec 256)) )(let ((?x6628 (storage_s x_0 x_1 w_2 4 w)))
 (let ((?x6605 (storage_s x_0 x_1 w_2 5 w)))
 (= ?x6605 ?x6628))))
 ))
 (let (($x6863 (forall ((n (_ BitVec 6)) )(let ((?x6632 (stack_s x_0 x_1 w_2 4 n)))
 (let ((?x6609 (stack_s x_0 x_1 w_2 5 n)))
 (or (bvsle (bvadd (_ bv60 6) (sc_s 4)) n) (= ?x6609 ?x6632)))))
 ))
 (let (($x3235 (= ?x805 (bvadd (_ bv1 6) (sc_s 4)))))
 (let ((?x6617 (used_gas_s x_0 x_1 w_2 5)))
 (let (($x6856 (= (stack_s x_0 x_1 w_2 5 (bvadd (_ bv63 6) (sc_s 4))) (stack_s x_0 x_1 w_2 4 (bvadd (_ bv63 6) (sc_s 4))))))
 (let (($x6852 (= (stack_s x_0 x_1 w_2 5 (bvadd (_ bv62 6) (sc_s 4))) (stack_s x_0 x_1 w_2 4 (bvadd (_ bv62 6) (sc_s 4))))))
 (let (($x6848 (= (stack_s x_0 x_1 w_2 5 (bvadd (_ bv61 6) (sc_s 4))) (stack_s x_0 x_1 w_2 4 (bvadd (_ bv61 6) (sc_s 4))))))
 (let ((?x4305 (sc_s 4)))
 (let ((?x3681 (bvadd (_ bv60 6) ?x4305)))
 (let ((?x6840 (stack_s x_0 x_1 w_2 4 ?x3681)))
 (let (($x3650 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 3)))) (_ bv0 1)))))
 (let (($x292 (exc_halt_s 3)))
 (let (($x5879 (= $x64 (or $x292 $x3650))))
 (let (($x6835 (forall ((w (_ BitVec 256)) )(let ((?x6662 (storage_s x_0 x_1 w_2 3 w)))
 (let ((?x6628 (storage_s x_0 x_1 w_2 4 w)))
 (= ?x6628 ?x6662))))
 ))
 (let (($x6834 (forall ((n (_ BitVec 6)) )(let ((?x6666 (stack_s x_0 x_1 w_2 3 n)))
 (let ((?x6632 (stack_s x_0 x_1 w_2 4 n)))
 (let ((?x275 (sc_s 3)))
 (let (($x5872 (bvsle ?x275 n)))
 (or $x5872 (= ?x6632 ?x6666)))))))
 ))
 (let (($x3649 (= ?x4305 (bvadd (_ bv1 6) (sc_s 3)))))
 (let ((?x6640 (used_gas_s x_0 x_1 w_2 4)))
 (let (($x3623 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 2)))) (_ bv0 1)))))
 (let (($x247 (exc_halt_s 2)))
 (let (($x3626 (= $x292 (or $x247 $x3623))))
 (let (($x6826 (forall ((w (_ BitVec 256)) )(let ((?x6682 (storage_s x_0 x_1 w_2 2 w)))
 (let ((?x6662 (storage_s x_0 x_1 w_2 3 w)))
 (= ?x6662 ?x6682))))
 ))
 (let (($x6825 (forall ((n (_ BitVec 6)) )(let ((?x6686 (stack_s x_0 x_1 w_2 2 n)))
 (let ((?x6666 (stack_s x_0 x_1 w_2 3 n)))
 (let ((?x218 (sc_s 2)))
 (let (($x3592 (bvsle ?x218 n)))
 (or $x3592 (= ?x6666 ?x6686)))))))
 ))
 (let ((?x275 (sc_s 3)))
 (let (($x3591 (= ?x275 (bvadd (_ bv1 6) (sc_s 2)))))
 (let ((?x6674 (used_gas_s x_0 x_1 w_2 3)))
 (let (($x4932 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 1)))) (_ bv0 1)))))
 (let (($x3587 (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 1))))))
 (let (($x189 (exc_halt_s 1)))
 (let (($x6813 (forall ((w (_ BitVec 256)) )(let ((?x6702 (storage_s x_0 x_1 w_2 1 w)))
 (let ((?x6682 (storage_s x_0 x_1 w_2 2 w)))
 (= ?x6682 ?x6702))))
 ))
 (let (($x6812 (forall ((n (_ BitVec 6)) )(let ((?x6706 (stack_s x_0 x_1 w_2 1 n)))
 (let ((?x6686 (stack_s x_0 x_1 w_2 2 n)))
 (let ((?x154 (sc_s 1)))
 (let ((?x3520 (bvadd (_ bv62 6) ?x154)))
 (let (($x3570 (bvsle ?x3520 n)))
 (or $x3570 (= ?x6686 ?x6706))))))))
 ))
 (let ((?x218 (sc_s 2)))
 (let (($x4922 (= ?x218 (bvadd (_ bv1 6) (sc_s 1)))))
 (let ((?x6694 (used_gas_s x_0 x_1 w_2 2)))
 (let ((?x154 (sc_s 1)))
 (let ((?x3496 (bvadd (_ bv63 6) ?x154)))
 (let ((?x6781 (stack_s x_0 x_1 w_2 1 ?x3496)))
 (let ((?x3520 (bvadd (_ bv62 6) ?x154)))
 (let ((?x6787 (stack_s x_0 x_1 w_2 1 ?x3520)))
 (let (($x6797 (forall ((w (_ BitVec 256)) )(let ((?x6449 (storage_s x_0 x_1 w_2 0 w)))
 (let ((?x6702 (storage_s x_0 x_1 w_2 1 w)))
 (= ?x6702 ?x6449))))
 ))
 (let (($x6796 (forall ((n (_ BitVec 6)) )(let ((?x6454 (stack_s x_0 x_1 w_2 0 n)))
 (let ((?x6706 (stack_s x_0 x_1 w_2 1 n)))
 (let ((?x72 (sc_s 0)))
 (let ((?x3525 (bvadd (_ bv62 6) ?x72)))
 (let (($x1720 (bvsle ?x3525 n)))
 (or $x1720 (= ?x6706 ?x6454))))))))
 ))
 (let (($x3552 (= ?x154 ?x72)))
 (let (($x6754 (forall ((w (_ BitVec 256)) )(let ((?x6449 (storage_s x_0 x_1 w_2 0 w)))
 (= ?x6449 (_ bv0 256))))
 ))
 (let (($x6755 (= ?x6445 0)))
 (let (($x3503 (not $x57)))
 (let (($x6759 (= (stack_s x_0 x_1 w_2 0 (_ bv1 6)) x_1)))
 (let (($x6761 (= (stack_s x_0 x_1 w_2 0 (_ bv0 6)) x_0)))
 (let (($x2521 (= ?x72 (_ bv2 6))))
 (and $x2521 $x6761 $x6759 $x3503 $x6755 $x6754 (= ?x6781 (stack_s x_0 x_1 w_2 0 (bvadd (_ bv62 6) ?x72))) (= ?x6787 (stack_s x_0 x_1 w_2 0 (bvadd (_ bv63 6) ?x72))) (= (used_gas_s x_0 x_1 w_2 1) (+ 3 ?x6445)) $x3552 $x6796 $x6797 (= $x189 (or $x57 (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) ?x72))))) (= (stack_s x_0 x_1 w_2 2 (bvadd (_ bv63 6) ?x218)) ?x6787) (= (stack_s x_0 x_1 w_2 2 ?x3520) ?x6787) (= (stack_s x_0 x_1 w_2 2 ?x3496) ?x6781) (= ?x6694 (+ 3 (used_gas_s x_0 x_1 w_2 1))) $x4922 $x6812 $x6813 (= $x247 (or $x189 $x3587 $x4932)) (= (stack_s x_0 x_1 w_2 3 ?x218) w_2) (= ?x6674 (+ 3 ?x6694)) $x3591 $x6825 $x6826 $x3626 (= (stack_s x_0 x_1 w_2 4 ?x275) (_ bv0 256)) (= ?x6640 (+ 3 ?x6674)) $x3649 $x6834 $x6835 $x5879 (= ?x6837 ?x6840) (= (stack_s x_0 x_1 w_2 5 ?x3681) ?x6840) $x6848 $x6852 $x6856 (= ?x6617 (+ 3 ?x6640)) $x3235 $x6863 $x6864 (= $x3979 (or $x64 $x3671 (not (bvsle (_ bv0 6) ?x3681)))) $x6875 $x6878 $x1661 $x6883 $x6884 $x6888 (= (stack_t x_0 x_1 w_2 1 (bvadd (_ bv63 6) ?x4023)) ?x6892) (= (stack_t x_0 x_1 w_2 1 ?x3016) ?x6892) (= (used_gas_t x_0 x_1 w_2 1) (+ 3 ?x6444)) $x3469 $x6902 $x6903 (= $x1920 (or $x56 $x3418 (not (bvsle (_ bv0 6) ?x3016)))) (= (stack_t x_0 x_1 w_2 2 ?x4023) w_2) (= ?x6542 (+ 3 (used_gas_t x_0 x_1 w_2 1))) $x3391 $x6916 $x6917 $x3333 (= (stack_t x_0 x_1 w_2 3 (bvadd (_ bv63 6) ?x2012)) ?x6921) (= (stack_t x_0 x_1 w_2 3 ?x556) ?x6921) $x6928 $x6932 $x6936 (= ?x6509 (+ 3 ?x6542)) $x2730 $x6942 $x6943 (= $x4125 (or $x535 $x903 $x3232)) (= ?x6948 (stack_t x_0 x_1 w_2 3 (bvadd (_ bv60 6) ?x2012))) (= ?x6953 (stack_t x_0 x_1 w_2 3 (bvadd (_ bv63 6) ?x2012))) (= ?x6956 (stack_t x_0 x_1 w_2 3 (bvadd (_ bv61 6) ?x2012))) (= ?x6961 (stack_t x_0 x_1 w_2 3 (bvadd (_ bv62 6) ?x2012))) (= ?x6473 (+ 3 ?x6509)) $x2799 $x6970 $x6971 $x5970 $x6977 (= (stack_t x_0 x_1 w_2 5 (bvadd (_ bv59 6) ?x919)) ?x6948) (= (stack_t x_0 x_1 w_2 5 (bvadd (_ bv60 6) ?x919)) ?x6953) (= (stack_t x_0 x_1 w_2 5 (bvadd (_ bv61 6) ?x919)) ?x6956) (= (stack_t x_0 x_1 w_2 5 (bvadd (_ bv62 6) ?x919)) ?x6961) $x6992 $x3278 $x6996 $x6997 $x1944 $x73 $x7000 $x58 $x6451 $x6446 (not (and $x929 $x7002 $x889 $x6434))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
 )
(check-sat)
(get-proof)