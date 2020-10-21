; PUSH cw_1 SLOAD NUMBER SWAP1 DUP3 SWAP1 => NUMBER DUP2 PUSH cw_1 SLOAD
(set-option :produce-proofs true)
; 
(set-info :status unknown)
(declare-fun storage_t ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 256)) (_ BitVec 256))
(declare-fun w_1 () (_ BitVec 256))
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
 (exists ((x_0 (_ BitVec 256)) (x_SLOAD_0 (_ BitVec 256)) (x_NUMBER (_ BitVec 256)) )(let (($x10843 (forall ((w (_ BitVec 256)) )(let ((?x5818 (storage_t x_0 x_SLOAD_0 w_1 x_NUMBER 4 w)))
 (let ((?x7513 (storage_s x_0 x_SLOAD_0 w_1 x_NUMBER 6 w)))
 (= ?x7513 ?x5818))))
 ))
 (let (($x7854 (exc_halt_t 4)))
 (let (($x7121 (exc_halt_s 6)))
 (let (($x11019 (= $x7121 $x7854)))
 (let (($x7481 (forall ((n (_ BitVec 6)) )(let ((?x11029 (stack_t x_0 x_SLOAD_0 w_1 x_NUMBER 4 n)))
 (let ((?x8875 (stack_s x_0 x_SLOAD_0 w_1 x_NUMBER 6 n)))
 (let (($x2176 (= ?x8875 ?x11029)))
 (or $x2176 (bvsle (sc_t 4) n))))))
 ))
 (let ((?x7495 (sc_t 4)))
 (let ((?x9114 (sc_s 6)))
 (let (($x7045 (= ?x9114 ?x7495)))
 (let (($x7542 (not (and $x7045 $x7481 $x11019 $x10843))))
 (let ((?x3922 (used_gas_t x_0 x_SLOAD_0 w_1 x_NUMBER 0)))
 (let ((?x5902 (used_gas_s x_0 x_SLOAD_0 w_1 x_NUMBER 0)))
 (let (($x4994 (= ?x5902 ?x3922)))
 (let (($x9667 (forall ((w (_ BitVec 256)) )(let ((?x294 (storage_t x_0 x_SLOAD_0 w_1 x_NUMBER 0 w)))
 (let ((?x6490 (storage_s x_0 x_SLOAD_0 w_1 x_NUMBER 0 w)))
 (= ?x6490 ?x294))))
 ))
 (let (($x56 (exc_halt_t 0)))
 (let (($x57 (exc_halt_s 0)))
 (let (($x58 (= $x57 $x56)))
 (let (($x7172 (forall ((n (_ BitVec 6)) )(let ((?x6824 (stack_t x_0 x_SLOAD_0 w_1 x_NUMBER 0 n)))
 (let ((?x7134 (stack_s x_0 x_SLOAD_0 w_1 x_NUMBER 0 n)))
 (let (($x9163 (= ?x7134 ?x6824)))
 (let ((?x63 (sc_t 0)))
 (let (($x5163 (bvsle ?x63 n)))
 (or $x5163 $x9163)))))))
 ))
 (let ((?x63 (sc_t 0)))
 (let ((?x72 (sc_s 0)))
 (let (($x73 (= ?x72 ?x63)))
 (let (($x11282 (= $x7854 (or (exc_halt_t 3) (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_t 3))))))))
 (let (($x5794 (forall ((w (_ BitVec 256)) )(let ((?x10046 (storage_t x_0 x_SLOAD_0 w_1 x_NUMBER 3 w)))
 (let ((?x5818 (storage_t x_0 x_SLOAD_0 w_1 x_NUMBER 4 w)))
 (= ?x5818 ?x10046))))
 ))
 (let (($x9274 (forall ((n (_ BitVec 6)) )(let ((?x11507 (stack_t x_0 x_SLOAD_0 w_1 x_NUMBER 3 n)))
 (let ((?x11029 (stack_t x_0 x_SLOAD_0 w_1 x_NUMBER 4 n)))
 (let (($x1371 (= ?x11029 ?x11507)))
 (or $x1371 (bvsle (bvadd (_ bv63 6) (sc_t 3)) n))))))
 ))
 (let ((?x6869 (used_gas_t x_0 x_SLOAD_0 w_1 x_NUMBER 4)))
 (let ((?x2681 (sc_t 3)))
 (let ((?x3491 (bvadd (_ bv63 6) ?x2681)))
 (let ((?x10767 (stack_t x_0 x_SLOAD_0 w_1 x_NUMBER 3 ?x3491)))
 (let ((?x11755 (stack_t x_0 x_SLOAD_0 w_1 x_NUMBER 4 (bvadd (_ bv63 6) ?x7495))))
 (let (($x4405 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_t 2)))) (_ bv0 1)))))
 (let (($x2620 (exc_halt_t 2)))
 (let (($x8725 (exc_halt_t 3)))
 (let (($x4346 (forall ((w (_ BitVec 256)) )(let ((?x10349 (storage_t x_0 x_SLOAD_0 w_1 x_NUMBER 2 w)))
 (let ((?x10046 (storage_t x_0 x_SLOAD_0 w_1 x_NUMBER 3 w)))
 (= ?x10046 ?x10349))))
 ))
 (let (($x4502 (forall ((n (_ BitVec 6)) )(let ((?x4617 (sc_t 2)))
 (let (($x4422 (bvsle ?x4617 n)))
 (let ((?x11872 (stack_t x_0 x_SLOAD_0 w_1 x_NUMBER 2 n)))
 (let ((?x11507 (stack_t x_0 x_SLOAD_0 w_1 x_NUMBER 3 n)))
 (let (($x1410 (= ?x11507 ?x11872)))
 (or $x1410 $x4422)))))))
 ))
 (let (($x4637 (= ?x2681 (bvadd (_ bv1 6) (sc_t 2)))))
 (let ((?x10109 (used_gas_t x_0 x_SLOAD_0 w_1 x_NUMBER 3)))
 (let (($x5984 (= ?x10109 (+ 3 (used_gas_t x_0 x_SLOAD_0 w_1 x_NUMBER 2)))))
 (let (($x10758 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_t 1)))) (_ bv0 1)))))
 (let (($x8390 (exc_halt_t 1)))
 (let (($x9659 (forall ((w (_ BitVec 256)) )(let ((?x4309 (storage_t x_0 x_SLOAD_0 w_1 x_NUMBER 1 w)))
 (let ((?x10349 (storage_t x_0 x_SLOAD_0 w_1 x_NUMBER 2 w)))
 (= ?x10349 ?x4309))))
 ))
 (let (($x283 (forall ((n (_ BitVec 6)) )(let ((?x10663 (stack_t x_0 x_SLOAD_0 w_1 x_NUMBER 1 n)))
 (let ((?x11872 (stack_t x_0 x_SLOAD_0 w_1 x_NUMBER 2 n)))
 (let (($x8187 (= ?x11872 ?x10663)))
 (or (bvsle (bvadd (_ bv62 6) (sc_t 1)) n) $x8187)))))
 ))
 (let ((?x4617 (sc_t 2)))
 (let (($x1620 (= ?x4617 (bvadd (_ bv1 6) (sc_t 1)))))
 (let ((?x7107 (used_gas_t x_0 x_SLOAD_0 w_1 x_NUMBER 2)))
 (let (($x1276 (= ?x7107 (+ 3 (used_gas_t x_0 x_SLOAD_0 w_1 x_NUMBER 1)))))
 (let ((?x2547 (sc_t 1)))
 (let ((?x4448 (bvadd (_ bv63 6) ?x2547)))
 (let ((?x9679 (stack_t x_0 x_SLOAD_0 w_1 x_NUMBER 1 ?x4448)))
 (let ((?x7051 (bvadd (_ bv62 6) ?x2547)))
 (let ((?x9795 (stack_t x_0 x_SLOAD_0 w_1 x_NUMBER 1 ?x7051)))
 (let (($x594 (= $x8390 (or $x56 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x63))) (_ bv0 1)))))))
 (let (($x9942 (forall ((w (_ BitVec 256)) )(let ((?x294 (storage_t x_0 x_SLOAD_0 w_1 x_NUMBER 0 w)))
 (let ((?x4309 (storage_t x_0 x_SLOAD_0 w_1 x_NUMBER 1 w)))
 (= ?x4309 ?x294))))
 ))
 (let (($x11841 (forall ((n (_ BitVec 6)) )(let ((?x6824 (stack_t x_0 x_SLOAD_0 w_1 x_NUMBER 0 n)))
 (let ((?x10663 (stack_t x_0 x_SLOAD_0 w_1 x_NUMBER 1 n)))
 (let (($x10889 (= ?x10663 ?x6824)))
 (let ((?x63 (sc_t 0)))
 (let (($x5163 (bvsle ?x63 n)))
 (or $x5163 $x10889)))))))
 ))
 (let (($x4893 (= ?x2547 (bvadd (_ bv1 6) ?x63))))
 (let (($x11578 (= $x7121 (or (exc_halt_s 5) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 5))))))))
 (let (($x119 (forall ((w (_ BitVec 256)) )(let ((?x5683 (storage_s x_0 x_SLOAD_0 w_1 x_NUMBER 5 w)))
 (let ((?x7513 (storage_s x_0 x_SLOAD_0 w_1 x_NUMBER 6 w)))
 (= ?x7513 ?x5683))))
 ))
 (let (($x4784 (forall ((n (_ BitVec 6)) )(let ((?x11398 (stack_s x_0 x_SLOAD_0 w_1 x_NUMBER 5 n)))
 (let ((?x8875 (stack_s x_0 x_SLOAD_0 w_1 x_NUMBER 6 n)))
 (let (($x4858 (= ?x8875 ?x11398)))
 (or $x4858 (bvsle (bvadd (_ bv62 6) (sc_s 5)) n))))))
 ))
 (let ((?x4319 (sc_s 5)))
 (let (($x752 (= ?x9114 ?x4319)))
 (let (($x1638 (= (used_gas_s x_0 x_SLOAD_0 w_1 x_NUMBER 6) (+ 3 (used_gas_s x_0 x_SLOAD_0 w_1 x_NUMBER 5)))))
 (let ((?x8396 (stack_s x_0 x_SLOAD_0 w_1 x_NUMBER 5 (bvadd (_ bv63 6) ?x4319))))
 (let ((?x9588 (stack_s x_0 x_SLOAD_0 w_1 x_NUMBER 6 (bvadd (_ bv62 6) ?x9114))))
 (let ((?x9218 (bvadd (_ bv62 6) ?x4319)))
 (let ((?x108 (stack_s x_0 x_SLOAD_0 w_1 x_NUMBER 5 ?x9218)))
 (let ((?x10535 (stack_s x_0 x_SLOAD_0 w_1 x_NUMBER 6 (bvadd (_ bv63 6) ?x9114))))
 (let (($x9175 (exc_halt_s 4)))
 (let (($x11221 (or (not (bvsle (_ bv0 6) (bvadd (_ bv61 6) (sc_s 4)))) (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 4)))) (_ bv0 1))) $x9175)))
 (let (($x1862 (exc_halt_s 5)))
 (let (($x5051 (forall ((w (_ BitVec 256)) )(let ((?x666 (storage_s x_0 x_SLOAD_0 w_1 x_NUMBER 4 w)))
 (let ((?x5683 (storage_s x_0 x_SLOAD_0 w_1 x_NUMBER 5 w)))
 (= ?x5683 ?x666))))
 ))
 (let (($x9998 (forall ((n (_ BitVec 6)) )(let ((?x1286 (stack_s x_0 x_SLOAD_0 w_1 x_NUMBER 4 n)))
 (let ((?x11398 (stack_s x_0 x_SLOAD_0 w_1 x_NUMBER 5 n)))
 (let (($x9834 (= ?x11398 ?x1286)))
 (or (bvsle (bvadd (_ bv61 6) (sc_s 4)) n) $x9834)))))
 ))
 (let ((?x7944 (used_gas_s x_0 x_SLOAD_0 w_1 x_NUMBER 5)))
 (let (($x4099 (= ?x7944 (+ 3 (used_gas_s x_0 x_SLOAD_0 w_1 x_NUMBER 4)))))
 (let ((?x9433 (sc_s 4)))
 (let ((?x1993 (bvadd (_ bv63 6) ?x9433)))
 (let ((?x10690 (stack_s x_0 x_SLOAD_0 w_1 x_NUMBER 4 ?x1993)))
 (let ((?x584 (bvadd (_ bv62 6) ?x9433)))
 (let ((?x3808 (stack_s x_0 x_SLOAD_0 w_1 x_NUMBER 4 ?x584)))
 (let ((?x3507 (bvadd (_ bv61 6) ?x9433)))
 (let ((?x6983 (stack_s x_0 x_SLOAD_0 w_1 x_NUMBER 4 ?x3507)))
 (let (($x4169 (= $x9175 (or (exc_halt_s 3) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 3))))))))
 (let (($x8533 (forall ((w (_ BitVec 256)) )(let ((?x10496 (storage_s x_0 x_SLOAD_0 w_1 x_NUMBER 3 w)))
 (let ((?x666 (storage_s x_0 x_SLOAD_0 w_1 x_NUMBER 4 w)))
 (= ?x666 ?x10496))))
 ))
 (let (($x4667 (forall ((n (_ BitVec 6)) )(let ((?x7760 (stack_s x_0 x_SLOAD_0 w_1 x_NUMBER 3 n)))
 (let ((?x1286 (stack_s x_0 x_SLOAD_0 w_1 x_NUMBER 4 n)))
 (let (($x660 (= ?x1286 ?x7760)))
 (or $x660 (bvsle (bvadd (_ bv62 6) (sc_s 3)) n))))))
 ))
 (let ((?x3851 (sc_s 3)))
 (let (($x1520 (= ?x9433 ?x3851)))
 (let ((?x6344 (used_gas_s x_0 x_SLOAD_0 w_1 x_NUMBER 4)))
 (let (($x315 (= ?x6344 (+ 3 (used_gas_s x_0 x_SLOAD_0 w_1 x_NUMBER 3)))))
 (let (($x10052 (exc_halt_s 2)))
 (let (($x11430 (or $x10052 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 2)))) (_ bv0 1))))))
 (let (($x8103 (exc_halt_s 3)))
 (let (($x1715 (forall ((w (_ BitVec 256)) )(let ((?x10786 (storage_s x_0 x_SLOAD_0 w_1 x_NUMBER 2 w)))
 (let ((?x10496 (storage_s x_0 x_SLOAD_0 w_1 x_NUMBER 3 w)))
 (= ?x10496 ?x10786))))
 ))
 (let (($x6121 (forall ((n (_ BitVec 6)) )(let ((?x6293 (stack_s x_0 x_SLOAD_0 w_1 x_NUMBER 2 n)))
 (let ((?x7760 (stack_s x_0 x_SLOAD_0 w_1 x_NUMBER 3 n)))
 (let (($x2269 (= ?x7760 ?x6293)))
 (or $x2269 (bvsle (sc_s 2) n))))))
 ))
 (let ((?x7897 (used_gas_s x_0 x_SLOAD_0 w_1 x_NUMBER 3)))
 (let (($x3898 (= $x10052 (or (exc_halt_s 1) (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_s 1))))))))
 (let (($x5306 (forall ((w (_ BitVec 256)) )(let ((?x4503 (storage_s x_0 x_SLOAD_0 w_1 x_NUMBER 1 w)))
 (let ((?x10786 (storage_s x_0 x_SLOAD_0 w_1 x_NUMBER 2 w)))
 (= ?x10786 ?x4503))))
 ))
 (let (($x6679 (forall ((n (_ BitVec 6)) )(let ((?x2773 (stack_s x_0 x_SLOAD_0 w_1 x_NUMBER 1 n)))
 (let ((?x6293 (stack_s x_0 x_SLOAD_0 w_1 x_NUMBER 2 n)))
 (let (($x4918 (= ?x6293 ?x2773)))
 (or $x4918 (bvsle (bvadd (_ bv63 6) (sc_s 1)) n))))))
 ))
 (let ((?x471 (used_gas_s x_0 x_SLOAD_0 w_1 x_NUMBER 2)))
 (let ((?x154 (sc_s 1)))
 (let ((?x5037 (bvadd (_ bv63 6) ?x154)))
 (let ((?x421 (stack_s x_0 x_SLOAD_0 w_1 x_NUMBER 1 ?x5037)))
 (let ((?x2272 (sc_s 2)))
 (let ((?x5543 (bvadd (_ bv63 6) ?x2272)))
 (let ((?x4036 (stack_s x_0 x_SLOAD_0 w_1 x_NUMBER 2 ?x5543)))
 (let (($x8780 (exc_halt_s 1)))
 (let (($x6093 (= $x8780 (or $x57 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x72))) (_ bv0 1)))))))
 (let (($x793 (forall ((w (_ BitVec 256)) )(let ((?x6490 (storage_s x_0 x_SLOAD_0 w_1 x_NUMBER 0 w)))
 (let ((?x4503 (storage_s x_0 x_SLOAD_0 w_1 x_NUMBER 1 w)))
 (= ?x4503 ?x6490))))
 ))
 (let (($x11475 (forall ((n (_ BitVec 6)) )(let ((?x7134 (stack_s x_0 x_SLOAD_0 w_1 x_NUMBER 0 n)))
 (let ((?x2773 (stack_s x_0 x_SLOAD_0 w_1 x_NUMBER 1 n)))
 (let ((?x72 (sc_s 0)))
 (let (($x834 (bvsle ?x72 n)))
 (or $x834 (= ?x2773 ?x7134)))))))
 ))
 (let (($x185 (= ?x154 (bvadd (_ bv1 6) ?x72))))
 (let ((?x1781 (used_gas_s x_0 x_SLOAD_0 w_1 x_NUMBER 1)))
 (let (($x6808 (= ?x1781 (+ 3 ?x5902))))
 (let (($x11151 (= (stack_s x_0 x_SLOAD_0 w_1 x_NUMBER 1 ?x72) w_1)))
 (let (($x9208 (forall ((w (_ BitVec 256)) )(let (($x5133 (= w (stack_s x_0 x_SLOAD_0 w_1 x_NUMBER 1 (bvadd (_ bv63 6) (sc_s 1))))))
 (let ((?x6490 (storage_s x_0 x_SLOAD_0 w_1 x_NUMBER 0 w)))
 (= ?x6490 (ite $x5133 x_SLOAD_0 (_ bv0 256))))))
 ))
 (let (($x4708 (= ?x5902 0)))
 (let (($x5099 (not $x57)))
 (let (($x8308 (= (stack_s x_0 x_SLOAD_0 w_1 x_NUMBER 0 (_ bv0 6)) x_0)))
 (let (($x8601 (= ?x72 (_ bv1 6))))
 (and $x8601 $x8308 $x5099 $x4708 $x9208 $x11151 $x6808 $x185 $x11475 $x793 $x6093 (= ?x4036 (storage_s x_0 x_SLOAD_0 w_1 x_NUMBER 1 ?x421)) (= ?x471 (+ 200 ?x1781)) (= ?x2272 ?x154) $x6679 $x5306 $x3898 (= (stack_s x_0 x_SLOAD_0 w_1 x_NUMBER 3 ?x2272) x_NUMBER) (= ?x7897 (+ 2 ?x471)) (= ?x3851 (bvadd (_ bv1 6) ?x2272)) $x6121 $x1715 (= $x8103 $x11430) (= ?x10690 (stack_s x_0 x_SLOAD_0 w_1 x_NUMBER 3 (bvadd (_ bv62 6) ?x3851))) (= ?x3808 (stack_s x_0 x_SLOAD_0 w_1 x_NUMBER 3 (bvadd (_ bv63 6) ?x3851))) $x315 $x1520 $x4667 $x8533 $x4169 (= ?x8396 ?x6983) (= (stack_s x_0 x_SLOAD_0 w_1 x_NUMBER 5 ?x3507) ?x6983) (= (stack_s x_0 x_SLOAD_0 w_1 x_NUMBER 5 ?x584) ?x3808) (= (stack_s x_0 x_SLOAD_0 w_1 x_NUMBER 5 ?x1993) ?x10690) $x4099 (= ?x4319 (bvadd (_ bv1 6) ?x9433)) $x9998 $x5051 (= $x1862 $x11221) (= ?x10535 ?x108) (= ?x9588 ?x8396) $x1638 $x752 $x4784 $x119 $x11578 (= (stack_t x_0 x_SLOAD_0 w_1 x_NUMBER 1 ?x63) x_NUMBER) (= (used_gas_t x_0 x_SLOAD_0 w_1 x_NUMBER 1) (+ 2 ?x3922)) $x4893 $x11841 $x9942 $x594 (= (stack_t x_0 x_SLOAD_0 w_1 x_NUMBER 2 (bvadd (_ bv63 6) ?x4617)) ?x9795) (= (stack_t x_0 x_SLOAD_0 w_1 x_NUMBER 2 ?x7051) ?x9795) (= (stack_t x_0 x_SLOAD_0 w_1 x_NUMBER 2 ?x4448) ?x9679) $x1276 $x1620 $x283 $x9659 (= $x2620 (or (not (bvsle (_ bv0 6) ?x7051)) $x8390 $x10758)) (= (stack_t x_0 x_SLOAD_0 w_1 x_NUMBER 3 ?x4617) w_1) $x5984 $x4637 $x4502 $x4346 (= $x8725 (or $x2620 $x4405)) (= ?x11755 (storage_t x_0 x_SLOAD_0 w_1 x_NUMBER 3 ?x10767)) (= ?x6869 (+ 200 ?x10109)) (= ?x7495 ?x2681) $x9274 $x5794 $x11282 $x73 $x7172 $x58 $x9667 $x4994 $x7542))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
 )
(check-sat)
(get-proof)
