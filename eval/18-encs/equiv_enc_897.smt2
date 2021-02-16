; SWAP1 PUSH cw_2 SWAP1 DUP3 SWAP1 => DUP1 PUSH cw_2 SWAP2 SWAP3
(set-option :produce-proofs true)
; 
(set-info :status unknown)
(declare-fun storage_t ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 256)) (_ BitVec 256))
(declare-fun w_2 () (_ BitVec 256))
(declare-fun storage_s ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 256)) (_ BitVec 256))
(declare-fun exc_halt_t (Int) Bool)
(declare-fun exc_halt_s (Int) Bool)
(declare-fun sc_t (Int) (_ BitVec 6))
(declare-fun stack_t ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 6)) (_ BitVec 256))
(declare-fun stack_s ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 6)) (_ BitVec 256))
(declare-fun sc_s (Int) (_ BitVec 6))
(declare-fun used_gas_t ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int) Int)
(declare-fun used_gas_s ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int) Int)
(assert
 (exists ((x_0 (_ BitVec 256)) (x_1 (_ BitVec 256)) )(let (($x439 (forall ((w (_ BitVec 256)) )(let ((?x2361 (storage_t x_0 x_1 w_2 4 w)))
 (let ((?x2644 (storage_s x_0 x_1 w_2 5 w)))
 (= ?x2644 ?x2361))))
 ))
 (let (($x7854 (exc_halt_t 4)))
 (let (($x1862 (exc_halt_s 5)))
 (let (($x1772 (= $x1862 $x7854)))
 (let (($x5195 (forall ((n (_ BitVec 6)) )(let ((?x7230 (stack_t x_0 x_1 w_2 4 n)))
 (let ((?x1018 (stack_s x_0 x_1 w_2 5 n)))
 (let (($x10324 (= ?x1018 ?x7230)))
 (or $x10324 (bvsle (sc_t 4) n))))))
 ))
 (let ((?x7495 (sc_t 4)))
 (let ((?x4319 (sc_s 5)))
 (let (($x8633 (= ?x4319 ?x7495)))
 (let ((?x622 (used_gas_t x_0 x_1 w_2 0)))
 (let ((?x8150 (used_gas_s x_0 x_1 w_2 0)))
 (let (($x1327 (= ?x8150 ?x622)))
 (let (($x3656 (forall ((w (_ BitVec 256)) )(let ((?x11599 (storage_t x_0 x_1 w_2 0 w)))
 (let ((?x8863 (storage_s x_0 x_1 w_2 0 w)))
 (= ?x8863 ?x11599))))
 ))
 (let (($x56 (exc_halt_t 0)))
 (let (($x57 (exc_halt_s 0)))
 (let (($x58 (= $x57 $x56)))
 (let (($x7979 (forall ((n (_ BitVec 6)) )(let ((?x6972 (stack_t x_0 x_1 w_2 0 n)))
 (let ((?x6025 (stack_s x_0 x_1 w_2 0 n)))
 (let (($x2698 (= ?x6025 ?x6972)))
 (or $x2698 (bvsle (sc_t 0) n))))))
 ))
 (let ((?x63 (sc_t 0)))
 (let ((?x72 (sc_s 0)))
 (let (($x73 (= ?x72 ?x63)))
 (let (($x8513 (= $x7854 (or (exc_halt_t 3) (not (bvsle (_ bv0 6) (bvadd (_ bv60 6) (sc_t 3))))))))
 (let (($x7347 (forall ((w (_ BitVec 256)) )(let ((?x9095 (storage_t x_0 x_1 w_2 3 w)))
 (let ((?x2361 (storage_t x_0 x_1 w_2 4 w)))
 (= ?x2361 ?x9095))))
 ))
 (let (($x1733 (forall ((n (_ BitVec 6)) )(let ((?x11510 (stack_t x_0 x_1 w_2 3 n)))
 (let ((?x7230 (stack_t x_0 x_1 w_2 4 n)))
 (or (bvsle (bvadd (_ bv60 6) (sc_t 3)) n) (= ?x7230 ?x11510)))))
 ))
 (let (($x11497 (= (used_gas_t x_0 x_1 w_2 4) (+ 3 (used_gas_t x_0 x_1 w_2 3)))))
 (let ((?x6847 (stack_t x_0 x_1 w_2 3 (bvadd (_ bv62 6) (sc_t 3)))))
 (let ((?x10744 (stack_t x_0 x_1 w_2 3 (bvadd (_ bv61 6) (sc_t 3)))))
 (let ((?x2002 (stack_t x_0 x_1 w_2 3 (bvadd (_ bv63 6) (sc_t 3)))))
 (let (($x3247 (= (stack_t x_0 x_1 w_2 4 (bvadd (_ bv63 6) ?x7495)) (stack_t x_0 x_1 w_2 3 (bvadd (_ bv60 6) (sc_t 3))))))
 (let (($x9131 (exc_halt_t 3)))
 (let (($x4213 (= $x9131 (or (exc_halt_t 2) (not (bvsle (_ bv0 6) (bvadd (_ bv61 6) (sc_t 2))))))))
 (let (($x2047 (forall ((w (_ BitVec 256)) )(let ((?x391 (storage_t x_0 x_1 w_2 2 w)))
 (let ((?x9095 (storage_t x_0 x_1 w_2 3 w)))
 (= ?x9095 ?x391))))
 ))
 (let (($x9151 (forall ((n (_ BitVec 6)) )(let ((?x5449 (stack_t x_0 x_1 w_2 2 n)))
 (let ((?x11510 (stack_t x_0 x_1 w_2 3 n)))
 (or (bvsle (bvadd (_ bv61 6) (sc_t 2)) n) (= ?x11510 ?x5449)))))
 ))
 (let ((?x10696 (used_gas_t x_0 x_1 w_2 3)))
 (let (($x3751 (exc_halt_t 1)))
 (let (($x3682 (or $x3751 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_t 1)))) (_ bv0 1))))))
 (let (($x7801 (exc_halt_t 2)))
 (let (($x6493 (forall ((w (_ BitVec 256)) )(let ((?x10819 (storage_t x_0 x_1 w_2 1 w)))
 (let ((?x391 (storage_t x_0 x_1 w_2 2 w)))
 (= ?x391 ?x10819))))
 ))
 (let (($x8362 (forall ((n (_ BitVec 6)) )(let ((?x10673 (stack_t x_0 x_1 w_2 1 n)))
 (let ((?x5449 (stack_t x_0 x_1 w_2 2 n)))
 (or (bvsle (sc_t 1) n) (= ?x5449 ?x10673)))))
 ))
 (let ((?x3303 (used_gas_t x_0 x_1 w_2 2)))
 (let (($x10325 (or $x56 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x63))) (_ bv0 1))) (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) ?x63))))))
 (let (($x11171 (forall ((w (_ BitVec 256)) )(let ((?x11599 (storage_t x_0 x_1 w_2 0 w)))
 (let ((?x10819 (storage_t x_0 x_1 w_2 1 w)))
 (= ?x10819 ?x11599))))
 ))
 (let (($x8695 (forall ((n (_ BitVec 6)) )(let ((?x6972 (stack_t x_0 x_1 w_2 0 n)))
 (let ((?x10673 (stack_t x_0 x_1 w_2 1 n)))
 (or (= ?x10673 ?x6972) (bvsle (bvadd (_ bv63 6) (sc_t 0)) n)))))
 ))
 (let ((?x7767 (bvadd (_ bv63 6) ?x63)))
 (let ((?x8533 (stack_t x_0 x_1 w_2 0 ?x7767)))
 (let (($x6498 (= $x1862 (or (exc_halt_s 4) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 4))))))))
 (let (($x8755 (forall ((w (_ BitVec 256)) )(let ((?x7030 (storage_s x_0 x_1 w_2 4 w)))
 (let ((?x2644 (storage_s x_0 x_1 w_2 5 w)))
 (= ?x2644 ?x7030))))
 ))
 (let (($x2802 (forall ((n (_ BitVec 6)) )(let ((?x6879 (stack_s x_0 x_1 w_2 4 n)))
 (let ((?x1018 (stack_s x_0 x_1 w_2 5 n)))
 (or (bvsle (bvadd (_ bv62 6) (sc_s 4)) n) (= ?x1018 ?x6879)))))
 ))
 (let (($x1005 (= (used_gas_s x_0 x_1 w_2 5) (+ 3 (used_gas_s x_0 x_1 w_2 4)))))
 (let ((?x4683 (stack_s x_0 x_1 w_2 4 (bvadd (_ bv63 6) (sc_s 4)))))
 (let (($x7007 (= (stack_s x_0 x_1 w_2 5 (bvadd (_ bv63 6) ?x4319)) (stack_s x_0 x_1 w_2 4 (bvadd (_ bv62 6) (sc_s 4))))))
 (let (($x8103 (exc_halt_s 3)))
 (let (($x10279 (or (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 3)))) (_ bv0 1))) (not (bvsle (_ bv0 6) (bvadd (_ bv61 6) (sc_s 3)))) $x8103)))
 (let (($x9175 (exc_halt_s 4)))
 (let (($x933 (forall ((w (_ BitVec 256)) )(let ((?x10468 (storage_s x_0 x_1 w_2 3 w)))
 (let ((?x7030 (storage_s x_0 x_1 w_2 4 w)))
 (= ?x7030 ?x10468))))
 ))
 (let (($x5904 (forall ((n (_ BitVec 6)) )(let ((?x9965 (stack_s x_0 x_1 w_2 3 n)))
 (let ((?x6879 (stack_s x_0 x_1 w_2 4 n)))
 (or (bvsle (bvadd (_ bv61 6) (sc_s 3)) n) (= ?x6879 ?x9965)))))
 ))
 (let ((?x3530 (used_gas_s x_0 x_1 w_2 4)))
 (let ((?x3851 (sc_s 3)))
 (let ((?x2271 (bvadd (_ bv63 6) ?x3851)))
 (let ((?x11825 (stack_s x_0 x_1 w_2 3 ?x2271)))
 (let ((?x1010 (bvadd (_ bv62 6) ?x3851)))
 (let ((?x4171 (stack_s x_0 x_1 w_2 3 ?x1010)))
 (let ((?x9618 (bvadd (_ bv61 6) ?x3851)))
 (let ((?x6665 (stack_s x_0 x_1 w_2 3 ?x9618)))
 (let (($x3894 (= $x8103 (or (exc_halt_s 2) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 2))))))))
 (let (($x3313 (forall ((w (_ BitVec 256)) )(let ((?x7771 (storage_s x_0 x_1 w_2 2 w)))
 (let ((?x10468 (storage_s x_0 x_1 w_2 3 w)))
 (= ?x10468 ?x7771))))
 ))
 (let (($x8040 (forall ((n (_ BitVec 6)) )(let ((?x518 (stack_s x_0 x_1 w_2 2 n)))
 (let ((?x9965 (stack_s x_0 x_1 w_2 3 n)))
 (or (= ?x9965 ?x518) (bvsle (bvadd (_ bv62 6) (sc_s 2)) n)))))
 ))
 (let ((?x7721 (used_gas_s x_0 x_1 w_2 3)))
 (let (($x8780 (exc_halt_s 1)))
 (let (($x11315 (or $x8780 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 1)))) (_ bv0 1))))))
 (let (($x10052 (exc_halt_s 2)))
 (let (($x4513 (forall ((w (_ BitVec 256)) )(let ((?x1498 (storage_s x_0 x_1 w_2 1 w)))
 (let ((?x7771 (storage_s x_0 x_1 w_2 2 w)))
 (= ?x7771 ?x1498))))
 ))
 (let (($x1141 (forall ((n (_ BitVec 6)) )(let ((?x315 (stack_s x_0 x_1 w_2 1 n)))
 (let ((?x518 (stack_s x_0 x_1 w_2 2 n)))
 (or (= ?x518 ?x315) (bvsle (sc_s 1) n)))))
 ))
 (let ((?x9079 (used_gas_s x_0 x_1 w_2 2)))
 (let (($x11311 (forall ((w (_ BitVec 256)) )(let ((?x8863 (storage_s x_0 x_1 w_2 0 w)))
 (let ((?x1498 (storage_s x_0 x_1 w_2 1 w)))
 (= ?x1498 ?x8863))))
 ))
 (let (($x7363 (forall ((n (_ BitVec 6)) )(let ((?x6025 (stack_s x_0 x_1 w_2 0 n)))
 (let ((?x315 (stack_s x_0 x_1 w_2 1 n)))
 (or (bvsle (bvadd (_ bv62 6) (sc_s 0)) n) (= ?x315 ?x6025)))))
 ))
 (let (($x5895 (= (stack_s x_0 x_1 w_2 1 (bvadd (_ bv62 6) (sc_s 1))) (stack_s x_0 x_1 w_2 0 (bvadd (_ bv63 6) ?x72)))))
 (let (($x5465 (= (stack_s x_0 x_1 w_2 1 (bvadd (_ bv63 6) (sc_s 1))) (stack_s x_0 x_1 w_2 0 (bvadd (_ bv62 6) ?x72)))))
 (let (($x11325 (forall ((w (_ BitVec 256)) )(let ((?x8863 (storage_s x_0 x_1 w_2 0 w)))
 (= ?x8863 (_ bv0 256))))
 ))
 (let (($x6938 (= ?x8150 0)))
 (let (($x7821 (= (stack_s x_0 x_1 w_2 0 (_ bv1 6)) x_1)))
 (let (($x9559 (= (stack_s x_0 x_1 w_2 0 (_ bv0 6)) x_0)))
 (let (($x6737 (= ?x72 (_ bv2 6))))
 (and $x6737 $x9559 $x7821 (not $x57) $x6938 $x11325 $x5465 $x5895 (= (used_gas_s x_0 x_1 w_2 1) (+ 3 ?x8150)) (= (sc_s 1) ?x72) $x7363 $x11311 (= $x8780 (or $x57 (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) ?x72))))) (= (stack_s x_0 x_1 w_2 2 (sc_s 1)) w_2) (= ?x9079 (+ 3 (used_gas_s x_0 x_1 w_2 1))) (= (sc_s 2) (bvadd (_ bv1 6) (sc_s 1))) $x1141 $x4513 (= $x10052 $x11315) (= ?x11825 (stack_s x_0 x_1 w_2 2 (bvadd (_ bv62 6) (sc_s 2)))) (= ?x4171 (stack_s x_0 x_1 w_2 2 (bvadd (_ bv63 6) (sc_s 2)))) (= ?x7721 (+ 3 ?x9079)) (= ?x3851 (sc_s 2)) $x8040 $x3313 $x3894 (= ?x4683 ?x6665) (= (stack_s x_0 x_1 w_2 4 ?x9618) ?x6665) (= (stack_s x_0 x_1 w_2 4 ?x1010) ?x4171) (= (stack_s x_0 x_1 w_2 4 ?x2271) ?x11825) (= ?x3530 (+ 3 ?x7721)) (= (sc_s 4) (bvadd (_ bv1 6) ?x3851)) $x5904 $x933 (= $x9175 $x10279) $x7007 (= (stack_s x_0 x_1 w_2 5 (bvadd (_ bv62 6) ?x4319)) ?x4683) $x1005 (= ?x4319 (sc_s 4)) $x2802 $x8755 $x6498 (= (stack_t x_0 x_1 w_2 1 (bvadd (_ bv63 6) (sc_t 1))) ?x8533) (= (stack_t x_0 x_1 w_2 1 ?x7767) ?x8533) (= (used_gas_t x_0 x_1 w_2 1) (+ 3 ?x622)) (= (sc_t 1) (bvadd (_ bv1 6) ?x63)) $x8695 $x11171 (= $x3751 $x10325) (= (stack_t x_0 x_1 w_2 2 (sc_t 1)) w_2) (= ?x3303 (+ 3 (used_gas_t x_0 x_1 w_2 1))) (= (sc_t 2) (bvadd (_ bv1 6) (sc_t 1))) $x8362 $x6493 (= $x7801 $x3682) (= ?x2002 (stack_t x_0 x_1 w_2 2 (bvadd (_ bv61 6) (sc_t 2)))) (= ?x10744 (stack_t x_0 x_1 w_2 2 (bvadd (_ bv63 6) (sc_t 2)))) (= ?x6847 (stack_t x_0 x_1 w_2 2 (bvadd (_ bv62 6) (sc_t 2)))) (= ?x10696 (+ 3 ?x3303)) (= (sc_t 3) (sc_t 2)) $x9151 $x2047 $x4213 $x3247 (= (stack_t x_0 x_1 w_2 4 (bvadd (_ bv60 6) ?x7495)) ?x2002) (= (stack_t x_0 x_1 w_2 4 (bvadd (_ bv61 6) ?x7495)) ?x10744) (= (stack_t x_0 x_1 w_2 4 (bvadd (_ bv62 6) ?x7495)) ?x6847) $x11497 (= ?x7495 (sc_t 3)) $x1733 $x7347 $x8513 $x73 $x7979 $x58 $x3656 $x1327 (not (and $x8633 $x5195 $x1772 $x439)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
 )
(check-sat)
(get-proof)