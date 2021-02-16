; PUSH cw_5 DUP4 PUSH cw_4 SWAP1 SWAP2 => DUP3 PUSH cw_4 PUSH cw_5
(set-option :produce-proofs true)
; 
(set-info :status unknown)
(declare-fun storage_t ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 256)) (_ BitVec 256))
(declare-fun w_4 () (_ BitVec 256))
(declare-fun w_5 () (_ BitVec 256))
(declare-fun storage_s ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 256)) (_ BitVec 256))
(declare-fun exc_halt_t (Int) Bool)
(declare-fun exc_halt_s (Int) Bool)
(declare-fun sc_t (Int) (_ BitVec 6))
(declare-fun stack_t ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 6)) (_ BitVec 256))
(declare-fun stack_s ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 6)) (_ BitVec 256))
(declare-fun sc_s (Int) (_ BitVec 6))
(declare-fun used_gas_t ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int) Int)
(declare-fun used_gas_s ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int) Int)
(assert
 (exists ((x_0 (_ BitVec 256)) (x_1 (_ BitVec 256)) (x_2 (_ BitVec 256)) )(let (($x11971 (forall ((w (_ BitVec 256)) )(let ((?x2591 (storage_t x_0 x_1 x_2 w_5 w_4 3 w)))
 (let ((?x1095 (storage_s x_0 x_1 x_2 w_5 w_4 5 w)))
 (= ?x1095 ?x2591))))
 ))
 (let (($x418 (exc_halt_t 3)))
 (let (($x1862 (exc_halt_s 5)))
 (let (($x5454 (= $x1862 $x418)))
 (let (($x2505 (forall ((n (_ BitVec 6)) )(let ((?x9232 (sc_t 3)))
 (let (($x3530 (bvsle ?x9232 n)))
 (let ((?x2121 (stack_t x_0 x_1 x_2 w_5 w_4 3 n)))
 (let ((?x4449 (stack_s x_0 x_1 x_2 w_5 w_4 5 n)))
 (let (($x559 (= ?x4449 ?x2121)))
 (or $x559 $x3530)))))))
 ))
 (let ((?x9232 (sc_t 3)))
 (let ((?x4319 (sc_s 5)))
 (let (($x6228 (= ?x4319 ?x9232)))
 (let ((?x4735 (used_gas_t x_0 x_1 x_2 w_5 w_4 0)))
 (let ((?x10645 (used_gas_s x_0 x_1 x_2 w_5 w_4 0)))
 (let (($x9428 (= ?x10645 ?x4735)))
 (let (($x6758 (forall ((w (_ BitVec 256)) )(let ((?x195 (storage_t x_0 x_1 x_2 w_5 w_4 0 w)))
 (let ((?x4494 (storage_s x_0 x_1 x_2 w_5 w_4 0 w)))
 (= ?x4494 ?x195))))
 ))
 (let (($x56 (exc_halt_t 0)))
 (let (($x57 (exc_halt_s 0)))
 (let (($x58 (= $x57 $x56)))
 (let (($x6054 (forall ((n (_ BitVec 6)) )(let ((?x11571 (stack_t x_0 x_1 x_2 w_5 w_4 0 n)))
 (let ((?x3426 (stack_s x_0 x_1 x_2 w_5 w_4 0 n)))
 (let (($x11584 (= ?x3426 ?x11571)))
 (let ((?x63 (sc_t 0)))
 (let (($x2738 (bvsle ?x63 n)))
 (or $x2738 $x11584)))))))
 ))
 (let ((?x63 (sc_t 0)))
 (let ((?x72 (sc_s 0)))
 (let (($x73 (= ?x72 ?x63)))
 (let (($x7905 (exc_halt_t 2)))
 (let (($x772 (or $x7905 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_t 2)))) (_ bv0 1))))))
 (let (($x9131 (= $x418 $x772)))
 (let (($x9039 (forall ((w (_ BitVec 256)) )(let ((?x8058 (storage_t x_0 x_1 x_2 w_5 w_4 2 w)))
 (let ((?x2591 (storage_t x_0 x_1 x_2 w_5 w_4 3 w)))
 (= ?x2591 ?x8058))))
 ))
 (let (($x11342 (forall ((n (_ BitVec 6)) )(let ((?x11248 (sc_t 2)))
 (let (($x9965 (bvsle ?x11248 n)))
 (let ((?x1220 (stack_t x_0 x_1 x_2 w_5 w_4 2 n)))
 (let ((?x2121 (stack_t x_0 x_1 x_2 w_5 w_4 3 n)))
 (or (= ?x2121 ?x1220) $x9965))))))
 ))
 (let (($x6668 (= ?x9232 (bvadd (_ bv1 6) (sc_t 2)))))
 (let (($x1417 (= (used_gas_t x_0 x_1 x_2 w_5 w_4 3) (+ 3 (used_gas_t x_0 x_1 x_2 w_5 w_4 2)))))
 (let (($x529 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_t 1)))) (_ bv0 1)))))
 (let (($x4852 (exc_halt_t 1)))
 (let (($x872 (= $x7905 (or $x4852 $x529))))
 (let (($x1421 (forall ((w (_ BitVec 256)) )(let ((?x670 (storage_t x_0 x_1 x_2 w_5 w_4 1 w)))
 (let ((?x8058 (storage_t x_0 x_1 x_2 w_5 w_4 2 w)))
 (= ?x8058 ?x670))))
 ))
 (let (($x5628 (forall ((n (_ BitVec 6)) )(let ((?x8841 (stack_t x_0 x_1 x_2 w_5 w_4 1 n)))
 (let ((?x1220 (stack_t x_0 x_1 x_2 w_5 w_4 2 n)))
 (let ((?x9666 (sc_t 1)))
 (let (($x37 (bvsle ?x9666 n)))
 (or $x37 (= ?x1220 ?x8841)))))))
 ))
 (let ((?x11248 (sc_t 2)))
 (let (($x3655 (= ?x11248 (bvadd (_ bv1 6) (sc_t 1)))))
 (let ((?x614 (used_gas_t x_0 x_1 x_2 w_5 w_4 2)))
 (let (($x6946 (not (bvsle (_ bv0 6) (bvadd (_ bv61 6) ?x63)))))
 (let (($x4057 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x63))) (_ bv0 1)))))
 (let (($x11561 (forall ((w (_ BitVec 256)) )(let ((?x195 (storage_t x_0 x_1 x_2 w_5 w_4 0 w)))
 (let ((?x670 (storage_t x_0 x_1 x_2 w_5 w_4 1 w)))
 (= ?x670 ?x195))))
 ))
 (let (($x5153 (forall ((n (_ BitVec 6)) )(let ((?x63 (sc_t 0)))
 (let ((?x7252 (bvadd (_ bv61 6) ?x63)))
 (let (($x5878 (bvsle ?x7252 n)))
 (let ((?x11571 (stack_t x_0 x_1 x_2 w_5 w_4 0 n)))
 (let ((?x8841 (stack_t x_0 x_1 x_2 w_5 w_4 1 n)))
 (or (= ?x8841 ?x11571) $x5878)))))))
 ))
 (let ((?x9666 (sc_t 1)))
 (let (($x3631 (= ?x9666 (bvadd (_ bv1 6) ?x63))))
 (let (($x7695 (= (stack_t x_0 x_1 x_2 w_5 w_4 1 (bvadd (_ bv63 6) ?x63)) (stack_t x_0 x_1 x_2 w_5 w_4 0 (bvadd (_ bv63 6) ?x63)))))
 (let (($x7248 (= (stack_t x_0 x_1 x_2 w_5 w_4 1 (bvadd (_ bv62 6) ?x63)) (stack_t x_0 x_1 x_2 w_5 w_4 0 (bvadd (_ bv62 6) ?x63)))))
 (let ((?x7252 (bvadd (_ bv61 6) ?x63)))
 (let ((?x2781 (stack_t x_0 x_1 x_2 w_5 w_4 0 ?x7252)))
 (let (($x10759 (= (stack_t x_0 x_1 x_2 w_5 w_4 1 (bvadd (_ bv63 6) ?x9666)) ?x2781)))
 (let (($x3392 (= $x1862 (or (exc_halt_s 4) (not (bvsle (_ bv0 6) (bvadd (_ bv61 6) (sc_s 4))))))))
 (let (($x9277 (forall ((w (_ BitVec 256)) )(let ((?x5796 (storage_s x_0 x_1 x_2 w_5 w_4 4 w)))
 (let ((?x1095 (storage_s x_0 x_1 x_2 w_5 w_4 5 w)))
 (= ?x1095 ?x5796))))
 ))
 (let (($x8734 (forall ((n (_ BitVec 6)) )(let ((?x2712 (stack_s x_0 x_1 x_2 w_5 w_4 4 n)))
 (let ((?x4449 (stack_s x_0 x_1 x_2 w_5 w_4 5 n)))
 (or (bvsle (bvadd (_ bv61 6) (sc_s 4)) n) (= ?x4449 ?x2712)))))
 ))
 (let ((?x9433 (sc_s 4)))
 (let (($x11305 (= ?x4319 ?x9433)))
 (let (($x10915 (= (used_gas_s x_0 x_1 x_2 w_5 w_4 5) (+ 3 (used_gas_s x_0 x_1 x_2 w_5 w_4 4)))))
 (let ((?x1557 (bvadd (_ bv62 6) ?x9433)))
 (let ((?x11618 (stack_s x_0 x_1 x_2 w_5 w_4 4 ?x1557)))
 (let (($x8577 (= (stack_s x_0 x_1 x_2 w_5 w_4 5 (bvadd (_ bv62 6) ?x4319)) ?x11618)))
 (let ((?x11699 (bvadd (_ bv63 6) ?x9433)))
 (let ((?x4587 (stack_s x_0 x_1 x_2 w_5 w_4 4 ?x11699)))
 (let (($x2256 (= (stack_s x_0 x_1 x_2 w_5 w_4 5 (bvadd (_ bv61 6) ?x4319)) ?x4587)))
 (let (($x1630 (= (stack_s x_0 x_1 x_2 w_5 w_4 5 (bvadd (_ bv63 6) ?x4319)) (stack_s x_0 x_1 x_2 w_5 w_4 4 (bvadd (_ bv61 6) ?x9433)))))
 (let (($x9175 (exc_halt_s 4)))
 (let (($x10301 (= $x9175 (or (exc_halt_s 3) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 3))))))))
 (let (($x464 (forall ((w (_ BitVec 256)) )(let ((?x6689 (storage_s x_0 x_1 x_2 w_5 w_4 3 w)))
 (let ((?x5796 (storage_s x_0 x_1 x_2 w_5 w_4 4 w)))
 (= ?x5796 ?x6689))))
 ))
 (let (($x1189 (forall ((n (_ BitVec 6)) )(let ((?x3851 (sc_s 3)))
 (let ((?x3651 (bvadd (_ bv62 6) ?x3851)))
 (let (($x3189 (bvsle ?x3651 n)))
 (let ((?x9594 (stack_s x_0 x_1 x_2 w_5 w_4 3 n)))
 (let ((?x2712 (stack_s x_0 x_1 x_2 w_5 w_4 4 n)))
 (or (= ?x2712 ?x9594) $x3189)))))))
 ))
 (let ((?x3851 (sc_s 3)))
 (let (($x2630 (= ?x9433 ?x3851)))
 (let ((?x1402 (used_gas_s x_0 x_1 x_2 w_5 w_4 4)))
 (let (($x2680 (= ?x11618 (stack_s x_0 x_1 x_2 w_5 w_4 3 (bvadd (_ bv63 6) ?x3851)))))
 (let (($x10342 (= ?x4587 (stack_s x_0 x_1 x_2 w_5 w_4 3 (bvadd (_ bv62 6) ?x3851)))))
 (let (($x10052 (exc_halt_s 2)))
 (let (($x3739 (or $x10052 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 2)))) (_ bv0 1))))))
 (let (($x8103 (exc_halt_s 3)))
 (let (($x2547 (= $x8103 $x3739)))
 (let (($x9911 (forall ((w (_ BitVec 256)) )(let ((?x3156 (storage_s x_0 x_1 x_2 w_5 w_4 2 w)))
 (let ((?x6689 (storage_s x_0 x_1 x_2 w_5 w_4 3 w)))
 (= ?x6689 ?x3156))))
 ))
 (let (($x8658 (forall ((n (_ BitVec 6)) )(let ((?x2272 (sc_s 2)))
 (let (($x8494 (bvsle ?x2272 n)))
 (let ((?x690 (stack_s x_0 x_1 x_2 w_5 w_4 2 n)))
 (let ((?x9594 (stack_s x_0 x_1 x_2 w_5 w_4 3 n)))
 (or (= ?x9594 ?x690) $x8494))))))
 ))
 (let (($x11281 (= ?x3851 (bvadd (_ bv1 6) (sc_s 2)))))
 (let ((?x10695 (used_gas_s x_0 x_1 x_2 w_5 w_4 3)))
 (let (($x8780 (exc_halt_s 1)))
 (let (($x672 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 1)))) (_ bv0 1)))))
 (let (($x3248 (forall ((w (_ BitVec 256)) )(let ((?x6931 (storage_s x_0 x_1 x_2 w_5 w_4 1 w)))
 (let ((?x3156 (storage_s x_0 x_1 x_2 w_5 w_4 2 w)))
 (= ?x3156 ?x6931))))
 ))
 (let (($x8361 (forall ((n (_ BitVec 6)) )(let ((?x1590 (stack_s x_0 x_1 x_2 w_5 w_4 1 n)))
 (let ((?x690 (stack_s x_0 x_1 x_2 w_5 w_4 2 n)))
 (or (bvsle (bvadd (_ bv60 6) (sc_s 1)) n) (= ?x690 ?x1590)))))
 ))
 (let ((?x2272 (sc_s 2)))
 (let (($x7044 (= ?x2272 (bvadd (_ bv1 6) (sc_s 1)))))
 (let ((?x8138 (used_gas_s x_0 x_1 x_2 w_5 w_4 2)))
 (let (($x3396 (= (stack_s x_0 x_1 x_2 w_5 w_4 2 (bvadd (_ bv63 6) (sc_s 1))) (stack_s x_0 x_1 x_2 w_5 w_4 1 (bvadd (_ bv63 6) (sc_s 1))))))
 (let (($x9968 (= (stack_s x_0 x_1 x_2 w_5 w_4 2 (bvadd (_ bv62 6) (sc_s 1))) (stack_s x_0 x_1 x_2 w_5 w_4 1 (bvadd (_ bv62 6) (sc_s 1))))))
 (let (($x3007 (= (stack_s x_0 x_1 x_2 w_5 w_4 2 (bvadd (_ bv61 6) (sc_s 1))) (stack_s x_0 x_1 x_2 w_5 w_4 1 (bvadd (_ bv61 6) (sc_s 1))))))
 (let ((?x154 (sc_s 1)))
 (let ((?x3292 (bvadd (_ bv60 6) ?x154)))
 (let ((?x4495 (stack_s x_0 x_1 x_2 w_5 w_4 1 ?x3292)))
 (let (($x4896 (= (stack_s x_0 x_1 x_2 w_5 w_4 2 (bvadd (_ bv63 6) ?x2272)) ?x4495)))
 (let (($x10729 (= $x8780 (or $x57 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x72))) (_ bv0 1)))))))
 (let (($x1363 (forall ((w (_ BitVec 256)) )(let ((?x4494 (storage_s x_0 x_1 x_2 w_5 w_4 0 w)))
 (let ((?x6931 (storage_s x_0 x_1 x_2 w_5 w_4 1 w)))
 (= ?x6931 ?x4494))))
 ))
 (let (($x9421 (forall ((n (_ BitVec 6)) )(let ((?x72 (sc_s 0)))
 (let (($x8299 (bvsle ?x72 n)))
 (let ((?x3426 (stack_s x_0 x_1 x_2 w_5 w_4 0 n)))
 (let ((?x1590 (stack_s x_0 x_1 x_2 w_5 w_4 1 n)))
 (or (= ?x1590 ?x3426) $x8299))))))
 ))
 (let (($x4778 (= ?x154 (bvadd (_ bv1 6) ?x72))))
 (let (($x11619 (forall ((w (_ BitVec 256)) )(let ((?x4494 (storage_s x_0 x_1 x_2 w_5 w_4 0 w)))
 (= ?x4494 (_ bv0 256))))
 ))
 (let (($x10357 (= ?x10645 0)))
 (let (($x2276 (not $x57)))
 (let (($x2298 (= (stack_s x_0 x_1 x_2 w_5 w_4 0 (_ bv2 6)) x_2)))
 (let (($x6728 (= (stack_s x_0 x_1 x_2 w_5 w_4 0 (_ bv1 6)) x_1)))
 (let (($x6087 (= (stack_s x_0 x_1 x_2 w_5 w_4 0 (_ bv0 6)) x_0)))
 (let (($x10534 (= ?x72 (_ bv3 6))))
 (and $x10534 $x6087 $x6728 $x2298 $x2276 $x10357 $x11619 (= (stack_s x_0 x_1 x_2 w_5 w_4 1 ?x72) w_5) (= (used_gas_s x_0 x_1 x_2 w_5 w_4 1) (+ 3 ?x10645)) $x4778 $x9421 $x1363 $x10729 $x4896 (= (stack_s x_0 x_1 x_2 w_5 w_4 2 ?x3292) ?x4495) $x3007 $x9968 $x3396 (= ?x8138 (+ 3 (used_gas_s x_0 x_1 x_2 w_5 w_4 1))) $x7044 $x8361 $x3248 (= $x10052 (or $x672 (not (bvsle (_ bv0 6) ?x3292)) $x8780)) (= (stack_s x_0 x_1 x_2 w_5 w_4 3 ?x2272) w_4) (= ?x10695 (+ 3 ?x8138)) $x11281 $x8658 $x9911 $x2547 $x10342 $x2680 (= ?x1402 (+ 3 ?x10695)) $x2630 $x1189 $x464 $x10301 $x1630 $x2256 $x8577 $x10915 $x11305 $x8734 $x9277 $x3392 $x10759 (= (stack_t x_0 x_1 x_2 w_5 w_4 1 ?x7252) ?x2781) $x7248 $x7695 (= (used_gas_t x_0 x_1 x_2 w_5 w_4 1) (+ 3 ?x4735)) $x3631 $x5153 $x11561 (= $x4852 (or $x56 $x4057 $x6946)) (= (stack_t x_0 x_1 x_2 w_5 w_4 2 ?x9666) w_4) (= ?x614 (+ 3 (used_gas_t x_0 x_1 x_2 w_5 w_4 1))) $x3655 $x5628 $x1421 $x872 (= (stack_t x_0 x_1 x_2 w_5 w_4 3 ?x11248) w_5) $x1417 $x6668 $x11342 $x9039 $x9131 $x73 $x6054 $x58 $x6758 $x9428 (not (and $x6228 $x2505 $x5454 $x11971))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
 )
(check-sat)
(get-proof)