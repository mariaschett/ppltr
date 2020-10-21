; PUSH cw_1 SWAP2 SWAP1 DUP2 SWAP1 => DUP2 PUSH cw_1 SWAP3 SWAP2
(set-option :produce-proofs true)
; 
(set-info :status unknown)
(declare-fun storage_t ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 256)) (_ BitVec 256))
(declare-fun w_1 () (_ BitVec 256))
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
 (exists ((x_0 (_ BitVec 256)) (x_1 (_ BitVec 256)) )(let (($x5043 (forall ((w (_ BitVec 256)) )(let ((?x474 (storage_t x_0 x_1 w_1 4 w)))
 (let ((?x1614 (storage_s x_0 x_1 w_1 5 w)))
 (= ?x1614 ?x474))))
 ))
 (let (($x7854 (exc_halt_t 4)))
 (let (($x1862 (exc_halt_s 5)))
 (let (($x1994 (= $x1862 $x7854)))
 (let (($x9734 (forall ((n (_ BitVec 6)) )(let ((?x7495 (sc_t 4)))
 (let (($x9682 (bvsle ?x7495 n)))
 (let ((?x7428 (stack_t x_0 x_1 w_1 4 n)))
 (let ((?x7964 (stack_s x_0 x_1 w_1 5 n)))
 (let (($x7148 (= ?x7964 ?x7428)))
 (or $x7148 $x9682)))))))
 ))
 (let ((?x7495 (sc_t 4)))
 (let ((?x4319 (sc_s 5)))
 (let (($x3074 (= ?x4319 ?x7495)))
 (let ((?x6035 (used_gas_t x_0 x_1 w_1 0)))
 (let ((?x5897 (used_gas_s x_0 x_1 w_1 0)))
 (let (($x3418 (= ?x5897 ?x6035)))
 (let (($x259 (forall ((w (_ BitVec 256)) )(let ((?x566 (storage_t x_0 x_1 w_1 0 w)))
 (let ((?x5291 (storage_s x_0 x_1 w_1 0 w)))
 (= ?x5291 ?x566))))
 ))
 (let (($x56 (exc_halt_t 0)))
 (let (($x57 (exc_halt_s 0)))
 (let (($x58 (= $x57 $x56)))
 (let (($x8025 (forall ((n (_ BitVec 6)) )(let ((?x63 (sc_t 0)))
 (let (($x7659 (bvsle ?x63 n)))
 (let ((?x7154 (stack_t x_0 x_1 w_1 0 n)))
 (let ((?x2246 (stack_s x_0 x_1 w_1 0 n)))
 (let (($x278 (= ?x2246 ?x7154)))
 (or $x278 $x7659)))))))
 ))
 (let ((?x63 (sc_t 0)))
 (let ((?x72 (sc_s 0)))
 (let (($x73 (= ?x72 ?x63)))
 (let (($x7886 (= $x7854 (or (exc_halt_t 3) (not (bvsle (_ bv0 6) (bvadd (_ bv61 6) (sc_t 3))))))))
 (let (($x11582 (forall ((w (_ BitVec 256)) )(let ((?x5772 (storage_t x_0 x_1 w_1 3 w)))
 (let ((?x474 (storage_t x_0 x_1 w_1 4 w)))
 (= ?x474 ?x5772))))
 ))
 (let (($x10913 (forall ((n (_ BitVec 6)) )(let ((?x1038 (stack_t x_0 x_1 w_1 3 n)))
 (let ((?x7428 (stack_t x_0 x_1 w_1 4 n)))
 (or (= ?x7428 ?x1038) (bvsle (bvadd (_ bv61 6) (sc_t 3)) n)))))
 ))
 (let ((?x10013 (sc_t 3)))
 (let (($x6034 (= ?x7495 ?x10013)))
 (let (($x5557 (= (used_gas_t x_0 x_1 w_1 4) (+ 3 (used_gas_t x_0 x_1 w_1 3)))))
 (let ((?x4646 (bvadd (_ bv62 6) ?x10013)))
 (let ((?x501 (stack_t x_0 x_1 w_1 3 ?x4646)))
 (let ((?x1593 (bvadd (_ bv63 6) ?x10013)))
 (let ((?x5302 (stack_t x_0 x_1 w_1 3 ?x1593)))
 (let ((?x4728 (bvadd (_ bv61 6) ?x10013)))
 (let ((?x5908 (stack_t x_0 x_1 w_1 3 ?x4728)))
 (let (($x9131 (exc_halt_t 3)))
 (let (($x7815 (= $x9131 (or (exc_halt_t 2) (not (bvsle (_ bv0 6) (bvadd (_ bv60 6) (sc_t 2))))))))
 (let (($x9680 (forall ((w (_ BitVec 256)) )(let ((?x4499 (storage_t x_0 x_1 w_1 2 w)))
 (let ((?x5772 (storage_t x_0 x_1 w_1 3 w)))
 (= ?x5772 ?x4499))))
 ))
 (let (($x3934 (forall ((n (_ BitVec 6)) )(let ((?x6180 (stack_t x_0 x_1 w_1 2 n)))
 (let ((?x1038 (stack_t x_0 x_1 w_1 3 n)))
 (or (= ?x1038 ?x6180) (bvsle (bvadd (_ bv60 6) (sc_t 2)) n)))))
 ))
 (let ((?x8352 (used_gas_t x_0 x_1 w_1 3)))
 (let (($x7207 (= (stack_t x_0 x_1 w_1 3 (bvadd (_ bv60 6) ?x10013)) (stack_t x_0 x_1 w_1 2 (bvadd (_ bv63 6) (sc_t 2))))))
 (let (($x5947 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_t 1)))) (_ bv0 1)))))
 (let (($x4852 (exc_halt_t 1)))
 (let (($x8186 (exc_halt_t 2)))
 (let (($x11228 (= $x8186 (or $x4852 $x5947))))
 (let (($x245 (forall ((w (_ BitVec 256)) )(let ((?x6033 (storage_t x_0 x_1 w_1 1 w)))
 (let ((?x4499 (storage_t x_0 x_1 w_1 2 w)))
 (= ?x4499 ?x6033))))
 ))
 (let (($x331 (forall ((n (_ BitVec 6)) )(let ((?x6776 (stack_t x_0 x_1 w_1 1 n)))
 (let ((?x6180 (stack_t x_0 x_1 w_1 2 n)))
 (let ((?x9666 (sc_t 1)))
 (let (($x6584 (bvsle ?x9666 n)))
 (or $x6584 (= ?x6180 ?x6776)))))))
 ))
 (let ((?x11248 (sc_t 2)))
 (let (($x902 (= ?x11248 (bvadd (_ bv1 6) (sc_t 1)))))
 (let ((?x8316 (used_gas_t x_0 x_1 w_1 2)))
 (let (($x124 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x63))) (_ bv0 1)))))
 (let (($x6760 (forall ((w (_ BitVec 256)) )(let ((?x566 (storage_t x_0 x_1 w_1 0 w)))
 (let ((?x6033 (storage_t x_0 x_1 w_1 1 w)))
 (= ?x6033 ?x566))))
 ))
 (let (($x10396 (forall ((n (_ BitVec 6)) )(let ((?x7154 (stack_t x_0 x_1 w_1 0 n)))
 (let ((?x6776 (stack_t x_0 x_1 w_1 1 n)))
 (or (= ?x6776 ?x7154) (bvsle (bvadd (_ bv62 6) (sc_t 0)) n)))))
 ))
 (let ((?x9666 (sc_t 1)))
 (let (($x5368 (= ?x9666 (bvadd (_ bv1 6) ?x63))))
 (let (($x7808 (= (stack_t x_0 x_1 w_1 1 (bvadd (_ bv63 6) ?x63)) (stack_t x_0 x_1 w_1 0 (bvadd (_ bv63 6) ?x63)))))
 (let ((?x8595 (bvadd (_ bv62 6) ?x63)))
 (let ((?x2011 (stack_t x_0 x_1 w_1 0 ?x8595)))
 (let (($x10460 (= $x1862 (or (exc_halt_s 4) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 4))))))))
 (let (($x1190 (forall ((w (_ BitVec 256)) )(let ((?x2730 (storage_s x_0 x_1 w_1 4 w)))
 (let ((?x1614 (storage_s x_0 x_1 w_1 5 w)))
 (= ?x1614 ?x2730))))
 ))
 (let (($x6250 (forall ((n (_ BitVec 6)) )(let ((?x10792 (stack_s x_0 x_1 w_1 4 n)))
 (let ((?x7964 (stack_s x_0 x_1 w_1 5 n)))
 (let ((?x9433 (sc_s 4)))
 (let ((?x3168 (bvadd (_ bv62 6) ?x9433)))
 (let (($x6251 (bvsle ?x3168 n)))
 (or $x6251 (= ?x7964 ?x10792))))))))
 ))
 (let ((?x9433 (sc_s 4)))
 (let (($x395 (= ?x4319 ?x9433)))
 (let (($x11071 (= (used_gas_s x_0 x_1 w_1 5) (+ 3 (used_gas_s x_0 x_1 w_1 4)))))
 (let ((?x4784 (bvadd (_ bv63 6) ?x9433)))
 (let ((?x10550 (stack_s x_0 x_1 w_1 4 ?x4784)))
 (let (($x10936 (= (stack_s x_0 x_1 w_1 5 (bvadd (_ bv63 6) ?x4319)) (stack_s x_0 x_1 w_1 4 (bvadd (_ bv62 6) ?x9433)))))
 (let (($x10959 (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 3))))))
 (let (($x8103 (exc_halt_s 3)))
 (let (($x6433 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 3)))) (_ bv0 1)))))
 (let (($x9175 (exc_halt_s 4)))
 (let (($x10634 (forall ((w (_ BitVec 256)) )(let ((?x6273 (storage_s x_0 x_1 w_1 3 w)))
 (let ((?x2730 (storage_s x_0 x_1 w_1 4 w)))
 (= ?x2730 ?x6273))))
 ))
 (let (($x10476 (forall ((n (_ BitVec 6)) )(let ((?x3851 (sc_s 3)))
 (let ((?x9007 (bvadd (_ bv62 6) ?x3851)))
 (let (($x5207 (bvsle ?x9007 n)))
 (let ((?x6935 (stack_s x_0 x_1 w_1 3 n)))
 (let ((?x10792 (stack_s x_0 x_1 w_1 4 n)))
 (or (= ?x10792 ?x6935) $x5207)))))))
 ))
 (let (($x2045 (= ?x9433 (bvadd (_ bv1 6) (sc_s 3)))))
 (let ((?x8974 (used_gas_s x_0 x_1 w_1 4)))
 (let ((?x3851 (sc_s 3)))
 (let ((?x1770 (bvadd (_ bv63 6) ?x3851)))
 (let ((?x9895 (stack_s x_0 x_1 w_1 3 ?x1770)))
 (let ((?x9007 (bvadd (_ bv62 6) ?x3851)))
 (let ((?x5770 (stack_s x_0 x_1 w_1 3 ?x9007)))
 (let (($x9252 (= $x8103 (or (exc_halt_s 2) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 2))))))))
 (let (($x9830 (forall ((w (_ BitVec 256)) )(let ((?x7220 (storage_s x_0 x_1 w_1 2 w)))
 (let ((?x6273 (storage_s x_0 x_1 w_1 3 w)))
 (= ?x6273 ?x7220))))
 ))
 (let (($x2780 (forall ((n (_ BitVec 6)) )(let ((?x6610 (stack_s x_0 x_1 w_1 2 n)))
 (let ((?x6935 (stack_s x_0 x_1 w_1 3 n)))
 (let ((?x2272 (sc_s 2)))
 (let ((?x5303 (bvadd (_ bv62 6) ?x2272)))
 (let (($x1494 (bvsle ?x5303 n)))
 (or $x1494 (= ?x6935 ?x6610))))))))
 ))
 (let ((?x2272 (sc_s 2)))
 (let (($x3963 (= ?x3851 ?x2272)))
 (let ((?x6960 (used_gas_s x_0 x_1 w_1 3)))
 (let (($x10052 (exc_halt_s 2)))
 (let (($x9951 (= $x10052 (or (exc_halt_s 1) (not (bvsle (_ bv0 6) (bvadd (_ bv61 6) (sc_s 1))))))))
 (let (($x1496 (forall ((w (_ BitVec 256)) )(let ((?x3817 (storage_s x_0 x_1 w_1 1 w)))
 (let ((?x7220 (storage_s x_0 x_1 w_1 2 w)))
 (= ?x7220 ?x3817))))
 ))
 (let (($x2945 (forall ((n (_ BitVec 6)) )(let ((?x4466 (stack_s x_0 x_1 w_1 1 n)))
 (let ((?x6610 (stack_s x_0 x_1 w_1 2 n)))
 (or (bvsle (bvadd (_ bv61 6) (sc_s 1)) n) (= ?x6610 ?x4466)))))
 ))
 (let ((?x154 (sc_s 1)))
 (let (($x2298 (= ?x2272 ?x154)))
 (let ((?x9258 (used_gas_s x_0 x_1 w_1 2)))
 (let ((?x5303 (bvadd (_ bv62 6) ?x2272)))
 (let ((?x2322 (stack_s x_0 x_1 w_1 2 ?x5303)))
 (let (($x1589 (= (stack_s x_0 x_1 w_1 2 (bvadd (_ bv61 6) ?x2272)) (stack_s x_0 x_1 w_1 1 (bvadd (_ bv63 6) ?x154)))))
 (let ((?x8276 (bvadd (_ bv63 6) ?x2272)))
 (let ((?x11745 (stack_s x_0 x_1 w_1 2 ?x8276)))
 (let (($x8780 (exc_halt_s 1)))
 (let (($x8607 (= $x8780 (or $x57 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x72))) (_ bv0 1)))))))
 (let (($x10385 (forall ((w (_ BitVec 256)) )(let ((?x5291 (storage_s x_0 x_1 w_1 0 w)))
 (let ((?x3817 (storage_s x_0 x_1 w_1 1 w)))
 (= ?x3817 ?x5291))))
 ))
 (let (($x8919 (forall ((n (_ BitVec 6)) )(let ((?x2246 (stack_s x_0 x_1 w_1 0 n)))
 (let ((?x4466 (stack_s x_0 x_1 w_1 1 n)))
 (let ((?x72 (sc_s 0)))
 (let (($x949 (bvsle ?x72 n)))
 (or $x949 (= ?x4466 ?x2246)))))))
 ))
 (let (($x6719 (= ?x154 (bvadd (_ bv1 6) ?x72))))
 (let (($x6299 (forall ((w (_ BitVec 256)) )(let ((?x5291 (storage_s x_0 x_1 w_1 0 w)))
 (= ?x5291 (_ bv0 256))))
 ))
 (let (($x10668 (= ?x5897 0)))
 (let (($x6704 (not $x57)))
 (let (($x11096 (= (stack_s x_0 x_1 w_1 0 (_ bv1 6)) x_1)))
 (let (($x11269 (= (stack_s x_0 x_1 w_1 0 (_ bv0 6)) x_0)))
 (let (($x11128 (= ?x72 (_ bv2 6))))
 (and $x11128 $x11269 $x11096 $x6704 $x10668 $x6299 (= (stack_s x_0 x_1 w_1 1 ?x72) w_1) (= (used_gas_s x_0 x_1 w_1 1) (+ 3 ?x5897)) $x6719 $x8919 $x10385 $x8607 (= ?x11745 (stack_s x_0 x_1 w_1 1 (bvadd (_ bv61 6) ?x154))) $x1589 (= ?x2322 (stack_s x_0 x_1 w_1 1 (bvadd (_ bv62 6) ?x154))) (= ?x9258 (+ 3 (used_gas_s x_0 x_1 w_1 1))) $x2298 $x2945 $x1496 $x9951 (= ?x9895 ?x2322) (= ?x5770 ?x11745) (= ?x6960 (+ 3 ?x9258)) $x3963 $x2780 $x9830 $x9252 (= ?x10550 ?x5770) (= (stack_s x_0 x_1 w_1 4 ?x9007) ?x5770) (= (stack_s x_0 x_1 w_1 4 ?x1770) ?x9895) (= ?x8974 (+ 3 ?x6960)) $x2045 $x10476 $x10634 (= $x9175 (or $x6433 $x8103 $x10959)) $x10936 (= (stack_s x_0 x_1 w_1 5 (bvadd (_ bv62 6) ?x4319)) ?x10550) $x11071 $x395 $x6250 $x1190 $x10460 (= (stack_t x_0 x_1 w_1 1 (bvadd (_ bv63 6) ?x9666)) ?x2011) (= (stack_t x_0 x_1 w_1 1 ?x8595) ?x2011) $x7808 (= (used_gas_t x_0 x_1 w_1 1) (+ 3 ?x6035)) $x5368 $x10396 $x6760 (= $x4852 (or $x56 $x124 (not (bvsle (_ bv0 6) ?x8595)))) (= (stack_t x_0 x_1 w_1 2 ?x9666) w_1) (= ?x8316 (+ 3 (used_gas_t x_0 x_1 w_1 1))) $x902 $x331 $x245 $x11228 (= ?x5302 (stack_t x_0 x_1 w_1 2 (bvadd (_ bv60 6) ?x11248))) $x7207 (= ?x5908 (stack_t x_0 x_1 w_1 2 (bvadd (_ bv61 6) ?x11248))) (= ?x501 (stack_t x_0 x_1 w_1 2 (bvadd (_ bv62 6) ?x11248))) (= ?x8352 (+ 3 ?x8316)) (= ?x10013 ?x11248) $x3934 $x9680 $x7815 (= (stack_t x_0 x_1 w_1 4 (bvadd (_ bv63 6) ?x7495)) ?x5908) (= (stack_t x_0 x_1 w_1 4 (bvadd (_ bv61 6) ?x7495)) ?x5302) (= (stack_t x_0 x_1 w_1 4 (bvadd (_ bv62 6) ?x7495)) ?x501) $x5557 $x6034 $x10913 $x11582 $x7886 $x73 $x8025 $x58 $x259 $x3418 (not (and $x3074 $x9734 $x1994 $x5043)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
 )
(check-sat)
(get-proof)
