; PUSH cw_2 SWAP2 DUP3 SWAP1 => PUSH cw_2 PUSH cw_2 SWAP3
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
 (exists ((x_0 (_ BitVec 256)) (x_1 (_ BitVec 256)) )(let (($x11459 (forall ((w (_ BitVec 256)) )(let ((?x1653 (storage_t x_0 x_1 w_2 3 w)))
 (let ((?x11370 (storage_s x_0 x_1 w_2 4 w)))
 (= ?x11370 ?x1653))))
 ))
 (let (($x9131 (exc_halt_t 3)))
 (let (($x9175 (exc_halt_s 4)))
 (let (($x5897 (= $x9175 $x9131)))
 (let (($x6714 (forall ((n (_ BitVec 6)) )(let ((?x11203 (stack_t x_0 x_1 w_2 3 n)))
 (let ((?x2531 (stack_s x_0 x_1 w_2 4 n)))
 (let (($x9402 (= ?x2531 ?x11203)))
 (let ((?x10013 (sc_t 3)))
 (let (($x2843 (bvsle ?x10013 n)))
 (or $x2843 $x9402)))))))
 ))
 (let ((?x10013 (sc_t 3)))
 (let ((?x9433 (sc_s 4)))
 (let (($x5952 (= ?x9433 ?x10013)))
 (let ((?x7713 (used_gas_t x_0 x_1 w_2 0)))
 (let ((?x5485 (used_gas_s x_0 x_1 w_2 0)))
 (let (($x6793 (= ?x5485 ?x7713)))
 (let (($x1946 (forall ((w (_ BitVec 256)) )(let ((?x1993 (storage_t x_0 x_1 w_2 0 w)))
 (let ((?x4169 (storage_s x_0 x_1 w_2 0 w)))
 (= ?x4169 ?x1993))))
 ))
 (let (($x56 (exc_halt_t 0)))
 (let (($x57 (exc_halt_s 0)))
 (let (($x58 (= $x57 $x56)))
 (let (($x4281 (forall ((n (_ BitVec 6)) )(let ((?x63 (sc_t 0)))
 (let (($x9079 (bvsle ?x63 n)))
 (let ((?x9030 (stack_t x_0 x_1 w_2 0 n)))
 (let ((?x5379 (stack_s x_0 x_1 w_2 0 n)))
 (let (($x688 (= ?x5379 ?x9030)))
 (or $x688 $x9079)))))))
 ))
 (let ((?x63 (sc_t 0)))
 (let ((?x72 (sc_s 0)))
 (let (($x73 (= ?x72 ?x63)))
 (let (($x5609 (= $x9131 (or (exc_halt_t 2) (not (bvsle (_ bv0 6) (bvadd (_ bv60 6) (sc_t 2))))))))
 (let (($x9789 (forall ((w (_ BitVec 256)) )(let ((?x10629 (storage_t x_0 x_1 w_2 2 w)))
 (let ((?x1653 (storage_t x_0 x_1 w_2 3 w)))
 (= ?x1653 ?x10629))))
 ))
 (let (($x754 (forall ((n (_ BitVec 6)) )(let ((?x8213 (stack_t x_0 x_1 w_2 2 n)))
 (let ((?x11203 (stack_t x_0 x_1 w_2 3 n)))
 (or (bvsle (bvadd (_ bv60 6) (sc_t 2)) n) (= ?x11203 ?x8213)))))
 ))
 (let ((?x11248 (sc_t 2)))
 (let (($x11942 (= ?x10013 ?x11248)))
 (let (($x7722 (= (used_gas_t x_0 x_1 w_2 3) (+ 3 (used_gas_t x_0 x_1 w_2 2)))))
 (let (($x11158 (= (stack_t x_0 x_1 w_2 3 (bvadd (_ bv62 6) ?x10013)) (stack_t x_0 x_1 w_2 2 (bvadd (_ bv62 6) ?x11248)))))
 (let (($x4644 (= (stack_t x_0 x_1 w_2 3 (bvadd (_ bv61 6) ?x10013)) (stack_t x_0 x_1 w_2 2 (bvadd (_ bv61 6) ?x11248)))))
 (let (($x11547 (= (stack_t x_0 x_1 w_2 3 (bvadd (_ bv60 6) ?x10013)) (stack_t x_0 x_1 w_2 2 (bvadd (_ bv63 6) ?x11248)))))
 (let (($x11673 (= (stack_t x_0 x_1 w_2 3 (bvadd (_ bv63 6) ?x10013)) (stack_t x_0 x_1 w_2 2 (bvadd (_ bv60 6) ?x11248)))))
 (let (($x4852 (exc_halt_t 1)))
 (let (($x8809 (or $x4852 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_t 1)))) (_ bv0 1))))))
 (let (($x4057 (exc_halt_t 2)))
 (let (($x4116 (= $x4057 $x8809)))
 (let (($x1425 (forall ((w (_ BitVec 256)) )(let ((?x8115 (storage_t x_0 x_1 w_2 1 w)))
 (let ((?x10629 (storage_t x_0 x_1 w_2 2 w)))
 (= ?x10629 ?x8115))))
 ))
 (let (($x7511 (forall ((n (_ BitVec 6)) )(let ((?x9666 (sc_t 1)))
 (let (($x9963 (bvsle ?x9666 n)))
 (let ((?x11192 (stack_t x_0 x_1 w_2 1 n)))
 (let ((?x8213 (stack_t x_0 x_1 w_2 2 n)))
 (or (= ?x8213 ?x11192) $x9963))))))
 ))
 (let (($x11433 (= ?x11248 (bvadd (_ bv1 6) (sc_t 1)))))
 (let ((?x11197 (used_gas_t x_0 x_1 w_2 2)))
 (let (($x3469 (= $x4852 (or $x56 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x63))) (_ bv0 1)))))))
 (let (($x3486 (forall ((w (_ BitVec 256)) )(let ((?x1993 (storage_t x_0 x_1 w_2 0 w)))
 (let ((?x8115 (storage_t x_0 x_1 w_2 1 w)))
 (= ?x8115 ?x1993))))
 ))
 (let (($x10997 (forall ((n (_ BitVec 6)) )(let ((?x63 (sc_t 0)))
 (let (($x9079 (bvsle ?x63 n)))
 (let ((?x9030 (stack_t x_0 x_1 w_2 0 n)))
 (let ((?x11192 (stack_t x_0 x_1 w_2 1 n)))
 (or (= ?x11192 ?x9030) $x9079))))))
 ))
 (let ((?x9666 (sc_t 1)))
 (let (($x9898 (= ?x9666 (bvadd (_ bv1 6) ?x63))))
 (let (($x1118 (= $x9175 (or (exc_halt_s 3) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 3))))))))
 (let (($x2516 (forall ((w (_ BitVec 256)) )(let ((?x3170 (storage_s x_0 x_1 w_2 3 w)))
 (let ((?x11370 (storage_s x_0 x_1 w_2 4 w)))
 (= ?x11370 ?x3170))))
 ))
 (let (($x9885 (forall ((n (_ BitVec 6)) )(let ((?x3851 (sc_s 3)))
 (let ((?x1039 (bvadd (_ bv62 6) ?x3851)))
 (let (($x11348 (bvsle ?x1039 n)))
 (let ((?x10637 (stack_s x_0 x_1 w_2 3 n)))
 (let ((?x2531 (stack_s x_0 x_1 w_2 4 n)))
 (or (= ?x2531 ?x10637) $x11348)))))))
 ))
 (let ((?x3851 (sc_s 3)))
 (let (($x9390 (= ?x9433 ?x3851)))
 (let (($x203 (= (used_gas_s x_0 x_1 w_2 4) (+ 3 (used_gas_s x_0 x_1 w_2 3)))))
 (let ((?x998 (bvadd (_ bv63 6) ?x3851)))
 (let ((?x11278 (stack_s x_0 x_1 w_2 3 ?x998)))
 (let (($x7474 (= (stack_s x_0 x_1 w_2 4 (bvadd (_ bv63 6) ?x9433)) (stack_s x_0 x_1 w_2 3 (bvadd (_ bv62 6) ?x3851)))))
 (let (($x11311 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 2)))) (_ bv0 1)))))
 (let (($x10052 (exc_halt_s 2)))
 (let (($x8103 (exc_halt_s 3)))
 (let (($x3049 (forall ((w (_ BitVec 256)) )(let ((?x10669 (storage_s x_0 x_1 w_2 2 w)))
 (let ((?x3170 (storage_s x_0 x_1 w_2 3 w)))
 (= ?x3170 ?x10669))))
 ))
 (let (($x410 (forall ((n (_ BitVec 6)) )(let ((?x10650 (stack_s x_0 x_1 w_2 2 n)))
 (let ((?x10637 (stack_s x_0 x_1 w_2 3 n)))
 (or (bvsle (bvadd (_ bv61 6) (sc_s 2)) n) (= ?x10637 ?x10650)))))
 ))
 (let (($x10998 (= ?x3851 (bvadd (_ bv1 6) (sc_s 2)))))
 (let ((?x3997 (used_gas_s x_0 x_1 w_2 3)))
 (let ((?x2272 (sc_s 2)))
 (let ((?x5528 (bvadd (_ bv63 6) ?x2272)))
 (let ((?x3008 (stack_s x_0 x_1 w_2 2 ?x5528)))
 (let ((?x9266 (bvadd (_ bv62 6) ?x2272)))
 (let ((?x4117 (stack_s x_0 x_1 w_2 2 ?x9266)))
 (let ((?x7406 (bvadd (_ bv61 6) ?x2272)))
 (let ((?x8487 (stack_s x_0 x_1 w_2 2 ?x7406)))
 (let (($x5325 (= $x10052 (or (exc_halt_s 1) (not (bvsle (_ bv0 6) (bvadd (_ bv61 6) (sc_s 1))))))))
 (let (($x8002 (forall ((w (_ BitVec 256)) )(let ((?x5416 (storage_s x_0 x_1 w_2 1 w)))
 (let ((?x10669 (storage_s x_0 x_1 w_2 2 w)))
 (= ?x10669 ?x5416))))
 ))
 (let (($x9856 (forall ((n (_ BitVec 6)) )(let ((?x7424 (stack_s x_0 x_1 w_2 1 n)))
 (let ((?x10650 (stack_s x_0 x_1 w_2 2 n)))
 (or (= ?x10650 ?x7424) (bvsle (bvadd (_ bv61 6) (sc_s 1)) n)))))
 ))
 (let ((?x154 (sc_s 1)))
 (let (($x2223 (= ?x2272 ?x154)))
 (let ((?x2640 (used_gas_s x_0 x_1 w_2 2)))
 (let (($x8780 (exc_halt_s 1)))
 (let (($x5465 (= $x8780 (or $x57 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x72))) (_ bv0 1)))))))
 (let (($x611 (forall ((w (_ BitVec 256)) )(let ((?x4169 (storage_s x_0 x_1 w_2 0 w)))
 (let ((?x5416 (storage_s x_0 x_1 w_2 1 w)))
 (= ?x5416 ?x4169))))
 ))
 (let (($x4144 (forall ((n (_ BitVec 6)) )(let ((?x5379 (stack_s x_0 x_1 w_2 0 n)))
 (let ((?x7424 (stack_s x_0 x_1 w_2 1 n)))
 (let ((?x72 (sc_s 0)))
 (let (($x4347 (bvsle ?x72 n)))
 (or $x4347 (= ?x7424 ?x5379)))))))
 ))
 (let (($x10888 (= ?x154 (bvadd (_ bv1 6) ?x72))))
 (let (($x9426 (forall ((w (_ BitVec 256)) )(let ((?x4169 (storage_s x_0 x_1 w_2 0 w)))
 (= ?x4169 (_ bv0 256))))
 ))
 (let (($x52 (= ?x5485 0)))
 (let (($x10995 (not $x57)))
 (let (($x9988 (= (stack_s x_0 x_1 w_2 0 (_ bv1 6)) x_1)))
 (let (($x11261 (= (stack_s x_0 x_1 w_2 0 (_ bv0 6)) x_0)))
 (let (($x11128 (= ?x72 (_ bv2 6))))
 (and $x11128 $x11261 $x9988 $x10995 $x52 $x9426 (= (stack_s x_0 x_1 w_2 1 ?x72) w_2) (= (used_gas_s x_0 x_1 w_2 1) (+ 3 ?x5485)) $x10888 $x4144 $x611 $x5465 (= ?x3008 (stack_s x_0 x_1 w_2 1 (bvadd (_ bv61 6) ?x154))) (= ?x8487 (stack_s x_0 x_1 w_2 1 (bvadd (_ bv63 6) ?x154))) (= ?x4117 (stack_s x_0 x_1 w_2 1 (bvadd (_ bv62 6) ?x154))) (= ?x2640 (+ 3 (used_gas_s x_0 x_1 w_2 1))) $x2223 $x9856 $x8002 $x5325 (= ?x11278 ?x8487) (= (stack_s x_0 x_1 w_2 3 ?x7406) ?x8487) (= (stack_s x_0 x_1 w_2 3 ?x9266) ?x4117) (= (stack_s x_0 x_1 w_2 3 ?x5528) ?x3008) (= ?x3997 (+ 3 ?x2640)) $x10998 $x410 $x3049 (= $x8103 (or (not (bvsle (_ bv0 6) ?x7406)) $x10052 $x11311)) $x7474 (= (stack_s x_0 x_1 w_2 4 (bvadd (_ bv62 6) ?x9433)) ?x11278) $x203 $x9390 $x9885 $x2516 $x1118 (= (stack_t x_0 x_1 w_2 1 ?x63) w_2) (= (used_gas_t x_0 x_1 w_2 1) (+ 3 ?x7713)) $x9898 $x10997 $x3486 $x3469 (= (stack_t x_0 x_1 w_2 2 ?x9666) w_2) (= ?x11197 (+ 3 (used_gas_t x_0 x_1 w_2 1))) $x11433 $x7511 $x1425 $x4116 $x11673 $x11547 $x4644 $x11158 $x7722 $x11942 $x754 $x9789 $x5609 $x73 $x4281 $x58 $x1946 $x6793 (not (and $x5952 $x6714 $x5897 $x11459))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
 )
(check-sat)
(get-proof)