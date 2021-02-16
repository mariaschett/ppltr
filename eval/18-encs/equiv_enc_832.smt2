; PUSH cw_7 PUSH cw_8 PUSH cw_7 SWAP1 => PUSH cw_7 DUP1 PUSH cw_8
(set-option :produce-proofs true)
; 
(set-info :status unknown)
(declare-fun storage_t ((_ BitVec 256) (_ BitVec 256) Int (_ BitVec 256)) (_ BitVec 256))
(declare-fun w_8 () (_ BitVec 256))
(declare-fun w_7 () (_ BitVec 256))
(declare-fun storage_s ((_ BitVec 256) (_ BitVec 256) Int (_ BitVec 256)) (_ BitVec 256))
(declare-fun exc_halt_t (Int) Bool)
(declare-fun exc_halt_s (Int) Bool)
(declare-fun stack_t ((_ BitVec 256) (_ BitVec 256) Int (_ BitVec 6)) (_ BitVec 256))
(declare-fun stack_s ((_ BitVec 256) (_ BitVec 256) Int (_ BitVec 6)) (_ BitVec 256))
(declare-fun sc_t (Int) (_ BitVec 6))
(declare-fun sc_s (Int) (_ BitVec 6))
(declare-fun used_gas_t ((_ BitVec 256) (_ BitVec 256) Int) Int)
(declare-fun used_gas_s ((_ BitVec 256) (_ BitVec 256) Int) Int)
(assert
 (let (($x663 (forall ((w (_ BitVec 256)) )(let ((?x4017 (storage_t w_7 w_8 3 w)))
 (let ((?x10139 (storage_s w_7 w_8 4 w)))
 (= ?x10139 ?x4017))))
 ))
 (let (($x418 (exc_halt_t 3)))
 (let (($x9175 (exc_halt_s 4)))
 (let (($x3479 (= $x9175 $x418)))
 (let (($x1321 (forall ((n (_ BitVec 6)) )(let ((?x2117 (stack_t w_7 w_8 3 n)))
 (let ((?x11089 (stack_s w_7 w_8 4 n)))
 (let (($x10598 (= ?x11089 ?x2117)))
 (let ((?x9232 (sc_t 3)))
 (let (($x3530 (bvsle ?x9232 n)))
 (or $x3530 $x10598)))))))
 ))
 (let ((?x9232 (sc_t 3)))
 (let ((?x9433 (sc_s 4)))
 (let (($x1048 (= ?x9433 ?x9232)))
 (let ((?x11849 (used_gas_t w_7 w_8 0)))
 (let ((?x8968 (used_gas_s w_7 w_8 0)))
 (let (($x10682 (= ?x8968 ?x11849)))
 (let (($x9980 (forall ((w (_ BitVec 256)) )(let ((?x5629 (storage_t w_7 w_8 0 w)))
 (let ((?x10220 (storage_s w_7 w_8 0 w)))
 (= ?x10220 ?x5629))))
 ))
 (let (($x56 (exc_halt_t 0)))
 (let (($x57 (exc_halt_s 0)))
 (let (($x58 (= $x57 $x56)))
 (let (($x9021 (forall ((n (_ BitVec 6)) )(let ((?x63 (sc_t 0)))
 (let (($x2738 (bvsle ?x63 n)))
 (let ((?x1965 (stack_t w_7 w_8 0 n)))
 (let ((?x5244 (stack_s w_7 w_8 0 n)))
 (let (($x537 (= ?x5244 ?x1965)))
 (or $x537 $x2738)))))))
 ))
 (let ((?x63 (sc_t 0)))
 (let ((?x72 (sc_s 0)))
 (let (($x73 (= ?x72 ?x63)))
 (let (($x1842 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_t 2)))) (_ bv0 1)))))
 (let (($x7905 (exc_halt_t 2)))
 (let (($x9131 (= $x418 (or $x7905 $x1842))))
 (let (($x4693 (forall ((w (_ BitVec 256)) )(let ((?x1764 (storage_t w_7 w_8 2 w)))
 (let ((?x4017 (storage_t w_7 w_8 3 w)))
 (= ?x4017 ?x1764))))
 ))
 (let (($x2933 (forall ((n (_ BitVec 6)) )(let ((?x11248 (sc_t 2)))
 (let (($x9965 (bvsle ?x11248 n)))
 (or (= (stack_t w_7 w_8 3 n) (stack_t w_7 w_8 2 n)) $x9965))))
 ))
 (let (($x6668 (= ?x9232 (bvadd (_ bv1 6) (sc_t 2)))))
 (let (($x5602 (= (used_gas_t w_7 w_8 3) (+ 3 (used_gas_t w_7 w_8 2)))))
 (let (($x4852 (exc_halt_t 1)))
 (let (($x1824 (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_t 1))))))
 (let (($x529 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_t 1)))) (_ bv0 1)))))
 (let (($x2432 (= $x7905 (or $x529 $x1824 $x4852))))
 (let (($x3197 (forall ((w (_ BitVec 256)) )(let ((?x5453 (storage_t w_7 w_8 1 w)))
 (let ((?x1764 (storage_t w_7 w_8 2 w)))
 (= ?x1764 ?x5453))))
 ))
 (let (($x2388 (forall ((n (_ BitVec 6)) )(let ((?x9666 (sc_t 1)))
 (let ((?x10665 (bvadd (_ bv63 6) ?x9666)))
 (let (($x5116 (bvsle ?x10665 n)))
 (or (= (stack_t w_7 w_8 2 n) (stack_t w_7 w_8 1 n)) $x5116)))))
 ))
 (let ((?x11248 (sc_t 2)))
 (let (($x3655 (= ?x11248 (bvadd (_ bv1 6) (sc_t 1)))))
 (let ((?x2097 (used_gas_t w_7 w_8 2)))
 (let ((?x9666 (sc_t 1)))
 (let ((?x10665 (bvadd (_ bv63 6) ?x9666)))
 (let ((?x8674 (stack_t w_7 w_8 1 ?x10665)))
 (let (($x489 (= $x4852 (or $x56 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x63))) (_ bv0 1)))))))
 (let (($x2480 (forall ((w (_ BitVec 256)) )(let ((?x5629 (storage_t w_7 w_8 0 w)))
 (let ((?x5453 (storage_t w_7 w_8 1 w)))
 (= ?x5453 ?x5629))))
 ))
 (let (($x830 (forall ((n (_ BitVec 6)) )(let ((?x63 (sc_t 0)))
 (let (($x2738 (bvsle ?x63 n)))
 (or $x2738 (= (stack_t w_7 w_8 1 n) (stack_t w_7 w_8 0 n))))))
 ))
 (let (($x3631 (= ?x9666 (bvadd (_ bv1 6) ?x63))))
 (let (($x10301 (= $x9175 (or (exc_halt_s 3) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 3))))))))
 (let (($x7463 (forall ((w (_ BitVec 256)) )(let ((?x9894 (storage_s w_7 w_8 3 w)))
 (let ((?x10139 (storage_s w_7 w_8 4 w)))
 (= ?x10139 ?x9894))))
 ))
 (let (($x6124 (forall ((n (_ BitVec 6)) )(let ((?x3851 (sc_s 3)))
 (let ((?x3651 (bvadd (_ bv62 6) ?x3851)))
 (let (($x3189 (bvsle ?x3651 n)))
 (or $x3189 (= (stack_s w_7 w_8 4 n) (stack_s w_7 w_8 3 n)))))))
 ))
 (let ((?x3851 (sc_s 3)))
 (let (($x2630 (= ?x9433 ?x3851)))
 (let (($x9907 (= (used_gas_s w_7 w_8 4) (+ 3 (used_gas_s w_7 w_8 3)))))
 (let (($x11226 (= (stack_s w_7 w_8 4 (bvadd (_ bv62 6) ?x9433)) (stack_s w_7 w_8 3 (bvadd (_ bv63 6) ?x3851)))))
 (let (($x10181 (= (stack_s w_7 w_8 4 (bvadd (_ bv63 6) ?x9433)) (stack_s w_7 w_8 3 (bvadd (_ bv62 6) ?x3851)))))
 (let (($x8580 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 2)))) (_ bv0 1)))))
 (let (($x10052 (exc_halt_s 2)))
 (let (($x8103 (exc_halt_s 3)))
 (let (($x2547 (= $x8103 (or $x10052 $x8580))))
 (let (($x3149 (forall ((w (_ BitVec 256)) )(let ((?x9716 (storage_s w_7 w_8 2 w)))
 (let ((?x9894 (storage_s w_7 w_8 3 w)))
 (= ?x9894 ?x9716))))
 ))
 (let (($x5167 (forall ((n (_ BitVec 6)) )(let ((?x2272 (sc_s 2)))
 (let (($x8494 (bvsle ?x2272 n)))
 (or (= (stack_s w_7 w_8 3 n) (stack_s w_7 w_8 2 n)) $x8494))))
 ))
 (let (($x11281 (= ?x3851 (bvadd (_ bv1 6) (sc_s 2)))))
 (let ((?x169 (used_gas_s w_7 w_8 3)))
 (let (($x672 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 1)))) (_ bv0 1)))))
 (let (($x8780 (exc_halt_s 1)))
 (let (($x4433 (= $x10052 (or $x8780 $x672))))
 (let (($x4003 (forall ((w (_ BitVec 256)) )(let ((?x11311 (storage_s w_7 w_8 1 w)))
 (let ((?x9716 (storage_s w_7 w_8 2 w)))
 (= ?x9716 ?x11311))))
 ))
 (let (($x11961 (forall ((n (_ BitVec 6)) )(let ((?x154 (sc_s 1)))
 (let (($x9053 (bvsle ?x154 n)))
 (or (= (stack_s w_7 w_8 2 n) (stack_s w_7 w_8 1 n)) $x9053))))
 ))
 (let ((?x2272 (sc_s 2)))
 (let (($x7044 (= ?x2272 (bvadd (_ bv1 6) (sc_s 1)))))
 (let ((?x9850 (used_gas_s w_7 w_8 2)))
 (let (($x10729 (= $x8780 (or $x57 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x72))) (_ bv0 1)))))))
 (let (($x2912 (forall ((w (_ BitVec 256)) )(let ((?x10220 (storage_s w_7 w_8 0 w)))
 (let ((?x11311 (storage_s w_7 w_8 1 w)))
 (= ?x11311 ?x10220))))
 ))
 (let (($x10911 (forall ((n (_ BitVec 6)) )(let ((?x72 (sc_s 0)))
 (let (($x8299 (bvsle ?x72 n)))
 (or $x8299 (= (stack_s w_7 w_8 1 n) (stack_s w_7 w_8 0 n))))))
 ))
 (let ((?x154 (sc_s 1)))
 (let (($x4778 (= ?x154 (bvadd (_ bv1 6) ?x72))))
 (let (($x10998 (forall ((w (_ BitVec 256)) )(let ((?x10220 (storage_s w_7 w_8 0 w)))
 (= ?x10220 (_ bv0 256))))
 ))
 (let (($x10335 (= ?x8968 0)))
 (let (($x2276 (not $x57)))
 (let (($x4825 (= ?x72 (_ bv0 6))))
 (and $x4825 $x2276 $x10335 $x10998 (= (stack_s w_7 w_8 1 ?x72) w_7) (= (used_gas_s w_7 w_8 1) (+ 3 ?x8968)) $x4778 $x10911 $x2912 $x10729 (= (stack_s w_7 w_8 2 ?x154) w_8) (= ?x9850 (+ 3 (used_gas_s w_7 w_8 1))) $x7044 $x11961 $x4003 $x4433 (= (stack_s w_7 w_8 3 ?x2272) w_7) (= ?x169 (+ 3 ?x9850)) $x11281 $x5167 $x3149 $x2547 $x10181 $x11226 $x9907 $x2630 $x6124 $x7463 $x10301 (= (stack_t w_7 w_8 1 ?x63) w_7) (= (used_gas_t w_7 w_8 1) (+ 3 ?x11849)) $x3631 $x830 $x2480 $x489 (= (stack_t w_7 w_8 2 (bvadd (_ bv63 6) ?x11248)) ?x8674) (= (stack_t w_7 w_8 2 ?x10665) ?x8674) (= ?x2097 (+ 3 (used_gas_t w_7 w_8 1))) $x3655 $x2388 $x3197 $x2432 (= (stack_t w_7 w_8 3 ?x11248) w_8) $x5602 $x6668 $x2933 $x4693 $x9131 $x73 $x9021 $x58 $x9980 $x10682 (not (and $x1048 $x1321 $x3479 $x663)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
(check-sat)
(get-proof)