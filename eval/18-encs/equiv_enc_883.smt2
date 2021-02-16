; PUSH cw_2 CALLER SWAP1 POP => CALLER
(set-option :produce-proofs true)
; 
(set-info :status unknown)
(declare-fun storage_t ((_ BitVec 256) (_ BitVec 256) Int (_ BitVec 256)) (_ BitVec 256))
(declare-fun w_2 () (_ BitVec 256))
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
 (exists ((x_CALLER (_ BitVec 256)) )(let (($x4390 (forall ((w (_ BitVec 256)) )(let ((?x10525 (storage_t w_2 x_CALLER 1 w)))
 (let ((?x3317 (storage_s w_2 x_CALLER 4 w)))
 (= ?x3317 ?x10525))))
 ))
 (let (($x11030 (exc_halt_t 1)))
 (let (($x9175 (exc_halt_s 4)))
 (let (($x6262 (= $x9175 $x11030)))
 (let (($x5858 (forall ((n (_ BitVec 6)) )(let ((?x7287 (stack_t w_2 x_CALLER 1 n)))
 (let ((?x1252 (stack_s w_2 x_CALLER 4 n)))
 (let (($x3984 (= ?x1252 ?x7287)))
 (let ((?x4135 (sc_t 1)))
 (let (($x3402 (bvsle ?x4135 n)))
 (or $x3402 $x3984)))))))
 ))
 (let ((?x4135 (sc_t 1)))
 (let ((?x9433 (sc_s 4)))
 (let (($x9748 (= ?x9433 ?x4135)))
 (let ((?x9057 (used_gas_t w_2 x_CALLER 0)))
 (let ((?x5778 (used_gas_s w_2 x_CALLER 0)))
 (let (($x10380 (= ?x5778 ?x9057)))
 (let (($x4695 (forall ((w (_ BitVec 256)) )(let ((?x6446 (storage_t w_2 x_CALLER 0 w)))
 (let ((?x2515 (storage_s w_2 x_CALLER 0 w)))
 (= ?x2515 ?x6446))))
 ))
 (let (($x56 (exc_halt_t 0)))
 (let (($x57 (exc_halt_s 0)))
 (let (($x58 (= $x57 $x56)))
 (let (($x2795 (forall ((n (_ BitVec 6)) )(let ((?x7233 (stack_t w_2 x_CALLER 0 n)))
 (let ((?x8312 (stack_s w_2 x_CALLER 0 n)))
 (let (($x4721 (= ?x8312 ?x7233)))
 (let ((?x63 (sc_t 0)))
 (let (($x3918 (bvsle ?x63 n)))
 (or $x3918 $x4721)))))))
 ))
 (let ((?x63 (sc_t 0)))
 (let ((?x72 (sc_s 0)))
 (let (($x73 (= ?x72 ?x63)))
 (let (($x8407 (= $x11030 (or $x56 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x63))) (_ bv0 1)))))))
 (let (($x2807 (forall ((w (_ BitVec 256)) )(let ((?x6446 (storage_t w_2 x_CALLER 0 w)))
 (let ((?x10525 (storage_t w_2 x_CALLER 1 w)))
 (= ?x10525 ?x6446))))
 ))
 (let (($x3298 (forall ((n (_ BitVec 6)) )(let ((?x63 (sc_t 0)))
 (let (($x3918 (bvsle ?x63 n)))
 (or $x3918 (= (stack_t w_2 x_CALLER 1 n) (stack_t w_2 x_CALLER 0 n))))))
 ))
 (let (($x4431 (= ?x4135 (bvadd (_ bv1 6) ?x63))))
 (let (($x6008 (= $x9175 (or (exc_halt_s 3) (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_s 3))))))))
 (let (($x166 (forall ((w (_ BitVec 256)) )(let ((?x7050 (storage_s w_2 x_CALLER 3 w)))
 (let ((?x3317 (storage_s w_2 x_CALLER 4 w)))
 (= ?x3317 ?x7050))))
 ))
 (let (($x3137 (forall ((n (_ BitVec 6)) )(or (bvsle (bvadd (_ bv63 6) (sc_s 3)) n) (= (stack_s w_2 x_CALLER 4 n) (stack_s w_2 x_CALLER 3 n))))
 ))
 (let (($x9919 (= (used_gas_s w_2 x_CALLER 4) (+ 2 (used_gas_s w_2 x_CALLER 3)))))
 (let (($x8103 (exc_halt_s 3)))
 (let (($x9650 (= $x8103 (or (exc_halt_s 2) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 2))))))))
 (let (($x5866 (forall ((w (_ BitVec 256)) )(let ((?x4820 (storage_s w_2 x_CALLER 2 w)))
 (let ((?x7050 (storage_s w_2 x_CALLER 3 w)))
 (= ?x7050 ?x4820))))
 ))
 (let (($x110 (forall ((n (_ BitVec 6)) )(or (bvsle (bvadd (_ bv62 6) (sc_s 2)) n) (= (stack_s w_2 x_CALLER 3 n) (stack_s w_2 x_CALLER 2 n))))
 ))
 (let ((?x2272 (sc_s 2)))
 (let ((?x3851 (sc_s 3)))
 (let (($x599 (= ?x3851 ?x2272)))
 (let ((?x4247 (used_gas_s w_2 x_CALLER 3)))
 (let (($x191 (= (stack_s w_2 x_CALLER 3 (bvadd (_ bv62 6) ?x3851)) (stack_s w_2 x_CALLER 2 (bvadd (_ bv63 6) ?x2272)))))
 (let (($x2627 (= (stack_s w_2 x_CALLER 3 (bvadd (_ bv63 6) ?x3851)) (stack_s w_2 x_CALLER 2 (bvadd (_ bv62 6) ?x2272)))))
 (let (($x8780 (exc_halt_s 1)))
 (let (($x2603 (or $x8780 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 1)))) (_ bv0 1))))))
 (let (($x10052 (exc_halt_s 2)))
 (let (($x11676 (forall ((w (_ BitVec 256)) )(let ((?x6564 (storage_s w_2 x_CALLER 1 w)))
 (let ((?x4820 (storage_s w_2 x_CALLER 2 w)))
 (= ?x4820 ?x6564))))
 ))
 (let (($x3961 (forall ((n (_ BitVec 6)) )(or (= (stack_s w_2 x_CALLER 2 n) (stack_s w_2 x_CALLER 1 n)) (bvsle (sc_s 1) n)))
 ))
 (let ((?x9418 (used_gas_s w_2 x_CALLER 2)))
 (let (($x11279 (= $x8780 (or $x57 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x72))) (_ bv0 1)))))))
 (let (($x11387 (forall ((w (_ BitVec 256)) )(let ((?x2515 (storage_s w_2 x_CALLER 0 w)))
 (let ((?x6564 (storage_s w_2 x_CALLER 1 w)))
 (= ?x6564 ?x2515))))
 ))
 (let (($x3114 (forall ((n (_ BitVec 6)) )(let ((?x72 (sc_s 0)))
 (let (($x5715 (bvsle ?x72 n)))
 (or $x5715 (= (stack_s w_2 x_CALLER 1 n) (stack_s w_2 x_CALLER 0 n))))))
 ))
 (let ((?x154 (sc_s 1)))
 (let (($x11116 (= ?x154 (bvadd (_ bv1 6) ?x72))))
 (let (($x8759 (forall ((w (_ BitVec 256)) )(let ((?x2515 (storage_s w_2 x_CALLER 0 w)))
 (= ?x2515 (_ bv0 256))))
 ))
 (let (($x8423 (= ?x5778 0)))
 (let (($x2113 (not $x57)))
 (let (($x4825 (= ?x72 (_ bv0 6))))
 (and $x4825 $x2113 $x8423 $x8759 (= (stack_s w_2 x_CALLER 1 ?x72) w_2) (= (used_gas_s w_2 x_CALLER 1) (+ 3 ?x5778)) $x11116 $x3114 $x11387 $x11279 (= (stack_s w_2 x_CALLER 2 ?x154) x_CALLER) (= ?x9418 (+ 2 (used_gas_s w_2 x_CALLER 1))) (= ?x2272 (bvadd (_ bv1 6) ?x154)) $x3961 $x11676 (= $x10052 $x2603) $x2627 $x191 (= ?x4247 (+ 3 ?x9418)) $x599 $x110 $x5866 $x9650 $x9919 (= ?x9433 (bvadd (_ bv63 6) ?x3851)) $x3137 $x166 $x6008 (= (stack_t w_2 x_CALLER 1 ?x63) x_CALLER) (= (used_gas_t w_2 x_CALLER 1) (+ 2 ?x9057)) $x4431 $x3298 $x2807 $x8407 $x73 $x2795 $x58 $x4695 $x10380 (not (and $x9748 $x5858 $x6262 $x4390))))))))))))))))))))))))))))))))))))))))))))))))))))))))
 )
(check-sat)
(get-proof)