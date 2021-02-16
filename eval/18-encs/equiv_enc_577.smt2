; PUSH cw_1 SWAP2 SWAP1 LT ISZERO => LT ISZERO PUSH cw_1 SWAP1
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
 (exists ((x_0 (_ BitVec 256)) (x_1 (_ BitVec 256)) )(let (($x10769 (forall ((w (_ BitVec 256)) )(let ((?x2874 (storage_t x_0 x_1 w_1 4 w)))
 (let ((?x3098 (storage_s x_0 x_1 w_1 5 w)))
 (= ?x3098 ?x2874))))
 ))
 (let (($x7854 (exc_halt_t 4)))
 (let (($x11317 (exc_halt_s 5)))
 (let (($x4320 (= $x11317 $x7854)))
 (let (($x11397 (forall ((n (_ BitVec 6)) )(let ((?x4818 (sc_t 4)))
 (let (($x6423 (bvsle ?x4818 n)))
 (let ((?x23 (stack_t x_0 x_1 w_1 4 n)))
 (let ((?x9963 (stack_s x_0 x_1 w_1 5 n)))
 (let (($x3355 (= ?x9963 ?x23)))
 (or $x3355 $x6423)))))))
 ))
 (let ((?x4818 (sc_t 4)))
 (let ((?x4319 (sc_s 5)))
 (let (($x3242 (= ?x4319 ?x4818)))
 (let ((?x5994 (used_gas_t x_0 x_1 w_1 0)))
 (let ((?x2241 (used_gas_s x_0 x_1 w_1 0)))
 (let (($x11845 (= ?x2241 ?x5994)))
 (let (($x2560 (forall ((w (_ BitVec 256)) )(let ((?x2266 (storage_t x_0 x_1 w_1 0 w)))
 (let ((?x9120 (storage_s x_0 x_1 w_1 0 w)))
 (= ?x9120 ?x2266))))
 ))
 (let (($x56 (exc_halt_t 0)))
 (let (($x57 (exc_halt_s 0)))
 (let (($x58 (= $x57 $x56)))
 (let (($x1867 (forall ((n (_ BitVec 6)) )(let ((?x63 (sc_t 0)))
 (let (($x4230 (bvsle ?x63 n)))
 (let ((?x2512 (stack_t x_0 x_1 w_1 0 n)))
 (let ((?x8077 (stack_s x_0 x_1 w_1 0 n)))
 (let (($x1857 (= ?x8077 ?x2512)))
 (or $x1857 $x4230)))))))
 ))
 (let ((?x63 (sc_t 0)))
 (let ((?x72 (sc_s 0)))
 (let (($x73 (= ?x72 ?x63)))
 (let (($x10988 (= $x7854 (or (exc_halt_t 3) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_t 3))))))))
 (let (($x4224 (forall ((w (_ BitVec 256)) )(let ((?x8514 (storage_t x_0 x_1 w_1 3 w)))
 (let ((?x2874 (storage_t x_0 x_1 w_1 4 w)))
 (= ?x2874 ?x8514))))
 ))
 (let (($x10151 (forall ((n (_ BitVec 6)) )(let ((?x10926 (sc_t 3)))
 (let ((?x9407 (bvadd (_ bv62 6) ?x10926)))
 (let (($x7500 (bvsle ?x9407 n)))
 (let ((?x11136 (stack_t x_0 x_1 w_1 3 n)))
 (let ((?x23 (stack_t x_0 x_1 w_1 4 n)))
 (or (= ?x23 ?x11136) $x7500)))))))
 ))
 (let ((?x10926 (sc_t 3)))
 (let (($x487 (= ?x4818 ?x10926)))
 (let (($x1203 (= (used_gas_t x_0 x_1 w_1 4) (+ 3 (used_gas_t x_0 x_1 w_1 3)))))
 (let (($x9806 (= (stack_t x_0 x_1 w_1 4 (bvadd (_ bv62 6) ?x4818)) (stack_t x_0 x_1 w_1 3 (bvadd (_ bv63 6) ?x10926)))))
 (let (($x9163 (= (stack_t x_0 x_1 w_1 4 (bvadd (_ bv63 6) ?x4818)) (stack_t x_0 x_1 w_1 3 (bvadd (_ bv62 6) ?x10926)))))
 (let (($x2163 (exc_halt_t 2)))
 (let (($x5873 (or $x2163 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_t 2)))) (_ bv0 1))))))
 (let (($x2670 (exc_halt_t 3)))
 (let (($x1149 (= $x2670 $x5873)))
 (let (($x4066 (forall ((w (_ BitVec 256)) )(let ((?x9710 (storage_t x_0 x_1 w_1 2 w)))
 (let ((?x8514 (storage_t x_0 x_1 w_1 3 w)))
 (= ?x8514 ?x9710))))
 ))
 (let (($x6704 (forall ((n (_ BitVec 6)) )(let ((?x2714 (sc_t 2)))
 (let (($x9222 (bvsle ?x2714 n)))
 (let ((?x4585 (stack_t x_0 x_1 w_1 2 n)))
 (let ((?x11136 (stack_t x_0 x_1 w_1 3 n)))
 (or (= ?x11136 ?x4585) $x9222))))))
 ))
 (let (($x8329 (= ?x10926 (bvadd (_ bv1 6) (sc_t 2)))))
 (let ((?x3790 (used_gas_t x_0 x_1 w_1 3)))
 (let (($x10126 (= $x2163 (or (exc_halt_t 1) (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_t 1))))))))
 (let (($x4836 (forall ((w (_ BitVec 256)) )(let ((?x5821 (storage_t x_0 x_1 w_1 1 w)))
 (let ((?x9710 (storage_t x_0 x_1 w_1 2 w)))
 (= ?x9710 ?x5821))))
 ))
 (let (($x6900 (forall ((n (_ BitVec 6)) )(let ((?x7154 (sc_t 1)))
 (let ((?x10834 (bvadd (_ bv63 6) ?x7154)))
 (let (($x10593 (bvsle ?x10834 n)))
 (let ((?x3957 (stack_t x_0 x_1 w_1 1 n)))
 (let ((?x4585 (stack_t x_0 x_1 w_1 2 n)))
 (or (= ?x4585 ?x3957) $x10593)))))))
 ))
 (let ((?x7154 (sc_t 1)))
 (let ((?x2714 (sc_t 2)))
 (let (($x3683 (= ?x2714 ?x7154)))
 (let ((?x4454 (used_gas_t x_0 x_1 w_1 2)))
 (let ((?x9441 (ite (= (stack_t x_0 x_1 w_1 1 (bvadd (_ bv63 6) ?x7154)) (_ bv0 256)) (_ bv1 256) (_ bv0 256))))
 (let (($x8377 (exc_halt_t 1)))
 (let (($x136 (= $x8377 (or $x56 (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) ?x63)))))))
 (let (($x11003 (forall ((w (_ BitVec 256)) )(let ((?x2266 (storage_t x_0 x_1 w_1 0 w)))
 (let ((?x5821 (storage_t x_0 x_1 w_1 1 w)))
 (= ?x5821 ?x2266))))
 ))
 (let (($x11460 (forall ((n (_ BitVec 6)) )(let ((?x2512 (stack_t x_0 x_1 w_1 0 n)))
 (let ((?x3957 (stack_t x_0 x_1 w_1 1 n)))
 (let ((?x63 (sc_t 0)))
 (let ((?x10467 (bvadd (_ bv62 6) ?x63)))
 (let (($x4206 (bvsle ?x10467 n)))
 (or $x4206 (= ?x3957 ?x2512))))))))
 ))
 (let ((?x1564 (bvadd (_ bv63 6) ?x63)))
 (let (($x4891 (= ?x7154 ?x1564)))
 (let (($x11261 (bvule (stack_t x_0 x_1 w_1 0 (bvadd (_ bv62 6) ?x63)) (stack_t x_0 x_1 w_1 0 ?x1564))))
 (let ((?x10834 (bvadd (_ bv63 6) ?x7154)))
 (let ((?x5903 (stack_t x_0 x_1 w_1 1 ?x10834)))
 (let (($x940 (= $x11317 (or (exc_halt_s 4) (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_s 4))))))))
 (let (($x8223 (forall ((w (_ BitVec 256)) )(let ((?x11622 (storage_s x_0 x_1 w_1 4 w)))
 (let ((?x3098 (storage_s x_0 x_1 w_1 5 w)))
 (= ?x3098 ?x11622))))
 ))
 (let (($x8743 (forall ((n (_ BitVec 6)) )(let ((?x4305 (sc_s 4)))
 (let ((?x4739 (bvadd (_ bv63 6) ?x4305)))
 (let (($x5213 (bvsle ?x4739 n)))
 (let ((?x7087 (stack_s x_0 x_1 w_1 4 n)))
 (let ((?x9963 (stack_s x_0 x_1 w_1 5 n)))
 (or (= ?x9963 ?x7087) $x5213)))))))
 ))
 (let ((?x4305 (sc_s 4)))
 (let (($x780 (= ?x4319 ?x4305)))
 (let (($x8546 (= (used_gas_s x_0 x_1 w_1 5) (+ 3 (used_gas_s x_0 x_1 w_1 4)))))
 (let ((?x3303 (ite (= (stack_s x_0 x_1 w_1 4 (bvadd (_ bv63 6) ?x4305)) (_ bv0 256)) (_ bv1 256) (_ bv0 256))))
 (let (($x7172 (exc_halt_s 4)))
 (let (($x6875 (= $x7172 (or (exc_halt_s 3) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 3))))))))
 (let (($x10465 (forall ((w (_ BitVec 256)) )(let ((?x2293 (storage_s x_0 x_1 w_1 3 w)))
 (let ((?x11622 (storage_s x_0 x_1 w_1 4 w)))
 (= ?x11622 ?x2293))))
 ))
 (let (($x6528 (forall ((n (_ BitVec 6)) )(let ((?x6719 (stack_s x_0 x_1 w_1 3 n)))
 (let ((?x7087 (stack_s x_0 x_1 w_1 4 n)))
 (let ((?x275 (sc_s 3)))
 (let ((?x10705 (bvadd (_ bv62 6) ?x275)))
 (let (($x4453 (bvsle ?x10705 n)))
 (or $x4453 (= ?x7087 ?x6719))))))))
 ))
 (let ((?x275 (sc_s 3)))
 (let ((?x10876 (bvadd (_ bv63 6) ?x275)))
 (let (($x8882 (= ?x4305 ?x10876)))
 (let ((?x5009 (used_gas_s x_0 x_1 w_1 4)))
 (let ((?x3815 (stack_s x_0 x_1 w_1 3 ?x10876)))
 (let ((?x10705 (bvadd (_ bv62 6) ?x275)))
 (let ((?x747 (stack_s x_0 x_1 w_1 3 ?x10705)))
 (let ((?x4739 (bvadd (_ bv63 6) ?x4305)))
 (let ((?x687 (stack_s x_0 x_1 w_1 4 ?x4739)))
 (let (($x292 (exc_halt_s 3)))
 (let (($x8310 (= $x292 (or (exc_halt_s 2) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 2))))))))
 (let (($x4108 (forall ((w (_ BitVec 256)) )(let ((?x5518 (storage_s x_0 x_1 w_1 2 w)))
 (let ((?x2293 (storage_s x_0 x_1 w_1 3 w)))
 (= ?x2293 ?x5518))))
 ))
 (let (($x531 (forall ((n (_ BitVec 6)) )(let ((?x851 (stack_s x_0 x_1 w_1 2 n)))
 (let ((?x6719 (stack_s x_0 x_1 w_1 3 n)))
 (let ((?x218 (sc_s 2)))
 (let ((?x11299 (bvadd (_ bv62 6) ?x218)))
 (let (($x9433 (bvsle ?x11299 n)))
 (or $x9433 (= ?x6719 ?x851))))))))
 ))
 (let ((?x218 (sc_s 2)))
 (let (($x7483 (= ?x275 ?x218)))
 (let ((?x3169 (used_gas_s x_0 x_1 w_1 3)))
 (let (($x247 (exc_halt_s 2)))
 (let (($x4868 (= $x247 (or (exc_halt_s 1) (not (bvsle (_ bv0 6) (bvadd (_ bv61 6) (sc_s 1))))))))
 (let (($x594 (forall ((w (_ BitVec 256)) )(let ((?x7362 (storage_s x_0 x_1 w_1 1 w)))
 (let ((?x5518 (storage_s x_0 x_1 w_1 2 w)))
 (= ?x5518 ?x7362))))
 ))
 (let (($x1769 (forall ((n (_ BitVec 6)) )(let ((?x154 (sc_s 1)))
 (let ((?x2426 (bvadd (_ bv61 6) ?x154)))
 (let (($x8608 (bvsle ?x2426 n)))
 (let ((?x2930 (stack_s x_0 x_1 w_1 1 n)))
 (let ((?x851 (stack_s x_0 x_1 w_1 2 n)))
 (or (= ?x851 ?x2930) $x8608)))))))
 ))
 (let ((?x154 (sc_s 1)))
 (let (($x8545 (= ?x218 ?x154)))
 (let ((?x98 (used_gas_s x_0 x_1 w_1 2)))
 (let ((?x11299 (bvadd (_ bv62 6) ?x218)))
 (let ((?x1317 (stack_s x_0 x_1 w_1 2 ?x11299)))
 (let (($x5545 (= (stack_s x_0 x_1 w_1 2 (bvadd (_ bv61 6) ?x218)) (stack_s x_0 x_1 w_1 1 (bvadd (_ bv63 6) ?x154)))))
 (let ((?x4853 (bvadd (_ bv63 6) ?x218)))
 (let ((?x3812 (stack_s x_0 x_1 w_1 2 ?x4853)))
 (let (($x189 (exc_halt_s 1)))
 (let (($x1815 (= $x189 (or $x57 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x72))) (_ bv0 1)))))))
 (let (($x3159 (forall ((w (_ BitVec 256)) )(let ((?x9120 (storage_s x_0 x_1 w_1 0 w)))
 (let ((?x7362 (storage_s x_0 x_1 w_1 1 w)))
 (= ?x7362 ?x9120))))
 ))
 (let (($x10734 (forall ((n (_ BitVec 6)) )(let ((?x8077 (stack_s x_0 x_1 w_1 0 n)))
 (let ((?x2930 (stack_s x_0 x_1 w_1 1 n)))
 (let ((?x72 (sc_s 0)))
 (let (($x4037 (bvsle ?x72 n)))
 (or $x4037 (= ?x2930 ?x8077)))))))
 ))
 (let (($x1117 (= ?x154 (bvadd (_ bv1 6) ?x72))))
 (let (($x9952 (forall ((w (_ BitVec 256)) )(let ((?x9120 (storage_s x_0 x_1 w_1 0 w)))
 (= ?x9120 (_ bv0 256))))
 ))
 (let (($x11567 (= ?x2241 0)))
 (let (($x5536 (not $x57)))
 (let (($x7841 (= (stack_s x_0 x_1 w_1 0 (_ bv1 6)) x_1)))
 (let (($x9804 (= (stack_s x_0 x_1 w_1 0 (_ bv0 6)) x_0)))
 (let (($x11128 (= ?x72 (_ bv2 6))))
 (and $x11128 $x9804 $x7841 $x5536 $x11567 $x9952 (= (stack_s x_0 x_1 w_1 1 ?x72) w_1) (= (used_gas_s x_0 x_1 w_1 1) (+ 3 ?x2241)) $x1117 $x10734 $x3159 $x1815 (= ?x3812 (stack_s x_0 x_1 w_1 1 (bvadd (_ bv61 6) ?x154))) $x5545 (= ?x1317 (stack_s x_0 x_1 w_1 1 (bvadd (_ bv62 6) ?x154))) (= ?x98 (+ 3 (used_gas_s x_0 x_1 w_1 1))) $x8545 $x1769 $x594 $x4868 (= ?x3815 ?x1317) (= ?x747 ?x3812) (= ?x3169 (+ 3 ?x98)) $x7483 $x531 $x4108 $x8310 (= ?x687 (ite (bvule ?x747 ?x3815) (_ bv0 256) (_ bv1 256))) (= ?x5009 (+ 3 ?x3169)) $x8882 $x6528 $x10465 $x6875 (= (stack_s x_0 x_1 w_1 5 (bvadd (_ bv63 6) ?x4319)) ?x3303) $x8546 $x780 $x8743 $x8223 $x940 (= ?x5903 (ite $x11261 (_ bv0 256) (_ bv1 256))) (= (used_gas_t x_0 x_1 w_1 1) (+ 3 ?x5994)) $x4891 $x11460 $x11003 $x136 (= (stack_t x_0 x_1 w_1 2 (bvadd (_ bv63 6) ?x2714)) ?x9441) (= ?x4454 (+ 3 (used_gas_t x_0 x_1 w_1 1))) $x3683 $x6900 $x4836 $x10126 (= (stack_t x_0 x_1 w_1 3 ?x2714) w_1) (= ?x3790 (+ 3 ?x4454)) $x8329 $x6704 $x4066 $x1149 $x9163 $x9806 $x1203 $x487 $x10151 $x4224 $x10988 $x73 $x1867 $x58 $x2560 $x11845 (not (and $x3242 $x11397 $x4320 $x10769))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
 )
(check-sat)
(get-proof)