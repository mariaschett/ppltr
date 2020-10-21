; SWAP1 SWAP2 PUSH cw_1 SWAP2 DUP2 SWAP1 => PUSH cw_1 SWAP2 SWAP3 DUP1 SWAP2
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
 (exists ((x_0 (_ BitVec 256)) (x_1 (_ BitVec 256)) (x_2 (_ BitVec 256)) )(let (($x212 (forall ((w (_ BitVec 256)) )(let ((?x11287 (storage_t x_0 x_1 x_2 w_1 5 w)))
 (let ((?x10383 (storage_s x_0 x_1 x_2 w_1 6 w)))
 (= ?x10383 ?x11287))))
 ))
 (let (($x1885 (exc_halt_t 5)))
 (let (($x772 (exc_halt_s 6)))
 (let (($x11108 (= $x772 $x1885)))
 (let (($x3491 (forall ((n (_ BitVec 6)) )(let ((?x8961 (sc_t 5)))
 (let (($x2169 (bvsle ?x8961 n)))
 (let ((?x6688 (stack_t x_0 x_1 x_2 w_1 5 n)))
 (let ((?x10806 (stack_s x_0 x_1 x_2 w_1 6 n)))
 (let (($x1242 (= ?x10806 ?x6688)))
 (or $x1242 $x2169)))))))
 ))
 (let ((?x8961 (sc_t 5)))
 (let ((?x926 (sc_s 6)))
 (let (($x5889 (= ?x926 ?x8961)))
 (let ((?x4497 (used_gas_t x_0 x_1 x_2 w_1 0)))
 (let ((?x3079 (used_gas_s x_0 x_1 x_2 w_1 0)))
 (let (($x11295 (= ?x3079 ?x4497)))
 (let (($x9947 (forall ((w (_ BitVec 256)) )(let ((?x10263 (storage_t x_0 x_1 x_2 w_1 0 w)))
 (let ((?x8464 (storage_s x_0 x_1 x_2 w_1 0 w)))
 (= ?x8464 ?x10263))))
 ))
 (let (($x56 (exc_halt_t 0)))
 (let (($x57 (exc_halt_s 0)))
 (let (($x58 (= $x57 $x56)))
 (let (($x585 (forall ((n (_ BitVec 6)) )(let ((?x63 (sc_t 0)))
 (let (($x11489 (bvsle ?x63 n)))
 (let ((?x10118 (stack_t x_0 x_1 x_2 w_1 0 n)))
 (let ((?x2916 (stack_s x_0 x_1 x_2 w_1 0 n)))
 (let (($x3756 (= ?x2916 ?x10118)))
 (or $x3756 $x11489)))))))
 ))
 (let ((?x63 (sc_t 0)))
 (let ((?x72 (sc_s 0)))
 (let (($x73 (= ?x72 ?x63)))
 (let (($x146 (= $x1885 (or (exc_halt_t 4) (not (bvsle (_ bv0 6) (bvadd (_ bv61 6) (sc_t 4))))))))
 (let (($x10922 (forall ((w (_ BitVec 256)) )(let ((?x1172 (storage_t x_0 x_1 x_2 w_1 4 w)))
 (let ((?x11287 (storage_t x_0 x_1 x_2 w_1 5 w)))
 (= ?x11287 ?x1172))))
 ))
 (let (($x4169 (forall ((n (_ BitVec 6)) )(let ((?x5517 (stack_t x_0 x_1 x_2 w_1 4 n)))
 (let ((?x6688 (stack_t x_0 x_1 x_2 w_1 5 n)))
 (or (= ?x6688 ?x5517) (bvsle (bvadd (_ bv61 6) (sc_t 4)) n)))))
 ))
 (let (($x8727 (= (used_gas_t x_0 x_1 x_2 w_1 5) (+ 3 (used_gas_t x_0 x_1 x_2 w_1 4)))))
 (let (($x2911 (= (stack_t x_0 x_1 x_2 w_1 5 (bvadd (_ bv62 6) ?x8961)) (stack_t x_0 x_1 x_2 w_1 4 (bvadd (_ bv62 6) (sc_t 4))))))
 (let ((?x4818 (sc_t 4)))
 (let ((?x7536 (bvadd (_ bv63 6) ?x4818)))
 (let ((?x7196 (stack_t x_0 x_1 x_2 w_1 4 ?x7536)))
 (let (($x1457 (= (stack_t x_0 x_1 x_2 w_1 5 (bvadd (_ bv63 6) ?x8961)) (stack_t x_0 x_1 x_2 w_1 4 (bvadd (_ bv61 6) ?x4818)))))
 (let (($x9699 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_t 3)))) (_ bv0 1)))))
 (let (($x3614 (exc_halt_t 3)))
 (let (($x7854 (exc_halt_t 4)))
 (let (($x9526 (forall ((w (_ BitVec 256)) )(let ((?x728 (storage_t x_0 x_1 x_2 w_1 3 w)))
 (let ((?x1172 (storage_t x_0 x_1 x_2 w_1 4 w)))
 (= ?x1172 ?x728))))
 ))
 (let (($x1831 (forall ((n (_ BitVec 6)) )(let ((?x7774 (stack_t x_0 x_1 x_2 w_1 3 n)))
 (let ((?x5517 (stack_t x_0 x_1 x_2 w_1 4 n)))
 (or (= ?x5517 ?x7774) (bvsle (bvadd (_ bv63 6) (sc_t 3)) n)))))
 ))
 (let (($x5791 (= ?x4818 (bvadd (_ bv1 6) (sc_t 3)))))
 (let ((?x7364 (used_gas_t x_0 x_1 x_2 w_1 4)))
 (let ((?x11304 (sc_t 3)))
 (let ((?x71 (bvadd (_ bv63 6) ?x11304)))
 (let ((?x1933 (stack_t x_0 x_1 x_2 w_1 3 ?x71)))
 (let (($x8131 (= $x3614 (or (exc_halt_t 2) (not (bvsle (_ bv0 6) (bvadd (_ bv60 6) (sc_t 2))))))))
 (let (($x4734 (forall ((w (_ BitVec 256)) )(let ((?x3094 (storage_t x_0 x_1 x_2 w_1 2 w)))
 (let ((?x728 (storage_t x_0 x_1 x_2 w_1 3 w)))
 (= ?x728 ?x3094))))
 ))
 (let (($x316 (forall ((n (_ BitVec 6)) )(let ((?x7497 (stack_t x_0 x_1 x_2 w_1 2 n)))
 (let ((?x7774 (stack_t x_0 x_1 x_2 w_1 3 n)))
 (or (= ?x7774 ?x7497) (bvsle (bvadd (_ bv60 6) (sc_t 2)) n)))))
 ))
 (let ((?x6158 (sc_t 2)))
 (let (($x8376 (= ?x11304 ?x6158)))
 (let ((?x8055 (used_gas_t x_0 x_1 x_2 w_1 3)))
 (let ((?x3562 (bvadd (_ bv62 6) ?x6158)))
 (let ((?x102 (stack_t x_0 x_1 x_2 w_1 2 ?x3562)))
 (let ((?x1339 (bvadd (_ bv61 6) ?x6158)))
 (let ((?x6347 (stack_t x_0 x_1 x_2 w_1 2 ?x1339)))
 (let ((?x2887 (bvadd (_ bv63 6) ?x6158)))
 (let ((?x8125 (stack_t x_0 x_1 x_2 w_1 2 ?x2887)))
 (let (($x8115 (exc_halt_t 2)))
 (let (($x6726 (= $x8115 (or (exc_halt_t 1) (not (bvsle (_ bv0 6) (bvadd (_ bv61 6) (sc_t 1))))))))
 (let (($x3066 (forall ((w (_ BitVec 256)) )(let ((?x8710 (storage_t x_0 x_1 x_2 w_1 1 w)))
 (let ((?x3094 (storage_t x_0 x_1 x_2 w_1 2 w)))
 (= ?x3094 ?x8710))))
 ))
 (let (($x2632 (forall ((n (_ BitVec 6)) )(let ((?x5977 (stack_t x_0 x_1 x_2 w_1 1 n)))
 (let ((?x7497 (stack_t x_0 x_1 x_2 w_1 2 n)))
 (or (= ?x7497 ?x5977) (bvsle (bvadd (_ bv61 6) (sc_t 1)) n)))))
 ))
 (let ((?x7154 (sc_t 1)))
 (let (($x6239 (= ?x6158 ?x7154)))
 (let ((?x10190 (used_gas_t x_0 x_1 x_2 w_1 2)))
 (let (($x8377 (exc_halt_t 1)))
 (let (($x6488 (= $x8377 (or $x56 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x63))) (_ bv0 1)))))))
 (let (($x8643 (forall ((w (_ BitVec 256)) )(let ((?x10263 (storage_t x_0 x_1 x_2 w_1 0 w)))
 (let ((?x8710 (storage_t x_0 x_1 x_2 w_1 1 w)))
 (= ?x8710 ?x10263))))
 ))
 (let (($x10909 (forall ((n (_ BitVec 6)) )(let ((?x63 (sc_t 0)))
 (let (($x11489 (bvsle ?x63 n)))
 (let ((?x10118 (stack_t x_0 x_1 x_2 w_1 0 n)))
 (let ((?x5977 (stack_t x_0 x_1 x_2 w_1 1 n)))
 (or (= ?x5977 ?x10118) $x11489))))))
 ))
 (let (($x10389 (= ?x7154 (bvadd (_ bv1 6) ?x63))))
 (let (($x914 (= $x772 (or (exc_halt_s 5) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 5))))))))
 (let (($x4933 (forall ((w (_ BitVec 256)) )(let ((?x7169 (storage_s x_0 x_1 x_2 w_1 5 w)))
 (let ((?x10383 (storage_s x_0 x_1 x_2 w_1 6 w)))
 (= ?x10383 ?x7169))))
 ))
 (let (($x2658 (forall ((n (_ BitVec 6)) )(let ((?x8863 (stack_s x_0 x_1 x_2 w_1 5 n)))
 (let ((?x10806 (stack_s x_0 x_1 x_2 w_1 6 n)))
 (let ((?x4319 (sc_s 5)))
 (let ((?x1867 (bvadd (_ bv62 6) ?x4319)))
 (let (($x981 (bvsle ?x1867 n)))
 (or $x981 (= ?x10806 ?x8863))))))))
 ))
 (let ((?x4319 (sc_s 5)))
 (let (($x11219 (= ?x926 ?x4319)))
 (let (($x1723 (= (used_gas_s x_0 x_1 x_2 w_1 6) (+ 3 (used_gas_s x_0 x_1 x_2 w_1 5)))))
 (let ((?x1539 (bvadd (_ bv63 6) ?x4319)))
 (let ((?x8762 (stack_s x_0 x_1 x_2 w_1 5 ?x1539)))
 (let (($x4152 (= (stack_s x_0 x_1 x_2 w_1 6 (bvadd (_ bv63 6) ?x926)) (stack_s x_0 x_1 x_2 w_1 5 (bvadd (_ bv62 6) ?x4319)))))
 (let (($x7172 (exc_halt_s 4)))
 (let (($x2233 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 4)))) (_ bv0 1)))))
 (let (($x1947 (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 4))))))
 (let (($x11317 (exc_halt_s 5)))
 (let (($x2835 (forall ((w (_ BitVec 256)) )(let ((?x5462 (storage_s x_0 x_1 x_2 w_1 4 w)))
 (let ((?x7169 (storage_s x_0 x_1 x_2 w_1 5 w)))
 (= ?x7169 ?x5462))))
 ))
 (let (($x930 (forall ((n (_ BitVec 6)) )(let ((?x9388 (stack_s x_0 x_1 x_2 w_1 4 n)))
 (let ((?x8863 (stack_s x_0 x_1 x_2 w_1 5 n)))
 (let ((?x4305 (sc_s 4)))
 (let ((?x5987 (bvadd (_ bv62 6) ?x4305)))
 (let (($x2047 (bvsle ?x5987 n)))
 (or $x2047 (= ?x8863 ?x9388))))))))
 ))
 (let (($x10051 (= ?x4319 (bvadd (_ bv1 6) (sc_s 4)))))
 (let ((?x2840 (used_gas_s x_0 x_1 x_2 w_1 5)))
 (let ((?x4305 (sc_s 4)))
 (let ((?x2899 (bvadd (_ bv63 6) ?x4305)))
 (let ((?x3090 (stack_s x_0 x_1 x_2 w_1 4 ?x2899)))
 (let ((?x5987 (bvadd (_ bv62 6) ?x4305)))
 (let ((?x4513 (stack_s x_0 x_1 x_2 w_1 4 ?x5987)))
 (let (($x5882 (= $x7172 (or (exc_halt_s 3) (not (bvsle (_ bv0 6) (bvadd (_ bv61 6) (sc_s 3))))))))
 (let (($x5608 (forall ((w (_ BitVec 256)) )(let ((?x9807 (storage_s x_0 x_1 x_2 w_1 3 w)))
 (let ((?x5462 (storage_s x_0 x_1 x_2 w_1 4 w)))
 (= ?x5462 ?x9807))))
 ))
 (let (($x3281 (forall ((n (_ BitVec 6)) )(let ((?x1026 (stack_s x_0 x_1 x_2 w_1 3 n)))
 (let ((?x9388 (stack_s x_0 x_1 x_2 w_1 4 n)))
 (let ((?x275 (sc_s 3)))
 (let ((?x5661 (bvadd (_ bv61 6) ?x275)))
 (let (($x10740 (bvsle ?x5661 n)))
 (or $x10740 (= ?x9388 ?x1026))))))))
 ))
 (let ((?x275 (sc_s 3)))
 (let (($x10711 (= ?x4305 ?x275)))
 (let ((?x124 (used_gas_s x_0 x_1 x_2 w_1 4)))
 (let (($x6947 (= (stack_s x_0 x_1 x_2 w_1 4 (bvadd (_ bv61 6) ?x4305)) (stack_s x_0 x_1 x_2 w_1 3 (bvadd (_ bv63 6) ?x275)))))
 (let (($x6754 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 2)))) (_ bv0 1)))))
 (let (($x247 (exc_halt_s 2)))
 (let (($x292 (exc_halt_s 3)))
 (let (($x1336 (= $x292 (or $x247 $x6754))))
 (let (($x11535 (forall ((w (_ BitVec 256)) )(let ((?x7668 (storage_s x_0 x_1 x_2 w_1 2 w)))
 (let ((?x9807 (storage_s x_0 x_1 x_2 w_1 3 w)))
 (= ?x9807 ?x7668))))
 ))
 (let (($x445 (forall ((n (_ BitVec 6)) )(let ((?x11357 (stack_s x_0 x_1 x_2 w_1 2 n)))
 (let ((?x1026 (stack_s x_0 x_1 x_2 w_1 3 n)))
 (let ((?x218 (sc_s 2)))
 (let (($x863 (bvsle ?x218 n)))
 (or $x863 (= ?x1026 ?x11357)))))))
 ))
 (let (($x9686 (= ?x275 (bvadd (_ bv1 6) (sc_s 2)))))
 (let ((?x7802 (used_gas_s x_0 x_1 x_2 w_1 3)))
 (let (($x8810 (= $x247 (or (exc_halt_s 1) (not (bvsle (_ bv0 6) (bvadd (_ bv61 6) (sc_s 1))))))))
 (let (($x8685 (forall ((w (_ BitVec 256)) )(let ((?x2960 (storage_s x_0 x_1 x_2 w_1 1 w)))
 (let ((?x7668 (storage_s x_0 x_1 x_2 w_1 2 w)))
 (= ?x7668 ?x2960))))
 ))
 (let (($x6237 (forall ((n (_ BitVec 6)) )(let ((?x1046 (stack_s x_0 x_1 x_2 w_1 1 n)))
 (let ((?x11357 (stack_s x_0 x_1 x_2 w_1 2 n)))
 (or (bvsle (bvadd (_ bv61 6) (sc_s 1)) n) (= ?x11357 ?x1046)))))
 ))
 (let ((?x154 (sc_s 1)))
 (let ((?x218 (sc_s 2)))
 (let (($x4858 (= ?x218 ?x154)))
 (let ((?x11046 (used_gas_s x_0 x_1 x_2 w_1 2)))
 (let ((?x9584 (bvadd (_ bv62 6) ?x154)))
 (let ((?x741 (stack_s x_0 x_1 x_2 w_1 1 ?x9584)))
 (let ((?x1096 (bvadd (_ bv63 6) ?x154)))
 (let ((?x9892 (stack_s x_0 x_1 x_2 w_1 1 ?x1096)))
 (let (($x3030 (= (stack_s x_0 x_1 x_2 w_1 2 (bvadd (_ bv63 6) ?x218)) (stack_s x_0 x_1 x_2 w_1 1 (bvadd (_ bv61 6) ?x154)))))
 (let (($x11048 (forall ((w (_ BitVec 256)) )(let ((?x8464 (storage_s x_0 x_1 x_2 w_1 0 w)))
 (let ((?x2960 (storage_s x_0 x_1 x_2 w_1 1 w)))
 (= ?x2960 ?x8464))))
 ))
 (let (($x8424 (forall ((n (_ BitVec 6)) )(let ((?x2916 (stack_s x_0 x_1 x_2 w_1 0 n)))
 (let ((?x1046 (stack_s x_0 x_1 x_2 w_1 1 n)))
 (or (= ?x1046 ?x2916) (bvsle (bvadd (_ bv62 6) (sc_s 0)) n)))))
 ))
 (let (($x5966 (= ?x154 ?x72)))
 (let (($x7890 (forall ((w (_ BitVec 256)) )(let ((?x8464 (storage_s x_0 x_1 x_2 w_1 0 w)))
 (= ?x8464 (_ bv0 256))))
 ))
 (let (($x2889 (= ?x3079 0)))
 (let (($x9420 (not $x57)))
 (let (($x8891 (= (stack_s x_0 x_1 x_2 w_1 0 (_ bv2 6)) x_2)))
 (let (($x1483 (= (stack_s x_0 x_1 x_2 w_1 0 (_ bv1 6)) x_1)))
 (let (($x10819 (= (stack_s x_0 x_1 x_2 w_1 0 (_ bv0 6)) x_0)))
 (let (($x120 (= ?x72 (_ bv3 6))))
 (and $x120 $x10819 $x1483 $x8891 $x9420 $x2889 $x7890 (= ?x9892 (stack_s x_0 x_1 x_2 w_1 0 (bvadd (_ bv62 6) ?x72))) (= ?x741 (stack_s x_0 x_1 x_2 w_1 0 (bvadd (_ bv63 6) ?x72))) (= (used_gas_s x_0 x_1 x_2 w_1 1) (+ 3 ?x3079)) $x5966 $x8424 $x11048 (= (exc_halt_s 1) (or $x57 (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) ?x72))))) $x3030 (= (stack_s x_0 x_1 x_2 w_1 2 (bvadd (_ bv61 6) ?x218)) ?x9892) (= (stack_s x_0 x_1 x_2 w_1 2 (bvadd (_ bv62 6) ?x218)) ?x741) (= ?x11046 (+ 3 (used_gas_s x_0 x_1 x_2 w_1 1))) $x4858 $x6237 $x8685 $x8810 (= (stack_s x_0 x_1 x_2 w_1 3 ?x218) w_1) (= ?x7802 (+ 3 ?x11046)) $x9686 $x445 $x11535 $x1336 (= ?x3090 (stack_s x_0 x_1 x_2 w_1 3 (bvadd (_ bv61 6) ?x275))) $x6947 (= ?x4513 (stack_s x_0 x_1 x_2 w_1 3 (bvadd (_ bv62 6) ?x275))) (= ?x124 (+ 3 ?x7802)) $x10711 $x3281 $x5608 $x5882 (= ?x8762 ?x4513) (= (stack_s x_0 x_1 x_2 w_1 5 ?x5987) ?x4513) (= (stack_s x_0 x_1 x_2 w_1 5 ?x2899) ?x3090) (= ?x2840 (+ 3 ?x124)) $x10051 $x930 $x2835 (= $x11317 (or $x1947 $x2233 $x7172)) $x4152 (= (stack_s x_0 x_1 x_2 w_1 6 (bvadd (_ bv62 6) ?x926)) ?x8762) $x1723 $x11219 $x2658 $x4933 $x914 (= (stack_t x_0 x_1 x_2 w_1 1 ?x63) w_1) (= (used_gas_t x_0 x_1 x_2 w_1 1) (+ 3 ?x4497)) $x10389 $x10909 $x8643 $x6488 (= ?x8125 (stack_t x_0 x_1 x_2 w_1 1 (bvadd (_ bv61 6) ?x7154))) (= ?x6347 (stack_t x_0 x_1 x_2 w_1 1 (bvadd (_ bv63 6) ?x7154))) (= ?x102 (stack_t x_0 x_1 x_2 w_1 1 (bvadd (_ bv62 6) ?x7154))) (= ?x10190 (+ 3 (used_gas_t x_0 x_1 x_2 w_1 1))) $x6239 $x2632 $x3066 $x6726 (= ?x1933 (stack_t x_0 x_1 x_2 w_1 2 (bvadd (_ bv60 6) ?x6158))) (= (stack_t x_0 x_1 x_2 w_1 3 (bvadd (_ bv60 6) ?x11304)) ?x8125) (= (stack_t x_0 x_1 x_2 w_1 3 (bvadd (_ bv61 6) ?x11304)) ?x6347) (= (stack_t x_0 x_1 x_2 w_1 3 (bvadd (_ bv62 6) ?x11304)) ?x102) (= ?x8055 (+ 3 ?x10190)) $x8376 $x316 $x4734 $x8131 (= ?x7196 ?x1933) (= (stack_t x_0 x_1 x_2 w_1 4 ?x71) ?x1933) (= ?x7364 (+ 3 ?x8055)) $x5791 $x1831 $x9526 (= $x7854 (or $x3614 (not (bvsle (_ bv0 6) ?x71)) $x9699)) $x1457 (= (stack_t x_0 x_1 x_2 w_1 5 (bvadd (_ bv61 6) ?x8961)) ?x7196) $x2911 $x8727 (= ?x8961 ?x4818) $x4169 $x10922 $x146 $x73 $x585 $x58 $x9947 $x11295 (not (and $x5889 $x3491 $x11108 $x212)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
 )
(check-sat)
(get-proof)
