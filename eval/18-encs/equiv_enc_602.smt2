; PUSH cw_2 SWAP2 DUP3 SWAP2 DUP3 SWAP2 => PUSH cw_2 PUSH cw_2 SWAP2 DUP3 SWAP4
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
 (exists ((x_0 (_ BitVec 256)) (x_1 (_ BitVec 256)) )(let (($x2964 (forall ((w (_ BitVec 256)) )(let ((?x6102 (storage_t x_0 x_1 w_2 5 w)))
 (let ((?x6167 (storage_s x_0 x_1 w_2 6 w)))
 (= ?x6167 ?x6102))))
 ))
 (let (($x1885 (exc_halt_t 5)))
 (let (($x772 (exc_halt_s 6)))
 (let (($x11108 (= $x772 $x1885)))
 (let (($x5100 (forall ((n (_ BitVec 6)) )(let ((?x815 (stack_t x_0 x_1 w_2 5 n)))
 (let ((?x9448 (stack_s x_0 x_1 w_2 6 n)))
 (let (($x5176 (= ?x9448 ?x815)))
 (or $x5176 (bvsle (sc_t 5) n))))))
 ))
 (let ((?x8961 (sc_t 5)))
 (let ((?x926 (sc_s 6)))
 (let (($x5889 (= ?x926 ?x8961)))
 (let ((?x10131 (used_gas_t x_0 x_1 w_2 0)))
 (let ((?x8617 (used_gas_s x_0 x_1 w_2 0)))
 (let (($x6882 (= ?x8617 ?x10131)))
 (let (($x7860 (forall ((w (_ BitVec 256)) )(let ((?x11112 (storage_t x_0 x_1 w_2 0 w)))
 (let ((?x4242 (storage_s x_0 x_1 w_2 0 w)))
 (= ?x4242 ?x11112))))
 ))
 (let (($x56 (exc_halt_t 0)))
 (let (($x57 (exc_halt_s 0)))
 (let (($x58 (= $x57 $x56)))
 (let (($x1597 (forall ((n (_ BitVec 6)) )(let ((?x63 (sc_t 0)))
 (let (($x11399 (bvsle ?x63 n)))
 (let ((?x10322 (stack_t x_0 x_1 w_2 0 n)))
 (let ((?x417 (stack_s x_0 x_1 w_2 0 n)))
 (let (($x717 (= ?x417 ?x10322)))
 (or $x717 $x11399)))))))
 ))
 (let ((?x63 (sc_t 0)))
 (let ((?x72 (sc_s 0)))
 (let (($x73 (= ?x72 ?x63)))
 (let (($x8709 (= $x1885 (or (exc_halt_t 4) (not (bvsle (_ bv0 6) (bvadd (_ bv59 6) (sc_t 4))))))))
 (let (($x1431 (forall ((w (_ BitVec 256)) )(let ((?x9664 (storage_t x_0 x_1 w_2 4 w)))
 (let ((?x6102 (storage_t x_0 x_1 w_2 5 w)))
 (= ?x6102 ?x9664))))
 ))
 (let (($x9438 (forall ((n (_ BitVec 6)) )(let ((?x1040 (stack_t x_0 x_1 w_2 4 n)))
 (let ((?x815 (stack_t x_0 x_1 w_2 5 n)))
 (or (= ?x815 ?x1040) (bvsle (bvadd (_ bv59 6) (sc_t 4)) n)))))
 ))
 (let (($x4980 (= (used_gas_t x_0 x_1 w_2 5) (+ 3 (used_gas_t x_0 x_1 w_2 4)))))
 (let (($x11861 (= (stack_t x_0 x_1 w_2 5 (bvadd (_ bv62 6) ?x8961)) (stack_t x_0 x_1 w_2 4 (bvadd (_ bv62 6) (sc_t 4))))))
 (let (($x10027 (= (stack_t x_0 x_1 w_2 5 (bvadd (_ bv61 6) ?x8961)) (stack_t x_0 x_1 w_2 4 (bvadd (_ bv61 6) (sc_t 4))))))
 (let (($x8256 (= (stack_t x_0 x_1 w_2 5 (bvadd (_ bv60 6) ?x8961)) (stack_t x_0 x_1 w_2 4 (bvadd (_ bv60 6) (sc_t 4))))))
 (let ((?x2827 (stack_t x_0 x_1 w_2 4 (bvadd (_ bv63 6) (sc_t 4)))))
 (let (($x4894 (= (stack_t x_0 x_1 w_2 5 (bvadd (_ bv63 6) ?x8961)) (stack_t x_0 x_1 w_2 4 (bvadd (_ bv59 6) (sc_t 4))))))
 (let (($x3614 (exc_halt_t 3)))
 (let (($x11706 (or (not (bvsle (_ bv0 6) (bvadd (_ bv61 6) (sc_t 3)))) (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_t 3)))) (_ bv0 1))) $x3614)))
 (let (($x7854 (exc_halt_t 4)))
 (let (($x10496 (forall ((w (_ BitVec 256)) )(let ((?x236 (storage_t x_0 x_1 w_2 3 w)))
 (let ((?x9664 (storage_t x_0 x_1 w_2 4 w)))
 (= ?x9664 ?x236))))
 ))
 (let (($x5570 (forall ((n (_ BitVec 6)) )(let ((?x489 (stack_t x_0 x_1 w_2 3 n)))
 (let ((?x1040 (stack_t x_0 x_1 w_2 4 n)))
 (or (bvsle (bvadd (_ bv61 6) (sc_t 3)) n) (= ?x1040 ?x489)))))
 ))
 (let ((?x1245 (used_gas_t x_0 x_1 w_2 4)))
 (let ((?x11304 (sc_t 3)))
 (let ((?x3412 (bvadd (_ bv63 6) ?x11304)))
 (let ((?x7135 (stack_t x_0 x_1 w_2 3 ?x3412)))
 (let ((?x305 (bvadd (_ bv62 6) ?x11304)))
 (let ((?x4229 (stack_t x_0 x_1 w_2 3 ?x305)))
 (let ((?x3237 (bvadd (_ bv61 6) ?x11304)))
 (let ((?x4574 (stack_t x_0 x_1 w_2 3 ?x3237)))
 (let (($x10435 (= $x3614 (or (exc_halt_t 2) (not (bvsle (_ bv0 6) (bvadd (_ bv61 6) (sc_t 2))))))))
 (let (($x4741 (forall ((w (_ BitVec 256)) )(let ((?x8136 (storage_t x_0 x_1 w_2 2 w)))
 (let ((?x236 (storage_t x_0 x_1 w_2 3 w)))
 (= ?x236 ?x8136))))
 ))
 (let (($x9327 (forall ((n (_ BitVec 6)) )(let ((?x7687 (stack_t x_0 x_1 w_2 2 n)))
 (let ((?x489 (stack_t x_0 x_1 w_2 3 n)))
 (or (= ?x489 ?x7687) (bvsle (bvadd (_ bv61 6) (sc_t 2)) n)))))
 ))
 (let ((?x8154 (used_gas_t x_0 x_1 w_2 3)))
 (let (($x8377 (exc_halt_t 1)))
 (let (($x9056 (or $x8377 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_t 1)))) (_ bv0 1))))))
 (let (($x9627 (exc_halt_t 2)))
 (let (($x10490 (forall ((w (_ BitVec 256)) )(let ((?x8269 (storage_t x_0 x_1 w_2 1 w)))
 (let ((?x8136 (storage_t x_0 x_1 w_2 2 w)))
 (= ?x8136 ?x8269))))
 ))
 (let (($x9001 (forall ((n (_ BitVec 6)) )(let ((?x9873 (stack_t x_0 x_1 w_2 1 n)))
 (let ((?x7687 (stack_t x_0 x_1 w_2 2 n)))
 (let ((?x7154 (sc_t 1)))
 (let (($x2077 (bvsle ?x7154 n)))
 (or $x2077 (= ?x7687 ?x9873)))))))
 ))
 (let ((?x5852 (used_gas_t x_0 x_1 w_2 2)))
 (let (($x521 (= $x8377 (or $x56 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x63))) (_ bv0 1)))))))
 (let (($x10697 (forall ((w (_ BitVec 256)) )(let ((?x11112 (storage_t x_0 x_1 w_2 0 w)))
 (let ((?x8269 (storage_t x_0 x_1 w_2 1 w)))
 (= ?x8269 ?x11112))))
 ))
 (let (($x6589 (forall ((n (_ BitVec 6)) )(let ((?x63 (sc_t 0)))
 (let (($x11399 (bvsle ?x63 n)))
 (let ((?x10322 (stack_t x_0 x_1 w_2 0 n)))
 (let ((?x9873 (stack_t x_0 x_1 w_2 1 n)))
 (or (= ?x9873 ?x10322) $x11399))))))
 ))
 (let (($x4310 (= $x772 (or (exc_halt_s 5) (not (bvsle (_ bv0 6) (bvadd (_ bv61 6) (sc_s 5))))))))
 (let (($x4761 (forall ((w (_ BitVec 256)) )(let ((?x11988 (storage_s x_0 x_1 w_2 5 w)))
 (let ((?x6167 (storage_s x_0 x_1 w_2 6 w)))
 (= ?x6167 ?x11988))))
 ))
 (let (($x1537 (forall ((n (_ BitVec 6)) )(let ((?x11620 (stack_s x_0 x_1 w_2 5 n)))
 (let ((?x9448 (stack_s x_0 x_1 w_2 6 n)))
 (or (= ?x9448 ?x11620) (bvsle (bvadd (_ bv61 6) (sc_s 5)) n)))))
 ))
 (let (($x1908 (= (used_gas_s x_0 x_1 w_2 6) (+ 3 (used_gas_s x_0 x_1 w_2 5)))))
 (let (($x8554 (= (stack_s x_0 x_1 w_2 6 (bvadd (_ bv62 6) ?x926)) (stack_s x_0 x_1 w_2 5 (bvadd (_ bv62 6) (sc_s 5))))))
 (let ((?x4319 (sc_s 5)))
 (let ((?x11178 (bvadd (_ bv63 6) ?x4319)))
 (let ((?x9988 (stack_s x_0 x_1 w_2 5 ?x11178)))
 (let (($x2378 (= (stack_s x_0 x_1 w_2 6 (bvadd (_ bv63 6) ?x926)) (stack_s x_0 x_1 w_2 5 (bvadd (_ bv61 6) ?x4319)))))
 (let (($x7172 (exc_halt_s 4)))
 (let (($x2009 (or (not (bvsle (_ bv0 6) (bvadd (_ bv61 6) (sc_s 4)))) $x7172 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 4)))) (_ bv0 1))))))
 (let (($x11317 (exc_halt_s 5)))
 (let (($x641 (forall ((w (_ BitVec 256)) )(let ((?x5687 (storage_s x_0 x_1 w_2 4 w)))
 (let ((?x11988 (storage_s x_0 x_1 w_2 5 w)))
 (= ?x11988 ?x5687))))
 ))
 (let (($x2606 (forall ((n (_ BitVec 6)) )(let ((?x3707 (stack_s x_0 x_1 w_2 4 n)))
 (let ((?x11620 (stack_s x_0 x_1 w_2 5 n)))
 (or (bvsle (bvadd (_ bv61 6) (sc_s 4)) n) (= ?x11620 ?x3707)))))
 ))
 (let ((?x10591 (used_gas_s x_0 x_1 w_2 5)))
 (let ((?x4305 (sc_s 4)))
 (let ((?x4400 (bvadd (_ bv63 6) ?x4305)))
 (let ((?x1799 (stack_s x_0 x_1 w_2 4 ?x4400)))
 (let ((?x11444 (bvadd (_ bv62 6) ?x4305)))
 (let ((?x664 (stack_s x_0 x_1 w_2 4 ?x11444)))
 (let ((?x5303 (bvadd (_ bv61 6) ?x4305)))
 (let ((?x10650 (stack_s x_0 x_1 w_2 4 ?x5303)))
 (let (($x11455 (= $x7172 (or (exc_halt_s 3) (not (bvsle (_ bv0 6) (bvadd (_ bv61 6) (sc_s 3))))))))
 (let (($x10043 (forall ((w (_ BitVec 256)) )(let ((?x1981 (storage_s x_0 x_1 w_2 3 w)))
 (let ((?x5687 (storage_s x_0 x_1 w_2 4 w)))
 (= ?x5687 ?x1981))))
 ))
 (let (($x5079 (forall ((n (_ BitVec 6)) )(let ((?x9392 (stack_s x_0 x_1 w_2 3 n)))
 (let ((?x3707 (stack_s x_0 x_1 w_2 4 n)))
 (or (= ?x3707 ?x9392) (bvsle (bvadd (_ bv61 6) (sc_s 3)) n)))))
 ))
 (let ((?x11054 (used_gas_s x_0 x_1 w_2 4)))
 (let (($x9863 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 2)))) (_ bv0 1)))))
 (let (($x247 (exc_halt_s 2)))
 (let (($x292 (exc_halt_s 3)))
 (let (($x7349 (forall ((w (_ BitVec 256)) )(let ((?x4867 (storage_s x_0 x_1 w_2 2 w)))
 (let ((?x1981 (storage_s x_0 x_1 w_2 3 w)))
 (= ?x1981 ?x4867))))
 ))
 (let (($x11158 (forall ((n (_ BitVec 6)) )(let ((?x5362 (stack_s x_0 x_1 w_2 2 n)))
 (let ((?x9392 (stack_s x_0 x_1 w_2 3 n)))
 (or (= ?x9392 ?x5362) (bvsle (bvadd (_ bv61 6) (sc_s 2)) n)))))
 ))
 (let ((?x275 (sc_s 3)))
 (let (($x1829 (= ?x275 (bvadd (_ bv1 6) (sc_s 2)))))
 (let ((?x4914 (used_gas_s x_0 x_1 w_2 3)))
 (let ((?x218 (sc_s 2)))
 (let ((?x5342 (bvadd (_ bv63 6) ?x218)))
 (let ((?x7300 (stack_s x_0 x_1 w_2 2 ?x5342)))
 (let ((?x11717 (bvadd (_ bv62 6) ?x218)))
 (let ((?x4187 (stack_s x_0 x_1 w_2 2 ?x11717)))
 (let ((?x3497 (bvadd (_ bv61 6) ?x218)))
 (let ((?x10558 (stack_s x_0 x_1 w_2 2 ?x3497)))
 (let (($x2521 (= $x247 (or (exc_halt_s 1) (not (bvsle (_ bv0 6) (bvadd (_ bv61 6) (sc_s 1))))))))
 (let (($x7370 (forall ((w (_ BitVec 256)) )(let ((?x3172 (storage_s x_0 x_1 w_2 1 w)))
 (let ((?x4867 (storage_s x_0 x_1 w_2 2 w)))
 (= ?x4867 ?x3172))))
 ))
 (let (($x8320 (forall ((n (_ BitVec 6)) )(let ((?x3029 (stack_s x_0 x_1 w_2 1 n)))
 (let ((?x5362 (stack_s x_0 x_1 w_2 2 n)))
 (or (bvsle (bvadd (_ bv61 6) (sc_s 1)) n) (= ?x5362 ?x3029)))))
 ))
 (let ((?x154 (sc_s 1)))
 (let (($x10223 (= ?x218 ?x154)))
 (let ((?x11872 (used_gas_s x_0 x_1 w_2 2)))
 (let (($x189 (exc_halt_s 1)))
 (let (($x5870 (= $x189 (or $x57 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x72))) (_ bv0 1)))))))
 (let (($x5019 (forall ((w (_ BitVec 256)) )(let ((?x4242 (storage_s x_0 x_1 w_2 0 w)))
 (let ((?x3172 (storage_s x_0 x_1 w_2 1 w)))
 (= ?x3172 ?x4242))))
 ))
 (let (($x9198 (forall ((n (_ BitVec 6)) )(let ((?x417 (stack_s x_0 x_1 w_2 0 n)))
 (let ((?x3029 (stack_s x_0 x_1 w_2 1 n)))
 (or (= ?x3029 ?x417) (bvsle (sc_s 0) n)))))
 ))
 (let (($x2270 (forall ((w (_ BitVec 256)) )(let ((?x4242 (storage_s x_0 x_1 w_2 0 w)))
 (= ?x4242 (_ bv0 256))))
 ))
 (let (($x4676 (= ?x8617 0)))
 (let (($x11580 (not $x57)))
 (let (($x1734 (= (stack_s x_0 x_1 w_2 0 (_ bv1 6)) x_1)))
 (let (($x11395 (= (stack_s x_0 x_1 w_2 0 (_ bv0 6)) x_0)))
 (let (($x11128 (= ?x72 (_ bv2 6))))
 (and $x11128 $x11395 $x1734 $x11580 $x4676 $x2270 (= (stack_s x_0 x_1 w_2 1 ?x72) w_2) (= (used_gas_s x_0 x_1 w_2 1) (+ 3 ?x8617)) (= ?x154 (bvadd (_ bv1 6) ?x72)) $x9198 $x5019 $x5870 (= ?x7300 (stack_s x_0 x_1 w_2 1 (bvadd (_ bv61 6) ?x154))) (= ?x10558 (stack_s x_0 x_1 w_2 1 (bvadd (_ bv63 6) ?x154))) (= ?x4187 (stack_s x_0 x_1 w_2 1 (bvadd (_ bv62 6) ?x154))) (= ?x11872 (+ 3 (used_gas_s x_0 x_1 w_2 1))) $x10223 $x8320 $x7370 $x2521 (= (stack_s x_0 x_1 w_2 3 (bvadd (_ bv63 6) ?x275)) ?x10558) (= (stack_s x_0 x_1 w_2 3 ?x3497) ?x10558) (= (stack_s x_0 x_1 w_2 3 ?x11717) ?x4187) (= (stack_s x_0 x_1 w_2 3 ?x5342) ?x7300) (= ?x4914 (+ 3 ?x11872)) $x1829 $x11158 $x7349 (= $x292 (or $x247 (not (bvsle (_ bv0 6) ?x3497)) $x9863)) (= ?x1799 (stack_s x_0 x_1 w_2 3 (bvadd (_ bv61 6) ?x275))) (= ?x10650 (stack_s x_0 x_1 w_2 3 (bvadd (_ bv63 6) ?x275))) (= ?x664 (stack_s x_0 x_1 w_2 3 (bvadd (_ bv62 6) ?x275))) (= ?x11054 (+ 3 ?x4914)) (= ?x4305 ?x275) $x5079 $x10043 $x11455 (= ?x9988 ?x10650) (= (stack_s x_0 x_1 w_2 5 ?x5303) ?x10650) (= (stack_s x_0 x_1 w_2 5 ?x11444) ?x664) (= (stack_s x_0 x_1 w_2 5 ?x4400) ?x1799) (= ?x10591 (+ 3 ?x11054)) (= ?x4319 (bvadd (_ bv1 6) ?x4305)) $x2606 $x641 (= $x11317 $x2009) $x2378 (= (stack_s x_0 x_1 w_2 6 (bvadd (_ bv61 6) ?x926)) ?x9988) $x8554 $x1908 (= ?x926 ?x4319) $x1537 $x4761 $x4310 (= (stack_t x_0 x_1 w_2 1 ?x63) w_2) (= (used_gas_t x_0 x_1 w_2 1) (+ 3 ?x10131)) (= (sc_t 1) (bvadd (_ bv1 6) ?x63)) $x6589 $x10697 $x521 (= (stack_t x_0 x_1 w_2 2 (sc_t 1)) w_2) (= ?x5852 (+ 3 (used_gas_t x_0 x_1 w_2 1))) (= (sc_t 2) (bvadd (_ bv1 6) (sc_t 1))) $x9001 $x10490 (= $x9627 $x9056) (= ?x7135 (stack_t x_0 x_1 w_2 2 (bvadd (_ bv61 6) (sc_t 2)))) (= ?x4574 (stack_t x_0 x_1 w_2 2 (bvadd (_ bv63 6) (sc_t 2)))) (= ?x4229 (stack_t x_0 x_1 w_2 2 (bvadd (_ bv62 6) (sc_t 2)))) (= ?x8154 (+ 3 ?x5852)) (= ?x11304 (sc_t 2)) $x9327 $x4741 $x10435 (= ?x2827 ?x4574) (= (stack_t x_0 x_1 w_2 4 ?x3237) ?x4574) (= (stack_t x_0 x_1 w_2 4 ?x305) ?x4229) (= (stack_t x_0 x_1 w_2 4 ?x3412) ?x7135) (= ?x1245 (+ 3 ?x8154)) (= (sc_t 4) (bvadd (_ bv1 6) ?x11304)) $x5570 $x10496 (= $x7854 $x11706) $x4894 (= (stack_t x_0 x_1 w_2 5 (bvadd (_ bv59 6) ?x8961)) ?x2827) $x8256 $x10027 $x11861 $x4980 (= ?x8961 (sc_t 4)) $x9438 $x1431 $x8709 $x73 $x1597 $x58 $x7860 $x6882 (not (and $x5889 $x5100 $x11108 $x2964)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
 )
(check-sat)
(get-proof)
