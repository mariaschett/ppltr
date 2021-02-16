; PUSH cw_5 PUSH cw_6 PUSH cw_7 PUSH cw_8 PUSH cw_5 SWAP1 => PUSH cw_5 PUSH cw_6 PUSH cw_7 DUP3 PUSH cw_8
(set-option :produce-proofs true)
; 
(set-info :status unknown)
(declare-fun storage_t ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 256)) (_ BitVec 256))
(declare-fun w_8 () (_ BitVec 256))
(declare-fun w_7 () (_ BitVec 256))
(declare-fun w_6 () (_ BitVec 256))
(declare-fun w_5 () (_ BitVec 256))
(declare-fun storage_s ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 256)) (_ BitVec 256))
(declare-fun exc_halt_t (Int) Bool)
(declare-fun exc_halt_s (Int) Bool)
(declare-fun stack_t ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 6)) (_ BitVec 256))
(declare-fun stack_s ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 6)) (_ BitVec 256))
(declare-fun sc_t (Int) (_ BitVec 6))
(declare-fun sc_s (Int) (_ BitVec 6))
(declare-fun used_gas_t ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int) Int)
(declare-fun used_gas_s ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int) Int)
(assert
 (let (($x1784 (forall ((w (_ BitVec 256)) )(let ((?x8113 (storage_t w_5 w_6 w_7 w_8 5 w)))
 (let ((?x1103 (storage_s w_5 w_6 w_7 w_8 6 w)))
 (= ?x1103 ?x8113))))
 ))
 (let (($x1885 (exc_halt_t 5)))
 (let (($x772 (exc_halt_s 6)))
 (let (($x11108 (= $x772 $x1885)))
 (let (($x1735 (forall ((n (_ BitVec 6)) )(let ((?x11303 (stack_t w_5 w_6 w_7 w_8 5 n)))
 (let ((?x6104 (stack_s w_5 w_6 w_7 w_8 6 n)))
 (let (($x11087 (= ?x6104 ?x11303)))
 (let ((?x8961 (sc_t 5)))
 (let (($x9968 (bvsle ?x8961 n)))
 (or $x9968 $x11087)))))))
 ))
 (let ((?x8961 (sc_t 5)))
 (let ((?x926 (sc_s 6)))
 (let (($x5889 (= ?x926 ?x8961)))
 (let ((?x5438 (used_gas_t w_5 w_6 w_7 w_8 0)))
 (let ((?x7232 (used_gas_s w_5 w_6 w_7 w_8 0)))
 (let (($x7687 (= ?x7232 ?x5438)))
 (let (($x11085 (forall ((w (_ BitVec 256)) )(let ((?x674 (storage_t w_5 w_6 w_7 w_8 0 w)))
 (let ((?x2301 (storage_s w_5 w_6 w_7 w_8 0 w)))
 (= ?x2301 ?x674))))
 ))
 (let (($x56 (exc_halt_t 0)))
 (let (($x57 (exc_halt_s 0)))
 (let (($x58 (= $x57 $x56)))
 (let (($x5051 (forall ((n (_ BitVec 6)) )(let ((?x63 (sc_t 0)))
 (let (($x10511 (bvsle ?x63 n)))
 (let ((?x2713 (stack_t w_5 w_6 w_7 w_8 0 n)))
 (let ((?x8690 (stack_s w_5 w_6 w_7 w_8 0 n)))
 (let (($x2126 (= ?x8690 ?x2713)))
 (or $x2126 $x10511)))))))
 ))
 (let ((?x63 (sc_t 0)))
 (let ((?x72 (sc_s 0)))
 (let (($x73 (= ?x72 ?x63)))
 (let (($x7854 (exc_halt_t 4)))
 (let (($x10397 (or $x7854 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_t 4)))) (_ bv0 1))))))
 (let (($x7762 (forall ((w (_ BitVec 256)) )(let ((?x5559 (storage_t w_5 w_6 w_7 w_8 4 w)))
 (let ((?x8113 (storage_t w_5 w_6 w_7 w_8 5 w)))
 (= ?x8113 ?x5559))))
 ))
 (let (($x7942 (forall ((n (_ BitVec 6)) )(let ((?x4818 (sc_t 4)))
 (let (($x6786 (bvsle ?x4818 n)))
 (let ((?x9957 (stack_t w_5 w_6 w_7 w_8 4 n)))
 (let ((?x11303 (stack_t w_5 w_6 w_7 w_8 5 n)))
 (or (= ?x11303 ?x9957) $x6786))))))
 ))
 (let (($x2273 (= (used_gas_t w_5 w_6 w_7 w_8 5) (+ 3 (used_gas_t w_5 w_6 w_7 w_8 4)))))
 (let (($x10488 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_t 3)))) (_ bv0 1)))))
 (let (($x6783 (exc_halt_t 3)))
 (let (($x4966 (forall ((w (_ BitVec 256)) )(let ((?x9293 (storage_t w_5 w_6 w_7 w_8 3 w)))
 (let ((?x5559 (storage_t w_5 w_6 w_7 w_8 4 w)))
 (= ?x5559 ?x9293))))
 ))
 (let (($x2697 (forall ((n (_ BitVec 6)) )(let ((?x7029 (stack_t w_5 w_6 w_7 w_8 3 n)))
 (let ((?x9957 (stack_t w_5 w_6 w_7 w_8 4 n)))
 (or (= ?x9957 ?x7029) (bvsle (bvadd (_ bv61 6) (sc_t 3)) n)))))
 ))
 (let ((?x4818 (sc_t 4)))
 (let (($x8781 (= ?x4818 (bvadd (_ bv1 6) (sc_t 3)))))
 (let ((?x6097 (used_gas_t w_5 w_6 w_7 w_8 4)))
 (let (($x2802 (= (stack_t w_5 w_6 w_7 w_8 4 (bvadd (_ bv63 6) (sc_t 3))) (stack_t w_5 w_6 w_7 w_8 3 (bvadd (_ bv63 6) (sc_t 3))))))
 (let (($x2219 (= (stack_t w_5 w_6 w_7 w_8 4 (bvadd (_ bv62 6) (sc_t 3))) (stack_t w_5 w_6 w_7 w_8 3 (bvadd (_ bv62 6) (sc_t 3))))))
 (let ((?x6438 (sc_t 3)))
 (let ((?x3511 (bvadd (_ bv61 6) ?x6438)))
 (let ((?x804 (stack_t w_5 w_6 w_7 w_8 3 ?x3511)))
 (let (($x8424 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_t 2)))) (_ bv0 1)))))
 (let (($x2163 (exc_halt_t 2)))
 (let (($x5035 (= $x6783 (or $x2163 $x8424))))
 (let (($x404 (forall ((w (_ BitVec 256)) )(let ((?x8698 (storage_t w_5 w_6 w_7 w_8 2 w)))
 (let ((?x9293 (storage_t w_5 w_6 w_7 w_8 3 w)))
 (= ?x9293 ?x8698))))
 ))
 (let (($x10755 (forall ((n (_ BitVec 6)) )(let ((?x2714 (sc_t 2)))
 (let (($x1920 (bvsle ?x2714 n)))
 (let ((?x4512 (stack_t w_5 w_6 w_7 w_8 2 n)))
 (let ((?x7029 (stack_t w_5 w_6 w_7 w_8 3 n)))
 (or (= ?x7029 ?x4512) $x1920))))))
 ))
 (let (($x7516 (= ?x6438 (bvadd (_ bv1 6) (sc_t 2)))))
 (let ((?x862 (used_gas_t w_5 w_6 w_7 w_8 3)))
 (let (($x9824 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_t 1)))) (_ bv0 1)))))
 (let (($x8377 (exc_halt_t 1)))
 (let (($x6999 (= $x2163 (or $x8377 $x9824))))
 (let (($x10430 (forall ((w (_ BitVec 256)) )(let ((?x1059 (storage_t w_5 w_6 w_7 w_8 1 w)))
 (let ((?x8698 (storage_t w_5 w_6 w_7 w_8 2 w)))
 (= ?x8698 ?x1059))))
 ))
 (let (($x6308 (forall ((n (_ BitVec 6)) )(let ((?x7154 (sc_t 1)))
 (let (($x8103 (bvsle ?x7154 n)))
 (let ((?x4059 (stack_t w_5 w_6 w_7 w_8 1 n)))
 (let ((?x4512 (stack_t w_5 w_6 w_7 w_8 2 n)))
 (or (= ?x4512 ?x4059) $x8103))))))
 ))
 (let ((?x2714 (sc_t 2)))
 (let (($x44 (= ?x2714 (bvadd (_ bv1 6) (sc_t 1)))))
 (let ((?x10520 (used_gas_t w_5 w_6 w_7 w_8 2)))
 (let (($x428 (= $x8377 (or $x56 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x63))) (_ bv0 1)))))))
 (let (($x10899 (forall ((w (_ BitVec 256)) )(let ((?x674 (storage_t w_5 w_6 w_7 w_8 0 w)))
 (let ((?x1059 (storage_t w_5 w_6 w_7 w_8 1 w)))
 (= ?x1059 ?x674))))
 ))
 (let (($x2932 (forall ((n (_ BitVec 6)) )(let ((?x63 (sc_t 0)))
 (let (($x10511 (bvsle ?x63 n)))
 (let ((?x2713 (stack_t w_5 w_6 w_7 w_8 0 n)))
 (let ((?x4059 (stack_t w_5 w_6 w_7 w_8 1 n)))
 (or (= ?x4059 ?x2713) $x10511))))))
 ))
 (let ((?x7154 (sc_t 1)))
 (let (($x888 (= ?x7154 (bvadd (_ bv1 6) ?x63))))
 (let (($x3884 (= $x772 (or (exc_halt_s 5) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 5))))))))
 (let (($x4388 (forall ((w (_ BitVec 256)) )(let ((?x1161 (storage_s w_5 w_6 w_7 w_8 5 w)))
 (let ((?x1103 (storage_s w_5 w_6 w_7 w_8 6 w)))
 (= ?x1103 ?x1161))))
 ))
 (let (($x6285 (forall ((n (_ BitVec 6)) )(let ((?x2394 (stack_s w_5 w_6 w_7 w_8 5 n)))
 (let ((?x6104 (stack_s w_5 w_6 w_7 w_8 6 n)))
 (let ((?x4319 (sc_s 5)))
 (let ((?x10059 (bvadd (_ bv62 6) ?x4319)))
 (let (($x4069 (bvsle ?x10059 n)))
 (or $x4069 (= ?x6104 ?x2394))))))))
 ))
 (let ((?x4319 (sc_s 5)))
 (let (($x2838 (= ?x926 ?x4319)))
 (let (($x4946 (= (used_gas_s w_5 w_6 w_7 w_8 6) (+ 3 (used_gas_s w_5 w_6 w_7 w_8 5)))))
 (let (($x3577 (= (stack_s w_5 w_6 w_7 w_8 6 (bvadd (_ bv62 6) ?x926)) (stack_s w_5 w_6 w_7 w_8 5 (bvadd (_ bv63 6) ?x4319)))))
 (let (($x3539 (= (stack_s w_5 w_6 w_7 w_8 6 (bvadd (_ bv63 6) ?x926)) (stack_s w_5 w_6 w_7 w_8 5 (bvadd (_ bv62 6) ?x4319)))))
 (let (($x8114 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 4)))) (_ bv0 1)))))
 (let (($x7172 (exc_halt_s 4)))
 (let (($x11317 (exc_halt_s 5)))
 (let (($x9911 (= $x11317 (or $x7172 $x8114))))
 (let (($x6232 (forall ((w (_ BitVec 256)) )(let ((?x7721 (storage_s w_5 w_6 w_7 w_8 4 w)))
 (let ((?x1161 (storage_s w_5 w_6 w_7 w_8 5 w)))
 (= ?x1161 ?x7721))))
 ))
 (let (($x495 (forall ((n (_ BitVec 6)) )(let ((?x4305 (sc_s 4)))
 (let (($x8577 (bvsle ?x4305 n)))
 (let ((?x1477 (stack_s w_5 w_6 w_7 w_8 4 n)))
 (let ((?x2394 (stack_s w_5 w_6 w_7 w_8 5 n)))
 (or (= ?x2394 ?x1477) $x8577))))))
 ))
 (let (($x1688 (= ?x4319 (bvadd (_ bv1 6) (sc_s 4)))))
 (let ((?x1906 (used_gas_s w_5 w_6 w_7 w_8 5)))
 (let (($x292 (exc_halt_s 3)))
 (let (($x5143 (or $x292 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 3)))) (_ bv0 1))))))
 (let (($x2439 (= $x7172 $x5143)))
 (let (($x7882 (forall ((w (_ BitVec 256)) )(let ((?x6106 (storage_s w_5 w_6 w_7 w_8 3 w)))
 (let ((?x7721 (storage_s w_5 w_6 w_7 w_8 4 w)))
 (= ?x7721 ?x6106))))
 ))
 (let (($x2544 (forall ((n (_ BitVec 6)) )(let ((?x275 (sc_s 3)))
 (let (($x11728 (bvsle ?x275 n)))
 (let ((?x8874 (stack_s w_5 w_6 w_7 w_8 3 n)))
 (let ((?x1477 (stack_s w_5 w_6 w_7 w_8 4 n)))
 (or (= ?x1477 ?x8874) $x11728))))))
 ))
 (let ((?x4305 (sc_s 4)))
 (let (($x5448 (= ?x4305 (bvadd (_ bv1 6) (sc_s 3)))))
 (let ((?x7847 (used_gas_s w_5 w_6 w_7 w_8 4)))
 (let (($x7451 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 2)))) (_ bv0 1)))))
 (let (($x247 (exc_halt_s 2)))
 (let (($x9479 (= $x292 (or $x247 $x7451))))
 (let (($x3139 (forall ((w (_ BitVec 256)) )(let ((?x3566 (storage_s w_5 w_6 w_7 w_8 2 w)))
 (let ((?x6106 (storage_s w_5 w_6 w_7 w_8 3 w)))
 (= ?x6106 ?x3566))))
 ))
 (let (($x3037 (forall ((n (_ BitVec 6)) )(let ((?x9974 (stack_s w_5 w_6 w_7 w_8 2 n)))
 (let ((?x8874 (stack_s w_5 w_6 w_7 w_8 3 n)))
 (let ((?x218 (sc_s 2)))
 (let (($x1516 (bvsle ?x218 n)))
 (or $x1516 (= ?x8874 ?x9974)))))))
 ))
 (let ((?x275 (sc_s 3)))
 (let (($x7268 (= ?x275 (bvadd (_ bv1 6) (sc_s 2)))))
 (let ((?x3793 (used_gas_s w_5 w_6 w_7 w_8 3)))
 (let (($x6570 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 1)))) (_ bv0 1)))))
 (let (($x189 (exc_halt_s 1)))
 (let (($x8519 (forall ((w (_ BitVec 256)) )(let ((?x2485 (storage_s w_5 w_6 w_7 w_8 1 w)))
 (let ((?x3566 (storage_s w_5 w_6 w_7 w_8 2 w)))
 (= ?x3566 ?x2485))))
 ))
 (let (($x8348 (forall ((n (_ BitVec 6)) )(let ((?x8179 (stack_s w_5 w_6 w_7 w_8 1 n)))
 (let ((?x9974 (stack_s w_5 w_6 w_7 w_8 2 n)))
 (or (bvsle (sc_s 1) n) (= ?x9974 ?x8179)))))
 ))
 (let ((?x218 (sc_s 2)))
 (let (($x4497 (= ?x218 (bvadd (_ bv1 6) (sc_s 1)))))
 (let ((?x368 (used_gas_s w_5 w_6 w_7 w_8 2)))
 (let (($x6847 (= $x189 (or $x57 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x72))) (_ bv0 1)))))))
 (let (($x11069 (forall ((w (_ BitVec 256)) )(let ((?x2301 (storage_s w_5 w_6 w_7 w_8 0 w)))
 (let ((?x2485 (storage_s w_5 w_6 w_7 w_8 1 w)))
 (= ?x2485 ?x2301))))
 ))
 (let (($x10445 (forall ((n (_ BitVec 6)) )(let ((?x72 (sc_s 0)))
 (let (($x6228 (bvsle ?x72 n)))
 (let ((?x8690 (stack_s w_5 w_6 w_7 w_8 0 n)))
 (let ((?x8179 (stack_s w_5 w_6 w_7 w_8 1 n)))
 (or (= ?x8179 ?x8690) $x6228))))))
 ))
 (let ((?x154 (sc_s 1)))
 (let (($x1581 (= ?x154 (bvadd (_ bv1 6) ?x72))))
 (let (($x3082 (forall ((w (_ BitVec 256)) )(let ((?x2301 (storage_s w_5 w_6 w_7 w_8 0 w)))
 (= ?x2301 (_ bv0 256))))
 ))
 (let (($x2753 (= ?x7232 0)))
 (let (($x7737 (not $x57)))
 (let (($x1074 (= ?x72 (_ bv0 6))))
 (and $x1074 $x7737 $x2753 $x3082 (= (stack_s w_5 w_6 w_7 w_8 1 ?x72) w_5) (= (used_gas_s w_5 w_6 w_7 w_8 1) (+ 3 ?x7232)) $x1581 $x10445 $x11069 $x6847 (= (stack_s w_5 w_6 w_7 w_8 2 ?x154) w_6) (= ?x368 (+ 3 (used_gas_s w_5 w_6 w_7 w_8 1))) $x4497 $x8348 $x8519 (= $x247 (or $x189 $x6570)) (= (stack_s w_5 w_6 w_7 w_8 3 ?x218) w_7) (= ?x3793 (+ 3 ?x368)) $x7268 $x3037 $x3139 $x9479 (= (stack_s w_5 w_6 w_7 w_8 4 ?x275) w_8) (= ?x7847 (+ 3 ?x3793)) $x5448 $x2544 $x7882 $x2439 (= (stack_s w_5 w_6 w_7 w_8 5 ?x4305) w_5) (= ?x1906 (+ 3 ?x7847)) $x1688 $x495 $x6232 $x9911 $x3539 $x3577 $x4946 $x2838 $x6285 $x4388 $x3884 (= (stack_t w_5 w_6 w_7 w_8 1 ?x63) w_5) (= (used_gas_t w_5 w_6 w_7 w_8 1) (+ 3 ?x5438)) $x888 $x2932 $x10899 $x428 (= (stack_t w_5 w_6 w_7 w_8 2 ?x7154) w_6) (= ?x10520 (+ 3 (used_gas_t w_5 w_6 w_7 w_8 1))) $x44 $x6308 $x10430 $x6999 (= (stack_t w_5 w_6 w_7 w_8 3 ?x2714) w_7) (= ?x862 (+ 3 ?x10520)) $x7516 $x10755 $x404 $x5035 (= (stack_t w_5 w_6 w_7 w_8 4 (bvadd (_ bv63 6) ?x4818)) ?x804) (= (stack_t w_5 w_6 w_7 w_8 4 ?x3511) ?x804) $x2219 $x2802 (= ?x6097 (+ 3 ?x862)) $x8781 $x2697 $x4966 (= $x7854 (or $x6783 $x10488 (not (bvsle (_ bv0 6) ?x3511)))) (= (stack_t w_5 w_6 w_7 w_8 5 ?x4818) w_8) $x2273 (= ?x8961 (bvadd (_ bv1 6) ?x4818)) $x7942 $x7762 (= $x1885 $x10397) $x73 $x5051 $x58 $x11085 $x7687 (not (and $x5889 $x1735 $x11108 $x1784))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
(check-sat)
(get-proof)