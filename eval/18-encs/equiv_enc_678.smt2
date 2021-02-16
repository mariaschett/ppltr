; PUSH cw_3 DUP3 DUP1 MLOAD PUSH cw_2 SWAP3 => PUSH cw_2 DUP3 DUP1 MLOAD PUSH cw_3
(set-option :produce-proofs true)
; 
(set-info :status unknown)
(declare-fun storage_t ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 256)) (_ BitVec 256))
(declare-fun w_2 () (_ BitVec 256))
(declare-fun w_3 () (_ BitVec 256))
(declare-fun storage_s ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 256)) (_ BitVec 256))
(declare-fun exc_halt_t (Int) Bool)
(declare-fun exc_halt_s (Int) Bool)
(declare-fun sc_t (Int) (_ BitVec 6))
(declare-fun stack_t ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 6)) (_ BitVec 256))
(declare-fun stack_s ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 6)) (_ BitVec 256))
(declare-fun sc_s (Int) (_ BitVec 6))
(declare-fun used_gas_t ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int) Int)
(declare-fun used_gas_s ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int) Int)
(declare-fun f_MLOAD ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256)) (_ BitVec 256))
(assert
 (exists ((x_0 (_ BitVec 256)) (x_1 (_ BitVec 256)) (x_MLOAD_0 (_ BitVec 256)) )(let (($x7438 (forall ((w (_ BitVec 256)) )(let ((?x3251 (storage_t x_0 x_1 w_3 w_2 x_MLOAD_0 5 w)))
 (let ((?x5835 (storage_s x_0 x_1 w_3 w_2 x_MLOAD_0 6 w)))
 (= ?x5835 ?x3251))))
 ))
 (let (($x1885 (exc_halt_t 5)))
 (let (($x772 (exc_halt_s 6)))
 (let (($x11108 (= $x772 $x1885)))
 (let (($x8737 (forall ((n (_ BitVec 6)) )(let ((?x6408 (stack_t x_0 x_1 w_3 w_2 x_MLOAD_0 5 n)))
 (let ((?x9509 (stack_s x_0 x_1 w_3 w_2 x_MLOAD_0 6 n)))
 (let (($x5331 (= ?x9509 ?x6408)))
 (or $x5331 (bvsle (sc_t 5) n))))))
 ))
 (let ((?x8961 (sc_t 5)))
 (let ((?x926 (sc_s 6)))
 (let (($x5889 (= ?x926 ?x8961)))
 (let (($x5876 (not (and $x5889 $x8737 $x11108 $x7438))))
 (let ((?x736 (used_gas_t x_0 x_1 w_3 w_2 x_MLOAD_0 0)))
 (let ((?x1762 (used_gas_s x_0 x_1 w_3 w_2 x_MLOAD_0 0)))
 (let (($x3216 (= ?x1762 ?x736)))
 (let (($x2868 (forall ((w (_ BitVec 256)) )(let ((?x8049 (storage_t x_0 x_1 w_3 w_2 x_MLOAD_0 0 w)))
 (let ((?x6875 (storage_s x_0 x_1 w_3 w_2 x_MLOAD_0 0 w)))
 (= ?x6875 ?x8049))))
 ))
 (let (($x56 (exc_halt_t 0)))
 (let (($x57 (exc_halt_s 0)))
 (let (($x58 (= $x57 $x56)))
 (let (($x9371 (forall ((n (_ BitVec 6)) )(let ((?x9809 (stack_t x_0 x_1 w_3 w_2 x_MLOAD_0 0 n)))
 (let ((?x1266 (stack_s x_0 x_1 w_3 w_2 x_MLOAD_0 0 n)))
 (let (($x7793 (= ?x1266 ?x9809)))
 (let ((?x63 (sc_t 0)))
 (let (($x7673 (bvsle ?x63 n)))
 (or $x7673 $x7793)))))))
 ))
 (let ((?x63 (sc_t 0)))
 (let ((?x72 (sc_s 0)))
 (let (($x73 (= ?x72 ?x63)))
 (let (($x7854 (exc_halt_t 4)))
 (let (($x7213 (or $x7854 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_t 4)))) (_ bv0 1))))))
 (let (($x6334 (forall ((w (_ BitVec 256)) )(let ((?x6950 (storage_t x_0 x_1 w_3 w_2 x_MLOAD_0 4 w)))
 (let ((?x3251 (storage_t x_0 x_1 w_3 w_2 x_MLOAD_0 5 w)))
 (= ?x3251 ?x6950))))
 ))
 (let (($x5519 (forall ((n (_ BitVec 6)) )(let ((?x4818 (sc_t 4)))
 (let (($x11615 (bvsle ?x4818 n)))
 (let ((?x380 (stack_t x_0 x_1 w_3 w_2 x_MLOAD_0 4 n)))
 (let ((?x6408 (stack_t x_0 x_1 w_3 w_2 x_MLOAD_0 5 n)))
 (let (($x7938 (= ?x6408 ?x380)))
 (or $x7938 $x11615)))))))
 ))
 (let (($x9263 (= (used_gas_t x_0 x_1 w_3 w_2 x_MLOAD_0 5) (+ 3 (used_gas_t x_0 x_1 w_3 w_2 x_MLOAD_0 4)))))
 (let (($x11257 (= $x7854 (or (exc_halt_t 3) (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_t 3))))))))
 (let (($x9226 (forall ((w (_ BitVec 256)) )(let ((?x6660 (storage_t x_0 x_1 w_3 w_2 x_MLOAD_0 3 w)))
 (let ((?x6950 (storage_t x_0 x_1 w_3 w_2 x_MLOAD_0 4 w)))
 (= ?x6950 ?x6660))))
 ))
 (let (($x4529 (forall ((n (_ BitVec 6)) )(let ((?x11304 (sc_t 3)))
 (let ((?x3894 (bvadd (_ bv63 6) ?x11304)))
 (let (($x9385 (bvsle ?x3894 n)))
 (let ((?x5806 (stack_t x_0 x_1 w_3 w_2 x_MLOAD_0 3 n)))
 (let ((?x380 (stack_t x_0 x_1 w_3 w_2 x_MLOAD_0 4 n)))
 (let (($x29 (= ?x380 ?x5806)))
 (or $x29 $x9385))))))))
 ))
 (let ((?x11304 (sc_t 3)))
 (let ((?x4818 (sc_t 4)))
 (let (($x11782 (= ?x4818 ?x11304)))
 (let ((?x4897 (used_gas_t x_0 x_1 w_3 w_2 x_MLOAD_0 4)))
 (let (($x615 (= ?x4897 (+ 3 (used_gas_t x_0 x_1 w_3 w_2 x_MLOAD_0 3)))))
 (let ((?x3894 (bvadd (_ bv63 6) ?x11304)))
 (let ((?x3622 (stack_t x_0 x_1 w_3 w_2 x_MLOAD_0 3 ?x3894)))
 (let ((?x1337 (bvadd (_ bv63 6) ?x4818)))
 (let ((?x5760 (stack_t x_0 x_1 w_3 w_2 x_MLOAD_0 4 ?x1337)))
 (let (($x8472 (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_t 2))))))
 (let (($x3470 (exc_halt_t 2)))
 (let (($x1855 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_t 2)))) (_ bv0 1)))))
 (let (($x3614 (exc_halt_t 3)))
 (let (($x8844 (forall ((w (_ BitVec 256)) )(let ((?x9877 (storage_t x_0 x_1 w_3 w_2 x_MLOAD_0 2 w)))
 (let ((?x6660 (storage_t x_0 x_1 w_3 w_2 x_MLOAD_0 3 w)))
 (= ?x6660 ?x9877))))
 ))
 (let (($x5950 (forall ((n (_ BitVec 6)) )(let ((?x11777 (stack_t x_0 x_1 w_3 w_2 x_MLOAD_0 2 n)))
 (let ((?x5806 (stack_t x_0 x_1 w_3 w_2 x_MLOAD_0 3 n)))
 (let (($x5550 (= ?x5806 ?x11777)))
 (let ((?x6158 (sc_t 2)))
 (let ((?x10639 (bvadd (_ bv63 6) ?x6158)))
 (let (($x3163 (bvsle ?x10639 n)))
 (or $x3163 $x5550))))))))
 ))
 (let (($x10331 (= ?x11304 (bvadd (_ bv1 6) (sc_t 2)))))
 (let ((?x1686 (used_gas_t x_0 x_1 w_3 w_2 x_MLOAD_0 3)))
 (let (($x590 (= ?x1686 (+ 3 (used_gas_t x_0 x_1 w_3 w_2 x_MLOAD_0 2)))))
 (let ((?x6158 (sc_t 2)))
 (let ((?x10639 (bvadd (_ bv63 6) ?x6158)))
 (let ((?x9750 (stack_t x_0 x_1 w_3 w_2 x_MLOAD_0 2 ?x10639)))
 (let (($x8377 (exc_halt_t 1)))
 (let (($x4195 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_t 1)))) (_ bv0 1)))))
 (let (($x309 (not (bvsle (_ bv0 6) (bvadd (_ bv61 6) (sc_t 1))))))
 (let (($x808 (= $x3470 (or $x309 $x4195 $x8377))))
 (let (($x5448 (forall ((w (_ BitVec 256)) )(let ((?x3098 (storage_t x_0 x_1 w_3 w_2 x_MLOAD_0 1 w)))
 (let ((?x9877 (storage_t x_0 x_1 w_3 w_2 x_MLOAD_0 2 w)))
 (= ?x9877 ?x3098))))
 ))
 (let (($x10943 (forall ((n (_ BitVec 6)) )(let ((?x7154 (sc_t 1)))
 (let ((?x549 (bvadd (_ bv61 6) ?x7154)))
 (let (($x11698 (bvsle ?x549 n)))
 (let ((?x1810 (stack_t x_0 x_1 w_3 w_2 x_MLOAD_0 1 n)))
 (let ((?x11777 (stack_t x_0 x_1 w_3 w_2 x_MLOAD_0 2 n)))
 (let (($x4726 (= ?x11777 ?x1810)))
 (or $x4726 $x11698))))))))
 ))
 (let (($x2783 (= ?x6158 (bvadd (_ bv1 6) (sc_t 1)))))
 (let ((?x9684 (used_gas_t x_0 x_1 w_3 w_2 x_MLOAD_0 2)))
 (let (($x5459 (= ?x9684 (+ 3 (used_gas_t x_0 x_1 w_3 w_2 x_MLOAD_0 1)))))
 (let ((?x7154 (sc_t 1)))
 (let ((?x6967 (bvadd (_ bv63 6) ?x7154)))
 (let ((?x8869 (stack_t x_0 x_1 w_3 w_2 x_MLOAD_0 1 ?x6967)))
 (let (($x7167 (= (stack_t x_0 x_1 w_3 w_2 x_MLOAD_0 2 (bvadd (_ bv62 6) ?x7154)) (stack_t x_0 x_1 w_3 w_2 x_MLOAD_0 1 (bvadd (_ bv62 6) ?x7154)))))
 (let ((?x549 (bvadd (_ bv61 6) ?x7154)))
 (let ((?x1819 (stack_t x_0 x_1 w_3 w_2 x_MLOAD_0 1 ?x549)))
 (let (($x4983 (= $x8377 (or $x56 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x63))) (_ bv0 1)))))))
 (let (($x804 (forall ((w (_ BitVec 256)) )(let ((?x8049 (storage_t x_0 x_1 w_3 w_2 x_MLOAD_0 0 w)))
 (let ((?x3098 (storage_t x_0 x_1 w_3 w_2 x_MLOAD_0 1 w)))
 (= ?x3098 ?x8049))))
 ))
 (let (($x7125 (forall ((n (_ BitVec 6)) )(let ((?x63 (sc_t 0)))
 (let (($x7673 (bvsle ?x63 n)))
 (let ((?x9809 (stack_t x_0 x_1 w_3 w_2 x_MLOAD_0 0 n)))
 (let ((?x1810 (stack_t x_0 x_1 w_3 w_2 x_MLOAD_0 1 n)))
 (or (= ?x1810 ?x9809) $x7673))))))
 ))
 (let (($x3228 (= ?x7154 (bvadd (_ bv1 6) ?x63))))
 (let ((?x4568 (used_gas_t x_0 x_1 w_3 w_2 x_MLOAD_0 1)))
 (let (($x10196 (= ?x4568 (+ 3 ?x736))))
 (let (($x9967 (= (stack_t x_0 x_1 w_3 w_2 x_MLOAD_0 1 ?x63) w_2)))
 (let (($x648 (= $x772 (or (exc_halt_s 5) (not (bvsle (_ bv0 6) (bvadd (_ bv60 6) (sc_s 5))))))))
 (let (($x6421 (forall ((w (_ BitVec 256)) )(let ((?x3273 (storage_s x_0 x_1 w_3 w_2 x_MLOAD_0 5 w)))
 (let ((?x5835 (storage_s x_0 x_1 w_3 w_2 x_MLOAD_0 6 w)))
 (= ?x5835 ?x3273))))
 ))
 (let (($x6961 (forall ((n (_ BitVec 6)) )(let ((?x9119 (stack_s x_0 x_1 w_3 w_2 x_MLOAD_0 5 n)))
 (let ((?x9509 (stack_s x_0 x_1 w_3 w_2 x_MLOAD_0 6 n)))
 (let (($x413 (= ?x9509 ?x9119)))
 (or $x413 (bvsle (bvadd (_ bv60 6) (sc_s 5)) n))))))
 ))
 (let ((?x4319 (sc_s 5)))
 (let (($x6201 (= ?x926 ?x4319)))
 (let (($x10099 (= (used_gas_s x_0 x_1 w_3 w_2 x_MLOAD_0 6) (+ 3 (used_gas_s x_0 x_1 w_3 w_2 x_MLOAD_0 5)))))
 (let ((?x5664 (bvadd (_ bv62 6) ?x4319)))
 (let ((?x2611 (stack_s x_0 x_1 w_3 w_2 x_MLOAD_0 5 ?x5664)))
 (let ((?x11339 (stack_s x_0 x_1 w_3 w_2 x_MLOAD_0 6 (bvadd (_ bv62 6) ?x926))))
 (let (($x8715 (= (stack_s x_0 x_1 w_3 w_2 x_MLOAD_0 6 (bvadd (_ bv61 6) ?x926)) (stack_s x_0 x_1 w_3 w_2 x_MLOAD_0 5 (bvadd (_ bv61 6) ?x4319)))))
 (let ((?x5078 (bvadd (_ bv63 6) ?x4319)))
 (let ((?x7157 (stack_s x_0 x_1 w_3 w_2 x_MLOAD_0 5 ?x5078)))
 (let (($x10455 (= (stack_s x_0 x_1 w_3 w_2 x_MLOAD_0 6 (bvadd (_ bv60 6) ?x926)) ?x7157)))
 (let ((?x1305 (bvadd (_ bv63 6) ?x926)))
 (let ((?x5201 (stack_s x_0 x_1 w_3 w_2 x_MLOAD_0 6 ?x1305)))
 (let (($x2709 (= ?x5201 (stack_s x_0 x_1 w_3 w_2 x_MLOAD_0 5 (bvadd (_ bv60 6) ?x4319)))))
 (let (($x7172 (exc_halt_s 4)))
 (let (($x5512 (or $x7172 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 4)))) (_ bv0 1))))))
 (let (($x11317 (exc_halt_s 5)))
 (let (($x576 (forall ((w (_ BitVec 256)) )(let ((?x11053 (storage_s x_0 x_1 w_3 w_2 x_MLOAD_0 4 w)))
 (let ((?x3273 (storage_s x_0 x_1 w_3 w_2 x_MLOAD_0 5 w)))
 (= ?x3273 ?x11053))))
 ))
 (let (($x11149 (forall ((n (_ BitVec 6)) )(let ((?x4632 (stack_s x_0 x_1 w_3 w_2 x_MLOAD_0 4 n)))
 (let ((?x9119 (stack_s x_0 x_1 w_3 w_2 x_MLOAD_0 5 n)))
 (let (($x6638 (= ?x9119 ?x4632)))
 (or $x6638 (bvsle (sc_s 4) n))))))
 ))
 (let ((?x8943 (used_gas_s x_0 x_1 w_3 w_2 x_MLOAD_0 5)))
 (let (($x7182 (= ?x8943 (+ 3 (used_gas_s x_0 x_1 w_3 w_2 x_MLOAD_0 4)))))
 (let (($x8322 (= $x7172 (or (exc_halt_s 3) (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_s 3))))))))
 (let (($x2690 (forall ((w (_ BitVec 256)) )(let ((?x4523 (storage_s x_0 x_1 w_3 w_2 x_MLOAD_0 3 w)))
 (let ((?x11053 (storage_s x_0 x_1 w_3 w_2 x_MLOAD_0 4 w)))
 (= ?x11053 ?x4523))))
 ))
 (let (($x2235 (forall ((n (_ BitVec 6)) )(let ((?x275 (sc_s 3)))
 (let ((?x1344 (bvadd (_ bv63 6) ?x275)))
 (let (($x11157 (bvsle ?x1344 n)))
 (let ((?x6530 (stack_s x_0 x_1 w_3 w_2 x_MLOAD_0 3 n)))
 (let ((?x4632 (stack_s x_0 x_1 w_3 w_2 x_MLOAD_0 4 n)))
 (let (($x4879 (= ?x4632 ?x6530)))
 (or $x4879 $x11157))))))))
 ))
 (let ((?x275 (sc_s 3)))
 (let ((?x4305 (sc_s 4)))
 (let (($x4997 (= ?x4305 ?x275)))
 (let ((?x7153 (used_gas_s x_0 x_1 w_3 w_2 x_MLOAD_0 4)))
 (let (($x11216 (= ?x7153 (+ 3 (used_gas_s x_0 x_1 w_3 w_2 x_MLOAD_0 3)))))
 (let ((?x1344 (bvadd (_ bv63 6) ?x275)))
 (let ((?x4033 (stack_s x_0 x_1 w_3 w_2 x_MLOAD_0 3 ?x1344)))
 (let ((?x11664 (bvadd (_ bv63 6) ?x4305)))
 (let ((?x6416 (stack_s x_0 x_1 w_3 w_2 x_MLOAD_0 4 ?x11664)))
 (let (($x11134 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 2)))) (_ bv0 1)))))
 (let (($x6735 (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_s 2))))))
 (let (($x247 (exc_halt_s 2)))
 (let (($x292 (exc_halt_s 3)))
 (let (($x10969 (forall ((w (_ BitVec 256)) )(let ((?x2364 (storage_s x_0 x_1 w_3 w_2 x_MLOAD_0 2 w)))
 (let ((?x4523 (storage_s x_0 x_1 w_3 w_2 x_MLOAD_0 3 w)))
 (= ?x4523 ?x2364))))
 ))
 (let (($x10734 (forall ((n (_ BitVec 6)) )(let ((?x7538 (stack_s x_0 x_1 w_3 w_2 x_MLOAD_0 2 n)))
 (let ((?x6530 (stack_s x_0 x_1 w_3 w_2 x_MLOAD_0 3 n)))
 (let (($x9811 (= ?x6530 ?x7538)))
 (let ((?x218 (sc_s 2)))
 (let ((?x2158 (bvadd (_ bv63 6) ?x218)))
 (let (($x2580 (bvsle ?x2158 n)))
 (or $x2580 $x9811))))))))
 ))
 (let (($x7369 (= ?x275 (bvadd (_ bv1 6) (sc_s 2)))))
 (let ((?x7011 (used_gas_s x_0 x_1 w_3 w_2 x_MLOAD_0 3)))
 (let (($x7261 (= ?x7011 (+ 3 (used_gas_s x_0 x_1 w_3 w_2 x_MLOAD_0 2)))))
 (let ((?x218 (sc_s 2)))
 (let ((?x2158 (bvadd (_ bv63 6) ?x218)))
 (let ((?x1566 (stack_s x_0 x_1 w_3 w_2 x_MLOAD_0 2 ?x2158)))
 (let (($x10203 (not (bvsle (_ bv0 6) (bvadd (_ bv61 6) (sc_s 1))))))
 (let (($x6759 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 1)))) (_ bv0 1)))))
 (let (($x189 (exc_halt_s 1)))
 (let (($x5119 (forall ((w (_ BitVec 256)) )(let ((?x996 (storage_s x_0 x_1 w_3 w_2 x_MLOAD_0 1 w)))
 (let ((?x2364 (storage_s x_0 x_1 w_3 w_2 x_MLOAD_0 2 w)))
 (= ?x2364 ?x996))))
 ))
 (let (($x11674 (forall ((n (_ BitVec 6)) )(let ((?x154 (sc_s 1)))
 (let ((?x9461 (bvadd (_ bv61 6) ?x154)))
 (let (($x5795 (bvsle ?x9461 n)))
 (let ((?x5561 (stack_s x_0 x_1 w_3 w_2 x_MLOAD_0 1 n)))
 (let ((?x7538 (stack_s x_0 x_1 w_3 w_2 x_MLOAD_0 2 n)))
 (let (($x1812 (= ?x7538 ?x5561)))
 (or $x1812 $x5795))))))))
 ))
 (let (($x6266 (= ?x218 (bvadd (_ bv1 6) (sc_s 1)))))
 (let ((?x1650 (used_gas_s x_0 x_1 w_3 w_2 x_MLOAD_0 2)))
 (let (($x2731 (= ?x1650 (+ 3 (used_gas_s x_0 x_1 w_3 w_2 x_MLOAD_0 1)))))
 (let ((?x154 (sc_s 1)))
 (let ((?x174 (bvadd (_ bv63 6) ?x154)))
 (let ((?x4943 (stack_s x_0 x_1 w_3 w_2 x_MLOAD_0 1 ?x174)))
 (let ((?x9694 (bvadd (_ bv62 6) ?x154)))
 (let ((?x4883 (stack_s x_0 x_1 w_3 w_2 x_MLOAD_0 1 ?x9694)))
 (let ((?x9461 (bvadd (_ bv61 6) ?x154)))
 (let ((?x5807 (stack_s x_0 x_1 w_3 w_2 x_MLOAD_0 1 ?x9461)))
 (let (($x10804 (= $x189 (or $x57 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x72))) (_ bv0 1)))))))
 (let (($x8964 (forall ((w (_ BitVec 256)) )(let ((?x6875 (storage_s x_0 x_1 w_3 w_2 x_MLOAD_0 0 w)))
 (let ((?x996 (storage_s x_0 x_1 w_3 w_2 x_MLOAD_0 1 w)))
 (= ?x996 ?x6875))))
 ))
 (let (($x965 (forall ((n (_ BitVec 6)) )(let ((?x72 (sc_s 0)))
 (let (($x3419 (bvsle ?x72 n)))
 (let ((?x1266 (stack_s x_0 x_1 w_3 w_2 x_MLOAD_0 0 n)))
 (let ((?x5561 (stack_s x_0 x_1 w_3 w_2 x_MLOAD_0 1 n)))
 (let (($x777 (= ?x5561 ?x1266)))
 (or $x777 $x3419)))))))
 ))
 (let (($x8655 (= ?x154 (bvadd (_ bv1 6) ?x72))))
 (let ((?x4567 (used_gas_s x_0 x_1 w_3 w_2 x_MLOAD_0 1)))
 (let (($x8629 (= ?x4567 (+ 3 ?x1762))))
 (let (($x8198 (forall ((w0 (_ BitVec 256)) )(let (($x6553 (= (stack_s x_0 x_1 w_3 w_2 x_MLOAD_0 3 (bvadd (_ bv63 6) (sc_s 3))) w0)))
 (let ((?x3169 (f_MLOAD x_0 x_1 w_3 w_2 x_MLOAD_0 w0)))
 (= ?x3169 (ite $x6553 x_MLOAD_0 (_ bv0 256))))))
 ))
 (let (($x4831 (forall ((w (_ BitVec 256)) )(let ((?x6875 (storage_s x_0 x_1 w_3 w_2 x_MLOAD_0 0 w)))
 (= ?x6875 (_ bv0 256))))
 ))
 (let (($x2506 (= ?x1762 0)))
 (let (($x7303 (not $x57)))
 (let (($x1888 (= (stack_s x_0 x_1 w_3 w_2 x_MLOAD_0 0 (_ bv1 6)) x_1)))
 (let (($x3828 (= (stack_s x_0 x_1 w_3 w_2 x_MLOAD_0 0 (_ bv0 6)) x_0)))
 (let (($x11128 (= ?x72 (_ bv2 6))))
 (and $x11128 $x3828 $x1888 $x7303 $x2506 $x4831 $x8198 (= (stack_s x_0 x_1 w_3 w_2 x_MLOAD_0 1 ?x72) w_3) $x8629 $x8655 $x965 $x8964 $x10804 (= ?x1566 ?x5807) (= (stack_s x_0 x_1 w_3 w_2 x_MLOAD_0 2 ?x9461) ?x5807) (= (stack_s x_0 x_1 w_3 w_2 x_MLOAD_0 2 ?x9694) ?x4883) (= (stack_s x_0 x_1 w_3 w_2 x_MLOAD_0 2 ?x174) ?x4943) $x2731 $x6266 $x11674 $x5119 (= $x247 (or $x189 $x6759 $x10203)) (= ?x4033 ?x1566) (= (stack_s x_0 x_1 w_3 w_2 x_MLOAD_0 3 ?x2158) ?x1566) $x7261 $x7369 $x10734 $x10969 (= $x292 (or $x247 $x6735 $x11134)) (= ?x6416 (f_MLOAD x_0 x_1 w_3 w_2 x_MLOAD_0 ?x4033)) $x11216 $x4997 $x2235 $x2690 $x8322 (= (stack_s x_0 x_1 w_3 w_2 x_MLOAD_0 5 ?x4305) w_2) $x7182 (= ?x4319 (bvadd (_ bv1 6) ?x4305)) $x11149 $x576 (= $x11317 $x5512) $x2709 $x10455 $x8715 (= ?x11339 ?x2611) $x10099 $x6201 $x6961 $x6421 $x648 $x9967 $x10196 $x3228 $x7125 $x804 $x4983 (= ?x9750 ?x1819) (= (stack_t x_0 x_1 w_3 w_2 x_MLOAD_0 2 ?x549) ?x1819) $x7167 (= (stack_t x_0 x_1 w_3 w_2 x_MLOAD_0 2 ?x6967) ?x8869) $x5459 $x2783 $x10943 $x5448 $x808 (= ?x3622 ?x9750) (= (stack_t x_0 x_1 w_3 w_2 x_MLOAD_0 3 ?x10639) ?x9750) $x590 $x10331 $x5950 $x8844 (= $x3614 (or $x1855 $x3470 $x8472)) (= ?x5760 (f_MLOAD x_0 x_1 w_3 w_2 x_MLOAD_0 ?x3622)) $x615 $x11782 $x4529 $x9226 $x11257 (= (stack_t x_0 x_1 w_3 w_2 x_MLOAD_0 5 ?x4818) w_3) $x9263 (= ?x8961 (bvadd (_ bv1 6) ?x4818)) $x5519 $x6334 (= $x1885 $x7213) $x73 $x9371 $x58 $x2868 $x3216 $x5876))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
 )
(check-sat)
(get-proof)