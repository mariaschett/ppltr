; PUSH cw_1 SWAP2 SWAP1 DUP5 SWAP1 => SWAP1 DUP4 PUSH cw_1 SWAP3
(set-option :produce-proofs true)
; 
(set-info :status unknown)
(declare-fun storage_t ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 256)) (_ BitVec 256))
(declare-fun w_1 () (_ BitVec 256))
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
 (exists ((x_0 (_ BitVec 256)) (x_1 (_ BitVec 256)) (x_2 (_ BitVec 256)) (x_3 (_ BitVec 256)) )(let (($x5390 (forall ((w (_ BitVec 256)) )(let ((?x9797 (storage_t x_0 x_1 x_2 x_3 w_1 4 w)))
 (let ((?x5373 (storage_s x_0 x_1 x_2 x_3 w_1 5 w)))
 (= ?x5373 ?x9797))))
 ))
 (let (($x7854 (exc_halt_t 4)))
 (let (($x11317 (exc_halt_s 5)))
 (let (($x6479 (= $x11317 $x7854)))
 (let (($x325 (forall ((n (_ BitVec 6)) )(let ((?x4818 (sc_t 4)))
 (let (($x6358 (bvsle ?x4818 n)))
 (let ((?x6845 (stack_t x_0 x_1 x_2 x_3 w_1 4 n)))
 (let ((?x682 (stack_s x_0 x_1 x_2 x_3 w_1 5 n)))
 (let (($x1037 (= ?x682 ?x6845)))
 (or $x1037 $x6358)))))))
 ))
 (let ((?x4818 (sc_t 4)))
 (let ((?x4319 (sc_s 5)))
 (let (($x1733 (= ?x4319 ?x4818)))
 (let ((?x3002 (used_gas_t x_0 x_1 x_2 x_3 w_1 0)))
 (let ((?x4974 (used_gas_s x_0 x_1 x_2 x_3 w_1 0)))
 (let (($x10070 (= ?x4974 ?x3002)))
 (let (($x1370 (forall ((w (_ BitVec 256)) )(let ((?x6531 (storage_t x_0 x_1 x_2 x_3 w_1 0 w)))
 (let ((?x600 (storage_s x_0 x_1 x_2 x_3 w_1 0 w)))
 (= ?x600 ?x6531))))
 ))
 (let (($x56 (exc_halt_t 0)))
 (let (($x57 (exc_halt_s 0)))
 (let (($x58 (= $x57 $x56)))
 (let (($x10820 (forall ((n (_ BitVec 6)) )(let ((?x1697 (stack_t x_0 x_1 x_2 x_3 w_1 0 n)))
 (let ((?x4332 (stack_s x_0 x_1 x_2 x_3 w_1 0 n)))
 (let (($x8365 (= ?x4332 ?x1697)))
 (let ((?x63 (sc_t 0)))
 (let (($x6552 (bvsle ?x63 n)))
 (or $x6552 $x8365)))))))
 ))
 (let ((?x63 (sc_t 0)))
 (let ((?x72 (sc_s 0)))
 (let (($x73 (= ?x72 ?x63)))
 (let (($x11930 (= $x7854 (or (exc_halt_t 3) (not (bvsle (_ bv0 6) (bvadd (_ bv60 6) (sc_t 3))))))))
 (let (($x9158 (forall ((w (_ BitVec 256)) )(let ((?x3151 (storage_t x_0 x_1 x_2 x_3 w_1 3 w)))
 (let ((?x9797 (storage_t x_0 x_1 x_2 x_3 w_1 4 w)))
 (= ?x9797 ?x3151))))
 ))
 (let (($x11062 (forall ((n (_ BitVec 6)) )(let ((?x11304 (sc_t 3)))
 (let ((?x7353 (bvadd (_ bv60 6) ?x11304)))
 (let (($x11313 (bvsle ?x7353 n)))
 (let ((?x9743 (stack_t x_0 x_1 x_2 x_3 w_1 3 n)))
 (let ((?x6845 (stack_t x_0 x_1 x_2 x_3 w_1 4 n)))
 (or (= ?x6845 ?x9743) $x11313)))))))
 ))
 (let ((?x11304 (sc_t 3)))
 (let (($x2509 (= ?x4818 ?x11304)))
 (let (($x11756 (= (used_gas_t x_0 x_1 x_2 x_3 w_1 4) (+ 3 (used_gas_t x_0 x_1 x_2 x_3 w_1 3)))))
 (let (($x6023 (= (stack_t x_0 x_1 x_2 x_3 w_1 4 (bvadd (_ bv62 6) ?x4818)) (stack_t x_0 x_1 x_2 x_3 w_1 3 (bvadd (_ bv62 6) ?x11304)))))
 (let (($x4137 (= (stack_t x_0 x_1 x_2 x_3 w_1 4 (bvadd (_ bv61 6) ?x4818)) (stack_t x_0 x_1 x_2 x_3 w_1 3 (bvadd (_ bv61 6) ?x11304)))))
 (let (($x515 (= (stack_t x_0 x_1 x_2 x_3 w_1 4 (bvadd (_ bv60 6) ?x4818)) (stack_t x_0 x_1 x_2 x_3 w_1 3 (bvadd (_ bv63 6) ?x11304)))))
 (let (($x11649 (= (stack_t x_0 x_1 x_2 x_3 w_1 4 (bvadd (_ bv63 6) ?x4818)) (stack_t x_0 x_1 x_2 x_3 w_1 3 (bvadd (_ bv60 6) ?x11304)))))
 (let (($x9674 (exc_halt_t 2)))
 (let (($x7563 (or $x9674 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_t 2)))) (_ bv0 1))))))
 (let (($x3614 (exc_halt_t 3)))
 (let (($x1977 (= $x3614 $x7563)))
 (let (($x11573 (forall ((w (_ BitVec 256)) )(let ((?x11968 (storage_t x_0 x_1 x_2 x_3 w_1 2 w)))
 (let ((?x3151 (storage_t x_0 x_1 x_2 x_3 w_1 3 w)))
 (= ?x3151 ?x11968))))
 ))
 (let (($x1944 (forall ((n (_ BitVec 6)) )(let ((?x4473 (stack_t x_0 x_1 x_2 x_3 w_1 2 n)))
 (let ((?x9743 (stack_t x_0 x_1 x_2 x_3 w_1 3 n)))
 (let ((?x6158 (sc_t 2)))
 (let (($x2552 (bvsle ?x6158 n)))
 (or $x2552 (= ?x9743 ?x4473)))))))
 ))
 (let (($x2667 (= ?x11304 (bvadd (_ bv1 6) (sc_t 2)))))
 (let ((?x3513 (used_gas_t x_0 x_1 x_2 x_3 w_1 3)))
 (let (($x8377 (exc_halt_t 1)))
 (let (($x3336 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_t 1)))) (_ bv0 1)))))
 (let (($x10808 (= $x9674 (or $x3336 $x8377 (not (bvsle (_ bv0 6) (bvadd (_ bv60 6) (sc_t 1))))))))
 (let (($x9499 (forall ((w (_ BitVec 256)) )(let ((?x9299 (storage_t x_0 x_1 x_2 x_3 w_1 1 w)))
 (let ((?x11968 (storage_t x_0 x_1 x_2 x_3 w_1 2 w)))
 (= ?x11968 ?x9299))))
 ))
 (let (($x9539 (forall ((n (_ BitVec 6)) )(let ((?x7154 (sc_t 1)))
 (let ((?x7321 (bvadd (_ bv60 6) ?x7154)))
 (let (($x11953 (bvsle ?x7321 n)))
 (let ((?x10460 (stack_t x_0 x_1 x_2 x_3 w_1 1 n)))
 (let ((?x4473 (stack_t x_0 x_1 x_2 x_3 w_1 2 n)))
 (or (= ?x4473 ?x10460) $x11953)))))))
 ))
 (let ((?x6158 (sc_t 2)))
 (let (($x3131 (= ?x6158 (bvadd (_ bv1 6) (sc_t 1)))))
 (let ((?x3385 (used_gas_t x_0 x_1 x_2 x_3 w_1 2)))
 (let ((?x7154 (sc_t 1)))
 (let ((?x3400 (bvadd (_ bv63 6) ?x7154)))
 (let ((?x10642 (stack_t x_0 x_1 x_2 x_3 w_1 1 ?x3400)))
 (let ((?x11582 (bvadd (_ bv62 6) ?x7154)))
 (let ((?x6777 (stack_t x_0 x_1 x_2 x_3 w_1 1 ?x11582)))
 (let (($x5023 (= (stack_t x_0 x_1 x_2 x_3 w_1 2 (bvadd (_ bv61 6) ?x7154)) (stack_t x_0 x_1 x_2 x_3 w_1 1 (bvadd (_ bv61 6) ?x7154)))))
 (let ((?x7321 (bvadd (_ bv60 6) ?x7154)))
 (let ((?x9840 (stack_t x_0 x_1 x_2 x_3 w_1 1 ?x7321)))
 (let (($x8963 (= $x8377 (or $x56 (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) ?x63)))))))
 (let (($x3724 (forall ((w (_ BitVec 256)) )(let ((?x6531 (storage_t x_0 x_1 x_2 x_3 w_1 0 w)))
 (let ((?x9299 (storage_t x_0 x_1 x_2 x_3 w_1 1 w)))
 (= ?x9299 ?x6531))))
 ))
 (let (($x10487 (forall ((n (_ BitVec 6)) )(let ((?x63 (sc_t 0)))
 (let ((?x816 (bvadd (_ bv62 6) ?x63)))
 (let (($x1110 (bvsle ?x816 n)))
 (let ((?x1697 (stack_t x_0 x_1 x_2 x_3 w_1 0 n)))
 (let ((?x10460 (stack_t x_0 x_1 x_2 x_3 w_1 1 n)))
 (or (= ?x10460 ?x1697) $x1110)))))))
 ))
 (let (($x9247 (= $x11317 (or (exc_halt_s 4) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 4))))))))
 (let (($x11570 (forall ((w (_ BitVec 256)) )(let ((?x3473 (storage_s x_0 x_1 x_2 x_3 w_1 4 w)))
 (let ((?x5373 (storage_s x_0 x_1 x_2 x_3 w_1 5 w)))
 (= ?x5373 ?x3473))))
 ))
 (let (($x2727 (forall ((n (_ BitVec 6)) )(let ((?x4305 (sc_s 4)))
 (let ((?x103 (bvadd (_ bv62 6) ?x4305)))
 (let (($x8692 (bvsle ?x103 n)))
 (let ((?x7808 (stack_s x_0 x_1 x_2 x_3 w_1 4 n)))
 (let ((?x682 (stack_s x_0 x_1 x_2 x_3 w_1 5 n)))
 (or (= ?x682 ?x7808) $x8692)))))))
 ))
 (let ((?x4305 (sc_s 4)))
 (let (($x3901 (= ?x4319 ?x4305)))
 (let (($x10197 (= (used_gas_s x_0 x_1 x_2 x_3 w_1 5) (+ 3 (used_gas_s x_0 x_1 x_2 x_3 w_1 4)))))
 (let ((?x8905 (bvadd (_ bv63 6) ?x4305)))
 (let ((?x6038 (stack_s x_0 x_1 x_2 x_3 w_1 4 ?x8905)))
 (let (($x11545 (= (stack_s x_0 x_1 x_2 x_3 w_1 5 (bvadd (_ bv63 6) ?x4319)) (stack_s x_0 x_1 x_2 x_3 w_1 4 (bvadd (_ bv62 6) ?x4305)))))
 (let (($x292 (exc_halt_s 3)))
 (let (($x5861 (or $x292 (not (bvsle (_ bv0 6) (bvadd (_ bv59 6) (sc_s 3)))) (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 3)))) (_ bv0 1))))))
 (let (($x7172 (exc_halt_s 4)))
 (let (($x4561 (forall ((w (_ BitVec 256)) )(let ((?x3455 (storage_s x_0 x_1 x_2 x_3 w_1 3 w)))
 (let ((?x3473 (storage_s x_0 x_1 x_2 x_3 w_1 4 w)))
 (= ?x3473 ?x3455))))
 ))
 (let (($x6247 (forall ((n (_ BitVec 6)) )(let ((?x5592 (stack_s x_0 x_1 x_2 x_3 w_1 3 n)))
 (let ((?x7808 (stack_s x_0 x_1 x_2 x_3 w_1 4 n)))
 (or (= ?x7808 ?x5592) (bvsle (bvadd (_ bv59 6) (sc_s 3)) n)))))
 ))
 (let ((?x977 (used_gas_s x_0 x_1 x_2 x_3 w_1 4)))
 (let ((?x275 (sc_s 3)))
 (let ((?x3112 (bvadd (_ bv63 6) ?x275)))
 (let ((?x5990 (stack_s x_0 x_1 x_2 x_3 w_1 3 ?x3112)))
 (let ((?x10583 (bvadd (_ bv62 6) ?x275)))
 (let ((?x4916 (stack_s x_0 x_1 x_2 x_3 w_1 3 ?x10583)))
 (let (($x3317 (= (stack_s x_0 x_1 x_2 x_3 w_1 4 (bvadd (_ bv61 6) ?x275)) (stack_s x_0 x_1 x_2 x_3 w_1 3 (bvadd (_ bv61 6) ?x275)))))
 (let (($x6388 (= (stack_s x_0 x_1 x_2 x_3 w_1 4 (bvadd (_ bv60 6) ?x275)) (stack_s x_0 x_1 x_2 x_3 w_1 3 (bvadd (_ bv60 6) ?x275)))))
 (let ((?x6418 (bvadd (_ bv59 6) ?x275)))
 (let ((?x10959 (stack_s x_0 x_1 x_2 x_3 w_1 3 ?x6418)))
 (let (($x10818 (= $x292 (or (exc_halt_s 2) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 2))))))))
 (let (($x10913 (forall ((w (_ BitVec 256)) )(let ((?x2993 (storage_s x_0 x_1 x_2 x_3 w_1 2 w)))
 (let ((?x3455 (storage_s x_0 x_1 x_2 x_3 w_1 3 w)))
 (= ?x3455 ?x2993))))
 ))
 (let (($x11665 (forall ((n (_ BitVec 6)) )(let ((?x1808 (stack_s x_0 x_1 x_2 x_3 w_1 2 n)))
 (let ((?x5592 (stack_s x_0 x_1 x_2 x_3 w_1 3 n)))
 (let ((?x218 (sc_s 2)))
 (let ((?x11936 (bvadd (_ bv62 6) ?x218)))
 (let (($x1347 (bvsle ?x11936 n)))
 (or $x1347 (= ?x5592 ?x1808))))))))
 ))
 (let ((?x218 (sc_s 2)))
 (let (($x530 (= ?x275 ?x218)))
 (let ((?x9873 (used_gas_s x_0 x_1 x_2 x_3 w_1 3)))
 (let (($x247 (exc_halt_s 2)))
 (let (($x7327 (= $x247 (or (exc_halt_s 1) (not (bvsle (_ bv0 6) (bvadd (_ bv61 6) (sc_s 1))))))))
 (let (($x9959 (forall ((w (_ BitVec 256)) )(let ((?x3034 (storage_s x_0 x_1 x_2 x_3 w_1 1 w)))
 (let ((?x2993 (storage_s x_0 x_1 x_2 x_3 w_1 2 w)))
 (= ?x2993 ?x3034))))
 ))
 (let (($x10247 (forall ((n (_ BitVec 6)) )(let ((?x2418 (stack_s x_0 x_1 x_2 x_3 w_1 1 n)))
 (let ((?x1808 (stack_s x_0 x_1 x_2 x_3 w_1 2 n)))
 (let ((?x154 (sc_s 1)))
 (let ((?x9786 (bvadd (_ bv61 6) ?x154)))
 (let (($x6844 (bvsle ?x9786 n)))
 (or $x6844 (= ?x1808 ?x2418))))))))
 ))
 (let ((?x154 (sc_s 1)))
 (let (($x675 (= ?x218 ?x154)))
 (let ((?x8616 (used_gas_s x_0 x_1 x_2 x_3 w_1 2)))
 (let ((?x11936 (bvadd (_ bv62 6) ?x218)))
 (let ((?x786 (stack_s x_0 x_1 x_2 x_3 w_1 2 ?x11936)))
 (let (($x10958 (= (stack_s x_0 x_1 x_2 x_3 w_1 2 (bvadd (_ bv61 6) ?x218)) (stack_s x_0 x_1 x_2 x_3 w_1 1 (bvadd (_ bv63 6) ?x154)))))
 (let ((?x9412 (bvadd (_ bv63 6) ?x218)))
 (let ((?x1125 (stack_s x_0 x_1 x_2 x_3 w_1 2 ?x9412)))
 (let (($x189 (exc_halt_s 1)))
 (let (($x1616 (= $x189 (or $x57 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x72))) (_ bv0 1)))))))
 (let (($x7322 (forall ((w (_ BitVec 256)) )(let ((?x600 (storage_s x_0 x_1 x_2 x_3 w_1 0 w)))
 (let ((?x3034 (storage_s x_0 x_1 x_2 x_3 w_1 1 w)))
 (= ?x3034 ?x600))))
 ))
 (let (($x4081 (forall ((n (_ BitVec 6)) )(let ((?x4332 (stack_s x_0 x_1 x_2 x_3 w_1 0 n)))
 (let ((?x2418 (stack_s x_0 x_1 x_2 x_3 w_1 1 n)))
 (let ((?x72 (sc_s 0)))
 (let (($x2753 (bvsle ?x72 n)))
 (or $x2753 (= ?x2418 ?x4332)))))))
 ))
 (let (($x11322 (= ?x154 (bvadd (_ bv1 6) ?x72))))
 (let (($x8107 (forall ((w (_ BitVec 256)) )(let ((?x600 (storage_s x_0 x_1 x_2 x_3 w_1 0 w)))
 (= ?x600 (_ bv0 256))))
 ))
 (let (($x6696 (= ?x4974 0)))
 (let (($x7449 (not $x57)))
 (let (($x6930 (= (stack_s x_0 x_1 x_2 x_3 w_1 0 (_ bv3 6)) x_3)))
 (let (($x11524 (= (stack_s x_0 x_1 x_2 x_3 w_1 0 (_ bv2 6)) x_2)))
 (let (($x6243 (= (stack_s x_0 x_1 x_2 x_3 w_1 0 (_ bv1 6)) x_1)))
 (let (($x1091 (= (stack_s x_0 x_1 x_2 x_3 w_1 0 (_ bv0 6)) x_0)))
 (let (($x11199 (= ?x72 (_ bv4 6))))
 (and $x11199 $x1091 $x6243 $x11524 $x6930 $x7449 $x6696 $x8107 (= (stack_s x_0 x_1 x_2 x_3 w_1 1 ?x72) w_1) (= (used_gas_s x_0 x_1 x_2 x_3 w_1 1) (+ 3 ?x4974)) $x11322 $x4081 $x7322 $x1616 (= ?x1125 (stack_s x_0 x_1 x_2 x_3 w_1 1 (bvadd (_ bv61 6) ?x154))) $x10958 (= ?x786 (stack_s x_0 x_1 x_2 x_3 w_1 1 (bvadd (_ bv62 6) ?x154))) (= ?x8616 (+ 3 (used_gas_s x_0 x_1 x_2 x_3 w_1 1))) $x675 $x10247 $x9959 $x7327 (= ?x5990 ?x786) (= ?x4916 ?x1125) (= ?x9873 (+ 3 ?x8616)) $x530 $x11665 $x10913 $x10818 (= ?x6038 ?x10959) (= (stack_s x_0 x_1 x_2 x_3 w_1 4 ?x6418) ?x10959) $x6388 $x3317 (= (stack_s x_0 x_1 x_2 x_3 w_1 4 ?x10583) ?x4916) (= (stack_s x_0 x_1 x_2 x_3 w_1 4 ?x3112) ?x5990) (= ?x977 (+ 3 ?x9873)) (= ?x4305 (bvadd (_ bv1 6) ?x275)) $x6247 $x4561 (= $x7172 $x5861) $x11545 (= (stack_s x_0 x_1 x_2 x_3 w_1 5 (bvadd (_ bv62 6) ?x4319)) ?x6038) $x10197 $x3901 $x2727 $x11570 $x9247 (= ?x10642 (stack_t x_0 x_1 x_2 x_3 w_1 0 (bvadd (_ bv62 6) ?x63))) (= ?x6777 (stack_t x_0 x_1 x_2 x_3 w_1 0 (bvadd (_ bv63 6) ?x63))) (= (used_gas_t x_0 x_1 x_2 x_3 w_1 1) (+ 3 ?x3002)) (= ?x7154 ?x63) $x10487 $x3724 $x8963 (= (stack_t x_0 x_1 x_2 x_3 w_1 2 (bvadd (_ bv63 6) ?x6158)) ?x9840) (= (stack_t x_0 x_1 x_2 x_3 w_1 2 ?x7321) ?x9840) $x5023 (= (stack_t x_0 x_1 x_2 x_3 w_1 2 ?x11582) ?x6777) (= (stack_t x_0 x_1 x_2 x_3 w_1 2 ?x3400) ?x10642) (= ?x3385 (+ 3 (used_gas_t x_0 x_1 x_2 x_3 w_1 1))) $x3131 $x9539 $x9499 $x10808 (= (stack_t x_0 x_1 x_2 x_3 w_1 3 ?x6158) w_1) (= ?x3513 (+ 3 ?x3385)) $x2667 $x1944 $x11573 $x1977 $x11649 $x515 $x4137 $x6023 $x11756 $x2509 $x11062 $x9158 $x11930 $x73 $x10820 $x58 $x1370 $x10070 (not (and $x1733 $x325 $x6479 $x5390)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
 )
(check-sat)
(get-proof)