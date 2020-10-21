; PUSH cw_0 DUP2 DUP3 SWAP2 POP DUP2 => DUP1 DUP1 DUP1
(set-option :produce-proofs true)
; 
(set-info :status unknown)
(declare-fun storage_t ((_ BitVec 256) (_ BitVec 256) Int (_ BitVec 256)) (_ BitVec 256))
(declare-fun w_0 () (_ BitVec 256))
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
 (exists ((x_0 (_ BitVec 256)) )(let (($x9529 (forall ((w (_ BitVec 256)) )(let ((?x10330 (storage_t x_0 w_0 3 w)))
 (let ((?x4441 (storage_s x_0 w_0 6 w)))
 (= ?x4441 ?x10330))))
 ))
 (let (($x3614 (exc_halt_t 3)))
 (let (($x772 (exc_halt_s 6)))
 (let (($x4434 (= $x772 $x3614)))
 (let (($x5745 (forall ((n (_ BitVec 6)) )(let ((?x7 (stack_t x_0 w_0 3 n)))
 (let ((?x8234 (stack_s x_0 w_0 6 n)))
 (let (($x8276 (= ?x8234 ?x7)))
 (let ((?x11304 (sc_t 3)))
 (let (($x5590 (bvsle ?x11304 n)))
 (or $x5590 $x8276)))))))
 ))
 (let ((?x11304 (sc_t 3)))
 (let ((?x926 (sc_s 6)))
 (let (($x10270 (= ?x926 ?x11304)))
 (let ((?x8238 (used_gas_t x_0 w_0 0)))
 (let ((?x10842 (used_gas_s x_0 w_0 0)))
 (let (($x11203 (= ?x10842 ?x8238)))
 (let (($x2163 (forall ((w (_ BitVec 256)) )(let ((?x10983 (storage_t x_0 w_0 0 w)))
 (let ((?x1449 (storage_s x_0 w_0 0 w)))
 (= ?x1449 ?x10983))))
 ))
 (let (($x56 (exc_halt_t 0)))
 (let (($x57 (exc_halt_s 0)))
 (let (($x58 (= $x57 $x56)))
 (let (($x6294 (forall ((n (_ BitVec 6)) )(let ((?x623 (stack_t x_0 w_0 0 n)))
 (let ((?x11114 (stack_s x_0 w_0 0 n)))
 (let (($x9229 (= ?x11114 ?x623)))
 (let ((?x63 (sc_t 0)))
 (let (($x6552 (bvsle ?x63 n)))
 (or $x6552 $x9229)))))))
 ))
 (let ((?x63 (sc_t 0)))
 (let ((?x72 (sc_s 0)))
 (let (($x73 (= ?x72 ?x63)))
 (let (($x9674 (exc_halt_t 2)))
 (let (($x8506 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_t 2)))) (_ bv0 1)))))
 (let (($x384 (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_t 2))))))
 (let (($x5482 (forall ((w (_ BitVec 256)) )(let ((?x6064 (storage_t x_0 w_0 2 w)))
 (let ((?x10330 (storage_t x_0 w_0 3 w)))
 (= ?x10330 ?x6064))))
 ))
 (let (($x10491 (forall ((n (_ BitVec 6)) )(let ((?x6158 (sc_t 2)))
 (let ((?x5173 (bvadd (_ bv63 6) ?x6158)))
 (let (($x3972 (bvsle ?x5173 n)))
 (or $x3972 (= (stack_t x_0 w_0 3 n) (stack_t x_0 w_0 2 n)))))))
 ))
 (let (($x2667 (= ?x11304 (bvadd (_ bv1 6) (sc_t 2)))))
 (let (($x2172 (= (used_gas_t x_0 w_0 3) (+ 3 (used_gas_t x_0 w_0 2)))))
 (let ((?x6158 (sc_t 2)))
 (let ((?x5173 (bvadd (_ bv63 6) ?x6158)))
 (let ((?x1131 (stack_t x_0 w_0 2 ?x5173)))
 (let (($x8377 (exc_halt_t 1)))
 (let (($x3336 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_t 1)))) (_ bv0 1)))))
 (let (($x4382 (= $x9674 (or $x3336 $x8377 (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_t 1))))))))
 (let (($x10130 (forall ((w (_ BitVec 256)) )(let ((?x182 (storage_t x_0 w_0 1 w)))
 (let ((?x6064 (storage_t x_0 w_0 2 w)))
 (= ?x6064 ?x182))))
 ))
 (let (($x6035 (forall ((n (_ BitVec 6)) )(let ((?x7154 (sc_t 1)))
 (let ((?x3400 (bvadd (_ bv63 6) ?x7154)))
 (let (($x9923 (bvsle ?x3400 n)))
 (or (= (stack_t x_0 w_0 2 n) (stack_t x_0 w_0 1 n)) $x9923)))))
 ))
 (let (($x3131 (= ?x6158 (bvadd (_ bv1 6) (sc_t 1)))))
 (let ((?x2923 (used_gas_t x_0 w_0 2)))
 (let ((?x7154 (sc_t 1)))
 (let ((?x3400 (bvadd (_ bv63 6) ?x7154)))
 (let ((?x2034 (stack_t x_0 w_0 1 ?x3400)))
 (let (($x9612 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x63))) (_ bv0 1)))))
 (let (($x2279 (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) ?x63)))))
 (let (($x7505 (= $x8377 (or $x56 $x2279 $x9612))))
 (let (($x3387 (forall ((w (_ BitVec 256)) )(let ((?x10983 (storage_t x_0 w_0 0 w)))
 (let ((?x182 (storage_t x_0 w_0 1 w)))
 (= ?x182 ?x10983))))
 ))
 (let (($x6724 (forall ((n (_ BitVec 6)) )(let ((?x63 (sc_t 0)))
 (let ((?x11871 (bvadd (_ bv63 6) ?x63)))
 (let (($x3708 (bvsle ?x11871 n)))
 (or $x3708 (= (stack_t x_0 w_0 1 n) (stack_t x_0 w_0 0 n)))))))
 ))
 (let (($x10097 (= ?x7154 (bvadd (_ bv1 6) ?x63))))
 (let ((?x11871 (bvadd (_ bv63 6) ?x63)))
 (let ((?x119 (stack_t x_0 w_0 0 ?x11871)))
 (let (($x11317 (exc_halt_s 5)))
 (let (($x8375 (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 5))))))
 (let (($x9378 (or (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 5)))) (_ bv0 1))) $x8375 $x11317)))
 (let (($x2857 (forall ((w (_ BitVec 256)) )(let ((?x7179 (storage_s x_0 w_0 5 w)))
 (let ((?x4441 (storage_s x_0 w_0 6 w)))
 (= ?x4441 ?x7179))))
 ))
 (let (($x9506 (forall ((n (_ BitVec 6)) )(let ((?x4319 (sc_s 5)))
 (let ((?x10863 (bvadd (_ bv62 6) ?x4319)))
 (let (($x3410 (bvsle ?x10863 n)))
 (or $x3410 (= (stack_s x_0 w_0 6 n) (stack_s x_0 w_0 5 n)))))))
 ))
 (let (($x2715 (= (used_gas_s x_0 w_0 6) (+ 3 (used_gas_s x_0 w_0 5)))))
 (let (($x11597 (= (stack_s x_0 w_0 6 (bvadd (_ bv63 6) (sc_s 5))) (stack_s x_0 w_0 5 (bvadd (_ bv63 6) (sc_s 5))))))
 (let ((?x4319 (sc_s 5)))
 (let ((?x10863 (bvadd (_ bv62 6) ?x4319)))
 (let ((?x11963 (stack_s x_0 w_0 5 ?x10863)))
 (let (($x5330 (= $x11317 (or (exc_halt_s 4) (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_s 4))))))))
 (let (($x2949 (forall ((w (_ BitVec 256)) )(let ((?x9150 (storage_s x_0 w_0 4 w)))
 (let ((?x7179 (storage_s x_0 w_0 5 w)))
 (= ?x7179 ?x9150))))
 ))
 (let (($x5429 (forall ((n (_ BitVec 6)) )(let ((?x4305 (sc_s 4)))
 (let ((?x8905 (bvadd (_ bv63 6) ?x4305)))
 (let (($x11638 (bvsle ?x8905 n)))
 (or (= (stack_s x_0 w_0 5 n) (stack_s x_0 w_0 4 n)) $x11638)))))
 ))
 (let ((?x4305 (sc_s 4)))
 (let ((?x8905 (bvadd (_ bv63 6) ?x4305)))
 (let (($x9903 (= ?x4319 ?x8905)))
 (let ((?x11291 (used_gas_s x_0 w_0 5)))
 (let (($x7172 (exc_halt_s 4)))
 (let (($x8001 (= $x7172 (or (exc_halt_s 3) (not (bvsle (_ bv0 6) (bvadd (_ bv61 6) (sc_s 3))))))))
 (let (($x10843 (forall ((w (_ BitVec 256)) )(let ((?x4390 (storage_s x_0 w_0 3 w)))
 (let ((?x9150 (storage_s x_0 w_0 4 w)))
 (= ?x9150 ?x4390))))
 ))
 (let (($x3167 (forall ((n (_ BitVec 6)) )(or (= (stack_s x_0 w_0 4 n) (stack_s x_0 w_0 3 n)) (bvsle (bvadd (_ bv61 6) (sc_s 3)) n)))
 ))
 (let ((?x275 (sc_s 3)))
 (let (($x5793 (= ?x4305 ?x275)))
 (let ((?x8382 (used_gas_s x_0 w_0 4)))
 (let (($x8391 (= (stack_s x_0 w_0 4 (bvadd (_ bv62 6) ?x4305)) (stack_s x_0 w_0 3 (bvadd (_ bv62 6) ?x275)))))
 (let ((?x3112 (bvadd (_ bv63 6) ?x275)))
 (let ((?x4215 (stack_s x_0 w_0 3 ?x3112)))
 (let (($x10762 (= (stack_s x_0 w_0 4 ?x8905) (stack_s x_0 w_0 3 (bvadd (_ bv61 6) ?x275)))))
 (let (($x1408 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 2)))) (_ bv0 1)))))
 (let (($x247 (exc_halt_s 2)))
 (let (($x292 (exc_halt_s 3)))
 (let (($x10893 (forall ((w (_ BitVec 256)) )(let ((?x3682 (storage_s x_0 w_0 2 w)))
 (let ((?x4390 (storage_s x_0 w_0 3 w)))
 (= ?x4390 ?x3682))))
 ))
 (let (($x1061 (forall ((n (_ BitVec 6)) )(or (bvsle (bvadd (_ bv61 6) (sc_s 2)) n) (= (stack_s x_0 w_0 3 n) (stack_s x_0 w_0 2 n))))
 ))
 (let (($x799 (= ?x275 (bvadd (_ bv1 6) (sc_s 2)))))
 (let ((?x8818 (used_gas_s x_0 w_0 3)))
 (let ((?x218 (sc_s 2)))
 (let ((?x9412 (bvadd (_ bv63 6) ?x218)))
 (let ((?x2776 (stack_s x_0 w_0 2 ?x9412)))
 (let (($x9666 (= (stack_s x_0 w_0 3 (bvadd (_ bv62 6) ?x218)) (stack_s x_0 w_0 2 (bvadd (_ bv62 6) ?x218)))))
 (let ((?x8710 (bvadd (_ bv61 6) ?x218)))
 (let ((?x3795 (stack_s x_0 w_0 2 ?x8710)))
 (let (($x10000 (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 1))))))
 (let (($x8084 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 1)))) (_ bv0 1)))))
 (let (($x189 (exc_halt_s 1)))
 (let (($x6422 (= $x247 (or $x189 $x8084 $x10000))))
 (let (($x5487 (forall ((w (_ BitVec 256)) )(let ((?x4309 (storage_s x_0 w_0 1 w)))
 (let ((?x3682 (storage_s x_0 w_0 2 w)))
 (= ?x3682 ?x4309))))
 ))
 (let (($x10140 (forall ((n (_ BitVec 6)) )(let ((?x154 (sc_s 1)))
 (let ((?x9977 (bvadd (_ bv62 6) ?x154)))
 (let (($x3591 (bvsle ?x9977 n)))
 (or $x3591 (= (stack_s x_0 w_0 2 n) (stack_s x_0 w_0 1 n)))))))
 ))
 (let (($x7629 (= ?x218 (bvadd (_ bv1 6) (sc_s 1)))))
 (let ((?x1495 (used_gas_s x_0 w_0 2)))
 (let (($x4630 (= (stack_s x_0 w_0 2 (bvadd (_ bv63 6) (sc_s 1))) (stack_s x_0 w_0 1 (bvadd (_ bv63 6) (sc_s 1))))))
 (let ((?x154 (sc_s 1)))
 (let ((?x9977 (bvadd (_ bv62 6) ?x154)))
 (let ((?x2573 (stack_s x_0 w_0 1 ?x9977)))
 (let (($x1616 (= $x189 (or $x57 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x72))) (_ bv0 1)))))))
 (let (($x8304 (forall ((w (_ BitVec 256)) )(let ((?x1449 (storage_s x_0 w_0 0 w)))
 (let ((?x4309 (storage_s x_0 w_0 1 w)))
 (= ?x4309 ?x1449))))
 ))
 (let (($x10529 (forall ((n (_ BitVec 6)) )(let ((?x72 (sc_s 0)))
 (let (($x2753 (bvsle ?x72 n)))
 (or $x2753 (= (stack_s x_0 w_0 1 n) (stack_s x_0 w_0 0 n))))))
 ))
 (let (($x11322 (= ?x154 (bvadd (_ bv1 6) ?x72))))
 (let (($x6472 (forall ((w (_ BitVec 256)) )(let ((?x1449 (storage_s x_0 w_0 0 w)))
 (= ?x1449 (_ bv0 256))))
 ))
 (let (($x1952 (= ?x10842 0)))
 (let (($x7449 (not $x57)))
 (let (($x4186 (= (stack_s x_0 w_0 0 (_ bv0 6)) x_0)))
 (let (($x7315 (= ?x72 (_ bv1 6))))
 (and $x7315 $x4186 $x7449 $x1952 $x6472 (= (stack_s x_0 w_0 1 ?x72) w_0) (= (used_gas_s x_0 w_0 1) (+ 3 ?x10842)) $x11322 $x10529 $x8304 $x1616 (= ?x2776 ?x2573) (= (stack_s x_0 w_0 2 ?x9977) ?x2573) $x4630 (= ?x1495 (+ 3 (used_gas_s x_0 w_0 1))) $x7629 $x10140 $x5487 $x6422 (= ?x4215 ?x3795) (= (stack_s x_0 w_0 3 ?x8710) ?x3795) $x9666 (= (stack_s x_0 w_0 3 ?x9412) ?x2776) (= ?x8818 (+ 3 ?x1495)) $x799 $x1061 $x10893 (= $x292 (or $x247 $x1408 (not (bvsle (_ bv0 6) ?x8710)))) $x10762 (= (stack_s x_0 w_0 4 (bvadd (_ bv61 6) ?x4305)) ?x4215) $x8391 (= ?x8382 (+ 3 ?x8818)) $x5793 $x3167 $x10843 $x8001 (= ?x11291 (+ 2 ?x8382)) $x9903 $x5429 $x2949 $x5330 (= (stack_s x_0 w_0 6 (bvadd (_ bv63 6) ?x926)) ?x11963) (= (stack_s x_0 w_0 6 ?x10863) ?x11963) $x11597 $x2715 (= ?x926 (bvadd (_ bv1 6) ?x4319)) $x9506 $x2857 (= $x772 $x9378) (= ?x2034 ?x119) (= (stack_t x_0 w_0 1 ?x11871) ?x119) (= (used_gas_t x_0 w_0 1) (+ 3 ?x8238)) $x10097 $x6724 $x3387 $x7505 (= ?x1131 ?x2034) (= (stack_t x_0 w_0 2 ?x3400) ?x2034) (= ?x2923 (+ 3 (used_gas_t x_0 w_0 1))) $x3131 $x6035 $x10130 $x4382 (= (stack_t x_0 w_0 3 (bvadd (_ bv63 6) ?x11304)) ?x1131) (= (stack_t x_0 w_0 3 ?x5173) ?x1131) $x2172 $x2667 $x10491 $x5482 (= $x3614 (or $x384 $x8506 $x9674)) $x73 $x6294 $x58 $x2163 $x11203 (not (and $x10270 $x5745 $x4434 $x9529)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
 )
(check-sat)
(get-proof)
