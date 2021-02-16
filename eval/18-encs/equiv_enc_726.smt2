; SWAP2 POP PUSH 0x00 DUP3 GT ISZERO => DUP1 SWAP3 POP ISZERO
(set-option :produce-proofs true)
; 
(set-info :status unknown)
(declare-fun storage_t ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 256)) (_ BitVec 256))
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
 (exists ((x_0 (_ BitVec 256)) (x_1 (_ BitVec 256)) (x_2 (_ BitVec 256)) )(let (($x7147 (forall ((w (_ BitVec 256)) )(let ((?x3696 (storage_t x_0 x_1 x_2 4 w)))
 (let ((?x8748 (storage_s x_0 x_1 x_2 6 w)))
 (= ?x8748 ?x3696))))
 ))
 (let (($x7854 (exc_halt_t 4)))
 (let (($x772 (exc_halt_s 6)))
 (let (($x4404 (= $x772 $x7854)))
 (let (($x1588 (forall ((n (_ BitVec 6)) )(let ((?x389 (stack_t x_0 x_1 x_2 4 n)))
 (let ((?x1141 (stack_s x_0 x_1 x_2 6 n)))
 (let (($x6855 (= ?x1141 ?x389)))
 (or $x6855 (bvsle (sc_t 4) n))))))
 ))
 (let ((?x10468 (sc_t 4)))
 (let ((?x926 (sc_s 6)))
 (let (($x5134 (= ?x926 ?x10468)))
 (let ((?x7722 (used_gas_t x_0 x_1 x_2 0)))
 (let ((?x3260 (used_gas_s x_0 x_1 x_2 0)))
 (let (($x2260 (= ?x3260 ?x7722)))
 (let (($x7325 (forall ((w (_ BitVec 256)) )(let ((?x2207 (storage_t x_0 x_1 x_2 0 w)))
 (let ((?x6597 (storage_s x_0 x_1 x_2 0 w)))
 (= ?x6597 ?x2207))))
 ))
 (let (($x56 (exc_halt_t 0)))
 (let (($x57 (exc_halt_s 0)))
 (let (($x58 (= $x57 $x56)))
 (let (($x410 (forall ((n (_ BitVec 6)) )(let ((?x63 (sc_t 0)))
 (let (($x11230 (bvsle ?x63 n)))
 (let ((?x4010 (stack_t x_0 x_1 x_2 0 n)))
 (let ((?x7935 (stack_s x_0 x_1 x_2 0 n)))
 (let (($x6680 (= ?x7935 ?x4010)))
 (or $x6680 $x11230)))))))
 ))
 (let ((?x63 (sc_t 0)))
 (let ((?x72 (sc_s 0)))
 (let (($x73 (= ?x72 ?x63)))
 (let (($x11502 (= $x7854 (or (exc_halt_t 3) (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_t 3))))))))
 (let (($x11656 (forall ((w (_ BitVec 256)) )(let ((?x3845 (storage_t x_0 x_1 x_2 3 w)))
 (let ((?x3696 (storage_t x_0 x_1 x_2 4 w)))
 (= ?x3696 ?x3845))))
 ))
 (let (($x371 (forall ((n (_ BitVec 6)) )(let ((?x11964 (sc_t 3)))
 (let ((?x1119 (bvadd (_ bv63 6) ?x11964)))
 (let (($x8570 (bvsle ?x1119 n)))
 (let ((?x5638 (stack_t x_0 x_1 x_2 3 n)))
 (let ((?x389 (stack_t x_0 x_1 x_2 4 n)))
 (or (= ?x389 ?x5638) $x8570)))))))
 ))
 (let (($x6252 (= (used_gas_t x_0 x_1 x_2 4) (+ 3 (used_gas_t x_0 x_1 x_2 3)))))
 (let ((?x599 (ite (= (stack_t x_0 x_1 x_2 3 (bvadd (_ bv63 6) (sc_t 3))) (_ bv0 256)) (_ bv1 256) (_ bv0 256))))
 (let (($x4112 (exc_halt_t 3)))
 (let (($x8175 (= $x4112 (or (exc_halt_t 2) (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_t 2))))))))
 (let (($x4843 (forall ((w (_ BitVec 256)) )(let ((?x5514 (storage_t x_0 x_1 x_2 2 w)))
 (let ((?x3845 (storage_t x_0 x_1 x_2 3 w)))
 (= ?x3845 ?x5514))))
 ))
 (let (($x8997 (forall ((n (_ BitVec 6)) )(let ((?x5269 (sc_t 2)))
 (let ((?x4748 (bvadd (_ bv63 6) ?x5269)))
 (let (($x10894 (bvsle ?x4748 n)))
 (let ((?x11297 (stack_t x_0 x_1 x_2 2 n)))
 (let ((?x5638 (stack_t x_0 x_1 x_2 3 n)))
 (or (= ?x5638 ?x11297) $x10894)))))))
 ))
 (let ((?x11119 (used_gas_t x_0 x_1 x_2 3)))
 (let (($x4675 (exc_halt_t 2)))
 (let (($x3439 (= $x4675 (or (exc_halt_t 1) (not (bvsle (_ bv0 6) (bvadd (_ bv60 6) (sc_t 1))))))))
 (let (($x2822 (forall ((w (_ BitVec 256)) )(let ((?x11350 (storage_t x_0 x_1 x_2 1 w)))
 (let ((?x5514 (storage_t x_0 x_1 x_2 2 w)))
 (= ?x5514 ?x11350))))
 ))
 (let (($x1637 (forall ((n (_ BitVec 6)) )(let ((?x10046 (stack_t x_0 x_1 x_2 1 n)))
 (let ((?x11297 (stack_t x_0 x_1 x_2 2 n)))
 (or (= ?x11297 ?x10046) (bvsle (bvadd (_ bv60 6) (sc_t 1)) n)))))
 ))
 (let ((?x7154 (sc_t 1)))
 (let ((?x5269 (sc_t 2)))
 (let (($x3126 (= ?x5269 ?x7154)))
 (let ((?x9555 (used_gas_t x_0 x_1 x_2 2)))
 (let (($x3666 (= (stack_t x_0 x_1 x_2 2 (bvadd (_ bv62 6) ?x5269)) (stack_t x_0 x_1 x_2 1 (bvadd (_ bv62 6) ?x7154)))))
 (let (($x8063 (= (stack_t x_0 x_1 x_2 2 (bvadd (_ bv61 6) ?x5269)) (stack_t x_0 x_1 x_2 1 (bvadd (_ bv61 6) ?x7154)))))
 (let ((?x1746 (bvadd (_ bv63 6) ?x7154)))
 (let ((?x6149 (stack_t x_0 x_1 x_2 1 ?x1746)))
 (let (($x3721 (= (stack_t x_0 x_1 x_2 2 (bvadd (_ bv63 6) ?x5269)) (stack_t x_0 x_1 x_2 1 (bvadd (_ bv60 6) ?x7154)))))
 (let (($x3554 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x63))) (_ bv0 1)))))
 (let (($x1423 (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) ?x63)))))
 (let (($x8377 (exc_halt_t 1)))
 (let (($x5086 (forall ((w (_ BitVec 256)) )(let ((?x2207 (storage_t x_0 x_1 x_2 0 w)))
 (let ((?x11350 (storage_t x_0 x_1 x_2 1 w)))
 (= ?x11350 ?x2207))))
 ))
 (let (($x9228 (forall ((n (_ BitVec 6)) )(let ((?x4010 (stack_t x_0 x_1 x_2 0 n)))
 (let ((?x10046 (stack_t x_0 x_1 x_2 1 n)))
 (let ((?x63 (sc_t 0)))
 (let ((?x10160 (bvadd (_ bv63 6) ?x63)))
 (let (($x785 (bvsle ?x10160 n)))
 (or $x785 (= ?x10046 ?x4010))))))))
 ))
 (let (($x688 (= ?x7154 (bvadd (_ bv1 6) ?x63))))
 (let ((?x10160 (bvadd (_ bv63 6) ?x63)))
 (let ((?x5465 (stack_t x_0 x_1 x_2 0 ?x10160)))
 (let (($x2138 (= $x772 (or (exc_halt_s 5) (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_s 5))))))))
 (let (($x11234 (forall ((w (_ BitVec 256)) )(let ((?x3717 (storage_s x_0 x_1 x_2 5 w)))
 (let ((?x8748 (storage_s x_0 x_1 x_2 6 w)))
 (= ?x8748 ?x3717))))
 ))
 (let (($x6445 (forall ((n (_ BitVec 6)) )(let ((?x700 (stack_s x_0 x_1 x_2 5 n)))
 (let ((?x1141 (stack_s x_0 x_1 x_2 6 n)))
 (or (bvsle (bvadd (_ bv63 6) (sc_s 5)) n) (= ?x1141 ?x700)))))
 ))
 (let ((?x4319 (sc_s 5)))
 (let (($x7923 (= ?x926 ?x4319)))
 (let (($x11474 (= (used_gas_s x_0 x_1 x_2 6) (+ 3 (used_gas_s x_0 x_1 x_2 5)))))
 (let ((?x490 (ite (= (stack_s x_0 x_1 x_2 5 (bvadd (_ bv63 6) ?x4319)) (_ bv0 256)) (_ bv1 256) (_ bv0 256))))
 (let (($x1862 (exc_halt_s 5)))
 (let (($x7458 (= $x1862 (or (exc_halt_s 4) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 4))))))))
 (let (($x4274 (forall ((w (_ BitVec 256)) )(let ((?x10575 (storage_s x_0 x_1 x_2 4 w)))
 (let ((?x3717 (storage_s x_0 x_1 x_2 5 w)))
 (= ?x3717 ?x10575))))
 ))
 (let (($x2610 (forall ((n (_ BitVec 6)) )(let ((?x4305 (sc_s 4)))
 (let ((?x5150 (bvadd (_ bv62 6) ?x4305)))
 (let (($x6839 (bvsle ?x5150 n)))
 (let ((?x8111 (stack_s x_0 x_1 x_2 4 n)))
 (let ((?x700 (stack_s x_0 x_1 x_2 5 n)))
 (or (= ?x700 ?x8111) $x6839)))))))
 ))
 (let ((?x1838 (used_gas_s x_0 x_1 x_2 5)))
 (let ((?x4305 (sc_s 4)))
 (let ((?x10368 (bvadd (_ bv63 6) ?x4305)))
 (let ((?x8827 (stack_s x_0 x_1 x_2 4 ?x10368)))
 (let ((?x2584 (ite (bvule ?x8827 (stack_s x_0 x_1 x_2 4 (bvadd (_ bv62 6) ?x4305))) (_ bv0 256) (_ bv1 256))))
 (let ((?x6605 (bvadd (_ bv63 6) ?x4319)))
 (let ((?x5377 (stack_s x_0 x_1 x_2 5 ?x6605)))
 (let (($x6338 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 3)))) (_ bv0 1)))))
 (let (($x292 (exc_halt_s 3)))
 (let (($x7172 (exc_halt_s 4)))
 (let (($x11858 (forall ((w (_ BitVec 256)) )(let ((?x2479 (storage_s x_0 x_1 x_2 3 w)))
 (let ((?x10575 (storage_s x_0 x_1 x_2 4 w)))
 (= ?x10575 ?x2479))))
 ))
 (let (($x2931 (forall ((n (_ BitVec 6)) )(let ((?x7043 (stack_s x_0 x_1 x_2 3 n)))
 (let ((?x8111 (stack_s x_0 x_1 x_2 4 n)))
 (or (bvsle (bvadd (_ bv61 6) (sc_s 3)) n) (= ?x8111 ?x7043)))))
 ))
 (let (($x11214 (= ?x4305 (bvadd (_ bv1 6) (sc_s 3)))))
 (let ((?x7055 (used_gas_s x_0 x_1 x_2 4)))
 (let (($x2297 (= (stack_s x_0 x_1 x_2 4 (bvadd (_ bv63 6) (sc_s 3))) (stack_s x_0 x_1 x_2 3 (bvadd (_ bv63 6) (sc_s 3))))))
 (let (($x1667 (= (stack_s x_0 x_1 x_2 4 (bvadd (_ bv62 6) (sc_s 3))) (stack_s x_0 x_1 x_2 3 (bvadd (_ bv62 6) (sc_s 3))))))
 (let ((?x275 (sc_s 3)))
 (let ((?x2689 (bvadd (_ bv61 6) ?x275)))
 (let ((?x144 (stack_s x_0 x_1 x_2 3 ?x2689)))
 (let (($x2376 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 2)))) (_ bv0 1)))))
 (let (($x247 (exc_halt_s 2)))
 (let (($x3156 (= $x292 (or $x247 $x2376))))
 (let (($x10484 (forall ((w (_ BitVec 256)) )(let ((?x11058 (storage_s x_0 x_1 x_2 2 w)))
 (let ((?x2479 (storage_s x_0 x_1 x_2 3 w)))
 (= ?x2479 ?x11058))))
 ))
 (let (($x11593 (forall ((n (_ BitVec 6)) )(let ((?x5495 (stack_s x_0 x_1 x_2 2 n)))
 (let ((?x7043 (stack_s x_0 x_1 x_2 3 n)))
 (let ((?x218 (sc_s 2)))
 (let (($x1340 (bvsle ?x218 n)))
 (or $x1340 (= ?x7043 ?x5495)))))))
 ))
 (let (($x7581 (= ?x275 (bvadd (_ bv1 6) (sc_s 2)))))
 (let ((?x5241 (used_gas_s x_0 x_1 x_2 3)))
 (let (($x1657 (= $x247 (or (exc_halt_s 1) (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_s 1))))))))
 (let (($x3880 (forall ((w (_ BitVec 256)) )(let ((?x10090 (storage_s x_0 x_1 x_2 1 w)))
 (let ((?x11058 (storage_s x_0 x_1 x_2 2 w)))
 (= ?x11058 ?x10090))))
 ))
 (let (($x3115 (forall ((n (_ BitVec 6)) )(let ((?x154 (sc_s 1)))
 (let ((?x3077 (bvadd (_ bv63 6) ?x154)))
 (let (($x8034 (bvsle ?x3077 n)))
 (let ((?x11883 (stack_s x_0 x_1 x_2 1 n)))
 (let ((?x5495 (stack_s x_0 x_1 x_2 2 n)))
 (or (= ?x5495 ?x11883) $x8034)))))))
 ))
 (let ((?x10568 (used_gas_s x_0 x_1 x_2 2)))
 (let (($x189 (exc_halt_s 1)))
 (let (($x4868 (= $x189 (or $x57 (not (bvsle (_ bv0 6) (bvadd (_ bv61 6) ?x72)))))))
 (let (($x2012 (forall ((w (_ BitVec 256)) )(let ((?x6597 (storage_s x_0 x_1 x_2 0 w)))
 (let ((?x10090 (storage_s x_0 x_1 x_2 1 w)))
 (= ?x10090 ?x6597))))
 ))
 (let (($x5387 (forall ((n (_ BitVec 6)) )(let ((?x72 (sc_s 0)))
 (let ((?x6584 (bvadd (_ bv61 6) ?x72)))
 (let (($x11469 (bvsle ?x6584 n)))
 (let ((?x7935 (stack_s x_0 x_1 x_2 0 n)))
 (let ((?x11883 (stack_s x_0 x_1 x_2 1 n)))
 (or (= ?x11883 ?x7935) $x11469)))))))
 ))
 (let ((?x154 (sc_s 1)))
 (let (($x6077 (= ?x154 ?x72)))
 (let (($x10375 (= (stack_s x_0 x_1 x_2 1 (bvadd (_ bv62 6) ?x154)) (stack_s x_0 x_1 x_2 0 (bvadd (_ bv62 6) ?x72)))))
 (let (($x1134 (= (stack_s x_0 x_1 x_2 1 (bvadd (_ bv61 6) ?x154)) (stack_s x_0 x_1 x_2 0 (bvadd (_ bv63 6) ?x72)))))
 (let (($x9314 (= (stack_s x_0 x_1 x_2 1 (bvadd (_ bv63 6) ?x154)) (stack_s x_0 x_1 x_2 0 (bvadd (_ bv61 6) ?x72)))))
 (let (($x3912 (forall ((w (_ BitVec 256)) )(let ((?x6597 (storage_s x_0 x_1 x_2 0 w)))
 (= ?x6597 (_ bv0 256))))
 ))
 (let (($x7568 (= ?x3260 0)))
 (let (($x5230 (not $x57)))
 (let (($x1629 (= (stack_s x_0 x_1 x_2 0 (_ bv2 6)) x_2)))
 (let (($x4428 (= (stack_s x_0 x_1 x_2 0 (_ bv1 6)) x_1)))
 (let (($x6176 (= (stack_s x_0 x_1 x_2 0 (_ bv0 6)) x_0)))
 (let (($x11425 (= ?x72 (_ bv3 6))))
 (and $x11425 $x6176 $x4428 $x1629 $x5230 $x7568 $x3912 $x9314 $x1134 $x10375 (= (used_gas_s x_0 x_1 x_2 1) (+ 3 ?x3260)) $x6077 $x5387 $x2012 $x4868 (= ?x10568 (+ 2 (used_gas_s x_0 x_1 x_2 1))) (= (sc_s 2) (bvadd (_ bv63 6) ?x154)) $x3115 $x3880 $x1657 (= (stack_s x_0 x_1 x_2 3 (sc_s 2)) (_ bv0 256)) (= ?x5241 (+ 3 ?x10568)) $x7581 $x11593 $x10484 $x3156 (= ?x8827 ?x144) (= (stack_s x_0 x_1 x_2 4 ?x2689) ?x144) $x1667 $x2297 (= ?x7055 (+ 3 ?x5241)) $x11214 $x2931 $x11858 (= $x7172 (or $x292 (not (bvsle (_ bv0 6) ?x2689)) $x6338)) (= ?x5377 ?x2584) (= ?x1838 (+ 3 ?x7055)) (= ?x4319 ?x10368) $x2610 $x4274 $x7458 (= (stack_s x_0 x_1 x_2 6 (bvadd (_ bv63 6) ?x926)) ?x490) $x11474 $x7923 $x6445 $x11234 $x2138 (= ?x6149 ?x5465) (= (stack_t x_0 x_1 x_2 1 ?x10160) ?x5465) (= (used_gas_t x_0 x_1 x_2 1) (+ 3 ?x7722)) $x688 $x9228 $x5086 (= $x8377 (or $x56 $x1423 $x3554)) $x3721 (= (stack_t x_0 x_1 x_2 2 (bvadd (_ bv60 6) ?x5269)) ?x6149) $x8063 $x3666 (= ?x9555 (+ 3 (used_gas_t x_0 x_1 x_2 1))) $x3126 $x1637 $x2822 $x3439 (= ?x11119 (+ 2 ?x9555)) (= (sc_t 3) (bvadd (_ bv63 6) ?x5269)) $x8997 $x4843 $x8175 (= (stack_t x_0 x_1 x_2 4 (bvadd (_ bv63 6) ?x10468)) ?x599) $x6252 (= ?x10468 (sc_t 3)) $x371 $x11656 $x11502 $x73 $x410 $x58 $x7325 $x2260 (not (and $x5134 $x1588 $x4404 $x7147)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
 )
(check-sat)
(get-proof)