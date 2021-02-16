; PUSH cw_5 DUP1 PUSH cw_6 PUSH 0x3b92 PUSH 0xffff AND => PUSH cw_5 PUSH cw_5 PUSH cw_6 PUSH 0x3b92
(set-option :produce-proofs true)
; 
(set-info :status unknown)
(declare-fun storage_t ((_ BitVec 256) (_ BitVec 256) Int (_ BitVec 256)) (_ BitVec 256))
(declare-fun w_6 () (_ BitVec 256))
(declare-fun w_5 () (_ BitVec 256))
(declare-fun storage_s ((_ BitVec 256) (_ BitVec 256) Int (_ BitVec 256)) (_ BitVec 256))
(declare-fun exc_halt_t (Int) Bool)
(declare-fun exc_halt_s (Int) Bool)
(declare-fun sc_t (Int) (_ BitVec 6))
(declare-fun stack_t ((_ BitVec 256) (_ BitVec 256) Int (_ BitVec 6)) (_ BitVec 256))
(declare-fun stack_s ((_ BitVec 256) (_ BitVec 256) Int (_ BitVec 6)) (_ BitVec 256))
(declare-fun sc_s (Int) (_ BitVec 6))
(declare-fun used_gas_t ((_ BitVec 256) (_ BitVec 256) Int) Int)
(declare-fun used_gas_s ((_ BitVec 256) (_ BitVec 256) Int) Int)
(assert
 (let (($x6238 (forall ((w (_ BitVec 256)) )(let ((?x3397 (storage_t w_5 w_6 4 w)))
 (let ((?x10137 (storage_s w_5 w_6 6 w)))
 (= ?x10137 ?x3397))))
 ))
 (let (($x7854 (exc_halt_t 4)))
 (let (($x772 (exc_halt_s 6)))
 (let (($x4404 (= $x772 $x7854)))
 (let (($x2723 (forall ((n (_ BitVec 6)) )(let ((?x9509 (stack_t w_5 w_6 4 n)))
 (let ((?x3960 (stack_s w_5 w_6 6 n)))
 (let (($x9815 (= ?x3960 ?x9509)))
 (or $x9815 (bvsle (sc_t 4) n))))))
 ))
 (let ((?x4818 (sc_t 4)))
 (let ((?x926 (sc_s 6)))
 (let (($x7899 (= ?x926 ?x4818)))
 (let ((?x1883 (used_gas_t w_5 w_6 0)))
 (let ((?x2502 (used_gas_s w_5 w_6 0)))
 (let (($x915 (= ?x2502 ?x1883)))
 (let (($x1195 (forall ((w (_ BitVec 256)) )(let ((?x4259 (storage_t w_5 w_6 0 w)))
 (let ((?x364 (storage_s w_5 w_6 0 w)))
 (= ?x364 ?x4259))))
 ))
 (let (($x56 (exc_halt_t 0)))
 (let (($x57 (exc_halt_s 0)))
 (let (($x58 (= $x57 $x56)))
 (let (($x6271 (forall ((n (_ BitVec 6)) )(let ((?x7434 (stack_t w_5 w_6 0 n)))
 (let ((?x11401 (stack_s w_5 w_6 0 n)))
 (let (($x9444 (= ?x11401 ?x7434)))
 (let ((?x63 (sc_t 0)))
 (let (($x2617 (bvsle ?x63 n)))
 (or $x2617 $x9444)))))))
 ))
 (let ((?x63 (sc_t 0)))
 (let ((?x72 (sc_s 0)))
 (let (($x73 (= ?x72 ?x63)))
 (let (($x4755 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_t 3)))) (_ bv0 1)))))
 (let (($x6783 (exc_halt_t 3)))
 (let (($x4902 (forall ((w (_ BitVec 256)) )(let ((?x1539 (storage_t w_5 w_6 3 w)))
 (let ((?x3397 (storage_t w_5 w_6 4 w)))
 (= ?x3397 ?x1539))))
 ))
 (let (($x11320 (forall ((n (_ BitVec 6)) )(let ((?x6438 (sc_t 3)))
 (let (($x5278 (bvsle ?x6438 n)))
 (or $x5278 (= (stack_t w_5 w_6 4 n) (stack_t w_5 w_6 3 n))))))
 ))
 (let (($x6479 (= ?x4818 (bvadd (_ bv1 6) (sc_t 3)))))
 (let (($x5215 (= (used_gas_t w_5 w_6 4) (+ 3 (used_gas_t w_5 w_6 3)))))
 (let (($x2163 (exc_halt_t 2)))
 (let (($x5871 (or $x2163 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_t 2)))) (_ bv0 1))))))
 (let (($x6807 (= $x6783 $x5871)))
 (let (($x7679 (forall ((w (_ BitVec 256)) )(let ((?x925 (storage_t w_5 w_6 2 w)))
 (let ((?x1539 (storage_t w_5 w_6 3 w)))
 (= ?x1539 ?x925))))
 ))
 (let (($x11832 (forall ((n (_ BitVec 6)) )(let ((?x2714 (sc_t 2)))
 (let (($x1155 (bvsle ?x2714 n)))
 (or $x1155 (= (stack_t w_5 w_6 3 n) (stack_t w_5 w_6 2 n))))))
 ))
 (let ((?x6438 (sc_t 3)))
 (let (($x1004 (= ?x6438 (bvadd (_ bv1 6) (sc_t 2)))))
 (let ((?x10194 (used_gas_t w_5 w_6 3)))
 (let (($x9864 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_t 1)))) (_ bv0 1)))))
 (let (($x8377 (exc_halt_t 1)))
 (let (($x10265 (forall ((w (_ BitVec 256)) )(let ((?x11115 (storage_t w_5 w_6 1 w)))
 (let ((?x925 (storage_t w_5 w_6 2 w)))
 (= ?x925 ?x11115))))
 ))
 (let (($x2801 (forall ((n (_ BitVec 6)) )(or (= (stack_t w_5 w_6 2 n) (stack_t w_5 w_6 1 n)) (bvsle (sc_t 1) n)))
 ))
 (let ((?x2714 (sc_t 2)))
 (let (($x3007 (= ?x2714 (bvadd (_ bv1 6) (sc_t 1)))))
 (let ((?x8773 (used_gas_t w_5 w_6 2)))
 (let (($x10110 (= $x8377 (or $x56 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x63))) (_ bv0 1)))))))
 (let (($x11097 (forall ((w (_ BitVec 256)) )(let ((?x4259 (storage_t w_5 w_6 0 w)))
 (let ((?x11115 (storage_t w_5 w_6 1 w)))
 (= ?x11115 ?x4259))))
 ))
 (let (($x10969 (forall ((n (_ BitVec 6)) )(let ((?x63 (sc_t 0)))
 (let (($x2617 (bvsle ?x63 n)))
 (or $x2617 (= (stack_t w_5 w_6 1 n) (stack_t w_5 w_6 0 n))))))
 ))
 (let ((?x7154 (sc_t 1)))
 (let (($x1385 (= ?x7154 (bvadd (_ bv1 6) ?x63))))
 (let (($x11468 (= $x772 (or (exc_halt_s 5) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 5))))))))
 (let (($x2518 (forall ((w (_ BitVec 256)) )(let ((?x2396 (storage_s w_5 w_6 5 w)))
 (let ((?x10137 (storage_s w_5 w_6 6 w)))
 (= ?x10137 ?x2396))))
 ))
 (let (($x552 (forall ((n (_ BitVec 6)) )(or (= (stack_s w_5 w_6 6 n) (stack_s w_5 w_6 5 n)) (bvsle (bvadd (_ bv62 6) (sc_s 5)) n)))
 ))
 (let (($x6794 (= (used_gas_s w_5 w_6 6) (+ 3 (used_gas_s w_5 w_6 5)))))
 (let ((?x7829 (bvor (bvnot (stack_s w_5 w_6 5 (bvadd (_ bv63 6) (sc_s 5)))) (bvnot (stack_s w_5 w_6 5 (bvadd (_ bv62 6) (sc_s 5)))))))
 (let (($x7172 (exc_halt_s 4)))
 (let (($x4746 (or $x7172 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 4)))) (_ bv0 1))))))
 (let (($x11317 (exc_halt_s 5)))
 (let (($x7390 (= $x11317 $x4746)))
 (let (($x3625 (forall ((w (_ BitVec 256)) )(let ((?x7620 (storage_s w_5 w_6 4 w)))
 (let ((?x2396 (storage_s w_5 w_6 5 w)))
 (= ?x2396 ?x7620))))
 ))
 (let (($x8934 (forall ((n (_ BitVec 6)) )(let ((?x4305 (sc_s 4)))
 (let (($x4047 (bvsle ?x4305 n)))
 (or $x4047 (= (stack_s w_5 w_6 5 n) (stack_s w_5 w_6 4 n))))))
 ))
 (let ((?x4319 (sc_s 5)))
 (let (($x9414 (= ?x4319 (bvadd (_ bv1 6) (sc_s 4)))))
 (let ((?x10367 (used_gas_s w_5 w_6 5)))
 (let (($x292 (exc_halt_s 3)))
 (let (($x4077 (or $x292 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 3)))) (_ bv0 1))))))
 (let (($x6234 (= $x7172 $x4077)))
 (let (($x380 (forall ((w (_ BitVec 256)) )(let ((?x9552 (storage_s w_5 w_6 3 w)))
 (let ((?x7620 (storage_s w_5 w_6 4 w)))
 (= ?x7620 ?x9552))))
 ))
 (let (($x1714 (forall ((n (_ BitVec 6)) )(let ((?x275 (sc_s 3)))
 (let (($x9133 (bvsle ?x275 n)))
 (or (= (stack_s w_5 w_6 4 n) (stack_s w_5 w_6 3 n)) $x9133))))
 ))
 (let ((?x4305 (sc_s 4)))
 (let (($x8307 (= ?x4305 (bvadd (_ bv1 6) (sc_s 3)))))
 (let ((?x11424 (used_gas_s w_5 w_6 4)))
 (let (($x4387 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 2)))) (_ bv0 1)))))
 (let (($x247 (exc_halt_s 2)))
 (let (($x3263 (forall ((w (_ BitVec 256)) )(let ((?x55 (storage_s w_5 w_6 2 w)))
 (let ((?x9552 (storage_s w_5 w_6 3 w)))
 (= ?x9552 ?x55))))
 ))
 (let (($x1930 (forall ((n (_ BitVec 6)) )(or (bvsle (sc_s 2) n) (= (stack_s w_5 w_6 3 n) (stack_s w_5 w_6 2 n))))
 ))
 (let ((?x275 (sc_s 3)))
 (let (($x5768 (= ?x275 (bvadd (_ bv1 6) (sc_s 2)))))
 (let ((?x59 (used_gas_s w_5 w_6 3)))
 (let (($x5753 (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_s 1))))))
 (let (($x189 (exc_halt_s 1)))
 (let (($x2585 (or $x189 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 1)))) (_ bv0 1))) $x5753)))
 (let (($x4374 (= $x247 $x2585)))
 (let (($x8057 (forall ((w (_ BitVec 256)) )(let ((?x3651 (storage_s w_5 w_6 1 w)))
 (let ((?x55 (storage_s w_5 w_6 2 w)))
 (= ?x55 ?x3651))))
 ))
 (let (($x10295 (forall ((n (_ BitVec 6)) )(let ((?x154 (sc_s 1)))
 (let ((?x4271 (bvadd (_ bv63 6) ?x154)))
 (let (($x3365 (bvsle ?x4271 n)))
 (or $x3365 (= (stack_s w_5 w_6 2 n) (stack_s w_5 w_6 1 n)))))))
 ))
 (let ((?x218 (sc_s 2)))
 (let (($x3363 (= ?x218 (bvadd (_ bv1 6) (sc_s 1)))))
 (let ((?x9219 (used_gas_s w_5 w_6 2)))
 (let ((?x154 (sc_s 1)))
 (let ((?x4271 (bvadd (_ bv63 6) ?x154)))
 (let ((?x784 (stack_s w_5 w_6 1 ?x4271)))
 (let (($x5377 (= $x189 (or $x57 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x72))) (_ bv0 1)))))))
 (let (($x2755 (forall ((w (_ BitVec 256)) )(let ((?x364 (storage_s w_5 w_6 0 w)))
 (let ((?x3651 (storage_s w_5 w_6 1 w)))
 (= ?x3651 ?x364))))
 ))
 (let (($x11675 (forall ((n (_ BitVec 6)) )(let ((?x72 (sc_s 0)))
 (let (($x5566 (bvsle ?x72 n)))
 (or $x5566 (= (stack_s w_5 w_6 1 n) (stack_s w_5 w_6 0 n))))))
 ))
 (let (($x9024 (= ?x154 (bvadd (_ bv1 6) ?x72))))
 (let (($x3121 (forall ((w (_ BitVec 256)) )(let ((?x364 (storage_s w_5 w_6 0 w)))
 (= ?x364 (_ bv0 256))))
 ))
 (let (($x1533 (= ?x2502 0)))
 (let (($x7461 (not $x57)))
 (let (($x1074 (= ?x72 (_ bv0 6))))
 (and $x1074 $x7461 $x1533 $x3121 (= (stack_s w_5 w_6 1 ?x72) w_5) (= (used_gas_s w_5 w_6 1) (+ 3 ?x2502)) $x9024 $x11675 $x2755 $x5377 (= (stack_s w_5 w_6 2 (bvadd (_ bv63 6) ?x218)) ?x784) (= (stack_s w_5 w_6 2 ?x4271) ?x784) (= ?x9219 (+ 3 (used_gas_s w_5 w_6 1))) $x3363 $x10295 $x8057 $x4374 (= (stack_s w_5 w_6 3 ?x218) w_6) (= ?x59 (+ 3 ?x9219)) $x5768 $x1930 $x3263 (= $x292 (or $x247 $x4387)) (= (stack_s w_5 w_6 4 ?x275) (_ bv15250 256)) (= ?x11424 (+ 3 ?x59)) $x8307 $x1714 $x380 $x6234 (= (stack_s w_5 w_6 5 ?x4305) (_ bv65535 256)) (= ?x10367 (+ 3 ?x11424)) $x9414 $x8934 $x3625 $x7390 (= (stack_s w_5 w_6 6 (bvadd (_ bv63 6) ?x926)) (bvnot ?x7829)) $x6794 (= ?x926 (bvadd (_ bv63 6) ?x4319)) $x552 $x2518 $x11468 (= (stack_t w_5 w_6 1 ?x63) w_5) (= (used_gas_t w_5 w_6 1) (+ 3 ?x1883)) $x1385 $x10969 $x11097 $x10110 (= (stack_t w_5 w_6 2 ?x7154) w_5) (= ?x8773 (+ 3 (used_gas_t w_5 w_6 1))) $x3007 $x2801 $x10265 (= $x2163 (or $x8377 $x9864)) (= (stack_t w_5 w_6 3 ?x2714) w_6) (= ?x10194 (+ 3 ?x8773)) $x1004 $x11832 $x7679 $x6807 (= (stack_t w_5 w_6 4 ?x6438) (_ bv15250 256)) $x5215 $x6479 $x11320 $x4902 (= $x7854 (or $x6783 $x4755)) $x73 $x6271 $x58 $x1195 $x915 (not (and $x7899 $x2723 $x4404 $x6238))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
(check-sat)
(get-proof)