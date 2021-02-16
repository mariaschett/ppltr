; PUSH cw_1 SWAP1 SWAP2 ADD => SWAP1 PUSH cw_1 ADD
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
 (exists ((x_0 (_ BitVec 256)) (x_1 (_ BitVec 256)) )(let (($x9451 (forall ((w (_ BitVec 256)) )(let ((?x5442 (storage_t x_0 x_1 w_1 3 w)))
 (let ((?x9397 (storage_s x_0 x_1 w_1 4 w)))
 (= ?x9397 ?x5442))))
 ))
 (let (($x6783 (exc_halt_t 3)))
 (let (($x7172 (exc_halt_s 4)))
 (let (($x8759 (= $x7172 $x6783)))
 (let (($x5824 (forall ((n (_ BitVec 6)) )(let ((?x6438 (sc_t 3)))
 (let (($x11627 (bvsle ?x6438 n)))
 (let ((?x11251 (stack_t x_0 x_1 w_1 3 n)))
 (let ((?x7439 (stack_s x_0 x_1 w_1 4 n)))
 (let (($x7508 (= ?x7439 ?x11251)))
 (or $x7508 $x11627)))))))
 ))
 (let ((?x6438 (sc_t 3)))
 (let ((?x4305 (sc_s 4)))
 (let (($x9842 (= ?x4305 ?x6438)))
 (let ((?x9597 (used_gas_t x_0 x_1 w_1 0)))
 (let ((?x1526 (used_gas_s x_0 x_1 w_1 0)))
 (let (($x753 (= ?x1526 ?x9597)))
 (let (($x5650 (forall ((w (_ BitVec 256)) )(let ((?x4459 (storage_t x_0 x_1 w_1 0 w)))
 (let ((?x11222 (storage_s x_0 x_1 w_1 0 w)))
 (= ?x11222 ?x4459))))
 ))
 (let (($x56 (exc_halt_t 0)))
 (let (($x57 (exc_halt_s 0)))
 (let (($x58 (= $x57 $x56)))
 (let (($x10319 (forall ((n (_ BitVec 6)) )(let ((?x9344 (stack_t x_0 x_1 w_1 0 n)))
 (let ((?x7952 (stack_s x_0 x_1 w_1 0 n)))
 (let (($x10810 (= ?x7952 ?x9344)))
 (let ((?x63 (sc_t 0)))
 (let (($x2525 (bvsle ?x63 n)))
 (or $x2525 $x10810)))))))
 ))
 (let ((?x63 (sc_t 0)))
 (let ((?x72 (sc_s 0)))
 (let (($x73 (= ?x72 ?x63)))
 (let (($x10273 (= $x6783 (or (exc_halt_t 2) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_t 2))))))))
 (let (($x2617 (forall ((w (_ BitVec 256)) )(let ((?x11128 (storage_t x_0 x_1 w_1 2 w)))
 (let ((?x5442 (storage_t x_0 x_1 w_1 3 w)))
 (= ?x5442 ?x11128))))
 ))
 (let (($x11849 (forall ((n (_ BitVec 6)) )(let ((?x600 (stack_t x_0 x_1 w_1 2 n)))
 (let ((?x11251 (stack_t x_0 x_1 w_1 3 n)))
 (or (bvsle (bvadd (_ bv62 6) (sc_t 2)) n) (= ?x11251 ?x600)))))
 ))
 (let (($x1385 (= (used_gas_t x_0 x_1 w_1 3) (+ 3 (used_gas_t x_0 x_1 w_1 2)))))
 (let ((?x1191 (bvadd (stack_t x_0 x_1 w_1 2 (bvadd (_ bv63 6) (sc_t 2))) (stack_t x_0 x_1 w_1 2 (bvadd (_ bv62 6) (sc_t 2))))))
 (let (($x9219 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_t 1)))) (_ bv0 1)))))
 (let (($x3508 (exc_halt_t 1)))
 (let (($x3071 (exc_halt_t 2)))
 (let (($x10458 (= $x3071 (or $x3508 $x9219))))
 (let (($x8736 (forall ((w (_ BitVec 256)) )(let ((?x10721 (storage_t x_0 x_1 w_1 1 w)))
 (let ((?x11128 (storage_t x_0 x_1 w_1 2 w)))
 (= ?x11128 ?x10721))))
 ))
 (let (($x2504 (forall ((n (_ BitVec 6)) )(let ((?x8347 (sc_t 1)))
 (let (($x8126 (bvsle ?x8347 n)))
 (let ((?x4844 (stack_t x_0 x_1 w_1 1 n)))
 (let ((?x600 (stack_t x_0 x_1 w_1 2 n)))
 (or (= ?x600 ?x4844) $x8126))))))
 ))
 (let ((?x2714 (sc_t 2)))
 (let (($x1996 (= ?x2714 (bvadd (_ bv1 6) (sc_t 1)))))
 (let ((?x11430 (used_gas_t x_0 x_1 w_1 2)))
 (let (($x6520 (forall ((w (_ BitVec 256)) )(let ((?x4459 (storage_t x_0 x_1 w_1 0 w)))
 (let ((?x10721 (storage_t x_0 x_1 w_1 1 w)))
 (= ?x10721 ?x4459))))
 ))
 (let (($x4762 (forall ((n (_ BitVec 6)) )(let ((?x9344 (stack_t x_0 x_1 w_1 0 n)))
 (let ((?x4844 (stack_t x_0 x_1 w_1 1 n)))
 (or (= ?x4844 ?x9344) (bvsle (bvadd (_ bv62 6) (sc_t 0)) n)))))
 ))
 (let (($x7077 (= (stack_t x_0 x_1 w_1 1 (bvadd (_ bv62 6) (sc_t 1))) (stack_t x_0 x_1 w_1 0 (bvadd (_ bv63 6) ?x63)))))
 (let (($x9288 (= (stack_t x_0 x_1 w_1 1 (bvadd (_ bv63 6) (sc_t 1))) (stack_t x_0 x_1 w_1 0 (bvadd (_ bv62 6) ?x63)))))
 (let (($x7783 (= $x7172 (or (exc_halt_s 3) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 3))))))))
 (let (($x8079 (forall ((w (_ BitVec 256)) )(let ((?x1024 (storage_s x_0 x_1 w_1 3 w)))
 (let ((?x9397 (storage_s x_0 x_1 w_1 4 w)))
 (= ?x9397 ?x1024))))
 ))
 (let (($x974 (forall ((n (_ BitVec 6)) )(let ((?x2030 (stack_s x_0 x_1 w_1 3 n)))
 (let ((?x7439 (stack_s x_0 x_1 w_1 4 n)))
 (let ((?x275 (sc_s 3)))
 (let ((?x6337 (bvadd (_ bv62 6) ?x275)))
 (let (($x6005 (bvsle ?x6337 n)))
 (or $x6005 (= ?x7439 ?x2030))))))))
 ))
 (let ((?x275 (sc_s 3)))
 (let ((?x3684 (bvadd (_ bv63 6) ?x275)))
 (let (($x11684 (= ?x4305 ?x3684)))
 (let (($x11475 (= (used_gas_s x_0 x_1 w_1 4) (+ 3 (used_gas_s x_0 x_1 w_1 3)))))
 (let ((?x6337 (bvadd (_ bv62 6) ?x275)))
 (let ((?x10196 (stack_s x_0 x_1 w_1 3 ?x6337)))
 (let ((?x6531 (stack_s x_0 x_1 w_1 3 ?x3684)))
 (let (($x7389 (= (stack_s x_0 x_1 w_1 4 (bvadd (_ bv63 6) ?x4305)) (bvadd ?x6531 ?x10196))))
 (let (($x292 (exc_halt_s 3)))
 (let (($x8330 (= $x292 (or (exc_halt_s 2) (not (bvsle (_ bv0 6) (bvadd (_ bv61 6) (sc_s 2))))))))
 (let (($x11281 (forall ((w (_ BitVec 256)) )(let ((?x8480 (storage_s x_0 x_1 w_1 2 w)))
 (let ((?x1024 (storage_s x_0 x_1 w_1 3 w)))
 (= ?x1024 ?x8480))))
 ))
 (let (($x8536 (forall ((n (_ BitVec 6)) )(let ((?x10277 (stack_s x_0 x_1 w_1 2 n)))
 (let ((?x2030 (stack_s x_0 x_1 w_1 3 n)))
 (let ((?x218 (sc_s 2)))
 (let ((?x861 (bvadd (_ bv61 6) ?x218)))
 (let (($x1357 (bvsle ?x861 n)))
 (or $x1357 (= ?x2030 ?x10277))))))))
 ))
 (let ((?x218 (sc_s 2)))
 (let (($x7762 (= ?x275 ?x218)))
 (let ((?x2983 (used_gas_s x_0 x_1 w_1 3)))
 (let ((?x5784 (bvadd (_ bv63 6) ?x218)))
 (let ((?x11528 (stack_s x_0 x_1 w_1 2 ?x5784)))
 (let (($x247 (exc_halt_s 2)))
 (let (($x4535 (= $x247 (or (exc_halt_s 1) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 1))))))))
 (let (($x4211 (forall ((w (_ BitVec 256)) )(let ((?x9005 (storage_s x_0 x_1 w_1 1 w)))
 (let ((?x8480 (storage_s x_0 x_1 w_1 2 w)))
 (= ?x8480 ?x9005))))
 ))
 (let (($x2057 (forall ((n (_ BitVec 6)) )(let ((?x10803 (stack_s x_0 x_1 w_1 1 n)))
 (let ((?x10277 (stack_s x_0 x_1 w_1 2 n)))
 (or (bvsle (bvadd (_ bv62 6) (sc_s 1)) n) (= ?x10277 ?x10803)))))
 ))
 (let ((?x154 (sc_s 1)))
 (let (($x1088 (= ?x218 ?x154)))
 (let ((?x255 (used_gas_s x_0 x_1 w_1 2)))
 (let ((?x5210 (bvadd (_ bv62 6) ?x218)))
 (let ((?x2187 (stack_s x_0 x_1 w_1 2 ?x5210)))
 (let (($x189 (exc_halt_s 1)))
 (let (($x9459 (= $x189 (or $x57 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x72))) (_ bv0 1)))))))
 (let (($x3914 (forall ((w (_ BitVec 256)) )(let ((?x11222 (storage_s x_0 x_1 w_1 0 w)))
 (let ((?x9005 (storage_s x_0 x_1 w_1 1 w)))
 (= ?x9005 ?x11222))))
 ))
 (let (($x3659 (forall ((n (_ BitVec 6)) )(let ((?x7952 (stack_s x_0 x_1 w_1 0 n)))
 (let ((?x10803 (stack_s x_0 x_1 w_1 1 n)))
 (let ((?x72 (sc_s 0)))
 (let (($x2401 (bvsle ?x72 n)))
 (or $x2401 (= ?x10803 ?x7952)))))))
 ))
 (let (($x10097 (= ?x154 (bvadd (_ bv1 6) ?x72))))
 (let (($x3097 (forall ((w (_ BitVec 256)) )(let ((?x11222 (storage_s x_0 x_1 w_1 0 w)))
 (= ?x11222 (_ bv0 256))))
 ))
 (let (($x8397 (= ?x1526 0)))
 (let (($x4511 (not $x57)))
 (let (($x9697 (= (stack_s x_0 x_1 w_1 0 (_ bv1 6)) x_1)))
 (let (($x8995 (= (stack_s x_0 x_1 w_1 0 (_ bv0 6)) x_0)))
 (let (($x7157 (= ?x72 (_ bv2 6))))
 (and $x7157 $x8995 $x9697 $x4511 $x8397 $x3097 (= (stack_s x_0 x_1 w_1 1 ?x72) w_1) (= (used_gas_s x_0 x_1 w_1 1) (+ 3 ?x1526)) $x10097 $x3659 $x3914 $x9459 (= ?x11528 (stack_s x_0 x_1 w_1 1 (bvadd (_ bv62 6) ?x154))) (= ?x2187 (stack_s x_0 x_1 w_1 1 (bvadd (_ bv63 6) ?x154))) (= ?x255 (+ 3 (used_gas_s x_0 x_1 w_1 1))) $x1088 $x2057 $x4211 $x4535 (= ?x6531 (stack_s x_0 x_1 w_1 2 (bvadd (_ bv61 6) ?x218))) (= (stack_s x_0 x_1 w_1 3 (bvadd (_ bv61 6) ?x275)) ?x11528) (= ?x10196 ?x2187) (= ?x2983 (+ 3 ?x255)) $x7762 $x8536 $x11281 $x8330 $x7389 $x11475 $x11684 $x974 $x8079 $x7783 $x9288 $x7077 (= (used_gas_t x_0 x_1 w_1 1) (+ 3 ?x9597)) (= (sc_t 1) ?x63) $x4762 $x6520 (= $x3508 (or $x56 (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) ?x63))))) (= (stack_t x_0 x_1 w_1 2 (sc_t 1)) w_1) (= ?x11430 (+ 3 (used_gas_t x_0 x_1 w_1 1))) $x1996 $x2504 $x8736 $x10458 (= (stack_t x_0 x_1 w_1 3 (bvadd (_ bv63 6) ?x6438)) ?x1191) $x1385 (= ?x6438 (bvadd (_ bv63 6) ?x2714)) $x11849 $x2617 $x10273 $x73 $x10319 $x58 $x5650 $x753 (not (and $x9842 $x5824 $x8759 $x9451)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
 )
(check-sat)
(get-proof)