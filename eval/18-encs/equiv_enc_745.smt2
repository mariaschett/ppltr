; PUSH 0x00 DUP5 MLOAD GT ISZERO => DUP4 MLOAD ISZERO
(set-option :produce-proofs true)
; 
(set-info :status unknown)
(declare-fun storage_t ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 256)) (_ BitVec 256))
(declare-fun storage_s ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 256)) (_ BitVec 256))
(declare-fun exc_halt_t (Int) Bool)
(declare-fun exc_halt_s (Int) Bool)
(declare-fun stack_t ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 6)) (_ BitVec 256))
(declare-fun stack_s ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 6)) (_ BitVec 256))
(declare-fun sc_t (Int) (_ BitVec 6))
(declare-fun sc_s (Int) (_ BitVec 6))
(declare-fun used_gas_t ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int) Int)
(declare-fun used_gas_s ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int) Int)
(declare-fun f_MLOAD ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256)) (_ BitVec 256))
(assert
 (exists ((x_0 (_ BitVec 256)) (x_1 (_ BitVec 256)) (x_2 (_ BitVec 256)) (x_3 (_ BitVec 256)) (x_MLOAD_0 (_ BitVec 256)) )(let (($x3640 (forall ((w (_ BitVec 256)) )(let ((?x8729 (storage_t x_0 x_1 x_2 x_3 x_MLOAD_0 3 w)))
 (let ((?x6363 (storage_s x_0 x_1 x_2 x_3 x_MLOAD_0 5 w)))
 (= ?x6363 ?x8729))))
 ))
 (let (($x4112 (exc_halt_t 3)))
 (let (($x1862 (exc_halt_s 5)))
 (let (($x34 (= $x1862 $x4112)))
 (let (($x11036 (forall ((n (_ BitVec 6)) )(let ((?x11092 (stack_t x_0 x_1 x_2 x_3 x_MLOAD_0 3 n)))
 (let ((?x11376 (stack_s x_0 x_1 x_2 x_3 x_MLOAD_0 5 n)))
 (let (($x9151 (= ?x11376 ?x11092)))
 (let ((?x11964 (sc_t 3)))
 (let (($x5242 (bvsle ?x11964 n)))
 (or $x5242 $x9151)))))))
 ))
 (let ((?x11964 (sc_t 3)))
 (let ((?x4319 (sc_s 5)))
 (let (($x3659 (= ?x4319 ?x11964)))
 (let ((?x323 (used_gas_t x_0 x_1 x_2 x_3 x_MLOAD_0 0)))
 (let ((?x1375 (used_gas_s x_0 x_1 x_2 x_3 x_MLOAD_0 0)))
 (let (($x7747 (= ?x1375 ?x323)))
 (let (($x6311 (forall ((w (_ BitVec 256)) )(let ((?x4844 (storage_t x_0 x_1 x_2 x_3 x_MLOAD_0 0 w)))
 (let ((?x1248 (storage_s x_0 x_1 x_2 x_3 x_MLOAD_0 0 w)))
 (= ?x1248 ?x4844))))
 ))
 (let (($x56 (exc_halt_t 0)))
 (let (($x57 (exc_halt_s 0)))
 (let (($x58 (= $x57 $x56)))
 (let (($x3729 (forall ((n (_ BitVec 6)) )(let ((?x6837 (stack_t x_0 x_1 x_2 x_3 x_MLOAD_0 0 n)))
 (let ((?x6474 (stack_s x_0 x_1 x_2 x_3 x_MLOAD_0 0 n)))
 (let (($x7710 (= ?x6474 ?x6837)))
 (let ((?x63 (sc_t 0)))
 (let (($x5904 (bvsle ?x63 n)))
 (or $x5904 $x7710)))))))
 ))
 (let ((?x63 (sc_t 0)))
 (let ((?x72 (sc_s 0)))
 (let (($x73 (= ?x72 ?x63)))
 (let (($x230 (= $x4112 (or (exc_halt_t 2) (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_t 2))))))))
 (let (($x4605 (forall ((w (_ BitVec 256)) )(let ((?x2384 (storage_t x_0 x_1 x_2 x_3 x_MLOAD_0 2 w)))
 (let ((?x8729 (storage_t x_0 x_1 x_2 x_3 x_MLOAD_0 3 w)))
 (= ?x8729 ?x2384))))
 ))
 (let (($x6216 (forall ((n (_ BitVec 6)) )(let ((?x981 (stack_t x_0 x_1 x_2 x_3 x_MLOAD_0 2 n)))
 (let ((?x11092 (stack_t x_0 x_1 x_2 x_3 x_MLOAD_0 3 n)))
 (or (= ?x11092 ?x981) (bvsle (bvadd (_ bv63 6) (sc_t 2)) n)))))
 ))
 (let ((?x2992 (sc_t 2)))
 (let (($x11604 (= ?x11964 ?x2992)))
 (let (($x1465 (= (used_gas_t x_0 x_1 x_2 x_3 x_MLOAD_0 3) (+ 3 (used_gas_t x_0 x_1 x_2 x_3 x_MLOAD_0 2)))))
 (let ((?x1711 (ite (= (stack_t x_0 x_1 x_2 x_3 x_MLOAD_0 2 (bvadd (_ bv63 6) ?x2992)) (_ bv0 256)) (_ bv1 256) (_ bv0 256))))
 (let (($x9580 (exc_halt_t 2)))
 (let (($x11009 (= $x9580 (or (exc_halt_t 1) (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_t 1))))))))
 (let (($x1986 (forall ((w (_ BitVec 256)) )(let ((?x1332 (storage_t x_0 x_1 x_2 x_3 x_MLOAD_0 1 w)))
 (let ((?x2384 (storage_t x_0 x_1 x_2 x_3 x_MLOAD_0 2 w)))
 (= ?x2384 ?x1332))))
 ))
 (let (($x4439 (forall ((n (_ BitVec 6)) )(let ((?x3379 (sc_t 1)))
 (let ((?x7595 (bvadd (_ bv63 6) ?x3379)))
 (let (($x6410 (bvsle ?x7595 n)))
 (let ((?x933 (stack_t x_0 x_1 x_2 x_3 x_MLOAD_0 1 n)))
 (let ((?x981 (stack_t x_0 x_1 x_2 x_3 x_MLOAD_0 2 n)))
 (or (= ?x981 ?x933) $x6410)))))))
 ))
 (let ((?x3379 (sc_t 1)))
 (let (($x10030 (= ?x2992 ?x3379)))
 (let ((?x3509 (used_gas_t x_0 x_1 x_2 x_3 x_MLOAD_0 2)))
 (let ((?x7595 (bvadd (_ bv63 6) ?x3379)))
 (let ((?x8524 (stack_t x_0 x_1 x_2 x_3 x_MLOAD_0 1 ?x7595)))
 (let ((?x2620 (bvadd (_ bv63 6) ?x2992)))
 (let ((?x6208 (stack_t x_0 x_1 x_2 x_3 x_MLOAD_0 2 ?x2620)))
 (let (($x11775 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x63))) (_ bv0 1)))))
 (let (($x408 (exc_halt_t 1)))
 (let (($x5317 (forall ((w (_ BitVec 256)) )(let ((?x4844 (storage_t x_0 x_1 x_2 x_3 x_MLOAD_0 0 w)))
 (let ((?x1332 (storage_t x_0 x_1 x_2 x_3 x_MLOAD_0 1 w)))
 (= ?x1332 ?x4844))))
 ))
 (let (($x11273 (forall ((n (_ BitVec 6)) )(let ((?x6837 (stack_t x_0 x_1 x_2 x_3 x_MLOAD_0 0 n)))
 (let ((?x933 (stack_t x_0 x_1 x_2 x_3 x_MLOAD_0 1 n)))
 (or (= ?x933 ?x6837) (bvsle (bvadd (_ bv60 6) (sc_t 0)) n)))))
 ))
 (let (($x8894 (= ?x3379 (bvadd (_ bv1 6) ?x63))))
 (let (($x8726 (= (stack_t x_0 x_1 x_2 x_3 x_MLOAD_0 1 (bvadd (_ bv63 6) ?x63)) (stack_t x_0 x_1 x_2 x_3 x_MLOAD_0 0 (bvadd (_ bv63 6) ?x63)))))
 (let (($x226 (= (stack_t x_0 x_1 x_2 x_3 x_MLOAD_0 1 (bvadd (_ bv62 6) ?x63)) (stack_t x_0 x_1 x_2 x_3 x_MLOAD_0 0 (bvadd (_ bv62 6) ?x63)))))
 (let (($x9679 (= (stack_t x_0 x_1 x_2 x_3 x_MLOAD_0 1 (bvadd (_ bv61 6) ?x63)) (stack_t x_0 x_1 x_2 x_3 x_MLOAD_0 0 (bvadd (_ bv61 6) ?x63)))))
 (let ((?x2333 (bvadd (_ bv60 6) ?x63)))
 (let ((?x5132 (stack_t x_0 x_1 x_2 x_3 x_MLOAD_0 0 ?x2333)))
 (let (($x2269 (= $x1862 (or (exc_halt_s 4) (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_s 4))))))))
 (let (($x11952 (forall ((w (_ BitVec 256)) )(let ((?x11418 (storage_s x_0 x_1 x_2 x_3 x_MLOAD_0 4 w)))
 (let ((?x6363 (storage_s x_0 x_1 x_2 x_3 x_MLOAD_0 5 w)))
 (= ?x6363 ?x11418))))
 ))
 (let (($x3649 (forall ((n (_ BitVec 6)) )(let ((?x8615 (stack_s x_0 x_1 x_2 x_3 x_MLOAD_0 4 n)))
 (let ((?x11376 (stack_s x_0 x_1 x_2 x_3 x_MLOAD_0 5 n)))
 (or (= ?x11376 ?x8615) (bvsle (bvadd (_ bv63 6) (sc_s 4)) n)))))
 ))
 (let ((?x3145 (sc_s 4)))
 (let (($x9024 (= ?x4319 ?x3145)))
 (let (($x3826 (= (used_gas_s x_0 x_1 x_2 x_3 x_MLOAD_0 5) (+ 3 (used_gas_s x_0 x_1 x_2 x_3 x_MLOAD_0 4)))))
 (let ((?x4073 (ite (= (stack_s x_0 x_1 x_2 x_3 x_MLOAD_0 4 (bvadd (_ bv63 6) ?x3145)) (_ bv0 256)) (_ bv1 256) (_ bv0 256))))
 (let (($x276 (exc_halt_s 4)))
 (let (($x467 (= $x276 (or (exc_halt_s 3) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 3))))))))
 (let (($x5298 (forall ((w (_ BitVec 256)) )(let ((?x2575 (storage_s x_0 x_1 x_2 x_3 x_MLOAD_0 3 w)))
 (let ((?x11418 (storage_s x_0 x_1 x_2 x_3 x_MLOAD_0 4 w)))
 (= ?x11418 ?x2575))))
 ))
 (let (($x186 (forall ((n (_ BitVec 6)) )(let ((?x3338 (stack_s x_0 x_1 x_2 x_3 x_MLOAD_0 3 n)))
 (let ((?x8615 (stack_s x_0 x_1 x_2 x_3 x_MLOAD_0 4 n)))
 (let ((?x4554 (sc_s 3)))
 (let ((?x7881 (bvadd (_ bv62 6) ?x4554)))
 (let (($x1229 (bvsle ?x7881 n)))
 (or $x1229 (= ?x8615 ?x3338))))))))
 ))
 (let ((?x1558 (used_gas_s x_0 x_1 x_2 x_3 x_MLOAD_0 4)))
 (let ((?x4554 (sc_s 3)))
 (let ((?x5619 (bvadd (_ bv63 6) ?x4554)))
 (let ((?x773 (stack_s x_0 x_1 x_2 x_3 x_MLOAD_0 3 ?x5619)))
 (let (($x7460 (bvule ?x773 (stack_s x_0 x_1 x_2 x_3 x_MLOAD_0 3 (bvadd (_ bv62 6) ?x4554)))))
 (let ((?x4151 (bvadd (_ bv63 6) ?x3145)))
 (let ((?x8094 (stack_s x_0 x_1 x_2 x_3 x_MLOAD_0 4 ?x4151)))
 (let (($x8777 (exc_halt_s 3)))
 (let (($x9861 (= $x8777 (or (exc_halt_s 2) (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_s 2))))))))
 (let (($x9966 (forall ((w (_ BitVec 256)) )(let ((?x578 (storage_s x_0 x_1 x_2 x_3 x_MLOAD_0 2 w)))
 (let ((?x2575 (storage_s x_0 x_1 x_2 x_3 x_MLOAD_0 3 w)))
 (= ?x2575 ?x578))))
 ))
 (let (($x4331 (forall ((n (_ BitVec 6)) )(let ((?x4064 (stack_s x_0 x_1 x_2 x_3 x_MLOAD_0 2 n)))
 (let ((?x3338 (stack_s x_0 x_1 x_2 x_3 x_MLOAD_0 3 n)))
 (let ((?x7378 (sc_s 2)))
 (let ((?x121 (bvadd (_ bv63 6) ?x7378)))
 (let (($x2407 (bvsle ?x121 n)))
 (or $x2407 (= ?x3338 ?x4064))))))))
 ))
 (let ((?x7378 (sc_s 2)))
 (let (($x1517 (= ?x4554 ?x7378)))
 (let ((?x1173 (used_gas_s x_0 x_1 x_2 x_3 x_MLOAD_0 3)))
 (let ((?x121 (bvadd (_ bv63 6) ?x7378)))
 (let ((?x4907 (stack_s x_0 x_1 x_2 x_3 x_MLOAD_0 2 ?x121)))
 (let (($x4172 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 1)))) (_ bv0 1)))))
 (let (($x2589 (exc_halt_s 1)))
 (let (($x7873 (exc_halt_s 2)))
 (let (($x11972 (forall ((w (_ BitVec 256)) )(let ((?x5995 (storage_s x_0 x_1 x_2 x_3 x_MLOAD_0 1 w)))
 (let ((?x578 (storage_s x_0 x_1 x_2 x_3 x_MLOAD_0 2 w)))
 (= ?x578 ?x5995))))
 ))
 (let (($x10051 (forall ((n (_ BitVec 6)) )(let ((?x10471 (stack_s x_0 x_1 x_2 x_3 x_MLOAD_0 1 n)))
 (let ((?x4064 (stack_s x_0 x_1 x_2 x_3 x_MLOAD_0 2 n)))
 (or (= ?x4064 ?x10471) (bvsle (bvadd (_ bv59 6) (sc_s 1)) n)))))
 ))
 (let (($x10133 (= ?x7378 (bvadd (_ bv1 6) (sc_s 1)))))
 (let ((?x2131 (used_gas_s x_0 x_1 x_2 x_3 x_MLOAD_0 2)))
 (let (($x1480 (= (stack_s x_0 x_1 x_2 x_3 x_MLOAD_0 2 (bvadd (_ bv63 6) (sc_s 1))) (stack_s x_0 x_1 x_2 x_3 x_MLOAD_0 1 (bvadd (_ bv63 6) (sc_s 1))))))
 (let (($x213 (= (stack_s x_0 x_1 x_2 x_3 x_MLOAD_0 2 (bvadd (_ bv62 6) (sc_s 1))) (stack_s x_0 x_1 x_2 x_3 x_MLOAD_0 1 (bvadd (_ bv62 6) (sc_s 1))))))
 (let (($x10383 (= (stack_s x_0 x_1 x_2 x_3 x_MLOAD_0 2 (bvadd (_ bv61 6) (sc_s 1))) (stack_s x_0 x_1 x_2 x_3 x_MLOAD_0 1 (bvadd (_ bv61 6) (sc_s 1))))))
 (let (($x6260 (= (stack_s x_0 x_1 x_2 x_3 x_MLOAD_0 2 (bvadd (_ bv60 6) (sc_s 1))) (stack_s x_0 x_1 x_2 x_3 x_MLOAD_0 1 (bvadd (_ bv60 6) (sc_s 1))))))
 (let ((?x154 (sc_s 1)))
 (let ((?x8809 (bvadd (_ bv59 6) ?x154)))
 (let ((?x8991 (stack_s x_0 x_1 x_2 x_3 x_MLOAD_0 1 ?x8809)))
 (let (($x10668 (= $x2589 (or $x57 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x72))) (_ bv0 1)))))))
 (let (($x10006 (forall ((w (_ BitVec 256)) )(let ((?x1248 (storage_s x_0 x_1 x_2 x_3 x_MLOAD_0 0 w)))
 (let ((?x5995 (storage_s x_0 x_1 x_2 x_3 x_MLOAD_0 1 w)))
 (= ?x5995 ?x1248))))
 ))
 (let (($x3859 (forall ((n (_ BitVec 6)) )(let ((?x72 (sc_s 0)))
 (let (($x9529 (bvsle ?x72 n)))
 (let ((?x6474 (stack_s x_0 x_1 x_2 x_3 x_MLOAD_0 0 n)))
 (let ((?x10471 (stack_s x_0 x_1 x_2 x_3 x_MLOAD_0 1 n)))
 (or (= ?x10471 ?x6474) $x9529))))))
 ))
 (let (($x5498 (= ?x154 (bvadd (_ bv1 6) ?x72))))
 (let (($x3906 (forall ((w0 (_ BitVec 256)) )(let (($x3122 (= (stack_s x_0 x_1 x_2 x_3 x_MLOAD_0 2 (bvadd (_ bv63 6) (sc_s 2))) w0)))
 (let ((?x2451 (f_MLOAD x_0 x_1 x_2 x_3 x_MLOAD_0 w0)))
 (= ?x2451 (ite $x3122 x_MLOAD_0 (_ bv0 256))))))
 ))
 (let (($x2551 (forall ((w (_ BitVec 256)) )(let ((?x1248 (storage_s x_0 x_1 x_2 x_3 x_MLOAD_0 0 w)))
 (= ?x1248 (_ bv0 256))))
 ))
 (let (($x5141 (= ?x1375 0)))
 (let (($x11010 (not $x57)))
 (let (($x6981 (= (stack_s x_0 x_1 x_2 x_3 x_MLOAD_0 0 (_ bv3 6)) x_3)))
 (let (($x8507 (= (stack_s x_0 x_1 x_2 x_3 x_MLOAD_0 0 (_ bv2 6)) x_2)))
 (let (($x6415 (= (stack_s x_0 x_1 x_2 x_3 x_MLOAD_0 0 (_ bv1 6)) x_1)))
 (let (($x2338 (= (stack_s x_0 x_1 x_2 x_3 x_MLOAD_0 0 (_ bv0 6)) x_0)))
 (let (($x11199 (= ?x72 (_ bv4 6))))
 (and $x11199 $x2338 $x6415 $x8507 $x6981 $x11010 $x5141 $x2551 $x3906 (= (stack_s x_0 x_1 x_2 x_3 x_MLOAD_0 1 ?x72) (_ bv0 256)) (= (used_gas_s x_0 x_1 x_2 x_3 x_MLOAD_0 1) (+ 3 ?x1375)) $x5498 $x3859 $x10006 $x10668 (= ?x4907 ?x8991) (= (stack_s x_0 x_1 x_2 x_3 x_MLOAD_0 2 ?x8809) ?x8991) $x6260 $x10383 $x213 $x1480 (= ?x2131 (+ 3 (used_gas_s x_0 x_1 x_2 x_3 x_MLOAD_0 1))) $x10133 $x10051 $x11972 (= $x7873 (or $x2589 $x4172 (not (bvsle (_ bv0 6) ?x8809)))) (= ?x773 (f_MLOAD x_0 x_1 x_2 x_3 x_MLOAD_0 ?x4907)) (= ?x1173 (+ 3 ?x2131)) $x1517 $x4331 $x9966 $x9861 (= ?x8094 (ite $x7460 (_ bv0 256) (_ bv1 256))) (= ?x1558 (+ 3 ?x1173)) (= ?x3145 ?x5619) $x186 $x5298 $x467 (= (stack_s x_0 x_1 x_2 x_3 x_MLOAD_0 5 (bvadd (_ bv63 6) ?x4319)) ?x4073) $x3826 $x9024 $x3649 $x11952 $x2269 (= ?x8524 ?x5132) (= (stack_t x_0 x_1 x_2 x_3 x_MLOAD_0 1 ?x2333) ?x5132) $x9679 $x226 $x8726 (= (used_gas_t x_0 x_1 x_2 x_3 x_MLOAD_0 1) (+ 3 ?x323)) $x8894 $x11273 $x5317 (= $x408 (or $x56 (not (bvsle (_ bv0 6) ?x2333)) $x11775)) (= ?x6208 (f_MLOAD x_0 x_1 x_2 x_3 x_MLOAD_0 ?x8524)) (= ?x3509 (+ 3 (used_gas_t x_0 x_1 x_2 x_3 x_MLOAD_0 1))) $x10030 $x4439 $x1986 $x11009 (= (stack_t x_0 x_1 x_2 x_3 x_MLOAD_0 3 (bvadd (_ bv63 6) ?x11964)) ?x1711) $x1465 $x11604 $x6216 $x4605 $x230 $x73 $x3729 $x58 $x6311 $x7747 (not (and $x3659 $x11036 $x34 $x3640)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
 )
(check-sat)
(get-proof)
