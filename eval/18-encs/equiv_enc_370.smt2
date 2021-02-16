; ADDRESS BALANCE SWAP1 POP => POP ADDRESS BALANCE
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
(declare-fun f_BALANCE ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256)) (_ BitVec 256))
(assert
 (exists ((x_0 (_ BitVec 256)) (x_ADDRESS (_ BitVec 256)) (x_BALANCE_0 (_ BitVec 256)) )(let (($x3669 (forall ((w (_ BitVec 256)) )(let ((?x3012 (storage_t x_0 x_ADDRESS x_BALANCE_0 3 w)))
 (let ((?x10276 (storage_s x_0 x_ADDRESS x_BALANCE_0 4 w)))
 (= ?x10276 ?x3012))))
 ))
 (let (($x6783 (exc_halt_t 3)))
 (let (($x7172 (exc_halt_s 4)))
 (let (($x5597 (= $x7172 $x6783)))
 (let (($x8342 (forall ((n (_ BitVec 6)) )(let ((?x6438 (sc_t 3)))
 (let (($x9809 (bvsle ?x6438 n)))
 (let ((?x9598 (stack_t x_0 x_ADDRESS x_BALANCE_0 3 n)))
 (let ((?x5648 (stack_s x_0 x_ADDRESS x_BALANCE_0 4 n)))
 (let (($x5763 (= ?x5648 ?x9598)))
 (or $x5763 $x9809)))))))
 ))
 (let ((?x6438 (sc_t 3)))
 (let ((?x4305 (sc_s 4)))
 (let (($x4841 (= ?x4305 ?x6438)))
 (let ((?x6809 (used_gas_t x_0 x_ADDRESS x_BALANCE_0 0)))
 (let ((?x5134 (used_gas_s x_0 x_ADDRESS x_BALANCE_0 0)))
 (let (($x4890 (= ?x5134 ?x6809)))
 (let (($x9363 (forall ((w (_ BitVec 256)) )(let ((?x9648 (storage_t x_0 x_ADDRESS x_BALANCE_0 0 w)))
 (let ((?x680 (storage_s x_0 x_ADDRESS x_BALANCE_0 0 w)))
 (= ?x680 ?x9648))))
 ))
 (let (($x56 (exc_halt_t 0)))
 (let (($x57 (exc_halt_s 0)))
 (let (($x58 (= $x57 $x56)))
 (let (($x10075 (forall ((n (_ BitVec 6)) )(let ((?x5452 (stack_t x_0 x_ADDRESS x_BALANCE_0 0 n)))
 (let ((?x10582 (stack_s x_0 x_ADDRESS x_BALANCE_0 0 n)))
 (let (($x9997 (= ?x10582 ?x5452)))
 (let ((?x63 (sc_t 0)))
 (let (($x4866 (bvsle ?x63 n)))
 (or $x4866 $x9997)))))))
 ))
 (let ((?x63 (sc_t 0)))
 (let ((?x72 (sc_s 0)))
 (let (($x73 (= ?x72 ?x63)))
 (let (($x5088 (= $x6783 (or (exc_halt_t 2) (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_t 2))))))))
 (let (($x7234 (forall ((w (_ BitVec 256)) )(let ((?x8624 (storage_t x_0 x_ADDRESS x_BALANCE_0 2 w)))
 (let ((?x3012 (storage_t x_0 x_ADDRESS x_BALANCE_0 3 w)))
 (= ?x3012 ?x8624))))
 ))
 (let (($x4254 (forall ((n (_ BitVec 6)) )(let ((?x10197 (stack_t x_0 x_ADDRESS x_BALANCE_0 2 n)))
 (let ((?x9598 (stack_t x_0 x_ADDRESS x_BALANCE_0 3 n)))
 (let (($x10306 (= ?x9598 ?x10197)))
 (let ((?x2714 (sc_t 2)))
 (let ((?x9149 (bvadd (_ bv63 6) ?x2714)))
 (let (($x8046 (bvsle ?x9149 n)))
 (or $x8046 $x10306))))))))
 ))
 (let ((?x2714 (sc_t 2)))
 (let (($x6299 (= ?x6438 ?x2714)))
 (let ((?x11511 (used_gas_t x_0 x_ADDRESS x_BALANCE_0 3)))
 (let ((?x9149 (bvadd (_ bv63 6) ?x2714)))
 (let ((?x9475 (stack_t x_0 x_ADDRESS x_BALANCE_0 2 ?x9149)))
 (let ((?x10141 (bvadd (_ bv63 6) ?x6438)))
 (let ((?x9758 (stack_t x_0 x_ADDRESS x_BALANCE_0 3 ?x10141)))
 (let (($x6319 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_t 1)))) (_ bv0 1)))))
 (let (($x3508 (exc_halt_t 1)))
 (let (($x5252 (exc_halt_t 2)))
 (let (($x10693 (= $x5252 (or $x3508 $x6319))))
 (let (($x7596 (forall ((w (_ BitVec 256)) )(let ((?x9213 (storage_t x_0 x_ADDRESS x_BALANCE_0 1 w)))
 (let ((?x8624 (storage_t x_0 x_ADDRESS x_BALANCE_0 2 w)))
 (= ?x8624 ?x9213))))
 ))
 (let (($x9408 (forall ((n (_ BitVec 6)) )(let ((?x5759 (stack_t x_0 x_ADDRESS x_BALANCE_0 1 n)))
 (let ((?x10197 (stack_t x_0 x_ADDRESS x_BALANCE_0 2 n)))
 (let (($x8228 (= ?x10197 ?x5759)))
 (let ((?x8347 (sc_t 1)))
 (let (($x7612 (bvsle ?x8347 n)))
 (or $x7612 $x8228)))))))
 ))
 (let (($x6242 (= ?x2714 (bvadd (_ bv1 6) (sc_t 1)))))
 (let ((?x8490 (used_gas_t x_0 x_ADDRESS x_BALANCE_0 2)))
 (let (($x3709 (= $x3508 (or $x56 (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) ?x63)))))))
 (let (($x6949 (forall ((w (_ BitVec 256)) )(let ((?x9648 (storage_t x_0 x_ADDRESS x_BALANCE_0 0 w)))
 (let ((?x9213 (storage_t x_0 x_ADDRESS x_BALANCE_0 1 w)))
 (= ?x9213 ?x9648))))
 ))
 (let (($x2861 (forall ((n (_ BitVec 6)) )(let ((?x63 (sc_t 0)))
 (let ((?x2902 (bvadd (_ bv63 6) ?x63)))
 (let (($x5501 (bvsle ?x2902 n)))
 (let ((?x5452 (stack_t x_0 x_ADDRESS x_BALANCE_0 0 n)))
 (let ((?x5759 (stack_t x_0 x_ADDRESS x_BALANCE_0 1 n)))
 (let (($x251 (= ?x5759 ?x5452)))
 (or $x251 $x5501))))))))
 ))
 (let ((?x2902 (bvadd (_ bv63 6) ?x63)))
 (let ((?x8347 (sc_t 1)))
 (let (($x6944 (= ?x8347 ?x2902)))
 (let (($x2097 (= $x7172 (or (exc_halt_s 3) (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_s 3))))))))
 (let (($x1199 (forall ((w (_ BitVec 256)) )(let ((?x5814 (storage_s x_0 x_ADDRESS x_BALANCE_0 3 w)))
 (let ((?x10276 (storage_s x_0 x_ADDRESS x_BALANCE_0 4 w)))
 (= ?x10276 ?x5814))))
 ))
 (let (($x554 (forall ((n (_ BitVec 6)) )(let ((?x275 (sc_s 3)))
 (let ((?x314 (bvadd (_ bv63 6) ?x275)))
 (let (($x11762 (bvsle ?x314 n)))
 (let ((?x1594 (stack_s x_0 x_ADDRESS x_BALANCE_0 3 n)))
 (let ((?x5648 (stack_s x_0 x_ADDRESS x_BALANCE_0 4 n)))
 (let (($x9244 (= ?x5648 ?x1594)))
 (or $x9244 $x11762))))))))
 ))
 (let ((?x275 (sc_s 3)))
 (let ((?x314 (bvadd (_ bv63 6) ?x275)))
 (let (($x3937 (= ?x4305 ?x314)))
 (let ((?x4879 (used_gas_s x_0 x_ADDRESS x_BALANCE_0 4)))
 (let (($x292 (exc_halt_s 3)))
 (let (($x8083 (= $x292 (or (exc_halt_s 2) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 2))))))))
 (let (($x6878 (forall ((w (_ BitVec 256)) )(let ((?x11604 (storage_s x_0 x_ADDRESS x_BALANCE_0 2 w)))
 (let ((?x5814 (storage_s x_0 x_ADDRESS x_BALANCE_0 3 w)))
 (= ?x5814 ?x11604))))
 ))
 (let (($x8333 (forall ((n (_ BitVec 6)) )(let ((?x6544 (stack_s x_0 x_ADDRESS x_BALANCE_0 2 n)))
 (let ((?x1594 (stack_s x_0 x_ADDRESS x_BALANCE_0 3 n)))
 (let ((?x218 (sc_s 2)))
 (let ((?x3067 (bvadd (_ bv62 6) ?x218)))
 (let (($x5606 (bvsle ?x3067 n)))
 (or $x5606 (= ?x1594 ?x6544))))))))
 ))
 (let ((?x218 (sc_s 2)))
 (let (($x4933 (= ?x275 ?x218)))
 (let ((?x1061 (used_gas_s x_0 x_ADDRESS x_BALANCE_0 3)))
 (let (($x4016 (= ?x1061 (+ 3 (used_gas_s x_0 x_ADDRESS x_BALANCE_0 2)))))
 (let ((?x5789 (bvadd (_ bv63 6) ?x218)))
 (let ((?x7785 (stack_s x_0 x_ADDRESS x_BALANCE_0 2 ?x5789)))
 (let ((?x4190 (bvadd (_ bv62 6) ?x275)))
 (let ((?x1988 (stack_s x_0 x_ADDRESS x_BALANCE_0 3 ?x4190)))
 (let ((?x3067 (bvadd (_ bv62 6) ?x218)))
 (let ((?x2154 (stack_s x_0 x_ADDRESS x_BALANCE_0 2 ?x3067)))
 (let ((?x5893 (stack_s x_0 x_ADDRESS x_BALANCE_0 3 ?x314)))
 (let (($x247 (exc_halt_s 2)))
 (let (($x4452 (= $x247 (or (exc_halt_s 1) (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_s 1))))))))
 (let (($x3486 (forall ((w (_ BitVec 256)) )(let ((?x4264 (storage_s x_0 x_ADDRESS x_BALANCE_0 1 w)))
 (let ((?x11604 (storage_s x_0 x_ADDRESS x_BALANCE_0 2 w)))
 (= ?x11604 ?x4264))))
 ))
 (let (($x11800 (forall ((n (_ BitVec 6)) )(let ((?x154 (sc_s 1)))
 (let ((?x9355 (bvadd (_ bv63 6) ?x154)))
 (let (($x11667 (bvsle ?x9355 n)))
 (let ((?x931 (stack_s x_0 x_ADDRESS x_BALANCE_0 1 n)))
 (let ((?x6544 (stack_s x_0 x_ADDRESS x_BALANCE_0 2 n)))
 (let (($x9671 (= ?x6544 ?x931)))
 (or $x9671 $x11667))))))))
 ))
 (let ((?x154 (sc_s 1)))
 (let (($x2182 (= ?x218 ?x154)))
 (let ((?x2559 (used_gas_s x_0 x_ADDRESS x_BALANCE_0 2)))
 (let ((?x9355 (bvadd (_ bv63 6) ?x154)))
 (let ((?x5497 (stack_s x_0 x_ADDRESS x_BALANCE_0 1 ?x9355)))
 (let (($x189 (exc_halt_s 1)))
 (let (($x9787 (= $x189 (or $x57 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x72))) (_ bv0 1)))))))
 (let (($x11036 (forall ((w (_ BitVec 256)) )(let ((?x680 (storage_s x_0 x_ADDRESS x_BALANCE_0 0 w)))
 (let ((?x4264 (storage_s x_0 x_ADDRESS x_BALANCE_0 1 w)))
 (= ?x4264 ?x680))))
 ))
 (let (($x11055 (forall ((n (_ BitVec 6)) )(let ((?x10582 (stack_s x_0 x_ADDRESS x_BALANCE_0 0 n)))
 (let ((?x931 (stack_s x_0 x_ADDRESS x_BALANCE_0 1 n)))
 (let (($x6534 (= ?x931 ?x10582)))
 (let ((?x72 (sc_s 0)))
 (let (($x3549 (bvsle ?x72 n)))
 (or $x3549 $x6534)))))))
 ))
 (let (($x10046 (= ?x154 (bvadd (_ bv1 6) ?x72))))
 (let (($x8299 (forall ((w0 (_ BitVec 256)) )(let ((?x4910 (ite (= (stack_s x_0 x_ADDRESS x_BALANCE_0 1 (bvadd (_ bv63 6) (sc_s 1))) w0) x_BALANCE_0 (_ bv0 256))))
 (let ((?x5386 (f_BALANCE x_0 x_ADDRESS x_BALANCE_0 w0)))
 (= ?x5386 ?x4910))))
 ))
 (let (($x6579 (forall ((w (_ BitVec 256)) )(let ((?x680 (storage_s x_0 x_ADDRESS x_BALANCE_0 0 w)))
 (= ?x680 (_ bv0 256))))
 ))
 (let (($x10644 (= ?x5134 0)))
 (let (($x1248 (not $x57)))
 (let (($x8115 (= (stack_s x_0 x_ADDRESS x_BALANCE_0 0 (_ bv0 6)) x_0)))
 (let (($x8685 (= ?x72 (_ bv1 6))))
 (and $x8685 $x8115 $x1248 $x10644 $x6579 $x8299 (= (stack_s x_0 x_ADDRESS x_BALANCE_0 1 ?x72) x_ADDRESS) (= (used_gas_s x_0 x_ADDRESS x_BALANCE_0 1) (+ 2 ?x5134)) $x10046 $x11055 $x11036 $x9787 (= ?x7785 (f_BALANCE x_0 x_ADDRESS x_BALANCE_0 ?x5497)) (= ?x2559 (+ 400 (used_gas_s x_0 x_ADDRESS x_BALANCE_0 1))) $x2182 $x11800 $x3486 $x4452 (= ?x5893 ?x2154) (= ?x1988 ?x7785) $x4016 $x4933 $x8333 $x6878 $x8083 (= ?x4879 (+ 2 ?x1061)) $x3937 $x554 $x1199 $x2097 (= (used_gas_t x_0 x_ADDRESS x_BALANCE_0 1) (+ 2 ?x6809)) $x6944 $x2861 $x6949 $x3709 (= (stack_t x_0 x_ADDRESS x_BALANCE_0 2 ?x8347) x_ADDRESS) (= ?x8490 (+ 2 (used_gas_t x_0 x_ADDRESS x_BALANCE_0 1))) $x6242 $x9408 $x7596 $x10693 (= ?x9758 (f_BALANCE x_0 x_ADDRESS x_BALANCE_0 ?x9475)) (= ?x11511 (+ 400 ?x8490)) $x6299 $x4254 $x7234 $x5088 $x73 $x10075 $x58 $x9363 $x4890 (not (and $x4841 $x8342 $x5597 $x3669)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
 )
(check-sat)
(get-proof)