; PUSH cw_2 TIMESTAMP PUSH cw_3 SWAP1 SWAP2 SWAP1 => TIMESTAMP PUSH cw_2 PUSH cw_3
(set-option :produce-proofs true)
; 
(set-info :status unknown)
(declare-fun storage_t ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 256)) (_ BitVec 256))
(declare-fun w_3 () (_ BitVec 256))
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
 (exists ((x_TIMESTAMP (_ BitVec 256)) )(let (($x2299 (forall ((w (_ BitVec 256)) )(let ((?x9156 (storage_t w_2 w_3 x_TIMESTAMP 3 w)))
 (let ((?x1688 (storage_s w_2 w_3 x_TIMESTAMP 6 w)))
 (= ?x1688 ?x9156))))
 ))
 (let (($x4112 (exc_halt_t 3)))
 (let (($x772 (exc_halt_s 6)))
 (let (($x465 (= $x772 $x4112)))
 (let (($x5281 (forall ((n (_ BitVec 6)) )(let ((?x11964 (sc_t 3)))
 (let (($x5242 (bvsle ?x11964 n)))
 (let ((?x10540 (stack_t w_2 w_3 x_TIMESTAMP 3 n)))
 (let ((?x4304 (stack_s w_2 w_3 x_TIMESTAMP 6 n)))
 (let (($x14 (= ?x4304 ?x10540)))
 (or $x14 $x5242)))))))
 ))
 (let ((?x11964 (sc_t 3)))
 (let ((?x926 (sc_s 6)))
 (let (($x3054 (= ?x926 ?x11964)))
 (let ((?x672 (used_gas_t w_2 w_3 x_TIMESTAMP 0)))
 (let ((?x894 (used_gas_s w_2 w_3 x_TIMESTAMP 0)))
 (let (($x1995 (= ?x894 ?x672)))
 (let (($x8115 (forall ((w (_ BitVec 256)) )(let ((?x4759 (storage_t w_2 w_3 x_TIMESTAMP 0 w)))
 (let ((?x4054 (storage_s w_2 w_3 x_TIMESTAMP 0 w)))
 (= ?x4054 ?x4759))))
 ))
 (let (($x56 (exc_halt_t 0)))
 (let (($x57 (exc_halt_s 0)))
 (let (($x58 (= $x57 $x56)))
 (let (($x11544 (forall ((n (_ BitVec 6)) )(let ((?x63 (sc_t 0)))
 (let (($x5904 (bvsle ?x63 n)))
 (let ((?x1919 (stack_t w_2 w_3 x_TIMESTAMP 0 n)))
 (let ((?x9983 (stack_s w_2 w_3 x_TIMESTAMP 0 n)))
 (let (($x730 (= ?x9983 ?x1919)))
 (or $x730 $x5904)))))))
 ))
 (let ((?x63 (sc_t 0)))
 (let ((?x72 (sc_s 0)))
 (let (($x73 (= ?x72 ?x63)))
 (let (($x9580 (exc_halt_t 2)))
 (let (($x6774 (or $x9580 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_t 2)))) (_ bv0 1))))))
 (let (($x6697 (= $x4112 $x6774)))
 (let (($x3620 (forall ((w (_ BitVec 256)) )(let ((?x11792 (storage_t w_2 w_3 x_TIMESTAMP 2 w)))
 (let ((?x9156 (storage_t w_2 w_3 x_TIMESTAMP 3 w)))
 (= ?x9156 ?x11792))))
 ))
 (let (($x11834 (forall ((n (_ BitVec 6)) )(let ((?x10289 (stack_t w_2 w_3 x_TIMESTAMP 2 n)))
 (let ((?x10540 (stack_t w_2 w_3 x_TIMESTAMP 3 n)))
 (let ((?x2992 (sc_t 2)))
 (let (($x1399 (bvsle ?x2992 n)))
 (or $x1399 (= ?x10540 ?x10289)))))))
 ))
 (let (($x8488 (= ?x11964 (bvadd (_ bv1 6) (sc_t 2)))))
 (let (($x707 (= (used_gas_t w_2 w_3 x_TIMESTAMP 3) (+ 3 (used_gas_t w_2 w_3 x_TIMESTAMP 2)))))
 (let (($x408 (exc_halt_t 1)))
 (let (($x1895 (or $x408 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_t 1)))) (_ bv0 1))))))
 (let (($x216 (= $x9580 $x1895)))
 (let (($x4820 (forall ((w (_ BitVec 256)) )(let ((?x11706 (storage_t w_2 w_3 x_TIMESTAMP 1 w)))
 (let ((?x11792 (storage_t w_2 w_3 x_TIMESTAMP 2 w)))
 (= ?x11792 ?x11706))))
 ))
 (let (($x10300 (forall ((n (_ BitVec 6)) )(let ((?x3379 (sc_t 1)))
 (let (($x10655 (bvsle ?x3379 n)))
 (let ((?x7434 (stack_t w_2 w_3 x_TIMESTAMP 1 n)))
 (let ((?x10289 (stack_t w_2 w_3 x_TIMESTAMP 2 n)))
 (or (= ?x10289 ?x7434) $x10655))))))
 ))
 (let ((?x2992 (sc_t 2)))
 (let (($x10503 (= ?x2992 (bvadd (_ bv1 6) (sc_t 1)))))
 (let ((?x8346 (used_gas_t w_2 w_3 x_TIMESTAMP 2)))
 (let (($x11464 (= $x408 (or $x56 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x63))) (_ bv0 1)))))))
 (let (($x991 (forall ((w (_ BitVec 256)) )(let ((?x4759 (storage_t w_2 w_3 x_TIMESTAMP 0 w)))
 (let ((?x11706 (storage_t w_2 w_3 x_TIMESTAMP 1 w)))
 (= ?x11706 ?x4759))))
 ))
 (let (($x8564 (forall ((n (_ BitVec 6)) )(let ((?x63 (sc_t 0)))
 (let (($x5904 (bvsle ?x63 n)))
 (let ((?x1919 (stack_t w_2 w_3 x_TIMESTAMP 0 n)))
 (let ((?x7434 (stack_t w_2 w_3 x_TIMESTAMP 1 n)))
 (or (= ?x7434 ?x1919) $x5904))))))
 ))
 (let ((?x3379 (sc_t 1)))
 (let (($x8894 (= ?x3379 (bvadd (_ bv1 6) ?x63))))
 (let (($x824 (= $x772 (or (exc_halt_s 5) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 5))))))))
 (let (($x11414 (forall ((w (_ BitVec 256)) )(let ((?x8075 (storage_s w_2 w_3 x_TIMESTAMP 5 w)))
 (let ((?x1688 (storage_s w_2 w_3 x_TIMESTAMP 6 w)))
 (= ?x1688 ?x8075))))
 ))
 (let (($x7859 (forall ((n (_ BitVec 6)) )(let ((?x9570 (stack_s w_2 w_3 x_TIMESTAMP 5 n)))
 (let ((?x4304 (stack_s w_2 w_3 x_TIMESTAMP 6 n)))
 (or (= ?x4304 ?x9570) (bvsle (bvadd (_ bv62 6) (sc_s 5)) n)))))
 ))
 (let (($x7685 (= (used_gas_s w_2 w_3 x_TIMESTAMP 6) (+ 3 (used_gas_s w_2 w_3 x_TIMESTAMP 5)))))
 (let ((?x4319 (sc_s 5)))
 (let ((?x5984 (bvadd (_ bv63 6) ?x4319)))
 (let ((?x4418 (stack_s w_2 w_3 x_TIMESTAMP 5 ?x5984)))
 (let ((?x3655 (bvadd (_ bv62 6) ?x4319)))
 (let ((?x1604 (stack_s w_2 w_3 x_TIMESTAMP 5 ?x3655)))
 (let (($x1862 (exc_halt_s 5)))
 (let (($x11327 (= $x1862 (or (exc_halt_s 4) (not (bvsle (_ bv0 6) (bvadd (_ bv61 6) (sc_s 4))))))))
 (let (($x11815 (forall ((w (_ BitVec 256)) )(let ((?x706 (storage_s w_2 w_3 x_TIMESTAMP 4 w)))
 (let ((?x8075 (storage_s w_2 w_3 x_TIMESTAMP 5 w)))
 (= ?x8075 ?x706))))
 ))
 (let (($x4400 (forall ((n (_ BitVec 6)) )(let ((?x1050 (stack_s w_2 w_3 x_TIMESTAMP 4 n)))
 (let ((?x9570 (stack_s w_2 w_3 x_TIMESTAMP 5 n)))
 (or (= ?x9570 ?x1050) (bvsle (bvadd (_ bv61 6) (sc_s 4)) n)))))
 ))
 (let ((?x3145 (sc_s 4)))
 (let (($x9024 (= ?x4319 ?x3145)))
 (let ((?x5362 (used_gas_s w_2 w_3 x_TIMESTAMP 5)))
 (let ((?x4151 (bvadd (_ bv63 6) ?x3145)))
 (let ((?x11668 (stack_s w_2 w_3 x_TIMESTAMP 4 ?x4151)))
 (let (($x276 (exc_halt_s 4)))
 (let (($x467 (= $x276 (or (exc_halt_s 3) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 3))))))))
 (let (($x11956 (forall ((w (_ BitVec 256)) )(let ((?x8090 (storage_s w_2 w_3 x_TIMESTAMP 3 w)))
 (let ((?x706 (storage_s w_2 w_3 x_TIMESTAMP 4 w)))
 (= ?x706 ?x8090))))
 ))
 (let (($x6622 (forall ((n (_ BitVec 6)) )(let ((?x4403 (stack_s w_2 w_3 x_TIMESTAMP 3 n)))
 (let ((?x1050 (stack_s w_2 w_3 x_TIMESTAMP 4 n)))
 (let ((?x4554 (sc_s 3)))
 (let ((?x7881 (bvadd (_ bv62 6) ?x4554)))
 (let (($x1229 (bvsle ?x7881 n)))
 (or $x1229 (= ?x1050 ?x4403))))))))
 ))
 (let ((?x4554 (sc_s 3)))
 (let (($x353 (= ?x3145 ?x4554)))
 (let ((?x5143 (used_gas_s w_2 w_3 x_TIMESTAMP 4)))
 (let ((?x10403 (bvadd (_ bv62 6) ?x3145)))
 (let ((?x8675 (stack_s w_2 w_3 x_TIMESTAMP 4 ?x10403)))
 (let (($x7873 (exc_halt_s 2)))
 (let (($x4225 (or $x7873 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 2)))) (_ bv0 1))))))
 (let (($x8777 (exc_halt_s 3)))
 (let (($x8915 (= $x8777 $x4225)))
 (let (($x10217 (forall ((w (_ BitVec 256)) )(let ((?x10310 (storage_s w_2 w_3 x_TIMESTAMP 2 w)))
 (let ((?x8090 (storage_s w_2 w_3 x_TIMESTAMP 3 w)))
 (= ?x8090 ?x10310))))
 ))
 (let (($x9453 (forall ((n (_ BitVec 6)) )(let ((?x6605 (stack_s w_2 w_3 x_TIMESTAMP 2 n)))
 (let ((?x4403 (stack_s w_2 w_3 x_TIMESTAMP 3 n)))
 (let ((?x7378 (sc_s 2)))
 (let (($x5791 (bvsle ?x7378 n)))
 (or $x5791 (= ?x4403 ?x6605)))))))
 ))
 (let (($x2780 (= ?x4554 (bvadd (_ bv1 6) (sc_s 2)))))
 (let ((?x3156 (used_gas_s w_2 w_3 x_TIMESTAMP 3)))
 (let (($x4172 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 1)))) (_ bv0 1)))))
 (let (($x2589 (exc_halt_s 1)))
 (let (($x3268 (forall ((w (_ BitVec 256)) )(let ((?x3525 (storage_s w_2 w_3 x_TIMESTAMP 1 w)))
 (let ((?x10310 (storage_s w_2 w_3 x_TIMESTAMP 2 w)))
 (= ?x10310 ?x3525))))
 ))
 (let (($x2842 (forall ((n (_ BitVec 6)) )(let ((?x3583 (stack_s w_2 w_3 x_TIMESTAMP 1 n)))
 (let ((?x6605 (stack_s w_2 w_3 x_TIMESTAMP 2 n)))
 (or (bvsle (sc_s 1) n) (= ?x6605 ?x3583)))))
 ))
 (let ((?x7378 (sc_s 2)))
 (let (($x10133 (= ?x7378 (bvadd (_ bv1 6) (sc_s 1)))))
 (let ((?x3242 (used_gas_s w_2 w_3 x_TIMESTAMP 2)))
 (let (($x10668 (= $x2589 (or $x57 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x72))) (_ bv0 1)))))))
 (let (($x3687 (forall ((w (_ BitVec 256)) )(let ((?x4054 (storage_s w_2 w_3 x_TIMESTAMP 0 w)))
 (let ((?x3525 (storage_s w_2 w_3 x_TIMESTAMP 1 w)))
 (= ?x3525 ?x4054))))
 ))
 (let (($x11594 (forall ((n (_ BitVec 6)) )(let ((?x72 (sc_s 0)))
 (let (($x9529 (bvsle ?x72 n)))
 (let ((?x9983 (stack_s w_2 w_3 x_TIMESTAMP 0 n)))
 (let ((?x3583 (stack_s w_2 w_3 x_TIMESTAMP 1 n)))
 (or (= ?x3583 ?x9983) $x9529))))))
 ))
 (let ((?x154 (sc_s 1)))
 (let (($x5498 (= ?x154 (bvadd (_ bv1 6) ?x72))))
 (let (($x4398 (forall ((w (_ BitVec 256)) )(let ((?x4054 (storage_s w_2 w_3 x_TIMESTAMP 0 w)))
 (= ?x4054 (_ bv0 256))))
 ))
 (let (($x11538 (= ?x894 0)))
 (let (($x11010 (not $x57)))
 (let (($x1972 (= ?x72 (_ bv0 6))))
 (and $x1972 $x11010 $x11538 $x4398 (= (stack_s w_2 w_3 x_TIMESTAMP 1 ?x72) w_2) (= (used_gas_s w_2 w_3 x_TIMESTAMP 1) (+ 3 ?x894)) $x5498 $x11594 $x3687 $x10668 (= (stack_s w_2 w_3 x_TIMESTAMP 2 ?x154) x_TIMESTAMP) (= ?x3242 (+ 2 (used_gas_s w_2 w_3 x_TIMESTAMP 1))) $x10133 $x2842 $x3268 (= $x7873 (or $x2589 $x4172)) (= (stack_s w_2 w_3 x_TIMESTAMP 3 ?x7378) w_3) (= ?x3156 (+ 3 ?x3242)) $x2780 $x9453 $x10217 $x8915 (= ?x11668 (stack_s w_2 w_3 x_TIMESTAMP 3 (bvadd (_ bv62 6) ?x4554))) (= ?x8675 (stack_s w_2 w_3 x_TIMESTAMP 3 (bvadd (_ bv63 6) ?x4554))) (= ?x5143 (+ 3 ?x3156)) $x353 $x6622 $x11956 $x467 (= ?x4418 (stack_s w_2 w_3 x_TIMESTAMP 4 (bvadd (_ bv61 6) ?x3145))) (= (stack_s w_2 w_3 x_TIMESTAMP 5 (bvadd (_ bv61 6) ?x4319)) ?x11668) (= ?x1604 ?x8675) (= ?x5362 (+ 3 ?x5143)) $x9024 $x4400 $x11815 $x11327 (= (stack_s w_2 w_3 x_TIMESTAMP 6 (bvadd (_ bv63 6) ?x926)) ?x1604) (= (stack_s w_2 w_3 x_TIMESTAMP 6 (bvadd (_ bv62 6) ?x926)) ?x4418) $x7685 (= ?x926 ?x4319) $x7859 $x11414 $x824 (= (stack_t w_2 w_3 x_TIMESTAMP 1 ?x63) x_TIMESTAMP) (= (used_gas_t w_2 w_3 x_TIMESTAMP 1) (+ 2 ?x672)) $x8894 $x8564 $x991 $x11464 (= (stack_t w_2 w_3 x_TIMESTAMP 2 ?x3379) w_2) (= ?x8346 (+ 3 (used_gas_t w_2 w_3 x_TIMESTAMP 1))) $x10503 $x10300 $x4820 $x216 (= (stack_t w_2 w_3 x_TIMESTAMP 3 ?x2992) w_3) $x707 $x8488 $x11834 $x3620 $x6697 $x73 $x11544 $x58 $x8115 $x1995 (not (and $x3054 $x5281 $x465 $x2299))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
 )
(check-sat)
(get-proof)
