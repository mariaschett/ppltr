; PUSH cw_2 DUP1 SLOAD SWAP3 SWAP1 => PUSH cw_2 SLOAD SWAP2 PUSH cw_2
(set-option :produce-proofs true)
; 
(set-info :status unknown)
(declare-fun storage_t ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 256)) (_ BitVec 256))
(declare-fun w_2 () (_ BitVec 256))
(declare-fun storage_s ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 256)) (_ BitVec 256))
(declare-fun exc_halt_t (Int) Bool)
(declare-fun exc_halt_s (Int) Bool)
(declare-fun sc_t (Int) (_ BitVec 6))
(declare-fun stack_t ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 6)) (_ BitVec 256))
(declare-fun stack_s ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 6)) (_ BitVec 256))
(declare-fun sc_s (Int) (_ BitVec 6))
(declare-fun used_gas_t ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int) Int)
(declare-fun used_gas_s ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int) Int)
(assert
 (exists ((x_0 (_ BitVec 256)) (x_1 (_ BitVec 256)) (x_SLOAD_0 (_ BitVec 256)) )(let (($x6399 (forall ((w (_ BitVec 256)) )(let ((?x5935 (storage_t x_0 x_1 x_SLOAD_0 w_2 4 w)))
 (let ((?x8079 (storage_s x_0 x_1 x_SLOAD_0 w_2 5 w)))
 (= ?x8079 ?x5935))))
 ))
 (let (($x7854 (exc_halt_t 4)))
 (let (($x1862 (exc_halt_s 5)))
 (let (($x4014 (= $x1862 $x7854)))
 (let (($x1889 (forall ((n (_ BitVec 6)) )(let ((?x10058 (stack_t x_0 x_1 x_SLOAD_0 w_2 4 n)))
 (let ((?x5375 (stack_s x_0 x_1 x_SLOAD_0 w_2 5 n)))
 (let (($x1506 (= ?x5375 ?x10058)))
 (or $x1506 (bvsle (sc_t 4) n))))))
 ))
 (let ((?x1366 (sc_t 4)))
 (let ((?x4319 (sc_s 5)))
 (let (($x2251 (= ?x4319 ?x1366)))
 (let ((?x1144 (used_gas_t x_0 x_1 x_SLOAD_0 w_2 0)))
 (let ((?x9258 (used_gas_s x_0 x_1 x_SLOAD_0 w_2 0)))
 (let (($x4409 (= ?x9258 ?x1144)))
 (let (($x4908 (forall ((w (_ BitVec 256)) )(let ((?x747 (storage_t x_0 x_1 x_SLOAD_0 w_2 0 w)))
 (let ((?x10883 (storage_s x_0 x_1 x_SLOAD_0 w_2 0 w)))
 (= ?x10883 ?x747))))
 ))
 (let (($x56 (exc_halt_t 0)))
 (let (($x57 (exc_halt_s 0)))
 (let (($x58 (= $x57 $x56)))
 (let (($x3934 (forall ((n (_ BitVec 6)) )(let ((?x1325 (stack_t x_0 x_1 x_SLOAD_0 w_2 0 n)))
 (let ((?x5767 (stack_s x_0 x_1 x_SLOAD_0 w_2 0 n)))
 (let (($x7941 (= ?x5767 ?x1325)))
 (let ((?x63 (sc_t 0)))
 (let (($x5904 (bvsle ?x63 n)))
 (or $x5904 $x7941)))))))
 ))
 (let ((?x63 (sc_t 0)))
 (let ((?x72 (sc_s 0)))
 (let (($x73 (= ?x72 ?x63)))
 (let (($x4112 (exc_halt_t 3)))
 (let (($x37 (or $x4112 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_t 3)))) (_ bv0 1))))))
 (let (($x11390 (forall ((w (_ BitVec 256)) )(let ((?x3732 (storage_t x_0 x_1 x_SLOAD_0 w_2 3 w)))
 (let ((?x5935 (storage_t x_0 x_1 x_SLOAD_0 w_2 4 w)))
 (= ?x5935 ?x3732))))
 ))
 (let (($x1882 (forall ((n (_ BitVec 6)) )(let ((?x1757 (stack_t x_0 x_1 x_SLOAD_0 w_2 3 n)))
 (let ((?x10058 (stack_t x_0 x_1 x_SLOAD_0 w_2 4 n)))
 (let ((?x11964 (sc_t 3)))
 (let (($x5242 (bvsle ?x11964 n)))
 (or $x5242 (= ?x10058 ?x1757)))))))
 ))
 (let (($x3577 (= (used_gas_t x_0 x_1 x_SLOAD_0 w_2 4) (+ 3 (used_gas_t x_0 x_1 x_SLOAD_0 w_2 3)))))
 (let (($x1753 (= $x4112 (or (exc_halt_t 2) (not (bvsle (_ bv0 6) (bvadd (_ bv61 6) (sc_t 2))))))))
 (let (($x653 (forall ((w (_ BitVec 256)) )(let ((?x10635 (storage_t x_0 x_1 x_SLOAD_0 w_2 2 w)))
 (let ((?x3732 (storage_t x_0 x_1 x_SLOAD_0 w_2 3 w)))
 (= ?x3732 ?x10635))))
 ))
 (let (($x4926 (forall ((n (_ BitVec 6)) )(let ((?x5006 (stack_t x_0 x_1 x_SLOAD_0 w_2 2 n)))
 (let ((?x1757 (stack_t x_0 x_1 x_SLOAD_0 w_2 3 n)))
 (let ((?x2992 (sc_t 2)))
 (let ((?x5555 (bvadd (_ bv61 6) ?x2992)))
 (let (($x2102 (bvsle ?x5555 n)))
 (or $x2102 (= ?x1757 ?x5006))))))))
 ))
 (let ((?x2992 (sc_t 2)))
 (let ((?x11964 (sc_t 3)))
 (let (($x11604 (= ?x11964 ?x2992)))
 (let ((?x4081 (used_gas_t x_0 x_1 x_SLOAD_0 w_2 3)))
 (let (($x5973 (= (stack_t x_0 x_1 x_SLOAD_0 w_2 3 (bvadd (_ bv62 6) ?x11964)) (stack_t x_0 x_1 x_SLOAD_0 w_2 2 (bvadd (_ bv62 6) ?x2992)))))
 (let ((?x2620 (bvadd (_ bv63 6) ?x2992)))
 (let ((?x6817 (stack_t x_0 x_1 x_SLOAD_0 w_2 2 ?x2620)))
 (let (($x4549 (= (stack_t x_0 x_1 x_SLOAD_0 w_2 3 (bvadd (_ bv63 6) ?x11964)) (stack_t x_0 x_1 x_SLOAD_0 w_2 2 (bvadd (_ bv61 6) ?x2992)))))
 (let (($x9580 (exc_halt_t 2)))
 (let (($x11009 (= $x9580 (or (exc_halt_t 1) (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_t 1))))))))
 (let (($x10239 (forall ((w (_ BitVec 256)) )(let ((?x10829 (storage_t x_0 x_1 x_SLOAD_0 w_2 1 w)))
 (let ((?x10635 (storage_t x_0 x_1 x_SLOAD_0 w_2 2 w)))
 (= ?x10635 ?x10829))))
 ))
 (let (($x6980 (forall ((n (_ BitVec 6)) )(let ((?x3881 (stack_t x_0 x_1 x_SLOAD_0 w_2 1 n)))
 (let ((?x5006 (stack_t x_0 x_1 x_SLOAD_0 w_2 2 n)))
 (or (= ?x5006 ?x3881) (bvsle (bvadd (_ bv63 6) (sc_t 1)) n)))))
 ))
 (let ((?x1268 (used_gas_t x_0 x_1 x_SLOAD_0 w_2 2)))
 (let ((?x1694 (storage_t x_0 x_1 x_SLOAD_0 w_2 1 (stack_t x_0 x_1 x_SLOAD_0 w_2 1 (bvadd (_ bv63 6) (sc_t 1))))))
 (let (($x408 (exc_halt_t 1)))
 (let (($x11464 (= $x408 (or $x56 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x63))) (_ bv0 1)))))))
 (let (($x10490 (forall ((w (_ BitVec 256)) )(let ((?x747 (storage_t x_0 x_1 x_SLOAD_0 w_2 0 w)))
 (let ((?x10829 (storage_t x_0 x_1 x_SLOAD_0 w_2 1 w)))
 (= ?x10829 ?x747))))
 ))
 (let (($x8266 (forall ((n (_ BitVec 6)) )(let ((?x63 (sc_t 0)))
 (let (($x5904 (bvsle ?x63 n)))
 (let ((?x1325 (stack_t x_0 x_1 x_SLOAD_0 w_2 0 n)))
 (let ((?x3881 (stack_t x_0 x_1 x_SLOAD_0 w_2 1 n)))
 (or (= ?x3881 ?x1325) $x5904))))))
 ))
 (let ((?x3379 (sc_t 1)))
 (let (($x8894 (= ?x3379 (bvadd (_ bv1 6) ?x63))))
 (let (($x10662 (= $x1862 (or (exc_halt_s 4) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 4))))))))
 (let (($x1275 (forall ((w (_ BitVec 256)) )(let ((?x4746 (storage_s x_0 x_1 x_SLOAD_0 w_2 4 w)))
 (let ((?x8079 (storage_s x_0 x_1 x_SLOAD_0 w_2 5 w)))
 (= ?x8079 ?x4746))))
 ))
 (let (($x8355 (forall ((n (_ BitVec 6)) )(let ((?x2839 (stack_s x_0 x_1 x_SLOAD_0 w_2 4 n)))
 (let ((?x5375 (stack_s x_0 x_1 x_SLOAD_0 w_2 5 n)))
 (or (= ?x5375 ?x2839) (bvsle (bvadd (_ bv62 6) (sc_s 4)) n)))))
 ))
 (let (($x243 (= (used_gas_s x_0 x_1 x_SLOAD_0 w_2 5) (+ 3 (used_gas_s x_0 x_1 x_SLOAD_0 w_2 4)))))
 (let ((?x3145 (sc_s 4)))
 (let ((?x4151 (bvadd (_ bv63 6) ?x3145)))
 (let ((?x11874 (stack_s x_0 x_1 x_SLOAD_0 w_2 4 ?x4151)))
 (let ((?x10403 (bvadd (_ bv62 6) ?x3145)))
 (let ((?x5941 (stack_s x_0 x_1 x_SLOAD_0 w_2 4 ?x10403)))
 (let (($x276 (exc_halt_s 4)))
 (let (($x5072 (= $x276 (or (exc_halt_s 3) (not (bvsle (_ bv0 6) (bvadd (_ bv60 6) (sc_s 3))))))))
 (let (($x7902 (forall ((w (_ BitVec 256)) )(let ((?x1483 (storage_s x_0 x_1 x_SLOAD_0 w_2 3 w)))
 (let ((?x4746 (storage_s x_0 x_1 x_SLOAD_0 w_2 4 w)))
 (= ?x4746 ?x1483))))
 ))
 (let (($x8627 (forall ((n (_ BitVec 6)) )(let ((?x3961 (stack_s x_0 x_1 x_SLOAD_0 w_2 3 n)))
 (let ((?x2839 (stack_s x_0 x_1 x_SLOAD_0 w_2 4 n)))
 (or (= ?x2839 ?x3961) (bvsle (bvadd (_ bv60 6) (sc_s 3)) n)))))
 ))
 (let ((?x4554 (sc_s 3)))
 (let (($x353 (= ?x3145 ?x4554)))
 (let ((?x331 (used_gas_s x_0 x_1 x_SLOAD_0 w_2 4)))
 (let (($x2696 (= (stack_s x_0 x_1 x_SLOAD_0 w_2 4 (bvadd (_ bv61 6) ?x3145)) (stack_s x_0 x_1 x_SLOAD_0 w_2 3 (bvadd (_ bv61 6) ?x4554)))))
 (let ((?x5619 (bvadd (_ bv63 6) ?x4554)))
 (let ((?x6131 (stack_s x_0 x_1 x_SLOAD_0 w_2 3 ?x5619)))
 (let (($x8777 (exc_halt_s 3)))
 (let (($x9861 (= $x8777 (or (exc_halt_s 2) (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_s 2))))))))
 (let (($x11204 (forall ((w (_ BitVec 256)) )(let ((?x1326 (storage_s x_0 x_1 x_SLOAD_0 w_2 2 w)))
 (let ((?x1483 (storage_s x_0 x_1 x_SLOAD_0 w_2 3 w)))
 (= ?x1483 ?x1326))))
 ))
 (let (($x6634 (forall ((n (_ BitVec 6)) )(let ((?x8097 (stack_s x_0 x_1 x_SLOAD_0 w_2 2 n)))
 (let ((?x3961 (stack_s x_0 x_1 x_SLOAD_0 w_2 3 n)))
 (or (bvsle (bvadd (_ bv63 6) (sc_s 2)) n) (= ?x3961 ?x8097)))))
 ))
 (let ((?x11409 (used_gas_s x_0 x_1 x_SLOAD_0 w_2 3)))
 (let ((?x7378 (sc_s 2)))
 (let ((?x121 (bvadd (_ bv63 6) ?x7378)))
 (let ((?x2119 (stack_s x_0 x_1 x_SLOAD_0 w_2 2 ?x121)))
 (let (($x2589 (exc_halt_s 1)))
 (let (($x7776 (or $x2589 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 1)))) (_ bv0 1))) (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_s 1)))))))
 (let (($x7873 (exc_halt_s 2)))
 (let (($x11786 (forall ((w (_ BitVec 256)) )(let ((?x8033 (storage_s x_0 x_1 x_SLOAD_0 w_2 1 w)))
 (let ((?x1326 (storage_s x_0 x_1 x_SLOAD_0 w_2 2 w)))
 (= ?x1326 ?x8033))))
 ))
 (let (($x9696 (forall ((n (_ BitVec 6)) )(let ((?x3134 (stack_s x_0 x_1 x_SLOAD_0 w_2 1 n)))
 (let ((?x8097 (stack_s x_0 x_1 x_SLOAD_0 w_2 2 n)))
 (or (bvsle (bvadd (_ bv63 6) (sc_s 1)) n) (= ?x8097 ?x3134)))))
 ))
 (let ((?x4850 (used_gas_s x_0 x_1 x_SLOAD_0 w_2 2)))
 (let ((?x154 (sc_s 1)))
 (let ((?x4689 (bvadd (_ bv63 6) ?x154)))
 (let ((?x4877 (stack_s x_0 x_1 x_SLOAD_0 w_2 1 ?x4689)))
 (let (($x10668 (= $x2589 (or $x57 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x72))) (_ bv0 1)))))))
 (let (($x1648 (forall ((w (_ BitVec 256)) )(let ((?x10883 (storage_s x_0 x_1 x_SLOAD_0 w_2 0 w)))
 (let ((?x8033 (storage_s x_0 x_1 x_SLOAD_0 w_2 1 w)))
 (= ?x8033 ?x10883))))
 ))
 (let (($x769 (forall ((n (_ BitVec 6)) )(let ((?x72 (sc_s 0)))
 (let (($x9529 (bvsle ?x72 n)))
 (let ((?x5767 (stack_s x_0 x_1 x_SLOAD_0 w_2 0 n)))
 (let ((?x3134 (stack_s x_0 x_1 x_SLOAD_0 w_2 1 n)))
 (or (= ?x3134 ?x5767) $x9529))))))
 ))
 (let (($x5498 (= ?x154 (bvadd (_ bv1 6) ?x72))))
 (let (($x4993 (forall ((w (_ BitVec 256)) )(let (($x6593 (= w (stack_s x_0 x_1 x_SLOAD_0 w_2 2 (bvadd (_ bv63 6) (sc_s 2))))))
 (let ((?x10883 (storage_s x_0 x_1 x_SLOAD_0 w_2 0 w)))
 (= ?x10883 (ite $x6593 x_SLOAD_0 (_ bv0 256))))))
 ))
 (let (($x4589 (= ?x9258 0)))
 (let (($x11010 (not $x57)))
 (let (($x6517 (= (stack_s x_0 x_1 x_SLOAD_0 w_2 0 (_ bv1 6)) x_1)))
 (let (($x11579 (= (stack_s x_0 x_1 x_SLOAD_0 w_2 0 (_ bv0 6)) x_0)))
 (let (($x11128 (= ?x72 (_ bv2 6))))
 (and $x11128 $x11579 $x6517 $x11010 $x4589 $x4993 (= (stack_s x_0 x_1 x_SLOAD_0 w_2 1 ?x72) w_2) (= (used_gas_s x_0 x_1 x_SLOAD_0 w_2 1) (+ 3 ?x9258)) $x5498 $x769 $x1648 $x10668 (= ?x2119 ?x4877) (= (stack_s x_0 x_1 x_SLOAD_0 w_2 2 ?x4689) ?x4877) (= ?x4850 (+ 3 (used_gas_s x_0 x_1 x_SLOAD_0 w_2 1))) (= ?x7378 (bvadd (_ bv1 6) ?x154)) $x9696 $x11786 (= $x7873 $x7776) (= ?x6131 (storage_s x_0 x_1 x_SLOAD_0 w_2 2 ?x2119)) (= ?x11409 (+ 200 ?x4850)) (= ?x4554 ?x7378) $x6634 $x11204 $x9861 (= ?x11874 (stack_s x_0 x_1 x_SLOAD_0 w_2 3 (bvadd (_ bv60 6) ?x4554))) (= (stack_s x_0 x_1 x_SLOAD_0 w_2 4 (bvadd (_ bv60 6) ?x3145)) ?x6131) $x2696 (= ?x5941 (stack_s x_0 x_1 x_SLOAD_0 w_2 3 (bvadd (_ bv62 6) ?x4554))) (= ?x331 (+ 3 ?x11409)) $x353 $x8627 $x7902 $x5072 (= (stack_s x_0 x_1 x_SLOAD_0 w_2 5 (bvadd (_ bv63 6) ?x4319)) ?x5941) (= (stack_s x_0 x_1 x_SLOAD_0 w_2 5 (bvadd (_ bv62 6) ?x4319)) ?x11874) $x243 (= ?x4319 ?x3145) $x8355 $x1275 $x10662 (= (stack_t x_0 x_1 x_SLOAD_0 w_2 1 ?x63) w_2) (= (used_gas_t x_0 x_1 x_SLOAD_0 w_2 1) (+ 3 ?x1144)) $x8894 $x8266 $x10490 $x11464 (= ?x6817 ?x1694) (= ?x1268 (+ 200 (used_gas_t x_0 x_1 x_SLOAD_0 w_2 1))) (= ?x2992 ?x3379) $x6980 $x10239 $x11009 $x4549 (= (stack_t x_0 x_1 x_SLOAD_0 w_2 3 (bvadd (_ bv61 6) ?x11964)) ?x6817) $x5973 (= ?x4081 (+ 3 ?x1268)) $x11604 $x4926 $x653 $x1753 (= (stack_t x_0 x_1 x_SLOAD_0 w_2 4 ?x11964) w_2) $x3577 (= ?x1366 (bvadd (_ bv1 6) ?x11964)) $x1882 $x11390 (= $x7854 $x37) $x73 $x3934 $x58 $x4908 $x4409 (not (and $x2251 $x1889 $x4014 $x6399)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
 )
(check-sat)
(get-proof)