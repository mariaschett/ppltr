; DUP1 SLOAD SWAP2 POP POP SWAP2 => SLOAD SWAP3 SWAP1 NOT POP
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
(assert
 (exists ((x_0 (_ BitVec 256)) (x_1 (_ BitVec 256)) (x_2 (_ BitVec 256)) (x_3 (_ BitVec 256)) (x_SLOAD_0 (_ BitVec 256)) )(let (($x246 (forall ((w (_ BitVec 256)) )(let ((?x6743 (storage_t x_0 x_1 x_2 x_3 x_SLOAD_0 5 w)))
 (let ((?x3102 (storage_s x_0 x_1 x_2 x_3 x_SLOAD_0 6 w)))
 (= ?x3102 ?x6743))))
 ))
 (let (($x886 (exc_halt_t 5)))
 (let (($x772 (exc_halt_s 6)))
 (let (($x889 (= $x772 $x886)))
 (let (($x4603 (forall ((n (_ BitVec 6)) )(let ((?x3696 (stack_t x_0 x_1 x_2 x_3 x_SLOAD_0 5 n)))
 (let ((?x7138 (stack_s x_0 x_1 x_2 x_3 x_SLOAD_0 6 n)))
 (let (($x9335 (= ?x7138 ?x3696)))
 (let ((?x919 (sc_t 5)))
 (let (($x8015 (bvsle ?x919 n)))
 (or $x8015 $x9335)))))))
 ))
 (let ((?x919 (sc_t 5)))
 (let ((?x926 (sc_s 6)))
 (let (($x929 (= ?x926 ?x919)))
 (let ((?x1941 (used_gas_t x_0 x_1 x_2 x_3 x_SLOAD_0 0)))
 (let ((?x8289 (used_gas_s x_0 x_1 x_2 x_3 x_SLOAD_0 0)))
 (let (($x9477 (= ?x8289 ?x1941)))
 (let (($x9773 (forall ((w (_ BitVec 256)) )(let ((?x793 (storage_t x_0 x_1 x_2 x_3 x_SLOAD_0 0 w)))
 (let ((?x10766 (storage_s x_0 x_1 x_2 x_3 x_SLOAD_0 0 w)))
 (= ?x10766 ?x793))))
 ))
 (let (($x56 (exc_halt_t 0)))
 (let (($x57 (exc_halt_s 0)))
 (let (($x58 (= $x57 $x56)))
 (let (($x6131 (forall ((n (_ BitVec 6)) )(let ((?x63 (sc_t 0)))
 (let (($x10077 (bvsle ?x63 n)))
 (let ((?x2929 (stack_t x_0 x_1 x_2 x_3 x_SLOAD_0 0 n)))
 (let ((?x1054 (stack_s x_0 x_1 x_2 x_3 x_SLOAD_0 0 n)))
 (let (($x9269 (= ?x1054 ?x2929)))
 (or $x9269 $x10077)))))))
 ))
 (let ((?x63 (sc_t 0)))
 (let ((?x72 (sc_s 0)))
 (let (($x73 (= ?x72 ?x63)))
 (let (($x4316 (= $x886 (or (exc_halt_t 4) (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_t 4))))))))
 (let (($x8668 (forall ((w (_ BitVec 256)) )(let ((?x10570 (storage_t x_0 x_1 x_2 x_3 x_SLOAD_0 4 w)))
 (let ((?x6743 (storage_t x_0 x_1 x_2 x_3 x_SLOAD_0 5 w)))
 (= ?x6743 ?x10570))))
 ))
 (let (($x2218 (forall ((n (_ BitVec 6)) )(let ((?x9708 (stack_t x_0 x_1 x_2 x_3 x_SLOAD_0 4 n)))
 (let ((?x3696 (stack_t x_0 x_1 x_2 x_3 x_SLOAD_0 5 n)))
 (let ((?x3757 (sc_t 4)))
 (let ((?x4637 (bvadd (_ bv63 6) ?x3757)))
 (let (($x8557 (bvsle ?x4637 n)))
 (or $x8557 (= ?x3696 ?x9708))))))))
 ))
 (let (($x5092 (= (used_gas_t x_0 x_1 x_2 x_3 x_SLOAD_0 5) (+ 2 (used_gas_t x_0 x_1 x_2 x_3 x_SLOAD_0 4)))))
 (let (($x3723 (exc_halt_t 4)))
 (let (($x10208 (= $x3723 (or (exc_halt_t 3) (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_t 3))))))))
 (let (($x888 (forall ((w (_ BitVec 256)) )(let ((?x5234 (storage_t x_0 x_1 x_2 x_3 x_SLOAD_0 3 w)))
 (let ((?x10570 (storage_t x_0 x_1 x_2 x_3 x_SLOAD_0 4 w)))
 (= ?x10570 ?x5234))))
 ))
 (let (($x3718 (forall ((n (_ BitVec 6)) )(let ((?x186 (stack_t x_0 x_1 x_2 x_3 x_SLOAD_0 3 n)))
 (let ((?x9708 (stack_t x_0 x_1 x_2 x_3 x_SLOAD_0 4 n)))
 (let ((?x3005 (sc_t 3)))
 (let ((?x4707 (bvadd (_ bv63 6) ?x3005)))
 (let (($x1150 (bvsle ?x4707 n)))
 (or $x1150 (= ?x9708 ?x186))))))))
 ))
 (let ((?x3005 (sc_t 3)))
 (let ((?x3757 (sc_t 4)))
 (let (($x134 (= ?x3757 ?x3005)))
 (let ((?x9072 (used_gas_t x_0 x_1 x_2 x_3 x_SLOAD_0 4)))
 (let (($x6642 (= (stack_t x_0 x_1 x_2 x_3 x_SLOAD_0 4 (bvadd (_ bv63 6) ?x3757)) (bvnot (stack_t x_0 x_1 x_2 x_3 x_SLOAD_0 3 (bvadd (_ bv63 6) ?x3005))))))
 (let (($x8325 (exc_halt_t 3)))
 (let (($x10178 (= $x8325 (or (exc_halt_t 2) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_t 2))))))))
 (let (($x4551 (forall ((w (_ BitVec 256)) )(let ((?x2306 (storage_t x_0 x_1 x_2 x_3 x_SLOAD_0 2 w)))
 (let ((?x5234 (storage_t x_0 x_1 x_2 x_3 x_SLOAD_0 3 w)))
 (= ?x5234 ?x2306))))
 ))
 (let (($x5178 (forall ((n (_ BitVec 6)) )(let ((?x729 (stack_t x_0 x_1 x_2 x_3 x_SLOAD_0 2 n)))
 (let ((?x186 (stack_t x_0 x_1 x_2 x_3 x_SLOAD_0 3 n)))
 (let ((?x4056 (sc_t 2)))
 (let ((?x5157 (bvadd (_ bv62 6) ?x4056)))
 (let (($x330 (bvsle ?x5157 n)))
 (or $x330 (= ?x186 ?x729))))))))
 ))
 (let ((?x4056 (sc_t 2)))
 (let (($x5357 (= ?x3005 ?x4056)))
 (let ((?x10733 (used_gas_t x_0 x_1 x_2 x_3 x_SLOAD_0 3)))
 (let ((?x5207 (bvadd (_ bv63 6) ?x4056)))
 (let ((?x1547 (stack_t x_0 x_1 x_2 x_3 x_SLOAD_0 2 ?x5207)))
 (let ((?x5157 (bvadd (_ bv62 6) ?x4056)))
 (let ((?x7632 (stack_t x_0 x_1 x_2 x_3 x_SLOAD_0 2 ?x5157)))
 (let ((?x4707 (bvadd (_ bv63 6) ?x3005)))
 (let ((?x6094 (stack_t x_0 x_1 x_2 x_3 x_SLOAD_0 3 ?x4707)))
 (let (($x903 (exc_halt_t 2)))
 (let (($x11097 (= $x903 (or (exc_halt_t 1) (not (bvsle (_ bv0 6) (bvadd (_ bv60 6) (sc_t 1))))))))
 (let (($x2046 (forall ((w (_ BitVec 256)) )(let ((?x8347 (storage_t x_0 x_1 x_2 x_3 x_SLOAD_0 1 w)))
 (let ((?x2306 (storage_t x_0 x_1 x_2 x_3 x_SLOAD_0 2 w)))
 (= ?x2306 ?x8347))))
 ))
 (let (($x9296 (forall ((n (_ BitVec 6)) )(let ((?x4023 (sc_t 1)))
 (let ((?x5645 (bvadd (_ bv60 6) ?x4023)))
 (let (($x6004 (bvsle ?x5645 n)))
 (let ((?x9585 (stack_t x_0 x_1 x_2 x_3 x_SLOAD_0 1 n)))
 (let ((?x729 (stack_t x_0 x_1 x_2 x_3 x_SLOAD_0 2 n)))
 (or (= ?x729 ?x9585) $x6004)))))))
 ))
 (let ((?x4023 (sc_t 1)))
 (let (($x10289 (= ?x4056 ?x4023)))
 (let ((?x313 (used_gas_t x_0 x_1 x_2 x_3 x_SLOAD_0 2)))
 (let (($x7067 (= (stack_t x_0 x_1 x_2 x_3 x_SLOAD_0 2 (bvadd (_ bv61 6) ?x4056)) (stack_t x_0 x_1 x_2 x_3 x_SLOAD_0 1 (bvadd (_ bv61 6) ?x4023)))))
 (let ((?x6175 (bvadd (_ bv63 6) ?x4023)))
 (let ((?x6001 (stack_t x_0 x_1 x_2 x_3 x_SLOAD_0 1 ?x6175)))
 (let (($x1920 (exc_halt_t 1)))
 (let (($x11201 (= $x1920 (or $x56 (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) ?x63)))))))
 (let (($x3655 (forall ((w (_ BitVec 256)) )(let ((?x793 (storage_t x_0 x_1 x_2 x_3 x_SLOAD_0 0 w)))
 (let ((?x8347 (storage_t x_0 x_1 x_2 x_3 x_SLOAD_0 1 w)))
 (= ?x8347 ?x793))))
 ))
 (let (($x8911 (forall ((n (_ BitVec 6)) )(let ((?x2929 (stack_t x_0 x_1 x_2 x_3 x_SLOAD_0 0 n)))
 (let ((?x9585 (stack_t x_0 x_1 x_2 x_3 x_SLOAD_0 1 n)))
 (let ((?x63 (sc_t 0)))
 (let ((?x7582 (bvadd (_ bv63 6) ?x63)))
 (let (($x9928 (bvsle ?x7582 n)))
 (or $x9928 (= ?x9585 ?x2929))))))))
 ))
 (let (($x6681 (= ?x4023 ?x63)))
 (let ((?x1754 (storage_t x_0 x_1 x_2 x_3 x_SLOAD_0 0 (stack_t x_0 x_1 x_2 x_3 x_SLOAD_0 0 (bvadd (_ bv63 6) ?x63)))))
 (let (($x9050 (= $x772 (or (exc_halt_s 5) (not (bvsle (_ bv0 6) (bvadd (_ bv61 6) (sc_s 5))))))))
 (let (($x6763 (forall ((w (_ BitVec 256)) )(let ((?x9944 (storage_s x_0 x_1 x_2 x_3 x_SLOAD_0 5 w)))
 (let ((?x3102 (storage_s x_0 x_1 x_2 x_3 x_SLOAD_0 6 w)))
 (= ?x3102 ?x9944))))
 ))
 (let (($x7986 (forall ((n (_ BitVec 6)) )(let ((?x7806 (stack_s x_0 x_1 x_2 x_3 x_SLOAD_0 5 n)))
 (let ((?x7138 (stack_s x_0 x_1 x_2 x_3 x_SLOAD_0 6 n)))
 (or (bvsle (bvadd (_ bv61 6) (sc_s 5)) n) (= ?x7138 ?x7806)))))
 ))
 (let ((?x805 (sc_s 5)))
 (let (($x4116 (= ?x926 ?x805)))
 (let (($x2792 (= (used_gas_s x_0 x_1 x_2 x_3 x_SLOAD_0 6) (+ 3 (used_gas_s x_0 x_1 x_2 x_3 x_SLOAD_0 5)))))
 (let (($x396 (= (stack_s x_0 x_1 x_2 x_3 x_SLOAD_0 6 (bvadd (_ bv62 6) ?x926)) (stack_s x_0 x_1 x_2 x_3 x_SLOAD_0 5 (bvadd (_ bv62 6) ?x805)))))
 (let (($x3485 (= (stack_s x_0 x_1 x_2 x_3 x_SLOAD_0 6 (bvadd (_ bv61 6) ?x926)) (stack_s x_0 x_1 x_2 x_3 x_SLOAD_0 5 (bvadd (_ bv63 6) ?x805)))))
 (let (($x5066 (= (stack_s x_0 x_1 x_2 x_3 x_SLOAD_0 6 (bvadd (_ bv63 6) ?x926)) (stack_s x_0 x_1 x_2 x_3 x_SLOAD_0 5 (bvadd (_ bv61 6) ?x805)))))
 (let (($x3979 (exc_halt_s 5)))
 (let (($x6362 (= $x3979 (or (exc_halt_s 4) (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_s 4))))))))
 (let (($x9098 (forall ((w (_ BitVec 256)) )(let ((?x412 (storage_s x_0 x_1 x_2 x_3 x_SLOAD_0 4 w)))
 (let ((?x9944 (storage_s x_0 x_1 x_2 x_3 x_SLOAD_0 5 w)))
 (= ?x9944 ?x412))))
 ))
 (let (($x8234 (forall ((n (_ BitVec 6)) )(let ((?x1246 (stack_s x_0 x_1 x_2 x_3 x_SLOAD_0 4 n)))
 (let ((?x7806 (stack_s x_0 x_1 x_2 x_3 x_SLOAD_0 5 n)))
 (let ((?x4305 (sc_s 4)))
 (let ((?x11391 (bvadd (_ bv63 6) ?x4305)))
 (let (($x6521 (bvsle ?x11391 n)))
 (or $x6521 (= ?x7806 ?x1246))))))))
 ))
 (let ((?x4305 (sc_s 4)))
 (let ((?x11391 (bvadd (_ bv63 6) ?x4305)))
 (let (($x8273 (= ?x805 ?x11391)))
 (let ((?x8680 (used_gas_s x_0 x_1 x_2 x_3 x_SLOAD_0 5)))
 (let (($x64 (exc_halt_s 4)))
 (let (($x11639 (= $x64 (or (exc_halt_s 3) (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_s 3))))))))
 (let (($x8451 (forall ((w (_ BitVec 256)) )(let ((?x6664 (storage_s x_0 x_1 x_2 x_3 x_SLOAD_0 3 w)))
 (let ((?x412 (storage_s x_0 x_1 x_2 x_3 x_SLOAD_0 4 w)))
 (= ?x412 ?x6664))))
 ))
 (let (($x8556 (forall ((n (_ BitVec 6)) )(let ((?x275 (sc_s 3)))
 (let ((?x318 (bvadd (_ bv63 6) ?x275)))
 (let (($x11790 (bvsle ?x318 n)))
 (let ((?x9116 (stack_s x_0 x_1 x_2 x_3 x_SLOAD_0 3 n)))
 (let ((?x1246 (stack_s x_0 x_1 x_2 x_3 x_SLOAD_0 4 n)))
 (or (= ?x1246 ?x9116) $x11790)))))))
 ))
 (let ((?x275 (sc_s 3)))
 (let ((?x318 (bvadd (_ bv63 6) ?x275)))
 (let (($x376 (= ?x4305 ?x318)))
 (let ((?x1357 (used_gas_s x_0 x_1 x_2 x_3 x_SLOAD_0 4)))
 (let (($x292 (exc_halt_s 3)))
 (let (($x759 (= $x292 (or (exc_halt_s 2) (not (bvsle (_ bv0 6) (bvadd (_ bv61 6) (sc_s 2))))))))
 (let (($x9692 (forall ((w (_ BitVec 256)) )(let ((?x8174 (storage_s x_0 x_1 x_2 x_3 x_SLOAD_0 2 w)))
 (let ((?x6664 (storage_s x_0 x_1 x_2 x_3 x_SLOAD_0 3 w)))
 (= ?x6664 ?x8174))))
 ))
 (let (($x8638 (forall ((n (_ BitVec 6)) )(let ((?x7799 (stack_s x_0 x_1 x_2 x_3 x_SLOAD_0 2 n)))
 (let ((?x9116 (stack_s x_0 x_1 x_2 x_3 x_SLOAD_0 3 n)))
 (or (= ?x9116 ?x7799) (bvsle (bvadd (_ bv61 6) (sc_s 2)) n)))))
 ))
 (let ((?x218 (sc_s 2)))
 (let (($x10128 (= ?x275 ?x218)))
 (let ((?x5584 (used_gas_s x_0 x_1 x_2 x_3 x_SLOAD_0 3)))
 (let (($x2253 (= (stack_s x_0 x_1 x_2 x_3 x_SLOAD_0 3 (bvadd (_ bv62 6) ?x275)) (stack_s x_0 x_1 x_2 x_3 x_SLOAD_0 2 (bvadd (_ bv62 6) ?x218)))))
 (let ((?x1185 (bvadd (_ bv63 6) ?x218)))
 (let ((?x1534 (stack_s x_0 x_1 x_2 x_3 x_SLOAD_0 2 ?x1185)))
 (let (($x4123 (= (stack_s x_0 x_1 x_2 x_3 x_SLOAD_0 3 ?x318) (stack_s x_0 x_1 x_2 x_3 x_SLOAD_0 2 (bvadd (_ bv61 6) ?x218)))))
 (let (($x247 (exc_halt_s 2)))
 (let (($x9609 (= $x247 (or (exc_halt_s 1) (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_s 1))))))))
 (let (($x8385 (forall ((w (_ BitVec 256)) )(let ((?x6566 (storage_s x_0 x_1 x_2 x_3 x_SLOAD_0 1 w)))
 (let ((?x8174 (storage_s x_0 x_1 x_2 x_3 x_SLOAD_0 2 w)))
 (= ?x8174 ?x6566))))
 ))
 (let (($x3466 (forall ((n (_ BitVec 6)) )(let ((?x154 (sc_s 1)))
 (let ((?x917 (bvadd (_ bv63 6) ?x154)))
 (let (($x6822 (bvsle ?x917 n)))
 (let ((?x9099 (stack_s x_0 x_1 x_2 x_3 x_SLOAD_0 1 n)))
 (let ((?x7799 (stack_s x_0 x_1 x_2 x_3 x_SLOAD_0 2 n)))
 (or (= ?x7799 ?x9099) $x6822)))))))
 ))
 (let ((?x154 (sc_s 1)))
 (let (($x10550 (= ?x218 ?x154)))
 (let ((?x1451 (used_gas_s x_0 x_1 x_2 x_3 x_SLOAD_0 2)))
 (let ((?x917 (bvadd (_ bv63 6) ?x154)))
 (let ((?x4881 (stack_s x_0 x_1 x_2 x_3 x_SLOAD_0 1 ?x917)))
 (let (($x6924 (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) ?x72)))))
 (let (($x3651 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x72))) (_ bv0 1)))))
 (let (($x189 (exc_halt_s 1)))
 (let (($x9226 (= $x189 (or $x57 $x3651 $x6924))))
 (let (($x4925 (forall ((w (_ BitVec 256)) )(let ((?x10766 (storage_s x_0 x_1 x_2 x_3 x_SLOAD_0 0 w)))
 (let ((?x6566 (storage_s x_0 x_1 x_2 x_3 x_SLOAD_0 1 w)))
 (= ?x6566 ?x10766))))
 ))
 (let (($x9034 (forall ((n (_ BitVec 6)) )(let ((?x1054 (stack_s x_0 x_1 x_2 x_3 x_SLOAD_0 0 n)))
 (let ((?x9099 (stack_s x_0 x_1 x_2 x_3 x_SLOAD_0 1 n)))
 (let ((?x72 (sc_s 0)))
 (let ((?x11623 (bvadd (_ bv63 6) ?x72)))
 (let (($x2975 (bvsle ?x11623 n)))
 (or $x2975 (= ?x9099 ?x1054))))))))
 ))
 (let (($x5350 (= ?x154 (bvadd (_ bv1 6) ?x72))))
 (let ((?x11623 (bvadd (_ bv63 6) ?x72)))
 (let ((?x269 (stack_s x_0 x_1 x_2 x_3 x_SLOAD_0 0 ?x11623)))
 (let (($x4358 (forall ((w (_ BitVec 256)) )(let (($x2519 (= w (stack_s x_0 x_1 x_2 x_3 x_SLOAD_0 1 (bvadd (_ bv63 6) (sc_s 1))))))
 (let ((?x10766 (storage_s x_0 x_1 x_2 x_3 x_SLOAD_0 0 w)))
 (= ?x10766 (ite $x2519 x_SLOAD_0 (_ bv0 256))))))
 ))
 (let (($x750 (= ?x8289 0)))
 (let (($x1898 (not $x57)))
 (let (($x10651 (= (stack_s x_0 x_1 x_2 x_3 x_SLOAD_0 0 (_ bv3 6)) x_3)))
 (let (($x7020 (= (stack_s x_0 x_1 x_2 x_3 x_SLOAD_0 0 (_ bv2 6)) x_2)))
 (let (($x4480 (= (stack_s x_0 x_1 x_2 x_3 x_SLOAD_0 0 (_ bv1 6)) x_1)))
 (let (($x700 (= (stack_s x_0 x_1 x_2 x_3 x_SLOAD_0 0 (_ bv0 6)) x_0)))
 (let (($x5868 (= ?x72 (_ bv4 6))))
 (and $x5868 $x700 $x4480 $x7020 $x10651 $x1898 $x750 $x4358 (= ?x4881 ?x269) (= (stack_s x_0 x_1 x_2 x_3 x_SLOAD_0 1 ?x11623) ?x269) (= (used_gas_s x_0 x_1 x_2 x_3 x_SLOAD_0 1) (+ 3 ?x8289)) $x5350 $x9034 $x4925 $x9226 (= ?x1534 (storage_s x_0 x_1 x_2 x_3 x_SLOAD_0 1 ?x4881)) (= ?x1451 (+ 200 (used_gas_s x_0 x_1 x_2 x_3 x_SLOAD_0 1))) $x10550 $x3466 $x8385 $x9609 $x4123 (= (stack_s x_0 x_1 x_2 x_3 x_SLOAD_0 3 (bvadd (_ bv61 6) ?x275)) ?x1534) $x2253 (= ?x5584 (+ 3 ?x1451)) $x10128 $x8638 $x9692 $x759 (= ?x1357 (+ 2 ?x5584)) $x376 $x8556 $x8451 $x11639 (= ?x8680 (+ 2 ?x1357)) $x8273 $x8234 $x9098 $x6362 $x5066 $x3485 $x396 $x2792 $x4116 $x7986 $x6763 $x9050 (= ?x6001 ?x1754) (= (used_gas_t x_0 x_1 x_2 x_3 x_SLOAD_0 1) (+ 200 ?x1941)) $x6681 $x8911 $x3655 $x11201 (= ?x1547 (stack_t x_0 x_1 x_2 x_3 x_SLOAD_0 1 (bvadd (_ bv60 6) ?x4023))) (= (stack_t x_0 x_1 x_2 x_3 x_SLOAD_0 2 (bvadd (_ bv60 6) ?x4056)) ?x6001) $x7067 (= ?x7632 (stack_t x_0 x_1 x_2 x_3 x_SLOAD_0 1 (bvadd (_ bv62 6) ?x4023))) (= ?x313 (+ 3 (used_gas_t x_0 x_1 x_2 x_3 x_SLOAD_0 1))) $x10289 $x9296 $x2046 $x11097 (= ?x6094 ?x7632) (= (stack_t x_0 x_1 x_2 x_3 x_SLOAD_0 3 (bvadd (_ bv62 6) ?x3005)) ?x1547) (= ?x10733 (+ 3 ?x313)) $x5357 $x5178 $x4551 $x10178 $x6642 (= ?x9072 (+ 3 ?x10733)) $x134 $x3718 $x888 $x10208 $x5092 (= ?x919 (bvadd (_ bv63 6) ?x3757)) $x2218 $x8668 $x4316 $x73 $x6131 $x58 $x9773 $x9477 (not (and $x929 $x4603 $x889 $x246)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
 )
(check-sat)
(get-proof)
