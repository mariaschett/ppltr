; PUSH 0x00 PUSH cw_4 DUP2 GT ISZERO => PUSH 0x00 PUSH 0x01
(set-option :produce-proofs true)
; 
(set-info :status unknown)
(declare-fun storage_t ((_ BitVec 256) Int (_ BitVec 256)) (_ BitVec 256))
(declare-fun w_4 () (_ BitVec 256))
(declare-fun storage_s ((_ BitVec 256) Int (_ BitVec 256)) (_ BitVec 256))
(declare-fun exc_halt_t (Int) Bool)
(declare-fun exc_halt_s (Int) Bool)
(declare-fun stack_t ((_ BitVec 256) Int (_ BitVec 6)) (_ BitVec 256))
(declare-fun stack_s ((_ BitVec 256) Int (_ BitVec 6)) (_ BitVec 256))
(declare-fun sc_t (Int) (_ BitVec 6))
(declare-fun sc_s (Int) (_ BitVec 6))
(declare-fun used_gas_t ((_ BitVec 256) Int) Int)
(declare-fun used_gas_s ((_ BitVec 256) Int) Int)
(assert
 (let (($x10432 (forall ((w (_ BitVec 256)) )(let ((?x9655 (storage_t w_4 2 w)))
 (let ((?x11043 (storage_s w_4 5 w)))
 (= ?x11043 ?x9655))))
 ))
 (let (($x4057 (exc_halt_t 2)))
 (let (($x1862 (exc_halt_s 5)))
 (let (($x11658 (= $x1862 $x4057)))
 (let (($x6831 (forall ((n (_ BitVec 6)) )(let ((?x117 (stack_t w_4 2 n)))
 (let ((?x6313 (stack_s w_4 5 n)))
 (let (($x6009 (= ?x6313 ?x117)))
 (or (bvsle (sc_t 2) n) $x6009)))))
 ))
 (let ((?x11248 (sc_t 2)))
 (let ((?x4319 (sc_s 5)))
 (let (($x8485 (= ?x4319 ?x11248)))
 (let ((?x7542 (used_gas_t w_4 0)))
 (let ((?x436 (used_gas_s w_4 0)))
 (let (($x3904 (= ?x436 ?x7542)))
 (let (($x2501 (forall ((w (_ BitVec 256)) )(let ((?x5969 (storage_t w_4 0 w)))
 (let ((?x796 (storage_s w_4 0 w)))
 (= ?x796 ?x5969))))
 ))
 (let (($x56 (exc_halt_t 0)))
 (let (($x57 (exc_halt_s 0)))
 (let (($x58 (= $x57 $x56)))
 (let (($x4520 (forall ((n (_ BitVec 6)) )(let ((?x63 (sc_t 0)))
 (let (($x9079 (bvsle ?x63 n)))
 (let ((?x9543 (stack_t w_4 0 n)))
 (let ((?x1685 (stack_s w_4 0 n)))
 (let (($x4821 (= ?x1685 ?x9543)))
 (or $x4821 $x9079)))))))
 ))
 (let ((?x63 (sc_t 0)))
 (let ((?x72 (sc_s 0)))
 (let (($x73 (= ?x72 ?x63)))
 (let (($x4852 (exc_halt_t 1)))
 (let (($x8809 (or $x4852 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_t 1)))) (_ bv0 1))))))
 (let (($x4116 (= $x4057 $x8809)))
 (let (($x909 (forall ((w (_ BitVec 256)) )(let ((?x11599 (storage_t w_4 1 w)))
 (let ((?x9655 (storage_t w_4 2 w)))
 (= ?x9655 ?x11599))))
 ))
 (let (($x11506 (forall ((n (_ BitVec 6)) )(let ((?x9666 (sc_t 1)))
 (let (($x9963 (bvsle ?x9666 n)))
 (or (= (stack_t w_4 2 n) (stack_t w_4 1 n)) $x9963))))
 ))
 (let (($x11433 (= ?x11248 (bvadd (_ bv1 6) (sc_t 1)))))
 (let (($x3469 (= $x4852 (or $x56 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x63))) (_ bv0 1)))))))
 (let (($x9001 (forall ((w (_ BitVec 256)) )(let ((?x5969 (storage_t w_4 0 w)))
 (let ((?x11599 (storage_t w_4 1 w)))
 (= ?x11599 ?x5969))))
 ))
 (let (($x2698 (forall ((n (_ BitVec 6)) )(let ((?x63 (sc_t 0)))
 (let (($x9079 (bvsle ?x63 n)))
 (or (= (stack_t w_4 1 n) (stack_t w_4 0 n)) $x9079))))
 ))
 (let ((?x9666 (sc_t 1)))
 (let (($x9898 (= ?x9666 (bvadd (_ bv1 6) ?x63))))
 (let (($x6651 (= $x1862 (or (exc_halt_s 4) (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_s 4))))))))
 (let (($x11570 (forall ((w (_ BitVec 256)) )(let ((?x9949 (storage_s w_4 4 w)))
 (let ((?x11043 (storage_s w_4 5 w)))
 (= ?x11043 ?x9949))))
 ))
 (let (($x9791 (forall ((n (_ BitVec 6)) )(or (bvsle (bvadd (_ bv63 6) (sc_s 4)) n) (= (stack_s w_4 5 n) (stack_s w_4 4 n))))
 ))
 (let ((?x9433 (sc_s 4)))
 (let (($x10447 (= ?x4319 ?x9433)))
 (let (($x6001 (= (stack_s w_4 5 (bvadd (_ bv63 6) ?x4319)) (ite (= (stack_s w_4 4 (bvadd (_ bv63 6) ?x9433)) (_ bv0 256)) (_ bv1 256) (_ bv0 256)))))
 (let (($x9175 (exc_halt_s 4)))
 (let (($x1118 (= $x9175 (or (exc_halt_s 3) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 3))))))))
 (let (($x8986 (forall ((w (_ BitVec 256)) )(let ((?x10271 (storage_s w_4 3 w)))
 (let ((?x9949 (storage_s w_4 4 w)))
 (= ?x9949 ?x10271))))
 ))
 (let (($x1046 (forall ((n (_ BitVec 6)) )(let ((?x3851 (sc_s 3)))
 (let ((?x1039 (bvadd (_ bv62 6) ?x3851)))
 (let (($x11348 (bvsle ?x1039 n)))
 (or (= (stack_s w_4 4 n) (stack_s w_4 3 n)) $x11348)))))
 ))
 (let ((?x3851 (sc_s 3)))
 (let ((?x998 (bvadd (_ bv63 6) ?x3851)))
 (let ((?x2464 (stack_s w_4 3 ?x998)))
 (let ((?x6083 (bvadd (_ bv63 6) ?x9433)))
 (let ((?x9508 (stack_s w_4 4 ?x6083)))
 (let (($x10276 (= ?x9508 (ite (bvule ?x2464 (stack_s w_4 3 (bvadd (_ bv62 6) ?x3851))) (_ bv0 256) (_ bv1 256)))))
 (let (($x11311 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 2)))) (_ bv0 1)))))
 (let (($x10052 (exc_halt_s 2)))
 (let (($x7460 (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 2))))))
 (let (($x8103 (exc_halt_s 3)))
 (let (($x11287 (forall ((w (_ BitVec 256)) )(let ((?x11285 (storage_s w_4 2 w)))
 (let ((?x10271 (storage_s w_4 3 w)))
 (= ?x10271 ?x11285))))
 ))
 (let (($x5700 (forall ((n (_ BitVec 6)) )(let ((?x2272 (sc_s 2)))
 (let ((?x9266 (bvadd (_ bv62 6) ?x2272)))
 (let (($x9051 (bvsle ?x9266 n)))
 (or $x9051 (= (stack_s w_4 3 n) (stack_s w_4 2 n)))))))
 ))
 (let (($x10998 (= ?x3851 (bvadd (_ bv1 6) (sc_s 2)))))
 (let (($x1388 (= (stack_s w_4 3 (bvadd (_ bv63 6) (sc_s 2))) (stack_s w_4 2 (bvadd (_ bv63 6) (sc_s 2))))))
 (let ((?x2272 (sc_s 2)))
 (let ((?x9266 (bvadd (_ bv62 6) ?x2272)))
 (let ((?x10694 (stack_s w_4 2 ?x9266)))
 (let (($x186 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 1)))) (_ bv0 1)))))
 (let (($x8780 (exc_halt_s 1)))
 (let (($x6565 (= $x10052 (or $x8780 $x186))))
 (let (($x6015 (forall ((w (_ BitVec 256)) )(let ((?x10875 (storage_s w_4 1 w)))
 (let ((?x11285 (storage_s w_4 2 w)))
 (= ?x11285 ?x10875))))
 ))
 (let (($x7196 (forall ((n (_ BitVec 6)) )(let ((?x154 (sc_s 1)))
 (let (($x4976 (bvsle ?x154 n)))
 (or $x4976 (= (stack_s w_4 2 n) (stack_s w_4 1 n))))))
 ))
 (let (($x2595 (= ?x2272 (bvadd (_ bv1 6) (sc_s 1)))))
 (let (($x5465 (= $x8780 (or $x57 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x72))) (_ bv0 1)))))))
 (let (($x1262 (forall ((w (_ BitVec 256)) )(let ((?x796 (storage_s w_4 0 w)))
 (let ((?x10875 (storage_s w_4 1 w)))
 (= ?x10875 ?x796))))
 ))
 (let (($x7235 (forall ((n (_ BitVec 6)) )(let ((?x72 (sc_s 0)))
 (let (($x4347 (bvsle ?x72 n)))
 (or $x4347 (= (stack_s w_4 1 n) (stack_s w_4 0 n))))))
 ))
 (let ((?x154 (sc_s 1)))
 (let (($x10888 (= ?x154 (bvadd (_ bv1 6) ?x72))))
 (let (($x699 (forall ((w (_ BitVec 256)) )(let ((?x796 (storage_s w_4 0 w)))
 (= ?x796 (_ bv0 256))))
 ))
 (let (($x2109 (= ?x436 0)))
 (let (($x10995 (not $x57)))
 (let (($x4825 (= ?x72 (_ bv0 6))))
 (and $x4825 $x10995 $x2109 $x699 (= (stack_s w_4 1 ?x72) (_ bv0 256)) (= (used_gas_s w_4 1) (+ 3 ?x436)) $x10888 $x7235 $x1262 $x5465 (= (stack_s w_4 2 ?x154) w_4) (= (used_gas_s w_4 2) (+ 3 (used_gas_s w_4 1))) $x2595 $x7196 $x6015 $x6565 (= ?x2464 ?x10694) (= (stack_s w_4 3 ?x9266) ?x10694) $x1388 (= (used_gas_s w_4 3) (+ 3 (used_gas_s w_4 2))) $x10998 $x5700 $x11287 (= $x8103 (or $x7460 $x10052 $x11311)) $x10276 (= (used_gas_s w_4 4) (+ 3 (used_gas_s w_4 3))) (= ?x9433 ?x998) $x1046 $x8986 $x1118 $x6001 (= (used_gas_s w_4 5) (+ 3 (used_gas_s w_4 4))) $x10447 $x9791 $x11570 $x6651 (= (stack_t w_4 1 ?x63) (_ bv0 256)) (= (used_gas_t w_4 1) (+ 3 ?x7542)) $x9898 $x2698 $x9001 $x3469 (= (stack_t w_4 2 ?x9666) (_ bv1 256)) (= (used_gas_t w_4 2) (+ 3 (used_gas_t w_4 1))) $x11433 $x11506 $x909 $x4116 $x73 $x4520 $x58 $x2501 $x3904 (not (and $x8485 $x6831 $x11658 $x10432))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
(check-sat)
(get-proof)