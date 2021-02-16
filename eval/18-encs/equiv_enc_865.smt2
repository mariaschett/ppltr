; CALLVALUE PUSH cw_1 MLOAD DUP1 DUP3 => CALLVALUE PUSH cw_1 MLOAD DUP1 CALLVALUE
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
(declare-fun f_MLOAD ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256)) (_ BitVec 256))
(assert
 (exists ((x_CALLVALUE (_ BitVec 256)) (x_MLOAD_0 (_ BitVec 256)) )(let (($x414 (forall ((w (_ BitVec 256)) )(let ((?x11228 (storage_t w_1 x_CALLVALUE x_MLOAD_0 5 w)))
 (let ((?x278 (storage_s w_1 x_CALLVALUE x_MLOAD_0 5 w)))
 (= ?x278 ?x11228))))
 ))
 (let (($x10311 (exc_halt_t 5)))
 (let (($x1862 (exc_halt_s 5)))
 (let (($x981 (= $x1862 $x10311)))
 (let (($x7632 (forall ((n (_ BitVec 6)) )(let ((?x3714 (stack_t w_1 x_CALLVALUE x_MLOAD_0 5 n)))
 (let ((?x259 (stack_s w_1 x_CALLVALUE x_MLOAD_0 5 n)))
 (let (($x3418 (= ?x259 ?x3714)))
 (or $x3418 (bvsle (sc_t 5) n))))))
 ))
 (let ((?x10036 (sc_t 5)))
 (let ((?x4319 (sc_s 5)))
 (let (($x11769 (= ?x4319 ?x10036)))
 (let ((?x175 (used_gas_t w_1 x_CALLVALUE x_MLOAD_0 0)))
 (let ((?x8825 (used_gas_s w_1 x_CALLVALUE x_MLOAD_0 0)))
 (let (($x7468 (= ?x8825 ?x175)))
 (let (($x1107 (forall ((w (_ BitVec 256)) )(let ((?x4073 (storage_t w_1 x_CALLVALUE x_MLOAD_0 0 w)))
 (let ((?x1427 (storage_s w_1 x_CALLVALUE x_MLOAD_0 0 w)))
 (= ?x1427 ?x4073))))
 ))
 (let (($x56 (exc_halt_t 0)))
 (let (($x57 (exc_halt_s 0)))
 (let (($x58 (= $x57 $x56)))
 (let (($x9069 (forall ((n (_ BitVec 6)) )(let ((?x484 (stack_t w_1 x_CALLVALUE x_MLOAD_0 0 n)))
 (let ((?x5083 (stack_s w_1 x_CALLVALUE x_MLOAD_0 0 n)))
 (let (($x6623 (= ?x5083 ?x484)))
 (let ((?x63 (sc_t 0)))
 (let (($x4370 (bvsle ?x63 n)))
 (or $x4370 $x6623)))))))
 ))
 (let ((?x63 (sc_t 0)))
 (let ((?x72 (sc_s 0)))
 (let (($x73 (= ?x72 ?x63)))
 (let (($x7854 (exc_halt_t 4)))
 (let (($x411 (or $x7854 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_t 4)))) (_ bv0 1))))))
 (let (($x1038 (forall ((w (_ BitVec 256)) )(let ((?x1404 (storage_t w_1 x_CALLVALUE x_MLOAD_0 4 w)))
 (let ((?x11228 (storage_t w_1 x_CALLVALUE x_MLOAD_0 5 w)))
 (= ?x11228 ?x1404))))
 ))
 (let (($x7852 (forall ((n (_ BitVec 6)) )(let ((?x7636 (stack_t w_1 x_CALLVALUE x_MLOAD_0 4 n)))
 (let ((?x3714 (stack_t w_1 x_CALLVALUE x_MLOAD_0 5 n)))
 (or (bvsle (sc_t 4) n) (= ?x3714 ?x7636)))))
 ))
 (let (($x6443 (= (used_gas_t w_1 x_CALLVALUE x_MLOAD_0 5) (+ 2 (used_gas_t w_1 x_CALLVALUE x_MLOAD_0 4)))))
 (let (($x9131 (exc_halt_t 3)))
 (let (($x1707 (or (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_t 3)))) $x9131 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_t 3)))) (_ bv0 1))))))
 (let (($x10592 (forall ((w (_ BitVec 256)) )(let ((?x2093 (storage_t w_1 x_CALLVALUE x_MLOAD_0 3 w)))
 (let ((?x1404 (storage_t w_1 x_CALLVALUE x_MLOAD_0 4 w)))
 (= ?x1404 ?x2093))))
 ))
 (let (($x1881 (forall ((n (_ BitVec 6)) )(let ((?x2205 (stack_t w_1 x_CALLVALUE x_MLOAD_0 3 n)))
 (let ((?x7636 (stack_t w_1 x_CALLVALUE x_MLOAD_0 4 n)))
 (or (bvsle (bvadd (_ bv63 6) (sc_t 3)) n) (= ?x7636 ?x2205)))))
 ))
 (let ((?x9673 (used_gas_t w_1 x_CALLVALUE x_MLOAD_0 4)))
 (let ((?x10013 (sc_t 3)))
 (let ((?x3835 (bvadd (_ bv63 6) ?x10013)))
 (let ((?x272 (stack_t w_1 x_CALLVALUE x_MLOAD_0 3 ?x3835)))
 (let (($x2061 (= $x9131 (or (exc_halt_t 2) (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_t 2))))))))
 (let (($x7890 (forall ((w (_ BitVec 256)) )(let ((?x2537 (storage_t w_1 x_CALLVALUE x_MLOAD_0 2 w)))
 (let ((?x2093 (storage_t w_1 x_CALLVALUE x_MLOAD_0 3 w)))
 (= ?x2093 ?x2537))))
 ))
 (let (($x8118 (forall ((n (_ BitVec 6)) )(let ((?x139 (stack_t w_1 x_CALLVALUE x_MLOAD_0 2 n)))
 (let ((?x2205 (stack_t w_1 x_CALLVALUE x_MLOAD_0 3 n)))
 (or (bvsle (bvadd (_ bv63 6) (sc_t 2)) n) (= ?x2205 ?x139)))))
 ))
 (let ((?x6978 (used_gas_t w_1 x_CALLVALUE x_MLOAD_0 3)))
 (let ((?x3516 (f_MLOAD w_1 x_CALLVALUE x_MLOAD_0 (stack_t w_1 x_CALLVALUE x_MLOAD_0 2 (bvadd (_ bv63 6) (sc_t 2))))))
 (let (($x4852 (exc_halt_t 1)))
 (let (($x9213 (or $x4852 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_t 1)))) (_ bv0 1))))))
 (let (($x6733 (exc_halt_t 2)))
 (let (($x4685 (forall ((w (_ BitVec 256)) )(let ((?x7668 (storage_t w_1 x_CALLVALUE x_MLOAD_0 1 w)))
 (let ((?x2537 (storage_t w_1 x_CALLVALUE x_MLOAD_0 2 w)))
 (= ?x2537 ?x7668))))
 ))
 (let (($x7082 (forall ((n (_ BitVec 6)) )(let ((?x2520 (stack_t w_1 x_CALLVALUE x_MLOAD_0 1 n)))
 (let ((?x139 (stack_t w_1 x_CALLVALUE x_MLOAD_0 2 n)))
 (or (= ?x139 ?x2520) (bvsle (sc_t 1) n)))))
 ))
 (let ((?x10059 (used_gas_t w_1 x_CALLVALUE x_MLOAD_0 2)))
 (let (($x71 (= $x4852 (or $x56 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x63))) (_ bv0 1)))))))
 (let (($x6406 (forall ((w (_ BitVec 256)) )(let ((?x4073 (storage_t w_1 x_CALLVALUE x_MLOAD_0 0 w)))
 (let ((?x7668 (storage_t w_1 x_CALLVALUE x_MLOAD_0 1 w)))
 (= ?x7668 ?x4073))))
 ))
 (let (($x11759 (forall ((n (_ BitVec 6)) )(let ((?x484 (stack_t w_1 x_CALLVALUE x_MLOAD_0 0 n)))
 (let ((?x2520 (stack_t w_1 x_CALLVALUE x_MLOAD_0 1 n)))
 (let ((?x63 (sc_t 0)))
 (let (($x4370 (bvsle ?x63 n)))
 (or $x4370 (= ?x2520 ?x484)))))))
 ))
 (let (($x9175 (exc_halt_s 4)))
 (let (($x4488 (or (not (bvsle (_ bv0 6) (bvadd (_ bv61 6) (sc_s 4)))) (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 4)))) (_ bv0 1))) $x9175)))
 (let (($x7994 (forall ((w (_ BitVec 256)) )(let ((?x8929 (storage_s w_1 x_CALLVALUE x_MLOAD_0 4 w)))
 (let ((?x278 (storage_s w_1 x_CALLVALUE x_MLOAD_0 5 w)))
 (= ?x278 ?x8929))))
 ))
 (let (($x9645 (forall ((n (_ BitVec 6)) )(let ((?x1792 (stack_s w_1 x_CALLVALUE x_MLOAD_0 4 n)))
 (let ((?x259 (stack_s w_1 x_CALLVALUE x_MLOAD_0 5 n)))
 (or (= ?x259 ?x1792) (bvsle (bvadd (_ bv61 6) (sc_s 4)) n)))))
 ))
 (let (($x3140 (= (used_gas_s w_1 x_CALLVALUE x_MLOAD_0 5) (+ 3 (used_gas_s w_1 x_CALLVALUE x_MLOAD_0 4)))))
 (let ((?x9433 (sc_s 4)))
 (let ((?x9405 (bvadd (_ bv63 6) ?x9433)))
 (let ((?x1659 (stack_s w_1 x_CALLVALUE x_MLOAD_0 4 ?x9405)))
 (let (($x3799 (= (stack_s w_1 x_CALLVALUE x_MLOAD_0 5 (bvadd (_ bv62 6) ?x9433)) (stack_s w_1 x_CALLVALUE x_MLOAD_0 4 (bvadd (_ bv62 6) ?x9433)))))
 (let ((?x1473 (bvadd (_ bv61 6) ?x9433)))
 (let ((?x1713 (stack_s w_1 x_CALLVALUE x_MLOAD_0 4 ?x1473)))
 (let (($x8103 (exc_halt_s 3)))
 (let (($x11970 (or $x8103 (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_s 3)))) (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 3)))) (_ bv0 1))))))
 (let (($x497 (forall ((w (_ BitVec 256)) )(let ((?x1257 (storage_s w_1 x_CALLVALUE x_MLOAD_0 3 w)))
 (let ((?x8929 (storage_s w_1 x_CALLVALUE x_MLOAD_0 4 w)))
 (= ?x8929 ?x1257))))
 ))
 (let (($x10858 (forall ((n (_ BitVec 6)) )(let ((?x8285 (stack_s w_1 x_CALLVALUE x_MLOAD_0 3 n)))
 (let ((?x1792 (stack_s w_1 x_CALLVALUE x_MLOAD_0 4 n)))
 (or (bvsle (bvadd (_ bv63 6) (sc_s 3)) n) (= ?x1792 ?x8285)))))
 ))
 (let ((?x6400 (used_gas_s w_1 x_CALLVALUE x_MLOAD_0 4)))
 (let ((?x3851 (sc_s 3)))
 (let ((?x9751 (bvadd (_ bv63 6) ?x3851)))
 (let ((?x6146 (stack_s w_1 x_CALLVALUE x_MLOAD_0 3 ?x9751)))
 (let (($x1400 (= $x8103 (or (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_s 2)))) (exc_halt_s 2)))))
 (let (($x812 (forall ((w (_ BitVec 256)) )(let ((?x1023 (storage_s w_1 x_CALLVALUE x_MLOAD_0 2 w)))
 (let ((?x1257 (storage_s w_1 x_CALLVALUE x_MLOAD_0 3 w)))
 (= ?x1257 ?x1023))))
 ))
 (let (($x9855 (forall ((n (_ BitVec 6)) )(let ((?x4333 (stack_s w_1 x_CALLVALUE x_MLOAD_0 2 n)))
 (let ((?x8285 (stack_s w_1 x_CALLVALUE x_MLOAD_0 3 n)))
 (or (bvsle (bvadd (_ bv63 6) (sc_s 2)) n) (= ?x8285 ?x4333)))))
 ))
 (let ((?x7527 (used_gas_s w_1 x_CALLVALUE x_MLOAD_0 3)))
 (let ((?x5186 (f_MLOAD w_1 x_CALLVALUE x_MLOAD_0 (stack_s w_1 x_CALLVALUE x_MLOAD_0 2 (bvadd (_ bv63 6) (sc_s 2))))))
 (let (($x8780 (exc_halt_s 1)))
 (let (($x4740 (or (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 1)))) (_ bv0 1))) $x8780)))
 (let (($x10052 (exc_halt_s 2)))
 (let (($x8446 (forall ((w (_ BitVec 256)) )(let ((?x9663 (storage_s w_1 x_CALLVALUE x_MLOAD_0 1 w)))
 (let ((?x1023 (storage_s w_1 x_CALLVALUE x_MLOAD_0 2 w)))
 (= ?x1023 ?x9663))))
 ))
 (let (($x938 (forall ((n (_ BitVec 6)) )(let ((?x9382 (stack_s w_1 x_CALLVALUE x_MLOAD_0 1 n)))
 (let ((?x4333 (stack_s w_1 x_CALLVALUE x_MLOAD_0 2 n)))
 (or (= ?x4333 ?x9382) (bvsle (sc_s 1) n)))))
 ))
 (let ((?x7151 (used_gas_s w_1 x_CALLVALUE x_MLOAD_0 2)))
 (let (($x1308 (= $x8780 (or $x57 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x72))) (_ bv0 1)))))))
 (let (($x3069 (forall ((w (_ BitVec 256)) )(let ((?x1427 (storage_s w_1 x_CALLVALUE x_MLOAD_0 0 w)))
 (let ((?x9663 (storage_s w_1 x_CALLVALUE x_MLOAD_0 1 w)))
 (= ?x9663 ?x1427))))
 ))
 (let (($x9742 (forall ((n (_ BitVec 6)) )(let ((?x5083 (stack_s w_1 x_CALLVALUE x_MLOAD_0 0 n)))
 (let ((?x9382 (stack_s w_1 x_CALLVALUE x_MLOAD_0 1 n)))
 (or (= ?x9382 ?x5083) (bvsle (sc_s 0) n)))))
 ))
 (let (($x5177 (forall ((w0 (_ BitVec 256)) )(let ((?x10064 (ite (= (stack_s w_1 x_CALLVALUE x_MLOAD_0 2 (bvadd (_ bv63 6) (sc_s 2))) w0) x_MLOAD_0 (_ bv0 256))))
 (let ((?x1176 (f_MLOAD w_1 x_CALLVALUE x_MLOAD_0 w0)))
 (= ?x1176 ?x10064))))
 ))
 (let (($x10595 (forall ((w (_ BitVec 256)) )(let ((?x1427 (storage_s w_1 x_CALLVALUE x_MLOAD_0 0 w)))
 (= ?x1427 (_ bv0 256))))
 ))
 (let (($x5619 (= ?x8825 0)))
 (let (($x4825 (= ?x72 (_ bv0 6))))
 (and $x4825 (not $x57) $x5619 $x10595 $x5177 (= (stack_s w_1 x_CALLVALUE x_MLOAD_0 1 ?x72) x_CALLVALUE) (= (used_gas_s w_1 x_CALLVALUE x_MLOAD_0 1) (+ 2 ?x8825)) (= (sc_s 1) (bvadd (_ bv1 6) ?x72)) $x9742 $x3069 $x1308 (= (stack_s w_1 x_CALLVALUE x_MLOAD_0 2 (sc_s 1)) w_1) (= ?x7151 (+ 3 (used_gas_s w_1 x_CALLVALUE x_MLOAD_0 1))) (= (sc_s 2) (bvadd (_ bv1 6) (sc_s 1))) $x938 $x8446 (= $x10052 $x4740) (= ?x6146 ?x5186) (= ?x7527 (+ 3 ?x7151)) (= ?x3851 (sc_s 2)) $x9855 $x812 $x1400 (= ?x1659 ?x6146) (= (stack_s w_1 x_CALLVALUE x_MLOAD_0 4 ?x9751) ?x6146) (= ?x6400 (+ 3 ?x7527)) (= ?x9433 (bvadd (_ bv1 6) ?x3851)) $x10858 $x497 (= $x9175 $x11970) (= (stack_s w_1 x_CALLVALUE x_MLOAD_0 5 (bvadd (_ bv63 6) ?x4319)) ?x1713) (= (stack_s w_1 x_CALLVALUE x_MLOAD_0 5 ?x1473) ?x1713) $x3799 (= (stack_s w_1 x_CALLVALUE x_MLOAD_0 5 ?x9405) ?x1659) $x3140 (= ?x4319 (bvadd (_ bv1 6) ?x9433)) $x9645 $x7994 (= $x1862 $x4488) (= (stack_t w_1 x_CALLVALUE x_MLOAD_0 1 ?x63) x_CALLVALUE) (= (used_gas_t w_1 x_CALLVALUE x_MLOAD_0 1) (+ 2 ?x175)) (= (sc_t 1) (bvadd (_ bv1 6) ?x63)) $x11759 $x6406 $x71 (= (stack_t w_1 x_CALLVALUE x_MLOAD_0 2 (sc_t 1)) w_1) (= ?x10059 (+ 3 (used_gas_t w_1 x_CALLVALUE x_MLOAD_0 1))) (= (sc_t 2) (bvadd (_ bv1 6) (sc_t 1))) $x7082 $x4685 (= $x6733 $x9213) (= ?x272 ?x3516) (= ?x6978 (+ 3 ?x10059)) (= ?x10013 (sc_t 2)) $x8118 $x7890 $x2061 (= (stack_t w_1 x_CALLVALUE x_MLOAD_0 4 (bvadd (_ bv63 6) (sc_t 4))) ?x272) (= (stack_t w_1 x_CALLVALUE x_MLOAD_0 4 ?x3835) ?x272) (= ?x9673 (+ 3 ?x6978)) (= (sc_t 4) (bvadd (_ bv1 6) ?x10013)) $x1881 $x10592 (= $x7854 $x1707) (= (stack_t w_1 x_CALLVALUE x_MLOAD_0 5 (sc_t 4)) x_CALLVALUE) $x6443 (= ?x10036 (bvadd (_ bv1 6) (sc_t 4))) $x7852 $x1038 (= $x10311 $x411) $x73 $x9069 $x58 $x1107 $x7468 (not (and $x11769 $x7632 $x981 $x414)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
 )
(check-sat)
(get-proof)