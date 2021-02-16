; PUSH 0x00 DUP3 MLOAD GT ISZERO => DUP2 MLOAD ISZERO
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
(declare-fun f_MLOAD ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256)) (_ BitVec 256))
(assert
 (exists ((x_0 (_ BitVec 256)) (x_1 (_ BitVec 256)) (x_MLOAD_0 (_ BitVec 256)) )(let (($x5386 (forall ((w (_ BitVec 256)) )(let ((?x283 (storage_t x_0 x_1 x_MLOAD_0 3 w)))
 (let ((?x6173 (storage_s x_0 x_1 x_MLOAD_0 5 w)))
 (= ?x6173 ?x283))))
 ))
 (let (($x9131 (exc_halt_t 3)))
 (let (($x1862 (exc_halt_s 5)))
 (let (($x11447 (= $x1862 $x9131)))
 (let (($x3285 (forall ((n (_ BitVec 6)) )(let ((?x10013 (sc_t 3)))
 (let (($x11349 (bvsle ?x10013 n)))
 (let ((?x2984 (stack_t x_0 x_1 x_MLOAD_0 3 n)))
 (let ((?x10484 (stack_s x_0 x_1 x_MLOAD_0 5 n)))
 (let (($x10412 (= ?x10484 ?x2984)))
 (or $x10412 $x11349)))))))
 ))
 (let ((?x10013 (sc_t 3)))
 (let ((?x4319 (sc_s 5)))
 (let (($x6289 (= ?x4319 ?x10013)))
 (let ((?x4502 (used_gas_t x_0 x_1 x_MLOAD_0 0)))
 (let ((?x4475 (used_gas_s x_0 x_1 x_MLOAD_0 0)))
 (let (($x11994 (= ?x4475 ?x4502)))
 (let (($x6398 (forall ((w (_ BitVec 256)) )(let ((?x6921 (storage_t x_0 x_1 x_MLOAD_0 0 w)))
 (let ((?x3997 (storage_s x_0 x_1 x_MLOAD_0 0 w)))
 (= ?x3997 ?x6921))))
 ))
 (let (($x56 (exc_halt_t 0)))
 (let (($x57 (exc_halt_s 0)))
 (let (($x58 (= $x57 $x56)))
 (let (($x2100 (forall ((n (_ BitVec 6)) )(let ((?x7624 (stack_t x_0 x_1 x_MLOAD_0 0 n)))
 (let ((?x8213 (stack_s x_0 x_1 x_MLOAD_0 0 n)))
 (let (($x6137 (= ?x8213 ?x7624)))
 (let ((?x63 (sc_t 0)))
 (let (($x3394 (bvsle ?x63 n)))
 (or $x3394 $x6137)))))))
 ))
 (let ((?x63 (sc_t 0)))
 (let ((?x72 (sc_s 0)))
 (let (($x73 (= ?x72 ?x63)))
 (let (($x10876 (= $x9131 (or (exc_halt_t 2) (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_t 2))))))))
 (let (($x3628 (forall ((w (_ BitVec 256)) )(let ((?x4483 (storage_t x_0 x_1 x_MLOAD_0 2 w)))
 (let ((?x283 (storage_t x_0 x_1 x_MLOAD_0 3 w)))
 (= ?x283 ?x4483))))
 ))
 (let (($x3801 (forall ((n (_ BitVec 6)) )(let ((?x6718 (sc_t 2)))
 (let ((?x10124 (bvadd (_ bv63 6) ?x6718)))
 (let (($x4759 (bvsle ?x10124 n)))
 (let ((?x8401 (stack_t x_0 x_1 x_MLOAD_0 2 n)))
 (let ((?x2984 (stack_t x_0 x_1 x_MLOAD_0 3 n)))
 (or (= ?x2984 ?x8401) $x4759)))))))
 ))
 (let ((?x6718 (sc_t 2)))
 (let (($x2518 (= ?x10013 ?x6718)))
 (let (($x5712 (= (used_gas_t x_0 x_1 x_MLOAD_0 3) (+ 3 (used_gas_t x_0 x_1 x_MLOAD_0 2)))))
 (let ((?x11897 (ite (= (stack_t x_0 x_1 x_MLOAD_0 2 (bvadd (_ bv63 6) ?x6718)) (_ bv0 256)) (_ bv1 256) (_ bv0 256))))
 (let (($x2189 (exc_halt_t 2)))
 (let (($x2205 (= $x2189 (or (exc_halt_t 1) (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_t 1))))))))
 (let (($x4689 (forall ((w (_ BitVec 256)) )(let ((?x2584 (storage_t x_0 x_1 x_MLOAD_0 1 w)))
 (let ((?x4483 (storage_t x_0 x_1 x_MLOAD_0 2 w)))
 (= ?x4483 ?x2584))))
 ))
 (let (($x2934 (forall ((n (_ BitVec 6)) )(let ((?x2707 (sc_t 1)))
 (let ((?x542 (bvadd (_ bv63 6) ?x2707)))
 (let (($x6332 (bvsle ?x542 n)))
 (let ((?x1786 (stack_t x_0 x_1 x_MLOAD_0 1 n)))
 (let ((?x8401 (stack_t x_0 x_1 x_MLOAD_0 2 n)))
 (or (= ?x8401 ?x1786) $x6332)))))))
 ))
 (let ((?x2707 (sc_t 1)))
 (let (($x7636 (= ?x6718 ?x2707)))
 (let ((?x11950 (used_gas_t x_0 x_1 x_MLOAD_0 2)))
 (let ((?x542 (bvadd (_ bv63 6) ?x2707)))
 (let ((?x8208 (stack_t x_0 x_1 x_MLOAD_0 1 ?x542)))
 (let ((?x10124 (bvadd (_ bv63 6) ?x6718)))
 (let ((?x7331 (stack_t x_0 x_1 x_MLOAD_0 2 ?x10124)))
 (let (($x8149 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x63))) (_ bv0 1)))))
 (let (($x11058 (exc_halt_t 1)))
 (let (($x8478 (= $x11058 (or $x56 (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) ?x63))) $x8149))))
 (let (($x6791 (forall ((w (_ BitVec 256)) )(let ((?x6921 (storage_t x_0 x_1 x_MLOAD_0 0 w)))
 (let ((?x2584 (storage_t x_0 x_1 x_MLOAD_0 1 w)))
 (= ?x2584 ?x6921))))
 ))
 (let (($x890 (forall ((n (_ BitVec 6)) )(let ((?x63 (sc_t 0)))
 (let ((?x6311 (bvadd (_ bv62 6) ?x63)))
 (let (($x11019 (bvsle ?x6311 n)))
 (let ((?x7624 (stack_t x_0 x_1 x_MLOAD_0 0 n)))
 (let ((?x1786 (stack_t x_0 x_1 x_MLOAD_0 1 n)))
 (or (= ?x1786 ?x7624) $x11019)))))))
 ))
 (let (($x5757 (= ?x2707 (bvadd (_ bv1 6) ?x63))))
 (let (($x11840 (= (stack_t x_0 x_1 x_MLOAD_0 1 (bvadd (_ bv63 6) ?x63)) (stack_t x_0 x_1 x_MLOAD_0 0 (bvadd (_ bv63 6) ?x63)))))
 (let ((?x6311 (bvadd (_ bv62 6) ?x63)))
 (let ((?x11282 (stack_t x_0 x_1 x_MLOAD_0 0 ?x6311)))
 (let (($x4554 (= $x1862 (or (exc_halt_s 4) (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_s 4))))))))
 (let (($x9167 (forall ((w (_ BitVec 256)) )(let ((?x10218 (storage_s x_0 x_1 x_MLOAD_0 4 w)))
 (let ((?x6173 (storage_s x_0 x_1 x_MLOAD_0 5 w)))
 (= ?x6173 ?x10218))))
 ))
 (let (($x1895 (forall ((n (_ BitVec 6)) )(let ((?x9433 (sc_s 4)))
 (let ((?x1620 (bvadd (_ bv63 6) ?x9433)))
 (let (($x7683 (bvsle ?x1620 n)))
 (let ((?x6023 (stack_s x_0 x_1 x_MLOAD_0 4 n)))
 (let ((?x10484 (stack_s x_0 x_1 x_MLOAD_0 5 n)))
 (or (= ?x10484 ?x6023) $x7683)))))))
 ))
 (let ((?x9433 (sc_s 4)))
 (let (($x6285 (= ?x4319 ?x9433)))
 (let (($x8536 (= (used_gas_s x_0 x_1 x_MLOAD_0 5) (+ 3 (used_gas_s x_0 x_1 x_MLOAD_0 4)))))
 (let ((?x89 (ite (= (stack_s x_0 x_1 x_MLOAD_0 4 (bvadd (_ bv63 6) ?x9433)) (_ bv0 256)) (_ bv1 256) (_ bv0 256))))
 (let (($x9175 (exc_halt_s 4)))
 (let (($x8838 (= $x9175 (or (exc_halt_s 3) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 3))))))))
 (let (($x11334 (forall ((w (_ BitVec 256)) )(let ((?x6314 (storage_s x_0 x_1 x_MLOAD_0 3 w)))
 (let ((?x10218 (storage_s x_0 x_1 x_MLOAD_0 4 w)))
 (= ?x10218 ?x6314))))
 ))
 (let (($x10418 (forall ((n (_ BitVec 6)) )(let ((?x3851 (sc_s 3)))
 (let ((?x4510 (bvadd (_ bv62 6) ?x3851)))
 (let (($x3583 (bvsle ?x4510 n)))
 (let ((?x11401 (stack_s x_0 x_1 x_MLOAD_0 3 n)))
 (let ((?x6023 (stack_s x_0 x_1 x_MLOAD_0 4 n)))
 (or (= ?x6023 ?x11401) $x3583)))))))
 ))
 (let ((?x3851 (sc_s 3)))
 (let ((?x9316 (bvadd (_ bv63 6) ?x3851)))
 (let (($x6297 (= ?x9433 ?x9316)))
 (let ((?x8679 (used_gas_s x_0 x_1 x_MLOAD_0 4)))
 (let ((?x8703 (stack_s x_0 x_1 x_MLOAD_0 3 ?x9316)))
 (let ((?x2134 (ite (bvule ?x8703 (stack_s x_0 x_1 x_MLOAD_0 3 (bvadd (_ bv62 6) ?x3851))) (_ bv0 256) (_ bv1 256))))
 (let ((?x1620 (bvadd (_ bv63 6) ?x9433)))
 (let ((?x6717 (stack_s x_0 x_1 x_MLOAD_0 4 ?x1620)))
 (let (($x8103 (exc_halt_s 3)))
 (let (($x4084 (= $x8103 (or (exc_halt_s 2) (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_s 2))))))))
 (let (($x9697 (forall ((w (_ BitVec 256)) )(let ((?x5710 (storage_s x_0 x_1 x_MLOAD_0 2 w)))
 (let ((?x6314 (storage_s x_0 x_1 x_MLOAD_0 3 w)))
 (= ?x6314 ?x5710))))
 ))
 (let (($x1804 (forall ((n (_ BitVec 6)) )(let ((?x3141 (stack_s x_0 x_1 x_MLOAD_0 2 n)))
 (let ((?x11401 (stack_s x_0 x_1 x_MLOAD_0 3 n)))
 (let ((?x2272 (sc_s 2)))
 (let ((?x7154 (bvadd (_ bv63 6) ?x2272)))
 (let (($x2805 (bvsle ?x7154 n)))
 (or $x2805 (= ?x11401 ?x3141))))))))
 ))
 (let ((?x2272 (sc_s 2)))
 (let (($x587 (= ?x3851 ?x2272)))
 (let ((?x1365 (used_gas_s x_0 x_1 x_MLOAD_0 3)))
 (let ((?x7154 (bvadd (_ bv63 6) ?x2272)))
 (let ((?x8535 (stack_s x_0 x_1 x_MLOAD_0 2 ?x7154)))
 (let (($x8780 (exc_halt_s 1)))
 (let (($x613 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 1)))) (_ bv0 1)))))
 (let (($x10052 (exc_halt_s 2)))
 (let (($x3935 (forall ((w (_ BitVec 256)) )(let ((?x8340 (storage_s x_0 x_1 x_MLOAD_0 1 w)))
 (let ((?x5710 (storage_s x_0 x_1 x_MLOAD_0 2 w)))
 (= ?x5710 ?x8340))))
 ))
 (let (($x10452 (forall ((n (_ BitVec 6)) )(let ((?x11647 (stack_s x_0 x_1 x_MLOAD_0 1 n)))
 (let ((?x3141 (stack_s x_0 x_1 x_MLOAD_0 2 n)))
 (or (= ?x3141 ?x11647) (bvsle (bvadd (_ bv61 6) (sc_s 1)) n)))))
 ))
 (let (($x7159 (= ?x2272 (bvadd (_ bv1 6) (sc_s 1)))))
 (let ((?x6102 (used_gas_s x_0 x_1 x_MLOAD_0 2)))
 (let (($x4341 (= (stack_s x_0 x_1 x_MLOAD_0 2 (bvadd (_ bv63 6) (sc_s 1))) (stack_s x_0 x_1 x_MLOAD_0 1 (bvadd (_ bv63 6) (sc_s 1))))))
 (let (($x3282 (= (stack_s x_0 x_1 x_MLOAD_0 2 (bvadd (_ bv62 6) (sc_s 1))) (stack_s x_0 x_1 x_MLOAD_0 1 (bvadd (_ bv62 6) (sc_s 1))))))
 (let ((?x154 (sc_s 1)))
 (let ((?x8293 (bvadd (_ bv61 6) ?x154)))
 (let ((?x4984 (stack_s x_0 x_1 x_MLOAD_0 1 ?x8293)))
 (let (($x426 (= $x8780 (or $x57 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x72))) (_ bv0 1)))))))
 (let (($x4816 (forall ((w (_ BitVec 256)) )(let ((?x3997 (storage_s x_0 x_1 x_MLOAD_0 0 w)))
 (let ((?x8340 (storage_s x_0 x_1 x_MLOAD_0 1 w)))
 (= ?x8340 ?x3997))))
 ))
 (let (($x5315 (forall ((n (_ BitVec 6)) )(let ((?x8213 (stack_s x_0 x_1 x_MLOAD_0 0 n)))
 (let ((?x11647 (stack_s x_0 x_1 x_MLOAD_0 1 n)))
 (let ((?x72 (sc_s 0)))
 (let (($x1781 (bvsle ?x72 n)))
 (or $x1781 (= ?x11647 ?x8213)))))))
 ))
 (let (($x4355 (= ?x154 (bvadd (_ bv1 6) ?x72))))
 (let (($x9125 (forall ((w0 (_ BitVec 256)) )(let ((?x4508 (ite (= (stack_s x_0 x_1 x_MLOAD_0 2 (bvadd (_ bv63 6) (sc_s 2))) w0) x_MLOAD_0 (_ bv0 256))))
 (let ((?x6149 (f_MLOAD x_0 x_1 x_MLOAD_0 w0)))
 (= ?x6149 ?x4508))))
 ))
 (let (($x1382 (forall ((w (_ BitVec 256)) )(let ((?x3997 (storage_s x_0 x_1 x_MLOAD_0 0 w)))
 (= ?x3997 (_ bv0 256))))
 ))
 (let (($x9162 (= ?x4475 0)))
 (let (($x7103 (not $x57)))
 (let (($x11669 (= (stack_s x_0 x_1 x_MLOAD_0 0 (_ bv1 6)) x_1)))
 (let (($x9304 (= (stack_s x_0 x_1 x_MLOAD_0 0 (_ bv0 6)) x_0)))
 (let (($x6476 (= ?x72 (_ bv2 6))))
 (and $x6476 $x9304 $x11669 $x7103 $x9162 $x1382 $x9125 (= (stack_s x_0 x_1 x_MLOAD_0 1 ?x72) (_ bv0 256)) (= (used_gas_s x_0 x_1 x_MLOAD_0 1) (+ 3 ?x4475)) $x4355 $x5315 $x4816 $x426 (= ?x8535 ?x4984) (= (stack_s x_0 x_1 x_MLOAD_0 2 ?x8293) ?x4984) $x3282 $x4341 (= ?x6102 (+ 3 (used_gas_s x_0 x_1 x_MLOAD_0 1))) $x7159 $x10452 $x3935 (= $x10052 (or $x613 (not (bvsle (_ bv0 6) ?x8293)) $x8780)) (= ?x8703 (f_MLOAD x_0 x_1 x_MLOAD_0 ?x8535)) (= ?x1365 (+ 3 ?x6102)) $x587 $x1804 $x9697 $x4084 (= ?x6717 ?x2134) (= ?x8679 (+ 3 ?x1365)) $x6297 $x10418 $x11334 $x8838 (= (stack_s x_0 x_1 x_MLOAD_0 5 (bvadd (_ bv63 6) ?x4319)) ?x89) $x8536 $x6285 $x1895 $x9167 $x4554 (= ?x8208 ?x11282) (= (stack_t x_0 x_1 x_MLOAD_0 1 ?x6311) ?x11282) $x11840 (= (used_gas_t x_0 x_1 x_MLOAD_0 1) (+ 3 ?x4502)) $x5757 $x890 $x6791 $x8478 (= ?x7331 (f_MLOAD x_0 x_1 x_MLOAD_0 ?x8208)) (= ?x11950 (+ 3 (used_gas_t x_0 x_1 x_MLOAD_0 1))) $x7636 $x2934 $x4689 $x2205 (= (stack_t x_0 x_1 x_MLOAD_0 3 (bvadd (_ bv63 6) ?x10013)) ?x11897) $x5712 $x2518 $x3801 $x3628 $x10876 $x73 $x2100 $x58 $x6398 $x11994 (not (and $x6289 $x3285 $x11447 $x5386)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
 )
(check-sat)
(get-proof)