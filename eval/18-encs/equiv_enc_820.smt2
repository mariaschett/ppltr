; PUSH cw_3 MLOAD PUSH cw_2 SWAP1 DUP2 SWAP1 => PUSH cw_2 DUP1 PUSH cw_3 MLOAD
(set-option :produce-proofs true)
; 
(set-info :status unknown)
(declare-fun storage_t ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 256)) (_ BitVec 256))
(declare-fun w_2 () (_ BitVec 256))
(declare-fun w_3 () (_ BitVec 256))
(declare-fun storage_s ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 256)) (_ BitVec 256))
(declare-fun exc_halt_t (Int) Bool)
(declare-fun exc_halt_s (Int) Bool)
(declare-fun stack_t ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 6)) (_ BitVec 256))
(declare-fun stack_s ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 6)) (_ BitVec 256))
(declare-fun sc_t (Int) (_ BitVec 6))
(declare-fun sc_s (Int) (_ BitVec 6))
(declare-fun used_gas_t ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int) Int)
(declare-fun used_gas_s ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int) Int)
(declare-fun f_MLOAD ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256)) (_ BitVec 256))
(assert
 (exists ((x_MLOAD_0 (_ BitVec 256)) )(let (($x4197 (forall ((w (_ BitVec 256)) )(let ((?x11846 (storage_t w_3 w_2 x_MLOAD_0 4 w)))
 (let ((?x6382 (storage_s w_3 w_2 x_MLOAD_0 6 w)))
 (= ?x6382 ?x11846))))
 ))
 (let (($x7854 (exc_halt_t 4)))
 (let (($x7121 (exc_halt_s 6)))
 (let (($x4084 (= $x7121 $x7854)))
 (let (($x11533 (forall ((n (_ BitVec 6)) )(let ((?x6322 (stack_t w_3 w_2 x_MLOAD_0 4 n)))
 (let ((?x2815 (stack_s w_3 w_2 x_MLOAD_0 6 n)))
 (let (($x8102 (= ?x2815 ?x6322)))
 (or (bvsle (sc_t 4) n) $x8102)))))
 ))
 (let ((?x7495 (sc_t 4)))
 (let ((?x9114 (sc_s 6)))
 (let (($x10418 (= ?x9114 ?x7495)))
 (let ((?x10970 (used_gas_t w_3 w_2 x_MLOAD_0 0)))
 (let ((?x2943 (used_gas_s w_3 w_2 x_MLOAD_0 0)))
 (let (($x6133 (= ?x2943 ?x10970)))
 (let (($x5292 (forall ((w (_ BitVec 256)) )(let ((?x3884 (storage_t w_3 w_2 x_MLOAD_0 0 w)))
 (let ((?x184 (storage_s w_3 w_2 x_MLOAD_0 0 w)))
 (= ?x184 ?x3884))))
 ))
 (let (($x56 (exc_halt_t 0)))
 (let (($x57 (exc_halt_s 0)))
 (let (($x58 (= $x57 $x56)))
 (let (($x11241 (forall ((n (_ BitVec 6)) )(let ((?x63 (sc_t 0)))
 (let (($x2738 (bvsle ?x63 n)))
 (let ((?x6596 (stack_t w_3 w_2 x_MLOAD_0 0 n)))
 (let ((?x6773 (stack_s w_3 w_2 x_MLOAD_0 0 n)))
 (let (($x1086 (= ?x6773 ?x6596)))
 (or $x1086 $x2738)))))))
 ))
 (let ((?x63 (sc_t 0)))
 (let ((?x72 (sc_s 0)))
 (let (($x73 (= ?x72 ?x63)))
 (let (($x9706 (= $x7854 (or (exc_halt_t 3) (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_t 3))))))))
 (let (($x9192 (forall ((w (_ BitVec 256)) )(let ((?x3519 (storage_t w_3 w_2 x_MLOAD_0 3 w)))
 (let ((?x11846 (storage_t w_3 w_2 x_MLOAD_0 4 w)))
 (= ?x11846 ?x3519))))
 ))
 (let (($x5706 (forall ((n (_ BitVec 6)) )(let ((?x2077 (stack_t w_3 w_2 x_MLOAD_0 3 n)))
 (let ((?x6322 (stack_t w_3 w_2 x_MLOAD_0 4 n)))
 (or (bvsle (bvadd (_ bv63 6) (sc_t 3)) n) (= ?x6322 ?x2077)))))
 ))
 (let (($x3638 (= (used_gas_t w_3 w_2 x_MLOAD_0 4) (+ 3 (used_gas_t w_3 w_2 x_MLOAD_0 3)))))
 (let ((?x5720 (f_MLOAD w_3 w_2 x_MLOAD_0 (stack_t w_3 w_2 x_MLOAD_0 3 (bvadd (_ bv63 6) (sc_t 3))))))
 (let (($x7905 (exc_halt_t 2)))
 (let (($x772 (or $x7905 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_t 2)))) (_ bv0 1))))))
 (let (($x418 (exc_halt_t 3)))
 (let (($x9081 (forall ((w (_ BitVec 256)) )(let ((?x4296 (storage_t w_3 w_2 x_MLOAD_0 2 w)))
 (let ((?x3519 (storage_t w_3 w_2 x_MLOAD_0 3 w)))
 (= ?x3519 ?x4296))))
 ))
 (let (($x1837 (forall ((n (_ BitVec 6)) )(let ((?x9993 (stack_t w_3 w_2 x_MLOAD_0 2 n)))
 (let ((?x2077 (stack_t w_3 w_2 x_MLOAD_0 3 n)))
 (or (= ?x2077 ?x9993) (bvsle (sc_t 2) n)))))
 ))
 (let ((?x4610 (used_gas_t w_3 w_2 x_MLOAD_0 3)))
 (let (($x4852 (exc_halt_t 1)))
 (let (($x6477 (or (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_t 1)))) (_ bv0 1))) (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_t 1)))) $x4852)))
 (let (($x5840 (forall ((w (_ BitVec 256)) )(let ((?x3465 (storage_t w_3 w_2 x_MLOAD_0 1 w)))
 (let ((?x4296 (storage_t w_3 w_2 x_MLOAD_0 2 w)))
 (= ?x4296 ?x3465))))
 ))
 (let (($x11622 (forall ((n (_ BitVec 6)) )(let ((?x5380 (stack_t w_3 w_2 x_MLOAD_0 1 n)))
 (let ((?x9993 (stack_t w_3 w_2 x_MLOAD_0 2 n)))
 (or (bvsle (bvadd (_ bv63 6) (sc_t 1)) n) (= ?x9993 ?x5380)))))
 ))
 (let ((?x5798 (used_gas_t w_3 w_2 x_MLOAD_0 2)))
 (let ((?x9666 (sc_t 1)))
 (let ((?x10665 (bvadd (_ bv63 6) ?x9666)))
 (let ((?x7973 (stack_t w_3 w_2 x_MLOAD_0 1 ?x10665)))
 (let (($x489 (= $x4852 (or $x56 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x63))) (_ bv0 1)))))))
 (let (($x1934 (forall ((w (_ BitVec 256)) )(let ((?x3884 (storage_t w_3 w_2 x_MLOAD_0 0 w)))
 (let ((?x3465 (storage_t w_3 w_2 x_MLOAD_0 1 w)))
 (= ?x3465 ?x3884))))
 ))
 (let (($x9939 (forall ((n (_ BitVec 6)) )(let ((?x6596 (stack_t w_3 w_2 x_MLOAD_0 0 n)))
 (let ((?x5380 (stack_t w_3 w_2 x_MLOAD_0 1 n)))
 (let ((?x63 (sc_t 0)))
 (let (($x2738 (bvsle ?x63 n)))
 (or $x2738 (= ?x5380 ?x6596)))))))
 ))
 (let (($x11205 (= $x7121 (or (exc_halt_s 5) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 5))))))))
 (let (($x11102 (forall ((w (_ BitVec 256)) )(let ((?x9190 (storage_s w_3 w_2 x_MLOAD_0 5 w)))
 (let ((?x6382 (storage_s w_3 w_2 x_MLOAD_0 6 w)))
 (= ?x6382 ?x9190))))
 ))
 (let (($x6996 (forall ((n (_ BitVec 6)) )(let ((?x3423 (stack_s w_3 w_2 x_MLOAD_0 5 n)))
 (let ((?x2815 (stack_s w_3 w_2 x_MLOAD_0 6 n)))
 (or (bvsle (bvadd (_ bv62 6) (sc_s 5)) n) (= ?x2815 ?x3423)))))
 ))
 (let (($x241 (= (used_gas_s w_3 w_2 x_MLOAD_0 6) (+ 3 (used_gas_s w_3 w_2 x_MLOAD_0 5)))))
 (let ((?x11625 (stack_s w_3 w_2 x_MLOAD_0 5 (bvadd (_ bv63 6) (sc_s 5)))))
 (let (($x9689 (= (stack_s w_3 w_2 x_MLOAD_0 6 (bvadd (_ bv63 6) ?x9114)) (stack_s w_3 w_2 x_MLOAD_0 5 (bvadd (_ bv62 6) (sc_s 5))))))
 (let (($x9175 (exc_halt_s 4)))
 (let (($x5800 (or (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 4)))) $x9175 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 4)))) (_ bv0 1))))))
 (let (($x1862 (exc_halt_s 5)))
 (let (($x7723 (forall ((w (_ BitVec 256)) )(let ((?x11707 (storage_s w_3 w_2 x_MLOAD_0 4 w)))
 (let ((?x9190 (storage_s w_3 w_2 x_MLOAD_0 5 w)))
 (= ?x9190 ?x11707))))
 ))
 (let (($x3226 (forall ((n (_ BitVec 6)) )(let ((?x11144 (stack_s w_3 w_2 x_MLOAD_0 4 n)))
 (let ((?x3423 (stack_s w_3 w_2 x_MLOAD_0 5 n)))
 (or (= ?x3423 ?x11144) (bvsle (bvadd (_ bv62 6) (sc_s 4)) n)))))
 ))
 (let ((?x10421 (used_gas_s w_3 w_2 x_MLOAD_0 5)))
 (let ((?x9433 (sc_s 4)))
 (let ((?x11699 (bvadd (_ bv63 6) ?x9433)))
 (let ((?x4353 (stack_s w_3 w_2 x_MLOAD_0 4 ?x11699)))
 (let ((?x1557 (bvadd (_ bv62 6) ?x9433)))
 (let ((?x10211 (stack_s w_3 w_2 x_MLOAD_0 4 ?x1557)))
 (let (($x10301 (= $x9175 (or (exc_halt_s 3) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 3))))))))
 (let (($x1355 (forall ((w (_ BitVec 256)) )(let ((?x1309 (storage_s w_3 w_2 x_MLOAD_0 3 w)))
 (let ((?x11707 (storage_s w_3 w_2 x_MLOAD_0 4 w)))
 (= ?x11707 ?x1309))))
 ))
 (let (($x724 (forall ((n (_ BitVec 6)) )(let ((?x9049 (stack_s w_3 w_2 x_MLOAD_0 3 n)))
 (let ((?x11144 (stack_s w_3 w_2 x_MLOAD_0 4 n)))
 (or (bvsle (bvadd (_ bv62 6) (sc_s 3)) n) (= ?x11144 ?x9049)))))
 ))
 (let ((?x10175 (used_gas_s w_3 w_2 x_MLOAD_0 4)))
 (let (($x10052 (exc_halt_s 2)))
 (let (($x3739 (or $x10052 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 2)))) (_ bv0 1))))))
 (let (($x8103 (exc_halt_s 3)))
 (let (($x11540 (forall ((w (_ BitVec 256)) )(let ((?x4838 (storage_s w_3 w_2 x_MLOAD_0 2 w)))
 (let ((?x1309 (storage_s w_3 w_2 x_MLOAD_0 3 w)))
 (= ?x1309 ?x4838))))
 ))
 (let (($x840 (forall ((n (_ BitVec 6)) )(let ((?x2913 (stack_s w_3 w_2 x_MLOAD_0 2 n)))
 (let ((?x9049 (stack_s w_3 w_2 x_MLOAD_0 3 n)))
 (or (= ?x9049 ?x2913) (bvsle (sc_s 2) n)))))
 ))
 (let ((?x4727 (used_gas_s w_3 w_2 x_MLOAD_0 3)))
 (let (($x5600 (= $x10052 (or (exc_halt_s 1) (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_s 1))))))))
 (let (($x9271 (forall ((w (_ BitVec 256)) )(let ((?x8897 (storage_s w_3 w_2 x_MLOAD_0 1 w)))
 (let ((?x4838 (storage_s w_3 w_2 x_MLOAD_0 2 w)))
 (= ?x4838 ?x8897))))
 ))
 (let (($x9202 (forall ((n (_ BitVec 6)) )(let ((?x4095 (stack_s w_3 w_2 x_MLOAD_0 1 n)))
 (let ((?x2913 (stack_s w_3 w_2 x_MLOAD_0 2 n)))
 (or (bvsle (bvadd (_ bv63 6) (sc_s 1)) n) (= ?x2913 ?x4095)))))
 ))
 (let ((?x2669 (used_gas_s w_3 w_2 x_MLOAD_0 2)))
 (let ((?x11182 (f_MLOAD w_3 w_2 x_MLOAD_0 (stack_s w_3 w_2 x_MLOAD_0 1 (bvadd (_ bv63 6) (sc_s 1))))))
 (let (($x8780 (exc_halt_s 1)))
 (let (($x10729 (= $x8780 (or $x57 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x72))) (_ bv0 1)))))))
 (let (($x8572 (forall ((w (_ BitVec 256)) )(let ((?x184 (storage_s w_3 w_2 x_MLOAD_0 0 w)))
 (let ((?x8897 (storage_s w_3 w_2 x_MLOAD_0 1 w)))
 (= ?x8897 ?x184))))
 ))
 (let (($x11073 (forall ((n (_ BitVec 6)) )(let ((?x6773 (stack_s w_3 w_2 x_MLOAD_0 0 n)))
 (let ((?x4095 (stack_s w_3 w_2 x_MLOAD_0 1 n)))
 (or (= ?x4095 ?x6773) (bvsle (sc_s 0) n)))))
 ))
 (let (($x10712 (forall ((w0 (_ BitVec 256)) )(let ((?x5082 (ite (= (stack_s w_3 w_2 x_MLOAD_0 1 (bvadd (_ bv63 6) (sc_s 1))) w0) x_MLOAD_0 (_ bv0 256))))
 (let ((?x5765 (f_MLOAD w_3 w_2 x_MLOAD_0 w0)))
 (= ?x5765 ?x5082))))
 ))
 (let (($x10054 (forall ((w (_ BitVec 256)) )(let ((?x184 (storage_s w_3 w_2 x_MLOAD_0 0 w)))
 (= ?x184 (_ bv0 256))))
 ))
 (let (($x7273 (= ?x2943 0)))
 (let (($x4825 (= ?x72 (_ bv0 6))))
 (and $x4825 (not $x57) $x7273 $x10054 $x10712 (= (stack_s w_3 w_2 x_MLOAD_0 1 ?x72) w_3) (= (used_gas_s w_3 w_2 x_MLOAD_0 1) (+ 3 ?x2943)) (= (sc_s 1) (bvadd (_ bv1 6) ?x72)) $x11073 $x8572 $x10729 (= (stack_s w_3 w_2 x_MLOAD_0 2 (bvadd (_ bv63 6) (sc_s 2))) ?x11182) (= ?x2669 (+ 3 (used_gas_s w_3 w_2 x_MLOAD_0 1))) (= (sc_s 2) (sc_s 1)) $x9202 $x9271 $x5600 (= (stack_s w_3 w_2 x_MLOAD_0 3 (sc_s 2)) w_2) (= ?x4727 (+ 3 ?x2669)) (= (sc_s 3) (bvadd (_ bv1 6) (sc_s 2))) $x840 $x11540 (= $x8103 $x3739) (= ?x4353 (stack_s w_3 w_2 x_MLOAD_0 3 (bvadd (_ bv62 6) (sc_s 3)))) (= ?x10211 (stack_s w_3 w_2 x_MLOAD_0 3 (bvadd (_ bv63 6) (sc_s 3)))) (= ?x10175 (+ 3 ?x4727)) (= ?x9433 (sc_s 3)) $x724 $x1355 $x10301 (= ?x11625 ?x10211) (= (stack_s w_3 w_2 x_MLOAD_0 5 ?x1557) ?x10211) (= (stack_s w_3 w_2 x_MLOAD_0 5 ?x11699) ?x4353) (= ?x10421 (+ 3 ?x10175)) (= (sc_s 5) (bvadd (_ bv1 6) ?x9433)) $x3226 $x7723 (= $x1862 $x5800) $x9689 (= (stack_s w_3 w_2 x_MLOAD_0 6 (bvadd (_ bv62 6) ?x9114)) ?x11625) $x241 (= ?x9114 (sc_s 5)) $x6996 $x11102 $x11205 (= (stack_t w_3 w_2 x_MLOAD_0 1 ?x63) w_2) (= (used_gas_t w_3 w_2 x_MLOAD_0 1) (+ 3 ?x10970)) (= ?x9666 (bvadd (_ bv1 6) ?x63)) $x9939 $x1934 $x489 (= (stack_t w_3 w_2 x_MLOAD_0 2 (bvadd (_ bv63 6) (sc_t 2))) ?x7973) (= (stack_t w_3 w_2 x_MLOAD_0 2 ?x10665) ?x7973) (= ?x5798 (+ 3 (used_gas_t w_3 w_2 x_MLOAD_0 1))) (= (sc_t 2) (bvadd (_ bv1 6) ?x9666)) $x11622 $x5840 (= $x7905 $x6477) (= (stack_t w_3 w_2 x_MLOAD_0 3 (sc_t 2)) w_3) (= ?x4610 (+ 3 ?x5798)) (= (sc_t 3) (bvadd (_ bv1 6) (sc_t 2))) $x1837 $x9081 (= $x418 $x772) (= (stack_t w_3 w_2 x_MLOAD_0 4 (bvadd (_ bv63 6) ?x7495)) ?x5720) $x3638 (= ?x7495 (sc_t 3)) $x5706 $x9192 $x9706 $x73 $x11241 $x58 $x5292 $x6133 (not (and $x10418 $x11533 $x4084 $x4197)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
 )
(check-sat)
(get-proof)
