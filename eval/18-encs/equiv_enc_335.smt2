; DUP2 DUP2 AND PUSH cw_1 SWAP1 => PUSH cw_1 DUP2 DUP4 AND
(set-option :produce-proofs true)
; 
(set-info :status unknown)
(declare-fun storage_t ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 256)) (_ BitVec 256))
(declare-fun w_1 () (_ BitVec 256))
(declare-fun storage_s ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 256)) (_ BitVec 256))
(declare-fun exc_halt_t (Int) Bool)
(declare-fun exc_halt_s (Int) Bool)
(declare-fun stack_t ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 6)) (_ BitVec 256))
(declare-fun stack_s ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 6)) (_ BitVec 256))
(declare-fun sc_t (Int) (_ BitVec 6))
(declare-fun sc_s (Int) (_ BitVec 6))
(declare-fun used_gas_t ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int) Int)
(declare-fun used_gas_s ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int) Int)
(assert
 (exists ((x_0 (_ BitVec 256)) (x_1 (_ BitVec 256)) )(let (($x5164 (forall ((w (_ BitVec 256)) )(let ((?x7888 (storage_t x_0 x_1 w_1 4 w)))
 (let ((?x3974 (storage_s x_0 x_1 w_1 5 w)))
 (= ?x3974 ?x7888))))
 ))
 (let (($x3723 (exc_halt_t 4)))
 (let (($x3979 (exc_halt_s 5)))
 (let (($x7567 (= $x3979 $x3723)))
 (let (($x2332 (forall ((n (_ BitVec 6)) )(let ((?x9921 (stack_t x_0 x_1 w_1 4 n)))
 (let ((?x9442 (stack_s x_0 x_1 w_1 5 n)))
 (let (($x10606 (= ?x9442 ?x9921)))
 (let ((?x3757 (sc_t 4)))
 (let (($x9962 (bvsle ?x3757 n)))
 (or $x9962 $x10606)))))))
 ))
 (let ((?x3757 (sc_t 4)))
 (let ((?x805 (sc_s 5)))
 (let (($x9143 (= ?x805 ?x3757)))
 (let ((?x10226 (used_gas_t x_0 x_1 w_1 0)))
 (let ((?x8919 (used_gas_s x_0 x_1 w_1 0)))
 (let (($x648 (= ?x8919 ?x10226)))
 (let (($x506 (forall ((w (_ BitVec 256)) )(let ((?x8671 (storage_t x_0 x_1 w_1 0 w)))
 (let ((?x1368 (storage_s x_0 x_1 w_1 0 w)))
 (= ?x1368 ?x8671))))
 ))
 (let (($x56 (exc_halt_t 0)))
 (let (($x57 (exc_halt_s 0)))
 (let (($x58 (= $x57 $x56)))
 (let (($x1447 (forall ((n (_ BitVec 6)) )(let ((?x5091 (stack_t x_0 x_1 w_1 0 n)))
 (let ((?x11694 (stack_s x_0 x_1 w_1 0 n)))
 (let (($x4307 (= ?x11694 ?x5091)))
 (let ((?x63 (sc_t 0)))
 (let (($x2337 (bvsle ?x63 n)))
 (or $x2337 $x4307)))))))
 ))
 (let ((?x63 (sc_t 0)))
 (let ((?x72 (sc_s 0)))
 (let (($x73 (= ?x72 ?x63)))
 (let (($x5132 (= $x3723 (or (exc_halt_t 3) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_t 3))))))))
 (let (($x9205 (forall ((w (_ BitVec 256)) )(let ((?x9579 (storage_t x_0 x_1 w_1 3 w)))
 (let ((?x7888 (storage_t x_0 x_1 w_1 4 w)))
 (= ?x7888 ?x9579))))
 ))
 (let (($x9930 (forall ((n (_ BitVec 6)) )(let ((?x6843 (stack_t x_0 x_1 w_1 3 n)))
 (let ((?x9921 (stack_t x_0 x_1 w_1 4 n)))
 (let (($x7638 (= ?x9921 ?x6843)))
 (let ((?x6438 (sc_t 3)))
 (let ((?x10529 (bvadd (_ bv62 6) ?x6438)))
 (let (($x6481 (bvsle ?x10529 n)))
 (or $x6481 $x7638))))))))
 ))
 (let ((?x6438 (sc_t 3)))
 (let ((?x10240 (bvadd (_ bv63 6) ?x6438)))
 (let (($x2331 (= ?x3757 ?x10240)))
 (let ((?x7629 (used_gas_t x_0 x_1 w_1 4)))
 (let (($x2272 (= ?x7629 (+ 3 (used_gas_t x_0 x_1 w_1 3)))))
 (let ((?x4767 (bvor (bvnot (stack_t x_0 x_1 w_1 3 ?x10240)) (bvnot (stack_t x_0 x_1 w_1 3 (bvadd (_ bv62 6) ?x6438))))))
 (let ((?x7359 (bvadd (_ bv63 6) ?x3757)))
 (let ((?x7988 (stack_t x_0 x_1 w_1 4 ?x7359)))
 (let (($x5252 (exc_halt_t 2)))
 (let (($x4927 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_t 2)))) (_ bv0 1)))))
 (let (($x1901 (not (bvsle (_ bv0 6) (bvadd (_ bv60 6) (sc_t 2))))))
 (let (($x6783 (exc_halt_t 3)))
 (let (($x8169 (forall ((w (_ BitVec 256)) )(let ((?x10595 (storage_t x_0 x_1 w_1 2 w)))
 (let ((?x9579 (storage_t x_0 x_1 w_1 3 w)))
 (= ?x9579 ?x10595))))
 ))
 (let (($x8256 (forall ((n (_ BitVec 6)) )(let ((?x2714 (sc_t 2)))
 (let ((?x4032 (bvadd (_ bv60 6) ?x2714)))
 (let (($x10600 (bvsle ?x4032 n)))
 (let ((?x10255 (stack_t x_0 x_1 w_1 2 n)))
 (let ((?x6843 (stack_t x_0 x_1 w_1 3 n)))
 (let (($x4710 (= ?x6843 ?x10255)))
 (or $x4710 $x10600))))))))
 ))
 (let (($x9136 (= ?x6438 (bvadd (_ bv1 6) (sc_t 2)))))
 (let ((?x9340 (used_gas_t x_0 x_1 w_1 3)))
 (let (($x6254 (= ?x9340 (+ 3 (used_gas_t x_0 x_1 w_1 2)))))
 (let ((?x2714 (sc_t 2)))
 (let ((?x8825 (bvadd (_ bv63 6) ?x2714)))
 (let ((?x3741 (stack_t x_0 x_1 w_1 2 ?x8825)))
 (let ((?x4439 (bvadd (_ bv62 6) ?x2714)))
 (let ((?x9224 (stack_t x_0 x_1 w_1 2 ?x4439)))
 (let ((?x6219 (bvadd (_ bv61 6) ?x2714)))
 (let ((?x10750 (stack_t x_0 x_1 w_1 2 ?x6219)))
 (let ((?x4032 (bvadd (_ bv60 6) ?x2714)))
 (let ((?x10510 (stack_t x_0 x_1 w_1 2 ?x4032)))
 (let ((?x5300 (stack_t x_0 x_1 w_1 3 ?x10240)))
 (let (($x7806 (= ?x5300 ?x10510)))
 (let (($x9927 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_t 1)))) (_ bv0 1)))))
 (let (($x326 (exc_halt_t 1)))
 (let (($x1859 (= $x5252 (or $x326 (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_t 1)))) $x9927))))
 (let (($x5875 (forall ((w (_ BitVec 256)) )(let ((?x1665 (storage_t x_0 x_1 w_1 1 w)))
 (let ((?x10595 (storage_t x_0 x_1 w_1 2 w)))
 (= ?x10595 ?x1665))))
 ))
 (let (($x8823 (forall ((n (_ BitVec 6)) )(let ((?x4461 (stack_t x_0 x_1 w_1 1 n)))
 (let ((?x10255 (stack_t x_0 x_1 w_1 2 n)))
 (let (($x9469 (= ?x10255 ?x4461)))
 (let ((?x11560 (sc_t 1)))
 (let ((?x2726 (bvadd (_ bv62 6) ?x11560)))
 (let (($x2848 (bvsle ?x2726 n)))
 (or $x2848 $x9469))))))))
 ))
 (let (($x10865 (= ?x2714 (bvadd (_ bv1 6) (sc_t 1)))))
 (let ((?x2759 (used_gas_t x_0 x_1 w_1 2)))
 (let (($x1425 (= ?x2759 (+ 3 (used_gas_t x_0 x_1 w_1 1)))))
 (let ((?x11560 (sc_t 1)))
 (let ((?x8596 (bvadd (_ bv63 6) ?x11560)))
 (let ((?x6511 (stack_t x_0 x_1 w_1 1 ?x8596)))
 (let (($x8854 (= (stack_t x_0 x_1 w_1 2 ?x8596) ?x6511)))
 (let ((?x2726 (bvadd (_ bv62 6) ?x11560)))
 (let ((?x4899 (stack_t x_0 x_1 w_1 1 ?x2726)))
 (let (($x5039 (= (stack_t x_0 x_1 w_1 2 ?x2726) ?x4899)))
 (let (($x3753 (= $x326 (or $x56 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x63))) (_ bv0 1)))))))
 (let (($x915 (forall ((w (_ BitVec 256)) )(let ((?x8671 (storage_t x_0 x_1 w_1 0 w)))
 (let ((?x1665 (storage_t x_0 x_1 w_1 1 w)))
 (= ?x1665 ?x8671))))
 ))
 (let (($x8626 (forall ((n (_ BitVec 6)) )(let ((?x5091 (stack_t x_0 x_1 w_1 0 n)))
 (let ((?x4461 (stack_t x_0 x_1 w_1 1 n)))
 (let (($x7207 (= ?x4461 ?x5091)))
 (let ((?x63 (sc_t 0)))
 (let (($x2337 (bvsle ?x63 n)))
 (or $x2337 $x7207)))))))
 ))
 (let (($x7646 (= ?x11560 (bvadd (_ bv1 6) ?x63))))
 (let ((?x4848 (used_gas_t x_0 x_1 w_1 1)))
 (let (($x5170 (= ?x4848 (+ 3 ?x10226))))
 (let (($x9010 (= $x3979 (or (exc_halt_s 4) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 4))))))))
 (let (($x991 (forall ((w (_ BitVec 256)) )(let ((?x10805 (storage_s x_0 x_1 w_1 4 w)))
 (let ((?x3974 (storage_s x_0 x_1 w_1 5 w)))
 (= ?x3974 ?x10805))))
 ))
 (let (($x6505 (forall ((n (_ BitVec 6)) )(let ((?x4305 (sc_s 4)))
 (let ((?x8912 (bvadd (_ bv62 6) ?x4305)))
 (let (($x9203 (bvsle ?x8912 n)))
 (let ((?x4524 (stack_s x_0 x_1 w_1 4 n)))
 (let ((?x9442 (stack_s x_0 x_1 w_1 5 n)))
 (or (= ?x9442 ?x4524) $x9203)))))))
 ))
 (let ((?x4305 (sc_s 4)))
 (let (($x7861 (= ?x805 ?x4305)))
 (let ((?x5920 (used_gas_s x_0 x_1 w_1 5)))
 (let (($x1013 (= ?x5920 (+ 3 (used_gas_s x_0 x_1 w_1 4)))))
 (let ((?x10093 (bvadd (_ bv63 6) ?x4305)))
 (let ((?x3647 (stack_s x_0 x_1 w_1 4 ?x10093)))
 (let ((?x7784 (bvadd (_ bv62 6) ?x805)))
 (let ((?x5107 (stack_s x_0 x_1 w_1 5 ?x7784)))
 (let (($x4060 (= ?x5107 ?x3647)))
 (let ((?x8912 (bvadd (_ bv62 6) ?x4305)))
 (let ((?x5980 (stack_s x_0 x_1 w_1 4 ?x8912)))
 (let ((?x8116 (bvadd (_ bv63 6) ?x805)))
 (let ((?x1505 (stack_s x_0 x_1 w_1 5 ?x8116)))
 (let (($x864 (= ?x1505 ?x5980)))
 (let (($x7104 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 3)))) (_ bv0 1)))))
 (let (($x292 (exc_halt_s 3)))
 (let (($x7172 (exc_halt_s 4)))
 (let (($x6801 (= $x7172 (or $x292 $x7104))))
 (let (($x6403 (forall ((w (_ BitVec 256)) )(let ((?x3608 (storage_s x_0 x_1 w_1 3 w)))
 (let ((?x10805 (storage_s x_0 x_1 w_1 4 w)))
 (= ?x10805 ?x3608))))
 ))
 (let (($x795 (forall ((n (_ BitVec 6)) )(let ((?x275 (sc_s 3)))
 (let (($x8265 (bvsle ?x275 n)))
 (let ((?x1156 (stack_s x_0 x_1 w_1 3 n)))
 (let ((?x4524 (stack_s x_0 x_1 w_1 4 n)))
 (let (($x1513 (= ?x4524 ?x1156)))
 (or $x1513 $x8265)))))))
 ))
 (let (($x9578 (= ?x4305 (bvadd (_ bv1 6) (sc_s 3)))))
 (let ((?x1956 (used_gas_s x_0 x_1 w_1 4)))
 (let (($x6936 (= ?x1956 (+ 3 (used_gas_s x_0 x_1 w_1 3)))))
 (let (($x4102 (= $x292 (or (exc_halt_s 2) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 2))))))))
 (let (($x3196 (forall ((w (_ BitVec 256)) )(let ((?x10028 (storage_s x_0 x_1 w_1 2 w)))
 (let ((?x3608 (storage_s x_0 x_1 w_1 3 w)))
 (= ?x3608 ?x10028))))
 ))
 (let (($x2354 (forall ((n (_ BitVec 6)) )(let ((?x6589 (stack_s x_0 x_1 w_1 2 n)))
 (let ((?x1156 (stack_s x_0 x_1 w_1 3 n)))
 (let (($x7619 (= ?x1156 ?x6589)))
 (let ((?x218 (sc_s 2)))
 (let ((?x2594 (bvadd (_ bv62 6) ?x218)))
 (let (($x5151 (bvsle ?x2594 n)))
 (or $x5151 $x7619))))))))
 ))
 (let ((?x218 (sc_s 2)))
 (let ((?x5548 (bvadd (_ bv63 6) ?x218)))
 (let ((?x275 (sc_s 3)))
 (let (($x1070 (= ?x275 ?x5548)))
 (let ((?x5263 (used_gas_s x_0 x_1 w_1 3)))
 (let (($x2142 (= ?x5263 (+ 3 (used_gas_s x_0 x_1 w_1 2)))))
 (let ((?x11243 (bvor (bvnot (stack_s x_0 x_1 w_1 2 ?x5548)) (bvnot (stack_s x_0 x_1 w_1 2 (bvadd (_ bv62 6) ?x218))))))
 (let ((?x6666 (bvadd (_ bv63 6) ?x275)))
 (let ((?x968 (stack_s x_0 x_1 w_1 3 ?x6666)))
 (let (($x9639 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 1)))) (_ bv0 1)))))
 (let (($x463 (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 1))))))
 (let (($x189 (exc_halt_s 1)))
 (let (($x247 (exc_halt_s 2)))
 (let (($x6465 (= $x247 (or $x189 $x463 $x9639))))
 (let (($x7285 (forall ((w (_ BitVec 256)) )(let ((?x8155 (storage_s x_0 x_1 w_1 1 w)))
 (let ((?x10028 (storage_s x_0 x_1 w_1 2 w)))
 (= ?x10028 ?x8155))))
 ))
 (let (($x2244 (forall ((n (_ BitVec 6)) )(let ((?x592 (stack_s x_0 x_1 w_1 1 n)))
 (let ((?x6589 (stack_s x_0 x_1 w_1 2 n)))
 (let (($x4471 (= ?x6589 ?x592)))
 (let ((?x154 (sc_s 1)))
 (let ((?x1353 (bvadd (_ bv62 6) ?x154)))
 (let (($x2007 (bvsle ?x1353 n)))
 (or $x2007 $x4471))))))))
 ))
 (let (($x747 (= ?x218 (bvadd (_ bv1 6) (sc_s 1)))))
 (let ((?x5213 (used_gas_s x_0 x_1 w_1 2)))
 (let (($x4482 (= ?x5213 (+ 3 (used_gas_s x_0 x_1 w_1 1)))))
 (let ((?x154 (sc_s 1)))
 (let ((?x10146 (bvadd (_ bv63 6) ?x154)))
 (let ((?x9608 (stack_s x_0 x_1 w_1 1 ?x10146)))
 (let (($x2967 (= (stack_s x_0 x_1 w_1 2 ?x10146) ?x9608)))
 (let ((?x1353 (bvadd (_ bv62 6) ?x154)))
 (let ((?x11634 (stack_s x_0 x_1 w_1 1 ?x1353)))
 (let (($x7568 (= (stack_s x_0 x_1 w_1 2 ?x1353) ?x11634)))
 (let ((?x5431 (stack_s x_0 x_1 w_1 2 ?x5548)))
 (let (($x2671 (= ?x5431 ?x11634)))
 (let (($x8536 (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) ?x72)))))
 (let (($x7088 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x72))) (_ bv0 1)))))
 (let (($x3557 (forall ((w (_ BitVec 256)) )(let ((?x1368 (storage_s x_0 x_1 w_1 0 w)))
 (let ((?x8155 (storage_s x_0 x_1 w_1 1 w)))
 (= ?x8155 ?x1368))))
 ))
 (let (($x6048 (forall ((n (_ BitVec 6)) )(let ((?x11694 (stack_s x_0 x_1 w_1 0 n)))
 (let ((?x592 (stack_s x_0 x_1 w_1 1 n)))
 (let (($x8747 (= ?x592 ?x11694)))
 (let ((?x72 (sc_s 0)))
 (let ((?x6241 (bvadd (_ bv62 6) ?x72)))
 (let (($x6993 (bvsle ?x6241 n)))
 (or $x6993 $x8747))))))))
 ))
 (let (($x3297 (= ?x154 (bvadd (_ bv1 6) ?x72))))
 (let ((?x1419 (used_gas_s x_0 x_1 w_1 1)))
 (let (($x1690 (= ?x1419 (+ 3 ?x8919))))
 (let ((?x3826 (bvadd (_ bv63 6) ?x72)))
 (let ((?x5996 (stack_s x_0 x_1 w_1 0 ?x3826)))
 (let ((?x6241 (bvadd (_ bv62 6) ?x72)))
 (let ((?x4634 (stack_s x_0 x_1 w_1 0 ?x6241)))
 (let (($x6121 (= ?x9608 ?x4634)))
 (let (($x8809 (forall ((w (_ BitVec 256)) )(let ((?x1368 (storage_s x_0 x_1 w_1 0 w)))
 (= ?x1368 (_ bv0 256))))
 ))
 (let (($x3620 (= ?x8919 0)))
 (let (($x4887 (not $x57)))
 (let (($x10550 (= (stack_s x_0 x_1 w_1 0 (_ bv1 6)) x_1)))
 (let (($x6739 (= (stack_s x_0 x_1 w_1 0 (_ bv0 6)) x_0)))
 (let (($x1103 (= ?x72 (_ bv2 6))))
 (and $x1103 $x6739 $x10550 $x4887 $x3620 $x8809 $x6121 (= (stack_s x_0 x_1 w_1 1 ?x6241) ?x4634) (= (stack_s x_0 x_1 w_1 1 ?x3826) ?x5996) $x1690 $x3297 $x6048 $x3557 (= $x189 (or $x57 $x7088 $x8536)) $x2671 $x7568 $x2967 $x4482 $x747 $x2244 $x7285 $x6465 (= ?x968 (bvnot ?x11243)) $x2142 $x1070 $x2354 $x3196 $x4102 (= (stack_s x_0 x_1 w_1 4 ?x275) w_1) $x6936 $x9578 $x795 $x6403 $x6801 $x864 $x4060 $x1013 $x7861 $x6505 $x991 $x9010 (= (stack_t x_0 x_1 w_1 1 ?x63) w_1) $x5170 $x7646 $x8626 $x915 $x3753 (= ?x3741 ?x4899) $x5039 $x8854 $x1425 $x10865 $x8823 $x5875 $x1859 $x7806 (= (stack_t x_0 x_1 w_1 3 ?x4032) ?x10510) (= (stack_t x_0 x_1 w_1 3 ?x6219) ?x10750) (= (stack_t x_0 x_1 w_1 3 ?x4439) ?x9224) (= (stack_t x_0 x_1 w_1 3 ?x8825) ?x3741) $x6254 $x9136 $x8256 $x8169 (= $x6783 (or $x1901 $x4927 $x5252)) (= ?x7988 (bvnot ?x4767)) $x2272 $x2331 $x9930 $x9205 $x5132 $x73 $x1447 $x58 $x506 $x648 (not (and $x9143 $x2332 $x7567 $x5164))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
 )
(check-sat)
(get-proof)
