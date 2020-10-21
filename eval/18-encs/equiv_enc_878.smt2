; PUSH 0x04 DUP1 DUP1 PUSH 0xa0 ADD SWAP1 => PUSH 0x04 PUSH 0xa4 PUSH 0x04
(set-option :produce-proofs true)
; 
(set-info :status unknown)
(declare-fun storage_t (Int (_ BitVec 256)) (_ BitVec 256))
(declare-fun storage_s (Int (_ BitVec 256)) (_ BitVec 256))
(declare-fun exc_halt_t (Int) Bool)
(declare-fun exc_halt_s (Int) Bool)
(declare-fun stack_t (Int (_ BitVec 6)) (_ BitVec 256))
(declare-fun stack_s (Int (_ BitVec 6)) (_ BitVec 256))
(declare-fun sc_t (Int) (_ BitVec 6))
(declare-fun sc_s (Int) (_ BitVec 6))
(declare-fun used_gas_t (Int) Int)
(declare-fun used_gas_s (Int) Int)
(assert
 (let (($x4247 (forall ((w (_ BitVec 256)) )(let ((?x3161 (storage_t 3 w)))
 (let ((?x6534 (storage_s 6 w)))
 (= ?x6534 ?x3161))))
 ))
 (let (($x9131 (exc_halt_t 3)))
 (let (($x7121 (exc_halt_s 6)))
 (let (($x10285 (= $x7121 $x9131)))
 (let (($x5146 (forall ((n (_ BitVec 6)) )(let ((?x5538 (stack_t 3 n)))
 (let ((?x1224 (stack_s 6 n)))
 (let (($x3511 (= ?x1224 ?x5538)))
 (let ((?x10013 (sc_t 3)))
 (let (($x2131 (bvsle ?x10013 n)))
 (or $x2131 $x3511)))))))
 ))
 (let ((?x10013 (sc_t 3)))
 (let ((?x9114 (sc_s 6)))
 (let (($x10208 (= ?x9114 ?x10013)))
 (let ((?x1252 (used_gas_t 0)))
 (let ((?x603 (used_gas_s 0)))
 (let (($x6037 (= ?x603 ?x1252)))
 (let (($x5172 (forall ((w (_ BitVec 256)) )(let ((?x10380 (storage_t 0 w)))
 (let ((?x7978 (storage_s 0 w)))
 (= ?x7978 ?x10380))))
 ))
 (let (($x56 (exc_halt_t 0)))
 (let (($x57 (exc_halt_s 0)))
 (let (($x58 (= $x57 $x56)))
 (let (($x7794 (forall ((n (_ BitVec 6)) )(let ((?x7120 (stack_t 0 n)))
 (let ((?x10386 (stack_s 0 n)))
 (let (($x10402 (= ?x10386 ?x7120)))
 (let ((?x63 (sc_t 0)))
 (let (($x4370 (bvsle ?x63 n)))
 (or $x4370 $x10402)))))))
 ))
 (let ((?x63 (sc_t 0)))
 (let ((?x72 (sc_s 0)))
 (let (($x73 (= ?x72 ?x63)))
 (let (($x6733 (exc_halt_t 2)))
 (let (($x7050 (or $x6733 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_t 2)))) (_ bv0 1))))))
 (let (($x2732 (= $x9131 $x7050)))
 (let (($x10569 (forall ((w (_ BitVec 256)) )(let ((?x10358 (storage_t 2 w)))
 (let ((?x3161 (storage_t 3 w)))
 (= ?x3161 ?x10358))))
 ))
 (let (($x9193 (forall ((n (_ BitVec 6)) )(let ((?x11248 (sc_t 2)))
 (let (($x3783 (bvsle ?x11248 n)))
 (or (= (stack_t 3 n) (stack_t 2 n)) $x3783))))
 ))
 (let (($x3557 (= ?x10013 (bvadd (_ bv1 6) (sc_t 2)))))
 (let (($x8478 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_t 1)))) (_ bv0 1)))))
 (let (($x4852 (exc_halt_t 1)))
 (let (($x4921 (= $x6733 (or $x4852 $x8478))))
 (let (($x10401 (forall ((w (_ BitVec 256)) )(let ((?x5778 (storage_t 1 w)))
 (let ((?x10358 (storage_t 2 w)))
 (= ?x10358 ?x5778))))
 ))
 (let (($x8763 (forall ((n (_ BitVec 6)) )(let ((?x9666 (sc_t 1)))
 (let (($x8463 (bvsle ?x9666 n)))
 (or (= (stack_t 2 n) (stack_t 1 n)) $x8463))))
 ))
 (let ((?x11248 (sc_t 2)))
 (let (($x9195 (= ?x11248 (bvadd (_ bv1 6) (sc_t 1)))))
 (let ((?x4646 (used_gas_t 2)))
 (let (($x8312 (= ?x4646 (+ 3 (used_gas_t 1)))))
 (let (($x71 (= $x4852 (or $x56 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x63))) (_ bv0 1)))))))
 (let (($x6446 (forall ((w (_ BitVec 256)) )(let ((?x10380 (storage_t 0 w)))
 (let ((?x5778 (storage_t 1 w)))
 (= ?x5778 ?x10380))))
 ))
 (let (($x9057 (forall ((n (_ BitVec 6)) )(let ((?x63 (sc_t 0)))
 (let (($x4370 (bvsle ?x63 n)))
 (or $x4370 (= (stack_t 1 n) (stack_t 0 n))))))
 ))
 (let ((?x9666 (sc_t 1)))
 (let (($x11820 (= ?x9666 (bvadd (_ bv1 6) ?x63))))
 (let ((?x7287 (used_gas_t 1)))
 (let (($x5942 (= ?x7287 (+ 3 ?x1252))))
 (let (($x11792 (= $x7121 (or (exc_halt_s 5) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 5))))))))
 (let (($x10247 (forall ((w (_ BitVec 256)) )(let ((?x8616 (storage_s 5 w)))
 (let ((?x6534 (storage_s 6 w)))
 (= ?x6534 ?x8616))))
 ))
 (let (($x3286 (forall ((n (_ BitVec 6)) )(let ((?x4319 (sc_s 5)))
 (let ((?x2123 (bvadd (_ bv62 6) ?x4319)))
 (let (($x8158 (bvsle ?x2123 n)))
 (let ((?x4736 (stack_s 5 n)))
 (let ((?x1224 (stack_s 6 n)))
 (let (($x4176 (= ?x1224 ?x4736)))
 (or $x4176 $x8158))))))))
 ))
 (let ((?x4319 (sc_s 5)))
 (let (($x4398 (= ?x9114 ?x4319)))
 (let (($x811 (= (used_gas_s 6) (+ 3 (used_gas_s 5)))))
 (let ((?x4362 (bvadd (_ bv63 6) ?x4319)))
 (let ((?x189 (stack_s 5 ?x4362)))
 (let ((?x11981 (bvadd (_ bv63 6) ?x9114)))
 (let ((?x2209 (stack_s 6 ?x11981)))
 (let (($x1862 (exc_halt_s 5)))
 (let (($x1757 (= $x1862 (or (exc_halt_s 4) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 4))))))))
 (let (($x5254 (forall ((w (_ BitVec 256)) )(let ((?x11898 (storage_s 4 w)))
 (let ((?x8616 (storage_s 5 w)))
 (= ?x8616 ?x11898))))
 ))
 (let (($x6922 (forall ((n (_ BitVec 6)) )(let ((?x9433 (sc_s 4)))
 (let ((?x3405 (bvadd (_ bv62 6) ?x9433)))
 (let (($x8627 (bvsle ?x3405 n)))
 (let ((?x11031 (stack_s 4 n)))
 (let ((?x4736 (stack_s 5 n)))
 (let (($x6210 (= ?x4736 ?x11031)))
 (or $x6210 $x8627))))))))
 ))
 (let ((?x9433 (sc_s 4)))
 (let ((?x9405 (bvadd (_ bv63 6) ?x9433)))
 (let (($x9173 (= ?x4319 ?x9405)))
 (let ((?x5185 (used_gas_s 5)))
 (let (($x7850 (= ?x5185 (+ 3 (used_gas_s 4)))))
 (let (($x1782 (= ?x189 (bvadd (stack_s 4 ?x9405) (stack_s 4 (bvadd (_ bv62 6) ?x9433))))))
 (let (($x2541 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 3)))) (_ bv0 1)))))
 (let (($x8103 (exc_halt_s 3)))
 (let (($x9175 (exc_halt_s 4)))
 (let (($x6274 (= $x9175 (or $x8103 $x2541))))
 (let (($x3707 (forall ((w (_ BitVec 256)) )(let ((?x1089 (storage_s 3 w)))
 (let ((?x11898 (storage_s 4 w)))
 (= ?x11898 ?x1089))))
 ))
 (let (($x8600 (forall ((n (_ BitVec 6)) )(let ((?x3851 (sc_s 3)))
 (let (($x955 (bvsle ?x3851 n)))
 (let ((?x11303 (stack_s 3 n)))
 (let ((?x11031 (stack_s 4 n)))
 (let (($x557 (= ?x11031 ?x11303)))
 (or $x557 $x955)))))))
 ))
 (let (($x9449 (= ?x9433 (bvadd (_ bv1 6) (sc_s 3)))))
 (let ((?x730 (used_gas_s 4)))
 (let (($x749 (= ?x730 (+ 3 (used_gas_s 3)))))
 (let (($x11936 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 2)))) (_ bv0 1)))))
 (let (($x10052 (exc_halt_s 2)))
 (let (($x3694 (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_s 2))))))
 (let (($x6537 (= $x8103 (or $x3694 $x10052 $x11936))))
 (let (($x6481 (forall ((w (_ BitVec 256)) )(let ((?x9643 (storage_s 2 w)))
 (let ((?x1089 (storage_s 3 w)))
 (= ?x1089 ?x9643))))
 ))
 (let (($x1216 (forall ((n (_ BitVec 6)) )(let ((?x1995 (stack_s 2 n)))
 (let ((?x11303 (stack_s 3 n)))
 (let (($x5504 (= ?x11303 ?x1995)))
 (let ((?x2272 (sc_s 2)))
 (let ((?x5283 (bvadd (_ bv63 6) ?x2272)))
 (let (($x914 (bvsle ?x5283 n)))
 (or $x914 $x5504))))))))
 ))
 (let ((?x3851 (sc_s 3)))
 (let (($x2865 (= ?x3851 (bvadd (_ bv1 6) (sc_s 2)))))
 (let ((?x9731 (used_gas_s 3)))
 (let (($x8839 (= ?x9731 (+ 3 (used_gas_s 2)))))
 (let ((?x2272 (sc_s 2)))
 (let ((?x5283 (bvadd (_ bv63 6) ?x2272)))
 (let ((?x7302 (stack_s 2 ?x5283)))
 (let (($x1599 (= (stack_s 3 ?x5283) ?x7302)))
 (let (($x11747 (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_s 1))))))
 (let (($x8780 (exc_halt_s 1)))
 (let (($x2355 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 1)))) (_ bv0 1)))))
 (let (($x6980 (= $x10052 (or $x2355 $x8780 $x11747))))
 (let (($x1052 (forall ((w (_ BitVec 256)) )(let ((?x177 (storage_s 1 w)))
 (let ((?x9643 (storage_s 2 w)))
 (= ?x9643 ?x177))))
 ))
 (let (($x8530 (forall ((n (_ BitVec 6)) )(let ((?x1702 (stack_s 1 n)))
 (let ((?x1995 (stack_s 2 n)))
 (let (($x10616 (= ?x1995 ?x1702)))
 (let ((?x154 (sc_s 1)))
 (let ((?x9317 (bvadd (_ bv63 6) ?x154)))
 (let (($x1815 (bvsle ?x9317 n)))
 (or $x1815 $x10616))))))))
 ))
 (let (($x6608 (= ?x2272 (bvadd (_ bv1 6) (sc_s 1)))))
 (let ((?x3903 (used_gas_s 2)))
 (let (($x8623 (= ?x3903 (+ 3 (used_gas_s 1)))))
 (let ((?x154 (sc_s 1)))
 (let ((?x9317 (bvadd (_ bv63 6) ?x154)))
 (let ((?x1612 (stack_s 1 ?x9317)))
 (let (($x1308 (= $x8780 (or $x57 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x72))) (_ bv0 1)))))))
 (let (($x8180 (forall ((w (_ BitVec 256)) )(let ((?x7978 (storage_s 0 w)))
 (let ((?x177 (storage_s 1 w)))
 (= ?x177 ?x7978))))
 ))
 (let (($x6720 (forall ((n (_ BitVec 6)) )(let ((?x72 (sc_s 0)))
 (let (($x10283 (bvsle ?x72 n)))
 (or (= (stack_s 1 n) (stack_s 0 n)) $x10283))))
 ))
 (let (($x8172 (= ?x154 (bvadd (_ bv1 6) ?x72))))
 (let ((?x4911 (used_gas_s 1)))
 (let (($x10711 (= ?x4911 (+ 3 ?x603))))
 (let (($x10574 (forall ((w (_ BitVec 256)) )(let ((?x7978 (storage_s 0 w)))
 (= ?x7978 (_ bv0 256))))
 ))
 (let (($x10048 (= ?x603 0)))
 (let (($x2248 (not $x57)))
 (let (($x4825 (= ?x72 (_ bv0 6))))
 (and $x4825 $x2248 $x10048 $x10574 (= (stack_s 1 ?x72) (_ bv4 256)) $x10711 $x8172 $x6720 $x8180 $x1308 (= ?x7302 ?x1612) (= (stack_s 2 ?x9317) ?x1612) $x8623 $x6608 $x8530 $x1052 $x6980 (= (stack_s 3 (bvadd (_ bv63 6) ?x3851)) ?x7302) $x1599 $x8839 $x2865 $x1216 $x6481 $x6537 (= (stack_s 4 ?x3851) (_ bv160 256)) $x749 $x9449 $x8600 $x3707 $x6274 $x1782 $x7850 $x9173 $x6922 $x5254 $x1757 (= ?x2209 (stack_s 5 (bvadd (_ bv62 6) ?x4319))) (= (stack_s 6 (bvadd (_ bv62 6) ?x9114)) ?x189) $x811 $x4398 $x3286 $x10247 $x11792 (= (stack_t 1 ?x63) (_ bv4 256)) $x5942 $x11820 $x9057 $x6446 $x71 (= (stack_t 2 ?x9666) (_ bv164 256)) $x8312 $x9195 $x8763 $x10401 $x4921 (= (stack_t 3 ?x11248) (_ bv4 256)) (= (used_gas_t 3) (+ 3 ?x4646)) $x3557 $x9193 $x10569 $x2732 $x73 $x7794 $x58 $x5172 $x6037 (not (and $x10208 $x5146 $x10285 $x4247))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
(check-sat)
(get-proof)
