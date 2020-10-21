; PUSH cw_3 SWAP1 SWAP2 SWAP1 => SWAP1 PUSH cw_3
(set-option :produce-proofs true)
; 
(set-info :status unknown)
(declare-fun storage_t ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 256)) (_ BitVec 256))
(declare-fun w_3 () (_ BitVec 256))
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
 (exists ((x_0 (_ BitVec 256)) (x_1 (_ BitVec 256)) )(let (($x1633 (forall ((w (_ BitVec 256)) )(let ((?x9917 (storage_t x_0 x_1 w_3 2 w)))
 (let ((?x4404 (storage_s x_0 x_1 w_3 4 w)))
 (= ?x4404 ?x9917))))
 ))
 (let (($x6842 (exc_halt_t 2)))
 (let (($x9175 (exc_halt_s 4)))
 (let (($x4445 (= $x9175 $x6842)))
 (let (($x7270 (forall ((n (_ BitVec 6)) )(let ((?x9666 (sc_t 2)))
 (let (($x6374 (bvsle ?x9666 n)))
 (let ((?x10086 (stack_t x_0 x_1 w_3 2 n)))
 (let ((?x9633 (stack_s x_0 x_1 w_3 4 n)))
 (let (($x48 (= ?x9633 ?x10086)))
 (or $x48 $x6374)))))))
 ))
 (let ((?x9666 (sc_t 2)))
 (let ((?x9433 (sc_s 4)))
 (let (($x2152 (= ?x9433 ?x9666)))
 (let ((?x6829 (used_gas_t x_0 x_1 w_3 0)))
 (let ((?x4334 (used_gas_s x_0 x_1 w_3 0)))
 (let (($x5740 (= ?x4334 ?x6829)))
 (let (($x10165 (forall ((w (_ BitVec 256)) )(let ((?x1587 (storage_t x_0 x_1 w_3 0 w)))
 (let ((?x6588 (storage_s x_0 x_1 w_3 0 w)))
 (= ?x6588 ?x1587))))
 ))
 (let (($x56 (exc_halt_t 0)))
 (let (($x57 (exc_halt_s 0)))
 (let (($x58 (= $x57 $x56)))
 (let (($x9929 (forall ((n (_ BitVec 6)) )(let ((?x63 (sc_t 0)))
 (let (($x3918 (bvsle ?x63 n)))
 (let ((?x6187 (stack_t x_0 x_1 w_3 0 n)))
 (let ((?x2975 (stack_s x_0 x_1 w_3 0 n)))
 (let (($x2328 (= ?x2975 ?x6187)))
 (or $x2328 $x3918)))))))
 ))
 (let ((?x63 (sc_t 0)))
 (let ((?x72 (sc_s 0)))
 (let (($x73 (= ?x72 ?x63)))
 (let (($x8403 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_t 1)))) (_ bv0 1)))))
 (let (($x11030 (exc_halt_t 1)))
 (let (($x10330 (forall ((w (_ BitVec 256)) )(let ((?x775 (storage_t x_0 x_1 w_3 1 w)))
 (let ((?x9917 (storage_t x_0 x_1 w_3 2 w)))
 (= ?x9917 ?x775))))
 ))
 (let (($x9798 (forall ((n (_ BitVec 6)) )(let ((?x3597 (stack_t x_0 x_1 w_3 1 n)))
 (let ((?x10086 (stack_t x_0 x_1 w_3 2 n)))
 (or (bvsle (sc_t 1) n) (= ?x10086 ?x3597)))))
 ))
 (let (($x9818 (= ?x9666 (bvadd (_ bv1 6) (sc_t 1)))))
 (let (($x4905 (= (used_gas_t x_0 x_1 w_3 2) (+ 3 (used_gas_t x_0 x_1 w_3 1)))))
 (let (($x3233 (forall ((w (_ BitVec 256)) )(let ((?x1587 (storage_t x_0 x_1 w_3 0 w)))
 (let ((?x775 (storage_t x_0 x_1 w_3 1 w)))
 (= ?x775 ?x1587))))
 ))
 (let (($x10768 (forall ((n (_ BitVec 6)) )(let ((?x6187 (stack_t x_0 x_1 w_3 0 n)))
 (let ((?x3597 (stack_t x_0 x_1 w_3 1 n)))
 (or (bvsle (bvadd (_ bv62 6) (sc_t 0)) n) (= ?x3597 ?x6187)))))
 ))
 (let (($x5135 (= (stack_t x_0 x_1 w_3 1 (bvadd (_ bv62 6) (sc_t 1))) (stack_t x_0 x_1 w_3 0 (bvadd (_ bv63 6) ?x63)))))
 (let (($x5189 (= (stack_t x_0 x_1 w_3 1 (bvadd (_ bv63 6) (sc_t 1))) (stack_t x_0 x_1 w_3 0 (bvadd (_ bv62 6) ?x63)))))
 (let (($x7924 (= $x9175 (or (exc_halt_s 3) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 3))))))))
 (let (($x6006 (forall ((w (_ BitVec 256)) )(let ((?x6354 (storage_s x_0 x_1 w_3 3 w)))
 (let ((?x4404 (storage_s x_0 x_1 w_3 4 w)))
 (= ?x4404 ?x6354))))
 ))
 (let (($x2055 (forall ((n (_ BitVec 6)) )(let ((?x3851 (sc_s 3)))
 (let ((?x6235 (bvadd (_ bv62 6) ?x3851)))
 (let (($x11846 (bvsle ?x6235 n)))
 (let ((?x11555 (stack_s x_0 x_1 w_3 3 n)))
 (let ((?x9633 (stack_s x_0 x_1 w_3 4 n)))
 (or (= ?x9633 ?x11555) $x11846)))))))
 ))
 (let ((?x3851 (sc_s 3)))
 (let (($x7570 (= ?x9433 ?x3851)))
 (let (($x3889 (= (used_gas_s x_0 x_1 w_3 4) (+ 3 (used_gas_s x_0 x_1 w_3 3)))))
 (let ((?x9078 (bvadd (_ bv63 6) ?x3851)))
 (let ((?x2956 (stack_s x_0 x_1 w_3 3 ?x9078)))
 (let ((?x6235 (bvadd (_ bv62 6) ?x3851)))
 (let ((?x5397 (stack_s x_0 x_1 w_3 3 ?x6235)))
 (let (($x8103 (exc_halt_s 3)))
 (let (($x10644 (= $x8103 (or (exc_halt_s 2) (not (bvsle (_ bv0 6) (bvadd (_ bv61 6) (sc_s 2))))))))
 (let (($x11355 (forall ((w (_ BitVec 256)) )(let ((?x8395 (storage_s x_0 x_1 w_3 2 w)))
 (let ((?x6354 (storage_s x_0 x_1 w_3 3 w)))
 (= ?x6354 ?x8395))))
 ))
 (let (($x9813 (forall ((n (_ BitVec 6)) )(let ((?x7711 (stack_s x_0 x_1 w_3 2 n)))
 (let ((?x11555 (stack_s x_0 x_1 w_3 3 n)))
 (or (bvsle (bvadd (_ bv61 6) (sc_s 2)) n) (= ?x11555 ?x7711)))))
 ))
 (let ((?x8320 (used_gas_s x_0 x_1 w_3 3)))
 (let ((?x2272 (sc_s 2)))
 (let ((?x3887 (bvadd (_ bv63 6) ?x2272)))
 (let ((?x8596 (stack_s x_0 x_1 w_3 2 ?x3887)))
 (let (($x10052 (exc_halt_s 2)))
 (let (($x6078 (= $x10052 (or (exc_halt_s 1) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 1))))))))
 (let (($x10602 (forall ((w (_ BitVec 256)) )(let ((?x9025 (storage_s x_0 x_1 w_3 1 w)))
 (let ((?x8395 (storage_s x_0 x_1 w_3 2 w)))
 (= ?x8395 ?x9025))))
 ))
 (let (($x3972 (forall ((n (_ BitVec 6)) )(let ((?x7681 (stack_s x_0 x_1 w_3 1 n)))
 (let ((?x7711 (stack_s x_0 x_1 w_3 2 n)))
 (or (bvsle (bvadd (_ bv62 6) (sc_s 1)) n) (= ?x7711 ?x7681)))))
 ))
 (let ((?x154 (sc_s 1)))
 (let (($x6737 (= ?x2272 ?x154)))
 (let ((?x3747 (used_gas_s x_0 x_1 w_3 2)))
 (let ((?x4643 (stack_s x_0 x_1 w_3 2 (bvadd (_ bv62 6) ?x2272))))
 (let (($x8780 (exc_halt_s 1)))
 (let (($x11279 (= $x8780 (or $x57 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x72))) (_ bv0 1)))))))
 (let (($x1568 (forall ((w (_ BitVec 256)) )(let ((?x6588 (storage_s x_0 x_1 w_3 0 w)))
 (let ((?x9025 (storage_s x_0 x_1 w_3 1 w)))
 (= ?x9025 ?x6588))))
 ))
 (let (($x5194 (forall ((n (_ BitVec 6)) )(let ((?x72 (sc_s 0)))
 (let (($x5715 (bvsle ?x72 n)))
 (let ((?x2975 (stack_s x_0 x_1 w_3 0 n)))
 (let ((?x7681 (stack_s x_0 x_1 w_3 1 n)))
 (or (= ?x7681 ?x2975) $x5715))))))
 ))
 (let (($x11116 (= ?x154 (bvadd (_ bv1 6) ?x72))))
 (let (($x6392 (forall ((w (_ BitVec 256)) )(let ((?x6588 (storage_s x_0 x_1 w_3 0 w)))
 (= ?x6588 (_ bv0 256))))
 ))
 (let (($x4157 (= ?x4334 0)))
 (let (($x2113 (not $x57)))
 (let (($x7902 (= (stack_s x_0 x_1 w_3 0 (_ bv1 6)) x_1)))
 (let (($x6396 (= (stack_s x_0 x_1 w_3 0 (_ bv0 6)) x_0)))
 (let (($x5626 (= ?x72 (_ bv2 6))))
 (and $x5626 $x6396 $x7902 $x2113 $x4157 $x6392 (= (stack_s x_0 x_1 w_3 1 ?x72) w_3) (= (used_gas_s x_0 x_1 w_3 1) (+ 3 ?x4334)) $x11116 $x5194 $x1568 $x11279 (= ?x8596 (stack_s x_0 x_1 w_3 1 (bvadd (_ bv62 6) ?x154))) (= ?x4643 (stack_s x_0 x_1 w_3 1 (bvadd (_ bv63 6) ?x154))) (= ?x3747 (+ 3 (used_gas_s x_0 x_1 w_3 1))) $x6737 $x3972 $x10602 $x6078 (= ?x2956 (stack_s x_0 x_1 w_3 2 (bvadd (_ bv61 6) ?x2272))) (= (stack_s x_0 x_1 w_3 3 (bvadd (_ bv61 6) ?x3851)) ?x8596) (= ?x5397 ?x4643) (= ?x8320 (+ 3 ?x3747)) (= ?x3851 ?x2272) $x9813 $x11355 $x10644 (= (stack_s x_0 x_1 w_3 4 (bvadd (_ bv63 6) ?x9433)) ?x5397) (= (stack_s x_0 x_1 w_3 4 (bvadd (_ bv62 6) ?x9433)) ?x2956) $x3889 $x7570 $x2055 $x6006 $x7924 $x5189 $x5135 (= (used_gas_t x_0 x_1 w_3 1) (+ 3 ?x6829)) (= (sc_t 1) ?x63) $x10768 $x3233 (= $x11030 (or $x56 (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) ?x63))))) (= (stack_t x_0 x_1 w_3 2 (sc_t 1)) w_3) $x4905 $x9818 $x9798 $x10330 (= $x6842 (or $x11030 $x8403)) $x73 $x9929 $x58 $x10165 $x5740 (not (and $x2152 $x7270 $x4445 $x1633))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
 )
(check-sat)
(get-proof)
