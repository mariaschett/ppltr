; SWAP2 SWAP1 SWAP2 SWAP1 => SWAP1 SWAP2
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
(assert
 (exists ((x_0 (_ BitVec 256)) (x_1 (_ BitVec 256)) (x_2 (_ BitVec 256)) )(let (($x2270 (forall ((w (_ BitVec 256)) )(let ((?x8825 (storage_t x_0 x_1 x_2 2 w)))
 (let ((?x9244 (storage_s x_0 x_1 x_2 4 w)))
 (= ?x9244 ?x8825))))
 ))
 (let (($x10055 (exc_halt_t 2)))
 (let (($x7172 (exc_halt_s 4)))
 (let (($x10478 (= $x7172 $x10055)))
 (let (($x6408 (forall ((n (_ BitVec 6)) )(let ((?x2714 (sc_t 2)))
 (let (($x7645 (bvsle ?x2714 n)))
 (let ((?x3204 (stack_t x_0 x_1 x_2 2 n)))
 (let ((?x3067 (stack_s x_0 x_1 x_2 4 n)))
 (let (($x544 (= ?x3067 ?x3204)))
 (or $x544 $x7645)))))))
 ))
 (let ((?x2714 (sc_t 2)))
 (let ((?x4305 (sc_s 4)))
 (let (($x11133 (= ?x4305 ?x2714)))
 (let ((?x11719 (used_gas_t x_0 x_1 x_2 0)))
 (let ((?x7240 (used_gas_s x_0 x_1 x_2 0)))
 (let (($x5606 (= ?x7240 ?x11719)))
 (let (($x10987 (forall ((w (_ BitVec 256)) )(let ((?x7497 (storage_t x_0 x_1 x_2 0 w)))
 (let ((?x6937 (storage_s x_0 x_1 x_2 0 w)))
 (= ?x6937 ?x7497))))
 ))
 (let (($x56 (exc_halt_t 0)))
 (let (($x57 (exc_halt_s 0)))
 (let (($x58 (= $x57 $x56)))
 (let (($x4198 (forall ((n (_ BitVec 6)) )(let ((?x5893 (stack_t x_0 x_1 x_2 0 n)))
 (let ((?x1594 (stack_s x_0 x_1 x_2 0 n)))
 (let (($x530 (= ?x1594 ?x5893)))
 (let ((?x63 (sc_t 0)))
 (let (($x368 (bvsle ?x63 n)))
 (or $x368 $x530)))))))
 ))
 (let ((?x63 (sc_t 0)))
 (let ((?x72 (sc_s 0)))
 (let (($x73 (= ?x72 ?x63)))
 (let (($x11449 (= $x10055 (or (exc_halt_t 1) (not (bvsle (_ bv0 6) (bvadd (_ bv61 6) (sc_t 1))))))))
 (let (($x11371 (forall ((w (_ BitVec 256)) )(let ((?x2892 (storage_t x_0 x_1 x_2 1 w)))
 (let ((?x8825 (storage_t x_0 x_1 x_2 2 w)))
 (= ?x8825 ?x2892))))
 ))
 (let (($x4443 (forall ((n (_ BitVec 6)) )(let ((?x7330 (stack_t x_0 x_1 x_2 1 n)))
 (let ((?x3204 (stack_t x_0 x_1 x_2 2 n)))
 (or (= ?x3204 ?x7330) (bvsle (bvadd (_ bv61 6) (sc_t 1)) n)))))
 ))
 (let ((?x9948 (used_gas_t x_0 x_1 x_2 2)))
 (let (($x977 (= ?x9948 (+ 3 (used_gas_t x_0 x_1 x_2 1)))))
 (let ((?x8347 (sc_t 1)))
 (let ((?x732 (bvadd (_ bv62 6) ?x8347)))
 (let ((?x7919 (stack_t x_0 x_1 x_2 1 ?x732)))
 (let ((?x718 (bvadd (_ bv62 6) ?x2714)))
 (let ((?x3619 (stack_t x_0 x_1 x_2 2 ?x718)))
 (let ((?x4465 (bvadd (_ bv63 6) ?x8347)))
 (let ((?x5109 (stack_t x_0 x_1 x_2 1 ?x4465)))
 (let ((?x5805 (bvadd (_ bv61 6) ?x2714)))
 (let ((?x10736 (stack_t x_0 x_1 x_2 2 ?x5805)))
 (let ((?x4736 (bvadd (_ bv61 6) ?x8347)))
 (let ((?x2428 (stack_t x_0 x_1 x_2 1 ?x4736)))
 (let ((?x10249 (bvadd (_ bv63 6) ?x2714)))
 (let ((?x7212 (stack_t x_0 x_1 x_2 2 ?x10249)))
 (let (($x2907 (= ?x7212 ?x2428)))
 (let (($x1318 (forall ((w (_ BitVec 256)) )(let ((?x7497 (storage_t x_0 x_1 x_2 0 w)))
 (let ((?x2892 (storage_t x_0 x_1 x_2 1 w)))
 (= ?x2892 ?x7497))))
 ))
 (let (($x3246 (forall ((n (_ BitVec 6)) )(let ((?x63 (sc_t 0)))
 (let ((?x3392 (bvadd (_ bv62 6) ?x63)))
 (let (($x4446 (bvsle ?x3392 n)))
 (let ((?x5893 (stack_t x_0 x_1 x_2 0 n)))
 (let ((?x7330 (stack_t x_0 x_1 x_2 1 n)))
 (let (($x3353 (= ?x7330 ?x5893)))
 (or $x3353 $x4446))))))))
 ))
 (let (($x6866 (= $x7172 (or (exc_halt_s 3) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 3))))))))
 (let (($x8765 (forall ((w (_ BitVec 256)) )(let ((?x4575 (storage_s x_0 x_1 x_2 3 w)))
 (let ((?x9244 (storage_s x_0 x_1 x_2 4 w)))
 (= ?x9244 ?x4575))))
 ))
 (let (($x3337 (forall ((n (_ BitVec 6)) )(let ((?x1885 (stack_s x_0 x_1 x_2 3 n)))
 (let ((?x3067 (stack_s x_0 x_1 x_2 4 n)))
 (let ((?x275 (sc_s 3)))
 (let ((?x8880 (bvadd (_ bv62 6) ?x275)))
 (let (($x6621 (bvsle ?x8880 n)))
 (or $x6621 (= ?x3067 ?x1885))))))))
 ))
 (let ((?x275 (sc_s 3)))
 (let (($x2450 (= ?x4305 ?x275)))
 (let (($x4100 (= (used_gas_s x_0 x_1 x_2 4) (+ 3 (used_gas_s x_0 x_1 x_2 3)))))
 (let ((?x11810 (bvadd (_ bv63 6) ?x275)))
 (let ((?x10486 (stack_s x_0 x_1 x_2 3 ?x11810)))
 (let (($x165 (= (stack_s x_0 x_1 x_2 4 (bvadd (_ bv62 6) ?x4305)) ?x10486)))
 (let ((?x8880 (bvadd (_ bv62 6) ?x275)))
 (let ((?x11412 (stack_s x_0 x_1 x_2 3 ?x8880)))
 (let (($x3952 (= (stack_s x_0 x_1 x_2 4 (bvadd (_ bv63 6) ?x4305)) ?x11412)))
 (let (($x292 (exc_halt_s 3)))
 (let (($x3706 (= $x292 (or (exc_halt_s 2) (not (bvsle (_ bv0 6) (bvadd (_ bv61 6) (sc_s 2))))))))
 (let (($x6260 (forall ((w (_ BitVec 256)) )(let ((?x10004 (storage_s x_0 x_1 x_2 2 w)))
 (let ((?x4575 (storage_s x_0 x_1 x_2 3 w)))
 (= ?x4575 ?x10004))))
 ))
 (let (($x10908 (forall ((n (_ BitVec 6)) )(let ((?x1413 (stack_s x_0 x_1 x_2 2 n)))
 (let ((?x1885 (stack_s x_0 x_1 x_2 3 n)))
 (let (($x6202 (= ?x1885 ?x1413)))
 (or (bvsle (bvadd (_ bv61 6) (sc_s 2)) n) $x6202)))))
 ))
 (let ((?x218 (sc_s 2)))
 (let (($x5288 (= ?x275 ?x218)))
 (let ((?x3988 (used_gas_s x_0 x_1 x_2 3)))
 (let ((?x6729 (bvadd (_ bv63 6) ?x218)))
 (let ((?x2597 (stack_s x_0 x_1 x_2 2 ?x6729)))
 (let (($x247 (exc_halt_s 2)))
 (let (($x7436 (= $x247 (or (exc_halt_s 1) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 1))))))))
 (let (($x3106 (forall ((w (_ BitVec 256)) )(let ((?x9764 (storage_s x_0 x_1 x_2 1 w)))
 (let ((?x10004 (storage_s x_0 x_1 x_2 2 w)))
 (= ?x10004 ?x9764))))
 ))
 (let (($x3743 (forall ((n (_ BitVec 6)) )(let ((?x154 (sc_s 1)))
 (let ((?x6807 (bvadd (_ bv62 6) ?x154)))
 (let (($x11512 (bvsle ?x6807 n)))
 (let ((?x5710 (stack_s x_0 x_1 x_2 1 n)))
 (let ((?x1413 (stack_s x_0 x_1 x_2 2 n)))
 (or (= ?x1413 ?x5710) $x11512)))))))
 ))
 (let ((?x154 (sc_s 1)))
 (let (($x8577 (= ?x218 ?x154)))
 (let ((?x1145 (used_gas_s x_0 x_1 x_2 2)))
 (let (($x4072 (= ?x1145 (+ 3 (used_gas_s x_0 x_1 x_2 1)))))
 (let ((?x7935 (bvadd (_ bv63 6) ?x154)))
 (let ((?x3296 (stack_s x_0 x_1 x_2 1 ?x7935)))
 (let ((?x7822 (bvadd (_ bv62 6) ?x218)))
 (let ((?x5350 (stack_s x_0 x_1 x_2 2 ?x7822)))
 (let (($x11458 (= ?x5350 ?x3296)))
 (let ((?x6807 (bvadd (_ bv62 6) ?x154)))
 (let ((?x9793 (stack_s x_0 x_1 x_2 1 ?x6807)))
 (let (($x9269 (= ?x2597 ?x9793)))
 (let (($x6187 (forall ((w (_ BitVec 256)) )(let ((?x6937 (storage_s x_0 x_1 x_2 0 w)))
 (let ((?x9764 (storage_s x_0 x_1 x_2 1 w)))
 (= ?x9764 ?x6937))))
 ))
 (let (($x11205 (forall ((n (_ BitVec 6)) )(let ((?x1594 (stack_s x_0 x_1 x_2 0 n)))
 (let ((?x5710 (stack_s x_0 x_1 x_2 1 n)))
 (let (($x1630 (= ?x5710 ?x1594)))
 (or $x1630 (bvsle (bvadd (_ bv61 6) (sc_s 0)) n))))))
 ))
 (let ((?x10571 (used_gas_s x_0 x_1 x_2 1)))
 (let (($x9020 (= ?x10571 (+ 3 ?x7240))))
 (let ((?x9903 (bvadd (_ bv63 6) ?x72)))
 (let ((?x4240 (stack_s x_0 x_1 x_2 0 ?x9903)))
 (let (($x76 (forall ((w (_ BitVec 256)) )(let ((?x6937 (storage_s x_0 x_1 x_2 0 w)))
 (= ?x6937 (_ bv0 256))))
 ))
 (let (($x4921 (= ?x7240 0)))
 (let (($x4102 (not $x57)))
 (let (($x3658 (= (stack_s x_0 x_1 x_2 0 (_ bv2 6)) x_2)))
 (let (($x6604 (= (stack_s x_0 x_1 x_2 0 (_ bv1 6)) x_1)))
 (let (($x489 (= (stack_s x_0 x_1 x_2 0 (_ bv0 6)) x_0)))
 (let (($x5315 (= ?x72 (_ bv3 6))))
 (and $x5315 $x489 $x6604 $x3658 $x4102 $x4921 $x76 (= ?x3296 (stack_s x_0 x_1 x_2 0 (bvadd (_ bv61 6) ?x72))) (= (stack_s x_0 x_1 x_2 1 (bvadd (_ bv61 6) ?x154)) ?x4240) (= ?x9793 (stack_s x_0 x_1 x_2 0 (bvadd (_ bv62 6) ?x72))) $x9020 (= ?x154 ?x72) $x11205 $x6187 (= (exc_halt_s 1) (or $x57 (not (bvsle (_ bv0 6) (bvadd (_ bv61 6) ?x72))))) $x9269 $x11458 $x4072 $x8577 $x3743 $x3106 $x7436 (= ?x10486 (stack_s x_0 x_1 x_2 2 (bvadd (_ bv61 6) ?x218))) (= (stack_s x_0 x_1 x_2 3 (bvadd (_ bv61 6) ?x275)) ?x2597) (= ?x11412 ?x5350) (= ?x3988 (+ 3 ?x1145)) $x5288 $x10908 $x6260 $x3706 $x3952 $x165 $x4100 $x2450 $x3337 $x8765 $x6866 (= ?x5109 (stack_t x_0 x_1 x_2 0 (bvadd (_ bv62 6) ?x63))) (= ?x7919 (stack_t x_0 x_1 x_2 0 (bvadd (_ bv63 6) ?x63))) (= (used_gas_t x_0 x_1 x_2 1) (+ 3 ?x11719)) (= ?x8347 ?x63) $x3246 $x1318 (= (exc_halt_t 1) (or $x56 (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) ?x63))))) $x2907 (= ?x10736 ?x5109) (= ?x3619 ?x7919) $x977 (= ?x2714 ?x8347) $x4443 $x11371 $x11449 $x73 $x4198 $x58 $x10987 $x5606 (not (and $x11133 $x6408 $x10478 $x2270))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
 )
(check-sat)
(get-proof)