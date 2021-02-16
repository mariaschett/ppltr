; CALLDATASIZE SWAP1 POP LT ISZERO ISZERO => POP CALLDATASIZE LT
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
 (exists ((x_0 (_ BitVec 256)) (x_1 (_ BitVec 256)) (x_CALLDATASIZE (_ BitVec 256)) )(let (($x717 (forall ((w (_ BitVec 256)) )(let ((?x8751 (storage_t x_0 x_1 x_CALLDATASIZE 3 w)))
 (let ((?x5484 (storage_s x_0 x_1 x_CALLDATASIZE 6 w)))
 (= ?x5484 ?x8751))))
 ))
 (let (($x6783 (exc_halt_t 3)))
 (let (($x772 (exc_halt_s 6)))
 (let (($x8012 (= $x772 $x6783)))
 (let (($x2810 (forall ((n (_ BitVec 6)) )(let ((?x6438 (sc_t 3)))
 (let (($x10295 (bvsle ?x6438 n)))
 (let ((?x10780 (stack_t x_0 x_1 x_CALLDATASIZE 3 n)))
 (let ((?x4232 (stack_s x_0 x_1 x_CALLDATASIZE 6 n)))
 (let (($x9670 (= ?x4232 ?x10780)))
 (or $x9670 $x10295)))))))
 ))
 (let ((?x6438 (sc_t 3)))
 (let ((?x926 (sc_s 6)))
 (let (($x2793 (= ?x926 ?x6438)))
 (let ((?x1928 (used_gas_t x_0 x_1 x_CALLDATASIZE 0)))
 (let ((?x9075 (used_gas_s x_0 x_1 x_CALLDATASIZE 0)))
 (let (($x577 (= ?x9075 ?x1928)))
 (let (($x8840 (forall ((w (_ BitVec 256)) )(let ((?x5825 (storage_t x_0 x_1 x_CALLDATASIZE 0 w)))
 (let ((?x10407 (storage_s x_0 x_1 x_CALLDATASIZE 0 w)))
 (= ?x10407 ?x5825))))
 ))
 (let (($x56 (exc_halt_t 0)))
 (let (($x57 (exc_halt_s 0)))
 (let (($x58 (= $x57 $x56)))
 (let (($x9058 (forall ((n (_ BitVec 6)) )(let ((?x63 (sc_t 0)))
 (let (($x4813 (bvsle ?x63 n)))
 (let ((?x830 (stack_t x_0 x_1 x_CALLDATASIZE 0 n)))
 (let ((?x5305 (stack_s x_0 x_1 x_CALLDATASIZE 0 n)))
 (let (($x1972 (= ?x5305 ?x830)))
 (or $x1972 $x4813)))))))
 ))
 (let ((?x63 (sc_t 0)))
 (let ((?x72 (sc_s 0)))
 (let (($x73 (= ?x72 ?x63)))
 (let (($x4237 (= $x6783 (or (exc_halt_t 2) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_t 2))))))))
 (let (($x3380 (forall ((w (_ BitVec 256)) )(let ((?x9104 (storage_t x_0 x_1 x_CALLDATASIZE 2 w)))
 (let ((?x8751 (storage_t x_0 x_1 x_CALLDATASIZE 3 w)))
 (= ?x8751 ?x9104))))
 ))
 (let (($x4329 (forall ((n (_ BitVec 6)) )(let ((?x3054 (stack_t x_0 x_1 x_CALLDATASIZE 2 n)))
 (let ((?x10780 (stack_t x_0 x_1 x_CALLDATASIZE 3 n)))
 (let (($x4784 (= ?x10780 ?x3054)))
 (or $x4784 (bvsle (bvadd (_ bv62 6) (sc_t 2)) n))))))
 ))
 (let ((?x5750 (used_gas_t x_0 x_1 x_CALLDATASIZE 3)))
 (let (($x2883 (= ?x5750 (+ 3 (used_gas_t x_0 x_1 x_CALLDATASIZE 2)))))
 (let ((?x2714 (sc_t 2)))
 (let ((?x7128 (bvadd (_ bv63 6) ?x2714)))
 (let ((?x4858 (stack_t x_0 x_1 x_CALLDATASIZE 2 ?x7128)))
 (let ((?x2021 (bvadd (_ bv62 6) ?x2714)))
 (let ((?x9840 (stack_t x_0 x_1 x_CALLDATASIZE 2 ?x2021)))
 (let ((?x10811 (bvadd (_ bv63 6) ?x6438)))
 (let ((?x11271 (stack_t x_0 x_1 x_CALLDATASIZE 3 ?x10811)))
 (let (($x8583 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_t 1)))) (_ bv0 1)))))
 (let (($x8377 (exc_halt_t 1)))
 (let (($x2163 (exc_halt_t 2)))
 (let (($x7597 (= $x2163 (or $x8377 $x8583))))
 (let (($x1925 (forall ((w (_ BitVec 256)) )(let ((?x10092 (storage_t x_0 x_1 x_CALLDATASIZE 1 w)))
 (let ((?x9104 (storage_t x_0 x_1 x_CALLDATASIZE 2 w)))
 (= ?x9104 ?x10092))))
 ))
 (let (($x10031 (forall ((n (_ BitVec 6)) )(let ((?x7154 (sc_t 1)))
 (let (($x6622 (bvsle ?x7154 n)))
 (let ((?x735 (stack_t x_0 x_1 x_CALLDATASIZE 1 n)))
 (let ((?x3054 (stack_t x_0 x_1 x_CALLDATASIZE 2 n)))
 (let (($x4893 (= ?x3054 ?x735)))
 (or $x4893 $x6622)))))))
 ))
 (let (($x8229 (= ?x2714 (bvadd (_ bv1 6) (sc_t 1)))))
 (let ((?x11278 (used_gas_t x_0 x_1 x_CALLDATASIZE 2)))
 (let (($x8324 (= $x8377 (or $x56 (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) ?x63)))))))
 (let (($x438 (forall ((w (_ BitVec 256)) )(let ((?x5825 (storage_t x_0 x_1 x_CALLDATASIZE 0 w)))
 (let ((?x10092 (storage_t x_0 x_1 x_CALLDATASIZE 1 w)))
 (= ?x10092 ?x5825))))
 ))
 (let (($x10068 (forall ((n (_ BitVec 6)) )(let ((?x830 (stack_t x_0 x_1 x_CALLDATASIZE 0 n)))
 (let ((?x735 (stack_t x_0 x_1 x_CALLDATASIZE 1 n)))
 (let (($x9825 (= ?x735 ?x830)))
 (let ((?x63 (sc_t 0)))
 (let ((?x7312 (bvadd (_ bv63 6) ?x63)))
 (let (($x8332 (bvsle ?x7312 n)))
 (or $x8332 $x9825))))))))
 ))
 (let ((?x7312 (bvadd (_ bv63 6) ?x63)))
 (let ((?x7154 (sc_t 1)))
 (let (($x10046 (= ?x7154 ?x7312)))
 (let (($x862 (= $x772 (or (exc_halt_s 5) (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_s 5))))))))
 (let (($x2644 (forall ((w (_ BitVec 256)) )(let ((?x3697 (storage_s x_0 x_1 x_CALLDATASIZE 5 w)))
 (let ((?x5484 (storage_s x_0 x_1 x_CALLDATASIZE 6 w)))
 (= ?x5484 ?x3697))))
 ))
 (let (($x541 (forall ((n (_ BitVec 6)) )(let ((?x4319 (sc_s 5)))
 (let ((?x4438 (bvadd (_ bv63 6) ?x4319)))
 (let (($x10430 (bvsle ?x4438 n)))
 (let ((?x11763 (stack_s x_0 x_1 x_CALLDATASIZE 5 n)))
 (let ((?x4232 (stack_s x_0 x_1 x_CALLDATASIZE 6 n)))
 (let (($x3783 (= ?x4232 ?x11763)))
 (or $x3783 $x10430))))))))
 ))
 (let ((?x4319 (sc_s 5)))
 (let (($x4730 (= ?x926 ?x4319)))
 (let (($x3841 (= (used_gas_s x_0 x_1 x_CALLDATASIZE 6) (+ 3 (used_gas_s x_0 x_1 x_CALLDATASIZE 5)))))
 (let ((?x4148 (ite (= (stack_s x_0 x_1 x_CALLDATASIZE 5 (bvadd (_ bv63 6) ?x4319)) (_ bv0 256)) (_ bv1 256) (_ bv0 256))))
 (let ((?x1637 (bvadd (_ bv63 6) ?x926)))
 (let ((?x2725 (stack_s x_0 x_1 x_CALLDATASIZE 6 ?x1637)))
 (let (($x11317 (exc_halt_s 5)))
 (let (($x8773 (= $x11317 (or (exc_halt_s 4) (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_s 4))))))))
 (let (($x6495 (forall ((w (_ BitVec 256)) )(let ((?x6440 (storage_s x_0 x_1 x_CALLDATASIZE 4 w)))
 (let ((?x3697 (storage_s x_0 x_1 x_CALLDATASIZE 5 w)))
 (= ?x3697 ?x6440))))
 ))
 (let (($x7518 (forall ((n (_ BitVec 6)) )(let ((?x9725 (stack_s x_0 x_1 x_CALLDATASIZE 4 n)))
 (let ((?x11763 (stack_s x_0 x_1 x_CALLDATASIZE 5 n)))
 (let (($x4312 (= ?x11763 ?x9725)))
 (or (bvsle (bvadd (_ bv63 6) (sc_s 4)) n) $x4312)))))
 ))
 (let ((?x4305 (sc_s 4)))
 (let (($x10520 (= ?x4319 ?x4305)))
 (let ((?x2654 (used_gas_s x_0 x_1 x_CALLDATASIZE 5)))
 (let (($x2937 (= ?x2654 (+ 3 (used_gas_s x_0 x_1 x_CALLDATASIZE 4)))))
 (let ((?x4897 (ite (= (stack_s x_0 x_1 x_CALLDATASIZE 4 (bvadd (_ bv63 6) ?x4305)) (_ bv0 256)) (_ bv1 256) (_ bv0 256))))
 (let ((?x4438 (bvadd (_ bv63 6) ?x4319)))
 (let ((?x10316 (stack_s x_0 x_1 x_CALLDATASIZE 5 ?x4438)))
 (let (($x7172 (exc_halt_s 4)))
 (let (($x4663 (= $x7172 (or (exc_halt_s 3) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 3))))))))
 (let (($x11171 (forall ((w (_ BitVec 256)) )(let ((?x10083 (storage_s x_0 x_1 x_CALLDATASIZE 3 w)))
 (let ((?x6440 (storage_s x_0 x_1 x_CALLDATASIZE 4 w)))
 (= ?x6440 ?x10083))))
 ))
 (let (($x9255 (forall ((n (_ BitVec 6)) )(let ((?x275 (sc_s 3)))
 (let ((?x7506 (bvadd (_ bv62 6) ?x275)))
 (let (($x4522 (bvsle ?x7506 n)))
 (let ((?x8422 (stack_s x_0 x_1 x_CALLDATASIZE 3 n)))
 (let ((?x9725 (stack_s x_0 x_1 x_CALLDATASIZE 4 n)))
 (or (= ?x9725 ?x8422) $x4522)))))))
 ))
 (let ((?x275 (sc_s 3)))
 (let ((?x10583 (bvadd (_ bv63 6) ?x275)))
 (let (($x5758 (= ?x4305 ?x10583)))
 (let ((?x4185 (used_gas_s x_0 x_1 x_CALLDATASIZE 4)))
 (let (($x10611 (= ?x4185 (+ 3 (used_gas_s x_0 x_1 x_CALLDATASIZE 3)))))
 (let ((?x11572 (stack_s x_0 x_1 x_CALLDATASIZE 3 ?x10583)))
 (let ((?x7506 (bvadd (_ bv62 6) ?x275)))
 (let ((?x6657 (stack_s x_0 x_1 x_CALLDATASIZE 3 ?x7506)))
 (let ((?x3271 (bvadd (_ bv63 6) ?x4305)))
 (let ((?x5698 (stack_s x_0 x_1 x_CALLDATASIZE 4 ?x3271)))
 (let (($x292 (exc_halt_s 3)))
 (let (($x8248 (= $x292 (or (exc_halt_s 2) (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_s 2))))))))
 (let (($x4717 (forall ((w (_ BitVec 256)) )(let ((?x5069 (storage_s x_0 x_1 x_CALLDATASIZE 2 w)))
 (let ((?x10083 (storage_s x_0 x_1 x_CALLDATASIZE 3 w)))
 (= ?x10083 ?x5069))))
 ))
 (let (($x6324 (forall ((n (_ BitVec 6)) )(let ((?x218 (sc_s 2)))
 (let ((?x616 (bvadd (_ bv63 6) ?x218)))
 (let (($x4640 (bvsle ?x616 n)))
 (let ((?x6980 (stack_s x_0 x_1 x_CALLDATASIZE 2 n)))
 (let ((?x8422 (stack_s x_0 x_1 x_CALLDATASIZE 3 n)))
 (let (($x1553 (= ?x8422 ?x6980)))
 (or $x1553 $x4640))))))))
 ))
 (let ((?x218 (sc_s 2)))
 (let ((?x616 (bvadd (_ bv63 6) ?x218)))
 (let (($x4670 (= ?x275 ?x616)))
 (let ((?x4758 (used_gas_s x_0 x_1 x_CALLDATASIZE 3)))
 (let (($x247 (exc_halt_s 2)))
 (let (($x9849 (= $x247 (or (exc_halt_s 1) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 1))))))))
 (let (($x5653 (forall ((w (_ BitVec 256)) )(let ((?x4309 (storage_s x_0 x_1 x_CALLDATASIZE 1 w)))
 (let ((?x5069 (storage_s x_0 x_1 x_CALLDATASIZE 2 w)))
 (= ?x5069 ?x4309))))
 ))
 (let (($x8677 (forall ((n (_ BitVec 6)) )(let ((?x1248 (stack_s x_0 x_1 x_CALLDATASIZE 1 n)))
 (let ((?x6980 (stack_s x_0 x_1 x_CALLDATASIZE 2 n)))
 (let ((?x154 (sc_s 1)))
 (let ((?x1237 (bvadd (_ bv62 6) ?x154)))
 (let (($x3989 (bvsle ?x1237 n)))
 (or $x3989 (= ?x6980 ?x1248))))))))
 ))
 (let ((?x154 (sc_s 1)))
 (let (($x10978 (= ?x218 ?x154)))
 (let ((?x4223 (used_gas_s x_0 x_1 x_CALLDATASIZE 2)))
 (let (($x8483 (= ?x4223 (+ 3 (used_gas_s x_0 x_1 x_CALLDATASIZE 1)))))
 (let ((?x4114 (bvadd (_ bv63 6) ?x154)))
 (let ((?x3778 (stack_s x_0 x_1 x_CALLDATASIZE 1 ?x4114)))
 (let ((?x3324 (bvadd (_ bv62 6) ?x218)))
 (let ((?x3174 (stack_s x_0 x_1 x_CALLDATASIZE 2 ?x3324)))
 (let (($x7373 (= ?x3174 ?x3778)))
 (let ((?x4250 (stack_s x_0 x_1 x_CALLDATASIZE 2 ?x616)))
 (let (($x4335 (= ?x4250 (stack_s x_0 x_1 x_CALLDATASIZE 1 (bvadd (_ bv62 6) ?x154)))))
 (let (($x189 (exc_halt_s 1)))
 (let (($x8008 (= $x189 (or $x57 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x72))) (_ bv0 1)))))))
 (let (($x4338 (forall ((w (_ BitVec 256)) )(let ((?x10407 (storage_s x_0 x_1 x_CALLDATASIZE 0 w)))
 (let ((?x4309 (storage_s x_0 x_1 x_CALLDATASIZE 1 w)))
 (= ?x4309 ?x10407))))
 ))
 (let (($x389 (forall ((n (_ BitVec 6)) )(let ((?x72 (sc_s 0)))
 (let (($x10187 (bvsle ?x72 n)))
 (let ((?x5305 (stack_s x_0 x_1 x_CALLDATASIZE 0 n)))
 (let ((?x1248 (stack_s x_0 x_1 x_CALLDATASIZE 1 n)))
 (let (($x3181 (= ?x1248 ?x5305)))
 (or $x3181 $x10187)))))))
 ))
 (let (($x2373 (= ?x154 (bvadd (_ bv1 6) ?x72))))
 (let (($x6694 (forall ((w (_ BitVec 256)) )(let ((?x10407 (storage_s x_0 x_1 x_CALLDATASIZE 0 w)))
 (= ?x10407 (_ bv0 256))))
 ))
 (let (($x10473 (= ?x9075 0)))
 (let (($x5107 (not $x57)))
 (let (($x2621 (= (stack_s x_0 x_1 x_CALLDATASIZE 0 (_ bv1 6)) x_1)))
 (let (($x4353 (= (stack_s x_0 x_1 x_CALLDATASIZE 0 (_ bv0 6)) x_0)))
 (let (($x11128 (= ?x72 (_ bv2 6))))
 (and $x11128 $x4353 $x2621 $x5107 $x10473 $x6694 (= (stack_s x_0 x_1 x_CALLDATASIZE 1 ?x72) x_CALLDATASIZE) (= (used_gas_s x_0 x_1 x_CALLDATASIZE 1) (+ 2 ?x9075)) $x2373 $x389 $x4338 $x8008 $x4335 $x7373 $x8483 $x10978 $x8677 $x5653 $x9849 (= ?x4758 (+ 2 ?x4223)) $x4670 $x6324 $x4717 $x8248 (= ?x5698 (ite (bvule ?x6657 ?x11572) (_ bv0 256) (_ bv1 256))) $x10611 $x5758 $x9255 $x11171 $x4663 (= ?x10316 ?x4897) $x2937 $x10520 $x7518 $x6495 $x8773 (= ?x2725 ?x4148) $x3841 $x4730 $x541 $x2644 $x862 (= (used_gas_t x_0 x_1 x_CALLDATASIZE 1) (+ 2 ?x1928)) $x10046 $x10068 $x438 $x8324 (= (stack_t x_0 x_1 x_CALLDATASIZE 2 ?x7154) x_CALLDATASIZE) (= ?x11278 (+ 2 (used_gas_t x_0 x_1 x_CALLDATASIZE 1))) $x8229 $x10031 $x1925 $x7597 (= ?x11271 (ite (bvule ?x9840 ?x4858) (_ bv0 256) (_ bv1 256))) $x2883 (= ?x6438 ?x7128) $x4329 $x3380 $x4237 $x73 $x9058 $x58 $x8840 $x577 (not (and $x2793 $x2810 $x8012 $x717)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
 )
(check-sat)
(get-proof)