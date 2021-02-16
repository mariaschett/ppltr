; SWAP3 SWAP1 SWAP3 OR => SWAP1 SWAP3 OR
(set-option :produce-proofs true)
; 
(set-info :status unknown)
(declare-fun storage_t ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 256)) (_ BitVec 256))
(declare-fun storage_s ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 256)) (_ BitVec 256))
(declare-fun exc_halt_t (Int) Bool)
(declare-fun exc_halt_s (Int) Bool)
(declare-fun stack_t ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 6)) (_ BitVec 256))
(declare-fun stack_s ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 6)) (_ BitVec 256))
(declare-fun sc_t (Int) (_ BitVec 6))
(declare-fun sc_s (Int) (_ BitVec 6))
(declare-fun used_gas_t ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int) Int)
(declare-fun used_gas_s ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int) Int)
(assert
 (exists ((x_0 (_ BitVec 256)) (x_1 (_ BitVec 256)) (x_2 (_ BitVec 256)) (x_3 (_ BitVec 256)) )(let (($x6121 (forall ((w (_ BitVec 256)) )(let ((?x9888 (storage_t x_0 x_1 x_2 x_3 3 w)))
 (let ((?x11130 (storage_s x_0 x_1 x_2 x_3 4 w)))
 (= ?x11130 ?x9888))))
 ))
 (let (($x6783 (exc_halt_t 3)))
 (let (($x7172 (exc_halt_s 4)))
 (let (($x6006 (= $x7172 $x6783)))
 (let (($x1248 (forall ((n (_ BitVec 6)) )(let ((?x1900 (stack_t x_0 x_1 x_2 x_3 3 n)))
 (let ((?x2932 (stack_s x_0 x_1 x_2 x_3 4 n)))
 (let (($x9238 (= ?x2932 ?x1900)))
 (let ((?x6438 (sc_t 3)))
 (let (($x1180 (bvsle ?x6438 n)))
 (or $x1180 $x9238)))))))
 ))
 (let ((?x6438 (sc_t 3)))
 (let ((?x4305 (sc_s 4)))
 (let (($x3162 (= ?x4305 ?x6438)))
 (let ((?x6145 (used_gas_t x_0 x_1 x_2 x_3 0)))
 (let ((?x9555 (used_gas_s x_0 x_1 x_2 x_3 0)))
 (let (($x2454 (= ?x9555 ?x6145)))
 (let (($x6726 (forall ((w (_ BitVec 256)) )(let ((?x2787 (storage_t x_0 x_1 x_2 x_3 0 w)))
 (let ((?x6040 (storage_s x_0 x_1 x_2 x_3 0 w)))
 (= ?x6040 ?x2787))))
 ))
 (let (($x56 (exc_halt_t 0)))
 (let (($x57 (exc_halt_s 0)))
 (let (($x58 (= $x57 $x56)))
 (let (($x6965 (forall ((n (_ BitVec 6)) )(let ((?x63 (sc_t 0)))
 (let (($x2855 (bvsle ?x63 n)))
 (let ((?x9656 (stack_t x_0 x_1 x_2 x_3 0 n)))
 (let ((?x5808 (stack_s x_0 x_1 x_2 x_3 0 n)))
 (let (($x2498 (= ?x5808 ?x9656)))
 (or $x2498 $x2855)))))))
 ))
 (let ((?x63 (sc_t 0)))
 (let ((?x72 (sc_s 0)))
 (let (($x73 (= ?x72 ?x63)))
 (let (($x503 (= $x6783 (or (exc_halt_t 2) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_t 2))))))))
 (let (($x2423 (forall ((w (_ BitVec 256)) )(let ((?x6173 (storage_t x_0 x_1 x_2 x_3 2 w)))
 (let ((?x9888 (storage_t x_0 x_1 x_2 x_3 3 w)))
 (= ?x9888 ?x6173))))
 ))
 (let (($x6559 (forall ((n (_ BitVec 6)) )(let ((?x3451 (stack_t x_0 x_1 x_2 x_3 2 n)))
 (let ((?x1900 (stack_t x_0 x_1 x_2 x_3 3 n)))
 (let ((?x2714 (sc_t 2)))
 (let ((?x3500 (bvadd (_ bv62 6) ?x2714)))
 (let (($x9241 (bvsle ?x3500 n)))
 (or $x9241 (= ?x1900 ?x3451))))))))
 ))
 (let (($x5058 (= (used_gas_t x_0 x_1 x_2 x_3 3) (+ 3 (used_gas_t x_0 x_1 x_2 x_3 2)))))
 (let ((?x2714 (sc_t 2)))
 (let ((?x3500 (bvadd (_ bv62 6) ?x2714)))
 (let ((?x8552 (stack_t x_0 x_1 x_2 x_3 2 ?x3500)))
 (let ((?x5618 (bvadd (_ bv63 6) ?x2714)))
 (let ((?x9933 (stack_t x_0 x_1 x_2 x_3 2 ?x5618)))
 (let (($x2680 (= (stack_t x_0 x_1 x_2 x_3 3 (bvadd (_ bv63 6) ?x6438)) (bvor ?x9933 ?x8552))))
 (let (($x2163 (exc_halt_t 2)))
 (let (($x2826 (= $x2163 (or (exc_halt_t 1) (not (bvsle (_ bv0 6) (bvadd (_ bv60 6) (sc_t 1))))))))
 (let (($x7319 (forall ((w (_ BitVec 256)) )(let ((?x3008 (storage_t x_0 x_1 x_2 x_3 1 w)))
 (let ((?x6173 (storage_t x_0 x_1 x_2 x_3 2 w)))
 (= ?x6173 ?x3008))))
 ))
 (let (($x8698 (forall ((n (_ BitVec 6)) )(let ((?x9857 (stack_t x_0 x_1 x_2 x_3 1 n)))
 (let ((?x3451 (stack_t x_0 x_1 x_2 x_3 2 n)))
 (or (bvsle (bvadd (_ bv60 6) (sc_t 1)) n) (= ?x3451 ?x9857)))))
 ))
 (let ((?x7154 (sc_t 1)))
 (let (($x6225 (= ?x2714 ?x7154)))
 (let ((?x990 (used_gas_t x_0 x_1 x_2 x_3 2)))
 (let (($x8988 (= (stack_t x_0 x_1 x_2 x_3 2 (bvadd (_ bv61 6) ?x2714)) (stack_t x_0 x_1 x_2 x_3 1 (bvadd (_ bv61 6) ?x7154)))))
 (let ((?x2239 (bvadd (_ bv63 6) ?x7154)))
 (let ((?x3404 (stack_t x_0 x_1 x_2 x_3 1 ?x2239)))
 (let (($x9319 (forall ((w (_ BitVec 256)) )(let ((?x2787 (storage_t x_0 x_1 x_2 x_3 0 w)))
 (let ((?x3008 (storage_t x_0 x_1 x_2 x_3 1 w)))
 (= ?x3008 ?x2787))))
 ))
 (let (($x2882 (forall ((n (_ BitVec 6)) )(let ((?x63 (sc_t 0)))
 (let ((?x3346 (bvadd (_ bv62 6) ?x63)))
 (let (($x5704 (bvsle ?x3346 n)))
 (let ((?x9656 (stack_t x_0 x_1 x_2 x_3 0 n)))
 (let ((?x9857 (stack_t x_0 x_1 x_2 x_3 1 n)))
 (or (= ?x9857 ?x9656) $x5704)))))))
 ))
 (let (($x6232 (= ?x7154 ?x63)))
 (let ((?x5273 (bvadd (_ bv62 6) ?x7154)))
 (let ((?x8626 (stack_t x_0 x_1 x_2 x_3 1 ?x5273)))
 (let (($x2571 (= $x7172 (or (exc_halt_s 3) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 3))))))))
 (let (($x985 (forall ((w (_ BitVec 256)) )(let ((?x6751 (storage_s x_0 x_1 x_2 x_3 3 w)))
 (let ((?x11130 (storage_s x_0 x_1 x_2 x_3 4 w)))
 (= ?x11130 ?x6751))))
 ))
 (let (($x8692 (forall ((n (_ BitVec 6)) )(let ((?x275 (sc_s 3)))
 (let ((?x1134 (bvadd (_ bv62 6) ?x275)))
 (let (($x7946 (bvsle ?x1134 n)))
 (let ((?x1367 (stack_s x_0 x_1 x_2 x_3 3 n)))
 (let ((?x2932 (stack_s x_0 x_1 x_2 x_3 4 n)))
 (or (= ?x2932 ?x1367) $x7946)))))))
 ))
 (let ((?x275 (sc_s 3)))
 (let ((?x9635 (bvadd (_ bv63 6) ?x275)))
 (let (($x6285 (= ?x4305 ?x9635)))
 (let (($x4946 (= (used_gas_s x_0 x_1 x_2 x_3 4) (+ 3 (used_gas_s x_0 x_1 x_2 x_3 3)))))
 (let ((?x1134 (bvadd (_ bv62 6) ?x275)))
 (let ((?x7575 (stack_s x_0 x_1 x_2 x_3 3 ?x1134)))
 (let ((?x5101 (stack_s x_0 x_1 x_2 x_3 3 ?x9635)))
 (let (($x7094 (= (stack_s x_0 x_1 x_2 x_3 4 (bvadd (_ bv63 6) ?x4305)) (bvor ?x5101 ?x7575))))
 (let (($x292 (exc_halt_s 3)))
 (let (($x5473 (= $x292 (or (exc_halt_s 2) (not (bvsle (_ bv0 6) (bvadd (_ bv60 6) (sc_s 2))))))))
 (let (($x2328 (forall ((w (_ BitVec 256)) )(let ((?x1268 (storage_s x_0 x_1 x_2 x_3 2 w)))
 (let ((?x6751 (storage_s x_0 x_1 x_2 x_3 3 w)))
 (= ?x6751 ?x1268))))
 ))
 (let (($x10458 (forall ((n (_ BitVec 6)) )(let ((?x6076 (stack_s x_0 x_1 x_2 x_3 2 n)))
 (let ((?x1367 (stack_s x_0 x_1 x_2 x_3 3 n)))
 (or (= ?x1367 ?x6076) (bvsle (bvadd (_ bv60 6) (sc_s 2)) n)))))
 ))
 (let ((?x218 (sc_s 2)))
 (let (($x6815 (= ?x275 ?x218)))
 (let ((?x6209 (used_gas_s x_0 x_1 x_2 x_3 3)))
 (let (($x2148 (= (stack_s x_0 x_1 x_2 x_3 3 (bvadd (_ bv61 6) ?x275)) (stack_s x_0 x_1 x_2 x_3 2 (bvadd (_ bv61 6) ?x218)))))
 (let ((?x7113 (bvadd (_ bv63 6) ?x218)))
 (let ((?x6085 (stack_s x_0 x_1 x_2 x_3 2 ?x7113)))
 (let (($x247 (exc_halt_s 2)))
 (let (($x7098 (= $x247 (or (exc_halt_s 1) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 1))))))))
 (let (($x8513 (forall ((w (_ BitVec 256)) )(let ((?x8136 (storage_s x_0 x_1 x_2 x_3 1 w)))
 (let ((?x1268 (storage_s x_0 x_1 x_2 x_3 2 w)))
 (= ?x1268 ?x8136))))
 ))
 (let (($x3846 (forall ((n (_ BitVec 6)) )(let ((?x154 (sc_s 1)))
 (let ((?x5676 (bvadd (_ bv62 6) ?x154)))
 (let (($x8928 (bvsle ?x5676 n)))
 (let ((?x8786 (stack_s x_0 x_1 x_2 x_3 1 n)))
 (let ((?x6076 (stack_s x_0 x_1 x_2 x_3 2 n)))
 (or (= ?x6076 ?x8786) $x8928)))))))
 ))
 (let ((?x154 (sc_s 1)))
 (let (($x8033 (= ?x218 ?x154)))
 (let ((?x6966 (used_gas_s x_0 x_1 x_2 x_3 2)))
 (let ((?x11673 (bvadd (_ bv63 6) ?x154)))
 (let ((?x10506 (stack_s x_0 x_1 x_2 x_3 1 ?x11673)))
 (let ((?x2577 (bvadd (_ bv62 6) ?x218)))
 (let ((?x10755 (stack_s x_0 x_1 x_2 x_3 2 ?x2577)))
 (let (($x804 (forall ((w (_ BitVec 256)) )(let ((?x6040 (storage_s x_0 x_1 x_2 x_3 0 w)))
 (let ((?x8136 (storage_s x_0 x_1 x_2 x_3 1 w)))
 (= ?x8136 ?x6040))))
 ))
 (let (($x10017 (forall ((n (_ BitVec 6)) )(let ((?x5808 (stack_s x_0 x_1 x_2 x_3 0 n)))
 (let ((?x8786 (stack_s x_0 x_1 x_2 x_3 1 n)))
 (or (bvsle (bvadd (_ bv60 6) (sc_s 0)) n) (= ?x8786 ?x5808)))))
 ))
 (let (($x2802 (= ?x154 ?x72)))
 (let ((?x5676 (bvadd (_ bv62 6) ?x154)))
 (let ((?x4861 (stack_s x_0 x_1 x_2 x_3 1 ?x5676)))
 (let (($x10791 (= (stack_s x_0 x_1 x_2 x_3 1 (bvadd (_ bv61 6) ?x154)) (stack_s x_0 x_1 x_2 x_3 0 (bvadd (_ bv61 6) ?x72)))))
 (let (($x2599 (= (stack_s x_0 x_1 x_2 x_3 1 (bvadd (_ bv60 6) ?x154)) (stack_s x_0 x_1 x_2 x_3 0 (bvadd (_ bv63 6) ?x72)))))
 (let (($x696 (forall ((w (_ BitVec 256)) )(let ((?x6040 (storage_s x_0 x_1 x_2 x_3 0 w)))
 (= ?x6040 (_ bv0 256))))
 ))
 (let (($x7399 (= ?x9555 0)))
 (let (($x10766 (not $x57)))
 (let (($x3779 (= (stack_s x_0 x_1 x_2 x_3 0 (_ bv3 6)) x_3)))
 (let (($x5360 (= (stack_s x_0 x_1 x_2 x_3 0 (_ bv2 6)) x_2)))
 (let (($x1494 (= (stack_s x_0 x_1 x_2 x_3 0 (_ bv1 6)) x_1)))
 (let (($x1323 (= (stack_s x_0 x_1 x_2 x_3 0 (_ bv0 6)) x_0)))
 (let (($x289 (= ?x72 (_ bv4 6))))
 (and $x289 $x1323 $x1494 $x5360 $x3779 $x10766 $x7399 $x696 (= ?x10506 (stack_s x_0 x_1 x_2 x_3 0 (bvadd (_ bv60 6) ?x72))) $x2599 $x10791 (= ?x4861 (stack_s x_0 x_1 x_2 x_3 0 (bvadd (_ bv62 6) ?x72))) (= (used_gas_s x_0 x_1 x_2 x_3 1) (+ 3 ?x9555)) $x2802 $x10017 $x804 (= (exc_halt_s 1) (or $x57 (not (bvsle (_ bv0 6) (bvadd (_ bv60 6) ?x72))))) (= ?x6085 ?x4861) (= ?x10755 ?x10506) (= ?x6966 (+ 3 (used_gas_s x_0 x_1 x_2 x_3 1))) $x8033 $x3846 $x8513 $x7098 (= ?x5101 (stack_s x_0 x_1 x_2 x_3 2 (bvadd (_ bv60 6) ?x218))) (= (stack_s x_0 x_1 x_2 x_3 3 (bvadd (_ bv60 6) ?x275)) ?x6085) $x2148 (= ?x7575 ?x10755) (= ?x6209 (+ 3 ?x6966)) $x6815 $x10458 $x2328 $x5473 $x7094 $x4946 $x6285 $x8692 $x985 $x2571 (= ?x3404 (stack_t x_0 x_1 x_2 x_3 0 (bvadd (_ bv62 6) ?x63))) (= ?x8626 (stack_t x_0 x_1 x_2 x_3 0 (bvadd (_ bv63 6) ?x63))) (= (used_gas_t x_0 x_1 x_2 x_3 1) (+ 3 ?x6145)) $x6232 $x2882 $x9319 (= (exc_halt_t 1) (or $x56 (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) ?x63))))) (= ?x9933 (stack_t x_0 x_1 x_2 x_3 1 (bvadd (_ bv60 6) ?x7154))) (= (stack_t x_0 x_1 x_2 x_3 2 (bvadd (_ bv60 6) ?x2714)) ?x3404) $x8988 (= ?x8552 ?x8626) (= ?x990 (+ 3 (used_gas_t x_0 x_1 x_2 x_3 1))) $x6225 $x8698 $x7319 $x2826 $x2680 $x5058 (= ?x6438 ?x5618) $x6559 $x2423 $x503 $x73 $x6965 $x58 $x6726 $x2454 (not (and $x3162 $x1248 $x6006 $x6121)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
 )
(check-sat)
(get-proof)