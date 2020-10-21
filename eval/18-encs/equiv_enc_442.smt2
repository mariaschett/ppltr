; PUSH cw_1 SWAP4 SWAP2 SWAP3 SWAP1 SWAP2 => SWAP1 SWAP2 PUSH cw_1 SWAP4
(set-option :produce-proofs true)
; 
(set-info :status unknown)
(declare-fun storage_t ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 256)) (_ BitVec 256))
(declare-fun w_1 () (_ BitVec 256))
(declare-fun storage_s ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 256)) (_ BitVec 256))
(declare-fun exc_halt_t (Int) Bool)
(declare-fun exc_halt_s (Int) Bool)
(declare-fun sc_t (Int) (_ BitVec 6))
(declare-fun stack_t ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 6)) (_ BitVec 256))
(declare-fun stack_s ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 6)) (_ BitVec 256))
(declare-fun sc_s (Int) (_ BitVec 6))
(declare-fun used_gas_t ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int) Int)
(declare-fun used_gas_s ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int) Int)
(assert
 (exists ((x_0 (_ BitVec 256)) (x_1 (_ BitVec 256)) (x_2 (_ BitVec 256)) (x_3 (_ BitVec 256)) )(let (($x3761 (forall ((w (_ BitVec 256)) )(let ((?x807 (storage_t x_0 x_1 x_2 x_3 w_1 4 w)))
 (let ((?x5554 (storage_s x_0 x_1 x_2 x_3 w_1 6 w)))
 (= ?x5554 ?x807))))
 ))
 (let (($x7722 (exc_halt_t 4)))
 (let (($x772 (exc_halt_s 6)))
 (let (($x5620 (= $x772 $x7722)))
 (let (($x6838 (forall ((n (_ BitVec 6)) )(let ((?x11631 (sc_t 4)))
 (let (($x6345 (bvsle ?x11631 n)))
 (let ((?x6779 (stack_t x_0 x_1 x_2 x_3 w_1 4 n)))
 (let ((?x7474 (stack_s x_0 x_1 x_2 x_3 w_1 6 n)))
 (let (($x1348 (= ?x7474 ?x6779)))
 (or $x1348 $x6345)))))))
 ))
 (let ((?x11631 (sc_t 4)))
 (let ((?x926 (sc_s 6)))
 (let (($x11559 (= ?x926 ?x11631)))
 (let ((?x4877 (used_gas_t x_0 x_1 x_2 x_3 w_1 0)))
 (let ((?x10851 (used_gas_s x_0 x_1 x_2 x_3 w_1 0)))
 (let (($x5311 (= ?x10851 ?x4877)))
 (let (($x10292 (forall ((w (_ BitVec 256)) )(let ((?x8566 (storage_t x_0 x_1 x_2 x_3 w_1 0 w)))
 (let ((?x8781 (storage_s x_0 x_1 x_2 x_3 w_1 0 w)))
 (= ?x8781 ?x8566))))
 ))
 (let (($x56 (exc_halt_t 0)))
 (let (($x57 (exc_halt_s 0)))
 (let (($x58 (= $x57 $x56)))
 (let (($x5521 (forall ((n (_ BitVec 6)) )(let ((?x63 (sc_t 0)))
 (let (($x9192 (bvsle ?x63 n)))
 (let ((?x10011 (stack_t x_0 x_1 x_2 x_3 w_1 0 n)))
 (let ((?x7162 (stack_s x_0 x_1 x_2 x_3 w_1 0 n)))
 (let (($x2456 (= ?x7162 ?x10011)))
 (or $x2456 $x9192)))))))
 ))
 (let ((?x63 (sc_t 0)))
 (let ((?x72 (sc_s 0)))
 (let (($x73 (= ?x72 ?x63)))
 (let (($x5313 (= $x7722 (or (exc_halt_t 3) (not (bvsle (_ bv0 6) (bvadd (_ bv59 6) (sc_t 3))))))))
 (let (($x9869 (forall ((w (_ BitVec 256)) )(let ((?x3628 (storage_t x_0 x_1 x_2 x_3 w_1 3 w)))
 (let ((?x807 (storage_t x_0 x_1 x_2 x_3 w_1 4 w)))
 (= ?x807 ?x3628))))
 ))
 (let (($x10813 (forall ((n (_ BitVec 6)) )(let ((?x533 (stack_t x_0 x_1 x_2 x_3 w_1 3 n)))
 (let ((?x6779 (stack_t x_0 x_1 x_2 x_3 w_1 4 n)))
 (or (= ?x6779 ?x533) (bvsle (bvadd (_ bv59 6) (sc_t 3)) n)))))
 ))
 (let ((?x6438 (sc_t 3)))
 (let (($x6358 (= ?x11631 ?x6438)))
 (let (($x8061 (= (used_gas_t x_0 x_1 x_2 x_3 w_1 4) (+ 3 (used_gas_t x_0 x_1 x_2 x_3 w_1 3)))))
 (let (($x8228 (= (stack_t x_0 x_1 x_2 x_3 w_1 4 (bvadd (_ bv62 6) ?x11631)) (stack_t x_0 x_1 x_2 x_3 w_1 3 (bvadd (_ bv62 6) ?x6438)))))
 (let (($x4716 (= (stack_t x_0 x_1 x_2 x_3 w_1 4 (bvadd (_ bv61 6) ?x11631)) (stack_t x_0 x_1 x_2 x_3 w_1 3 (bvadd (_ bv61 6) ?x6438)))))
 (let (($x2534 (= (stack_t x_0 x_1 x_2 x_3 w_1 4 (bvadd (_ bv60 6) ?x11631)) (stack_t x_0 x_1 x_2 x_3 w_1 3 (bvadd (_ bv60 6) ?x6438)))))
 (let (($x5835 (= (stack_t x_0 x_1 x_2 x_3 w_1 4 (bvadd (_ bv59 6) ?x11631)) (stack_t x_0 x_1 x_2 x_3 w_1 3 (bvadd (_ bv63 6) ?x6438)))))
 (let (($x6548 (= (stack_t x_0 x_1 x_2 x_3 w_1 4 (bvadd (_ bv63 6) ?x11631)) (stack_t x_0 x_1 x_2 x_3 w_1 3 (bvadd (_ bv59 6) ?x6438)))))
 (let (($x4835 (exc_halt_t 2)))
 (let (($x8474 (or $x4835 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_t 2)))) (_ bv0 1))))))
 (let (($x6783 (exc_halt_t 3)))
 (let (($x5879 (= $x6783 $x8474)))
 (let (($x8853 (forall ((w (_ BitVec 256)) )(let ((?x4225 (storage_t x_0 x_1 x_2 x_3 w_1 2 w)))
 (let ((?x3628 (storage_t x_0 x_1 x_2 x_3 w_1 3 w)))
 (= ?x3628 ?x4225))))
 ))
 (let (($x10137 (forall ((n (_ BitVec 6)) )(let ((?x2714 (sc_t 2)))
 (let (($x9224 (bvsle ?x2714 n)))
 (let ((?x2523 (stack_t x_0 x_1 x_2 x_3 w_1 2 n)))
 (let ((?x533 (stack_t x_0 x_1 x_2 x_3 w_1 3 n)))
 (or (= ?x533 ?x2523) $x9224))))))
 ))
 (let (($x3556 (= ?x6438 (bvadd (_ bv1 6) (sc_t 2)))))
 (let ((?x5287 (used_gas_t x_0 x_1 x_2 x_3 w_1 3)))
 (let (($x2000 (= $x4835 (or (exc_halt_t 1) (not (bvsle (_ bv0 6) (bvadd (_ bv61 6) (sc_t 1))))))))
 (let (($x5021 (forall ((w (_ BitVec 256)) )(let ((?x10271 (storage_t x_0 x_1 x_2 x_3 w_1 1 w)))
 (let ((?x4225 (storage_t x_0 x_1 x_2 x_3 w_1 2 w)))
 (= ?x4225 ?x10271))))
 ))
 (let (($x304 (forall ((n (_ BitVec 6)) )(let ((?x6912 (stack_t x_0 x_1 x_2 x_3 w_1 1 n)))
 (let ((?x2523 (stack_t x_0 x_1 x_2 x_3 w_1 2 n)))
 (let ((?x8347 (sc_t 1)))
 (let ((?x3932 (bvadd (_ bv61 6) ?x8347)))
 (let (($x245 (bvsle ?x3932 n)))
 (or $x245 (= ?x2523 ?x6912))))))))
 ))
 (let ((?x8347 (sc_t 1)))
 (let ((?x2714 (sc_t 2)))
 (let (($x6528 (= ?x2714 ?x8347)))
 (let ((?x8032 (used_gas_t x_0 x_1 x_2 x_3 w_1 2)))
 (let ((?x2952 (bvadd (_ bv62 6) ?x8347)))
 (let ((?x717 (stack_t x_0 x_1 x_2 x_3 w_1 1 ?x2952)))
 (let ((?x4092 (bvadd (_ bv63 6) ?x8347)))
 (let ((?x737 (stack_t x_0 x_1 x_2 x_3 w_1 1 ?x4092)))
 (let (($x11381 (= (stack_t x_0 x_1 x_2 x_3 w_1 2 (bvadd (_ bv63 6) ?x2714)) (stack_t x_0 x_1 x_2 x_3 w_1 1 (bvadd (_ bv61 6) ?x8347)))))
 (let (($x3508 (exc_halt_t 1)))
 (let (($x10214 (= $x3508 (or $x56 (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) ?x63)))))))
 (let (($x4273 (forall ((w (_ BitVec 256)) )(let ((?x8566 (storage_t x_0 x_1 x_2 x_3 w_1 0 w)))
 (let ((?x10271 (storage_t x_0 x_1 x_2 x_3 w_1 1 w)))
 (= ?x10271 ?x8566))))
 ))
 (let (($x6902 (forall ((n (_ BitVec 6)) )(let ((?x63 (sc_t 0)))
 (let ((?x11156 (bvadd (_ bv62 6) ?x63)))
 (let (($x9494 (bvsle ?x11156 n)))
 (let ((?x10011 (stack_t x_0 x_1 x_2 x_3 w_1 0 n)))
 (let ((?x6912 (stack_t x_0 x_1 x_2 x_3 w_1 1 n)))
 (or (= ?x6912 ?x10011) $x9494)))))))
 ))
 (let (($x2906 (= ?x8347 ?x63)))
 (let (($x7909 (= $x772 (or (exc_halt_s 5) (not (bvsle (_ bv0 6) (bvadd (_ bv61 6) (sc_s 5))))))))
 (let (($x5570 (forall ((w (_ BitVec 256)) )(let ((?x10546 (storage_s x_0 x_1 x_2 x_3 w_1 5 w)))
 (let ((?x5554 (storage_s x_0 x_1 x_2 x_3 w_1 6 w)))
 (= ?x5554 ?x10546))))
 ))
 (let (($x893 (forall ((n (_ BitVec 6)) )(let ((?x6307 (stack_s x_0 x_1 x_2 x_3 w_1 5 n)))
 (let ((?x7474 (stack_s x_0 x_1 x_2 x_3 w_1 6 n)))
 (let ((?x4319 (sc_s 5)))
 (let ((?x4197 (bvadd (_ bv61 6) ?x4319)))
 (let (($x1833 (bvsle ?x4197 n)))
 (or $x1833 (= ?x7474 ?x6307))))))))
 ))
 (let ((?x4319 (sc_s 5)))
 (let (($x1292 (= ?x926 ?x4319)))
 (let (($x5352 (= (used_gas_s x_0 x_1 x_2 x_3 w_1 6) (+ 3 (used_gas_s x_0 x_1 x_2 x_3 w_1 5)))))
 (let ((?x8326 (bvadd (_ bv62 6) ?x4319)))
 (let ((?x5709 (stack_s x_0 x_1 x_2 x_3 w_1 5 ?x8326)))
 (let ((?x5401 (bvadd (_ bv63 6) ?x4319)))
 (let ((?x9467 (stack_s x_0 x_1 x_2 x_3 w_1 5 ?x5401)))
 (let (($x1650 (= (stack_s x_0 x_1 x_2 x_3 w_1 6 (bvadd (_ bv63 6) ?x926)) (stack_s x_0 x_1 x_2 x_3 w_1 5 (bvadd (_ bv61 6) ?x4319)))))
 (let (($x11317 (exc_halt_s 5)))
 (let (($x6879 (= $x11317 (or (exc_halt_s 4) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 4))))))))
 (let (($x10167 (forall ((w (_ BitVec 256)) )(let ((?x5427 (storage_s x_0 x_1 x_2 x_3 w_1 4 w)))
 (let ((?x10546 (storage_s x_0 x_1 x_2 x_3 w_1 5 w)))
 (= ?x10546 ?x5427))))
 ))
 (let (($x4455 (forall ((n (_ BitVec 6)) )(let ((?x4305 (sc_s 4)))
 (let ((?x9762 (bvadd (_ bv62 6) ?x4305)))
 (let (($x8231 (bvsle ?x9762 n)))
 (let ((?x5598 (stack_s x_0 x_1 x_2 x_3 w_1 4 n)))
 (let ((?x6307 (stack_s x_0 x_1 x_2 x_3 w_1 5 n)))
 (or (= ?x6307 ?x5598) $x8231)))))))
 ))
 (let ((?x4305 (sc_s 4)))
 (let (($x201 (= ?x4319 ?x4305)))
 (let ((?x9870 (used_gas_s x_0 x_1 x_2 x_3 w_1 5)))
 (let (($x7172 (exc_halt_s 4)))
 (let (($x8864 (= $x7172 (or (exc_halt_s 3) (not (bvsle (_ bv0 6) (bvadd (_ bv60 6) (sc_s 3))))))))
 (let (($x6619 (forall ((w (_ BitVec 256)) )(let ((?x2463 (storage_s x_0 x_1 x_2 x_3 w_1 3 w)))
 (let ((?x5427 (storage_s x_0 x_1 x_2 x_3 w_1 4 w)))
 (= ?x5427 ?x2463))))
 ))
 (let (($x7268 (forall ((n (_ BitVec 6)) )(let ((?x4170 (stack_s x_0 x_1 x_2 x_3 w_1 3 n)))
 (let ((?x5598 (stack_s x_0 x_1 x_2 x_3 w_1 4 n)))
 (or (bvsle (bvadd (_ bv60 6) (sc_s 3)) n) (= ?x5598 ?x4170)))))
 ))
 (let ((?x275 (sc_s 3)))
 (let (($x1977 (= ?x4305 ?x275)))
 (let ((?x5945 (used_gas_s x_0 x_1 x_2 x_3 w_1 4)))
 (let ((?x2146 (bvadd (_ bv62 6) ?x275)))
 (let ((?x11609 (stack_s x_0 x_1 x_2 x_3 w_1 3 ?x2146)))
 (let ((?x9762 (bvadd (_ bv62 6) ?x4305)))
 (let ((?x9699 (stack_s x_0 x_1 x_2 x_3 w_1 4 ?x9762)))
 (let ((?x1869 (bvadd (_ bv61 6) ?x275)))
 (let ((?x10934 (stack_s x_0 x_1 x_2 x_3 w_1 3 ?x1869)))
 (let ((?x5064 (bvadd (_ bv63 6) ?x275)))
 (let ((?x3010 (stack_s x_0 x_1 x_2 x_3 w_1 3 ?x5064)))
 (let ((?x9660 (bvadd (_ bv63 6) ?x4305)))
 (let ((?x9109 (stack_s x_0 x_1 x_2 x_3 w_1 4 ?x9660)))
 (let (($x292 (exc_halt_s 3)))
 (let (($x1524 (= $x292 (or (exc_halt_s 2) (not (bvsle (_ bv0 6) (bvadd (_ bv61 6) (sc_s 2))))))))
 (let (($x3189 (forall ((w (_ BitVec 256)) )(let ((?x1581 (storage_s x_0 x_1 x_2 x_3 w_1 2 w)))
 (let ((?x2463 (storage_s x_0 x_1 x_2 x_3 w_1 3 w)))
 (= ?x2463 ?x1581))))
 ))
 (let (($x185 (forall ((n (_ BitVec 6)) )(let ((?x7903 (stack_s x_0 x_1 x_2 x_3 w_1 2 n)))
 (let ((?x4170 (stack_s x_0 x_1 x_2 x_3 w_1 3 n)))
 (let ((?x218 (sc_s 2)))
 (let ((?x9942 (bvadd (_ bv61 6) ?x218)))
 (let (($x2636 (bvsle ?x9942 n)))
 (or $x2636 (= ?x4170 ?x7903))))))))
 ))
 (let ((?x218 (sc_s 2)))
 (let (($x2085 (= ?x275 ?x218)))
 (let ((?x9371 (used_gas_s x_0 x_1 x_2 x_3 w_1 3)))
 (let (($x247 (exc_halt_s 2)))
 (let (($x5670 (= $x247 (or (exc_halt_s 1) (not (bvsle (_ bv0 6) (bvadd (_ bv59 6) (sc_s 1))))))))
 (let (($x7531 (forall ((w (_ BitVec 256)) )(let ((?x7006 (storage_s x_0 x_1 x_2 x_3 w_1 1 w)))
 (let ((?x1581 (storage_s x_0 x_1 x_2 x_3 w_1 2 w)))
 (= ?x1581 ?x7006))))
 ))
 (let (($x8237 (forall ((n (_ BitVec 6)) )(let ((?x9448 (stack_s x_0 x_1 x_2 x_3 w_1 1 n)))
 (let ((?x7903 (stack_s x_0 x_1 x_2 x_3 w_1 2 n)))
 (or (= ?x7903 ?x9448) (bvsle (bvadd (_ bv59 6) (sc_s 1)) n)))))
 ))
 (let ((?x154 (sc_s 1)))
 (let (($x3997 (= ?x218 ?x154)))
 (let ((?x11175 (used_gas_s x_0 x_1 x_2 x_3 w_1 2)))
 (let ((?x11592 (bvadd (_ bv62 6) ?x218)))
 (let ((?x3021 (stack_s x_0 x_1 x_2 x_3 w_1 2 ?x11592)))
 (let ((?x9942 (bvadd (_ bv61 6) ?x218)))
 (let ((?x8464 (stack_s x_0 x_1 x_2 x_3 w_1 2 ?x9942)))
 (let (($x9475 (= (stack_s x_0 x_1 x_2 x_3 w_1 2 (bvadd (_ bv60 6) ?x218)) (stack_s x_0 x_1 x_2 x_3 w_1 1 (bvadd (_ bv60 6) ?x154)))))
 (let (($x9758 (= (stack_s x_0 x_1 x_2 x_3 w_1 2 (bvadd (_ bv59 6) ?x218)) (stack_s x_0 x_1 x_2 x_3 w_1 1 (bvadd (_ bv63 6) ?x154)))))
 (let ((?x3813 (bvadd (_ bv63 6) ?x218)))
 (let ((?x11667 (stack_s x_0 x_1 x_2 x_3 w_1 2 ?x3813)))
 (let (($x189 (exc_halt_s 1)))
 (let (($x6205 (= $x189 (or $x57 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x72))) (_ bv0 1)))))))
 (let (($x10958 (forall ((w (_ BitVec 256)) )(let ((?x8781 (storage_s x_0 x_1 x_2 x_3 w_1 0 w)))
 (let ((?x7006 (storage_s x_0 x_1 x_2 x_3 w_1 1 w)))
 (= ?x7006 ?x8781))))
 ))
 (let (($x6905 (forall ((n (_ BitVec 6)) )(let ((?x7162 (stack_s x_0 x_1 x_2 x_3 w_1 0 n)))
 (let ((?x9448 (stack_s x_0 x_1 x_2 x_3 w_1 1 n)))
 (let ((?x72 (sc_s 0)))
 (let (($x5726 (bvsle ?x72 n)))
 (or $x5726 (= ?x9448 ?x7162)))))))
 ))
 (let (($x9258 (= ?x154 (bvadd (_ bv1 6) ?x72))))
 (let (($x11741 (forall ((w (_ BitVec 256)) )(let ((?x8781 (storage_s x_0 x_1 x_2 x_3 w_1 0 w)))
 (= ?x8781 (_ bv0 256))))
 ))
 (let (($x3144 (= ?x10851 0)))
 (let (($x1141 (not $x57)))
 (let (($x1188 (= (stack_s x_0 x_1 x_2 x_3 w_1 0 (_ bv3 6)) x_3)))
 (let (($x6107 (= (stack_s x_0 x_1 x_2 x_3 w_1 0 (_ bv2 6)) x_2)))
 (let (($x6650 (= (stack_s x_0 x_1 x_2 x_3 w_1 0 (_ bv1 6)) x_1)))
 (let (($x5216 (= (stack_s x_0 x_1 x_2 x_3 w_1 0 (_ bv0 6)) x_0)))
 (let (($x289 (= ?x72 (_ bv4 6))))
 (and $x289 $x5216 $x6650 $x6107 $x1188 $x1141 $x3144 $x11741 (= (stack_s x_0 x_1 x_2 x_3 w_1 1 ?x72) w_1) (= (used_gas_s x_0 x_1 x_2 x_3 w_1 1) (+ 3 ?x10851)) $x9258 $x6905 $x10958 $x6205 (= ?x11667 (stack_s x_0 x_1 x_2 x_3 w_1 1 (bvadd (_ bv59 6) ?x154))) $x9758 $x9475 (= ?x8464 (stack_s x_0 x_1 x_2 x_3 w_1 1 (bvadd (_ bv61 6) ?x154))) (= ?x3021 (stack_s x_0 x_1 x_2 x_3 w_1 1 (bvadd (_ bv62 6) ?x154))) (= ?x11175 (+ 3 (used_gas_s x_0 x_1 x_2 x_3 w_1 1))) $x3997 $x8237 $x7531 $x5670 (= ?x3010 ?x8464) (= ?x10934 ?x11667) (= ?x11609 ?x3021) (= ?x9371 (+ 3 ?x11175)) $x2085 $x185 $x3189 $x1524 (= ?x9109 (stack_s x_0 x_1 x_2 x_3 w_1 3 (bvadd (_ bv60 6) ?x275))) (= (stack_s x_0 x_1 x_2 x_3 w_1 4 (bvadd (_ bv60 6) ?x4305)) ?x3010) (= (stack_s x_0 x_1 x_2 x_3 w_1 4 (bvadd (_ bv61 6) ?x4305)) ?x10934) (= ?x9699 ?x11609) (= ?x5945 (+ 3 ?x9371)) $x1977 $x7268 $x6619 $x8864 (= ?x9467 ?x9699) (= ?x5709 ?x9109) (= ?x9870 (+ 3 ?x5945)) $x201 $x4455 $x10167 $x6879 $x1650 (= (stack_s x_0 x_1 x_2 x_3 w_1 6 (bvadd (_ bv61 6) ?x926)) ?x9467) (= (stack_s x_0 x_1 x_2 x_3 w_1 6 (bvadd (_ bv62 6) ?x926)) ?x5709) $x5352 $x1292 $x893 $x5570 $x7909 (= ?x737 (stack_t x_0 x_1 x_2 x_3 w_1 0 (bvadd (_ bv62 6) ?x63))) (= ?x717 (stack_t x_0 x_1 x_2 x_3 w_1 0 (bvadd (_ bv63 6) ?x63))) (= (used_gas_t x_0 x_1 x_2 x_3 w_1 1) (+ 3 ?x4877)) $x2906 $x6902 $x4273 $x10214 $x11381 (= (stack_t x_0 x_1 x_2 x_3 w_1 2 (bvadd (_ bv61 6) ?x2714)) ?x737) (= (stack_t x_0 x_1 x_2 x_3 w_1 2 (bvadd (_ bv62 6) ?x2714)) ?x717) (= ?x8032 (+ 3 (used_gas_t x_0 x_1 x_2 x_3 w_1 1))) $x6528 $x304 $x5021 $x2000 (= (stack_t x_0 x_1 x_2 x_3 w_1 3 ?x2714) w_1) (= ?x5287 (+ 3 ?x8032)) $x3556 $x10137 $x8853 $x5879 $x6548 $x5835 $x2534 $x4716 $x8228 $x8061 $x6358 $x10813 $x9869 $x5313 $x73 $x5521 $x58 $x10292 $x5311 (not (and $x11559 $x6838 $x5620 $x3761)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
 )
(check-sat)
(get-proof)
