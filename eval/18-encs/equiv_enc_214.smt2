; CALLDATASIZE SWAP1 DUP1 DUP1 CALLDATALOAD SWAP1 => DUP1 DUP2 CALLDATALOAD CALLDATASIZE SWAP3
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
(declare-fun f_CALLDATALOAD ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256)) (_ BitVec 256))
(assert
 (exists ((x_0 (_ BitVec 256)) (x_CALLDATALOAD_0 (_ BitVec 256)) (x_CALLDATASIZE (_ BitVec 256)) )(let (($x10573 (forall ((w (_ BitVec 256)) )(let ((?x10703 (storage_t x_0 x_CALLDATALOAD_0 x_CALLDATASIZE 5 w)))
 (let ((?x3705 (storage_s x_0 x_CALLDATALOAD_0 x_CALLDATASIZE 6 w)))
 (= ?x3705 ?x10703))))
 ))
 (let (($x886 (exc_halt_t 5)))
 (let (($x772 (exc_halt_s 6)))
 (let (($x889 (= $x772 $x886)))
 (let (($x10661 (forall ((n (_ BitVec 6)) )(let ((?x919 (sc_t 5)))
 (let (($x8015 (bvsle ?x919 n)))
 (let ((?x2675 (stack_t x_0 x_CALLDATALOAD_0 x_CALLDATASIZE 5 n)))
 (let ((?x7267 (stack_s x_0 x_CALLDATALOAD_0 x_CALLDATASIZE 6 n)))
 (let (($x6674 (= ?x7267 ?x2675)))
 (or $x6674 $x8015)))))))
 ))
 (let ((?x919 (sc_t 5)))
 (let ((?x926 (sc_s 6)))
 (let (($x929 (= ?x926 ?x919)))
 (let ((?x71 (used_gas_t x_0 x_CALLDATALOAD_0 x_CALLDATASIZE 0)))
 (let ((?x9071 (used_gas_s x_0 x_CALLDATALOAD_0 x_CALLDATASIZE 0)))
 (let (($x7416 (= ?x9071 ?x71)))
 (let (($x8196 (forall ((w (_ BitVec 256)) )(let ((?x2478 (storage_t x_0 x_CALLDATALOAD_0 x_CALLDATASIZE 0 w)))
 (let ((?x5314 (storage_s x_0 x_CALLDATALOAD_0 x_CALLDATASIZE 0 w)))
 (= ?x5314 ?x2478))))
 ))
 (let (($x56 (exc_halt_t 0)))
 (let (($x57 (exc_halt_s 0)))
 (let (($x58 (= $x57 $x56)))
 (let (($x2106 (forall ((n (_ BitVec 6)) )(let ((?x63 (sc_t 0)))
 (let (($x10077 (bvsle ?x63 n)))
 (let ((?x1939 (stack_t x_0 x_CALLDATALOAD_0 x_CALLDATASIZE 0 n)))
 (let ((?x989 (stack_s x_0 x_CALLDATALOAD_0 x_CALLDATASIZE 0 n)))
 (let (($x6471 (= ?x989 ?x1939)))
 (or $x6471 $x10077)))))))
 ))
 (let ((?x63 (sc_t 0)))
 (let ((?x72 (sc_s 0)))
 (let (($x73 (= ?x72 ?x63)))
 (let (($x2195 (= $x886 (or (exc_halt_t 4) (not (bvsle (_ bv0 6) (bvadd (_ bv60 6) (sc_t 4))))))))
 (let (($x8150 (forall ((w (_ BitVec 256)) )(let ((?x7184 (storage_t x_0 x_CALLDATALOAD_0 x_CALLDATASIZE 4 w)))
 (let ((?x10703 (storage_t x_0 x_CALLDATALOAD_0 x_CALLDATASIZE 5 w)))
 (= ?x10703 ?x7184))))
 ))
 (let (($x5547 (forall ((n (_ BitVec 6)) )(let ((?x7052 (stack_t x_0 x_CALLDATALOAD_0 x_CALLDATASIZE 4 n)))
 (let ((?x2675 (stack_t x_0 x_CALLDATALOAD_0 x_CALLDATASIZE 5 n)))
 (or (bvsle (bvadd (_ bv60 6) (sc_t 4)) n) (= ?x2675 ?x7052)))))
 ))
 (let ((?x3757 (sc_t 4)))
 (let (($x10263 (= ?x919 ?x3757)))
 (let (($x2145 (= (used_gas_t x_0 x_CALLDATALOAD_0 x_CALLDATASIZE 5) (+ 3 (used_gas_t x_0 x_CALLDATALOAD_0 x_CALLDATASIZE 4)))))
 (let ((?x9981 (stack_t x_0 x_CALLDATALOAD_0 x_CALLDATASIZE 4 (bvadd (_ bv62 6) ?x3757))))
 (let ((?x10424 (stack_t x_0 x_CALLDATALOAD_0 x_CALLDATASIZE 4 (bvadd (_ bv61 6) ?x3757))))
 (let ((?x4637 (bvadd (_ bv63 6) ?x3757)))
 (let ((?x11410 (stack_t x_0 x_CALLDATALOAD_0 x_CALLDATASIZE 4 ?x4637)))
 (let (($x4217 (= (stack_t x_0 x_CALLDATALOAD_0 x_CALLDATASIZE 5 (bvadd (_ bv63 6) ?x919)) (stack_t x_0 x_CALLDATALOAD_0 x_CALLDATASIZE 4 (bvadd (_ bv60 6) ?x3757)))))
 (let (($x8325 (exc_halt_t 3)))
 (let (($x10319 (or $x8325 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_t 3)))) (_ bv0 1))))))
 (let (($x3723 (exc_halt_t 4)))
 (let (($x5273 (= $x3723 $x10319)))
 (let (($x9453 (forall ((w (_ BitVec 256)) )(let ((?x9420 (storage_t x_0 x_CALLDATALOAD_0 x_CALLDATASIZE 3 w)))
 (let ((?x7184 (storage_t x_0 x_CALLDATALOAD_0 x_CALLDATASIZE 4 w)))
 (= ?x7184 ?x9420))))
 ))
 (let (($x8208 (forall ((n (_ BitVec 6)) )(let ((?x3005 (sc_t 3)))
 (let (($x9001 (bvsle ?x3005 n)))
 (let ((?x325 (stack_t x_0 x_CALLDATALOAD_0 x_CALLDATASIZE 3 n)))
 (let ((?x7052 (stack_t x_0 x_CALLDATALOAD_0 x_CALLDATASIZE 4 n)))
 (let (($x2336 (= ?x7052 ?x325)))
 (or $x2336 $x9001)))))))
 ))
 (let (($x66 (= ?x3757 (bvadd (_ bv1 6) (sc_t 3)))))
 (let ((?x9261 (used_gas_t x_0 x_CALLDATALOAD_0 x_CALLDATASIZE 4)))
 (let (($x11242 (= $x8325 (or (exc_halt_t 2) (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_t 2))))))))
 (let (($x3787 (forall ((w (_ BitVec 256)) )(let ((?x7156 (storage_t x_0 x_CALLDATALOAD_0 x_CALLDATASIZE 2 w)))
 (let ((?x9420 (storage_t x_0 x_CALLDATALOAD_0 x_CALLDATASIZE 3 w)))
 (= ?x9420 ?x7156))))
 ))
 (let (($x7081 (forall ((n (_ BitVec 6)) )(let ((?x2709 (stack_t x_0 x_CALLDATALOAD_0 x_CALLDATASIZE 2 n)))
 (let ((?x325 (stack_t x_0 x_CALLDATALOAD_0 x_CALLDATASIZE 3 n)))
 (let (($x11205 (= ?x325 ?x2709)))
 (let ((?x4056 (sc_t 2)))
 (let ((?x5207 (bvadd (_ bv63 6) ?x4056)))
 (let (($x10506 (bvsle ?x5207 n)))
 (or $x10506 $x11205))))))))
 ))
 (let ((?x4056 (sc_t 2)))
 (let ((?x3005 (sc_t 3)))
 (let (($x5357 (= ?x3005 ?x4056)))
 (let ((?x5078 (used_gas_t x_0 x_CALLDATALOAD_0 x_CALLDATASIZE 3)))
 (let (($x682 (= ?x5078 (+ 3 (used_gas_t x_0 x_CALLDATALOAD_0 x_CALLDATASIZE 2)))))
 (let ((?x5207 (bvadd (_ bv63 6) ?x4056)))
 (let ((?x7030 (stack_t x_0 x_CALLDATALOAD_0 x_CALLDATASIZE 2 ?x5207)))
 (let ((?x4707 (bvadd (_ bv63 6) ?x3005)))
 (let ((?x2581 (stack_t x_0 x_CALLDATALOAD_0 x_CALLDATASIZE 3 ?x4707)))
 (let (($x7602 (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_t 1))))))
 (let (($x4670 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_t 1)))) (_ bv0 1)))))
 (let (($x1920 (exc_halt_t 1)))
 (let (($x903 (exc_halt_t 2)))
 (let (($x10235 (= $x903 (or $x1920 $x4670 $x7602))))
 (let (($x10214 (forall ((w (_ BitVec 256)) )(let ((?x10906 (storage_t x_0 x_CALLDATALOAD_0 x_CALLDATASIZE 1 w)))
 (let ((?x7156 (storage_t x_0 x_CALLDATALOAD_0 x_CALLDATASIZE 2 w)))
 (= ?x7156 ?x10906))))
 ))
 (let (($x4127 (forall ((n (_ BitVec 6)) )(let ((?x4023 (sc_t 1)))
 (let ((?x8180 (bvadd (_ bv62 6) ?x4023)))
 (let (($x6372 (bvsle ?x8180 n)))
 (let ((?x9459 (stack_t x_0 x_CALLDATALOAD_0 x_CALLDATASIZE 1 n)))
 (let ((?x2709 (stack_t x_0 x_CALLDATALOAD_0 x_CALLDATASIZE 2 n)))
 (let (($x869 (= ?x2709 ?x9459)))
 (or $x869 $x6372))))))))
 ))
 (let (($x7292 (= ?x4056 (bvadd (_ bv1 6) (sc_t 1)))))
 (let ((?x6272 (used_gas_t x_0 x_CALLDATALOAD_0 x_CALLDATASIZE 2)))
 (let (($x7754 (= ?x6272 (+ 3 (used_gas_t x_0 x_CALLDATALOAD_0 x_CALLDATASIZE 1)))))
 (let ((?x4023 (sc_t 1)))
 (let ((?x6175 (bvadd (_ bv63 6) ?x4023)))
 (let ((?x7209 (stack_t x_0 x_CALLDATALOAD_0 x_CALLDATASIZE 1 ?x6175)))
 (let ((?x8180 (bvadd (_ bv62 6) ?x4023)))
 (let ((?x4048 (stack_t x_0 x_CALLDATALOAD_0 x_CALLDATASIZE 1 ?x8180)))
 (let (($x7035 (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) ?x63)))))
 (let (($x5287 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x63))) (_ bv0 1)))))
 (let (($x650 (= $x1920 (or $x56 $x5287 $x7035))))
 (let (($x3120 (forall ((w (_ BitVec 256)) )(let ((?x2478 (storage_t x_0 x_CALLDATALOAD_0 x_CALLDATASIZE 0 w)))
 (let ((?x10906 (storage_t x_0 x_CALLDATALOAD_0 x_CALLDATASIZE 1 w)))
 (= ?x10906 ?x2478))))
 ))
 (let (($x2382 (forall ((n (_ BitVec 6)) )(let ((?x63 (sc_t 0)))
 (let ((?x7582 (bvadd (_ bv63 6) ?x63)))
 (let (($x9928 (bvsle ?x7582 n)))
 (let ((?x1939 (stack_t x_0 x_CALLDATALOAD_0 x_CALLDATASIZE 0 n)))
 (let ((?x9459 (stack_t x_0 x_CALLDATALOAD_0 x_CALLDATASIZE 1 n)))
 (let (($x1192 (= ?x9459 ?x1939)))
 (or $x1192 $x9928))))))))
 ))
 (let (($x5112 (= ?x4023 (bvadd (_ bv1 6) ?x63))))
 (let ((?x5335 (used_gas_t x_0 x_CALLDATALOAD_0 x_CALLDATASIZE 1)))
 (let (($x6581 (= ?x5335 (+ 3 ?x71))))
 (let ((?x7582 (bvadd (_ bv63 6) ?x63)))
 (let ((?x9955 (stack_t x_0 x_CALLDATALOAD_0 x_CALLDATASIZE 0 ?x7582)))
 (let (($x5429 (= (stack_t x_0 x_CALLDATALOAD_0 x_CALLDATASIZE 1 ?x7582) ?x9955)))
 (let (($x1241 (= $x772 (or (exc_halt_s 5) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 5))))))))
 (let (($x124 (forall ((w (_ BitVec 256)) )(let ((?x11318 (storage_s x_0 x_CALLDATALOAD_0 x_CALLDATASIZE 5 w)))
 (let ((?x3705 (storage_s x_0 x_CALLDATALOAD_0 x_CALLDATASIZE 6 w)))
 (= ?x3705 ?x11318))))
 ))
 (let (($x10129 (forall ((n (_ BitVec 6)) )(let ((?x7499 (stack_s x_0 x_CALLDATALOAD_0 x_CALLDATASIZE 5 n)))
 (let ((?x7267 (stack_s x_0 x_CALLDATALOAD_0 x_CALLDATASIZE 6 n)))
 (let ((?x805 (sc_s 5)))
 (let ((?x9825 (bvadd (_ bv62 6) ?x805)))
 (let (($x10379 (bvsle ?x9825 n)))
 (or $x10379 (= ?x7267 ?x7499))))))))
 ))
 (let ((?x805 (sc_s 5)))
 (let (($x4116 (= ?x926 ?x805)))
 (let (($x5877 (= (used_gas_s x_0 x_CALLDATALOAD_0 x_CALLDATASIZE 6) (+ 3 (used_gas_s x_0 x_CALLDATALOAD_0 x_CALLDATASIZE 5)))))
 (let ((?x11362 (bvadd (_ bv63 6) ?x805)))
 (let ((?x1046 (stack_s x_0 x_CALLDATALOAD_0 x_CALLDATASIZE 5 ?x11362)))
 (let ((?x9825 (bvadd (_ bv62 6) ?x805)))
 (let ((?x3249 (stack_s x_0 x_CALLDATALOAD_0 x_CALLDATASIZE 5 ?x9825)))
 (let (($x3979 (exc_halt_s 5)))
 (let (($x6362 (= $x3979 (or (exc_halt_s 4) (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_s 4))))))))
 (let (($x2789 (forall ((w (_ BitVec 256)) )(let ((?x5295 (storage_s x_0 x_CALLDATALOAD_0 x_CALLDATASIZE 4 w)))
 (let ((?x11318 (storage_s x_0 x_CALLDATALOAD_0 x_CALLDATASIZE 5 w)))
 (= ?x11318 ?x5295))))
 ))
 (let (($x3335 (forall ((n (_ BitVec 6)) )(let ((?x3664 (stack_s x_0 x_CALLDATALOAD_0 x_CALLDATASIZE 4 n)))
 (let ((?x7499 (stack_s x_0 x_CALLDATALOAD_0 x_CALLDATASIZE 5 n)))
 (let (($x9615 (= ?x7499 ?x3664)))
 (let ((?x4305 (sc_s 4)))
 (let ((?x11391 (bvadd (_ bv63 6) ?x4305)))
 (let (($x6521 (bvsle ?x11391 n)))
 (or $x6521 $x9615))))))))
 ))
 (let ((?x4305 (sc_s 4)))
 (let (($x10230 (= ?x805 ?x4305)))
 (let ((?x2724 (used_gas_s x_0 x_CALLDATALOAD_0 x_CALLDATASIZE 5)))
 (let (($x6065 (= ?x2724 (+ 3 (used_gas_s x_0 x_CALLDATALOAD_0 x_CALLDATASIZE 4)))))
 (let ((?x11391 (bvadd (_ bv63 6) ?x4305)))
 (let ((?x5959 (stack_s x_0 x_CALLDATALOAD_0 x_CALLDATASIZE 4 ?x11391)))
 (let (($x11601 (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_s 3))))))
 (let (($x4879 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 3)))) (_ bv0 1)))))
 (let (($x292 (exc_halt_s 3)))
 (let (($x64 (exc_halt_s 4)))
 (let (($x3281 (forall ((w (_ BitVec 256)) )(let ((?x11312 (storage_s x_0 x_CALLDATALOAD_0 x_CALLDATASIZE 3 w)))
 (let ((?x5295 (storage_s x_0 x_CALLDATALOAD_0 x_CALLDATASIZE 4 w)))
 (= ?x5295 ?x11312))))
 ))
 (let (($x3057 (forall ((n (_ BitVec 6)) )(let ((?x275 (sc_s 3)))
 (let ((?x318 (bvadd (_ bv63 6) ?x275)))
 (let (($x11790 (bvsle ?x318 n)))
 (let ((?x824 (stack_s x_0 x_CALLDATALOAD_0 x_CALLDATASIZE 3 n)))
 (let ((?x3664 (stack_s x_0 x_CALLDATALOAD_0 x_CALLDATASIZE 4 n)))
 (let (($x6677 (= ?x3664 ?x824)))
 (or $x6677 $x11790))))))))
 ))
 (let (($x10369 (= ?x4305 (bvadd (_ bv1 6) (sc_s 3)))))
 (let ((?x10788 (used_gas_s x_0 x_CALLDATALOAD_0 x_CALLDATASIZE 4)))
 (let (($x7335 (= ?x10788 (+ 3 (used_gas_s x_0 x_CALLDATALOAD_0 x_CALLDATASIZE 3)))))
 (let ((?x275 (sc_s 3)))
 (let ((?x318 (bvadd (_ bv63 6) ?x275)))
 (let ((?x11304 (stack_s x_0 x_CALLDATALOAD_0 x_CALLDATASIZE 3 ?x318)))
 (let (($x3243 (= (stack_s x_0 x_CALLDATALOAD_0 x_CALLDATASIZE 4 ?x318) ?x11304)))
 (let (($x4965 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 2)))) (_ bv0 1)))))
 (let (($x338 (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_s 2))))))
 (let (($x247 (exc_halt_s 2)))
 (let (($x3262 (forall ((w (_ BitVec 256)) )(let ((?x194 (storage_s x_0 x_CALLDATALOAD_0 x_CALLDATASIZE 2 w)))
 (let ((?x11312 (storage_s x_0 x_CALLDATALOAD_0 x_CALLDATASIZE 3 w)))
 (= ?x11312 ?x194))))
 ))
 (let (($x5193 (forall ((n (_ BitVec 6)) )(let ((?x218 (sc_s 2)))
 (let ((?x1185 (bvadd (_ bv63 6) ?x218)))
 (let (($x7743 (bvsle ?x1185 n)))
 (let ((?x7261 (stack_s x_0 x_CALLDATALOAD_0 x_CALLDATASIZE 2 n)))
 (let ((?x824 (stack_s x_0 x_CALLDATALOAD_0 x_CALLDATASIZE 3 n)))
 (let (($x4121 (= ?x824 ?x7261)))
 (or $x4121 $x7743))))))))
 ))
 (let (($x1235 (= ?x275 (bvadd (_ bv1 6) (sc_s 2)))))
 (let ((?x2729 (used_gas_s x_0 x_CALLDATALOAD_0 x_CALLDATASIZE 3)))
 (let (($x3886 (= ?x2729 (+ 3 (used_gas_s x_0 x_CALLDATALOAD_0 x_CALLDATASIZE 2)))))
 (let ((?x218 (sc_s 2)))
 (let ((?x1185 (bvadd (_ bv63 6) ?x218)))
 (let ((?x2330 (stack_s x_0 x_CALLDATALOAD_0 x_CALLDATASIZE 2 ?x1185)))
 (let (($x3065 (= $x247 (or (exc_halt_s 1) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 1))))))))
 (let (($x4935 (forall ((w (_ BitVec 256)) )(let ((?x9220 (storage_s x_0 x_CALLDATALOAD_0 x_CALLDATASIZE 1 w)))
 (let ((?x194 (storage_s x_0 x_CALLDATALOAD_0 x_CALLDATASIZE 2 w)))
 (= ?x194 ?x9220))))
 ))
 (let (($x2854 (forall ((n (_ BitVec 6)) )(let ((?x154 (sc_s 1)))
 (let ((?x4164 (bvadd (_ bv62 6) ?x154)))
 (let (($x4700 (bvsle ?x4164 n)))
 (let ((?x7134 (stack_s x_0 x_CALLDATALOAD_0 x_CALLDATASIZE 1 n)))
 (let ((?x7261 (stack_s x_0 x_CALLDATALOAD_0 x_CALLDATASIZE 2 n)))
 (let (($x3771 (= ?x7261 ?x7134)))
 (or $x3771 $x4700))))))))
 ))
 (let ((?x154 (sc_s 1)))
 (let (($x10550 (= ?x218 ?x154)))
 (let ((?x8620 (used_gas_s x_0 x_CALLDATALOAD_0 x_CALLDATASIZE 2)))
 (let (($x11199 (= ?x8620 (+ 3 (used_gas_s x_0 x_CALLDATALOAD_0 x_CALLDATASIZE 1)))))
 (let ((?x917 (bvadd (_ bv63 6) ?x154)))
 (let ((?x3133 (stack_s x_0 x_CALLDATALOAD_0 x_CALLDATASIZE 1 ?x917)))
 (let ((?x732 (bvadd (_ bv62 6) ?x218)))
 (let ((?x11587 (stack_s x_0 x_CALLDATALOAD_0 x_CALLDATASIZE 2 ?x732)))
 (let (($x189 (exc_halt_s 1)))
 (let (($x7671 (= $x189 (or $x57 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x72))) (_ bv0 1)))))))
 (let (($x11144 (forall ((w (_ BitVec 256)) )(let ((?x5314 (storage_s x_0 x_CALLDATALOAD_0 x_CALLDATASIZE 0 w)))
 (let ((?x9220 (storage_s x_0 x_CALLDATALOAD_0 x_CALLDATASIZE 1 w)))
 (= ?x9220 ?x5314))))
 ))
 (let (($x925 (forall ((n (_ BitVec 6)) )(let ((?x989 (stack_s x_0 x_CALLDATALOAD_0 x_CALLDATASIZE 0 n)))
 (let ((?x7134 (stack_s x_0 x_CALLDATALOAD_0 x_CALLDATASIZE 1 n)))
 (let (($x11306 (= ?x7134 ?x989)))
 (let ((?x72 (sc_s 0)))
 (let (($x5211 (bvsle ?x72 n)))
 (or $x5211 $x11306)))))))
 ))
 (let (($x5350 (= ?x154 (bvadd (_ bv1 6) ?x72))))
 (let (($x10608 (forall ((w0 (_ BitVec 256)) )(let ((?x3256 (ite (= (stack_s x_0 x_CALLDATALOAD_0 x_CALLDATASIZE 4 (bvadd (_ bv63 6) (sc_s 4))) w0) x_CALLDATALOAD_0 (_ bv0 256))))
 (let ((?x8106 (f_CALLDATALOAD x_0 x_CALLDATALOAD_0 x_CALLDATASIZE w0)))
 (= ?x8106 ?x3256))))
 ))
 (let (($x2582 (forall ((w (_ BitVec 256)) )(let ((?x5314 (storage_s x_0 x_CALLDATALOAD_0 x_CALLDATASIZE 0 w)))
 (= ?x5314 (_ bv0 256))))
 ))
 (let (($x7504 (= ?x9071 0)))
 (let (($x1898 (not $x57)))
 (let (($x86 (= (stack_s x_0 x_CALLDATALOAD_0 x_CALLDATASIZE 0 (_ bv0 6)) x_0)))
 (let (($x8685 (= ?x72 (_ bv1 6))))
 (and $x8685 $x86 $x1898 $x7504 $x2582 $x10608 (= (stack_s x_0 x_CALLDATALOAD_0 x_CALLDATASIZE 1 ?x72) x_CALLDATASIZE) (= (used_gas_s x_0 x_CALLDATALOAD_0 x_CALLDATASIZE 1) (+ 2 ?x9071)) $x5350 $x925 $x11144 $x7671 (= ?x2330 (stack_s x_0 x_CALLDATALOAD_0 x_CALLDATASIZE 1 (bvadd (_ bv62 6) ?x154))) (= ?x11587 ?x3133) $x11199 $x10550 $x2854 $x4935 $x3065 (= ?x11304 ?x2330) (= (stack_s x_0 x_CALLDATALOAD_0 x_CALLDATASIZE 3 ?x1185) ?x2330) $x3886 $x1235 $x5193 $x3262 (= $x292 (or $x247 $x338 $x4965)) (= ?x5959 ?x11304) $x3243 $x7335 $x10369 $x3057 $x3281 (= $x64 (or $x292 $x4879 $x11601)) (= ?x1046 (f_CALLDATALOAD x_0 x_CALLDATALOAD_0 x_CALLDATASIZE ?x5959)) $x6065 $x10230 $x3335 $x2789 $x6362 (= (stack_s x_0 x_CALLDATALOAD_0 x_CALLDATASIZE 6 (bvadd (_ bv63 6) ?x926)) ?x3249) (= (stack_s x_0 x_CALLDATALOAD_0 x_CALLDATASIZE 6 (bvadd (_ bv62 6) ?x926)) ?x1046) $x5877 $x4116 $x10129 $x124 $x1241 (= ?x7209 ?x9955) $x5429 $x6581 $x5112 $x2382 $x3120 $x650 (= ?x7030 ?x4048) (= (stack_t x_0 x_CALLDATALOAD_0 x_CALLDATASIZE 2 ?x8180) ?x4048) (= (stack_t x_0 x_CALLDATALOAD_0 x_CALLDATASIZE 2 ?x6175) ?x7209) $x7754 $x7292 $x4127 $x10214 $x10235 (= ?x2581 (f_CALLDATALOAD x_0 x_CALLDATALOAD_0 x_CALLDATASIZE ?x7030)) $x682 $x5357 $x7081 $x3787 $x11242 (= (stack_t x_0 x_CALLDATALOAD_0 x_CALLDATASIZE 4 ?x3005) x_CALLDATASIZE) (= ?x9261 (+ 2 ?x5078)) $x66 $x8208 $x9453 $x5273 $x4217 (= (stack_t x_0 x_CALLDATALOAD_0 x_CALLDATASIZE 5 (bvadd (_ bv60 6) ?x919)) ?x11410) (= (stack_t x_0 x_CALLDATALOAD_0 x_CALLDATASIZE 5 (bvadd (_ bv61 6) ?x919)) ?x10424) (= (stack_t x_0 x_CALLDATALOAD_0 x_CALLDATASIZE 5 (bvadd (_ bv62 6) ?x919)) ?x9981) $x2145 $x10263 $x5547 $x8150 $x2195 $x73 $x2106 $x58 $x8196 $x7416 (not (and $x929 $x10661 $x889 $x10573))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
 )
(check-sat)
(get-proof)