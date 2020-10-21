; DUP4 SWAP3 DUP2 DUP3 DUP4 SWAP3 => DUP1 DUP2 DUP3 DUP7 SWAP6
(set-option :produce-proofs true)
; 
(set-info :status unknown)
(declare-fun storage_t ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 256)) (_ BitVec 256))
(declare-fun storage_s ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 256)) (_ BitVec 256))
(declare-fun exc_halt_t (Int) Bool)
(declare-fun exc_halt_s (Int) Bool)
(declare-fun sc_t (Int) (_ BitVec 6))
(declare-fun stack_t ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 6)) (_ BitVec 256))
(declare-fun stack_s ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 6)) (_ BitVec 256))
(declare-fun sc_s (Int) (_ BitVec 6))
(declare-fun used_gas_t ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int) Int)
(declare-fun used_gas_s ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int) Int)
(assert
 (exists ((x_0 (_ BitVec 256)) (x_1 (_ BitVec 256)) (x_2 (_ BitVec 256)) (x_3 (_ BitVec 256)) )(let (($x10739 (forall ((w (_ BitVec 256)) )(let ((?x10206 (storage_t x_0 x_1 x_2 x_3 5 w)))
 (let ((?x9703 (storage_s x_0 x_1 x_2 x_3 6 w)))
 (= ?x9703 ?x10206))))
 ))
 (let (($x886 (exc_halt_t 5)))
 (let (($x772 (exc_halt_s 6)))
 (let (($x889 (= $x772 $x886)))
 (let (($x7504 (forall ((n (_ BitVec 6)) )(let ((?x919 (sc_t 5)))
 (let (($x4107 (bvsle ?x919 n)))
 (let ((?x3705 (stack_t x_0 x_1 x_2 x_3 5 n)))
 (let ((?x6687 (stack_s x_0 x_1 x_2 x_3 6 n)))
 (let (($x647 (= ?x6687 ?x3705)))
 (or $x647 $x4107)))))))
 ))
 (let ((?x919 (sc_t 5)))
 (let ((?x926 (sc_s 6)))
 (let (($x929 (= ?x926 ?x919)))
 (let (($x6907 (not (and $x929 $x7504 $x889 $x10739))))
 (let ((?x2321 (used_gas_t x_0 x_1 x_2 x_3 0)))
 (let ((?x10688 (used_gas_s x_0 x_1 x_2 x_3 0)))
 (let (($x10731 (= ?x10688 ?x2321)))
 (let (($x10735 (forall ((w (_ BitVec 256)) )(let ((?x6204 (storage_t x_0 x_1 x_2 x_3 0 w)))
 (let ((?x10728 (storage_s x_0 x_1 x_2 x_3 0 w)))
 (= ?x10728 ?x6204))))
 ))
 (let (($x56 (exc_halt_t 0)))
 (let (($x57 (exc_halt_s 0)))
 (let (($x58 (= $x57 $x56)))
 (let (($x3764 (forall ((n (_ BitVec 6)) )(let ((?x10737 (stack_t x_0 x_1 x_2 x_3 0 n)))
 (let ((?x943 (stack_s x_0 x_1 x_2 x_3 0 n)))
 (let (($x10738 (= ?x943 ?x10737)))
 (let ((?x63 (sc_t 0)))
 (let (($x4561 (bvsle ?x63 n)))
 (or $x4561 $x10738)))))))
 ))
 (let ((?x63 (sc_t 0)))
 (let ((?x72 (sc_s 0)))
 (let (($x73 (= ?x72 ?x63)))
 (let (($x1837 (= $x886 (or (exc_halt_t 4) (not (bvsle (_ bv0 6) (bvadd (_ bv57 6) (sc_t 4))))))))
 (let (($x7164 (forall ((w (_ BitVec 256)) )(let ((?x10740 (storage_t x_0 x_1 x_2 x_3 4 w)))
 (let ((?x10206 (storage_t x_0 x_1 x_2 x_3 5 w)))
 (= ?x10206 ?x10740))))
 ))
 (let (($x8337 (forall ((n (_ BitVec 6)) )(let ((?x10745 (stack_t x_0 x_1 x_2 x_3 4 n)))
 (let ((?x3705 (stack_t x_0 x_1 x_2 x_3 5 n)))
 (let (($x6055 (= ?x3705 ?x10745)))
 (or $x6055 (bvsle (bvadd (_ bv57 6) (sc_t 4)) n))))))
 ))
 (let ((?x3757 (sc_t 4)))
 (let (($x107 (= ?x919 ?x3757)))
 (let (($x2380 (= (used_gas_t x_0 x_1 x_2 x_3 5) (+ 3 (used_gas_t x_0 x_1 x_2 x_3 4)))))
 (let (($x2442 (= (stack_t x_0 x_1 x_2 x_3 5 (bvadd (_ bv62 6) ?x919)) (stack_t x_0 x_1 x_2 x_3 4 (bvadd (_ bv62 6) ?x3757)))))
 (let (($x10573 (= (stack_t x_0 x_1 x_2 x_3 5 (bvadd (_ bv61 6) ?x919)) (stack_t x_0 x_1 x_2 x_3 4 (bvadd (_ bv61 6) ?x3757)))))
 (let (($x3486 (= (stack_t x_0 x_1 x_2 x_3 5 (bvadd (_ bv60 6) ?x919)) (stack_t x_0 x_1 x_2 x_3 4 (bvadd (_ bv60 6) ?x3757)))))
 (let (($x8515 (= (stack_t x_0 x_1 x_2 x_3 5 (bvadd (_ bv59 6) ?x919)) (stack_t x_0 x_1 x_2 x_3 4 (bvadd (_ bv59 6) ?x3757)))))
 (let ((?x6369 (bvadd (_ bv58 6) ?x3757)))
 (let ((?x6040 (stack_t x_0 x_1 x_2 x_3 4 ?x6369)))
 (let ((?x4901 (stack_t x_0 x_1 x_2 x_3 5 (bvadd (_ bv58 6) ?x919))))
 (let ((?x6105 (bvadd (_ bv63 6) ?x3757)))
 (let ((?x7302 (stack_t x_0 x_1 x_2 x_3 4 ?x6105)))
 (let ((?x9841 (bvadd (_ bv63 6) ?x919)))
 (let ((?x2319 (stack_t x_0 x_1 x_2 x_3 5 ?x9841)))
 (let (($x10336 (exc_halt_t 3)))
 (let (($x6306 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_t 3)))) (_ bv0 1)))))
 (let (($x3723 (exc_halt_t 4)))
 (let (($x10455 (forall ((w (_ BitVec 256)) )(let ((?x10458 (storage_t x_0 x_1 x_2 x_3 3 w)))
 (let ((?x10740 (storage_t x_0 x_1 x_2 x_3 4 w)))
 (= ?x10740 ?x10458))))
 ))
 (let (($x2628 (forall ((n (_ BitVec 6)) )(let ((?x4127 (stack_t x_0 x_1 x_2 x_3 3 n)))
 (let ((?x10745 (stack_t x_0 x_1 x_2 x_3 4 n)))
 (let (($x3593 (= ?x10745 ?x4127)))
 (or $x3593 (bvsle (bvadd (_ bv57 6) (sc_t 3)) n))))))
 ))
 (let (($x6790 (= ?x3757 (bvadd (_ bv1 6) (sc_t 3)))))
 (let ((?x10747 (used_gas_t x_0 x_1 x_2 x_3 4)))
 (let (($x5200 (= ?x10747 (+ 3 (used_gas_t x_0 x_1 x_2 x_3 3)))))
 (let ((?x2012 (sc_t 3)))
 (let ((?x4070 (bvadd (_ bv63 6) ?x2012)))
 (let ((?x6787 (stack_t x_0 x_1 x_2 x_3 3 ?x4070)))
 (let (($x423 (= (stack_t x_0 x_1 x_2 x_3 4 ?x4070) ?x6787)))
 (let ((?x7056 (bvadd (_ bv62 6) ?x2012)))
 (let ((?x7090 (stack_t x_0 x_1 x_2 x_3 3 ?x7056)))
 (let (($x9265 (= (stack_t x_0 x_1 x_2 x_3 4 ?x7056) ?x7090)))
 (let ((?x7081 (bvadd (_ bv61 6) ?x2012)))
 (let ((?x2540 (stack_t x_0 x_1 x_2 x_3 3 ?x7081)))
 (let (($x7585 (= (stack_t x_0 x_1 x_2 x_3 4 ?x7081) ?x2540)))
 (let (($x2736 (= (stack_t x_0 x_1 x_2 x_3 4 (bvadd (_ bv60 6) ?x2012)) (stack_t x_0 x_1 x_2 x_3 3 (bvadd (_ bv60 6) ?x2012)))))
 (let (($x6892 (= (stack_t x_0 x_1 x_2 x_3 4 (bvadd (_ bv59 6) ?x2012)) (stack_t x_0 x_1 x_2 x_3 3 (bvadd (_ bv59 6) ?x2012)))))
 (let (($x10537 (= (stack_t x_0 x_1 x_2 x_3 4 (bvadd (_ bv58 6) ?x2012)) (stack_t x_0 x_1 x_2 x_3 3 (bvadd (_ bv58 6) ?x2012)))))
 (let ((?x10521 (bvadd (_ bv57 6) ?x2012)))
 (let ((?x9026 (stack_t x_0 x_1 x_2 x_3 3 ?x10521)))
 (let (($x6776 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_t 2)))) (_ bv0 1)))))
 (let (($x6616 (not (bvsle (_ bv0 6) (bvadd (_ bv61 6) (sc_t 2))))))
 (let (($x903 (exc_halt_t 2)))
 (let (($x7084 (forall ((w (_ BitVec 256)) )(let ((?x10136 (storage_t x_0 x_1 x_2 x_3 2 w)))
 (let ((?x10458 (storage_t x_0 x_1 x_2 x_3 3 w)))
 (= ?x10458 ?x10136))))
 ))
 (let (($x3245 (forall ((n (_ BitVec 6)) )(let ((?x10083 (stack_t x_0 x_1 x_2 x_3 2 n)))
 (let ((?x4127 (stack_t x_0 x_1 x_2 x_3 3 n)))
 (or (= ?x4127 ?x10083) (bvsle (bvadd (_ bv61 6) (sc_t 2)) n)))))
 ))
 (let (($x8282 (= ?x2012 (bvadd (_ bv1 6) (sc_t 2)))))
 (let ((?x3491 (used_gas_t x_0 x_1 x_2 x_3 3)))
 (let (($x6895 (= ?x3491 (+ 3 (used_gas_t x_0 x_1 x_2 x_3 2)))))
 (let ((?x4056 (sc_t 2)))
 (let ((?x6390 (bvadd (_ bv63 6) ?x4056)))
 (let ((?x7113 (stack_t x_0 x_1 x_2 x_3 2 ?x6390)))
 (let ((?x6845 (bvadd (_ bv62 6) ?x4056)))
 (let ((?x7076 (stack_t x_0 x_1 x_2 x_3 2 ?x6845)))
 (let ((?x5336 (bvadd (_ bv61 6) ?x4056)))
 (let ((?x5306 (stack_t x_0 x_1 x_2 x_3 2 ?x5336)))
 (let (($x7300 (= ?x6787 ?x5306)))
 (let (($x1920 (exc_halt_t 1)))
 (let (($x390 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_t 1)))) (_ bv0 1)))))
 (let (($x7193 (forall ((w (_ BitVec 256)) )(let ((?x3941 (storage_t x_0 x_1 x_2 x_3 1 w)))
 (let ((?x10136 (storage_t x_0 x_1 x_2 x_3 2 w)))
 (= ?x10136 ?x3941))))
 ))
 (let (($x1413 (forall ((n (_ BitVec 6)) )(let ((?x10703 (stack_t x_0 x_1 x_2 x_3 1 n)))
 (let ((?x10083 (stack_t x_0 x_1 x_2 x_3 2 n)))
 (let (($x1044 (= ?x10083 ?x10703)))
 (or $x1044 (bvsle (bvadd (_ bv62 6) (sc_t 1)) n))))))
 ))
 (let (($x8461 (= ?x4056 (bvadd (_ bv1 6) (sc_t 1)))))
 (let ((?x6689 (used_gas_t x_0 x_1 x_2 x_3 2)))
 (let (($x6542 (= ?x6689 (+ 3 (used_gas_t x_0 x_1 x_2 x_3 1)))))
 (let ((?x4023 (sc_t 1)))
 (let ((?x7954 (bvadd (_ bv63 6) ?x4023)))
 (let ((?x7790 (stack_t x_0 x_1 x_2 x_3 1 ?x7954)))
 (let (($x6136 (= (stack_t x_0 x_1 x_2 x_3 2 ?x7954) ?x7790)))
 (let ((?x8421 (bvadd (_ bv62 6) ?x4023)))
 (let ((?x3570 (stack_t x_0 x_1 x_2 x_3 1 ?x8421)))
 (let (($x6191 (= (stack_t x_0 x_1 x_2 x_3 2 ?x8421) ?x3570)))
 (let (($x8845 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x63))) (_ bv0 1)))))
 (let (($x7910 (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) ?x63)))))
 (let (($x7805 (= $x1920 (or $x56 $x7910 $x8845))))
 (let (($x4773 (forall ((w (_ BitVec 256)) )(let ((?x6204 (storage_t x_0 x_1 x_2 x_3 0 w)))
 (let ((?x3941 (storage_t x_0 x_1 x_2 x_3 1 w)))
 (= ?x3941 ?x6204))))
 ))
 (let (($x2454 (forall ((n (_ BitVec 6)) )(let ((?x63 (sc_t 0)))
 (let ((?x741 (bvadd (_ bv63 6) ?x63)))
 (let (($x9323 (bvsle ?x741 n)))
 (let ((?x10737 (stack_t x_0 x_1 x_2 x_3 0 n)))
 (let ((?x10703 (stack_t x_0 x_1 x_2 x_3 1 n)))
 (let (($x613 (= ?x10703 ?x10737)))
 (or $x613 $x9323))))))))
 ))
 (let (($x8795 (= ?x4023 (bvadd (_ bv1 6) ?x63))))
 (let ((?x2925 (used_gas_t x_0 x_1 x_2 x_3 1)))
 (let (($x367 (= ?x2925 (+ 3 ?x2321))))
 (let ((?x741 (bvadd (_ bv63 6) ?x63)))
 (let ((?x9305 (stack_t x_0 x_1 x_2 x_3 0 ?x741)))
 (let (($x8502 (= $x772 (or (exc_halt_s 5) (not (bvsle (_ bv0 6) (bvadd (_ bv60 6) (sc_s 5))))))))
 (let (($x9523 (forall ((w (_ BitVec 256)) )(let ((?x10647 (storage_s x_0 x_1 x_2 x_3 5 w)))
 (let ((?x9703 (storage_s x_0 x_1 x_2 x_3 6 w)))
 (= ?x9703 ?x10647))))
 ))
 (let (($x8960 (forall ((n (_ BitVec 6)) )(let ((?x2760 (stack_s x_0 x_1 x_2 x_3 5 n)))
 (let ((?x6687 (stack_s x_0 x_1 x_2 x_3 6 n)))
 (let (($x4082 (= ?x6687 ?x2760)))
 (or $x4082 (bvsle (bvadd (_ bv60 6) (sc_s 5)) n))))))
 ))
 (let ((?x805 (sc_s 5)))
 (let (($x3879 (= ?x926 ?x805)))
 (let (($x6832 (= (used_gas_s x_0 x_1 x_2 x_3 6) (+ 3 (used_gas_s x_0 x_1 x_2 x_3 5)))))
 (let ((?x6337 (bvadd (_ bv62 6) ?x805)))
 (let ((?x6338 (stack_s x_0 x_1 x_2 x_3 5 ?x6337)))
 (let (($x4072 (= (stack_s x_0 x_1 x_2 x_3 6 (bvadd (_ bv62 6) ?x926)) ?x6338)))
 (let ((?x10112 (bvadd (_ bv61 6) ?x805)))
 (let ((?x10177 (stack_s x_0 x_1 x_2 x_3 5 ?x10112)))
 (let ((?x5037 (bvadd (_ bv61 6) ?x926)))
 (let ((?x4063 (stack_s x_0 x_1 x_2 x_3 6 ?x5037)))
 (let ((?x10909 (bvadd (_ bv63 6) ?x805)))
 (let ((?x10334 (stack_s x_0 x_1 x_2 x_3 5 ?x10909)))
 (let ((?x329 (bvadd (_ bv60 6) ?x805)))
 (let ((?x6593 (stack_s x_0 x_1 x_2 x_3 5 ?x329)))
 (let ((?x3670 (bvadd (_ bv63 6) ?x926)))
 (let ((?x6577 (stack_s x_0 x_1 x_2 x_3 6 ?x3670)))
 (let (($x6469 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 4)))) (_ bv0 1)))))
 (let (($x64 (exc_halt_s 4)))
 (let (($x3979 (exc_halt_s 5)))
 (let (($x9177 (= $x3979 (or $x64 $x6469 (not (bvsle (_ bv0 6) (bvadd (_ bv60 6) (sc_s 4))))))))
 (let (($x6837 (forall ((w (_ BitVec 256)) )(let ((?x10464 (storage_s x_0 x_1 x_2 x_3 4 w)))
 (let ((?x10647 (storage_s x_0 x_1 x_2 x_3 5 w)))
 (= ?x10647 ?x10464))))
 ))
 (let (($x10987 (forall ((n (_ BitVec 6)) )(let ((?x4305 (sc_s 4)))
 (let ((?x10240 (bvadd (_ bv60 6) ?x4305)))
 (let (($x9316 (bvsle ?x10240 n)))
 (let ((?x6327 (stack_s x_0 x_1 x_2 x_3 4 n)))
 (let ((?x2760 (stack_s x_0 x_1 x_2 x_3 5 n)))
 (let (($x6344 (= ?x2760 ?x6327)))
 (or $x6344 $x9316))))))))
 ))
 (let (($x9675 (= ?x805 (bvadd (_ bv1 6) (sc_s 4)))))
 (let ((?x10638 (used_gas_s x_0 x_1 x_2 x_3 5)))
 (let (($x7696 (= ?x10638 (+ 3 (used_gas_s x_0 x_1 x_2 x_3 4)))))
 (let ((?x4305 (sc_s 4)))
 (let ((?x10988 (bvadd (_ bv63 6) ?x4305)))
 (let ((?x5459 (stack_s x_0 x_1 x_2 x_3 4 ?x10988)))
 (let ((?x803 (bvadd (_ bv62 6) ?x4305)))
 (let ((?x4765 (stack_s x_0 x_1 x_2 x_3 4 ?x803)))
 (let ((?x9411 (bvadd (_ bv61 6) ?x4305)))
 (let ((?x6342 (stack_s x_0 x_1 x_2 x_3 4 ?x9411)))
 (let ((?x10240 (bvadd (_ bv60 6) ?x4305)))
 (let ((?x2877 (stack_s x_0 x_1 x_2 x_3 4 ?x10240)))
 (let (($x6200 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 3)))) (_ bv0 1)))))
 (let (($x326 (not (bvsle (_ bv0 6) (bvadd (_ bv61 6) (sc_s 3))))))
 (let (($x292 (exc_halt_s 3)))
 (let (($x6318 (= $x64 (or $x292 $x326 $x6200))))
 (let (($x5392 (forall ((w (_ BitVec 256)) )(let ((?x7304 (storage_s x_0 x_1 x_2 x_3 3 w)))
 (let ((?x10464 (storage_s x_0 x_1 x_2 x_3 4 w)))
 (= ?x10464 ?x7304))))
 ))
 (let (($x656 (forall ((n (_ BitVec 6)) )(let ((?x275 (sc_s 3)))
 (let ((?x10954 (bvadd (_ bv61 6) ?x275)))
 (let (($x9554 (bvsle ?x10954 n)))
 (let ((?x129 (stack_s x_0 x_1 x_2 x_3 3 n)))
 (let ((?x6327 (stack_s x_0 x_1 x_2 x_3 4 n)))
 (or (= ?x6327 ?x129) $x9554)))))))
 ))
 (let (($x9387 (= ?x4305 (bvadd (_ bv1 6) (sc_s 3)))))
 (let ((?x2728 (used_gas_s x_0 x_1 x_2 x_3 4)))
 (let (($x5077 (= ?x2728 (+ 3 (used_gas_s x_0 x_1 x_2 x_3 3)))))
 (let ((?x275 (sc_s 3)))
 (let ((?x11143 (bvadd (_ bv63 6) ?x275)))
 (let ((?x6539 (stack_s x_0 x_1 x_2 x_3 3 ?x11143)))
 (let (($x8498 (= (stack_s x_0 x_1 x_2 x_3 4 ?x11143) ?x6539)))
 (let ((?x7469 (bvadd (_ bv62 6) ?x275)))
 (let ((?x6556 (stack_s x_0 x_1 x_2 x_3 3 ?x7469)))
 (let (($x923 (= (stack_s x_0 x_1 x_2 x_3 4 ?x7469) ?x6556)))
 (let ((?x10954 (bvadd (_ bv61 6) ?x275)))
 (let ((?x8542 (stack_s x_0 x_1 x_2 x_3 3 ?x10954)))
 (let (($x2382 (= (stack_s x_0 x_1 x_2 x_3 4 ?x10954) ?x8542)))
 (let (($x7374 (= ?x5459 ?x8542)))
 (let (($x7610 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 2)))) (_ bv0 1)))))
 (let (($x7316 (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 2))))))
 (let (($x247 (exc_halt_s 2)))
 (let (($x861 (forall ((w (_ BitVec 256)) )(let ((?x10072 (storage_s x_0 x_1 x_2 x_3 2 w)))
 (let ((?x7304 (storage_s x_0 x_1 x_2 x_3 3 w)))
 (= ?x7304 ?x10072))))
 ))
 (let (($x8091 (forall ((n (_ BitVec 6)) )(let ((?x218 (sc_s 2)))
 (let ((?x2626 (bvadd (_ bv62 6) ?x218)))
 (let (($x1116 (bvsle ?x2626 n)))
 (let ((?x8415 (stack_s x_0 x_1 x_2 x_3 2 n)))
 (let ((?x129 (stack_s x_0 x_1 x_2 x_3 3 n)))
 (let (($x838 (= ?x129 ?x8415)))
 (or $x838 $x1116))))))))
 ))
 (let (($x6679 (= ?x275 (bvadd (_ bv1 6) (sc_s 2)))))
 (let ((?x7042 (used_gas_s x_0 x_1 x_2 x_3 3)))
 (let (($x7646 (= ?x7042 (+ 3 (used_gas_s x_0 x_1 x_2 x_3 2)))))
 (let ((?x218 (sc_s 2)))
 (let ((?x10959 (bvadd (_ bv63 6) ?x218)))
 (let ((?x6561 (stack_s x_0 x_1 x_2 x_3 2 ?x10959)))
 (let ((?x2626 (bvadd (_ bv62 6) ?x218)))
 (let ((?x6157 (stack_s x_0 x_1 x_2 x_3 2 ?x2626)))
 (let (($x667 (= $x247 (or (exc_halt_s 1) (not (bvsle (_ bv0 6) (bvadd (_ bv60 6) (sc_s 1))))))))
 (let (($x6553 (forall ((w (_ BitVec 256)) )(let ((?x6580 (storage_s x_0 x_1 x_2 x_3 1 w)))
 (let ((?x10072 (storage_s x_0 x_1 x_2 x_3 2 w)))
 (= ?x10072 ?x6580))))
 ))
 (let (($x8102 (forall ((n (_ BitVec 6)) )(let ((?x10660 (stack_s x_0 x_1 x_2 x_3 1 n)))
 (let ((?x8415 (stack_s x_0 x_1 x_2 x_3 2 n)))
 (or (bvsle (bvadd (_ bv60 6) (sc_s 1)) n) (= ?x8415 ?x10660)))))
 ))
 (let ((?x154 (sc_s 1)))
 (let (($x7228 (= ?x218 ?x154)))
 (let ((?x409 (used_gas_s x_0 x_1 x_2 x_3 2)))
 (let (($x3737 (= ?x409 (+ 3 (used_gas_s x_0 x_1 x_2 x_3 1)))))
 (let ((?x3661 (bvadd (_ bv61 6) ?x154)))
 (let ((?x9649 (stack_s x_0 x_1 x_2 x_3 1 ?x3661)))
 (let ((?x10790 (bvadd (_ bv61 6) ?x218)))
 (let ((?x124 (stack_s x_0 x_1 x_2 x_3 2 ?x10790)))
 (let ((?x11178 (bvadd (_ bv63 6) ?x154)))
 (let ((?x9510 (stack_s x_0 x_1 x_2 x_3 1 ?x11178)))
 (let ((?x6493 (bvadd (_ bv60 6) ?x154)))
 (let ((?x6492 (stack_s x_0 x_1 x_2 x_3 1 ?x6493)))
 (let (($x6590 (= ?x6561 ?x6492)))
 (let (($x9655 (not (bvsle (_ bv0 6) (bvadd (_ bv60 6) ?x72)))))
 (let (($x7020 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x72))) (_ bv0 1)))))
 (let (($x189 (exc_halt_s 1)))
 (let (($x661 (forall ((w (_ BitVec 256)) )(let ((?x10728 (storage_s x_0 x_1 x_2 x_3 0 w)))
 (let ((?x6580 (storage_s x_0 x_1 x_2 x_3 1 w)))
 (= ?x6580 ?x10728))))
 ))
 (let (($x7549 (forall ((n (_ BitVec 6)) )(let ((?x943 (stack_s x_0 x_1 x_2 x_3 0 n)))
 (let ((?x10660 (stack_s x_0 x_1 x_2 x_3 1 n)))
 (or (bvsle (bvadd (_ bv60 6) (sc_s 0)) n) (= ?x10660 ?x943)))))
 ))
 (let (($x10964 (= ?x154 (bvadd (_ bv1 6) ?x72))))
 (let ((?x3560 (used_gas_s x_0 x_1 x_2 x_3 1)))
 (let (($x518 (= ?x3560 (+ 3 ?x10688))))
 (let ((?x10482 (bvadd (_ bv63 6) ?x72)))
 (let ((?x9580 (stack_s x_0 x_1 x_2 x_3 0 ?x10482)))
 (let ((?x11206 (bvadd (_ bv62 6) ?x72)))
 (let ((?x3602 (stack_s x_0 x_1 x_2 x_3 0 ?x11206)))
 (let ((?x7520 (bvadd (_ bv61 6) ?x72)))
 (let ((?x6399 (stack_s x_0 x_1 x_2 x_3 0 ?x7520)))
 (let ((?x7031 (bvadd (_ bv60 6) ?x72)))
 (let ((?x7361 (stack_s x_0 x_1 x_2 x_3 0 ?x7031)))
 (let (($x6441 (= ?x9510 ?x7361)))
 (let (($x9460 (forall ((w (_ BitVec 256)) )(let ((?x10728 (storage_s x_0 x_1 x_2 x_3 0 w)))
 (= ?x10728 (_ bv0 256))))
 ))
 (let (($x6078 (= ?x10688 0)))
 (let (($x11171 (not $x57)))
 (let (($x7001 (= (stack_s x_0 x_1 x_2 x_3 0 (_ bv3 6)) x_3)))
 (let (($x7871 (= (stack_s x_0 x_1 x_2 x_3 0 (_ bv2 6)) x_2)))
 (let (($x3814 (= (stack_s x_0 x_1 x_2 x_3 0 (_ bv1 6)) x_1)))
 (let (($x738 (= (stack_s x_0 x_1 x_2 x_3 0 (_ bv0 6)) x_0)))
 (let (($x4882 (= ?x72 (_ bv4 6))))
 (and $x4882 $x738 $x3814 $x7871 $x7001 $x11171 $x6078 $x9460 $x6441 (= (stack_s x_0 x_1 x_2 x_3 1 ?x7031) ?x7361) (= (stack_s x_0 x_1 x_2 x_3 1 ?x7520) ?x6399) (= (stack_s x_0 x_1 x_2 x_3 1 ?x11206) ?x3602) (= (stack_s x_0 x_1 x_2 x_3 1 ?x10482) ?x9580) $x518 $x10964 $x7549 $x661 (= $x189 (or $x57 $x7020 $x9655)) $x6590 (= (stack_s x_0 x_1 x_2 x_3 2 (bvadd (_ bv60 6) ?x218)) ?x9510) (= ?x124 ?x9649) (= ?x6157 (stack_s x_0 x_1 x_2 x_3 1 (bvadd (_ bv62 6) ?x154))) $x3737 $x7228 $x8102 $x6553 $x667 (= ?x6539 ?x6157) (= (stack_s x_0 x_1 x_2 x_3 3 ?x2626) ?x6157) (= (stack_s x_0 x_1 x_2 x_3 3 ?x10959) ?x6561) $x7646 $x6679 $x8091 $x861 (= $x292 (or $x247 $x7316 $x7610)) $x7374 $x2382 $x923 $x8498 $x5077 $x9387 $x656 $x5392 $x6318 (= ?x10334 ?x2877) (= (stack_s x_0 x_1 x_2 x_3 5 ?x10240) ?x2877) (= (stack_s x_0 x_1 x_2 x_3 5 ?x9411) ?x6342) (= (stack_s x_0 x_1 x_2 x_3 5 ?x803) ?x4765) (= (stack_s x_0 x_1 x_2 x_3 5 ?x10988) ?x5459) $x7696 $x9675 $x10987 $x6837 $x9177 (= ?x6577 ?x6593) (= (stack_s x_0 x_1 x_2 x_3 6 (bvadd (_ bv60 6) ?x926)) ?x10334) (= ?x4063 ?x10177) $x4072 $x6832 $x3879 $x8960 $x9523 $x8502 (= ?x7790 ?x9305) (= (stack_t x_0 x_1 x_2 x_3 1 ?x741) ?x9305) $x367 $x8795 $x2454 $x4773 $x7805 (= ?x7113 ?x3570) $x6191 $x6136 $x6542 $x8461 $x1413 $x7193 (= $x903 (or $x390 (not (bvsle (_ bv0 6) ?x8421)) $x1920)) $x7300 (= (stack_t x_0 x_1 x_2 x_3 3 ?x5336) ?x5306) (= (stack_t x_0 x_1 x_2 x_3 3 ?x6845) ?x7076) (= (stack_t x_0 x_1 x_2 x_3 3 ?x6390) ?x7113) $x6895 $x8282 $x3245 $x7084 (= $x10336 (or $x903 $x6616 $x6776)) (= ?x7302 ?x9026) (= (stack_t x_0 x_1 x_2 x_3 4 ?x10521) ?x9026) $x10537 $x6892 $x2736 $x7585 $x9265 $x423 $x5200 $x6790 $x2628 $x10455 (= $x3723 (or $x6306 (not (bvsle (_ bv0 6) ?x10521)) $x10336)) (= ?x2319 (stack_t x_0 x_1 x_2 x_3 4 (bvadd (_ bv57 6) ?x3757))) (= (stack_t x_0 x_1 x_2 x_3 5 (bvadd (_ bv57 6) ?x919)) ?x7302) (= ?x4901 ?x6040) $x8515 $x3486 $x10573 $x2442 $x2380 $x107 $x8337 $x7164 $x1837 $x73 $x3764 $x58 $x10735 $x10731 $x6907)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
 )
(check-sat)
(get-proof)
