; PUSH cw_1 SWAP2 SWAP1 SWAP2 => SWAP1 PUSH cw_1
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
 (exists ((x_0 (_ BitVec 256)) (x_1 (_ BitVec 256)) )(let (($x7468 (forall ((w (_ BitVec 256)) )(let ((?x4499 (storage_t x_0 x_1 w_1 2 w)))
 (let ((?x2730 (storage_s x_0 x_1 w_1 4 w)))
 (= ?x2730 ?x4499))))
 ))
 (let (($x8186 (exc_halt_t 2)))
 (let (($x9175 (exc_halt_s 4)))
 (let (($x175 (= $x9175 $x8186)))
 (let (($x1074 (forall ((n (_ BitVec 6)) )(let ((?x6180 (stack_t x_0 x_1 w_1 2 n)))
 (let ((?x10792 (stack_s x_0 x_1 w_1 4 n)))
 (let (($x11848 (= ?x10792 ?x6180)))
 (let ((?x11248 (sc_t 2)))
 (let (($x8371 (bvsle ?x11248 n)))
 (or $x8371 $x11848)))))))
 ))
 (let ((?x11248 (sc_t 2)))
 (let ((?x9433 (sc_s 4)))
 (let (($x7802 (= ?x9433 ?x11248)))
 (let ((?x6035 (used_gas_t x_0 x_1 w_1 0)))
 (let ((?x5897 (used_gas_s x_0 x_1 w_1 0)))
 (let (($x3418 (= ?x5897 ?x6035)))
 (let (($x259 (forall ((w (_ BitVec 256)) )(let ((?x566 (storage_t x_0 x_1 w_1 0 w)))
 (let ((?x5291 (storage_s x_0 x_1 w_1 0 w)))
 (= ?x5291 ?x566))))
 ))
 (let (($x56 (exc_halt_t 0)))
 (let (($x57 (exc_halt_s 0)))
 (let (($x58 (= $x57 $x56)))
 (let (($x8025 (forall ((n (_ BitVec 6)) )(let ((?x63 (sc_t 0)))
 (let (($x7659 (bvsle ?x63 n)))
 (let ((?x7154 (stack_t x_0 x_1 w_1 0 n)))
 (let ((?x2246 (stack_s x_0 x_1 w_1 0 n)))
 (let (($x278 (= ?x2246 ?x7154)))
 (or $x278 $x7659)))))))
 ))
 (let ((?x63 (sc_t 0)))
 (let ((?x72 (sc_s 0)))
 (let (($x73 (= ?x72 ?x63)))
 (let (($x5947 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_t 1)))) (_ bv0 1)))))
 (let (($x4852 (exc_halt_t 1)))
 (let (($x11228 (= $x8186 (or $x4852 $x5947))))
 (let (($x245 (forall ((w (_ BitVec 256)) )(let ((?x6033 (storage_t x_0 x_1 w_1 1 w)))
 (let ((?x4499 (storage_t x_0 x_1 w_1 2 w)))
 (= ?x4499 ?x6033))))
 ))
 (let (($x331 (forall ((n (_ BitVec 6)) )(let ((?x6776 (stack_t x_0 x_1 w_1 1 n)))
 (let ((?x6180 (stack_t x_0 x_1 w_1 2 n)))
 (let ((?x9666 (sc_t 1)))
 (let (($x6584 (bvsle ?x9666 n)))
 (or $x6584 (= ?x6180 ?x6776)))))))
 ))
 (let (($x902 (= ?x11248 (bvadd (_ bv1 6) (sc_t 1)))))
 (let ((?x8316 (used_gas_t x_0 x_1 w_1 2)))
 (let (($x5920 (= ?x8316 (+ 3 (used_gas_t x_0 x_1 w_1 1)))))
 (let (($x5592 (= (stack_t x_0 x_1 w_1 2 (sc_t 1)) w_1)))
 (let (($x6760 (forall ((w (_ BitVec 256)) )(let ((?x566 (storage_t x_0 x_1 w_1 0 w)))
 (let ((?x6033 (storage_t x_0 x_1 w_1 1 w)))
 (= ?x6033 ?x566))))
 ))
 (let (($x10396 (forall ((n (_ BitVec 6)) )(let ((?x63 (sc_t 0)))
 (let ((?x8595 (bvadd (_ bv62 6) ?x63)))
 (let (($x9451 (bvsle ?x8595 n)))
 (let ((?x7154 (stack_t x_0 x_1 w_1 0 n)))
 (let ((?x6776 (stack_t x_0 x_1 w_1 1 n)))
 (let (($x70 (= ?x6776 ?x7154)))
 (or $x70 $x9451))))))))
 ))
 (let ((?x10508 (used_gas_t x_0 x_1 w_1 1)))
 (let (($x3868 (= ?x10508 (+ 3 ?x6035))))
 (let ((?x9204 (bvadd (_ bv63 6) ?x63)))
 (let ((?x3421 (stack_t x_0 x_1 w_1 0 ?x9204)))
 (let ((?x8595 (bvadd (_ bv62 6) ?x63)))
 (let ((?x2011 (stack_t x_0 x_1 w_1 0 ?x8595)))
 (let (($x1513 (= (stack_t x_0 x_1 w_1 1 (bvadd (_ bv63 6) (sc_t 1))) ?x2011)))
 (let (($x935 (= $x9175 (or (exc_halt_s 3) (not (bvsle (_ bv0 6) (bvadd (_ bv61 6) (sc_s 3))))))))
 (let (($x10634 (forall ((w (_ BitVec 256)) )(let ((?x6273 (storage_s x_0 x_1 w_1 3 w)))
 (let ((?x2730 (storage_s x_0 x_1 w_1 4 w)))
 (= ?x2730 ?x6273))))
 ))
 (let (($x6533 (forall ((n (_ BitVec 6)) )(let ((?x6935 (stack_s x_0 x_1 w_1 3 n)))
 (let ((?x10792 (stack_s x_0 x_1 w_1 4 n)))
 (let (($x1345 (= ?x10792 ?x6935)))
 (or (bvsle (bvadd (_ bv61 6) (sc_s 3)) n) $x1345)))))
 ))
 (let ((?x3851 (sc_s 3)))
 (let (($x8303 (= ?x9433 ?x3851)))
 (let ((?x8974 (used_gas_s x_0 x_1 w_1 4)))
 (let (($x9410 (= ?x8974 (+ 3 (used_gas_s x_0 x_1 w_1 3)))))
 (let ((?x9007 (bvadd (_ bv62 6) ?x3851)))
 (let ((?x5770 (stack_s x_0 x_1 w_1 3 ?x9007)))
 (let ((?x3168 (bvadd (_ bv62 6) ?x9433)))
 (let ((?x2091 (stack_s x_0 x_1 w_1 4 ?x3168)))
 (let ((?x1770 (bvadd (_ bv63 6) ?x3851)))
 (let ((?x9895 (stack_s x_0 x_1 w_1 3 ?x1770)))
 (let ((?x4784 (bvadd (_ bv63 6) ?x9433)))
 (let ((?x10550 (stack_s x_0 x_1 w_1 4 ?x4784)))
 (let (($x8103 (exc_halt_s 3)))
 (let (($x9252 (= $x8103 (or (exc_halt_s 2) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 2))))))))
 (let (($x9830 (forall ((w (_ BitVec 256)) )(let ((?x7220 (storage_s x_0 x_1 w_1 2 w)))
 (let ((?x6273 (storage_s x_0 x_1 w_1 3 w)))
 (= ?x6273 ?x7220))))
 ))
 (let (($x2780 (forall ((n (_ BitVec 6)) )(let ((?x6610 (stack_s x_0 x_1 w_1 2 n)))
 (let ((?x6935 (stack_s x_0 x_1 w_1 3 n)))
 (let ((?x2272 (sc_s 2)))
 (let ((?x5303 (bvadd (_ bv62 6) ?x2272)))
 (let (($x1494 (bvsle ?x5303 n)))
 (or $x1494 (= ?x6935 ?x6610))))))))
 ))
 (let ((?x2272 (sc_s 2)))
 (let (($x3963 (= ?x3851 ?x2272)))
 (let ((?x6960 (used_gas_s x_0 x_1 w_1 3)))
 (let (($x10534 (= ?x6960 (+ 3 (used_gas_s x_0 x_1 w_1 2)))))
 (let ((?x8276 (bvadd (_ bv63 6) ?x2272)))
 (let ((?x11745 (stack_s x_0 x_1 w_1 2 ?x8276)))
 (let (($x1451 (= ?x5770 ?x11745)))
 (let ((?x5303 (bvadd (_ bv62 6) ?x2272)))
 (let ((?x2322 (stack_s x_0 x_1 w_1 2 ?x5303)))
 (let (($x9723 (= ?x9895 ?x2322)))
 (let (($x10052 (exc_halt_s 2)))
 (let (($x9951 (= $x10052 (or (exc_halt_s 1) (not (bvsle (_ bv0 6) (bvadd (_ bv61 6) (sc_s 1))))))))
 (let (($x1496 (forall ((w (_ BitVec 256)) )(let ((?x3817 (storage_s x_0 x_1 w_1 1 w)))
 (let ((?x7220 (storage_s x_0 x_1 w_1 2 w)))
 (= ?x7220 ?x3817))))
 ))
 (let (($x2945 (forall ((n (_ BitVec 6)) )(let ((?x4466 (stack_s x_0 x_1 w_1 1 n)))
 (let ((?x6610 (stack_s x_0 x_1 w_1 2 n)))
 (let (($x5721 (= ?x6610 ?x4466)))
 (let ((?x154 (sc_s 1)))
 (let ((?x9844 (bvadd (_ bv61 6) ?x154)))
 (let (($x3348 (bvsle ?x9844 n)))
 (or $x3348 $x5721))))))))
 ))
 (let ((?x154 (sc_s 1)))
 (let (($x2298 (= ?x2272 ?x154)))
 (let ((?x9258 (used_gas_s x_0 x_1 w_1 2)))
 (let (($x11786 (= ?x9258 (+ 3 (used_gas_s x_0 x_1 w_1 1)))))
 (let (($x3127 (= ?x2322 (stack_s x_0 x_1 w_1 1 (bvadd (_ bv62 6) ?x154)))))
 (let ((?x11468 (bvadd (_ bv63 6) ?x154)))
 (let ((?x849 (stack_s x_0 x_1 w_1 1 ?x11468)))
 (let (($x1589 (= (stack_s x_0 x_1 w_1 2 (bvadd (_ bv61 6) ?x2272)) ?x849)))
 (let (($x7388 (= ?x11745 (stack_s x_0 x_1 w_1 1 (bvadd (_ bv61 6) ?x154)))))
 (let (($x8780 (exc_halt_s 1)))
 (let (($x8607 (= $x8780 (or $x57 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x72))) (_ bv0 1)))))))
 (let (($x10385 (forall ((w (_ BitVec 256)) )(let ((?x5291 (storage_s x_0 x_1 w_1 0 w)))
 (let ((?x3817 (storage_s x_0 x_1 w_1 1 w)))
 (= ?x3817 ?x5291))))
 ))
 (let (($x8919 (forall ((n (_ BitVec 6)) )(let ((?x2246 (stack_s x_0 x_1 w_1 0 n)))
 (let ((?x4466 (stack_s x_0 x_1 w_1 1 n)))
 (let ((?x72 (sc_s 0)))
 (let (($x949 (bvsle ?x72 n)))
 (or $x949 (= ?x4466 ?x2246)))))))
 ))
 (let (($x6719 (= ?x154 (bvadd (_ bv1 6) ?x72))))
 (let ((?x7399 (used_gas_s x_0 x_1 w_1 1)))
 (let (($x4205 (= ?x7399 (+ 3 ?x5897))))
 (let (($x7269 (= (stack_s x_0 x_1 w_1 1 ?x72) w_1)))
 (let (($x6299 (forall ((w (_ BitVec 256)) )(let ((?x5291 (storage_s x_0 x_1 w_1 0 w)))
 (= ?x5291 (_ bv0 256))))
 ))
 (let (($x10668 (= ?x5897 0)))
 (let (($x6704 (not $x57)))
 (let (($x11096 (= (stack_s x_0 x_1 w_1 0 (_ bv1 6)) x_1)))
 (let (($x11269 (= (stack_s x_0 x_1 w_1 0 (_ bv0 6)) x_0)))
 (let (($x11128 (= ?x72 (_ bv2 6))))
 (and $x11128 $x11269 $x11096 $x6704 $x10668 $x6299 $x7269 $x4205 $x6719 $x8919 $x10385 $x8607 $x7388 $x1589 $x3127 $x11786 $x2298 $x2945 $x1496 $x9951 $x9723 $x1451 $x10534 $x3963 $x2780 $x9830 $x9252 (= ?x10550 (stack_s x_0 x_1 w_1 3 (bvadd (_ bv61 6) ?x3851))) (= (stack_s x_0 x_1 w_1 4 (bvadd (_ bv61 6) ?x9433)) ?x9895) (= ?x2091 ?x5770) $x9410 $x8303 $x6533 $x10634 $x935 $x1513 (= (stack_t x_0 x_1 w_1 1 (bvadd (_ bv62 6) (sc_t 1))) ?x3421) $x3868 (= (sc_t 1) ?x63) $x10396 $x6760 (= $x4852 (or $x56 (not (bvsle (_ bv0 6) ?x8595)))) $x5592 $x5920 $x902 $x331 $x245 $x11228 $x73 $x8025 $x58 $x259 $x3418 (not (and $x7802 $x1074 $x175 $x7468)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
 )
(check-sat)
(get-proof)