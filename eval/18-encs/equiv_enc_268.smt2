; CALLER SWAP1 PUSH cw_2 SWAP1 PUSH cw_3 SWAP1 => PUSH cw_2 PUSH cw_3 CALLER SWAP3
(set-option :produce-proofs true)
; 
(set-info :status unknown)
(declare-fun storage_t ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 256)) (_ BitVec 256))
(declare-fun w_3 () (_ BitVec 256))
(declare-fun w_2 () (_ BitVec 256))
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
 (exists ((x_0 (_ BitVec 256)) (x_CALLER (_ BitVec 256)) )(let (($x7430 (forall ((w (_ BitVec 256)) )(let ((?x2676 (storage_t x_0 w_2 w_3 x_CALLER 4 w)))
 (let ((?x7602 (storage_s x_0 w_2 w_3 x_CALLER 6 w)))
 (= ?x7602 ?x2676))))
 ))
 (let (($x3723 (exc_halt_t 4)))
 (let (($x772 (exc_halt_s 6)))
 (let (($x1357 (= $x772 $x3723)))
 (let (($x9208 (forall ((n (_ BitVec 6)) )(let ((?x4107 (stack_t x_0 w_2 w_3 x_CALLER 4 n)))
 (let ((?x7081 (stack_s x_0 w_2 w_3 x_CALLER 6 n)))
 (let (($x8730 (= ?x7081 ?x4107)))
 (let ((?x3757 (sc_t 4)))
 (let (($x8252 (bvsle ?x3757 n)))
 (or $x8252 $x8730)))))))
 ))
 (let ((?x3757 (sc_t 4)))
 (let ((?x926 (sc_s 6)))
 (let (($x5103 (= ?x926 ?x3757)))
 (let ((?x7583 (used_gas_t x_0 w_2 w_3 x_CALLER 0)))
 (let ((?x2456 (used_gas_s x_0 w_2 w_3 x_CALLER 0)))
 (let (($x4846 (= ?x2456 ?x7583)))
 (let (($x5136 (forall ((w (_ BitVec 256)) )(let ((?x11242 (storage_t x_0 w_2 w_3 x_CALLER 0 w)))
 (let ((?x256 (storage_s x_0 w_2 w_3 x_CALLER 0 w)))
 (= ?x256 ?x11242))))
 ))
 (let (($x56 (exc_halt_t 0)))
 (let (($x57 (exc_halt_s 0)))
 (let (($x58 (= $x57 $x56)))
 (let (($x711 (forall ((n (_ BitVec 6)) )(let ((?x2023 (stack_t x_0 w_2 w_3 x_CALLER 0 n)))
 (let ((?x10467 (stack_s x_0 w_2 w_3 x_CALLER 0 n)))
 (let (($x8775 (= ?x10467 ?x2023)))
 (let ((?x63 (sc_t 0)))
 (let (($x4036 (bvsle ?x63 n)))
 (or $x4036 $x8775)))))))
 ))
 (let ((?x63 (sc_t 0)))
 (let ((?x72 (sc_s 0)))
 (let (($x73 (= ?x72 ?x63)))
 (let (($x7135 (= $x3723 (or (exc_halt_t 3) (not (bvsle (_ bv0 6) (bvadd (_ bv60 6) (sc_t 3))))))))
 (let (($x6835 (forall ((w (_ BitVec 256)) )(let ((?x8088 (storage_t x_0 w_2 w_3 x_CALLER 3 w)))
 (let ((?x2676 (storage_t x_0 w_2 w_3 x_CALLER 4 w)))
 (= ?x2676 ?x8088))))
 ))
 (let (($x7463 (forall ((n (_ BitVec 6)) )(let ((?x3552 (stack_t x_0 w_2 w_3 x_CALLER 3 n)))
 (let ((?x4107 (stack_t x_0 w_2 w_3 x_CALLER 4 n)))
 (or (= ?x4107 ?x3552) (bvsle (bvadd (_ bv60 6) (sc_t 3)) n)))))
 ))
 (let ((?x6378 (sc_t 3)))
 (let (($x9328 (= ?x3757 ?x6378)))
 (let (($x7515 (= (used_gas_t x_0 w_2 w_3 x_CALLER 4) (+ 3 (used_gas_t x_0 w_2 w_3 x_CALLER 3)))))
 (let (($x8096 (= (stack_t x_0 w_2 w_3 x_CALLER 4 (bvadd (_ bv62 6) ?x3757)) (stack_t x_0 w_2 w_3 x_CALLER 3 (bvadd (_ bv62 6) ?x6378)))))
 (let (($x2479 (= (stack_t x_0 w_2 w_3 x_CALLER 4 (bvadd (_ bv61 6) ?x3757)) (stack_t x_0 w_2 w_3 x_CALLER 3 (bvadd (_ bv61 6) ?x6378)))))
 (let (($x5703 (= (stack_t x_0 w_2 w_3 x_CALLER 4 (bvadd (_ bv60 6) ?x3757)) (stack_t x_0 w_2 w_3 x_CALLER 3 (bvadd (_ bv63 6) ?x6378)))))
 (let (($x11238 (= (stack_t x_0 w_2 w_3 x_CALLER 4 (bvadd (_ bv63 6) ?x3757)) (stack_t x_0 w_2 w_3 x_CALLER 3 (bvadd (_ bv60 6) ?x6378)))))
 (let (($x11478 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_t 2)))) (_ bv0 1)))))
 (let (($x903 (exc_halt_t 2)))
 (let (($x8009 (exc_halt_t 3)))
 (let (($x7526 (= $x8009 (or $x903 $x11478))))
 (let (($x9379 (forall ((w (_ BitVec 256)) )(let ((?x655 (storage_t x_0 w_2 w_3 x_CALLER 2 w)))
 (let ((?x8088 (storage_t x_0 w_2 w_3 x_CALLER 3 w)))
 (= ?x8088 ?x655))))
 ))
 (let (($x10252 (forall ((n (_ BitVec 6)) )(let ((?x1278 (stack_t x_0 w_2 w_3 x_CALLER 2 n)))
 (let ((?x3552 (stack_t x_0 w_2 w_3 x_CALLER 3 n)))
 (let ((?x4056 (sc_t 2)))
 (let (($x1865 (bvsle ?x4056 n)))
 (or $x1865 (= ?x3552 ?x1278)))))))
 ))
 (let (($x8976 (= ?x6378 (bvadd (_ bv1 6) (sc_t 2)))))
 (let ((?x2886 (used_gas_t x_0 w_2 w_3 x_CALLER 3)))
 (let (($x10206 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_t 1)))) (_ bv0 1)))))
 (let (($x1920 (exc_halt_t 1)))
 (let (($x8504 (= $x903 (or $x1920 $x10206))))
 (let (($x1603 (forall ((w (_ BitVec 256)) )(let ((?x4592 (storage_t x_0 w_2 w_3 x_CALLER 1 w)))
 (let ((?x655 (storage_t x_0 w_2 w_3 x_CALLER 2 w)))
 (= ?x655 ?x4592))))
 ))
 (let (($x5652 (forall ((n (_ BitVec 6)) )(let ((?x4023 (sc_t 1)))
 (let (($x10796 (bvsle ?x4023 n)))
 (let ((?x4749 (stack_t x_0 w_2 w_3 x_CALLER 1 n)))
 (let ((?x1278 (stack_t x_0 w_2 w_3 x_CALLER 2 n)))
 (or (= ?x1278 ?x4749) $x10796))))))
 ))
 (let ((?x4056 (sc_t 2)))
 (let (($x7668 (= ?x4056 (bvadd (_ bv1 6) (sc_t 1)))))
 (let ((?x448 (used_gas_t x_0 w_2 w_3 x_CALLER 2)))
 (let (($x8425 (= $x1920 (or $x56 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x63))) (_ bv0 1)))))))
 (let (($x4687 (forall ((w (_ BitVec 256)) )(let ((?x11242 (storage_t x_0 w_2 w_3 x_CALLER 0 w)))
 (let ((?x4592 (storage_t x_0 w_2 w_3 x_CALLER 1 w)))
 (= ?x4592 ?x11242))))
 ))
 (let (($x3008 (forall ((n (_ BitVec 6)) )(let ((?x2023 (stack_t x_0 w_2 w_3 x_CALLER 0 n)))
 (let ((?x4749 (stack_t x_0 w_2 w_3 x_CALLER 1 n)))
 (let ((?x63 (sc_t 0)))
 (let (($x4036 (bvsle ?x63 n)))
 (or $x4036 (= ?x4749 ?x2023)))))))
 ))
 (let ((?x4023 (sc_t 1)))
 (let (($x7805 (= ?x4023 (bvadd (_ bv1 6) ?x63))))
 (let (($x2362 (= $x772 (or (exc_halt_s 5) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 5))))))))
 (let (($x8660 (forall ((w (_ BitVec 256)) )(let ((?x6741 (storage_s x_0 w_2 w_3 x_CALLER 5 w)))
 (let ((?x7602 (storage_s x_0 w_2 w_3 x_CALLER 6 w)))
 (= ?x7602 ?x6741))))
 ))
 (let (($x1347 (forall ((n (_ BitVec 6)) )(let ((?x805 (sc_s 5)))
 (let ((?x798 (bvadd (_ bv62 6) ?x805)))
 (let (($x10739 (bvsle ?x798 n)))
 (let ((?x10464 (stack_s x_0 w_2 w_3 x_CALLER 5 n)))
 (let ((?x7081 (stack_s x_0 w_2 w_3 x_CALLER 6 n)))
 (or (= ?x7081 ?x10464) $x10739)))))))
 ))
 (let ((?x805 (sc_s 5)))
 (let (($x421 (= ?x926 ?x805)))
 (let (($x1270 (= (used_gas_s x_0 w_2 w_3 x_CALLER 6) (+ 3 (used_gas_s x_0 w_2 w_3 x_CALLER 5)))))
 (let (($x2111 (= (stack_s x_0 w_2 w_3 x_CALLER 6 (bvadd (_ bv62 6) ?x926)) (stack_s x_0 w_2 w_3 x_CALLER 5 (bvadd (_ bv63 6) ?x805)))))
 (let (($x5166 (= (stack_s x_0 w_2 w_3 x_CALLER 6 (bvadd (_ bv63 6) ?x926)) (stack_s x_0 w_2 w_3 x_CALLER 5 (bvadd (_ bv62 6) ?x805)))))
 (let (($x2721 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 4)))) (_ bv0 1)))))
 (let (($x64 (exc_halt_s 4)))
 (let (($x3979 (exc_halt_s 5)))
 (let (($x11623 (= $x3979 (or $x64 $x2721))))
 (let (($x6687 (forall ((w (_ BitVec 256)) )(let ((?x9640 (storage_s x_0 w_2 w_3 x_CALLER 4 w)))
 (let ((?x6741 (storage_s x_0 w_2 w_3 x_CALLER 5 w)))
 (= ?x6741 ?x9640))))
 ))
 (let (($x7172 (forall ((n (_ BitVec 6)) )(let ((?x8463 (stack_s x_0 w_2 w_3 x_CALLER 4 n)))
 (let ((?x10464 (stack_s x_0 w_2 w_3 x_CALLER 5 n)))
 (let ((?x4305 (sc_s 4)))
 (let (($x6116 (bvsle ?x4305 n)))
 (or $x6116 (= ?x10464 ?x8463)))))))
 ))
 (let (($x7084 (= ?x805 (bvadd (_ bv1 6) (sc_s 4)))))
 (let ((?x7496 (used_gas_s x_0 w_2 w_3 x_CALLER 5)))
 (let (($x8448 (= $x64 (or (exc_halt_s 3) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 3))))))))
 (let (($x8387 (forall ((w (_ BitVec 256)) )(let ((?x6114 (storage_s x_0 w_2 w_3 x_CALLER 3 w)))
 (let ((?x9640 (storage_s x_0 w_2 w_3 x_CALLER 4 w)))
 (= ?x9640 ?x6114))))
 ))
 (let (($x10619 (forall ((n (_ BitVec 6)) )(let ((?x7620 (stack_s x_0 w_2 w_3 x_CALLER 3 n)))
 (let ((?x8463 (stack_s x_0 w_2 w_3 x_CALLER 4 n)))
 (let ((?x275 (sc_s 3)))
 (let ((?x10770 (bvadd (_ bv62 6) ?x275)))
 (let (($x6326 (bvsle ?x10770 n)))
 (or $x6326 (= ?x8463 ?x7620))))))))
 ))
 (let ((?x275 (sc_s 3)))
 (let ((?x4305 (sc_s 4)))
 (let (($x2249 (= ?x4305 ?x275)))
 (let ((?x8776 (used_gas_s x_0 w_2 w_3 x_CALLER 4)))
 (let (($x5847 (= (stack_s x_0 w_2 w_3 x_CALLER 4 (bvadd (_ bv62 6) ?x4305)) (stack_s x_0 w_2 w_3 x_CALLER 3 (bvadd (_ bv63 6) ?x275)))))
 (let (($x5901 (= (stack_s x_0 w_2 w_3 x_CALLER 4 (bvadd (_ bv63 6) ?x4305)) (stack_s x_0 w_2 w_3 x_CALLER 3 (bvadd (_ bv62 6) ?x275)))))
 (let (($x247 (exc_halt_s 2)))
 (let (($x7270 (or $x247 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 2)))) (_ bv0 1))))))
 (let (($x292 (exc_halt_s 3)))
 (let (($x5809 (= $x292 $x7270)))
 (let (($x1447 (forall ((w (_ BitVec 256)) )(let ((?x7368 (storage_s x_0 w_2 w_3 x_CALLER 2 w)))
 (let ((?x6114 (storage_s x_0 w_2 w_3 x_CALLER 3 w)))
 (= ?x6114 ?x7368))))
 ))
 (let (($x10766 (forall ((n (_ BitVec 6)) )(let ((?x218 (sc_s 2)))
 (let (($x10650 (bvsle ?x218 n)))
 (let ((?x754 (stack_s x_0 w_2 w_3 x_CALLER 2 n)))
 (let ((?x7620 (stack_s x_0 w_2 w_3 x_CALLER 3 n)))
 (or (= ?x7620 ?x754) $x10650))))))
 ))
 (let (($x7531 (= ?x275 (bvadd (_ bv1 6) (sc_s 2)))))
 (let ((?x10602 (used_gas_s x_0 w_2 w_3 x_CALLER 3)))
 (let (($x6805 (= $x247 (or (exc_halt_s 1) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 1))))))))
 (let (($x2930 (forall ((w (_ BitVec 256)) )(let ((?x9120 (storage_s x_0 w_2 w_3 x_CALLER 1 w)))
 (let ((?x7368 (storage_s x_0 w_2 w_3 x_CALLER 2 w)))
 (= ?x7368 ?x9120))))
 ))
 (let (($x7619 (forall ((n (_ BitVec 6)) )(let ((?x10004 (stack_s x_0 w_2 w_3 x_CALLER 1 n)))
 (let ((?x754 (stack_s x_0 w_2 w_3 x_CALLER 2 n)))
 (or (= ?x754 ?x10004) (bvsle (bvadd (_ bv62 6) (sc_s 1)) n)))))
 ))
 (let ((?x154 (sc_s 1)))
 (let ((?x218 (sc_s 2)))
 (let (($x2333 (= ?x218 ?x154)))
 (let ((?x8139 (used_gas_s x_0 w_2 w_3 x_CALLER 2)))
 (let (($x4525 (= (stack_s x_0 w_2 w_3 x_CALLER 2 (bvadd (_ bv62 6) ?x218)) (stack_s x_0 w_2 w_3 x_CALLER 1 (bvadd (_ bv63 6) ?x154)))))
 (let (($x1616 (= (stack_s x_0 w_2 w_3 x_CALLER 2 (bvadd (_ bv63 6) ?x218)) (stack_s x_0 w_2 w_3 x_CALLER 1 (bvadd (_ bv62 6) ?x154)))))
 (let (($x189 (exc_halt_s 1)))
 (let (($x4103 (= $x189 (or $x57 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x72))) (_ bv0 1)))))))
 (let (($x9737 (forall ((w (_ BitVec 256)) )(let ((?x256 (storage_s x_0 w_2 w_3 x_CALLER 0 w)))
 (let ((?x9120 (storage_s x_0 w_2 w_3 x_CALLER 1 w)))
 (= ?x9120 ?x256))))
 ))
 (let (($x34 (forall ((n (_ BitVec 6)) )(let ((?x10467 (stack_s x_0 w_2 w_3 x_CALLER 0 n)))
 (let ((?x10004 (stack_s x_0 w_2 w_3 x_CALLER 1 n)))
 (let ((?x72 (sc_s 0)))
 (let (($x2948 (bvsle ?x72 n)))
 (or $x2948 (= ?x10004 ?x10467)))))))
 ))
 (let (($x6822 (= ?x154 (bvadd (_ bv1 6) ?x72))))
 (let (($x2031 (forall ((w (_ BitVec 256)) )(let ((?x256 (storage_s x_0 w_2 w_3 x_CALLER 0 w)))
 (= ?x256 (_ bv0 256))))
 ))
 (let (($x6997 (= ?x2456 0)))
 (let (($x4562 (not $x57)))
 (let (($x7864 (= (stack_s x_0 w_2 w_3 x_CALLER 0 (_ bv0 6)) x_0)))
 (let (($x8685 (= ?x72 (_ bv1 6))))
 (and $x8685 $x7864 $x4562 $x6997 $x2031 (= (stack_s x_0 w_2 w_3 x_CALLER 1 ?x72) x_CALLER) (= (used_gas_s x_0 w_2 w_3 x_CALLER 1) (+ 2 ?x2456)) $x6822 $x34 $x9737 $x4103 $x1616 $x4525 (= ?x8139 (+ 3 (used_gas_s x_0 w_2 w_3 x_CALLER 1))) $x2333 $x7619 $x2930 $x6805 (= (stack_s x_0 w_2 w_3 x_CALLER 3 ?x218) w_2) (= ?x10602 (+ 3 ?x8139)) $x7531 $x10766 $x1447 $x5809 $x5901 $x5847 (= ?x8776 (+ 3 ?x10602)) $x2249 $x10619 $x8387 $x8448 (= (stack_s x_0 w_2 w_3 x_CALLER 5 ?x4305) w_3) (= ?x7496 (+ 3 ?x8776)) $x7084 $x7172 $x6687 $x11623 $x5166 $x2111 $x1270 $x421 $x1347 $x8660 $x2362 (= (stack_t x_0 w_2 w_3 x_CALLER 1 ?x63) w_2) (= (used_gas_t x_0 w_2 w_3 x_CALLER 1) (+ 3 ?x7583)) $x7805 $x3008 $x4687 $x8425 (= (stack_t x_0 w_2 w_3 x_CALLER 2 ?x4023) w_3) (= ?x448 (+ 3 (used_gas_t x_0 w_2 w_3 x_CALLER 1))) $x7668 $x5652 $x1603 $x8504 (= (stack_t x_0 w_2 w_3 x_CALLER 3 ?x4056) x_CALLER) (= ?x2886 (+ 2 ?x448)) $x8976 $x10252 $x9379 $x7526 $x11238 $x5703 $x2479 $x8096 $x7515 $x9328 $x7463 $x6835 $x7135 $x73 $x711 $x58 $x5136 $x4846 (not (and $x5103 $x9208 $x1357 $x7430))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
 )
(check-sat)
(get-proof)