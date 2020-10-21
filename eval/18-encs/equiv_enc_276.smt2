; DUP1 PUSH cw_2 PUSH cw_3 SWAP1 SWAP2 => PUSH cw_2 PUSH cw_3 DUP3
(set-option :produce-proofs true)
; 
(set-info :status unknown)
(declare-fun storage_t ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 256)) (_ BitVec 256))
(declare-fun w_3 () (_ BitVec 256))
(declare-fun w_2 () (_ BitVec 256))
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
 (exists ((x_0 (_ BitVec 256)) )(let (($x743 (forall ((w (_ BitVec 256)) )(let ((?x4327 (storage_t x_0 w_2 w_3 3 w)))
 (let ((?x1756 (storage_s x_0 w_2 w_3 5 w)))
 (= ?x1756 ?x4327))))
 ))
 (let (($x8009 (exc_halt_t 3)))
 (let (($x3979 (exc_halt_s 5)))
 (let (($x777 (= $x3979 $x8009)))
 (let (($x675 (forall ((n (_ BitVec 6)) )(let ((?x6378 (sc_t 3)))
 (let (($x9619 (bvsle ?x6378 n)))
 (let ((?x2083 (stack_t x_0 w_2 w_3 3 n)))
 (let ((?x2631 (stack_s x_0 w_2 w_3 5 n)))
 (let (($x1289 (= ?x2631 ?x2083)))
 (or $x1289 $x9619)))))))
 ))
 (let ((?x6378 (sc_t 3)))
 (let ((?x805 (sc_s 5)))
 (let (($x11568 (= ?x805 ?x6378)))
 (let ((?x6512 (used_gas_t x_0 w_2 w_3 0)))
 (let ((?x657 (used_gas_s x_0 w_2 w_3 0)))
 (let (($x5860 (= ?x657 ?x6512)))
 (let (($x7841 (forall ((w (_ BitVec 256)) )(let ((?x2092 (storage_t x_0 w_2 w_3 0 w)))
 (let ((?x7199 (storage_s x_0 w_2 w_3 0 w)))
 (= ?x7199 ?x2092))))
 ))
 (let (($x56 (exc_halt_t 0)))
 (let (($x57 (exc_halt_s 0)))
 (let (($x58 (= $x57 $x56)))
 (let (($x9040 (forall ((n (_ BitVec 6)) )(let ((?x1781 (stack_t x_0 w_2 w_3 0 n)))
 (let ((?x9192 (stack_s x_0 w_2 w_3 0 n)))
 (let (($x6156 (= ?x9192 ?x1781)))
 (let ((?x63 (sc_t 0)))
 (let (($x4036 (bvsle ?x63 n)))
 (or $x4036 $x6156)))))))
 ))
 (let ((?x63 (sc_t 0)))
 (let ((?x72 (sc_s 0)))
 (let (($x73 (= ?x72 ?x63)))
 (let (($x11478 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_t 2)))) (_ bv0 1)))))
 (let (($x903 (exc_halt_t 2)))
 (let (($x5410 (forall ((w (_ BitVec 256)) )(let ((?x9979 (storage_t x_0 w_2 w_3 2 w)))
 (let ((?x4327 (storage_t x_0 w_2 w_3 3 w)))
 (= ?x4327 ?x9979))))
 ))
 (let (($x5614 (forall ((n (_ BitVec 6)) )(let ((?x3909 (stack_t x_0 w_2 w_3 2 n)))
 (let ((?x2083 (stack_t x_0 w_2 w_3 3 n)))
 (or (bvsle (bvadd (_ bv61 6) (sc_t 2)) n) (= ?x2083 ?x3909)))))
 ))
 (let (($x8976 (= ?x6378 (bvadd (_ bv1 6) (sc_t 2)))))
 (let (($x3935 (= (used_gas_t x_0 w_2 w_3 3) (+ 3 (used_gas_t x_0 w_2 w_3 2)))))
 (let (($x473 (= (stack_t x_0 w_2 w_3 3 (bvadd (_ bv63 6) (sc_t 2))) (stack_t x_0 w_2 w_3 2 (bvadd (_ bv63 6) (sc_t 2))))))
 (let (($x11537 (= (stack_t x_0 w_2 w_3 3 (bvadd (_ bv62 6) (sc_t 2))) (stack_t x_0 w_2 w_3 2 (bvadd (_ bv62 6) (sc_t 2))))))
 (let ((?x4056 (sc_t 2)))
 (let ((?x2319 (bvadd (_ bv61 6) ?x4056)))
 (let ((?x6536 (stack_t x_0 w_2 w_3 2 ?x2319)))
 (let (($x10206 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_t 1)))) (_ bv0 1)))))
 (let (($x1920 (exc_halt_t 1)))
 (let (($x8504 (= $x903 (or $x1920 $x10206))))
 (let (($x11285 (forall ((w (_ BitVec 256)) )(let ((?x7930 (storage_t x_0 w_2 w_3 1 w)))
 (let ((?x9979 (storage_t x_0 w_2 w_3 2 w)))
 (= ?x9979 ?x7930))))
 ))
 (let (($x1269 (forall ((n (_ BitVec 6)) )(let ((?x4023 (sc_t 1)))
 (let (($x10796 (bvsle ?x4023 n)))
 (let ((?x8875 (stack_t x_0 w_2 w_3 1 n)))
 (let ((?x3909 (stack_t x_0 w_2 w_3 2 n)))
 (or (= ?x3909 ?x8875) $x10796))))))
 ))
 (let (($x7668 (= ?x4056 (bvadd (_ bv1 6) (sc_t 1)))))
 (let ((?x3085 (used_gas_t x_0 w_2 w_3 2)))
 (let (($x8425 (= $x1920 (or $x56 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x63))) (_ bv0 1)))))))
 (let (($x641 (forall ((w (_ BitVec 256)) )(let ((?x2092 (storage_t x_0 w_2 w_3 0 w)))
 (let ((?x7930 (storage_t x_0 w_2 w_3 1 w)))
 (= ?x7930 ?x2092))))
 ))
 (let (($x1617 (forall ((n (_ BitVec 6)) )(let ((?x1781 (stack_t x_0 w_2 w_3 0 n)))
 (let ((?x8875 (stack_t x_0 w_2 w_3 1 n)))
 (let ((?x63 (sc_t 0)))
 (let (($x4036 (bvsle ?x63 n)))
 (or $x4036 (= ?x8875 ?x1781)))))))
 ))
 (let ((?x4023 (sc_t 1)))
 (let (($x7805 (= ?x4023 (bvadd (_ bv1 6) ?x63))))
 (let (($x8001 (= $x3979 (or (exc_halt_s 4) (not (bvsle (_ bv0 6) (bvadd (_ bv61 6) (sc_s 4))))))))
 (let (($x5428 (forall ((w (_ BitVec 256)) )(let ((?x9064 (storage_s x_0 w_2 w_3 4 w)))
 (let ((?x1756 (storage_s x_0 w_2 w_3 5 w)))
 (= ?x1756 ?x9064))))
 ))
 (let (($x2310 (forall ((n (_ BitVec 6)) )(let ((?x4305 (sc_s 4)))
 (let ((?x4447 (bvadd (_ bv61 6) ?x4305)))
 (let (($x10724 (bvsle ?x4447 n)))
 (let ((?x9015 (stack_s x_0 w_2 w_3 4 n)))
 (let ((?x2631 (stack_s x_0 w_2 w_3 5 n)))
 (or (= ?x2631 ?x9015) $x10724)))))))
 ))
 (let ((?x4305 (sc_s 4)))
 (let (($x5649 (= ?x805 ?x4305)))
 (let (($x2085 (= (used_gas_s x_0 w_2 w_3 5) (+ 3 (used_gas_s x_0 w_2 w_3 4)))))
 (let ((?x6694 (bvadd (_ bv62 6) ?x4305)))
 (let ((?x6649 (stack_s x_0 w_2 w_3 4 ?x6694)))
 (let ((?x7778 (bvadd (_ bv63 6) ?x4305)))
 (let ((?x2828 (stack_s x_0 w_2 w_3 4 ?x7778)))
 (let (($x7769 (= (stack_s x_0 w_2 w_3 5 (bvadd (_ bv63 6) ?x805)) (stack_s x_0 w_2 w_3 4 (bvadd (_ bv61 6) ?x4305)))))
 (let (($x64 (exc_halt_s 4)))
 (let (($x8448 (= $x64 (or (exc_halt_s 3) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 3))))))))
 (let (($x10486 (forall ((w (_ BitVec 256)) )(let ((?x7229 (storage_s x_0 w_2 w_3 3 w)))
 (let ((?x9064 (storage_s x_0 w_2 w_3 4 w)))
 (= ?x9064 ?x7229))))
 ))
 (let (($x5372 (forall ((n (_ BitVec 6)) )(let ((?x275 (sc_s 3)))
 (let ((?x10770 (bvadd (_ bv62 6) ?x275)))
 (let (($x6326 (bvsle ?x10770 n)))
 (let ((?x1864 (stack_s x_0 w_2 w_3 3 n)))
 (let ((?x9015 (stack_s x_0 w_2 w_3 4 n)))
 (or (= ?x9015 ?x1864) $x6326)))))))
 ))
 (let ((?x275 (sc_s 3)))
 (let (($x2249 (= ?x4305 ?x275)))
 (let ((?x2366 (used_gas_s x_0 w_2 w_3 4)))
 (let (($x8415 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 2)))) (_ bv0 1)))))
 (let (($x247 (exc_halt_s 2)))
 (let (($x292 (exc_halt_s 3)))
 (let (($x5809 (= $x292 (or $x247 $x8415))))
 (let (($x10341 (forall ((w (_ BitVec 256)) )(let ((?x2335 (storage_s x_0 w_2 w_3 2 w)))
 (let ((?x7229 (storage_s x_0 w_2 w_3 3 w)))
 (= ?x7229 ?x2335))))
 ))
 (let (($x9193 (forall ((n (_ BitVec 6)) )(let ((?x218 (sc_s 2)))
 (let (($x10650 (bvsle ?x218 n)))
 (let ((?x2053 (stack_s x_0 w_2 w_3 2 n)))
 (let ((?x1864 (stack_s x_0 w_2 w_3 3 n)))
 (or (= ?x1864 ?x2053) $x10650))))))
 ))
 (let (($x7531 (= ?x275 (bvadd (_ bv1 6) (sc_s 2)))))
 (let ((?x5155 (used_gas_s x_0 w_2 w_3 3)))
 (let (($x9045 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 1)))) (_ bv0 1)))))
 (let (($x189 (exc_halt_s 1)))
 (let (($x4238 (= $x247 (or $x189 $x9045))))
 (let (($x3376 (forall ((w (_ BitVec 256)) )(let ((?x1393 (storage_s x_0 w_2 w_3 1 w)))
 (let ((?x2335 (storage_s x_0 w_2 w_3 2 w)))
 (= ?x2335 ?x1393))))
 ))
 (let (($x6962 (forall ((n (_ BitVec 6)) )(let ((?x8518 (stack_s x_0 w_2 w_3 1 n)))
 (let ((?x2053 (stack_s x_0 w_2 w_3 2 n)))
 (let ((?x154 (sc_s 1)))
 (let (($x402 (bvsle ?x154 n)))
 (or $x402 (= ?x2053 ?x8518)))))))
 ))
 (let ((?x218 (sc_s 2)))
 (let (($x2524 (= ?x218 (bvadd (_ bv1 6) (sc_s 1)))))
 (let ((?x182 (used_gas_s x_0 w_2 w_3 2)))
 (let (($x6779 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x72))) (_ bv0 1)))))
 (let (($x3660 (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) ?x72)))))
 (let (($x3329 (= $x189 (or $x57 $x3660 $x6779))))
 (let (($x1995 (forall ((w (_ BitVec 256)) )(let ((?x7199 (storage_s x_0 w_2 w_3 0 w)))
 (let ((?x1393 (storage_s x_0 w_2 w_3 1 w)))
 (= ?x1393 ?x7199))))
 ))
 (let (($x9012 (forall ((n (_ BitVec 6)) )(let ((?x72 (sc_s 0)))
 (let ((?x5742 (bvadd (_ bv63 6) ?x72)))
 (let (($x9929 (bvsle ?x5742 n)))
 (let ((?x9192 (stack_s x_0 w_2 w_3 0 n)))
 (let ((?x8518 (stack_s x_0 w_2 w_3 1 n)))
 (or (= ?x8518 ?x9192) $x9929)))))))
 ))
 (let ((?x154 (sc_s 1)))
 (let (($x6822 (= ?x154 (bvadd (_ bv1 6) ?x72))))
 (let ((?x5742 (bvadd (_ bv63 6) ?x72)))
 (let ((?x5512 (stack_s x_0 w_2 w_3 0 ?x5742)))
 (let (($x8021 (forall ((w (_ BitVec 256)) )(let ((?x7199 (storage_s x_0 w_2 w_3 0 w)))
 (= ?x7199 (_ bv0 256))))
 ))
 (let (($x4362 (= ?x657 0)))
 (let (($x4562 (not $x57)))
 (let (($x11409 (= (stack_s x_0 w_2 w_3 0 (_ bv0 6)) x_0)))
 (let (($x8685 (= ?x72 (_ bv1 6))))
 (and $x8685 $x11409 $x4562 $x4362 $x8021 (= (stack_s x_0 w_2 w_3 1 (bvadd (_ bv63 6) ?x154)) ?x5512) (= (stack_s x_0 w_2 w_3 1 ?x5742) ?x5512) (= (used_gas_s x_0 w_2 w_3 1) (+ 3 ?x657)) $x6822 $x9012 $x1995 $x3329 (= (stack_s x_0 w_2 w_3 2 ?x154) w_2) (= ?x182 (+ 3 (used_gas_s x_0 w_2 w_3 1))) $x2524 $x6962 $x3376 $x4238 (= (stack_s x_0 w_2 w_3 3 ?x218) w_3) (= ?x5155 (+ 3 ?x182)) $x7531 $x9193 $x10341 $x5809 (= ?x2828 (stack_s x_0 w_2 w_3 3 (bvadd (_ bv62 6) ?x275))) (= ?x6649 (stack_s x_0 w_2 w_3 3 (bvadd (_ bv63 6) ?x275))) (= ?x2366 (+ 3 ?x5155)) $x2249 $x5372 $x10486 $x8448 $x7769 (= (stack_s x_0 w_2 w_3 5 (bvadd (_ bv61 6) ?x805)) ?x2828) (= (stack_s x_0 w_2 w_3 5 (bvadd (_ bv62 6) ?x805)) ?x6649) $x2085 $x5649 $x2310 $x5428 $x8001 (= (stack_t x_0 w_2 w_3 1 ?x63) w_2) (= (used_gas_t x_0 w_2 w_3 1) (+ 3 ?x6512)) $x7805 $x1617 $x641 $x8425 (= (stack_t x_0 w_2 w_3 2 ?x4023) w_3) (= ?x3085 (+ 3 (used_gas_t x_0 w_2 w_3 1))) $x7668 $x1269 $x11285 $x8504 (= (stack_t x_0 w_2 w_3 3 (bvadd (_ bv63 6) ?x6378)) ?x6536) (= (stack_t x_0 w_2 w_3 3 ?x2319) ?x6536) $x11537 $x473 $x3935 $x8976 $x5614 $x5410 (= $x8009 (or $x903 (not (bvsle (_ bv0 6) ?x2319)) $x11478)) $x73 $x9040 $x58 $x7841 $x5860 (not (and $x11568 $x675 $x777 $x743))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
 )
(check-sat)
(get-proof)
