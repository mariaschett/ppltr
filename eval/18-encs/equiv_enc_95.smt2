; PUSH cw_6 DUP1 PUSH cw_4 PUSH cw_5 EXP SWAP1 => PUSH cw_6 PUSH cw_4 PUSH cw_5 EXP PUSH cw_6
(set-option :produce-proofs true)
; 
(set-info :status unknown)
(declare-fun storage_t ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 256)) (_ BitVec 256))
(declare-fun w_5 () (_ BitVec 256))
(declare-fun w_4 () (_ BitVec 256))
(declare-fun w_6 () (_ BitVec 256))
(declare-fun storage_s ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 256)) (_ BitVec 256))
(declare-fun exc_halt_t (Int) Bool)
(declare-fun exc_halt_s (Int) Bool)
(declare-fun sc_t (Int) (_ BitVec 6))
(declare-fun stack_t ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 6)) (_ BitVec 256))
(declare-fun stack_s ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 6)) (_ BitVec 256))
(declare-fun sc_s (Int) (_ BitVec 6))
(declare-fun used_gas_t ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int) Int)
(declare-fun used_gas_s ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int) Int)
(declare-fun f_EXP ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256)) (_ BitVec 256))
(assert
 (exists ((x_EXP_0 (_ BitVec 256)) )(let (($x8491 (forall ((w (_ BitVec 256)) )(let ((?x8097 (storage_t w_6 w_4 w_5 x_EXP_0 5 w)))
 (let ((?x8478 (storage_s w_6 w_4 w_5 x_EXP_0 6 w)))
 (= ?x8478 ?x8097))))
 ))
 (let (($x886 (exc_halt_t 5)))
 (let (($x772 (exc_halt_s 6)))
 (let (($x889 (= $x772 $x886)))
 (let (($x3275 (forall ((n (_ BitVec 6)) )(let ((?x919 (sc_t 5)))
 (let (($x5442 (bvsle ?x919 n)))
 (let ((?x4330 (stack_t w_6 w_4 w_5 x_EXP_0 5 n)))
 (let ((?x1022 (stack_s w_6 w_4 w_5 x_EXP_0 6 n)))
 (let (($x3032 (= ?x1022 ?x4330)))
 (or $x3032 $x5442)))))))
 ))
 (let ((?x919 (sc_t 5)))
 (let ((?x926 (sc_s 6)))
 (let (($x929 (= ?x926 ?x919)))
 (let ((?x8529 (used_gas_t w_6 w_4 w_5 x_EXP_0 0)))
 (let ((?x2413 (used_gas_s w_6 w_4 w_5 x_EXP_0 0)))
 (let (($x3019 (= ?x2413 ?x8529)))
 (let (($x2791 (forall ((w (_ BitVec 256)) )(let ((?x2439 (storage_t w_6 w_4 w_5 x_EXP_0 0 w)))
 (let ((?x2900 (storage_s w_6 w_4 w_5 x_EXP_0 0 w)))
 (= ?x2900 ?x2439))))
 ))
 (let (($x56 (exc_halt_t 0)))
 (let (($x57 (exc_halt_s 0)))
 (let (($x58 (= $x57 $x56)))
 (let (($x6931 (forall ((n (_ BitVec 6)) )(let ((?x63 (sc_t 0)))
 (let (($x5858 (bvsle ?x63 n)))
 (let ((?x8418 (stack_t w_6 w_4 w_5 x_EXP_0 0 n)))
 (let ((?x8540 (stack_s w_6 w_4 w_5 x_EXP_0 0 n)))
 (let (($x2854 (= ?x8540 ?x8418)))
 (or $x2854 $x5858)))))))
 ))
 (let ((?x63 (sc_t 0)))
 (let ((?x72 (sc_s 0)))
 (let (($x73 (= ?x72 ?x63)))
 (let (($x3723 (exc_halt_t 4)))
 (let (($x2824 (or $x3723 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_t 4)))) (_ bv0 1))))))
 (let (($x3341 (forall ((w (_ BitVec 256)) )(let ((?x2829 (storage_t w_6 w_4 w_5 x_EXP_0 4 w)))
 (let ((?x8097 (storage_t w_6 w_4 w_5 x_EXP_0 5 w)))
 (= ?x8097 ?x2829))))
 ))
 (let (($x944 (forall ((n (_ BitVec 6)) )(let ((?x3757 (sc_t 4)))
 (let (($x4910 (bvsle ?x3757 n)))
 (let ((?x2625 (stack_t w_6 w_4 w_5 x_EXP_0 4 n)))
 (let ((?x4330 (stack_t w_6 w_4 w_5 x_EXP_0 5 n)))
 (or (= ?x4330 ?x2625) $x4910))))))
 ))
 (let (($x2487 (= (used_gas_t w_6 w_4 w_5 x_EXP_0 5) (+ 3 (used_gas_t w_6 w_4 w_5 x_EXP_0 4)))))
 (let (($x2023 (= $x3723 (or (exc_halt_t 3) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_t 3))))))))
 (let (($x6706 (forall ((w (_ BitVec 256)) )(let ((?x8194 (storage_t w_6 w_4 w_5 x_EXP_0 3 w)))
 (let ((?x2829 (storage_t w_6 w_4 w_5 x_EXP_0 4 w)))
 (= ?x2829 ?x8194))))
 ))
 (let (($x6794 (forall ((n (_ BitVec 6)) )(let ((?x2012 (sc_t 3)))
 (let ((?x2033 (bvadd (_ bv62 6) ?x2012)))
 (let (($x9471 (bvsle ?x2033 n)))
 (let ((?x3343 (stack_t w_6 w_4 w_5 x_EXP_0 3 n)))
 (let ((?x2625 (stack_t w_6 w_4 w_5 x_EXP_0 4 n)))
 (or (= ?x2625 ?x3343) $x9471)))))))
 ))
 (let ((?x2012 (sc_t 3)))
 (let ((?x7515 (bvadd (_ bv63 6) ?x2012)))
 (let ((?x3757 (sc_t 4)))
 (let (($x9473 (= ?x3757 ?x7515)))
 (let ((?x2444 (used_gas_t w_6 w_4 w_5 x_EXP_0 4)))
 (let ((?x3322 (f_EXP w_6 w_4 w_5 x_EXP_0 (stack_t w_6 w_4 w_5 x_EXP_0 3 ?x7515) (stack_t w_6 w_4 w_5 x_EXP_0 3 (bvadd (_ bv62 6) ?x2012)))))
 (let (($x5188 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_t 2)))) (_ bv0 1)))))
 (let (($x903 (exc_halt_t 2)))
 (let (($x10336 (exc_halt_t 3)))
 (let (($x10875 (= $x10336 (or $x903 $x5188))))
 (let (($x3679 (forall ((w (_ BitVec 256)) )(let ((?x8672 (storage_t w_6 w_4 w_5 x_EXP_0 2 w)))
 (let ((?x8194 (storage_t w_6 w_4 w_5 x_EXP_0 3 w)))
 (= ?x8194 ?x8672))))
 ))
 (let (($x7034 (forall ((n (_ BitVec 6)) )(let ((?x2426 (stack_t w_6 w_4 w_5 x_EXP_0 2 n)))
 (let ((?x3343 (stack_t w_6 w_4 w_5 x_EXP_0 3 n)))
 (let ((?x4056 (sc_t 2)))
 (let (($x355 (bvsle ?x4056 n)))
 (or $x355 (= ?x3343 ?x2426)))))))
 ))
 (let (($x5182 (= ?x2012 (bvadd (_ bv1 6) (sc_t 2)))))
 (let ((?x8684 (used_gas_t w_6 w_4 w_5 x_EXP_0 3)))
 (let (($x265 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_t 1)))) (_ bv0 1)))))
 (let (($x1920 (exc_halt_t 1)))
 (let (($x4061 (= $x903 (or $x1920 $x265))))
 (let (($x7102 (forall ((w (_ BitVec 256)) )(let ((?x2783 (storage_t w_6 w_4 w_5 x_EXP_0 1 w)))
 (let ((?x8672 (storage_t w_6 w_4 w_5 x_EXP_0 2 w)))
 (= ?x8672 ?x2783))))
 ))
 (let (($x3674 (forall ((n (_ BitVec 6)) )(let ((?x4023 (sc_t 1)))
 (let (($x10889 (bvsle ?x4023 n)))
 (let ((?x2365 (stack_t w_6 w_4 w_5 x_EXP_0 1 n)))
 (let ((?x2426 (stack_t w_6 w_4 w_5 x_EXP_0 2 n)))
 (or (= ?x2426 ?x2365) $x10889))))))
 ))
 (let ((?x4056 (sc_t 2)))
 (let (($x8278 (= ?x4056 (bvadd (_ bv1 6) (sc_t 1)))))
 (let ((?x8638 (used_gas_t w_6 w_4 w_5 x_EXP_0 2)))
 (let (($x2670 (= $x1920 (or $x56 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x63))) (_ bv0 1)))))))
 (let (($x2567 (forall ((w (_ BitVec 256)) )(let ((?x2439 (storage_t w_6 w_4 w_5 x_EXP_0 0 w)))
 (let ((?x2783 (storage_t w_6 w_4 w_5 x_EXP_0 1 w)))
 (= ?x2783 ?x2439))))
 ))
 (let (($x7219 (forall ((n (_ BitVec 6)) )(let ((?x63 (sc_t 0)))
 (let (($x5858 (bvsle ?x63 n)))
 (let ((?x8418 (stack_t w_6 w_4 w_5 x_EXP_0 0 n)))
 (let ((?x2365 (stack_t w_6 w_4 w_5 x_EXP_0 1 n)))
 (or (= ?x2365 ?x8418) $x5858))))))
 ))
 (let ((?x4023 (sc_t 1)))
 (let (($x5939 (= ?x4023 (bvadd (_ bv1 6) ?x63))))
 (let (($x6068 (= $x772 (or (exc_halt_s 5) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 5))))))))
 (let (($x7223 (forall ((w (_ BitVec 256)) )(let ((?x3111 (storage_s w_6 w_4 w_5 x_EXP_0 5 w)))
 (let ((?x8478 (storage_s w_6 w_4 w_5 x_EXP_0 6 w)))
 (= ?x8478 ?x3111))))
 ))
 (let (($x6261 (forall ((n (_ BitVec 6)) )(let ((?x8769 (stack_s w_6 w_4 w_5 x_EXP_0 5 n)))
 (let ((?x1022 (stack_s w_6 w_4 w_5 x_EXP_0 6 n)))
 (let ((?x805 (sc_s 5)))
 (let ((?x10560 (bvadd (_ bv62 6) ?x805)))
 (let (($x47 (bvsle ?x10560 n)))
 (or $x47 (= ?x1022 ?x8769))))))))
 ))
 (let ((?x805 (sc_s 5)))
 (let (($x517 (= ?x926 ?x805)))
 (let (($x2551 (= (used_gas_s w_6 w_4 w_5 x_EXP_0 6) (+ 3 (used_gas_s w_6 w_4 w_5 x_EXP_0 5)))))
 (let ((?x10905 (bvadd (_ bv63 6) ?x805)))
 (let ((?x4406 (stack_s w_6 w_4 w_5 x_EXP_0 5 ?x10905)))
 (let (($x5627 (= (stack_s w_6 w_4 w_5 x_EXP_0 6 (bvadd (_ bv63 6) ?x926)) (stack_s w_6 w_4 w_5 x_EXP_0 5 (bvadd (_ bv62 6) ?x805)))))
 (let (($x3979 (exc_halt_s 5)))
 (let (($x4660 (= $x3979 (or (exc_halt_s 4) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 4))))))))
 (let (($x2856 (forall ((w (_ BitVec 256)) )(let ((?x2953 (storage_s w_6 w_4 w_5 x_EXP_0 4 w)))
 (let ((?x3111 (storage_s w_6 w_4 w_5 x_EXP_0 5 w)))
 (= ?x3111 ?x2953))))
 ))
 (let (($x2564 (forall ((n (_ BitVec 6)) )(let ((?x6618 (stack_s w_6 w_4 w_5 x_EXP_0 4 n)))
 (let ((?x8769 (stack_s w_6 w_4 w_5 x_EXP_0 5 n)))
 (let ((?x4305 (sc_s 4)))
 (let ((?x10588 (bvadd (_ bv62 6) ?x4305)))
 (let (($x1394 (bvsle ?x10588 n)))
 (or $x1394 (= ?x8769 ?x6618))))))))
 ))
 (let ((?x4305 (sc_s 4)))
 (let ((?x10396 (bvadd (_ bv63 6) ?x4305)))
 (let (($x8926 (= ?x805 ?x10396)))
 (let ((?x8343 (used_gas_s w_6 w_4 w_5 x_EXP_0 5)))
 (let ((?x8299 (f_EXP w_6 w_4 w_5 x_EXP_0 (stack_s w_6 w_4 w_5 x_EXP_0 4 ?x10396) (stack_s w_6 w_4 w_5 x_EXP_0 4 (bvadd (_ bv62 6) ?x4305)))))
 (let (($x10592 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 3)))) (_ bv0 1)))))
 (let (($x292 (exc_halt_s 3)))
 (let (($x64 (exc_halt_s 4)))
 (let (($x10752 (= $x64 (or $x292 $x10592))))
 (let (($x8173 (forall ((w (_ BitVec 256)) )(let ((?x2453 (storage_s w_6 w_4 w_5 x_EXP_0 3 w)))
 (let ((?x2953 (storage_s w_6 w_4 w_5 x_EXP_0 4 w)))
 (= ?x2953 ?x2453))))
 ))
 (let (($x6030 (forall ((n (_ BitVec 6)) )(let ((?x275 (sc_s 3)))
 (let (($x6550 (bvsle ?x275 n)))
 (let ((?x5512 (stack_s w_6 w_4 w_5 x_EXP_0 3 n)))
 (let ((?x6618 (stack_s w_6 w_4 w_5 x_EXP_0 4 n)))
 (or (= ?x6618 ?x5512) $x6550))))))
 ))
 (let (($x10912 (= ?x4305 (bvadd (_ bv1 6) (sc_s 3)))))
 (let ((?x3110 (used_gas_s w_6 w_4 w_5 x_EXP_0 4)))
 (let (($x9170 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 2)))) (_ bv0 1)))))
 (let (($x247 (exc_halt_s 2)))
 (let (($x1452 (= $x292 (or $x247 $x9170))))
 (let (($x5709 (forall ((w (_ BitVec 256)) )(let ((?x2745 (storage_s w_6 w_4 w_5 x_EXP_0 2 w)))
 (let ((?x2453 (storage_s w_6 w_4 w_5 x_EXP_0 3 w)))
 (= ?x2453 ?x2745))))
 ))
 (let (($x2152 (forall ((n (_ BitVec 6)) )(let ((?x218 (sc_s 2)))
 (let (($x6476 (bvsle ?x218 n)))
 (let ((?x5280 (stack_s w_6 w_4 w_5 x_EXP_0 2 n)))
 (let ((?x5512 (stack_s w_6 w_4 w_5 x_EXP_0 3 n)))
 (or (= ?x5512 ?x5280) $x6476))))))
 ))
 (let ((?x275 (sc_s 3)))
 (let (($x3640 (= ?x275 (bvadd (_ bv1 6) (sc_s 2)))))
 (let ((?x2210 (used_gas_s w_6 w_4 w_5 x_EXP_0 3)))
 (let (($x7166 (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_s 1))))))
 (let (($x6375 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 1)))) (_ bv0 1)))))
 (let (($x189 (exc_halt_s 1)))
 (let (($x1515 (= $x247 (or $x189 $x6375 $x7166))))
 (let (($x2992 (forall ((w (_ BitVec 256)) )(let ((?x6222 (storage_s w_6 w_4 w_5 x_EXP_0 1 w)))
 (let ((?x2745 (storage_s w_6 w_4 w_5 x_EXP_0 2 w)))
 (= ?x2745 ?x6222))))
 ))
 (let (($x8553 (forall ((n (_ BitVec 6)) )(let ((?x154 (sc_s 1)))
 (let ((?x5483 (bvadd (_ bv63 6) ?x154)))
 (let (($x8076 (bvsle ?x5483 n)))
 (let ((?x8415 (stack_s w_6 w_4 w_5 x_EXP_0 1 n)))
 (let ((?x5280 (stack_s w_6 w_4 w_5 x_EXP_0 2 n)))
 (or (= ?x5280 ?x8415) $x8076)))))))
 ))
 (let ((?x218 (sc_s 2)))
 (let (($x10982 (= ?x218 (bvadd (_ bv1 6) (sc_s 1)))))
 (let ((?x3020 (used_gas_s w_6 w_4 w_5 x_EXP_0 2)))
 (let ((?x154 (sc_s 1)))
 (let ((?x5483 (bvadd (_ bv63 6) ?x154)))
 (let ((?x3184 (stack_s w_6 w_4 w_5 x_EXP_0 1 ?x5483)))
 (let (($x2046 (= $x189 (or $x57 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x72))) (_ bv0 1)))))))
 (let (($x5878 (forall ((w (_ BitVec 256)) )(let ((?x2900 (storage_s w_6 w_4 w_5 x_EXP_0 0 w)))
 (let ((?x6222 (storage_s w_6 w_4 w_5 x_EXP_0 1 w)))
 (= ?x6222 ?x2900))))
 ))
 (let (($x2944 (forall ((n (_ BitVec 6)) )(let ((?x72 (sc_s 0)))
 (let (($x10975 (bvsle ?x72 n)))
 (let ((?x8540 (stack_s w_6 w_4 w_5 x_EXP_0 0 n)))
 (let ((?x8415 (stack_s w_6 w_4 w_5 x_EXP_0 1 n)))
 (or (= ?x8415 ?x8540) $x10975))))))
 ))
 (let (($x7560 (= ?x154 (bvadd (_ bv1 6) ?x72))))
 (let (($x7388 (forall ((w0 (_ BitVec 256)) (w1 (_ BitVec 256)) )(let (($x3133 (= (stack_s w_6 w_4 w_5 x_EXP_0 4 (bvadd (_ bv62 6) (sc_s 4))) w1)))
 (let (($x5174 (= (stack_s w_6 w_4 w_5 x_EXP_0 4 (bvadd (_ bv63 6) (sc_s 4))) w0)))
 (let ((?x2712 (f_EXP w_6 w_4 w_5 x_EXP_0 w0 w1)))
 (= ?x2712 (ite (and $x5174 $x3133) x_EXP_0 (_ bv0 256)))))))
 ))
 (let (($x3331 (forall ((w (_ BitVec 256)) )(let ((?x2900 (storage_s w_6 w_4 w_5 x_EXP_0 0 w)))
 (= ?x2900 (_ bv0 256))))
 ))
 (let (($x8417 (= ?x2413 0)))
 (let (($x4937 (not $x57)))
 (let (($x136 (= ?x72 (_ bv0 6))))
 (and $x136 $x4937 $x8417 $x3331 $x7388 (= (stack_s w_6 w_4 w_5 x_EXP_0 1 ?x72) w_6) (= (used_gas_s w_6 w_4 w_5 x_EXP_0 1) (+ 3 ?x2413)) $x7560 $x2944 $x5878 $x2046 (= (stack_s w_6 w_4 w_5 x_EXP_0 2 (bvadd (_ bv63 6) ?x218)) ?x3184) (= (stack_s w_6 w_4 w_5 x_EXP_0 2 ?x5483) ?x3184) (= ?x3020 (+ 3 (used_gas_s w_6 w_4 w_5 x_EXP_0 1))) $x10982 $x8553 $x2992 $x1515 (= (stack_s w_6 w_4 w_5 x_EXP_0 3 ?x218) w_4) (= ?x2210 (+ 3 ?x3020)) $x3640 $x2152 $x5709 $x1452 (= (stack_s w_6 w_4 w_5 x_EXP_0 4 ?x275) w_5) (= ?x3110 (+ 3 ?x2210)) $x10912 $x6030 $x8173 $x10752 (= ?x4406 ?x8299) (= ?x8343 (+ 10 ?x3110)) $x8926 $x2564 $x2856 $x4660 $x5627 (= (stack_s w_6 w_4 w_5 x_EXP_0 6 (bvadd (_ bv62 6) ?x926)) ?x4406) $x2551 $x517 $x6261 $x7223 $x6068 (= (stack_t w_6 w_4 w_5 x_EXP_0 1 ?x63) w_6) (= (used_gas_t w_6 w_4 w_5 x_EXP_0 1) (+ 3 ?x8529)) $x5939 $x7219 $x2567 $x2670 (= (stack_t w_6 w_4 w_5 x_EXP_0 2 ?x4023) w_4) (= ?x8638 (+ 3 (used_gas_t w_6 w_4 w_5 x_EXP_0 1))) $x8278 $x3674 $x7102 $x4061 (= (stack_t w_6 w_4 w_5 x_EXP_0 3 ?x4056) w_5) (= ?x8684 (+ 3 ?x8638)) $x5182 $x7034 $x3679 $x10875 (= (stack_t w_6 w_4 w_5 x_EXP_0 4 (bvadd (_ bv63 6) ?x3757)) ?x3322) (= ?x2444 (+ 10 ?x8684)) $x9473 $x6794 $x6706 $x2023 (= (stack_t w_6 w_4 w_5 x_EXP_0 5 ?x3757) w_6) $x2487 (= ?x919 (bvadd (_ bv1 6) ?x3757)) $x944 $x3341 (= $x886 $x2824) $x73 $x6931 $x58 $x2791 $x3019 (not (and $x929 $x3275 $x889 $x8491)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
 )
(check-sat)
(get-proof)
