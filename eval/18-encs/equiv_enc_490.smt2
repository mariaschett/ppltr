; DUP1 MLOAD PUSH cw_1 DUP4 ADD SWAP1 => PUSH cw_1 DUP3 ADD DUP2 MLOAD
(set-option :produce-proofs true)
; 
(set-info :status unknown)
(declare-fun storage_t ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 256)) (_ BitVec 256))
(declare-fun w_1 () (_ BitVec 256))
(declare-fun storage_s ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 256)) (_ BitVec 256))
(declare-fun exc_halt_t (Int) Bool)
(declare-fun exc_halt_s (Int) Bool)
(declare-fun stack_t ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 6)) (_ BitVec 256))
(declare-fun stack_s ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 6)) (_ BitVec 256))
(declare-fun sc_t (Int) (_ BitVec 6))
(declare-fun sc_s (Int) (_ BitVec 6))
(declare-fun used_gas_t ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int) Int)
(declare-fun used_gas_s ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int) Int)
(declare-fun f_MLOAD ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256)) (_ BitVec 256))
(assert
 (exists ((x_0 (_ BitVec 256)) (x_1 (_ BitVec 256)) (x_MLOAD_0 (_ BitVec 256)) )(let (($x4245 (forall ((w (_ BitVec 256)) )(let ((?x1953 (storage_t x_0 x_1 w_1 x_MLOAD_0 5 w)))
 (let ((?x1874 (storage_s x_0 x_1 w_1 x_MLOAD_0 6 w)))
 (= ?x1874 ?x1953))))
 ))
 (let (($x886 (exc_halt_t 5)))
 (let (($x772 (exc_halt_s 6)))
 (let (($x889 (= $x772 $x886)))
 (let (($x1965 (forall ((n (_ BitVec 6)) )(let ((?x4213 (stack_t x_0 x_1 w_1 x_MLOAD_0 5 n)))
 (let ((?x2160 (stack_s x_0 x_1 w_1 x_MLOAD_0 6 n)))
 (let (($x9342 (= ?x2160 ?x4213)))
 (let ((?x919 (sc_t 5)))
 (let (($x7452 (bvsle ?x919 n)))
 (or $x7452 $x9342)))))))
 ))
 (let ((?x919 (sc_t 5)))
 (let ((?x926 (sc_s 6)))
 (let (($x929 (= ?x926 ?x919)))
 (let ((?x5195 (used_gas_t x_0 x_1 w_1 x_MLOAD_0 0)))
 (let ((?x10016 (used_gas_s x_0 x_1 w_1 x_MLOAD_0 0)))
 (let (($x10155 (= ?x10016 ?x5195)))
 (let (($x7011 (forall ((w (_ BitVec 256)) )(let ((?x9191 (storage_t x_0 x_1 w_1 x_MLOAD_0 0 w)))
 (let ((?x10537 (storage_s x_0 x_1 w_1 x_MLOAD_0 0 w)))
 (= ?x10537 ?x9191))))
 ))
 (let (($x56 (exc_halt_t 0)))
 (let (($x57 (exc_halt_s 0)))
 (let (($x58 (= $x57 $x56)))
 (let (($x6357 (forall ((n (_ BitVec 6)) )(let ((?x63 (sc_t 0)))
 (let (($x4790 (bvsle ?x63 n)))
 (let ((?x9995 (stack_t x_0 x_1 w_1 x_MLOAD_0 0 n)))
 (let ((?x9704 (stack_s x_0 x_1 w_1 x_MLOAD_0 0 n)))
 (let (($x719 (= ?x9704 ?x9995)))
 (or $x719 $x4790)))))))
 ))
 (let ((?x63 (sc_t 0)))
 (let ((?x72 (sc_s 0)))
 (let (($x73 (= ?x72 ?x63)))
 (let (($x1743 (= $x886 (or (exc_halt_t 4) (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_t 4))))))))
 (let (($x5617 (forall ((w (_ BitVec 256)) )(let ((?x11126 (storage_t x_0 x_1 w_1 x_MLOAD_0 4 w)))
 (let ((?x1953 (storage_t x_0 x_1 w_1 x_MLOAD_0 5 w)))
 (= ?x1953 ?x11126))))
 ))
 (let (($x7070 (forall ((n (_ BitVec 6)) )(let ((?x7427 (stack_t x_0 x_1 w_1 x_MLOAD_0 4 n)))
 (let ((?x4213 (stack_t x_0 x_1 w_1 x_MLOAD_0 5 n)))
 (let ((?x11631 (sc_t 4)))
 (let ((?x4972 (bvadd (_ bv63 6) ?x11631)))
 (let (($x832 (bvsle ?x4972 n)))
 (or $x832 (= ?x4213 ?x7427))))))))
 ))
 (let ((?x11631 (sc_t 4)))
 (let (($x5137 (= ?x919 ?x11631)))
 (let (($x7327 (= (used_gas_t x_0 x_1 w_1 x_MLOAD_0 5) (+ 3 (used_gas_t x_0 x_1 w_1 x_MLOAD_0 4)))))
 (let ((?x4972 (bvadd (_ bv63 6) ?x11631)))
 (let ((?x3639 (stack_t x_0 x_1 w_1 x_MLOAD_0 4 ?x4972)))
 (let (($x8181 (= (stack_t x_0 x_1 w_1 x_MLOAD_0 5 (bvadd (_ bv63 6) ?x919)) (f_MLOAD x_0 x_1 w_1 x_MLOAD_0 ?x3639))))
 (let (($x6783 (exc_halt_t 3)))
 (let (($x1866 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_t 3)))) (_ bv0 1)))))
 (let (($x774 (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_t 3))))))
 (let (($x7722 (exc_halt_t 4)))
 (let (($x6738 (= $x7722 (or $x774 $x1866 $x6783))))
 (let (($x3583 (forall ((w (_ BitVec 256)) )(let ((?x8672 (storage_t x_0 x_1 w_1 x_MLOAD_0 3 w)))
 (let ((?x11126 (storage_t x_0 x_1 w_1 x_MLOAD_0 4 w)))
 (= ?x11126 ?x8672))))
 ))
 (let (($x5365 (forall ((n (_ BitVec 6)) )(let ((?x6438 (sc_t 3)))
 (let ((?x7878 (bvadd (_ bv62 6) ?x6438)))
 (let (($x7227 (bvsle ?x7878 n)))
 (let ((?x3499 (stack_t x_0 x_1 w_1 x_MLOAD_0 3 n)))
 (let ((?x7427 (stack_t x_0 x_1 w_1 x_MLOAD_0 4 n)))
 (let (($x6235 (= ?x7427 ?x3499)))
 (or $x6235 $x7227))))))))
 ))
 (let (($x6249 (= ?x11631 (bvadd (_ bv1 6) (sc_t 3)))))
 (let ((?x10062 (used_gas_t x_0 x_1 w_1 x_MLOAD_0 4)))
 (let (($x6964 (= ?x10062 (+ 3 (used_gas_t x_0 x_1 w_1 x_MLOAD_0 3)))))
 (let ((?x6438 (sc_t 3)))
 (let ((?x6568 (bvadd (_ bv63 6) ?x6438)))
 (let ((?x7314 (stack_t x_0 x_1 w_1 x_MLOAD_0 3 ?x6568)))
 (let (($x8218 (= (stack_t x_0 x_1 w_1 x_MLOAD_0 4 ?x6568) ?x7314)))
 (let ((?x7878 (bvadd (_ bv62 6) ?x6438)))
 (let ((?x4383 (stack_t x_0 x_1 w_1 x_MLOAD_0 3 ?x7878)))
 (let (($x1917 (= (stack_t x_0 x_1 w_1 x_MLOAD_0 4 ?x7878) ?x4383)))
 (let (($x657 (= $x6783 (or (exc_halt_t 2) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_t 2))))))))
 (let (($x11222 (forall ((w (_ BitVec 256)) )(let ((?x10764 (storage_t x_0 x_1 w_1 x_MLOAD_0 2 w)))
 (let ((?x8672 (storage_t x_0 x_1 w_1 x_MLOAD_0 3 w)))
 (= ?x8672 ?x10764))))
 ))
 (let (($x7366 (forall ((n (_ BitVec 6)) )(let ((?x2714 (sc_t 2)))
 (let ((?x3686 (bvadd (_ bv62 6) ?x2714)))
 (let (($x6499 (bvsle ?x3686 n)))
 (let ((?x6636 (stack_t x_0 x_1 w_1 x_MLOAD_0 2 n)))
 (let ((?x3499 (stack_t x_0 x_1 w_1 x_MLOAD_0 3 n)))
 (let (($x71 (= ?x3499 ?x6636)))
 (or $x71 $x6499))))))))
 ))
 (let ((?x2714 (sc_t 2)))
 (let ((?x4833 (bvadd (_ bv63 6) ?x2714)))
 (let (($x10533 (= ?x6438 ?x4833)))
 (let ((?x1184 (used_gas_t x_0 x_1 w_1 x_MLOAD_0 3)))
 (let (($x519 (= ?x1184 (+ 3 (used_gas_t x_0 x_1 w_1 x_MLOAD_0 2)))))
 (let ((?x1364 (stack_t x_0 x_1 w_1 x_MLOAD_0 2 ?x4833)))
 (let (($x4578 (= ?x7314 (bvadd ?x1364 (stack_t x_0 x_1 w_1 x_MLOAD_0 2 (bvadd (_ bv62 6) ?x2714))))))
 (let (($x9876 (not (bvsle (_ bv0 6) (bvadd (_ bv61 6) (sc_t 1))))))
 (let (($x3854 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_t 1)))) (_ bv0 1)))))
 (let (($x3508 (exc_halt_t 1)))
 (let (($x2163 (exc_halt_t 2)))
 (let (($x4331 (= $x2163 (or $x3508 $x3854 $x9876))))
 (let (($x9038 (forall ((w (_ BitVec 256)) )(let ((?x5958 (storage_t x_0 x_1 w_1 x_MLOAD_0 1 w)))
 (let ((?x10764 (storage_t x_0 x_1 w_1 x_MLOAD_0 2 w)))
 (= ?x10764 ?x5958))))
 ))
 (let (($x6729 (forall ((n (_ BitVec 6)) )(let ((?x8347 (sc_t 1)))
 (let ((?x3171 (bvadd (_ bv61 6) ?x8347)))
 (let (($x6358 (bvsle ?x3171 n)))
 (let ((?x6513 (stack_t x_0 x_1 w_1 x_MLOAD_0 1 n)))
 (let ((?x6636 (stack_t x_0 x_1 w_1 x_MLOAD_0 2 n)))
 (let (($x2186 (= ?x6636 ?x6513)))
 (or $x2186 $x6358))))))))
 ))
 (let (($x1950 (= ?x2714 (bvadd (_ bv1 6) (sc_t 1)))))
 (let ((?x369 (used_gas_t x_0 x_1 w_1 x_MLOAD_0 2)))
 (let (($x632 (= ?x369 (+ 3 (used_gas_t x_0 x_1 w_1 x_MLOAD_0 1)))))
 (let ((?x8347 (sc_t 1)))
 (let ((?x9514 (bvadd (_ bv63 6) ?x8347)))
 (let ((?x5927 (stack_t x_0 x_1 w_1 x_MLOAD_0 1 ?x9514)))
 (let (($x9062 (= (stack_t x_0 x_1 w_1 x_MLOAD_0 2 ?x9514) ?x5927)))
 (let (($x143 (= (stack_t x_0 x_1 w_1 x_MLOAD_0 2 (bvadd (_ bv62 6) ?x8347)) (stack_t x_0 x_1 w_1 x_MLOAD_0 1 (bvadd (_ bv62 6) ?x8347)))))
 (let ((?x3171 (bvadd (_ bv61 6) ?x8347)))
 (let ((?x9626 (stack_t x_0 x_1 w_1 x_MLOAD_0 1 ?x3171)))
 (let (($x9644 (= (stack_t x_0 x_1 w_1 x_MLOAD_0 2 ?x3171) ?x9626)))
 (let (($x7741 (= ?x1364 ?x9626)))
 (let (($x2506 (= $x3508 (or $x56 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x63))) (_ bv0 1)))))))
 (let (($x4762 (forall ((w (_ BitVec 256)) )(let ((?x9191 (storage_t x_0 x_1 w_1 x_MLOAD_0 0 w)))
 (let ((?x5958 (storage_t x_0 x_1 w_1 x_MLOAD_0 1 w)))
 (= ?x5958 ?x9191))))
 ))
 (let (($x11219 (forall ((n (_ BitVec 6)) )(let ((?x63 (sc_t 0)))
 (let (($x4790 (bvsle ?x63 n)))
 (let ((?x9995 (stack_t x_0 x_1 w_1 x_MLOAD_0 0 n)))
 (let ((?x6513 (stack_t x_0 x_1 w_1 x_MLOAD_0 1 n)))
 (let (($x1511 (= ?x6513 ?x9995)))
 (or $x1511 $x4790)))))))
 ))
 (let (($x7449 (= ?x8347 (bvadd (_ bv1 6) ?x63))))
 (let ((?x6391 (used_gas_t x_0 x_1 w_1 x_MLOAD_0 1)))
 (let (($x5934 (= ?x6391 (+ 3 ?x5195))))
 (let (($x10009 (= (stack_t x_0 x_1 w_1 x_MLOAD_0 1 ?x63) w_1)))
 (let (($x1001 (= $x772 (or (exc_halt_s 5) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 5))))))))
 (let (($x11368 (forall ((w (_ BitVec 256)) )(let ((?x2842 (storage_s x_0 x_1 w_1 x_MLOAD_0 5 w)))
 (let ((?x1874 (storage_s x_0 x_1 w_1 x_MLOAD_0 6 w)))
 (= ?x1874 ?x2842))))
 ))
 (let (($x7285 (forall ((n (_ BitVec 6)) )(let ((?x4319 (sc_s 5)))
 (let ((?x3094 (bvadd (_ bv62 6) ?x4319)))
 (let (($x9914 (bvsle ?x3094 n)))
 (let ((?x225 (stack_s x_0 x_1 w_1 x_MLOAD_0 5 n)))
 (let ((?x2160 (stack_s x_0 x_1 w_1 x_MLOAD_0 6 n)))
 (or (= ?x2160 ?x225) $x9914)))))))
 ))
 (let ((?x4319 (sc_s 5)))
 (let (($x1302 (= ?x926 ?x4319)))
 (let (($x1548 (= (used_gas_s x_0 x_1 w_1 x_MLOAD_0 6) (+ 3 (used_gas_s x_0 x_1 w_1 x_MLOAD_0 5)))))
 (let ((?x3388 (bvadd (_ bv63 6) ?x4319)))
 (let ((?x9675 (stack_s x_0 x_1 w_1 x_MLOAD_0 5 ?x3388)))
 (let (($x5553 (= (stack_s x_0 x_1 w_1 x_MLOAD_0 6 (bvadd (_ bv62 6) ?x926)) ?x9675)))
 (let (($x2259 (= (stack_s x_0 x_1 w_1 x_MLOAD_0 6 (bvadd (_ bv63 6) ?x926)) (stack_s x_0 x_1 w_1 x_MLOAD_0 5 (bvadd (_ bv62 6) ?x4319)))))
 (let (($x11317 (exc_halt_s 5)))
 (let (($x108 (= $x11317 (or (exc_halt_s 4) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 4))))))))
 (let (($x8400 (forall ((w (_ BitVec 256)) )(let ((?x9896 (storage_s x_0 x_1 w_1 x_MLOAD_0 4 w)))
 (let ((?x2842 (storage_s x_0 x_1 w_1 x_MLOAD_0 5 w)))
 (= ?x2842 ?x9896))))
 ))
 (let (($x8385 (forall ((n (_ BitVec 6)) )(let ((?x4936 (stack_s x_0 x_1 w_1 x_MLOAD_0 4 n)))
 (let ((?x225 (stack_s x_0 x_1 w_1 x_MLOAD_0 5 n)))
 (let (($x7757 (= ?x225 ?x4936)))
 (let ((?x4305 (sc_s 4)))
 (let ((?x11568 (bvadd (_ bv62 6) ?x4305)))
 (let (($x573 (bvsle ?x11568 n)))
 (or $x573 $x7757))))))))
 ))
 (let ((?x4305 (sc_s 4)))
 (let ((?x10159 (bvadd (_ bv63 6) ?x4305)))
 (let (($x129 (= ?x4319 ?x10159)))
 (let ((?x8482 (used_gas_s x_0 x_1 w_1 x_MLOAD_0 5)))
 (let (($x7458 (= ?x8482 (+ 3 (used_gas_s x_0 x_1 w_1 x_MLOAD_0 4)))))
 (let ((?x11568 (bvadd (_ bv62 6) ?x4305)))
 (let ((?x4660 (stack_s x_0 x_1 w_1 x_MLOAD_0 4 ?x11568)))
 (let ((?x7262 (stack_s x_0 x_1 w_1 x_MLOAD_0 4 ?x10159)))
 (let (($x908 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 3)))) (_ bv0 1)))))
 (let (($x292 (exc_halt_s 3)))
 (let (($x7172 (exc_halt_s 4)))
 (let (($x2732 (forall ((w (_ BitVec 256)) )(let ((?x1084 (storage_s x_0 x_1 w_1 x_MLOAD_0 3 w)))
 (let ((?x9896 (storage_s x_0 x_1 w_1 x_MLOAD_0 4 w)))
 (= ?x9896 ?x1084))))
 ))
 (let (($x11558 (forall ((n (_ BitVec 6)) )(let ((?x86 (stack_s x_0 x_1 w_1 x_MLOAD_0 3 n)))
 (let ((?x4936 (stack_s x_0 x_1 w_1 x_MLOAD_0 4 n)))
 (let (($x8560 (= ?x4936 ?x86)))
 (or (bvsle (bvadd (_ bv60 6) (sc_s 3)) n) $x8560)))))
 ))
 (let (($x10718 (= ?x4305 (bvadd (_ bv1 6) (sc_s 3)))))
 (let ((?x9185 (used_gas_s x_0 x_1 w_1 x_MLOAD_0 4)))
 (let (($x7853 (= ?x9185 (+ 3 (used_gas_s x_0 x_1 w_1 x_MLOAD_0 3)))))
 (let ((?x275 (sc_s 3)))
 (let ((?x9670 (bvadd (_ bv63 6) ?x275)))
 (let ((?x10038 (stack_s x_0 x_1 w_1 x_MLOAD_0 3 ?x9670)))
 (let (($x11043 (= (stack_s x_0 x_1 w_1 x_MLOAD_0 4 ?x9670) ?x10038)))
 (let ((?x10426 (bvadd (_ bv62 6) ?x275)))
 (let ((?x11004 (stack_s x_0 x_1 w_1 x_MLOAD_0 3 ?x10426)))
 (let (($x8863 (= (stack_s x_0 x_1 w_1 x_MLOAD_0 4 (bvadd (_ bv61 6) ?x275)) (stack_s x_0 x_1 w_1 x_MLOAD_0 3 (bvadd (_ bv61 6) ?x275)))))
 (let ((?x1213 (bvadd (_ bv60 6) ?x275)))
 (let ((?x5514 (stack_s x_0 x_1 w_1 x_MLOAD_0 3 ?x1213)))
 (let (($x247 (exc_halt_s 2)))
 (let (($x10424 (or $x247 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 2)))) (_ bv0 1))))))
 (let (($x3505 (= $x292 $x10424)))
 (let (($x3812 (forall ((w (_ BitVec 256)) )(let ((?x1748 (storage_s x_0 x_1 w_1 x_MLOAD_0 2 w)))
 (let ((?x1084 (storage_s x_0 x_1 w_1 x_MLOAD_0 3 w)))
 (= ?x1084 ?x1748))))
 ))
 (let (($x5749 (forall ((n (_ BitVec 6)) )(let ((?x218 (sc_s 2)))
 (let (($x6191 (bvsle ?x218 n)))
 (let ((?x6571 (stack_s x_0 x_1 w_1 x_MLOAD_0 2 n)))
 (let ((?x86 (stack_s x_0 x_1 w_1 x_MLOAD_0 3 n)))
 (let (($x5056 (= ?x86 ?x6571)))
 (or $x5056 $x6191)))))))
 ))
 (let (($x11700 (= ?x275 (bvadd (_ bv1 6) (sc_s 2)))))
 (let ((?x6692 (used_gas_s x_0 x_1 w_1 x_MLOAD_0 3)))
 (let (($x5223 (= ?x6692 (+ 3 (used_gas_s x_0 x_1 w_1 x_MLOAD_0 2)))))
 (let (($x5502 (= $x247 (or (exc_halt_s 1) (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_s 1))))))))
 (let (($x849 (forall ((w (_ BitVec 256)) )(let ((?x8926 (storage_s x_0 x_1 w_1 x_MLOAD_0 1 w)))
 (let ((?x1748 (storage_s x_0 x_1 w_1 x_MLOAD_0 2 w)))
 (= ?x1748 ?x8926))))
 ))
 (let (($x4096 (forall ((n (_ BitVec 6)) )(let ((?x154 (sc_s 1)))
 (let ((?x634 (bvadd (_ bv63 6) ?x154)))
 (let (($x7540 (bvsle ?x634 n)))
 (let ((?x707 (stack_s x_0 x_1 w_1 x_MLOAD_0 1 n)))
 (let ((?x6571 (stack_s x_0 x_1 w_1 x_MLOAD_0 2 n)))
 (let (($x2091 (= ?x6571 ?x707)))
 (or $x2091 $x7540))))))))
 ))
 (let ((?x154 (sc_s 1)))
 (let ((?x218 (sc_s 2)))
 (let (($x1181 (= ?x218 ?x154)))
 (let ((?x8924 (used_gas_s x_0 x_1 w_1 x_MLOAD_0 2)))
 (let (($x1110 (= ?x8924 (+ 3 (used_gas_s x_0 x_1 w_1 x_MLOAD_0 1)))))
 (let ((?x634 (bvadd (_ bv63 6) ?x154)))
 (let ((?x5578 (stack_s x_0 x_1 w_1 x_MLOAD_0 1 ?x634)))
 (let ((?x7280 (bvadd (_ bv63 6) ?x218)))
 (let ((?x11632 (stack_s x_0 x_1 w_1 x_MLOAD_0 2 ?x7280)))
 (let (($x9588 (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) ?x72)))))
 (let (($x1483 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x72))) (_ bv0 1)))))
 (let (($x189 (exc_halt_s 1)))
 (let (($x2767 (forall ((w (_ BitVec 256)) )(let ((?x10537 (storage_s x_0 x_1 w_1 x_MLOAD_0 0 w)))
 (let ((?x8926 (storage_s x_0 x_1 w_1 x_MLOAD_0 1 w)))
 (= ?x8926 ?x10537))))
 ))
 (let (($x8574 (forall ((n (_ BitVec 6)) )(let ((?x72 (sc_s 0)))
 (let ((?x10931 (bvadd (_ bv63 6) ?x72)))
 (let (($x11794 (bvsle ?x10931 n)))
 (let ((?x9704 (stack_s x_0 x_1 w_1 x_MLOAD_0 0 n)))
 (let ((?x707 (stack_s x_0 x_1 w_1 x_MLOAD_0 1 n)))
 (let (($x8862 (= ?x707 ?x9704)))
 (or $x8862 $x11794))))))))
 ))
 (let (($x11645 (= ?x154 (bvadd (_ bv1 6) ?x72))))
 (let ((?x3900 (used_gas_s x_0 x_1 w_1 x_MLOAD_0 1)))
 (let (($x743 (= ?x3900 (+ 3 ?x10016))))
 (let ((?x10931 (bvadd (_ bv63 6) ?x72)))
 (let ((?x8953 (stack_s x_0 x_1 w_1 x_MLOAD_0 0 ?x10931)))
 (let (($x8504 (= (stack_s x_0 x_1 w_1 x_MLOAD_0 1 ?x10931) ?x8953)))
 (let (($x2903 (forall ((w0 (_ BitVec 256)) )(let (($x4940 (= (stack_s x_0 x_1 w_1 x_MLOAD_0 1 (bvadd (_ bv63 6) (sc_s 1))) w0)))
 (let ((?x7038 (f_MLOAD x_0 x_1 w_1 x_MLOAD_0 w0)))
 (= ?x7038 (ite $x4940 x_MLOAD_0 (_ bv0 256))))))
 ))
 (let (($x4109 (forall ((w (_ BitVec 256)) )(let ((?x10537 (storage_s x_0 x_1 w_1 x_MLOAD_0 0 w)))
 (= ?x10537 (_ bv0 256))))
 ))
 (let (($x4366 (= ?x10016 0)))
 (let (($x11450 (not $x57)))
 (let (($x9472 (= (stack_s x_0 x_1 w_1 x_MLOAD_0 0 (_ bv1 6)) x_1)))
 (let (($x668 (= (stack_s x_0 x_1 w_1 x_MLOAD_0 0 (_ bv0 6)) x_0)))
 (let (($x11128 (= ?x72 (_ bv2 6))))
 (and $x11128 $x668 $x9472 $x11450 $x4366 $x4109 $x2903 (= ?x5578 ?x8953) $x8504 $x743 $x11645 $x8574 $x2767 (= $x189 (or $x57 $x1483 $x9588)) (= ?x11632 (f_MLOAD x_0 x_1 w_1 x_MLOAD_0 ?x5578)) $x1110 $x1181 $x4096 $x849 $x5502 (= (stack_s x_0 x_1 w_1 x_MLOAD_0 3 ?x218) w_1) $x5223 $x11700 $x5749 $x3812 $x3505 (= ?x7262 ?x5514) (= (stack_s x_0 x_1 w_1 x_MLOAD_0 4 ?x1213) ?x5514) $x8863 (= (stack_s x_0 x_1 w_1 x_MLOAD_0 4 ?x10426) ?x11004) $x11043 $x7853 $x10718 $x11558 $x2732 (= $x7172 (or $x292 $x908 (not (bvsle (_ bv0 6) ?x1213)))) (= ?x9675 (bvadd ?x7262 ?x4660)) $x7458 $x129 $x8385 $x8400 $x108 $x2259 $x5553 $x1548 $x1302 $x7285 $x11368 $x1001 $x10009 $x5934 $x7449 $x11219 $x4762 $x2506 $x7741 $x9644 $x143 $x9062 $x632 $x1950 $x6729 $x9038 $x4331 $x4578 $x519 $x10533 $x7366 $x11222 $x657 (= ?x3639 ?x4383) $x1917 $x8218 $x6964 $x6249 $x5365 $x3583 $x6738 $x8181 $x7327 $x5137 $x7070 $x5617 $x1743 $x73 $x6357 $x58 $x7011 $x10155 (not (and $x929 $x1965 $x889 $x4245))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
 )
(check-sat)
(get-proof)
