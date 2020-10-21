; DUP2 MLOAD SWAP2 POP PUSH cw_2 PUSH cw_3 => PUSH cw_2 PUSH cw_3 SWAP3 MLOAD SWAP3
(set-option :produce-proofs true)
; 
(set-info :status unknown)
(declare-fun storage_t ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 256)) (_ BitVec 256))
(declare-fun w_3 () (_ BitVec 256))
(declare-fun w_2 () (_ BitVec 256))
(declare-fun storage_s ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 256)) (_ BitVec 256))
(declare-fun exc_halt_t (Int) Bool)
(declare-fun exc_halt_s (Int) Bool)
(declare-fun sc_t (Int) (_ BitVec 6))
(declare-fun stack_t ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 6)) (_ BitVec 256))
(declare-fun stack_s ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 6)) (_ BitVec 256))
(declare-fun sc_s (Int) (_ BitVec 6))
(declare-fun used_gas_t ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int) Int)
(declare-fun used_gas_s ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int) Int)
(declare-fun f_MLOAD ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256)) (_ BitVec 256))
(assert
 (exists ((x_0 (_ BitVec 256)) (x_1 (_ BitVec 256)) (x_MLOAD_0 (_ BitVec 256)) )(let (($x144 (forall ((w (_ BitVec 256)) )(let ((?x10852 (storage_t x_0 x_1 w_2 w_3 x_MLOAD_0 5 w)))
 (let ((?x10856 (storage_s x_0 x_1 w_2 w_3 x_MLOAD_0 6 w)))
 (= ?x10856 ?x10852))))
 ))
 (let (($x886 (exc_halt_t 5)))
 (let (($x772 (exc_halt_s 6)))
 (let (($x889 (= $x772 $x886)))
 (let (($x5625 (forall ((n (_ BitVec 6)) )(let ((?x919 (sc_t 5)))
 (let (($x7410 (bvsle ?x919 n)))
 (let ((?x1863 (stack_t x_0 x_1 w_2 w_3 x_MLOAD_0 5 n)))
 (let ((?x4769 (stack_s x_0 x_1 w_2 w_3 x_MLOAD_0 6 n)))
 (let (($x312 (= ?x4769 ?x1863)))
 (or $x312 $x7410)))))))
 ))
 (let ((?x919 (sc_t 5)))
 (let ((?x926 (sc_s 6)))
 (let (($x929 (= ?x926 ?x919)))
 (let ((?x4656 (used_gas_t x_0 x_1 w_2 w_3 x_MLOAD_0 0)))
 (let ((?x4621 (used_gas_s x_0 x_1 w_2 w_3 x_MLOAD_0 0)))
 (let (($x4320 (= ?x4621 ?x4656)))
 (let (($x4750 (forall ((w (_ BitVec 256)) )(let ((?x4747 (storage_t x_0 x_1 w_2 w_3 x_MLOAD_0 0 w)))
 (let ((?x664 (storage_s x_0 x_1 w_2 w_3 x_MLOAD_0 0 w)))
 (= ?x664 ?x4747))))
 ))
 (let (($x56 (exc_halt_t 0)))
 (let (($x57 (exc_halt_s 0)))
 (let (($x58 (= $x57 $x56)))
 (let (($x7733 (forall ((n (_ BitVec 6)) )(let ((?x4609 (stack_t x_0 x_1 w_2 w_3 x_MLOAD_0 0 n)))
 (let ((?x6212 (stack_s x_0 x_1 w_2 w_3 x_MLOAD_0 0 n)))
 (let (($x8203 (= ?x6212 ?x4609)))
 (let ((?x63 (sc_t 0)))
 (let (($x8037 (bvsle ?x63 n)))
 (or $x8037 $x8203)))))))
 ))
 (let ((?x63 (sc_t 0)))
 (let ((?x72 (sc_s 0)))
 (let (($x73 (= ?x72 ?x63)))
 (let (($x9194 (= $x886 (or (exc_halt_t 4) (not (bvsle (_ bv0 6) (bvadd (_ bv60 6) (sc_t 4))))))))
 (let (($x7488 (forall ((w (_ BitVec 256)) )(let ((?x1363 (storage_t x_0 x_1 w_2 w_3 x_MLOAD_0 4 w)))
 (let ((?x10852 (storage_t x_0 x_1 w_2 w_3 x_MLOAD_0 5 w)))
 (= ?x10852 ?x1363))))
 ))
 (let (($x2822 (forall ((n (_ BitVec 6)) )(let ((?x10826 (stack_t x_0 x_1 w_2 w_3 x_MLOAD_0 4 n)))
 (let ((?x1863 (stack_t x_0 x_1 w_2 w_3 x_MLOAD_0 5 n)))
 (or (bvsle (bvadd (_ bv60 6) (sc_t 4)) n) (= ?x1863 ?x10826)))))
 ))
 (let ((?x3757 (sc_t 4)))
 (let (($x8813 (= ?x919 ?x3757)))
 (let (($x9796 (= (used_gas_t x_0 x_1 w_2 w_3 x_MLOAD_0 5) (+ 3 (used_gas_t x_0 x_1 w_2 w_3 x_MLOAD_0 4)))))
 (let (($x3720 (= (stack_t x_0 x_1 w_2 w_3 x_MLOAD_0 5 (bvadd (_ bv62 6) ?x919)) (stack_t x_0 x_1 w_2 w_3 x_MLOAD_0 4 (bvadd (_ bv62 6) ?x3757)))))
 (let (($x1173 (= (stack_t x_0 x_1 w_2 w_3 x_MLOAD_0 5 (bvadd (_ bv61 6) ?x919)) (stack_t x_0 x_1 w_2 w_3 x_MLOAD_0 4 (bvadd (_ bv61 6) ?x3757)))))
 (let ((?x8247 (bvadd (_ bv63 6) ?x3757)))
 (let ((?x3584 (stack_t x_0 x_1 w_2 w_3 x_MLOAD_0 4 ?x8247)))
 (let (($x6803 (= (stack_t x_0 x_1 w_2 w_3 x_MLOAD_0 5 (bvadd (_ bv60 6) ?x919)) ?x3584)))
 (let (($x8017 (= (stack_t x_0 x_1 w_2 w_3 x_MLOAD_0 5 (bvadd (_ bv63 6) ?x919)) (stack_t x_0 x_1 w_2 w_3 x_MLOAD_0 4 (bvadd (_ bv60 6) ?x3757)))))
 (let (($x3723 (exc_halt_t 4)))
 (let (($x637 (= $x3723 (or (exc_halt_t 3) (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_t 3))))))))
 (let (($x8188 (forall ((w (_ BitVec 256)) )(let ((?x4962 (storage_t x_0 x_1 w_2 w_3 x_MLOAD_0 3 w)))
 (let ((?x1363 (storage_t x_0 x_1 w_2 w_3 x_MLOAD_0 4 w)))
 (= ?x1363 ?x4962))))
 ))
 (let (($x9398 (forall ((n (_ BitVec 6)) )(let ((?x2012 (sc_t 3)))
 (let ((?x8856 (bvadd (_ bv63 6) ?x2012)))
 (let (($x8005 (bvsle ?x8856 n)))
 (let ((?x10144 (stack_t x_0 x_1 w_2 w_3 x_MLOAD_0 3 n)))
 (let ((?x10826 (stack_t x_0 x_1 w_2 w_3 x_MLOAD_0 4 n)))
 (or (= ?x10826 ?x10144) $x8005)))))))
 ))
 (let ((?x2012 (sc_t 3)))
 (let (($x5385 (= ?x3757 ?x2012)))
 (let ((?x5133 (used_gas_t x_0 x_1 w_2 w_3 x_MLOAD_0 4)))
 (let ((?x8856 (bvadd (_ bv63 6) ?x2012)))
 (let ((?x2529 (stack_t x_0 x_1 w_2 w_3 x_MLOAD_0 3 ?x8856)))
 (let (($x10336 (exc_halt_t 3)))
 (let (($x7770 (= $x10336 (or (exc_halt_t 2) (not (bvsle (_ bv0 6) (bvadd (_ bv60 6) (sc_t 2))))))))
 (let (($x4221 (forall ((w (_ BitVec 256)) )(let ((?x10793 (storage_t x_0 x_1 w_2 w_3 x_MLOAD_0 2 w)))
 (let ((?x4962 (storage_t x_0 x_1 w_2 w_3 x_MLOAD_0 3 w)))
 (= ?x4962 ?x10793))))
 ))
 (let (($x3934 (forall ((n (_ BitVec 6)) )(let ((?x4056 (sc_t 2)))
 (let ((?x7955 (bvadd (_ bv60 6) ?x4056)))
 (let (($x8703 (bvsle ?x7955 n)))
 (let ((?x1543 (stack_t x_0 x_1 w_2 w_3 x_MLOAD_0 2 n)))
 (let ((?x10144 (stack_t x_0 x_1 w_2 w_3 x_MLOAD_0 3 n)))
 (or (= ?x10144 ?x1543) $x8703)))))))
 ))
 (let ((?x4056 (sc_t 2)))
 (let (($x2966 (= ?x2012 ?x4056)))
 (let ((?x537 (used_gas_t x_0 x_1 w_2 w_3 x_MLOAD_0 3)))
 (let (($x2917 (= (stack_t x_0 x_1 w_2 w_3 x_MLOAD_0 3 (bvadd (_ bv62 6) ?x2012)) (stack_t x_0 x_1 w_2 w_3 x_MLOAD_0 2 (bvadd (_ bv62 6) ?x4056)))))
 (let (($x2708 (= (stack_t x_0 x_1 w_2 w_3 x_MLOAD_0 3 (bvadd (_ bv61 6) ?x2012)) (stack_t x_0 x_1 w_2 w_3 x_MLOAD_0 2 (bvadd (_ bv61 6) ?x4056)))))
 (let (($x1877 (= (stack_t x_0 x_1 w_2 w_3 x_MLOAD_0 3 (bvadd (_ bv60 6) ?x2012)) (stack_t x_0 x_1 w_2 w_3 x_MLOAD_0 2 (bvadd (_ bv63 6) ?x4056)))))
 (let (($x321 (= ?x2529 (stack_t x_0 x_1 w_2 w_3 x_MLOAD_0 2 (bvadd (_ bv60 6) ?x4056)))))
 (let (($x7962 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_t 1)))) (_ bv0 1)))))
 (let (($x1920 (exc_halt_t 1)))
 (let (($x903 (exc_halt_t 2)))
 (let (($x2178 (= $x903 (or $x1920 $x7962))))
 (let (($x3401 (forall ((w (_ BitVec 256)) )(let ((?x10657 (storage_t x_0 x_1 w_2 w_3 x_MLOAD_0 1 w)))
 (let ((?x10793 (storage_t x_0 x_1 w_2 w_3 x_MLOAD_0 2 w)))
 (= ?x10793 ?x10657))))
 ))
 (let (($x3693 (forall ((n (_ BitVec 6)) )(let ((?x4023 (sc_t 1)))
 (let (($x9004 (bvsle ?x4023 n)))
 (let ((?x10457 (stack_t x_0 x_1 w_2 w_3 x_MLOAD_0 1 n)))
 (let ((?x1543 (stack_t x_0 x_1 w_2 w_3 x_MLOAD_0 2 n)))
 (or (= ?x1543 ?x10457) $x9004))))))
 ))
 (let (($x6865 (= ?x4056 (bvadd (_ bv1 6) (sc_t 1)))))
 (let ((?x10833 (used_gas_t x_0 x_1 w_2 w_3 x_MLOAD_0 2)))
 (let (($x2108 (= $x1920 (or $x56 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x63))) (_ bv0 1)))))))
 (let (($x151 (forall ((w (_ BitVec 256)) )(let ((?x4747 (storage_t x_0 x_1 w_2 w_3 x_MLOAD_0 0 w)))
 (let ((?x10657 (storage_t x_0 x_1 w_2 w_3 x_MLOAD_0 1 w)))
 (= ?x10657 ?x4747))))
 ))
 (let (($x2750 (forall ((n (_ BitVec 6)) )(let ((?x63 (sc_t 0)))
 (let (($x8037 (bvsle ?x63 n)))
 (let ((?x4609 (stack_t x_0 x_1 w_2 w_3 x_MLOAD_0 0 n)))
 (let ((?x10457 (stack_t x_0 x_1 w_2 w_3 x_MLOAD_0 1 n)))
 (or (= ?x10457 ?x4609) $x8037))))))
 ))
 (let ((?x4023 (sc_t 1)))
 (let (($x2532 (= ?x4023 (bvadd (_ bv1 6) ?x63))))
 (let (($x3979 (exc_halt_s 5)))
 (let (($x1625 (or $x3979 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 5)))) (_ bv0 1))))))
 (let (($x3253 (forall ((w (_ BitVec 256)) )(let ((?x3292 (storage_s x_0 x_1 w_2 w_3 x_MLOAD_0 5 w)))
 (let ((?x10856 (storage_s x_0 x_1 w_2 w_3 x_MLOAD_0 6 w)))
 (= ?x10856 ?x3292))))
 ))
 (let (($x2648 (forall ((n (_ BitVec 6)) )(let ((?x10357 (stack_s x_0 x_1 w_2 w_3 x_MLOAD_0 5 n)))
 (let ((?x4769 (stack_s x_0 x_1 w_2 w_3 x_MLOAD_0 6 n)))
 (or (bvsle (sc_s 5) n) (= ?x4769 ?x10357)))))
 ))
 (let (($x7317 (= (used_gas_s x_0 x_1 w_2 w_3 x_MLOAD_0 6) (+ 3 (used_gas_s x_0 x_1 w_2 w_3 x_MLOAD_0 5)))))
 (let (($x64 (exc_halt_s 4)))
 (let (($x3406 (or $x64 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 4)))) (_ bv0 1))))))
 (let (($x6775 (= $x3979 $x3406)))
 (let (($x6123 (forall ((w (_ BitVec 256)) )(let ((?x10882 (storage_s x_0 x_1 w_2 w_3 x_MLOAD_0 4 w)))
 (let ((?x3292 (storage_s x_0 x_1 w_2 w_3 x_MLOAD_0 5 w)))
 (= ?x3292 ?x10882))))
 ))
 (let (($x3240 (forall ((n (_ BitVec 6)) )(let ((?x4305 (sc_s 4)))
 (let (($x8502 (bvsle ?x4305 n)))
 (let ((?x90 (stack_s x_0 x_1 w_2 w_3 x_MLOAD_0 4 n)))
 (let ((?x10357 (stack_s x_0 x_1 w_2 w_3 x_MLOAD_0 5 n)))
 (or (= ?x10357 ?x90) $x8502))))))
 ))
 (let ((?x805 (sc_s 5)))
 (let (($x8151 (= ?x805 (bvadd (_ bv1 6) (sc_s 4)))))
 (let ((?x43 (used_gas_s x_0 x_1 w_2 w_3 x_MLOAD_0 5)))
 (let (($x6796 (= $x64 (or (exc_halt_s 3) (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_s 3))))))))
 (let (($x2638 (forall ((w (_ BitVec 256)) )(let ((?x9467 (storage_s x_0 x_1 w_2 w_3 x_MLOAD_0 3 w)))
 (let ((?x10882 (storage_s x_0 x_1 w_2 w_3 x_MLOAD_0 4 w)))
 (= ?x10882 ?x9467))))
 ))
 (let (($x3087 (forall ((n (_ BitVec 6)) )(let ((?x1055 (stack_s x_0 x_1 w_2 w_3 x_MLOAD_0 3 n)))
 (let ((?x90 (stack_s x_0 x_1 w_2 w_3 x_MLOAD_0 4 n)))
 (let ((?x275 (sc_s 3)))
 (let ((?x1725 (bvadd (_ bv63 6) ?x275)))
 (let (($x7265 (bvsle ?x1725 n)))
 (or $x7265 (= ?x90 ?x1055))))))))
 ))
 (let ((?x275 (sc_s 3)))
 (let ((?x1725 (bvadd (_ bv63 6) ?x275)))
 (let ((?x4305 (sc_s 4)))
 (let (($x7321 (= ?x4305 ?x1725)))
 (let ((?x10596 (used_gas_s x_0 x_1 w_2 w_3 x_MLOAD_0 4)))
 (let (($x292 (exc_halt_s 3)))
 (let (($x1562 (= $x292 (or (exc_halt_s 2) (not (bvsle (_ bv0 6) (bvadd (_ bv61 6) (sc_s 2))))))))
 (let (($x11092 (forall ((w (_ BitVec 256)) )(let ((?x2588 (storage_s x_0 x_1 w_2 w_3 x_MLOAD_0 2 w)))
 (let ((?x9467 (storage_s x_0 x_1 w_2 w_3 x_MLOAD_0 3 w)))
 (= ?x9467 ?x2588))))
 ))
 (let (($x3825 (forall ((n (_ BitVec 6)) )(let ((?x218 (sc_s 2)))
 (let ((?x6160 (bvadd (_ bv61 6) ?x218)))
 (let (($x6022 (bvsle ?x6160 n)))
 (let ((?x6812 (stack_s x_0 x_1 w_2 w_3 x_MLOAD_0 2 n)))
 (let ((?x1055 (stack_s x_0 x_1 w_2 w_3 x_MLOAD_0 3 n)))
 (or (= ?x1055 ?x6812) $x6022)))))))
 ))
 (let ((?x218 (sc_s 2)))
 (let (($x4319 (= ?x275 ?x218)))
 (let ((?x2357 (used_gas_s x_0 x_1 w_2 w_3 x_MLOAD_0 3)))
 (let (($x2746 (= (stack_s x_0 x_1 w_2 w_3 x_MLOAD_0 3 (bvadd (_ bv62 6) ?x275)) (stack_s x_0 x_1 w_2 w_3 x_MLOAD_0 2 (bvadd (_ bv62 6) ?x218)))))
 (let ((?x7253 (bvadd (_ bv63 6) ?x218)))
 (let ((?x8214 (stack_s x_0 x_1 w_2 w_3 x_MLOAD_0 2 ?x7253)))
 (let (($x322 (= (stack_s x_0 x_1 w_2 w_3 x_MLOAD_0 3 (bvadd (_ bv61 6) ?x275)) ?x8214)))
 (let (($x2566 (= (stack_s x_0 x_1 w_2 w_3 x_MLOAD_0 3 ?x1725) (stack_s x_0 x_1 w_2 w_3 x_MLOAD_0 2 (bvadd (_ bv61 6) ?x218)))))
 (let (($x247 (exc_halt_s 2)))
 (let (($x1881 (= $x247 (or (exc_halt_s 1) (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_s 1))))))))
 (let (($x5675 (forall ((w (_ BitVec 256)) )(let ((?x10893 (storage_s x_0 x_1 w_2 w_3 x_MLOAD_0 1 w)))
 (let ((?x2588 (storage_s x_0 x_1 w_2 w_3 x_MLOAD_0 2 w)))
 (= ?x2588 ?x10893))))
 ))
 (let (($x8453 (forall ((n (_ BitVec 6)) )(let ((?x154 (sc_s 1)))
 (let ((?x7299 (bvadd (_ bv63 6) ?x154)))
 (let (($x7263 (bvsle ?x7299 n)))
 (let ((?x4690 (stack_s x_0 x_1 w_2 w_3 x_MLOAD_0 1 n)))
 (let ((?x6812 (stack_s x_0 x_1 w_2 w_3 x_MLOAD_0 2 n)))
 (or (= ?x6812 ?x4690) $x7263)))))))
 ))
 (let ((?x154 (sc_s 1)))
 (let (($x1981 (= ?x218 ?x154)))
 (let ((?x4378 (used_gas_s x_0 x_1 w_2 w_3 x_MLOAD_0 2)))
 (let ((?x7299 (bvadd (_ bv63 6) ?x154)))
 (let ((?x8308 (stack_s x_0 x_1 w_2 w_3 x_MLOAD_0 1 ?x7299)))
 (let (($x2924 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x72))) (_ bv0 1)))))
 (let (($x189 (exc_halt_s 1)))
 (let (($x11003 (forall ((w (_ BitVec 256)) )(let ((?x664 (storage_s x_0 x_1 w_2 w_3 x_MLOAD_0 0 w)))
 (let ((?x10893 (storage_s x_0 x_1 w_2 w_3 x_MLOAD_0 1 w)))
 (= ?x10893 ?x664))))
 ))
 (let (($x2927 (forall ((n (_ BitVec 6)) )(let ((?x6212 (stack_s x_0 x_1 w_2 w_3 x_MLOAD_0 0 n)))
 (let ((?x4690 (stack_s x_0 x_1 w_2 w_3 x_MLOAD_0 1 n)))
 (or (bvsle (bvadd (_ bv62 6) (sc_s 0)) n) (= ?x4690 ?x6212)))))
 ))
 (let (($x6201 (= ?x154 (bvadd (_ bv1 6) ?x72))))
 (let (($x11041 (= (stack_s x_0 x_1 w_2 w_3 x_MLOAD_0 1 (bvadd (_ bv63 6) ?x72)) (stack_s x_0 x_1 w_2 w_3 x_MLOAD_0 0 (bvadd (_ bv63 6) ?x72)))))
 (let ((?x1691 (bvadd (_ bv62 6) ?x72)))
 (let ((?x2042 (stack_s x_0 x_1 w_2 w_3 x_MLOAD_0 0 ?x1691)))
 (let (($x3300 (forall ((w0 (_ BitVec 256)) )(let (($x391 (= (stack_s x_0 x_1 w_2 w_3 x_MLOAD_0 1 (bvadd (_ bv63 6) (sc_s 1))) w0)))
 (let ((?x4479 (f_MLOAD x_0 x_1 w_2 w_3 x_MLOAD_0 w0)))
 (= ?x4479 (ite $x391 x_MLOAD_0 (_ bv0 256))))))
 ))
 (let (($x10775 (forall ((w (_ BitVec 256)) )(let ((?x664 (storage_s x_0 x_1 w_2 w_3 x_MLOAD_0 0 w)))
 (= ?x664 (_ bv0 256))))
 ))
 (let (($x10784 (= ?x4621 0)))
 (let (($x3899 (not $x57)))
 (let (($x5094 (= (stack_s x_0 x_1 w_2 w_3 x_MLOAD_0 0 (_ bv1 6)) x_1)))
 (let (($x10840 (= (stack_s x_0 x_1 w_2 w_3 x_MLOAD_0 0 (_ bv0 6)) x_0)))
 (let (($x206 (= ?x72 (_ bv2 6))))
 (and $x206 $x10840 $x5094 $x3899 $x10784 $x10775 $x3300 (= ?x8308 ?x2042) (= (stack_s x_0 x_1 w_2 w_3 x_MLOAD_0 1 ?x1691) ?x2042) $x11041 (= (used_gas_s x_0 x_1 w_2 w_3 x_MLOAD_0 1) (+ 3 ?x4621)) $x6201 $x2927 $x11003 (= $x189 (or $x57 $x2924 (not (bvsle (_ bv0 6) ?x1691)))) (= ?x8214 (f_MLOAD x_0 x_1 w_2 w_3 x_MLOAD_0 ?x8308)) (= ?x4378 (+ 3 (used_gas_s x_0 x_1 w_2 w_3 x_MLOAD_0 1))) $x1981 $x8453 $x5675 $x1881 $x2566 $x322 $x2746 (= ?x2357 (+ 3 ?x4378)) $x4319 $x3825 $x11092 $x1562 (= ?x10596 (+ 2 ?x2357)) $x7321 $x3087 $x2638 $x6796 (= (stack_s x_0 x_1 w_2 w_3 x_MLOAD_0 5 ?x4305) w_2) (= ?x43 (+ 3 ?x10596)) $x8151 $x3240 $x6123 $x6775 (= (stack_s x_0 x_1 w_2 w_3 x_MLOAD_0 6 ?x805) w_3) $x7317 (= ?x926 (bvadd (_ bv1 6) ?x805)) $x2648 $x3253 (= $x772 $x1625) (= (stack_t x_0 x_1 w_2 w_3 x_MLOAD_0 1 ?x63) w_2) (= (used_gas_t x_0 x_1 w_2 w_3 x_MLOAD_0 1) (+ 3 ?x4656)) $x2532 $x2750 $x151 $x2108 (= (stack_t x_0 x_1 w_2 w_3 x_MLOAD_0 2 ?x4023) w_3) (= ?x10833 (+ 3 (used_gas_t x_0 x_1 w_2 w_3 x_MLOAD_0 1))) $x6865 $x3693 $x3401 $x2178 $x321 $x1877 $x2708 $x2917 (= ?x537 (+ 3 ?x10833)) $x2966 $x3934 $x4221 $x7770 (= ?x3584 (f_MLOAD x_0 x_1 w_2 w_3 x_MLOAD_0 ?x2529)) (= ?x5133 (+ 3 ?x537)) $x5385 $x9398 $x8188 $x637 $x8017 $x6803 $x1173 $x3720 $x9796 $x8813 $x2822 $x7488 $x9194 $x73 $x7733 $x58 $x4750 $x4320 (not (and $x929 $x5625 $x889 $x144)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
 )
(check-sat)
(get-proof)
