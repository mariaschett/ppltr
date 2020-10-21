; PUSH cw_1 SWAP3 SWAP1 SWAP2 AND SWAP1 => SWAP2 AND PUSH cw_1 SWAP2
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
(assert
 (exists ((x_0 (_ BitVec 256)) (x_1 (_ BitVec 256)) (x_2 (_ BitVec 256)) )(let (($x10715 (forall ((w (_ BitVec 256)) )(let ((?x10625 (storage_t x_0 x_1 x_2 w_1 4 w)))
 (let ((?x4426 (storage_s x_0 x_1 x_2 w_1 6 w)))
 (= ?x4426 ?x10625))))
 ))
 (let (($x7722 (exc_halt_t 4)))
 (let (($x772 (exc_halt_s 6)))
 (let (($x5620 (= $x772 $x7722)))
 (let (($x4873 (forall ((n (_ BitVec 6)) )(let ((?x2060 (stack_t x_0 x_1 x_2 w_1 4 n)))
 (let ((?x1449 (stack_s x_0 x_1 x_2 w_1 6 n)))
 (let (($x5501 (= ?x1449 ?x2060)))
 (let ((?x11631 (sc_t 4)))
 (let (($x4767 (bvsle ?x11631 n)))
 (or $x4767 $x5501)))))))
 ))
 (let ((?x11631 (sc_t 4)))
 (let ((?x926 (sc_s 6)))
 (let (($x11559 (= ?x926 ?x11631)))
 (let ((?x9535 (used_gas_t x_0 x_1 x_2 w_1 0)))
 (let ((?x3942 (used_gas_s x_0 x_1 x_2 w_1 0)))
 (let (($x6313 (= ?x3942 ?x9535)))
 (let (($x3308 (forall ((w (_ BitVec 256)) )(let ((?x8396 (storage_t x_0 x_1 x_2 w_1 0 w)))
 (let ((?x11272 (storage_s x_0 x_1 x_2 w_1 0 w)))
 (= ?x11272 ?x8396))))
 ))
 (let (($x56 (exc_halt_t 0)))
 (let (($x57 (exc_halt_s 0)))
 (let (($x58 (= $x57 $x56)))
 (let (($x9406 (forall ((n (_ BitVec 6)) )(let ((?x955 (stack_t x_0 x_1 x_2 w_1 0 n)))
 (let ((?x8002 (stack_s x_0 x_1 x_2 w_1 0 n)))
 (let (($x10371 (= ?x8002 ?x955)))
 (let ((?x63 (sc_t 0)))
 (let (($x4790 (bvsle ?x63 n)))
 (or $x4790 $x10371)))))))
 ))
 (let ((?x63 (sc_t 0)))
 (let ((?x72 (sc_s 0)))
 (let (($x73 (= ?x72 ?x63)))
 (let (($x4952 (= $x7722 (or (exc_halt_t 3) (not (bvsle (_ bv0 6) (bvadd (_ bv61 6) (sc_t 3))))))))
 (let (($x4133 (forall ((w (_ BitVec 256)) )(let ((?x11331 (storage_t x_0 x_1 x_2 w_1 3 w)))
 (let ((?x10625 (storage_t x_0 x_1 x_2 w_1 4 w)))
 (= ?x10625 ?x11331))))
 ))
 (let (($x978 (forall ((n (_ BitVec 6)) )(let ((?x4148 (stack_t x_0 x_1 x_2 w_1 3 n)))
 (let ((?x2060 (stack_t x_0 x_1 x_2 w_1 4 n)))
 (or (bvsle (bvadd (_ bv61 6) (sc_t 3)) n) (= ?x2060 ?x4148)))))
 ))
 (let (($x4285 (= (used_gas_t x_0 x_1 x_2 w_1 4) (+ 3 (used_gas_t x_0 x_1 x_2 w_1 3)))))
 (let (($x4815 (= (stack_t x_0 x_1 x_2 w_1 4 (bvadd (_ bv62 6) ?x11631)) (stack_t x_0 x_1 x_2 w_1 3 (bvadd (_ bv62 6) (sc_t 3))))))
 (let (($x3063 (= (stack_t x_0 x_1 x_2 w_1 4 (bvadd (_ bv61 6) ?x11631)) (stack_t x_0 x_1 x_2 w_1 3 (bvadd (_ bv63 6) (sc_t 3))))))
 (let (($x4443 (= (stack_t x_0 x_1 x_2 w_1 4 (bvadd (_ bv63 6) ?x11631)) (stack_t x_0 x_1 x_2 w_1 3 (bvadd (_ bv61 6) (sc_t 3))))))
 (let (($x2163 (exc_halt_t 2)))
 (let (($x2249 (or $x2163 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_t 2)))) (_ bv0 1))))))
 (let (($x6783 (exc_halt_t 3)))
 (let (($x8762 (= $x6783 $x2249)))
 (let (($x5263 (forall ((w (_ BitVec 256)) )(let ((?x5712 (storage_t x_0 x_1 x_2 w_1 2 w)))
 (let ((?x11331 (storage_t x_0 x_1 x_2 w_1 3 w)))
 (= ?x11331 ?x5712))))
 ))
 (let (($x1274 (forall ((n (_ BitVec 6)) )(let ((?x2714 (sc_t 2)))
 (let (($x9647 (bvsle ?x2714 n)))
 (let ((?x3491 (stack_t x_0 x_1 x_2 w_1 2 n)))
 (let ((?x4148 (stack_t x_0 x_1 x_2 w_1 3 n)))
 (or (= ?x4148 ?x3491) $x9647))))))
 ))
 (let ((?x6438 (sc_t 3)))
 (let (($x9574 (= ?x6438 (bvadd (_ bv1 6) (sc_t 2)))))
 (let ((?x11699 (used_gas_t x_0 x_1 x_2 w_1 3)))
 (let (($x8722 (= $x2163 (or (exc_halt_t 1) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_t 1))))))))
 (let (($x2487 (forall ((w (_ BitVec 256)) )(let ((?x7044 (storage_t x_0 x_1 x_2 w_1 1 w)))
 (let ((?x5712 (storage_t x_0 x_1 x_2 w_1 2 w)))
 (= ?x5712 ?x7044))))
 ))
 (let (($x3921 (forall ((n (_ BitVec 6)) )(let ((?x8347 (sc_t 1)))
 (let ((?x4898 (bvadd (_ bv62 6) ?x8347)))
 (let (($x4429 (bvsle ?x4898 n)))
 (let ((?x4587 (stack_t x_0 x_1 x_2 w_1 1 n)))
 (let ((?x3491 (stack_t x_0 x_1 x_2 w_1 2 n)))
 (or (= ?x3491 ?x4587) $x4429)))))))
 ))
 (let ((?x8347 (sc_t 1)))
 (let ((?x9514 (bvadd (_ bv63 6) ?x8347)))
 (let ((?x2714 (sc_t 2)))
 (let (($x2744 (= ?x2714 ?x9514)))
 (let ((?x7938 (used_gas_t x_0 x_1 x_2 w_1 2)))
 (let ((?x4687 (bvor (bvnot (stack_t x_0 x_1 x_2 w_1 1 ?x9514)) (bvnot (stack_t x_0 x_1 x_2 w_1 1 (bvadd (_ bv62 6) ?x8347))))))
 (let (($x4294 (= (stack_t x_0 x_1 x_2 w_1 2 (bvadd (_ bv63 6) ?x2714)) (bvnot ?x4687))))
 (let (($x3508 (exc_halt_t 1)))
 (let (($x6585 (= $x3508 (or $x56 (not (bvsle (_ bv0 6) (bvadd (_ bv61 6) ?x63)))))))
 (let (($x7598 (forall ((w (_ BitVec 256)) )(let ((?x8396 (storage_t x_0 x_1 x_2 w_1 0 w)))
 (let ((?x7044 (storage_t x_0 x_1 x_2 w_1 1 w)))
 (= ?x7044 ?x8396))))
 ))
 (let (($x5497 (forall ((n (_ BitVec 6)) )(let ((?x955 (stack_t x_0 x_1 x_2 w_1 0 n)))
 (let ((?x4587 (stack_t x_0 x_1 x_2 w_1 1 n)))
 (let ((?x63 (sc_t 0)))
 (let ((?x10032 (bvadd (_ bv61 6) ?x63)))
 (let (($x3043 (bvsle ?x10032 n)))
 (or $x3043 (= ?x4587 ?x955))))))))
 ))
 (let (($x5230 (= ?x8347 ?x63)))
 (let ((?x4898 (bvadd (_ bv62 6) ?x8347)))
 (let ((?x5456 (stack_t x_0 x_1 x_2 w_1 1 ?x4898)))
 (let (($x1666 (= (stack_t x_0 x_1 x_2 w_1 1 (bvadd (_ bv61 6) ?x8347)) (stack_t x_0 x_1 x_2 w_1 0 (bvadd (_ bv63 6) ?x63)))))
 (let ((?x4897 (stack_t x_0 x_1 x_2 w_1 1 ?x9514)))
 (let (($x1001 (= $x772 (or (exc_halt_s 5) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 5))))))))
 (let (($x10789 (forall ((w (_ BitVec 256)) )(let ((?x2976 (storage_s x_0 x_1 x_2 w_1 5 w)))
 (let ((?x4426 (storage_s x_0 x_1 x_2 w_1 6 w)))
 (= ?x4426 ?x2976))))
 ))
 (let (($x1943 (forall ((n (_ BitVec 6)) )(let ((?x2363 (stack_s x_0 x_1 x_2 w_1 5 n)))
 (let ((?x1449 (stack_s x_0 x_1 x_2 w_1 6 n)))
 (let ((?x4319 (sc_s 5)))
 (let ((?x3094 (bvadd (_ bv62 6) ?x4319)))
 (let (($x9914 (bvsle ?x3094 n)))
 (or $x9914 (= ?x1449 ?x2363))))))))
 ))
 (let ((?x4319 (sc_s 5)))
 (let (($x1302 (= ?x926 ?x4319)))
 (let (($x9891 (= (used_gas_s x_0 x_1 x_2 w_1 6) (+ 3 (used_gas_s x_0 x_1 x_2 w_1 5)))))
 (let ((?x3388 (bvadd (_ bv63 6) ?x4319)))
 (let ((?x988 (stack_s x_0 x_1 x_2 w_1 5 ?x3388)))
 (let (($x3036 (= (stack_s x_0 x_1 x_2 w_1 6 (bvadd (_ bv63 6) ?x926)) (stack_s x_0 x_1 x_2 w_1 5 (bvadd (_ bv62 6) ?x4319)))))
 (let (($x11317 (exc_halt_s 5)))
 (let (($x108 (= $x11317 (or (exc_halt_s 4) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 4))))))))
 (let (($x2690 (forall ((w (_ BitVec 256)) )(let ((?x5736 (storage_s x_0 x_1 x_2 w_1 4 w)))
 (let ((?x2976 (storage_s x_0 x_1 x_2 w_1 5 w)))
 (= ?x2976 ?x5736))))
 ))
 (let (($x5327 (forall ((n (_ BitVec 6)) )(let ((?x5703 (stack_s x_0 x_1 x_2 w_1 4 n)))
 (let ((?x2363 (stack_s x_0 x_1 x_2 w_1 5 n)))
 (let ((?x4305 (sc_s 4)))
 (let ((?x11568 (bvadd (_ bv62 6) ?x4305)))
 (let (($x573 (bvsle ?x11568 n)))
 (or $x573 (= ?x2363 ?x5703))))))))
 ))
 (let ((?x4305 (sc_s 4)))
 (let ((?x10159 (bvadd (_ bv63 6) ?x4305)))
 (let (($x129 (= ?x4319 ?x10159)))
 (let ((?x1039 (used_gas_s x_0 x_1 x_2 w_1 5)))
 (let ((?x8880 (bvor (bvnot (stack_s x_0 x_1 x_2 w_1 4 ?x10159)) (bvnot (stack_s x_0 x_1 x_2 w_1 4 (bvadd (_ bv62 6) ?x4305))))))
 (let (($x7172 (exc_halt_s 4)))
 (let (($x7612 (= $x7172 (or (exc_halt_s 3) (not (bvsle (_ bv0 6) (bvadd (_ bv61 6) (sc_s 3))))))))
 (let (($x1615 (forall ((w (_ BitVec 256)) )(let ((?x6587 (storage_s x_0 x_1 x_2 w_1 3 w)))
 (let ((?x5736 (storage_s x_0 x_1 x_2 w_1 4 w)))
 (= ?x5736 ?x6587))))
 ))
 (let (($x2338 (forall ((n (_ BitVec 6)) )(let ((?x4352 (stack_s x_0 x_1 x_2 w_1 3 n)))
 (let ((?x5703 (stack_s x_0 x_1 x_2 w_1 4 n)))
 (or (= ?x5703 ?x4352) (bvsle (bvadd (_ bv61 6) (sc_s 3)) n)))))
 ))
 (let ((?x275 (sc_s 3)))
 (let (($x3822 (= ?x4305 ?x275)))
 (let ((?x9790 (used_gas_s x_0 x_1 x_2 w_1 4)))
 (let ((?x10426 (bvadd (_ bv62 6) ?x275)))
 (let ((?x1145 (stack_s x_0 x_1 x_2 w_1 3 ?x10426)))
 (let ((?x11568 (bvadd (_ bv62 6) ?x4305)))
 (let ((?x389 (stack_s x_0 x_1 x_2 w_1 4 ?x11568)))
 (let ((?x9670 (bvadd (_ bv63 6) ?x275)))
 (let ((?x6187 (stack_s x_0 x_1 x_2 w_1 3 ?x9670)))
 (let ((?x10004 (stack_s x_0 x_1 x_2 w_1 4 ?x10159)))
 (let (($x292 (exc_halt_s 3)))
 (let (($x3705 (= $x292 (or (exc_halt_s 2) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 2))))))))
 (let (($x1208 (forall ((w (_ BitVec 256)) )(let ((?x4568 (storage_s x_0 x_1 x_2 w_1 2 w)))
 (let ((?x6587 (storage_s x_0 x_1 x_2 w_1 3 w)))
 (= ?x6587 ?x4568))))
 ))
 (let (($x11226 (forall ((n (_ BitVec 6)) )(let ((?x218 (sc_s 2)))
 (let ((?x6909 (bvadd (_ bv62 6) ?x218)))
 (let (($x11825 (bvsle ?x6909 n)))
 (let ((?x10798 (stack_s x_0 x_1 x_2 w_1 2 n)))
 (let ((?x4352 (stack_s x_0 x_1 x_2 w_1 3 n)))
 (or (= ?x4352 ?x10798) $x11825)))))))
 ))
 (let ((?x218 (sc_s 2)))
 (let (($x2729 (= ?x275 ?x218)))
 (let ((?x11797 (used_gas_s x_0 x_1 x_2 w_1 3)))
 (let (($x247 (exc_halt_s 2)))
 (let (($x9838 (= $x247 (or (exc_halt_s 1) (not (bvsle (_ bv0 6) (bvadd (_ bv60 6) (sc_s 1))))))))
 (let (($x4714 (forall ((w (_ BitVec 256)) )(let ((?x9705 (storage_s x_0 x_1 x_2 w_1 1 w)))
 (let ((?x4568 (storage_s x_0 x_1 x_2 w_1 2 w)))
 (= ?x4568 ?x9705))))
 ))
 (let (($x2597 (forall ((n (_ BitVec 6)) )(let ((?x3403 (stack_s x_0 x_1 x_2 w_1 1 n)))
 (let ((?x10798 (stack_s x_0 x_1 x_2 w_1 2 n)))
 (or (bvsle (bvadd (_ bv60 6) (sc_s 1)) n) (= ?x10798 ?x3403)))))
 ))
 (let ((?x154 (sc_s 1)))
 (let (($x1181 (= ?x218 ?x154)))
 (let ((?x6182 (used_gas_s x_0 x_1 x_2 w_1 2)))
 (let ((?x6909 (bvadd (_ bv62 6) ?x218)))
 (let ((?x5985 (stack_s x_0 x_1 x_2 w_1 2 ?x6909)))
 (let (($x10571 (= (stack_s x_0 x_1 x_2 w_1 2 (bvadd (_ bv61 6) ?x218)) (stack_s x_0 x_1 x_2 w_1 1 (bvadd (_ bv61 6) ?x154)))))
 (let (($x489 (= (stack_s x_0 x_1 x_2 w_1 2 (bvadd (_ bv60 6) ?x218)) (stack_s x_0 x_1 x_2 w_1 1 (bvadd (_ bv63 6) ?x154)))))
 (let ((?x7280 (bvadd (_ bv63 6) ?x218)))
 (let ((?x10207 (stack_s x_0 x_1 x_2 w_1 2 ?x7280)))
 (let (($x189 (exc_halt_s 1)))
 (let (($x3774 (= $x189 (or $x57 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x72))) (_ bv0 1)))))))
 (let (($x6030 (forall ((w (_ BitVec 256)) )(let ((?x11272 (storage_s x_0 x_1 x_2 w_1 0 w)))
 (let ((?x9705 (storage_s x_0 x_1 x_2 w_1 1 w)))
 (= ?x9705 ?x11272))))
 ))
 (let (($x3342 (forall ((n (_ BitVec 6)) )(let ((?x72 (sc_s 0)))
 (let (($x6179 (bvsle ?x72 n)))
 (let ((?x8002 (stack_s x_0 x_1 x_2 w_1 0 n)))
 (let ((?x3403 (stack_s x_0 x_1 x_2 w_1 1 n)))
 (or (= ?x3403 ?x8002) $x6179))))))
 ))
 (let (($x11645 (= ?x154 (bvadd (_ bv1 6) ?x72))))
 (let (($x9536 (forall ((w (_ BitVec 256)) )(let ((?x11272 (storage_s x_0 x_1 x_2 w_1 0 w)))
 (= ?x11272 (_ bv0 256))))
 ))
 (let (($x1139 (= ?x3942 0)))
 (let (($x11450 (not $x57)))
 (let (($x9264 (= (stack_s x_0 x_1 x_2 w_1 0 (_ bv2 6)) x_2)))
 (let (($x4066 (= (stack_s x_0 x_1 x_2 w_1 0 (_ bv1 6)) x_1)))
 (let (($x9806 (= (stack_s x_0 x_1 x_2 w_1 0 (_ bv0 6)) x_0)))
 (let (($x5315 (= ?x72 (_ bv3 6))))
 (and $x5315 $x9806 $x4066 $x9264 $x11450 $x1139 $x9536 (= (stack_s x_0 x_1 x_2 w_1 1 ?x72) w_1) (= (used_gas_s x_0 x_1 x_2 w_1 1) (+ 3 ?x3942)) $x11645 $x3342 $x6030 $x3774 (= ?x10207 (stack_s x_0 x_1 x_2 w_1 1 (bvadd (_ bv60 6) ?x154))) $x489 $x10571 (= ?x5985 (stack_s x_0 x_1 x_2 w_1 1 (bvadd (_ bv62 6) ?x154))) (= ?x6182 (+ 3 (used_gas_s x_0 x_1 x_2 w_1 1))) $x1181 $x2597 $x4714 $x9838 (= ?x6187 ?x5985) (= ?x1145 ?x10207) (= ?x11797 (+ 3 ?x6182)) $x2729 $x11226 $x1208 $x3705 (= ?x10004 (stack_s x_0 x_1 x_2 w_1 3 (bvadd (_ bv61 6) ?x275))) (= (stack_s x_0 x_1 x_2 w_1 4 (bvadd (_ bv61 6) ?x4305)) ?x6187) (= ?x389 ?x1145) (= ?x9790 (+ 3 ?x11797)) $x3822 $x2338 $x1615 $x7612 (= ?x988 (bvnot ?x8880)) (= ?x1039 (+ 3 ?x9790)) $x129 $x5327 $x2690 $x108 $x3036 (= (stack_s x_0 x_1 x_2 w_1 6 (bvadd (_ bv62 6) ?x926)) ?x988) $x9891 $x1302 $x1943 $x10789 $x1001 (= ?x4897 (stack_t x_0 x_1 x_2 w_1 0 (bvadd (_ bv61 6) ?x63))) $x1666 (= ?x5456 (stack_t x_0 x_1 x_2 w_1 0 (bvadd (_ bv62 6) ?x63))) (= (used_gas_t x_0 x_1 x_2 w_1 1) (+ 3 ?x9535)) $x5230 $x5497 $x7598 $x6585 $x4294 (= ?x7938 (+ 3 (used_gas_t x_0 x_1 x_2 w_1 1))) $x2744 $x3921 $x2487 $x8722 (= (stack_t x_0 x_1 x_2 w_1 3 ?x2714) w_1) (= ?x11699 (+ 3 ?x7938)) $x9574 $x1274 $x5263 $x8762 $x4443 $x3063 $x4815 $x4285 (= ?x11631 ?x6438) $x978 $x4133 $x4952 $x73 $x9406 $x58 $x3308 $x6313 (not (and $x11559 $x4873 $x5620 $x10715))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
 )
(check-sat)
(get-proof)
