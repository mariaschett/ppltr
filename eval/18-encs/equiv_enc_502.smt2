; PUSH cw_3 DUP1 CALLDATALOAD SWAP2 SWAP1 => PUSH cw_3 CALLDATALOAD SWAP1 PUSH cw_3
(set-option :produce-proofs true)
; 
(set-info :status unknown)
(declare-fun storage_t ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 256)) (_ BitVec 256))
(declare-fun w_3 () (_ BitVec 256))
(declare-fun storage_s ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 256)) (_ BitVec 256))
(declare-fun exc_halt_t (Int) Bool)
(declare-fun exc_halt_s (Int) Bool)
(declare-fun stack_t ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 6)) (_ BitVec 256))
(declare-fun stack_s ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 6)) (_ BitVec 256))
(declare-fun sc_t (Int) (_ BitVec 6))
(declare-fun sc_s (Int) (_ BitVec 6))
(declare-fun used_gas_t ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int) Int)
(declare-fun used_gas_s ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int) Int)
(declare-fun f_CALLDATALOAD ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256)) (_ BitVec 256))
(assert
 (exists ((x_0 (_ BitVec 256)) (x_CALLDATALOAD_0 (_ BitVec 256)) )(let (($x3477 (forall ((w (_ BitVec 256)) )(let ((?x1283 (storage_t x_0 w_3 x_CALLDATALOAD_0 4 w)))
 (let ((?x5929 (storage_s x_0 w_3 x_CALLDATALOAD_0 5 w)))
 (= ?x5929 ?x1283))))
 ))
 (let (($x7854 (exc_halt_t 4)))
 (let (($x11317 (exc_halt_s 5)))
 (let (($x8936 (= $x11317 $x7854)))
 (let (($x1338 (forall ((n (_ BitVec 6)) )(let ((?x11618 (stack_t x_0 w_3 x_CALLDATALOAD_0 4 n)))
 (let ((?x8741 (stack_s x_0 w_3 x_CALLDATALOAD_0 5 n)))
 (let (($x3839 (= ?x8741 ?x11618)))
 (let ((?x4818 (sc_t 4)))
 (let (($x1923 (bvsle ?x4818 n)))
 (or $x1923 $x3839)))))))
 ))
 (let ((?x4818 (sc_t 4)))
 (let ((?x4319 (sc_s 5)))
 (let (($x10544 (= ?x4319 ?x4818)))
 (let (($x10682 (not (and $x10544 $x1338 $x8936 $x3477))))
 (let ((?x7292 (used_gas_t x_0 w_3 x_CALLDATALOAD_0 0)))
 (let ((?x11377 (used_gas_s x_0 w_3 x_CALLDATALOAD_0 0)))
 (let (($x2182 (= ?x11377 ?x7292)))
 (let (($x11084 (forall ((w (_ BitVec 256)) )(let ((?x2415 (storage_t x_0 w_3 x_CALLDATALOAD_0 0 w)))
 (let ((?x1808 (storage_s x_0 w_3 x_CALLDATALOAD_0 0 w)))
 (= ?x1808 ?x2415))))
 ))
 (let (($x56 (exc_halt_t 0)))
 (let (($x57 (exc_halt_s 0)))
 (let (($x58 (= $x57 $x56)))
 (let (($x7526 (forall ((n (_ BitVec 6)) )(let ((?x3361 (stack_t x_0 w_3 x_CALLDATALOAD_0 0 n)))
 (let ((?x7523 (stack_s x_0 w_3 x_CALLDATALOAD_0 0 n)))
 (let (($x3796 (= ?x7523 ?x3361)))
 (let ((?x63 (sc_t 0)))
 (let (($x2855 (bvsle ?x63 n)))
 (or $x2855 $x3796)))))))
 ))
 (let ((?x63 (sc_t 0)))
 (let ((?x72 (sc_s 0)))
 (let (($x73 (= ?x72 ?x63)))
 (let (($x6783 (exc_halt_t 3)))
 (let (($x147 (or $x6783 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_t 3)))) (_ bv0 1))))))
 (let (($x3184 (= $x7854 $x147)))
 (let (($x2209 (forall ((w (_ BitVec 256)) )(let ((?x3334 (storage_t x_0 w_3 x_CALLDATALOAD_0 3 w)))
 (let ((?x1283 (storage_t x_0 w_3 x_CALLDATALOAD_0 4 w)))
 (= ?x1283 ?x3334))))
 ))
 (let (($x11374 (forall ((n (_ BitVec 6)) )(let ((?x477 (stack_t x_0 w_3 x_CALLDATALOAD_0 3 n)))
 (let ((?x11618 (stack_t x_0 w_3 x_CALLDATALOAD_0 4 n)))
 (let (($x11796 (= ?x11618 ?x477)))
 (let ((?x6438 (sc_t 3)))
 (let (($x1180 (bvsle ?x6438 n)))
 (or $x1180 $x11796)))))))
 ))
 (let (($x10007 (= ?x4818 (bvadd (_ bv1 6) (sc_t 3)))))
 (let ((?x11159 (used_gas_t x_0 w_3 x_CALLDATALOAD_0 4)))
 (let (($x503 (= $x6783 (or (exc_halt_t 2) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_t 2))))))))
 (let (($x4773 (forall ((w (_ BitVec 256)) )(let ((?x9134 (storage_t x_0 w_3 x_CALLDATALOAD_0 2 w)))
 (let ((?x3334 (storage_t x_0 w_3 x_CALLDATALOAD_0 3 w)))
 (= ?x3334 ?x9134))))
 ))
 (let (($x3551 (forall ((n (_ BitVec 6)) )(let ((?x11614 (stack_t x_0 w_3 x_CALLDATALOAD_0 2 n)))
 (let ((?x477 (stack_t x_0 w_3 x_CALLDATALOAD_0 3 n)))
 (let (($x6427 (= ?x477 ?x11614)))
 (or $x6427 (bvsle (bvadd (_ bv62 6) (sc_t 2)) n))))))
 ))
 (let ((?x2714 (sc_t 2)))
 (let ((?x6438 (sc_t 3)))
 (let (($x4836 (= ?x6438 ?x2714)))
 (let ((?x11615 (used_gas_t x_0 w_3 x_CALLDATALOAD_0 3)))
 (let (($x10139 (= ?x11615 (+ 3 (used_gas_t x_0 w_3 x_CALLDATALOAD_0 2)))))
 (let ((?x5618 (bvadd (_ bv63 6) ?x2714)))
 (let ((?x7475 (stack_t x_0 w_3 x_CALLDATALOAD_0 2 ?x5618)))
 (let ((?x10934 (bvadd (_ bv62 6) ?x6438)))
 (let ((?x5475 (stack_t x_0 w_3 x_CALLDATALOAD_0 3 ?x10934)))
 (let ((?x3500 (bvadd (_ bv62 6) ?x2714)))
 (let ((?x3945 (stack_t x_0 w_3 x_CALLDATALOAD_0 2 ?x3500)))
 (let ((?x5797 (bvadd (_ bv63 6) ?x6438)))
 (let ((?x8130 (stack_t x_0 w_3 x_CALLDATALOAD_0 3 ?x5797)))
 (let (($x1369 (= ?x8130 ?x3945)))
 (let (($x2163 (exc_halt_t 2)))
 (let (($x8830 (= $x2163 (or (exc_halt_t 1) (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_t 1))))))))
 (let (($x5925 (forall ((w (_ BitVec 256)) )(let ((?x6094 (storage_t x_0 w_3 x_CALLDATALOAD_0 1 w)))
 (let ((?x9134 (storage_t x_0 w_3 x_CALLDATALOAD_0 2 w)))
 (= ?x9134 ?x6094))))
 ))
 (let (($x2303 (forall ((n (_ BitVec 6)) )(let ((?x7154 (sc_t 1)))
 (let ((?x2239 (bvadd (_ bv63 6) ?x7154)))
 (let (($x2352 (bvsle ?x2239 n)))
 (let ((?x1828 (stack_t x_0 w_3 x_CALLDATALOAD_0 1 n)))
 (let ((?x11614 (stack_t x_0 w_3 x_CALLDATALOAD_0 2 n)))
 (let (($x81 (= ?x11614 ?x1828)))
 (or $x81 $x2352))))))))
 ))
 (let ((?x7154 (sc_t 1)))
 (let (($x6225 (= ?x2714 ?x7154)))
 (let ((?x11205 (used_gas_t x_0 w_3 x_CALLDATALOAD_0 2)))
 (let (($x7288 (= ?x11205 (+ 3 (used_gas_t x_0 w_3 x_CALLDATALOAD_0 1)))))
 (let ((?x2239 (bvadd (_ bv63 6) ?x7154)))
 (let ((?x5907 (stack_t x_0 w_3 x_CALLDATALOAD_0 1 ?x2239)))
 (let (($x8377 (exc_halt_t 1)))
 (let (($x7349 (= $x8377 (or $x56 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x63))) (_ bv0 1)))))))
 (let (($x3586 (forall ((w (_ BitVec 256)) )(let ((?x2415 (storage_t x_0 w_3 x_CALLDATALOAD_0 0 w)))
 (let ((?x6094 (storage_t x_0 w_3 x_CALLDATALOAD_0 1 w)))
 (= ?x6094 ?x2415))))
 ))
 (let (($x2991 (forall ((n (_ BitVec 6)) )(let ((?x3361 (stack_t x_0 w_3 x_CALLDATALOAD_0 0 n)))
 (let ((?x1828 (stack_t x_0 w_3 x_CALLDATALOAD_0 1 n)))
 (let ((?x63 (sc_t 0)))
 (let (($x2855 (bvsle ?x63 n)))
 (or $x2855 (= ?x1828 ?x3361)))))))
 ))
 (let (($x6089 (= ?x7154 (bvadd (_ bv1 6) ?x63))))
 (let ((?x1008 (used_gas_t x_0 w_3 x_CALLDATALOAD_0 1)))
 (let (($x3666 (= ?x1008 (+ 3 ?x7292))))
 (let (($x6694 (= (stack_t x_0 w_3 x_CALLDATALOAD_0 1 ?x63) w_3)))
 (let (($x8880 (= $x11317 (or (exc_halt_s 4) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 4))))))))
 (let (($x6401 (forall ((w (_ BitVec 256)) )(let ((?x9478 (storage_s x_0 w_3 x_CALLDATALOAD_0 4 w)))
 (let ((?x5929 (storage_s x_0 w_3 x_CALLDATALOAD_0 5 w)))
 (= ?x5929 ?x9478))))
 ))
 (let (($x4649 (forall ((n (_ BitVec 6)) )(let ((?x8223 (stack_s x_0 w_3 x_CALLDATALOAD_0 4 n)))
 (let ((?x8741 (stack_s x_0 w_3 x_CALLDATALOAD_0 5 n)))
 (let (($x8198 (= ?x8741 ?x8223)))
 (let ((?x4305 (sc_s 4)))
 (let ((?x5394 (bvadd (_ bv62 6) ?x4305)))
 (let (($x53 (bvsle ?x5394 n)))
 (or $x53 $x8198))))))))
 ))
 (let ((?x4305 (sc_s 4)))
 (let (($x9760 (= ?x4319 ?x4305)))
 (let ((?x8628 (used_gas_s x_0 w_3 x_CALLDATALOAD_0 5)))
 (let (($x1989 (= ?x8628 (+ 3 (used_gas_s x_0 w_3 x_CALLDATALOAD_0 4)))))
 (let ((?x4388 (bvadd (_ bv63 6) ?x4305)))
 (let ((?x3529 (stack_s x_0 w_3 x_CALLDATALOAD_0 4 ?x4388)))
 (let ((?x4138 (bvadd (_ bv62 6) ?x4319)))
 (let ((?x1114 (stack_s x_0 w_3 x_CALLDATALOAD_0 5 ?x4138)))
 (let (($x11217 (= ?x1114 ?x3529)))
 (let ((?x5394 (bvadd (_ bv62 6) ?x4305)))
 (let ((?x7402 (stack_s x_0 w_3 x_CALLDATALOAD_0 4 ?x5394)))
 (let ((?x8324 (bvadd (_ bv63 6) ?x4319)))
 (let ((?x5390 (stack_s x_0 w_3 x_CALLDATALOAD_0 5 ?x8324)))
 (let (($x450 (= ?x5390 ?x7402)))
 (let (($x7172 (exc_halt_s 4)))
 (let (($x7694 (= $x7172 (or (exc_halt_s 3) (not (bvsle (_ bv0 6) (bvadd (_ bv61 6) (sc_s 3))))))))
 (let (($x11062 (forall ((w (_ BitVec 256)) )(let ((?x1884 (storage_s x_0 w_3 x_CALLDATALOAD_0 3 w)))
 (let ((?x9478 (storage_s x_0 w_3 x_CALLDATALOAD_0 4 w)))
 (= ?x9478 ?x1884))))
 ))
 (let (($x5615 (forall ((n (_ BitVec 6)) )(let ((?x11624 (stack_s x_0 w_3 x_CALLDATALOAD_0 3 n)))
 (let ((?x8223 (stack_s x_0 w_3 x_CALLDATALOAD_0 4 n)))
 (let (($x8705 (= ?x8223 ?x11624)))
 (let ((?x275 (sc_s 3)))
 (let ((?x6308 (bvadd (_ bv61 6) ?x275)))
 (let (($x2513 (bvsle ?x6308 n)))
 (or $x2513 $x8705))))))))
 ))
 (let ((?x275 (sc_s 3)))
 (let (($x2594 (= ?x4305 ?x275)))
 (let ((?x2265 (used_gas_s x_0 w_3 x_CALLDATALOAD_0 4)))
 (let (($x4868 (= ?x2265 (+ 3 (used_gas_s x_0 w_3 x_CALLDATALOAD_0 3)))))
 (let ((?x9635 (bvadd (_ bv63 6) ?x275)))
 (let ((?x685 (stack_s x_0 w_3 x_CALLDATALOAD_0 3 ?x9635)))
 (let (($x292 (exc_halt_s 3)))
 (let (($x9269 (= $x292 (or (exc_halt_s 2) (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_s 2))))))))
 (let (($x921 (forall ((w (_ BitVec 256)) )(let ((?x9751 (storage_s x_0 w_3 x_CALLDATALOAD_0 2 w)))
 (let ((?x1884 (storage_s x_0 w_3 x_CALLDATALOAD_0 3 w)))
 (= ?x1884 ?x9751))))
 ))
 (let (($x6450 (forall ((n (_ BitVec 6)) )(let ((?x260 (stack_s x_0 w_3 x_CALLDATALOAD_0 2 n)))
 (let ((?x11624 (stack_s x_0 w_3 x_CALLDATALOAD_0 3 n)))
 (let (($x11517 (= ?x11624 ?x260)))
 (let ((?x218 (sc_s 2)))
 (let ((?x7113 (bvadd (_ bv63 6) ?x218)))
 (let (($x10404 (bvsle ?x7113 n)))
 (or $x10404 $x11517))))))))
 ))
 (let ((?x218 (sc_s 2)))
 (let (($x6815 (= ?x275 ?x218)))
 (let ((?x9595 (used_gas_s x_0 w_3 x_CALLDATALOAD_0 3)))
 (let (($x9206 (= ?x9595 (+ 3 (used_gas_s x_0 w_3 x_CALLDATALOAD_0 2)))))
 (let ((?x7113 (bvadd (_ bv63 6) ?x218)))
 (let ((?x11642 (stack_s x_0 w_3 x_CALLDATALOAD_0 2 ?x7113)))
 (let (($x7459 (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_s 1))))))
 (let (($x1803 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 1)))) (_ bv0 1)))))
 (let (($x189 (exc_halt_s 1)))
 (let (($x247 (exc_halt_s 2)))
 (let (($x355 (= $x247 (or $x189 $x1803 $x7459))))
 (let (($x157 (forall ((w (_ BitVec 256)) )(let ((?x2567 (storage_s x_0 w_3 x_CALLDATALOAD_0 1 w)))
 (let ((?x9751 (storage_s x_0 w_3 x_CALLDATALOAD_0 2 w)))
 (= ?x9751 ?x2567))))
 ))
 (let (($x2086 (forall ((n (_ BitVec 6)) )(let ((?x154 (sc_s 1)))
 (let ((?x11673 (bvadd (_ bv63 6) ?x154)))
 (let (($x10494 (bvsle ?x11673 n)))
 (let ((?x8755 (stack_s x_0 w_3 x_CALLDATALOAD_0 1 n)))
 (let ((?x260 (stack_s x_0 w_3 x_CALLDATALOAD_0 2 n)))
 (let (($x4135 (= ?x260 ?x8755)))
 (or $x4135 $x10494))))))))
 ))
 (let (($x11672 (= ?x218 (bvadd (_ bv1 6) (sc_s 1)))))
 (let ((?x9057 (used_gas_s x_0 w_3 x_CALLDATALOAD_0 2)))
 (let (($x2399 (= ?x9057 (+ 3 (used_gas_s x_0 w_3 x_CALLDATALOAD_0 1)))))
 (let ((?x154 (sc_s 1)))
 (let ((?x11673 (bvadd (_ bv63 6) ?x154)))
 (let ((?x8554 (stack_s x_0 w_3 x_CALLDATALOAD_0 1 ?x11673)))
 (let (($x1676 (= (stack_s x_0 w_3 x_CALLDATALOAD_0 2 ?x11673) ?x8554)))
 (let (($x7131 (= ?x11642 ?x8554)))
 (let (($x9208 (= $x189 (or $x57 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x72))) (_ bv0 1)))))))
 (let (($x1961 (forall ((w (_ BitVec 256)) )(let ((?x1808 (storage_s x_0 w_3 x_CALLDATALOAD_0 0 w)))
 (let ((?x2567 (storage_s x_0 w_3 x_CALLDATALOAD_0 1 w)))
 (= ?x2567 ?x1808))))
 ))
 (let (($x11541 (forall ((n (_ BitVec 6)) )(let ((?x72 (sc_s 0)))
 (let (($x10612 (bvsle ?x72 n)))
 (let ((?x7523 (stack_s x_0 w_3 x_CALLDATALOAD_0 0 n)))
 (let ((?x8755 (stack_s x_0 w_3 x_CALLDATALOAD_0 1 n)))
 (or (= ?x8755 ?x7523) $x10612))))))
 ))
 (let (($x6169 (= ?x154 (bvadd (_ bv1 6) ?x72))))
 (let ((?x5069 (used_gas_s x_0 w_3 x_CALLDATALOAD_0 1)))
 (let (($x2073 (= ?x5069 (+ 3 ?x11377))))
 (let (($x4032 (= (stack_s x_0 w_3 x_CALLDATALOAD_0 1 ?x72) w_3)))
 (let (($x3354 (forall ((w0 (_ BitVec 256)) )(let ((?x9090 (ite (= (stack_s x_0 w_3 x_CALLDATALOAD_0 2 (bvadd (_ bv63 6) (sc_s 2))) w0) x_CALLDATALOAD_0 (_ bv0 256))))
 (let ((?x1708 (f_CALLDATALOAD x_0 w_3 x_CALLDATALOAD_0 w0)))
 (= ?x1708 ?x9090))))
 ))
 (let (($x8031 (forall ((w (_ BitVec 256)) )(let ((?x1808 (storage_s x_0 w_3 x_CALLDATALOAD_0 0 w)))
 (= ?x1808 (_ bv0 256))))
 ))
 (let (($x1660 (= ?x11377 0)))
 (let (($x10766 (not $x57)))
 (let (($x10370 (= (stack_s x_0 w_3 x_CALLDATALOAD_0 0 (_ bv0 6)) x_0)))
 (let (($x7865 (= ?x72 (_ bv1 6))))
 (and $x7865 $x10370 $x10766 $x1660 $x8031 $x3354 $x4032 $x2073 $x6169 $x11541 $x1961 $x9208 $x7131 $x1676 $x2399 $x11672 $x2086 $x157 $x355 (= ?x685 (f_CALLDATALOAD x_0 w_3 x_CALLDATALOAD_0 ?x11642)) $x9206 $x6815 $x6450 $x921 $x9269 (= ?x3529 (stack_s x_0 w_3 x_CALLDATALOAD_0 3 (bvadd (_ bv61 6) ?x275))) (= (stack_s x_0 w_3 x_CALLDATALOAD_0 4 (bvadd (_ bv61 6) ?x4305)) ?x685) (= ?x7402 (stack_s x_0 w_3 x_CALLDATALOAD_0 3 (bvadd (_ bv62 6) ?x275))) $x4868 $x2594 $x5615 $x11062 $x7694 $x450 $x11217 $x1989 $x9760 $x4649 $x6401 $x8880 $x6694 $x3666 $x6089 $x2991 $x3586 $x7349 (= ?x7475 (f_CALLDATALOAD x_0 w_3 x_CALLDATALOAD_0 ?x5907)) $x7288 $x6225 $x2303 $x5925 $x8830 $x1369 (= ?x5475 ?x7475) $x10139 $x4836 $x3551 $x4773 $x503 (= (stack_t x_0 w_3 x_CALLDATALOAD_0 4 ?x6438) w_3) (= ?x11159 (+ 3 ?x11615)) $x10007 $x11374 $x2209 $x3184 $x73 $x7526 $x58 $x11084 $x2182 $x10682)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
 )
(check-sat)
(get-proof)