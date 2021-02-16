; SWAP1 DUP2 SWAP1 DUP2 DUP1 DUP1 => DUP1 SWAP2 DUP3 DUP3 DUP5
(set-option :produce-proofs true)
; 
(set-info :status unknown)
(declare-fun storage_t ((_ BitVec 256) (_ BitVec 256) Int (_ BitVec 256)) (_ BitVec 256))
(declare-fun storage_s ((_ BitVec 256) (_ BitVec 256) Int (_ BitVec 256)) (_ BitVec 256))
(declare-fun exc_halt_t (Int) Bool)
(declare-fun exc_halt_s (Int) Bool)
(declare-fun stack_t ((_ BitVec 256) (_ BitVec 256) Int (_ BitVec 6)) (_ BitVec 256))
(declare-fun stack_s ((_ BitVec 256) (_ BitVec 256) Int (_ BitVec 6)) (_ BitVec 256))
(declare-fun sc_t (Int) (_ BitVec 6))
(declare-fun sc_s (Int) (_ BitVec 6))
(declare-fun used_gas_t ((_ BitVec 256) (_ BitVec 256) Int) Int)
(declare-fun used_gas_s ((_ BitVec 256) (_ BitVec 256) Int) Int)
(assert
 (exists ((x_0 (_ BitVec 256)) (x_1 (_ BitVec 256)) )(let (($x11664 (forall ((w (_ BitVec 256)) )(let ((?x5595 (storage_t x_0 x_1 5 w)))
 (let ((?x10812 (storage_s x_0 x_1 6 w)))
 (= ?x10812 ?x5595))))
 ))
 (let (($x886 (exc_halt_t 5)))
 (let (($x772 (exc_halt_s 6)))
 (let (($x889 (= $x772 $x886)))
 (let (($x8488 (forall ((n (_ BitVec 6)) )(let ((?x6644 (stack_t x_0 x_1 5 n)))
 (let ((?x9243 (stack_s x_0 x_1 6 n)))
 (let (($x3696 (= ?x9243 ?x6644)))
 (or (bvsle (sc_t 5) n) $x3696)))))
 ))
 (let ((?x919 (sc_t 5)))
 (let ((?x926 (sc_s 6)))
 (let (($x929 (= ?x926 ?x919)))
 (let ((?x9098 (used_gas_t x_0 x_1 0)))
 (let ((?x4164 (used_gas_s x_0 x_1 0)))
 (let (($x5786 (= ?x4164 ?x9098)))
 (let (($x8240 (forall ((w (_ BitVec 256)) )(let ((?x4845 (storage_t x_0 x_1 0 w)))
 (let ((?x8680 (storage_s x_0 x_1 0 w)))
 (= ?x8680 ?x4845))))
 ))
 (let (($x56 (exc_halt_t 0)))
 (let (($x57 (exc_halt_s 0)))
 (let (($x58 (= $x57 $x56)))
 (let (($x10850 (forall ((n (_ BitVec 6)) )(let ((?x10600 (stack_t x_0 x_1 0 n)))
 (let ((?x1080 (stack_s x_0 x_1 0 n)))
 (let (($x6607 (= ?x1080 ?x10600)))
 (let ((?x63 (sc_t 0)))
 (let (($x5025 (bvsle ?x63 n)))
 (or $x5025 $x6607)))))))
 ))
 (let ((?x63 (sc_t 0)))
 (let ((?x72 (sc_s 0)))
 (let (($x73 (= ?x72 ?x63)))
 (let (($x3723 (exc_halt_t 4)))
 (let (($x11481 (or (not (bvsle (_ bv0 6) (bvadd (_ bv59 6) (sc_t 4)))) $x3723 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_t 4)))) (_ bv0 1))))))
 (let (($x3256 (forall ((w (_ BitVec 256)) )(let ((?x9050 (storage_t x_0 x_1 4 w)))
 (let ((?x5595 (storage_t x_0 x_1 5 w)))
 (= ?x5595 ?x9050))))
 ))
 (let (($x10782 (forall ((n (_ BitVec 6)) )(or (bvsle (bvadd (_ bv59 6) (sc_t 4)) n) (= (stack_t x_0 x_1 5 n) (stack_t x_0 x_1 4 n))))
 ))
 (let (($x4300 (= (used_gas_t x_0 x_1 5) (+ 3 (used_gas_t x_0 x_1 4)))))
 (let ((?x3757 (sc_t 4)))
 (let ((?x6188 (bvadd (_ bv63 6) ?x3757)))
 (let ((?x5908 (stack_t x_0 x_1 4 ?x6188)))
 (let ((?x5334 (bvadd (_ bv62 6) ?x3757)))
 (let ((?x1511 (stack_t x_0 x_1 4 ?x5334)))
 (let ((?x186 (bvadd (_ bv61 6) ?x3757)))
 (let ((?x2118 (stack_t x_0 x_1 4 ?x186)))
 (let ((?x4444 (bvadd (_ bv60 6) ?x3757)))
 (let ((?x9539 (stack_t x_0 x_1 4 ?x4444)))
 (let ((?x10220 (bvadd (_ bv59 6) ?x3757)))
 (let ((?x8995 (stack_t x_0 x_1 4 ?x10220)))
 (let (($x2900 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_t 3)))) (_ bv0 1)))))
 (let (($x1097 (exc_halt_t 3)))
 (let (($x9930 (forall ((w (_ BitVec 256)) )(let ((?x9825 (storage_t x_0 x_1 3 w)))
 (let ((?x9050 (storage_t x_0 x_1 4 w)))
 (= ?x9050 ?x9825))))
 ))
 (let (($x10314 (forall ((n (_ BitVec 6)) )(let ((?x2495 (stack_t x_0 x_1 3 n)))
 (let ((?x5855 (stack_t x_0 x_1 4 n)))
 (let (($x7286 (= ?x5855 ?x2495)))
 (or (bvsle (bvadd (_ bv61 6) (sc_t 3)) n) $x7286)))))
 ))
 (let (($x9090 (= ?x3757 (bvadd (_ bv1 6) (sc_t 3)))))
 (let ((?x1901 (used_gas_t x_0 x_1 4)))
 (let (($x4001 (= ?x1901 (+ 3 (used_gas_t x_0 x_1 3)))))
 (let ((?x6011 (sc_t 3)))
 (let ((?x10160 (bvadd (_ bv63 6) ?x6011)))
 (let ((?x7364 (stack_t x_0 x_1 3 ?x10160)))
 (let ((?x10298 (bvadd (_ bv62 6) ?x6011)))
 (let ((?x9841 (stack_t x_0 x_1 3 ?x10298)))
 (let ((?x5475 (bvadd (_ bv61 6) ?x6011)))
 (let ((?x3189 (stack_t x_0 x_1 3 ?x5475)))
 (let (($x6639 (not (bvsle (_ bv0 6) (bvadd (_ bv61 6) (sc_t 2))))))
 (let (($x5252 (exc_halt_t 2)))
 (let (($x3975 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_t 2)))) (_ bv0 1)))))
 (let (($x462 (forall ((w (_ BitVec 256)) )(let ((?x1396 (storage_t x_0 x_1 2 w)))
 (let ((?x9825 (storage_t x_0 x_1 3 w)))
 (= ?x9825 ?x1396))))
 ))
 (let (($x4462 (forall ((n (_ BitVec 6)) )(let ((?x4573 (stack_t x_0 x_1 2 n)))
 (let ((?x2495 (stack_t x_0 x_1 3 n)))
 (let (($x9582 (= ?x2495 ?x4573)))
 (let ((?x2714 (sc_t 2)))
 (let ((?x1481 (bvadd (_ bv61 6) ?x2714)))
 (let (($x892 (bvsle ?x1481 n)))
 (or $x892 $x9582))))))))
 ))
 (let (($x1161 (= ?x6011 (bvadd (_ bv1 6) (sc_t 2)))))
 (let ((?x1547 (used_gas_t x_0 x_1 3)))
 (let (($x7316 (= ?x1547 (+ 3 (used_gas_t x_0 x_1 2)))))
 (let ((?x2714 (sc_t 2)))
 (let ((?x270 (bvadd (_ bv63 6) ?x2714)))
 (let ((?x2575 (stack_t x_0 x_1 2 ?x270)))
 (let (($x9889 (= (stack_t x_0 x_1 3 ?x270) ?x2575)))
 (let ((?x3738 (bvadd (_ bv62 6) ?x2714)))
 (let ((?x295 (stack_t x_0 x_1 2 ?x3738)))
 (let (($x2611 (= (stack_t x_0 x_1 3 ?x3738) ?x295)))
 (let ((?x1481 (bvadd (_ bv61 6) ?x2714)))
 (let ((?x4244 (stack_t x_0 x_1 2 ?x1481)))
 (let (($x5066 (= (stack_t x_0 x_1 3 ?x1481) ?x4244)))
 (let (($x2457 (= $x5252 (or (exc_halt_t 1) (not (bvsle (_ bv0 6) (bvadd (_ bv61 6) (sc_t 1))))))))
 (let (($x7631 (forall ((w (_ BitVec 256)) )(let ((?x5698 (storage_t x_0 x_1 1 w)))
 (let ((?x1396 (storage_t x_0 x_1 2 w)))
 (= ?x1396 ?x5698))))
 ))
 (let (($x2034 (forall ((n (_ BitVec 6)) )(or (= (stack_t x_0 x_1 2 n) (stack_t x_0 x_1 1 n)) (bvsle (bvadd (_ bv61 6) (sc_t 1)) n)))
 ))
 (let ((?x6052 (sc_t 1)))
 (let (($x8971 (= ?x2714 ?x6052)))
 (let ((?x3149 (used_gas_t x_0 x_1 2)))
 (let (($x4577 (= ?x3149 (+ 3 (used_gas_t x_0 x_1 1)))))
 (let ((?x8956 (bvadd (_ bv61 6) ?x6052)))
 (let ((?x5206 (stack_t x_0 x_1 1 ?x8956)))
 (let (($x8526 (= ?x2575 ?x5206)))
 (let (($x6067 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x63))) (_ bv0 1)))))
 (let (($x1117 (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) ?x63)))))
 (let (($x3120 (exc_halt_t 1)))
 (let (($x3663 (= $x3120 (or $x56 $x1117 $x6067))))
 (let (($x11025 (forall ((w (_ BitVec 256)) )(let ((?x4845 (storage_t x_0 x_1 0 w)))
 (let ((?x5698 (storage_t x_0 x_1 1 w)))
 (= ?x5698 ?x4845))))
 ))
 (let (($x9451 (forall ((n (_ BitVec 6)) )(let ((?x63 (sc_t 0)))
 (let ((?x3437 (bvadd (_ bv63 6) ?x63)))
 (let (($x5581 (bvsle ?x3437 n)))
 (let ((?x10600 (stack_t x_0 x_1 0 n)))
 (let ((?x1483 (stack_t x_0 x_1 1 n)))
 (let (($x2682 (= ?x1483 ?x10600)))
 (or $x2682 $x5581))))))))
 ))
 (let (($x4139 (= ?x6052 (bvadd (_ bv1 6) ?x63))))
 (let ((?x5326 (used_gas_t x_0 x_1 1)))
 (let (($x11803 (= ?x5326 (+ 3 ?x9098))))
 (let ((?x3437 (bvadd (_ bv63 6) ?x63)))
 (let ((?x10614 (stack_t x_0 x_1 0 ?x3437)))
 (let (($x634 (= (stack_t x_0 x_1 1 ?x3437) ?x10614)))
 (let (($x3979 (exc_halt_s 5)))
 (let (($x5870 (or (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 5)))) (_ bv0 1))) $x3979 (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_s 5)))))))
 (let (($x10642 (forall ((w (_ BitVec 256)) )(let ((?x6541 (storage_s x_0 x_1 5 w)))
 (let ((?x10812 (storage_s x_0 x_1 6 w)))
 (= ?x10812 ?x6541))))
 ))
 (let (($x8284 (forall ((n (_ BitVec 6)) )(let ((?x3839 (stack_s x_0 x_1 5 n)))
 (let ((?x9243 (stack_s x_0 x_1 6 n)))
 (let (($x10922 (= ?x9243 ?x3839)))
 (or (bvsle (bvadd (_ bv63 6) (sc_s 5)) n) $x10922)))))
 ))
 (let (($x10097 (= (used_gas_s x_0 x_1 6) (+ 3 (used_gas_s x_0 x_1 5)))))
 (let ((?x805 (sc_s 5)))
 (let ((?x9431 (bvadd (_ bv63 6) ?x805)))
 (let ((?x3338 (stack_s x_0 x_1 5 ?x9431)))
 (let (($x7172 (exc_halt_s 4)))
 (let (($x4535 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 4)))) (_ bv0 1)))))
 (let (($x3234 (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_s 4))))))
 (let (($x11116 (forall ((w (_ BitVec 256)) )(let ((?x10268 (storage_s x_0 x_1 4 w)))
 (let ((?x6541 (storage_s x_0 x_1 5 w)))
 (= ?x6541 ?x10268))))
 ))
 (let (($x5956 (forall ((n (_ BitVec 6)) )(let ((?x2973 (stack_s x_0 x_1 4 n)))
 (let ((?x3839 (stack_s x_0 x_1 5 n)))
 (let (($x9118 (= ?x3839 ?x2973)))
 (let ((?x4305 (sc_s 4)))
 (let ((?x1110 (bvadd (_ bv63 6) ?x4305)))
 (let (($x2321 (bvsle ?x1110 n)))
 (or $x2321 $x9118))))))))
 ))
 (let (($x11113 (= ?x805 (bvadd (_ bv1 6) (sc_s 4)))))
 (let ((?x9799 (used_gas_s x_0 x_1 5)))
 (let (($x6150 (= ?x9799 (+ 3 (used_gas_s x_0 x_1 4)))))
 (let ((?x4305 (sc_s 4)))
 (let ((?x1110 (bvadd (_ bv63 6) ?x4305)))
 (let ((?x3460 (stack_s x_0 x_1 4 ?x1110)))
 (let (($x8079 (= (stack_s x_0 x_1 5 ?x1110) ?x3460)))
 (let (($x5075 (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 3))))))
 (let (($x3194 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 3)))) (_ bv0 1)))))
 (let (($x292 (exc_halt_s 3)))
 (let (($x1387 (forall ((w (_ BitVec 256)) )(let ((?x1088 (storage_s x_0 x_1 3 w)))
 (let ((?x10268 (storage_s x_0 x_1 4 w)))
 (= ?x10268 ?x1088))))
 ))
 (let (($x1647 (forall ((n (_ BitVec 6)) )(let ((?x275 (sc_s 3)))
 (let ((?x7283 (bvadd (_ bv62 6) ?x275)))
 (let (($x49 (bvsle ?x7283 n)))
 (or $x49 (= (stack_s x_0 x_1 4 n) (stack_s x_0 x_1 3 n)))))))
 ))
 (let (($x8427 (= ?x4305 (bvadd (_ bv1 6) (sc_s 3)))))
 (let ((?x11305 (used_gas_s x_0 x_1 4)))
 (let (($x5243 (= ?x11305 (+ 3 (used_gas_s x_0 x_1 3)))))
 (let ((?x275 (sc_s 3)))
 (let ((?x7071 (bvadd (_ bv63 6) ?x275)))
 (let ((?x6364 (stack_s x_0 x_1 3 ?x7071)))
 (let ((?x7283 (bvadd (_ bv62 6) ?x275)))
 (let ((?x7129 (stack_s x_0 x_1 3 ?x7283)))
 (let (($x5622 (= ?x3460 ?x7129)))
 (let (($x2572 (= $x292 (or (exc_halt_s 2) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 2))))))))
 (let (($x10423 (forall ((w (_ BitVec 256)) )(let ((?x5347 (storage_s x_0 x_1 2 w)))
 (let ((?x1088 (storage_s x_0 x_1 3 w)))
 (= ?x1088 ?x5347))))
 ))
 (let (($x615 (forall ((n (_ BitVec 6)) )(let ((?x218 (sc_s 2)))
 (let ((?x7367 (bvadd (_ bv62 6) ?x218)))
 (let (($x2513 (bvsle ?x7367 n)))
 (or (= (stack_s x_0 x_1 3 n) (stack_s x_0 x_1 2 n)) $x2513)))))
 ))
 (let ((?x218 (sc_s 2)))
 (let (($x5891 (= ?x275 ?x218)))
 (let ((?x4963 (used_gas_s x_0 x_1 3)))
 (let (($x7292 (= ?x4963 (+ 3 (used_gas_s x_0 x_1 2)))))
 (let ((?x7367 (bvadd (_ bv62 6) ?x218)))
 (let ((?x2562 (stack_s x_0 x_1 2 ?x7367)))
 (let (($x6728 (= ?x6364 ?x2562)))
 (let (($x4106 (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 1))))))
 (let (($x2265 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 1)))) (_ bv0 1)))))
 (let (($x189 (exc_halt_s 1)))
 (let (($x247 (exc_halt_s 2)))
 (let (($x261 (forall ((w (_ BitVec 256)) )(let ((?x8762 (storage_s x_0 x_1 1 w)))
 (let ((?x5347 (storage_s x_0 x_1 2 w)))
 (= ?x5347 ?x8762))))
 ))
 (let (($x1727 (forall ((n (_ BitVec 6)) )(let ((?x154 (sc_s 1)))
 (let ((?x7504 (bvadd (_ bv62 6) ?x154)))
 (let (($x10307 (bvsle ?x7504 n)))
 (or $x10307 (= (stack_s x_0 x_1 2 n) (stack_s x_0 x_1 1 n)))))))
 ))
 (let (($x1555 (= ?x218 (bvadd (_ bv1 6) (sc_s 1)))))
 (let ((?x1830 (used_gas_s x_0 x_1 2)))
 (let (($x9791 (= ?x1830 (+ 3 (used_gas_s x_0 x_1 1)))))
 (let ((?x154 (sc_s 1)))
 (let ((?x9976 (bvadd (_ bv63 6) ?x154)))
 (let ((?x6355 (stack_s x_0 x_1 1 ?x9976)))
 (let ((?x7504 (bvadd (_ bv62 6) ?x154)))
 (let ((?x92 (stack_s x_0 x_1 1 ?x7504)))
 (let ((?x5148 (bvadd (_ bv63 6) ?x218)))
 (let ((?x7849 (stack_s x_0 x_1 2 ?x5148)))
 (let (($x7673 (= ?x7849 ?x92)))
 (let (($x5562 (forall ((w (_ BitVec 256)) )(let ((?x8680 (storage_s x_0 x_1 0 w)))
 (let ((?x8762 (storage_s x_0 x_1 1 w)))
 (= ?x8762 ?x8680))))
 ))
 (let (($x8990 (forall ((n (_ BitVec 6)) )(or (= (stack_s x_0 x_1 1 n) (stack_s x_0 x_1 0 n)) (bvsle (bvadd (_ bv62 6) (sc_s 0)) n)))
 ))
 (let ((?x9365 (used_gas_s x_0 x_1 1)))
 (let (($x8863 (= ?x9365 (+ 3 ?x4164))))
 (let ((?x2590 (bvadd (_ bv62 6) ?x72)))
 (let ((?x8456 (stack_s x_0 x_1 0 ?x2590)))
 (let (($x4053 (= ?x6355 ?x8456)))
 (let (($x468 (forall ((w (_ BitVec 256)) )(let ((?x8680 (storage_s x_0 x_1 0 w)))
 (= ?x8680 (_ bv0 256))))
 ))
 (let (($x579 (= ?x4164 0)))
 (let (($x5167 (not $x57)))
 (let (($x10798 (= (stack_s x_0 x_1 0 (_ bv1 6)) x_1)))
 (let (($x8174 (= (stack_s x_0 x_1 0 (_ bv0 6)) x_0)))
 (let (($x1660 (= ?x72 (_ bv2 6))))
 (and $x1660 $x8174 $x10798 $x5167 $x579 $x468 $x4053 (= ?x92 (stack_s x_0 x_1 0 (bvadd (_ bv63 6) ?x72))) $x8863 (= ?x154 ?x72) $x8990 $x5562 (= $x189 (or $x57 (not (bvsle (_ bv0 6) ?x2590)))) $x7673 (= (stack_s x_0 x_1 2 ?x7504) ?x92) (= (stack_s x_0 x_1 2 ?x9976) ?x6355) $x9791 $x1555 $x1727 $x261 (= $x247 (or $x189 $x2265 $x4106)) $x6728 (= ?x7129 ?x7849) $x7292 $x5891 $x615 $x10423 $x2572 $x5622 (= (stack_s x_0 x_1 4 ?x7283) ?x7129) (= (stack_s x_0 x_1 4 ?x7071) ?x6364) $x5243 $x8427 $x1647 $x1387 (= $x7172 (or $x292 $x3194 $x5075)) (= ?x3338 ?x3460) $x8079 $x6150 $x11113 $x5956 $x11116 (= $x3979 (or $x3234 $x4535 $x7172)) (= (stack_s x_0 x_1 6 (bvadd (_ bv63 6) ?x926)) ?x3338) (= (stack_s x_0 x_1 6 ?x9431) ?x3338) $x10097 (= ?x926 (bvadd (_ bv1 6) ?x805)) $x8284 $x10642 (= $x772 $x5870) (= (stack_t x_0 x_1 1 (bvadd (_ bv63 6) ?x6052)) ?x10614) $x634 $x11803 $x4139 $x9451 $x11025 $x3663 $x8526 (= ?x4244 (stack_t x_0 x_1 1 (bvadd (_ bv63 6) ?x6052))) (= ?x295 (stack_t x_0 x_1 1 (bvadd (_ bv62 6) ?x6052))) $x4577 $x8971 $x2034 $x7631 $x2457 (= ?x7364 ?x4244) $x5066 $x2611 $x9889 $x7316 $x1161 $x4462 $x462 (= $x1097 (or $x3975 $x5252 $x6639)) (= ?x5908 ?x3189) (= (stack_t x_0 x_1 4 ?x5475) ?x3189) (= (stack_t x_0 x_1 4 ?x10298) ?x9841) (= (stack_t x_0 x_1 4 ?x10160) ?x7364) $x4001 $x9090 $x10314 $x9930 (= $x3723 (or $x1097 $x2900 (not (bvsle (_ bv0 6) ?x5475)))) (= (stack_t x_0 x_1 5 (bvadd (_ bv63 6) ?x919)) ?x8995) (= (stack_t x_0 x_1 5 ?x10220) ?x8995) (= (stack_t x_0 x_1 5 ?x4444) ?x9539) (= (stack_t x_0 x_1 5 ?x186) ?x2118) (= (stack_t x_0 x_1 5 ?x5334) ?x1511) (= (stack_t x_0 x_1 5 ?x6188) ?x5908) $x4300 (= ?x919 (bvadd (_ bv1 6) ?x3757)) $x10782 $x3256 (= $x886 $x11481) $x73 $x10850 $x58 $x8240 $x5786 (not (and $x929 $x8488 $x889 $x11664)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
 )
(check-sat)
(get-proof)