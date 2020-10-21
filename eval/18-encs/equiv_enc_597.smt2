; SWAP1 DUP2 SWAP1 DUP4 SWAP1 DUP2 => DUP3 DUP2 SWAP3 DUP5
(set-option :produce-proofs true)
; 
(set-info :status unknown)
(declare-fun storage_t ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 256)) (_ BitVec 256))
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
 (exists ((x_0 (_ BitVec 256)) (x_1 (_ BitVec 256)) (x_2 (_ BitVec 256)) )(let (($x4613 (forall ((w (_ BitVec 256)) )(let ((?x7467 (storage_t x_0 x_1 x_2 4 w)))
 (let ((?x7388 (storage_s x_0 x_1 x_2 6 w)))
 (= ?x7388 ?x7467))))
 ))
 (let (($x7854 (exc_halt_t 4)))
 (let (($x772 (exc_halt_s 6)))
 (let (($x4404 (= $x772 $x7854)))
 (let (($x3897 (forall ((n (_ BitVec 6)) )(let ((?x4818 (sc_t 4)))
 (let (($x7387 (bvsle ?x4818 n)))
 (let ((?x329 (stack_t x_0 x_1 x_2 4 n)))
 (let ((?x10855 (stack_s x_0 x_1 x_2 6 n)))
 (let (($x3585 (= ?x10855 ?x329)))
 (or $x3585 $x7387)))))))
 ))
 (let ((?x4818 (sc_t 4)))
 (let ((?x926 (sc_s 6)))
 (let (($x7899 (= ?x926 ?x4818)))
 (let ((?x5852 (used_gas_t x_0 x_1 x_2 0)))
 (let ((?x3411 (used_gas_s x_0 x_1 x_2 0)))
 (let (($x3707 (= ?x3411 ?x5852)))
 (let (($x2479 (forall ((w (_ BitVec 256)) )(let ((?x3910 (storage_t x_0 x_1 x_2 0 w)))
 (let ((?x793 (storage_s x_0 x_1 x_2 0 w)))
 (= ?x793 ?x3910))))
 ))
 (let (($x56 (exc_halt_t 0)))
 (let (($x57 (exc_halt_s 0)))
 (let (($x58 (= $x57 $x56)))
 (let (($x11810 (forall ((n (_ BitVec 6)) )(let ((?x63 (sc_t 0)))
 (let (($x10756 (bvsle ?x63 n)))
 (let ((?x762 (stack_t x_0 x_1 x_2 0 n)))
 (let ((?x10131 (stack_s x_0 x_1 x_2 0 n)))
 (let (($x3407 (= ?x10131 ?x762)))
 (or $x3407 $x10756)))))))
 ))
 (let ((?x63 (sc_t 0)))
 (let ((?x72 (sc_s 0)))
 (let (($x73 (= ?x72 ?x63)))
 (let (($x9834 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_t 3)))) (_ bv0 1)))))
 (let (($x3614 (exc_halt_t 3)))
 (let (($x2665 (forall ((w (_ BitVec 256)) )(let ((?x8780 (storage_t x_0 x_1 x_2 3 w)))
 (let ((?x7467 (storage_t x_0 x_1 x_2 4 w)))
 (= ?x7467 ?x8780))))
 ))
 (let (($x6342 (forall ((n (_ BitVec 6)) )(let ((?x2703 (stack_t x_0 x_1 x_2 3 n)))
 (let ((?x329 (stack_t x_0 x_1 x_2 4 n)))
 (or (= ?x329 ?x2703) (bvsle (bvadd (_ bv59 6) (sc_t 3)) n)))))
 ))
 (let (($x5041 (= ?x4818 (bvadd (_ bv1 6) (sc_t 3)))))
 (let (($x2313 (= (used_gas_t x_0 x_1 x_2 4) (+ 3 (used_gas_t x_0 x_1 x_2 3)))))
 (let ((?x11304 (sc_t 3)))
 (let ((?x8209 (bvadd (_ bv63 6) ?x11304)))
 (let ((?x2291 (stack_t x_0 x_1 x_2 3 ?x8209)))
 (let ((?x8334 (bvadd (_ bv62 6) ?x11304)))
 (let ((?x3406 (stack_t x_0 x_1 x_2 3 ?x8334)))
 (let ((?x9203 (bvadd (_ bv61 6) ?x11304)))
 (let ((?x1455 (stack_t x_0 x_1 x_2 3 ?x9203)))
 (let ((?x8993 (bvadd (_ bv60 6) ?x11304)))
 (let ((?x10039 (stack_t x_0 x_1 x_2 3 ?x8993)))
 (let ((?x8691 (bvadd (_ bv59 6) ?x11304)))
 (let ((?x5625 (stack_t x_0 x_1 x_2 3 ?x8691)))
 (let (($x9569 (= $x3614 (or (exc_halt_t 2) (not (bvsle (_ bv0 6) (bvadd (_ bv60 6) (sc_t 2))))))))
 (let (($x6586 (forall ((w (_ BitVec 256)) )(let ((?x8235 (storage_t x_0 x_1 x_2 2 w)))
 (let ((?x8780 (storage_t x_0 x_1 x_2 3 w)))
 (= ?x8780 ?x8235))))
 ))
 (let (($x10838 (forall ((n (_ BitVec 6)) )(let ((?x5485 (stack_t x_0 x_1 x_2 2 n)))
 (let ((?x2703 (stack_t x_0 x_1 x_2 3 n)))
 (let (($x10275 (= ?x2703 ?x5485)))
 (or (bvsle (bvadd (_ bv60 6) (sc_t 2)) n) $x10275)))))
 ))
 (let ((?x2714 (sc_t 2)))
 (let (($x9044 (= ?x11304 ?x2714)))
 (let ((?x897 (used_gas_t x_0 x_1 x_2 3)))
 (let (($x3927 (= ?x897 (+ 3 (used_gas_t x_0 x_1 x_2 2)))))
 (let (($x10555 (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_t 1))))))
 (let (($x8951 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_t 1)))) (_ bv0 1)))))
 (let (($x8377 (exc_halt_t 1)))
 (let (($x4500 (exc_halt_t 2)))
 (let (($x3305 (forall ((w (_ BitVec 256)) )(let ((?x10732 (storage_t x_0 x_1 x_2 1 w)))
 (let ((?x8235 (storage_t x_0 x_1 x_2 2 w)))
 (= ?x8235 ?x10732))))
 ))
 (let (($x214 (forall ((n (_ BitVec 6)) )(let ((?x7154 (sc_t 1)))
 (let ((?x5672 (bvadd (_ bv62 6) ?x7154)))
 (let (($x6513 (bvsle ?x5672 n)))
 (let ((?x2250 (stack_t x_0 x_1 x_2 1 n)))
 (let ((?x5485 (stack_t x_0 x_1 x_2 2 n)))
 (let (($x5187 (= ?x5485 ?x2250)))
 (or $x5187 $x6513))))))))
 ))
 (let (($x10940 (= ?x2714 (bvadd (_ bv1 6) (sc_t 1)))))
 (let ((?x6171 (used_gas_t x_0 x_1 x_2 2)))
 (let ((?x7154 (sc_t 1)))
 (let ((?x1361 (bvadd (_ bv63 6) ?x7154)))
 (let ((?x1543 (stack_t x_0 x_1 x_2 1 ?x1361)))
 (let ((?x5672 (bvadd (_ bv62 6) ?x7154)))
 (let ((?x9871 (stack_t x_0 x_1 x_2 1 ?x5672)))
 (let (($x3762 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x63))) (_ bv0 1)))))
 (let (($x6573 (forall ((w (_ BitVec 256)) )(let ((?x3910 (storage_t x_0 x_1 x_2 0 w)))
 (let ((?x10732 (storage_t x_0 x_1 x_2 1 w)))
 (= ?x10732 ?x3910))))
 ))
 (let (($x4420 (forall ((n (_ BitVec 6)) )(let ((?x762 (stack_t x_0 x_1 x_2 0 n)))
 (let ((?x2250 (stack_t x_0 x_1 x_2 1 n)))
 (let (($x6149 (= ?x2250 ?x762)))
 (or $x6149 (bvsle (bvadd (_ bv61 6) (sc_t 0)) n))))))
 ))
 (let (($x3866 (= ?x7154 (bvadd (_ bv1 6) ?x63))))
 (let (($x8607 (= (stack_t x_0 x_1 x_2 1 (bvadd (_ bv63 6) ?x63)) (stack_t x_0 x_1 x_2 0 (bvadd (_ bv63 6) ?x63)))))
 (let (($x1085 (= (stack_t x_0 x_1 x_2 1 (bvadd (_ bv62 6) ?x63)) (stack_t x_0 x_1 x_2 0 (bvadd (_ bv62 6) ?x63)))))
 (let ((?x7221 (bvadd (_ bv61 6) ?x63)))
 (let ((?x9398 (stack_t x_0 x_1 x_2 0 ?x7221)))
 (let (($x11317 (exc_halt_s 5)))
 (let (($x5080 (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 5))))))
 (let (($x9086 (or $x5080 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 5)))) (_ bv0 1))) $x11317)))
 (let (($x5707 (forall ((w (_ BitVec 256)) )(let ((?x1821 (storage_s x_0 x_1 x_2 5 w)))
 (let ((?x7388 (storage_s x_0 x_1 x_2 6 w)))
 (= ?x7388 ?x1821))))
 ))
 (let (($x4182 (forall ((n (_ BitVec 6)) )(let ((?x2862 (stack_s x_0 x_1 x_2 5 n)))
 (let ((?x10855 (stack_s x_0 x_1 x_2 6 n)))
 (let (($x7201 (= ?x10855 ?x2862)))
 (let ((?x4319 (sc_s 5)))
 (let ((?x1846 (bvadd (_ bv62 6) ?x4319)))
 (let (($x3036 (bvsle ?x1846 n)))
 (or $x3036 $x7201))))))))
 ))
 (let (($x4660 (= (used_gas_s x_0 x_1 x_2 6) (+ 3 (used_gas_s x_0 x_1 x_2 5)))))
 (let ((?x4319 (sc_s 5)))
 (let ((?x669 (bvadd (_ bv63 6) ?x4319)))
 (let ((?x7713 (stack_s x_0 x_1 x_2 5 ?x669)))
 (let ((?x1846 (bvadd (_ bv62 6) ?x4319)))
 (let ((?x2760 (stack_s x_0 x_1 x_2 5 ?x1846)))
 (let (($x9721 (= $x11317 (or (exc_halt_s 4) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 4))))))))
 (let (($x6645 (forall ((w (_ BitVec 256)) )(let ((?x8154 (storage_s x_0 x_1 x_2 4 w)))
 (let ((?x1821 (storage_s x_0 x_1 x_2 5 w)))
 (= ?x1821 ?x8154))))
 ))
 (let (($x10896 (forall ((n (_ BitVec 6)) )(let ((?x9580 (stack_s x_0 x_1 x_2 4 n)))
 (let ((?x2862 (stack_s x_0 x_1 x_2 5 n)))
 (let (($x8781 (= ?x2862 ?x9580)))
 (let ((?x4305 (sc_s 4)))
 (let ((?x4191 (bvadd (_ bv62 6) ?x4305)))
 (let (($x525 (bvsle ?x4191 n)))
 (or $x525 $x8781))))))))
 ))
 (let ((?x4305 (sc_s 4)))
 (let (($x11749 (= ?x4319 ?x4305)))
 (let ((?x10519 (used_gas_s x_0 x_1 x_2 5)))
 (let (($x2501 (= ?x10519 (+ 3 (used_gas_s x_0 x_1 x_2 4)))))
 (let (($x292 (exc_halt_s 3)))
 (let (($x221 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 3)))) (_ bv0 1)))))
 (let (($x7172 (exc_halt_s 4)))
 (let (($x4086 (forall ((w (_ BitVec 256)) )(let ((?x6598 (storage_s x_0 x_1 x_2 3 w)))
 (let ((?x8154 (storage_s x_0 x_1 x_2 4 w)))
 (= ?x8154 ?x6598))))
 ))
 (let (($x4944 (forall ((n (_ BitVec 6)) )(let ((?x8857 (stack_s x_0 x_1 x_2 3 n)))
 (let ((?x9580 (stack_s x_0 x_1 x_2 4 n)))
 (let (($x9593 (= ?x9580 ?x8857)))
 (or $x9593 (bvsle (bvadd (_ bv60 6) (sc_s 3)) n))))))
 ))
 (let (($x2447 (= ?x4305 (bvadd (_ bv1 6) (sc_s 3)))))
 (let ((?x7103 (used_gas_s x_0 x_1 x_2 4)))
 (let (($x224 (= ?x7103 (+ 3 (used_gas_s x_0 x_1 x_2 3)))))
 (let ((?x275 (sc_s 3)))
 (let ((?x397 (bvadd (_ bv63 6) ?x275)))
 (let ((?x1245 (stack_s x_0 x_1 x_2 3 ?x397)))
 (let ((?x9733 (bvadd (_ bv62 6) ?x275)))
 (let ((?x4711 (stack_s x_0 x_1 x_2 3 ?x9733)))
 (let (($x879 (= (stack_s x_0 x_1 x_2 4 (bvadd (_ bv61 6) ?x275)) (stack_s x_0 x_1 x_2 3 (bvadd (_ bv61 6) ?x275)))))
 (let ((?x5401 (bvadd (_ bv60 6) ?x275)))
 (let ((?x9061 (stack_s x_0 x_1 x_2 3 ?x5401)))
 (let (($x4020 (= $x292 (or (exc_halt_s 2) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 2))))))))
 (let (($x2272 (forall ((w (_ BitVec 256)) )(let ((?x5226 (storage_s x_0 x_1 x_2 2 w)))
 (let ((?x6598 (storage_s x_0 x_1 x_2 3 w)))
 (= ?x6598 ?x5226))))
 ))
 (let (($x6508 (forall ((n (_ BitVec 6)) )(let ((?x218 (sc_s 2)))
 (let ((?x1134 (bvadd (_ bv62 6) ?x218)))
 (let (($x8883 (bvsle ?x1134 n)))
 (let ((?x7204 (stack_s x_0 x_1 x_2 2 n)))
 (let ((?x8857 (stack_s x_0 x_1 x_2 3 n)))
 (let (($x6616 (= ?x8857 ?x7204)))
 (or $x6616 $x8883))))))))
 ))
 (let ((?x218 (sc_s 2)))
 (let (($x9337 (= ?x275 ?x218)))
 (let ((?x9381 (used_gas_s x_0 x_1 x_2 3)))
 (let (($x4355 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 1)))) (_ bv0 1)))))
 (let (($x4324 (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 1))))))
 (let (($x189 (exc_halt_s 1)))
 (let (($x247 (exc_halt_s 2)))
 (let (($x10444 (forall ((w (_ BitVec 256)) )(let ((?x11112 (storage_s x_0 x_1 x_2 1 w)))
 (let ((?x5226 (storage_s x_0 x_1 x_2 2 w)))
 (= ?x5226 ?x11112))))
 ))
 (let (($x7830 (forall ((n (_ BitVec 6)) )(let ((?x6131 (stack_s x_0 x_1 x_2 1 n)))
 (let ((?x7204 (stack_s x_0 x_1 x_2 2 n)))
 (let ((?x154 (sc_s 1)))
 (let ((?x6153 (bvadd (_ bv62 6) ?x154)))
 (let (($x6121 (bvsle ?x6153 n)))
 (or $x6121 (= ?x7204 ?x6131))))))))
 ))
 (let (($x4558 (= ?x218 (bvadd (_ bv1 6) (sc_s 1)))))
 (let ((?x7997 (used_gas_s x_0 x_1 x_2 2)))
 (let (($x1040 (= ?x7997 (+ 3 (used_gas_s x_0 x_1 x_2 1)))))
 (let ((?x154 (sc_s 1)))
 (let ((?x1744 (bvadd (_ bv63 6) ?x154)))
 (let ((?x6167 (stack_s x_0 x_1 x_2 1 ?x1744)))
 (let ((?x6153 (bvadd (_ bv62 6) ?x154)))
 (let ((?x2644 (stack_s x_0 x_1 x_2 1 ?x6153)))
 (let ((?x2958 (bvadd (_ bv63 6) ?x218)))
 (let ((?x6169 (stack_s x_0 x_1 x_2 2 ?x2958)))
 (let (($x7420 (= ?x6169 ?x2644)))
 (let (($x1996 (= $x189 (or $x57 (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) ?x72)))))))
 (let (($x698 (forall ((w (_ BitVec 256)) )(let ((?x793 (storage_s x_0 x_1 x_2 0 w)))
 (let ((?x11112 (storage_s x_0 x_1 x_2 1 w)))
 (= ?x11112 ?x793))))
 ))
 (let (($x9402 (forall ((n (_ BitVec 6)) )(let ((?x10131 (stack_s x_0 x_1 x_2 0 n)))
 (let ((?x6131 (stack_s x_0 x_1 x_2 1 n)))
 (let (($x8617 (= ?x6131 ?x10131)))
 (let ((?x72 (sc_s 0)))
 (let ((?x9009 (bvadd (_ bv62 6) ?x72)))
 (let (($x3358 (bvsle ?x9009 n)))
 (or $x3358 $x8617))))))))
 ))
 (let (($x4441 (= ?x154 ?x72)))
 (let (($x7726 (forall ((w (_ BitVec 256)) )(let ((?x793 (storage_s x_0 x_1 x_2 0 w)))
 (= ?x793 (_ bv0 256))))
 ))
 (let (($x7274 (= ?x3411 0)))
 (let (($x11625 (not $x57)))
 (let (($x9817 (= (stack_s x_0 x_1 x_2 0 (_ bv2 6)) x_2)))
 (let (($x9154 (= (stack_s x_0 x_1 x_2 0 (_ bv1 6)) x_1)))
 (let (($x8082 (= (stack_s x_0 x_1 x_2 0 (_ bv0 6)) x_0)))
 (let (($x8470 (= ?x72 (_ bv3 6))))
 (and $x8470 $x8082 $x9154 $x9817 $x11625 $x7274 $x7726 (= ?x6167 (stack_s x_0 x_1 x_2 0 (bvadd (_ bv62 6) ?x72))) (= ?x2644 (stack_s x_0 x_1 x_2 0 (bvadd (_ bv63 6) ?x72))) (= (used_gas_s x_0 x_1 x_2 1) (+ 3 ?x3411)) $x4441 $x9402 $x698 $x1996 $x7420 (= (stack_s x_0 x_1 x_2 2 ?x6153) ?x2644) (= (stack_s x_0 x_1 x_2 2 ?x1744) ?x6167) $x1040 $x4558 $x7830 $x10444 (= $x247 (or $x189 $x4324 $x4355)) (= ?x1245 (stack_s x_0 x_1 x_2 2 (bvadd (_ bv62 6) ?x218))) (= ?x4711 ?x6169) (= ?x9381 (+ 3 ?x7997)) $x9337 $x6508 $x2272 $x4020 (= (stack_s x_0 x_1 x_2 4 (bvadd (_ bv63 6) ?x4305)) ?x9061) (= (stack_s x_0 x_1 x_2 4 ?x5401) ?x9061) $x879 (= (stack_s x_0 x_1 x_2 4 ?x9733) ?x4711) (= (stack_s x_0 x_1 x_2 4 ?x397) ?x1245) $x224 $x2447 $x4944 $x4086 (= $x7172 (or $x221 $x292 (not (bvsle (_ bv0 6) ?x5401)))) (= ?x7713 (stack_s x_0 x_1 x_2 4 (bvadd (_ bv62 6) ?x4305))) (= ?x2760 (stack_s x_0 x_1 x_2 4 (bvadd (_ bv63 6) ?x4305))) $x2501 $x11749 $x10896 $x6645 $x9721 (= (stack_s x_0 x_1 x_2 6 (bvadd (_ bv63 6) ?x926)) ?x2760) (= (stack_s x_0 x_1 x_2 6 ?x1846) ?x2760) (= (stack_s x_0 x_1 x_2 6 ?x669) ?x7713) $x4660 (= ?x926 (bvadd (_ bv1 6) ?x4319)) $x4182 $x5707 (= $x772 $x9086) (= ?x1543 ?x9398) (= (stack_t x_0 x_1 x_2 1 ?x7221) ?x9398) $x1085 $x8607 (= (used_gas_t x_0 x_1 x_2 1) (+ 3 ?x5852)) $x3866 $x4420 $x6573 (= $x8377 (or $x56 $x3762 (not (bvsle (_ bv0 6) ?x7221)))) (= (stack_t x_0 x_1 x_2 2 (bvadd (_ bv63 6) ?x2714)) ?x9871) (= (stack_t x_0 x_1 x_2 2 ?x5672) ?x9871) (= (stack_t x_0 x_1 x_2 2 ?x1361) ?x1543) (= ?x6171 (+ 3 (used_gas_t x_0 x_1 x_2 1))) $x10940 $x214 $x3305 (= $x4500 (or $x8377 $x8951 $x10555)) (= ?x2291 (stack_t x_0 x_1 x_2 2 (bvadd (_ bv60 6) ?x2714))) (= ?x10039 (stack_t x_0 x_1 x_2 2 (bvadd (_ bv63 6) ?x2714))) (= ?x1455 (stack_t x_0 x_1 x_2 2 (bvadd (_ bv61 6) ?x2714))) (= ?x3406 (stack_t x_0 x_1 x_2 2 (bvadd (_ bv62 6) ?x2714))) $x3927 $x9044 $x10838 $x6586 $x9569 (= (stack_t x_0 x_1 x_2 4 (bvadd (_ bv63 6) ?x4818)) ?x5625) (= (stack_t x_0 x_1 x_2 4 ?x8691) ?x5625) (= (stack_t x_0 x_1 x_2 4 ?x8993) ?x10039) (= (stack_t x_0 x_1 x_2 4 ?x9203) ?x1455) (= (stack_t x_0 x_1 x_2 4 ?x8334) ?x3406) (= (stack_t x_0 x_1 x_2 4 ?x8209) ?x2291) $x2313 $x5041 $x6342 $x2665 (= $x7854 (or (not (bvsle (_ bv0 6) ?x8691)) $x3614 $x9834)) $x73 $x11810 $x58 $x2479 $x3707 (not (and $x7899 $x3897 $x4404 $x4613))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
 )
(check-sat)
(get-proof)
