; DUP4 PUSH cw_1 DUP2 LT ISZERO ISZERO => DUP4 PUSH cw_1 DUP6 LT
(set-option :produce-proofs true)
; 
(set-info :status unknown)
(declare-fun storage_t ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 256)) (_ BitVec 256))
(declare-fun w_1 () (_ BitVec 256))
(declare-fun storage_s ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 256)) (_ BitVec 256))
(declare-fun exc_halt_t (Int) Bool)
(declare-fun exc_halt_s (Int) Bool)
(declare-fun stack_t ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 6)) (_ BitVec 256))
(declare-fun stack_s ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 6)) (_ BitVec 256))
(declare-fun sc_t (Int) (_ BitVec 6))
(declare-fun sc_s (Int) (_ BitVec 6))
(declare-fun used_gas_t ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int) Int)
(declare-fun used_gas_s ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int) Int)
(assert
 (exists ((x_0 (_ BitVec 256)) (x_1 (_ BitVec 256)) (x_2 (_ BitVec 256)) (x_3 (_ BitVec 256)) )(let (($x1936 (forall ((w (_ BitVec 256)) )(let ((?x6538 (storage_t x_0 x_1 x_2 x_3 w_1 4 w)))
 (let ((?x6595 (storage_s x_0 x_1 x_2 x_3 w_1 6 w)))
 (= ?x6595 ?x6538))))
 ))
 (let (($x7854 (exc_halt_t 4)))
 (let (($x772 (exc_halt_s 6)))
 (let (($x4404 (= $x772 $x7854)))
 (let (($x6748 (forall ((n (_ BitVec 6)) )(let ((?x11210 (stack_t x_0 x_1 x_2 x_3 w_1 4 n)))
 (let ((?x11658 (stack_s x_0 x_1 x_2 x_3 w_1 6 n)))
 (let (($x8684 (= ?x11658 ?x11210)))
 (let ((?x4818 (sc_t 4)))
 (let (($x531 (bvsle ?x4818 n)))
 (or $x531 $x8684)))))))
 ))
 (let ((?x4818 (sc_t 4)))
 (let ((?x926 (sc_s 6)))
 (let (($x7899 (= ?x926 ?x4818)))
 (let ((?x9159 (used_gas_t x_0 x_1 x_2 x_3 w_1 0)))
 (let ((?x6484 (used_gas_s x_0 x_1 x_2 x_3 w_1 0)))
 (let (($x8962 (= ?x6484 ?x9159)))
 (let (($x5011 (forall ((w (_ BitVec 256)) )(let ((?x5312 (storage_t x_0 x_1 x_2 x_3 w_1 0 w)))
 (let ((?x3137 (storage_s x_0 x_1 x_2 x_3 w_1 0 w)))
 (= ?x3137 ?x5312))))
 ))
 (let (($x56 (exc_halt_t 0)))
 (let (($x57 (exc_halt_s 0)))
 (let (($x58 (= $x57 $x56)))
 (let (($x10599 (forall ((n (_ BitVec 6)) )(let ((?x63 (sc_t 0)))
 (let (($x11489 (bvsle ?x63 n)))
 (let ((?x11154 (stack_t x_0 x_1 x_2 x_3 w_1 0 n)))
 (let ((?x1617 (stack_s x_0 x_1 x_2 x_3 w_1 0 n)))
 (let (($x11290 (= ?x1617 ?x11154)))
 (or $x11290 $x11489)))))))
 ))
 (let ((?x63 (sc_t 0)))
 (let ((?x72 (sc_s 0)))
 (let (($x73 (= ?x72 ?x63)))
 (let (($x4619 (= $x7854 (or (exc_halt_t 3) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_t 3))))))))
 (let (($x1085 (forall ((w (_ BitVec 256)) )(let ((?x2829 (storage_t x_0 x_1 x_2 x_3 w_1 3 w)))
 (let ((?x6538 (storage_t x_0 x_1 x_2 x_3 w_1 4 w)))
 (= ?x6538 ?x2829))))
 ))
 (let (($x9307 (forall ((n (_ BitVec 6)) )(let ((?x1619 (stack_t x_0 x_1 x_2 x_3 w_1 3 n)))
 (let ((?x11210 (stack_t x_0 x_1 x_2 x_3 w_1 4 n)))
 (or (= ?x11210 ?x1619) (bvsle (bvadd (_ bv62 6) (sc_t 3)) n)))))
 ))
 (let (($x11920 (= (used_gas_t x_0 x_1 x_2 x_3 w_1 4) (+ 3 (used_gas_t x_0 x_1 x_2 x_3 w_1 3)))))
 (let ((?x11304 (sc_t 3)))
 (let ((?x71 (bvadd (_ bv63 6) ?x11304)))
 (let ((?x2008 (stack_t x_0 x_1 x_2 x_3 w_1 3 ?x71)))
 (let (($x5588 (bvule (stack_t x_0 x_1 x_2 x_3 w_1 3 (bvadd (_ bv62 6) ?x11304)) ?x2008)))
 (let (($x1455 (= (stack_t x_0 x_1 x_2 x_3 w_1 4 (bvadd (_ bv63 6) ?x4818)) (ite $x5588 (_ bv0 256) (_ bv1 256)))))
 (let (($x10157 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_t 2)))) (_ bv0 1)))))
 (let (($x8115 (exc_halt_t 2)))
 (let (($x3614 (exc_halt_t 3)))
 (let (($x10838 (forall ((w (_ BitVec 256)) )(let ((?x9170 (storage_t x_0 x_1 x_2 x_3 w_1 2 w)))
 (let ((?x2829 (storage_t x_0 x_1 x_2 x_3 w_1 3 w)))
 (= ?x2829 ?x9170))))
 ))
 (let (($x8821 (forall ((n (_ BitVec 6)) )(let ((?x8669 (stack_t x_0 x_1 x_2 x_3 w_1 2 n)))
 (let ((?x1619 (stack_t x_0 x_1 x_2 x_3 w_1 3 n)))
 (or (bvsle (bvadd (_ bv58 6) (sc_t 2)) n) (= ?x1619 ?x8669)))))
 ))
 (let (($x1640 (= ?x11304 (bvadd (_ bv1 6) (sc_t 2)))))
 (let ((?x7408 (used_gas_t x_0 x_1 x_2 x_3 w_1 3)))
 (let (($x2612 (= (stack_t x_0 x_1 x_2 x_3 w_1 3 (bvadd (_ bv63 6) (sc_t 2))) (stack_t x_0 x_1 x_2 x_3 w_1 2 (bvadd (_ bv63 6) (sc_t 2))))))
 (let (($x6231 (= (stack_t x_0 x_1 x_2 x_3 w_1 3 (bvadd (_ bv62 6) (sc_t 2))) (stack_t x_0 x_1 x_2 x_3 w_1 2 (bvadd (_ bv62 6) (sc_t 2))))))
 (let (($x1000 (= (stack_t x_0 x_1 x_2 x_3 w_1 3 (bvadd (_ bv61 6) (sc_t 2))) (stack_t x_0 x_1 x_2 x_3 w_1 2 (bvadd (_ bv61 6) (sc_t 2))))))
 (let (($x6174 (= (stack_t x_0 x_1 x_2 x_3 w_1 3 (bvadd (_ bv60 6) (sc_t 2))) (stack_t x_0 x_1 x_2 x_3 w_1 2 (bvadd (_ bv60 6) (sc_t 2))))))
 (let (($x2322 (= (stack_t x_0 x_1 x_2 x_3 w_1 3 (bvadd (_ bv59 6) (sc_t 2))) (stack_t x_0 x_1 x_2 x_3 w_1 2 (bvadd (_ bv59 6) (sc_t 2))))))
 (let ((?x6158 (sc_t 2)))
 (let ((?x319 (bvadd (_ bv58 6) ?x6158)))
 (let ((?x569 (stack_t x_0 x_1 x_2 x_3 w_1 2 ?x319)))
 (let (($x6160 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_t 1)))) (_ bv0 1)))))
 (let (($x8377 (exc_halt_t 1)))
 (let (($x11320 (= $x8115 (or $x8377 $x6160))))
 (let (($x6743 (forall ((w (_ BitVec 256)) )(let ((?x318 (storage_t x_0 x_1 x_2 x_3 w_1 1 w)))
 (let ((?x9170 (storage_t x_0 x_1 x_2 x_3 w_1 2 w)))
 (= ?x9170 ?x318))))
 ))
 (let (($x4676 (forall ((n (_ BitVec 6)) )(let ((?x458 (stack_t x_0 x_1 x_2 x_3 w_1 1 n)))
 (let ((?x8669 (stack_t x_0 x_1 x_2 x_3 w_1 2 n)))
 (let ((?x7154 (sc_t 1)))
 (let (($x3342 (bvsle ?x7154 n)))
 (or $x3342 (= ?x8669 ?x458)))))))
 ))
 (let (($x9815 (= ?x6158 (bvadd (_ bv1 6) (sc_t 1)))))
 (let ((?x6290 (used_gas_t x_0 x_1 x_2 x_3 w_1 2)))
 (let (($x2483 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x63))) (_ bv0 1)))))
 (let (($x1428 (forall ((w (_ BitVec 256)) )(let ((?x5312 (storage_t x_0 x_1 x_2 x_3 w_1 0 w)))
 (let ((?x318 (storage_t x_0 x_1 x_2 x_3 w_1 1 w)))
 (= ?x318 ?x5312))))
 ))
 (let (($x3624 (forall ((n (_ BitVec 6)) )(let ((?x11154 (stack_t x_0 x_1 x_2 x_3 w_1 0 n)))
 (let ((?x458 (stack_t x_0 x_1 x_2 x_3 w_1 1 n)))
 (or (bvsle (bvadd (_ bv60 6) (sc_t 0)) n) (= ?x458 ?x11154)))))
 ))
 (let ((?x7154 (sc_t 1)))
 (let (($x10389 (= ?x7154 (bvadd (_ bv1 6) ?x63))))
 (let (($x11890 (= (stack_t x_0 x_1 x_2 x_3 w_1 1 (bvadd (_ bv63 6) ?x63)) (stack_t x_0 x_1 x_2 x_3 w_1 0 (bvadd (_ bv63 6) ?x63)))))
 (let (($x5225 (= (stack_t x_0 x_1 x_2 x_3 w_1 1 (bvadd (_ bv62 6) ?x63)) (stack_t x_0 x_1 x_2 x_3 w_1 0 (bvadd (_ bv62 6) ?x63)))))
 (let (($x3865 (= (stack_t x_0 x_1 x_2 x_3 w_1 1 (bvadd (_ bv61 6) ?x63)) (stack_t x_0 x_1 x_2 x_3 w_1 0 (bvadd (_ bv61 6) ?x63)))))
 (let ((?x7343 (bvadd (_ bv60 6) ?x63)))
 (let ((?x8538 (stack_t x_0 x_1 x_2 x_3 w_1 0 ?x7343)))
 (let (($x8716 (= $x772 (or (exc_halt_s 5) (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_s 5))))))))
 (let (($x10256 (forall ((w (_ BitVec 256)) )(let ((?x7281 (storage_s x_0 x_1 x_2 x_3 w_1 5 w)))
 (let ((?x6595 (storage_s x_0 x_1 x_2 x_3 w_1 6 w)))
 (= ?x6595 ?x7281))))
 ))
 (let (($x8280 (forall ((n (_ BitVec 6)) )(let ((?x183 (stack_s x_0 x_1 x_2 x_3 w_1 5 n)))
 (let ((?x11658 (stack_s x_0 x_1 x_2 x_3 w_1 6 n)))
 (or (= ?x11658 ?x183) (bvsle (bvadd (_ bv63 6) (sc_s 5)) n)))))
 ))
 (let ((?x4319 (sc_s 5)))
 (let (($x11219 (= ?x926 ?x4319)))
 (let (($x4106 (= (used_gas_s x_0 x_1 x_2 x_3 w_1 6) (+ 3 (used_gas_s x_0 x_1 x_2 x_3 w_1 5)))))
 (let ((?x1330 (ite (= (stack_s x_0 x_1 x_2 x_3 w_1 5 (bvadd (_ bv63 6) ?x4319)) (_ bv0 256)) (_ bv1 256) (_ bv0 256))))
 (let (($x11317 (exc_halt_s 5)))
 (let (($x6378 (= $x11317 (or (exc_halt_s 4) (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_s 4))))))))
 (let (($x9551 (forall ((w (_ BitVec 256)) )(let ((?x4942 (storage_s x_0 x_1 x_2 x_3 w_1 4 w)))
 (let ((?x7281 (storage_s x_0 x_1 x_2 x_3 w_1 5 w)))
 (= ?x7281 ?x4942))))
 ))
 (let (($x10630 (forall ((n (_ BitVec 6)) )(let ((?x999 (stack_s x_0 x_1 x_2 x_3 w_1 4 n)))
 (let ((?x183 (stack_s x_0 x_1 x_2 x_3 w_1 5 n)))
 (let ((?x4305 (sc_s 4)))
 (let ((?x2899 (bvadd (_ bv63 6) ?x4305)))
 (let (($x3711 (bvsle ?x2899 n)))
 (or $x3711 (= ?x183 ?x999))))))))
 ))
 (let ((?x4305 (sc_s 4)))
 (let (($x9268 (= ?x4319 ?x4305)))
 (let ((?x1970 (used_gas_s x_0 x_1 x_2 x_3 w_1 5)))
 (let ((?x3122 (ite (= (stack_s x_0 x_1 x_2 x_3 w_1 4 (bvadd (_ bv63 6) ?x4305)) (_ bv0 256)) (_ bv1 256) (_ bv0 256))))
 (let ((?x1539 (bvadd (_ bv63 6) ?x4319)))
 (let ((?x8804 (stack_s x_0 x_1 x_2 x_3 w_1 5 ?x1539)))
 (let (($x7172 (exc_halt_s 4)))
 (let (($x9998 (= $x7172 (or (exc_halt_s 3) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 3))))))))
 (let (($x963 (forall ((w (_ BitVec 256)) )(let ((?x6023 (storage_s x_0 x_1 x_2 x_3 w_1 3 w)))
 (let ((?x4942 (storage_s x_0 x_1 x_2 x_3 w_1 4 w)))
 (= ?x4942 ?x6023))))
 ))
 (let (($x315 (forall ((n (_ BitVec 6)) )(let ((?x3269 (stack_s x_0 x_1 x_2 x_3 w_1 3 n)))
 (let ((?x999 (stack_s x_0 x_1 x_2 x_3 w_1 4 n)))
 (let ((?x275 (sc_s 3)))
 (let ((?x4269 (bvadd (_ bv62 6) ?x275)))
 (let (($x1714 (bvsle ?x4269 n)))
 (or $x1714 (= ?x999 ?x3269))))))))
 ))
 (let ((?x275 (sc_s 3)))
 (let ((?x7749 (bvadd (_ bv63 6) ?x275)))
 (let (($x1275 (= ?x4305 ?x7749)))
 (let ((?x8286 (used_gas_s x_0 x_1 x_2 x_3 w_1 4)))
 (let ((?x977 (stack_s x_0 x_1 x_2 x_3 w_1 3 ?x7749)))
 (let (($x2900 (bvule (stack_s x_0 x_1 x_2 x_3 w_1 3 (bvadd (_ bv62 6) ?x275)) ?x977)))
 (let ((?x2899 (bvadd (_ bv63 6) ?x4305)))
 (let ((?x1079 (stack_s x_0 x_1 x_2 x_3 w_1 4 ?x2899)))
 (let (($x6754 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 2)))) (_ bv0 1)))))
 (let (($x1297 (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 2))))))
 (let (($x247 (exc_halt_s 2)))
 (let (($x292 (exc_halt_s 3)))
 (let (($x5277 (= $x292 (or $x247 $x1297 $x6754))))
 (let (($x6578 (forall ((w (_ BitVec 256)) )(let ((?x6125 (storage_s x_0 x_1 x_2 x_3 w_1 2 w)))
 (let ((?x6023 (storage_s x_0 x_1 x_2 x_3 w_1 3 w)))
 (= ?x6023 ?x6125))))
 ))
 (let (($x8426 (forall ((n (_ BitVec 6)) )(let ((?x218 (sc_s 2)))
 (let ((?x1898 (bvadd (_ bv62 6) ?x218)))
 (let (($x10861 (bvsle ?x1898 n)))
 (let ((?x3575 (stack_s x_0 x_1 x_2 x_3 w_1 2 n)))
 (let ((?x3269 (stack_s x_0 x_1 x_2 x_3 w_1 3 n)))
 (or (= ?x3269 ?x3575) $x10861)))))))
 ))
 (let (($x9686 (= ?x275 (bvadd (_ bv1 6) (sc_s 2)))))
 (let ((?x4013 (used_gas_s x_0 x_1 x_2 x_3 w_1 3)))
 (let (($x2873 (= (stack_s x_0 x_1 x_2 x_3 w_1 3 (bvadd (_ bv63 6) (sc_s 2))) (stack_s x_0 x_1 x_2 x_3 w_1 2 (bvadd (_ bv63 6) (sc_s 2))))))
 (let ((?x218 (sc_s 2)))
 (let ((?x1898 (bvadd (_ bv62 6) ?x218)))
 (let ((?x7052 (stack_s x_0 x_1 x_2 x_3 w_1 2 ?x1898)))
 (let (($x2257 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 1)))) (_ bv0 1)))))
 (let (($x189 (exc_halt_s 1)))
 (let (($x5935 (= $x247 (or $x189 $x2257))))
 (let (($x1056 (forall ((w (_ BitVec 256)) )(let ((?x10712 (storage_s x_0 x_1 x_2 x_3 w_1 1 w)))
 (let ((?x6125 (storage_s x_0 x_1 x_2 x_3 w_1 2 w)))
 (= ?x6125 ?x10712))))
 ))
 (let (($x853 (forall ((n (_ BitVec 6)) )(let ((?x154 (sc_s 1)))
 (let (($x1977 (bvsle ?x154 n)))
 (let ((?x11625 (stack_s x_0 x_1 x_2 x_3 w_1 1 n)))
 (let ((?x3575 (stack_s x_0 x_1 x_2 x_3 w_1 2 n)))
 (or (= ?x3575 ?x11625) $x1977))))))
 ))
 (let (($x7493 (= ?x218 (bvadd (_ bv1 6) (sc_s 1)))))
 (let ((?x6804 (used_gas_s x_0 x_1 x_2 x_3 w_1 2)))
 (let (($x10696 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x72))) (_ bv0 1)))))
 (let (($x4112 (= $x189 (or $x57 (not (bvsle (_ bv0 6) (bvadd (_ bv60 6) ?x72))) $x10696))))
 (let (($x7808 (forall ((w (_ BitVec 256)) )(let ((?x3137 (storage_s x_0 x_1 x_2 x_3 w_1 0 w)))
 (let ((?x10712 (storage_s x_0 x_1 x_2 x_3 w_1 1 w)))
 (= ?x10712 ?x3137))))
 ))
 (let (($x3386 (forall ((n (_ BitVec 6)) )(let ((?x1617 (stack_s x_0 x_1 x_2 x_3 w_1 0 n)))
 (let ((?x11625 (stack_s x_0 x_1 x_2 x_3 w_1 1 n)))
 (let ((?x72 (sc_s 0)))
 (let ((?x2728 (bvadd (_ bv60 6) ?x72)))
 (let (($x1811 (bvsle ?x2728 n)))
 (or $x1811 (= ?x11625 ?x1617))))))))
 ))
 (let ((?x154 (sc_s 1)))
 (let (($x3182 (= ?x154 (bvadd (_ bv1 6) ?x72))))
 (let (($x268 (= (stack_s x_0 x_1 x_2 x_3 w_1 1 (bvadd (_ bv63 6) ?x72)) (stack_s x_0 x_1 x_2 x_3 w_1 0 (bvadd (_ bv63 6) ?x72)))))
 (let (($x7233 (= (stack_s x_0 x_1 x_2 x_3 w_1 1 (bvadd (_ bv62 6) ?x72)) (stack_s x_0 x_1 x_2 x_3 w_1 0 (bvadd (_ bv62 6) ?x72)))))
 (let (($x3913 (= (stack_s x_0 x_1 x_2 x_3 w_1 1 (bvadd (_ bv61 6) ?x72)) (stack_s x_0 x_1 x_2 x_3 w_1 0 (bvadd (_ bv61 6) ?x72)))))
 (let ((?x2728 (bvadd (_ bv60 6) ?x72)))
 (let ((?x11233 (stack_s x_0 x_1 x_2 x_3 w_1 0 ?x2728)))
 (let (($x3446 (forall ((w (_ BitVec 256)) )(let ((?x3137 (storage_s x_0 x_1 x_2 x_3 w_1 0 w)))
 (= ?x3137 (_ bv0 256))))
 ))
 (let (($x4332 (= ?x6484 0)))
 (let (($x9420 (not $x57)))
 (let (($x10545 (= (stack_s x_0 x_1 x_2 x_3 w_1 0 (_ bv3 6)) x_3)))
 (let (($x9654 (= (stack_s x_0 x_1 x_2 x_3 w_1 0 (_ bv2 6)) x_2)))
 (let (($x1293 (= (stack_s x_0 x_1 x_2 x_3 w_1 0 (_ bv1 6)) x_1)))
 (let (($x2070 (= (stack_s x_0 x_1 x_2 x_3 w_1 0 (_ bv0 6)) x_0)))
 (let (($x11199 (= ?x72 (_ bv4 6))))
 (and $x11199 $x2070 $x1293 $x9654 $x10545 $x9420 $x4332 $x3446 (= (stack_s x_0 x_1 x_2 x_3 w_1 1 (bvadd (_ bv63 6) ?x154)) ?x11233) (= (stack_s x_0 x_1 x_2 x_3 w_1 1 ?x2728) ?x11233) $x3913 $x7233 $x268 (= (used_gas_s x_0 x_1 x_2 x_3 w_1 1) (+ 3 ?x6484)) $x3182 $x3386 $x7808 $x4112 (= (stack_s x_0 x_1 x_2 x_3 w_1 2 ?x154) w_1) (= ?x6804 (+ 3 (used_gas_s x_0 x_1 x_2 x_3 w_1 1))) $x7493 $x853 $x1056 $x5935 (= ?x977 ?x7052) (= (stack_s x_0 x_1 x_2 x_3 w_1 3 ?x1898) ?x7052) $x2873 (= ?x4013 (+ 3 ?x6804)) $x9686 $x8426 $x6578 $x5277 (= ?x1079 (ite $x2900 (_ bv0 256) (_ bv1 256))) (= ?x8286 (+ 3 ?x4013)) $x1275 $x315 $x963 $x9998 (= ?x8804 ?x3122) (= ?x1970 (+ 3 ?x8286)) $x9268 $x10630 $x9551 $x6378 (= (stack_s x_0 x_1 x_2 x_3 w_1 6 (bvadd (_ bv63 6) ?x926)) ?x1330) $x4106 $x11219 $x8280 $x10256 $x8716 (= (stack_t x_0 x_1 x_2 x_3 w_1 1 (bvadd (_ bv63 6) ?x7154)) ?x8538) (= (stack_t x_0 x_1 x_2 x_3 w_1 1 ?x7343) ?x8538) $x3865 $x5225 $x11890 (= (used_gas_t x_0 x_1 x_2 x_3 w_1 1) (+ 3 ?x9159)) $x10389 $x3624 $x1428 (= $x8377 (or $x56 (not (bvsle (_ bv0 6) ?x7343)) $x2483)) (= (stack_t x_0 x_1 x_2 x_3 w_1 2 ?x7154) w_1) (= ?x6290 (+ 3 (used_gas_t x_0 x_1 x_2 x_3 w_1 1))) $x9815 $x4676 $x6743 $x11320 (= ?x2008 ?x569) (= (stack_t x_0 x_1 x_2 x_3 w_1 3 ?x319) ?x569) $x2322 $x6174 $x1000 $x6231 $x2612 (= ?x7408 (+ 3 ?x6290)) $x1640 $x8821 $x10838 (= $x3614 (or (not (bvsle (_ bv0 6) ?x319)) $x8115 $x10157)) $x1455 $x11920 (= ?x4818 ?x71) $x9307 $x1085 $x4619 $x73 $x10599 $x58 $x5011 $x8962 (not (and $x7899 $x6748 $x4404 $x1936))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
 )
(check-sat)
(get-proof)