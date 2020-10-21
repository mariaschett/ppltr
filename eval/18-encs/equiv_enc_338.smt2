; SWAP3 SWAP1 SWAP3 OR OR OR => OR OR OR
(set-option :produce-proofs true)
; 
(set-info :status unknown)
(declare-fun storage_t ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 256)) (_ BitVec 256))
(declare-fun storage_s ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 256)) (_ BitVec 256))
(declare-fun exc_halt_t (Int) Bool)
(declare-fun exc_halt_s (Int) Bool)
(declare-fun sc_t (Int) (_ BitVec 6))
(declare-fun stack_t ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 6)) (_ BitVec 256))
(declare-fun stack_s ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 6)) (_ BitVec 256))
(declare-fun sc_s (Int) (_ BitVec 6))
(declare-fun used_gas_t ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int) Int)
(declare-fun used_gas_s ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int) Int)
(assert
 (exists ((x_0 (_ BitVec 256)) (x_1 (_ BitVec 256)) (x_2 (_ BitVec 256)) (x_3 (_ BitVec 256)) )(let (($x2865 (forall ((w (_ BitVec 256)) )(let ((?x2975 (storage_t x_0 x_1 x_2 x_3 3 w)))
 (let ((?x3753 (storage_s x_0 x_1 x_2 x_3 6 w)))
 (= ?x3753 ?x2975))))
 ))
 (let (($x6783 (exc_halt_t 3)))
 (let (($x772 (exc_halt_s 6)))
 (let (($x3870 (= $x772 $x6783)))
 (let (($x5856 (forall ((n (_ BitVec 6)) )(let ((?x2349 (stack_t x_0 x_1 x_2 x_3 3 n)))
 (let ((?x7788 (stack_s x_0 x_1 x_2 x_3 6 n)))
 (let (($x5152 (= ?x7788 ?x2349)))
 (or $x5152 (bvsle (sc_t 3) n))))))
 ))
 (let ((?x6438 (sc_t 3)))
 (let ((?x926 (sc_s 6)))
 (let (($x5324 (= ?x926 ?x6438)))
 (let ((?x10865 (used_gas_t x_0 x_1 x_2 x_3 0)))
 (let ((?x11193 (used_gas_s x_0 x_1 x_2 x_3 0)))
 (let (($x4723 (= ?x11193 ?x10865)))
 (let (($x2674 (forall ((w (_ BitVec 256)) )(let ((?x8725 (storage_t x_0 x_1 x_2 x_3 0 w)))
 (let ((?x1406 (storage_s x_0 x_1 x_2 x_3 0 w)))
 (= ?x1406 ?x8725))))
 ))
 (let (($x56 (exc_halt_t 0)))
 (let (($x57 (exc_halt_s 0)))
 (let (($x58 (= $x57 $x56)))
 (let (($x7843 (forall ((n (_ BitVec 6)) )(let ((?x6348 (stack_t x_0 x_1 x_2 x_3 0 n)))
 (let ((?x4787 (stack_s x_0 x_1 x_2 x_3 0 n)))
 (let (($x10478 (= ?x4787 ?x6348)))
 (or (bvsle (sc_t 0) n) $x10478)))))
 ))
 (let ((?x63 (sc_t 0)))
 (let ((?x72 (sc_s 0)))
 (let (($x73 (= ?x72 ?x63)))
 (let (($x3918 (= $x6783 (or (exc_halt_t 2) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_t 2))))))))
 (let (($x1628 (forall ((w (_ BitVec 256)) )(let ((?x3244 (storage_t x_0 x_1 x_2 x_3 2 w)))
 (let ((?x2975 (storage_t x_0 x_1 x_2 x_3 3 w)))
 (= ?x2975 ?x3244))))
 ))
 (let (($x11151 (forall ((n (_ BitVec 6)) )(let ((?x6219 (stack_t x_0 x_1 x_2 x_3 2 n)))
 (let ((?x2349 (stack_t x_0 x_1 x_2 x_3 3 n)))
 (or (bvsle (bvadd (_ bv62 6) (sc_t 2)) n) (= ?x2349 ?x6219)))))
 ))
 (let (($x11224 (= (used_gas_t x_0 x_1 x_2 x_3 3) (+ 3 (used_gas_t x_0 x_1 x_2 x_3 2)))))
 (let ((?x2714 (sc_t 2)))
 (let ((?x11413 (bvadd (_ bv63 6) ?x2714)))
 (let ((?x6361 (stack_t x_0 x_1 x_2 x_3 2 ?x11413)))
 (let (($x9520 (= (stack_t x_0 x_1 x_2 x_3 3 (bvadd (_ bv63 6) ?x6438)) (bvor ?x6361 (stack_t x_0 x_1 x_2 x_3 2 (bvadd (_ bv62 6) ?x2714))))))
 (let (($x5252 (exc_halt_t 2)))
 (let (($x11167 (= $x5252 (or (exc_halt_t 1) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_t 1))))))))
 (let (($x11749 (forall ((w (_ BitVec 256)) )(let ((?x2030 (storage_t x_0 x_1 x_2 x_3 1 w)))
 (let ((?x3244 (storage_t x_0 x_1 x_2 x_3 2 w)))
 (= ?x3244 ?x2030))))
 ))
 (let (($x5284 (forall ((n (_ BitVec 6)) )(let ((?x5736 (stack_t x_0 x_1 x_2 x_3 1 n)))
 (let ((?x6219 (stack_t x_0 x_1 x_2 x_3 2 n)))
 (or (bvsle (bvadd (_ bv62 6) (sc_t 1)) n) (= ?x6219 ?x5736)))))
 ))
 (let ((?x4439 (used_gas_t x_0 x_1 x_2 x_3 2)))
 (let ((?x5151 (sc_t 1)))
 (let ((?x4612 (bvadd (_ bv63 6) ?x5151)))
 (let ((?x1506 (stack_t x_0 x_1 x_2 x_3 1 ?x4612)))
 (let (($x1449 (= ?x6361 (bvor ?x1506 (stack_t x_0 x_1 x_2 x_3 1 (bvadd (_ bv62 6) ?x5151))))))
 (let (($x3452 (forall ((w (_ BitVec 256)) )(let ((?x8725 (storage_t x_0 x_1 x_2 x_3 0 w)))
 (let ((?x2030 (storage_t x_0 x_1 x_2 x_3 1 w)))
 (= ?x2030 ?x8725))))
 ))
 (let (($x4698 (forall ((n (_ BitVec 6)) )(let ((?x6348 (stack_t x_0 x_1 x_2 x_3 0 n)))
 (let ((?x5736 (stack_t x_0 x_1 x_2 x_3 1 n)))
 (or (bvsle (bvadd (_ bv62 6) (sc_t 0)) n) (= ?x5736 ?x6348)))))
 ))
 (let ((?x11703 (bvor (stack_t x_0 x_1 x_2 x_3 0 (bvadd (_ bv63 6) ?x63)) (stack_t x_0 x_1 x_2 x_3 0 (bvadd (_ bv62 6) ?x63)))))
 (let (($x6724 (= $x772 (or (exc_halt_s 5) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 5))))))))
 (let (($x3738 (forall ((w (_ BitVec 256)) )(let ((?x1439 (storage_s x_0 x_1 x_2 x_3 5 w)))
 (let ((?x3753 (storage_s x_0 x_1 x_2 x_3 6 w)))
 (= ?x3753 ?x1439))))
 ))
 (let (($x3112 (forall ((n (_ BitVec 6)) )(let ((?x10904 (stack_s x_0 x_1 x_2 x_3 5 n)))
 (let ((?x7788 (stack_s x_0 x_1 x_2 x_3 6 n)))
 (or (= ?x7788 ?x10904) (bvsle (bvadd (_ bv62 6) (sc_s 5)) n)))))
 ))
 (let (($x10298 (= (used_gas_s x_0 x_1 x_2 x_3 6) (+ 3 (used_gas_s x_0 x_1 x_2 x_3 5)))))
 (let ((?x4319 (sc_s 5)))
 (let ((?x379 (bvadd (_ bv63 6) ?x4319)))
 (let ((?x4557 (stack_s x_0 x_1 x_2 x_3 5 ?x379)))
 (let (($x5783 (= (stack_s x_0 x_1 x_2 x_3 6 (bvadd (_ bv63 6) ?x926)) (bvor ?x4557 (stack_s x_0 x_1 x_2 x_3 5 (bvadd (_ bv62 6) ?x4319))))))
 (let (($x11317 (exc_halt_s 5)))
 (let (($x2852 (= $x11317 (or (exc_halt_s 4) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 4))))))))
 (let (($x4971 (forall ((w (_ BitVec 256)) )(let ((?x10139 (storage_s x_0 x_1 x_2 x_3 4 w)))
 (let ((?x1439 (storage_s x_0 x_1 x_2 x_3 5 w)))
 (= ?x1439 ?x10139))))
 ))
 (let (($x9646 (forall ((n (_ BitVec 6)) )(let ((?x10219 (stack_s x_0 x_1 x_2 x_3 4 n)))
 (let ((?x10904 (stack_s x_0 x_1 x_2 x_3 5 n)))
 (or (= ?x10904 ?x10219) (bvsle (bvadd (_ bv62 6) (sc_s 4)) n)))))
 ))
 (let ((?x7505 (used_gas_s x_0 x_1 x_2 x_3 5)))
 (let ((?x4305 (sc_s 4)))
 (let ((?x4445 (bvadd (_ bv63 6) ?x4305)))
 (let ((?x11356 (stack_s x_0 x_1 x_2 x_3 4 ?x4445)))
 (let (($x7618 (= ?x4557 (bvor ?x11356 (stack_s x_0 x_1 x_2 x_3 4 (bvadd (_ bv62 6) ?x4305))))))
 (let (($x7172 (exc_halt_s 4)))
 (let (($x6996 (= $x7172 (or (exc_halt_s 3) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 3))))))))
 (let (($x10854 (forall ((w (_ BitVec 256)) )(let ((?x2704 (storage_s x_0 x_1 x_2 x_3 3 w)))
 (let ((?x10139 (storage_s x_0 x_1 x_2 x_3 4 w)))
 (= ?x10139 ?x2704))))
 ))
 (let (($x3657 (forall ((n (_ BitVec 6)) )(let ((?x11144 (stack_s x_0 x_1 x_2 x_3 3 n)))
 (let ((?x10219 (stack_s x_0 x_1 x_2 x_3 4 n)))
 (or (= ?x10219 ?x11144) (bvsle (bvadd (_ bv62 6) (sc_s 3)) n)))))
 ))
 (let ((?x8920 (used_gas_s x_0 x_1 x_2 x_3 4)))
 (let ((?x275 (sc_s 3)))
 (let ((?x7426 (bvadd (_ bv62 6) ?x275)))
 (let ((?x7249 (stack_s x_0 x_1 x_2 x_3 3 ?x7426)))
 (let ((?x8928 (bvadd (_ bv63 6) ?x275)))
 (let ((?x7169 (stack_s x_0 x_1 x_2 x_3 3 ?x8928)))
 (let (($x292 (exc_halt_s 3)))
 (let (($x9853 (= $x292 (or (exc_halt_s 2) (not (bvsle (_ bv0 6) (bvadd (_ bv60 6) (sc_s 2))))))))
 (let (($x9817 (forall ((w (_ BitVec 256)) )(let ((?x4162 (storage_s x_0 x_1 x_2 x_3 2 w)))
 (let ((?x2704 (storage_s x_0 x_1 x_2 x_3 3 w)))
 (= ?x2704 ?x4162))))
 ))
 (let (($x10311 (forall ((n (_ BitVec 6)) )(let ((?x5123 (stack_s x_0 x_1 x_2 x_3 2 n)))
 (let ((?x11144 (stack_s x_0 x_1 x_2 x_3 3 n)))
 (or (= ?x11144 ?x5123) (bvsle (bvadd (_ bv60 6) (sc_s 2)) n)))))
 ))
 (let ((?x6382 (used_gas_s x_0 x_1 x_2 x_3 3)))
 (let ((?x10415 (stack_s x_0 x_1 x_2 x_3 2 (bvadd (_ bv62 6) (sc_s 2)))))
 (let (($x4819 (= (stack_s x_0 x_1 x_2 x_3 3 (bvadd (_ bv61 6) ?x275)) (stack_s x_0 x_1 x_2 x_3 2 (bvadd (_ bv61 6) (sc_s 2))))))
 (let ((?x2197 (stack_s x_0 x_1 x_2 x_3 2 (bvadd (_ bv63 6) (sc_s 2)))))
 (let (($x652 (= ?x7169 (stack_s x_0 x_1 x_2 x_3 2 (bvadd (_ bv60 6) (sc_s 2))))))
 (let (($x247 (exc_halt_s 2)))
 (let (($x9404 (= $x247 (or (exc_halt_s 1) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 1))))))))
 (let (($x2506 (forall ((w (_ BitVec 256)) )(let ((?x6506 (storage_s x_0 x_1 x_2 x_3 1 w)))
 (let ((?x4162 (storage_s x_0 x_1 x_2 x_3 2 w)))
 (= ?x4162 ?x6506))))
 ))
 (let (($x9280 (forall ((n (_ BitVec 6)) )(let ((?x10395 (stack_s x_0 x_1 x_2 x_3 1 n)))
 (let ((?x5123 (stack_s x_0 x_1 x_2 x_3 2 n)))
 (or (bvsle (bvadd (_ bv62 6) (sc_s 1)) n) (= ?x5123 ?x10395)))))
 ))
 (let ((?x5364 (used_gas_s x_0 x_1 x_2 x_3 2)))
 (let ((?x11150 (stack_s x_0 x_1 x_2 x_3 1 (bvadd (_ bv63 6) (sc_s 1)))))
 (let ((?x154 (sc_s 1)))
 (let ((?x11153 (bvadd (_ bv62 6) ?x154)))
 (let ((?x11756 (stack_s x_0 x_1 x_2 x_3 1 ?x11153)))
 (let (($x3268 (forall ((w (_ BitVec 256)) )(let ((?x1406 (storage_s x_0 x_1 x_2 x_3 0 w)))
 (let ((?x6506 (storage_s x_0 x_1 x_2 x_3 1 w)))
 (= ?x6506 ?x1406))))
 ))
 (let (($x1885 (forall ((n (_ BitVec 6)) )(let ((?x4787 (stack_s x_0 x_1 x_2 x_3 0 n)))
 (let ((?x10395 (stack_s x_0 x_1 x_2 x_3 1 n)))
 (or (= ?x10395 ?x4787) (bvsle (bvadd (_ bv60 6) (sc_s 0)) n)))))
 ))
 (let (($x11828 (= (stack_s x_0 x_1 x_2 x_3 1 (bvadd (_ bv61 6) ?x154)) (stack_s x_0 x_1 x_2 x_3 0 (bvadd (_ bv61 6) ?x72)))))
 (let (($x6119 (= (stack_s x_0 x_1 x_2 x_3 1 (bvadd (_ bv60 6) ?x154)) (stack_s x_0 x_1 x_2 x_3 0 (bvadd (_ bv63 6) ?x72)))))
 (let (($x6242 (forall ((w (_ BitVec 256)) )(let ((?x1406 (storage_s x_0 x_1 x_2 x_3 0 w)))
 (= ?x1406 (_ bv0 256))))
 ))
 (let (($x3422 (= ?x11193 0)))
 (let (($x9232 (= (stack_s x_0 x_1 x_2 x_3 0 (_ bv3 6)) x_3)))
 (let (($x2519 (= (stack_s x_0 x_1 x_2 x_3 0 (_ bv2 6)) x_2)))
 (let (($x2336 (= (stack_s x_0 x_1 x_2 x_3 0 (_ bv1 6)) x_1)))
 (let (($x6806 (= (stack_s x_0 x_1 x_2 x_3 0 (_ bv0 6)) x_0)))
 (let (($x289 (= ?x72 (_ bv4 6))))
 (and $x289 $x6806 $x2336 $x2519 $x9232 (not $x57) $x3422 $x6242 (= ?x11150 (stack_s x_0 x_1 x_2 x_3 0 (bvadd (_ bv60 6) ?x72))) $x6119 $x11828 (= ?x11756 (stack_s x_0 x_1 x_2 x_3 0 (bvadd (_ bv62 6) ?x72))) (= (used_gas_s x_0 x_1 x_2 x_3 1) (+ 3 ?x11193)) (= ?x154 ?x72) $x1885 $x3268 (= (exc_halt_s 1) (or $x57 (not (bvsle (_ bv0 6) (bvadd (_ bv60 6) ?x72))))) (= ?x2197 ?x11756) (= ?x10415 ?x11150) (= ?x5364 (+ 3 (used_gas_s x_0 x_1 x_2 x_3 1))) (= (sc_s 2) ?x154) $x9280 $x2506 $x9404 $x652 (= (stack_s x_0 x_1 x_2 x_3 3 (bvadd (_ bv60 6) ?x275)) ?x2197) $x4819 (= ?x7249 ?x10415) (= ?x6382 (+ 3 ?x5364)) (= ?x275 (sc_s 2)) $x10311 $x9817 $x9853 (= ?x11356 (bvor ?x7169 ?x7249)) (= ?x8920 (+ 3 ?x6382)) (= ?x4305 ?x8928) $x3657 $x10854 $x6996 $x7618 (= ?x7505 (+ 3 ?x8920)) (= ?x4319 ?x4445) $x9646 $x4971 $x2852 $x5783 $x10298 (= ?x926 ?x379) $x3112 $x3738 $x6724 (= ?x1506 ?x11703) (= (used_gas_t x_0 x_1 x_2 x_3 1) (+ 3 ?x10865)) (= ?x5151 (bvadd (_ bv63 6) ?x63)) $x4698 $x3452 (= (exc_halt_t 1) (or $x56 (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) ?x63))))) $x1449 (= ?x4439 (+ 3 (used_gas_t x_0 x_1 x_2 x_3 1))) (= ?x2714 ?x4612) $x5284 $x11749 $x11167 $x9520 $x11224 (= ?x6438 ?x11413) $x11151 $x1628 $x3918 $x73 $x7843 $x58 $x2674 $x4723 (not (and $x5324 $x5856 $x3870 $x2865)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
 )
(check-sat)
(get-proof)
