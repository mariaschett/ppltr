; SWAP1 SWAP2 PUSH cw_1 SWAP1 DUP3 SWAP1 => PUSH cw_1 SWAP1 DUP1 SWAP3 SWAP4
(set-option :produce-proofs true)
; 
(set-info :status unknown)
(declare-fun storage_t ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 256)) (_ BitVec 256))
(declare-fun w_1 () (_ BitVec 256))
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
 (exists ((x_0 (_ BitVec 256)) (x_1 (_ BitVec 256)) (x_2 (_ BitVec 256)) )(let (($x9017 (forall ((w (_ BitVec 256)) )(let ((?x6103 (storage_t x_0 x_1 x_2 w_1 5 w)))
 (let ((?x9617 (storage_s x_0 x_1 x_2 w_1 6 w)))
 (= ?x9617 ?x6103))))
 ))
 (let (($x1885 (exc_halt_t 5)))
 (let (($x772 (exc_halt_s 6)))
 (let (($x11108 (= $x772 $x1885)))
 (let (($x11208 (forall ((n (_ BitVec 6)) )(let ((?x8961 (sc_t 5)))
 (let (($x2543 (bvsle ?x8961 n)))
 (let ((?x11086 (stack_t x_0 x_1 x_2 w_1 5 n)))
 (let ((?x4981 (stack_s x_0 x_1 x_2 w_1 6 n)))
 (let (($x1780 (= ?x4981 ?x11086)))
 (or $x1780 $x2543)))))))
 ))
 (let ((?x8961 (sc_t 5)))
 (let ((?x926 (sc_s 6)))
 (let (($x5889 (= ?x926 ?x8961)))
 (let ((?x11378 (used_gas_t x_0 x_1 x_2 w_1 0)))
 (let ((?x355 (used_gas_s x_0 x_1 x_2 w_1 0)))
 (let (($x5132 (= ?x355 ?x11378)))
 (let (($x10930 (forall ((w (_ BitVec 256)) )(let ((?x2415 (storage_t x_0 x_1 x_2 w_1 0 w)))
 (let ((?x702 (storage_s x_0 x_1 x_2 w_1 0 w)))
 (= ?x702 ?x2415))))
 ))
 (let (($x56 (exc_halt_t 0)))
 (let (($x57 (exc_halt_s 0)))
 (let (($x58 (= $x57 $x56)))
 (let (($x6026 (forall ((n (_ BitVec 6)) )(let ((?x63 (sc_t 0)))
 (let (($x5223 (bvsle ?x63 n)))
 (let ((?x693 (stack_t x_0 x_1 x_2 w_1 0 n)))
 (let ((?x7734 (stack_s x_0 x_1 x_2 w_1 0 n)))
 (let (($x3458 (= ?x7734 ?x693)))
 (or $x3458 $x5223)))))))
 ))
 (let ((?x63 (sc_t 0)))
 (let ((?x72 (sc_s 0)))
 (let (($x73 (= ?x72 ?x63)))
 (let (($x2969 (= $x1885 (or (exc_halt_t 4) (not (bvsle (_ bv0 6) (bvadd (_ bv59 6) (sc_t 4))))))))
 (let (($x5261 (forall ((w (_ BitVec 256)) )(let ((?x11435 (storage_t x_0 x_1 x_2 w_1 4 w)))
 (let ((?x6103 (storage_t x_0 x_1 x_2 w_1 5 w)))
 (= ?x6103 ?x11435))))
 ))
 (let (($x8089 (forall ((n (_ BitVec 6)) )(let ((?x4936 (stack_t x_0 x_1 x_2 w_1 4 n)))
 (let ((?x11086 (stack_t x_0 x_1 x_2 w_1 5 n)))
 (or (bvsle (bvadd (_ bv59 6) (sc_t 4)) n) (= ?x11086 ?x4936)))))
 ))
 (let ((?x4818 (sc_t 4)))
 (let (($x3181 (= ?x8961 ?x4818)))
 (let (($x1636 (= (used_gas_t x_0 x_1 x_2 w_1 5) (+ 3 (used_gas_t x_0 x_1 x_2 w_1 4)))))
 (let ((?x1413 (bvadd (_ bv62 6) ?x4818)))
 (let ((?x5542 (stack_t x_0 x_1 x_2 w_1 4 ?x1413)))
 (let ((?x8115 (bvadd (_ bv61 6) ?x4818)))
 (let ((?x5728 (stack_t x_0 x_1 x_2 w_1 4 ?x8115)))
 (let ((?x2888 (bvadd (_ bv60 6) ?x4818)))
 (let ((?x10997 (stack_t x_0 x_1 x_2 w_1 4 ?x2888)))
 (let ((?x7493 (bvadd (_ bv63 6) ?x4818)))
 (let ((?x5780 (stack_t x_0 x_1 x_2 w_1 4 ?x7493)))
 (let (($x3507 (= (stack_t x_0 x_1 x_2 w_1 5 (bvadd (_ bv63 6) ?x8961)) (stack_t x_0 x_1 x_2 w_1 4 (bvadd (_ bv59 6) ?x4818)))))
 (let (($x7854 (exc_halt_t 4)))
 (let (($x8209 (= $x7854 (or (exc_halt_t 3) (not (bvsle (_ bv0 6) (bvadd (_ bv60 6) (sc_t 3))))))))
 (let (($x3136 (forall ((w (_ BitVec 256)) )(let ((?x3031 (storage_t x_0 x_1 x_2 w_1 3 w)))
 (let ((?x11435 (storage_t x_0 x_1 x_2 w_1 4 w)))
 (= ?x11435 ?x3031))))
 ))
 (let (($x5381 (forall ((n (_ BitVec 6)) )(let ((?x11304 (sc_t 3)))
 (let ((?x5177 (bvadd (_ bv60 6) ?x11304)))
 (let (($x10470 (bvsle ?x5177 n)))
 (let ((?x9980 (stack_t x_0 x_1 x_2 w_1 3 n)))
 (let ((?x4936 (stack_t x_0 x_1 x_2 w_1 4 n)))
 (or (= ?x4936 ?x9980) $x10470)))))))
 ))
 (let ((?x11304 (sc_t 3)))
 (let (($x2257 (= ?x4818 ?x11304)))
 (let ((?x7835 (used_gas_t x_0 x_1 x_2 w_1 4)))
 (let (($x9443 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_t 2)))) (_ bv0 1)))))
 (let (($x9287 (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_t 2))))))
 (let (($x2556 (exc_halt_t 2)))
 (let (($x3614 (exc_halt_t 3)))
 (let (($x2388 (forall ((w (_ BitVec 256)) )(let ((?x8903 (storage_t x_0 x_1 x_2 w_1 2 w)))
 (let ((?x3031 (storage_t x_0 x_1 x_2 w_1 3 w)))
 (= ?x3031 ?x8903))))
 ))
 (let (($x7632 (forall ((n (_ BitVec 6)) )(let ((?x1260 (stack_t x_0 x_1 x_2 w_1 2 n)))
 (let ((?x9980 (stack_t x_0 x_1 x_2 w_1 3 n)))
 (let ((?x2714 (sc_t 2)))
 (let ((?x4909 (bvadd (_ bv63 6) ?x2714)))
 (let (($x10496 (bvsle ?x4909 n)))
 (or $x10496 (= ?x9980 ?x1260))))))))
 ))
 (let (($x6852 (= ?x11304 (bvadd (_ bv1 6) (sc_t 2)))))
 (let ((?x983 (used_gas_t x_0 x_1 x_2 w_1 3)))
 (let ((?x2714 (sc_t 2)))
 (let ((?x4909 (bvadd (_ bv63 6) ?x2714)))
 (let ((?x5785 (stack_t x_0 x_1 x_2 w_1 2 ?x4909)))
 (let (($x9202 (= $x2556 (or (exc_halt_t 1) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_t 1))))))))
 (let (($x1919 (forall ((w (_ BitVec 256)) )(let ((?x5693 (storage_t x_0 x_1 x_2 w_1 1 w)))
 (let ((?x8903 (storage_t x_0 x_1 x_2 w_1 2 w)))
 (= ?x8903 ?x5693))))
 ))
 (let (($x1193 (forall ((n (_ BitVec 6)) )(let ((?x7154 (sc_t 1)))
 (let ((?x723 (bvadd (_ bv62 6) ?x7154)))
 (let (($x1462 (bvsle ?x723 n)))
 (let ((?x7961 (stack_t x_0 x_1 x_2 w_1 1 n)))
 (let ((?x1260 (stack_t x_0 x_1 x_2 w_1 2 n)))
 (or (= ?x1260 ?x7961) $x1462)))))))
 ))
 (let ((?x7969 (used_gas_t x_0 x_1 x_2 w_1 2)))
 (let (($x4328 (= (stack_t x_0 x_1 x_2 w_1 2 (bvadd (_ bv62 6) ?x2714)) (stack_t x_0 x_1 x_2 w_1 1 (bvadd (_ bv63 6) (sc_t 1))))))
 (let (($x6286 (= ?x5785 (stack_t x_0 x_1 x_2 w_1 1 (bvadd (_ bv62 6) (sc_t 1))))))
 (let (($x8377 (exc_halt_t 1)))
 (let (($x6851 (= $x8377 (or $x56 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x63))) (_ bv0 1)))))))
 (let (($x187 (forall ((w (_ BitVec 256)) )(let ((?x2415 (storage_t x_0 x_1 x_2 w_1 0 w)))
 (let ((?x5693 (storage_t x_0 x_1 x_2 w_1 1 w)))
 (= ?x5693 ?x2415))))
 ))
 (let (($x7072 (forall ((n (_ BitVec 6)) )(let ((?x693 (stack_t x_0 x_1 x_2 w_1 0 n)))
 (let ((?x7961 (stack_t x_0 x_1 x_2 w_1 1 n)))
 (let ((?x63 (sc_t 0)))
 (let (($x5223 (bvsle ?x63 n)))
 (or $x5223 (= ?x7961 ?x693)))))))
 ))
 (let ((?x7154 (sc_t 1)))
 (let (($x6360 (= ?x7154 (bvadd (_ bv1 6) ?x63))))
 (let (($x10454 (= $x772 (or (exc_halt_s 5) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 5))))))))
 (let (($x9717 (forall ((w (_ BitVec 256)) )(let ((?x6057 (storage_s x_0 x_1 x_2 w_1 5 w)))
 (let ((?x9617 (storage_s x_0 x_1 x_2 w_1 6 w)))
 (= ?x9617 ?x6057))))
 ))
 (let (($x11363 (forall ((n (_ BitVec 6)) )(let ((?x775 (stack_s x_0 x_1 x_2 w_1 5 n)))
 (let ((?x4981 (stack_s x_0 x_1 x_2 w_1 6 n)))
 (let ((?x4319 (sc_s 5)))
 (let ((?x10793 (bvadd (_ bv62 6) ?x4319)))
 (let (($x832 (bvsle ?x10793 n)))
 (or $x832 (= ?x4981 ?x775))))))))
 ))
 (let (($x3747 (= (used_gas_s x_0 x_1 x_2 w_1 6) (+ 3 (used_gas_s x_0 x_1 x_2 w_1 5)))))
 (let ((?x4319 (sc_s 5)))
 (let ((?x5044 (bvadd (_ bv63 6) ?x4319)))
 (let ((?x9178 (stack_s x_0 x_1 x_2 w_1 5 ?x5044)))
 (let (($x5049 (= (stack_s x_0 x_1 x_2 w_1 6 (bvadd (_ bv63 6) ?x926)) (stack_s x_0 x_1 x_2 w_1 5 (bvadd (_ bv62 6) ?x4319)))))
 (let (($x7172 (exc_halt_s 4)))
 (let (($x5772 (or (not (bvsle (_ bv0 6) (bvadd (_ bv61 6) (sc_s 4)))) $x7172 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 4)))) (_ bv0 1))))))
 (let (($x11317 (exc_halt_s 5)))
 (let (($x6547 (forall ((w (_ BitVec 256)) )(let ((?x9622 (storage_s x_0 x_1 x_2 w_1 4 w)))
 (let ((?x6057 (storage_s x_0 x_1 x_2 w_1 5 w)))
 (= ?x6057 ?x9622))))
 ))
 (let (($x8263 (forall ((n (_ BitVec 6)) )(let ((?x9937 (stack_s x_0 x_1 x_2 w_1 4 n)))
 (let ((?x775 (stack_s x_0 x_1 x_2 w_1 5 n)))
 (or (= ?x775 ?x9937) (bvsle (bvadd (_ bv61 6) (sc_s 4)) n)))))
 ))
 (let ((?x7304 (used_gas_s x_0 x_1 x_2 w_1 5)))
 (let ((?x4305 (sc_s 4)))
 (let ((?x11384 (bvadd (_ bv63 6) ?x4305)))
 (let ((?x1788 (stack_s x_0 x_1 x_2 w_1 4 ?x11384)))
 (let ((?x9687 (bvadd (_ bv62 6) ?x4305)))
 (let ((?x4650 (stack_s x_0 x_1 x_2 w_1 4 ?x9687)))
 (let ((?x9006 (bvadd (_ bv61 6) ?x4305)))
 (let ((?x9579 (stack_s x_0 x_1 x_2 w_1 4 ?x9006)))
 (let (($x2827 (= $x7172 (or (exc_halt_s 3) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 3))))))))
 (let (($x3532 (forall ((w (_ BitVec 256)) )(let ((?x6629 (storage_s x_0 x_1 x_2 w_1 3 w)))
 (let ((?x9622 (storage_s x_0 x_1 x_2 w_1 4 w)))
 (= ?x9622 ?x6629))))
 ))
 (let (($x10463 (forall ((n (_ BitVec 6)) )(let ((?x275 (sc_s 3)))
 (let ((?x8423 (bvadd (_ bv62 6) ?x275)))
 (let (($x11620 (bvsle ?x8423 n)))
 (let ((?x8088 (stack_s x_0 x_1 x_2 w_1 3 n)))
 (let ((?x9937 (stack_s x_0 x_1 x_2 w_1 4 n)))
 (or (= ?x9937 ?x8088) $x11620)))))))
 ))
 (let ((?x275 (sc_s 3)))
 (let (($x10167 (= ?x4305 ?x275)))
 (let ((?x7289 (used_gas_s x_0 x_1 x_2 w_1 4)))
 (let (($x4983 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 2)))) (_ bv0 1)))))
 (let (($x247 (exc_halt_s 2)))
 (let (($x292 (exc_halt_s 3)))
 (let (($x5300 (= $x292 (or $x247 $x4983))))
 (let (($x9751 (forall ((w (_ BitVec 256)) )(let ((?x11127 (storage_s x_0 x_1 x_2 w_1 2 w)))
 (let ((?x6629 (storage_s x_0 x_1 x_2 w_1 3 w)))
 (= ?x6629 ?x11127))))
 ))
 (let (($x7693 (forall ((n (_ BitVec 6)) )(let ((?x218 (sc_s 2)))
 (let (($x4739 (bvsle ?x218 n)))
 (let ((?x8225 (stack_s x_0 x_1 x_2 w_1 2 n)))
 (let ((?x8088 (stack_s x_0 x_1 x_2 w_1 3 n)))
 (or (= ?x8088 ?x8225) $x4739))))))
 ))
 (let (($x11123 (= ?x275 (bvadd (_ bv1 6) (sc_s 2)))))
 (let ((?x760 (used_gas_s x_0 x_1 x_2 w_1 3)))
 (let (($x4848 (= $x247 (or (exc_halt_s 1) (not (bvsle (_ bv0 6) (bvadd (_ bv61 6) (sc_s 1))))))))
 (let (($x8533 (forall ((w (_ BitVec 256)) )(let ((?x5688 (storage_s x_0 x_1 x_2 w_1 1 w)))
 (let ((?x11127 (storage_s x_0 x_1 x_2 w_1 2 w)))
 (= ?x11127 ?x5688))))
 ))
 (let (($x2585 (forall ((n (_ BitVec 6)) )(let ((?x5922 (stack_s x_0 x_1 x_2 w_1 1 n)))
 (let ((?x8225 (stack_s x_0 x_1 x_2 w_1 2 n)))
 (let ((?x154 (sc_s 1)))
 (let ((?x6317 (bvadd (_ bv61 6) ?x154)))
 (let (($x2273 (bvsle ?x6317 n)))
 (or $x2273 (= ?x8225 ?x5922))))))))
 ))
 (let ((?x154 (sc_s 1)))
 (let ((?x218 (sc_s 2)))
 (let (($x5559 (= ?x218 ?x154)))
 (let ((?x1042 (used_gas_s x_0 x_1 x_2 w_1 2)))
 (let ((?x4794 (bvadd (_ bv62 6) ?x154)))
 (let ((?x9473 (stack_s x_0 x_1 x_2 w_1 1 ?x4794)))
 (let ((?x1136 (bvadd (_ bv63 6) ?x154)))
 (let ((?x11554 (stack_s x_0 x_1 x_2 w_1 1 ?x1136)))
 (let (($x11830 (= (stack_s x_0 x_1 x_2 w_1 2 (bvadd (_ bv63 6) ?x218)) (stack_s x_0 x_1 x_2 w_1 1 (bvadd (_ bv61 6) ?x154)))))
 (let (($x7672 (forall ((w (_ BitVec 256)) )(let ((?x702 (storage_s x_0 x_1 x_2 w_1 0 w)))
 (let ((?x5688 (storage_s x_0 x_1 x_2 w_1 1 w)))
 (= ?x5688 ?x702))))
 ))
 (let (($x3666 (forall ((n (_ BitVec 6)) )(let ((?x7734 (stack_s x_0 x_1 x_2 w_1 0 n)))
 (let ((?x5922 (stack_s x_0 x_1 x_2 w_1 1 n)))
 (or (= ?x5922 ?x7734) (bvsle (bvadd (_ bv62 6) (sc_s 0)) n)))))
 ))
 (let (($x6048 (forall ((w (_ BitVec 256)) )(let ((?x702 (storage_s x_0 x_1 x_2 w_1 0 w)))
 (= ?x702 (_ bv0 256))))
 ))
 (let (($x10849 (= ?x355 0)))
 (let (($x4616 (not $x57)))
 (let (($x11202 (= (stack_s x_0 x_1 x_2 w_1 0 (_ bv2 6)) x_2)))
 (let (($x2254 (= (stack_s x_0 x_1 x_2 w_1 0 (_ bv1 6)) x_1)))
 (let (($x5804 (= (stack_s x_0 x_1 x_2 w_1 0 (_ bv0 6)) x_0)))
 (let (($x3492 (= ?x72 (_ bv3 6))))
 (and $x3492 $x5804 $x2254 $x11202 $x4616 $x10849 $x6048 (= ?x11554 (stack_s x_0 x_1 x_2 w_1 0 (bvadd (_ bv62 6) ?x72))) (= ?x9473 (stack_s x_0 x_1 x_2 w_1 0 (bvadd (_ bv63 6) ?x72))) (= (used_gas_s x_0 x_1 x_2 w_1 1) (+ 3 ?x355)) (= ?x154 ?x72) $x3666 $x7672 (= (exc_halt_s 1) (or $x57 (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) ?x72))))) $x11830 (= (stack_s x_0 x_1 x_2 w_1 2 (bvadd (_ bv61 6) ?x218)) ?x11554) (= (stack_s x_0 x_1 x_2 w_1 2 (bvadd (_ bv62 6) ?x218)) ?x9473) (= ?x1042 (+ 3 (used_gas_s x_0 x_1 x_2 w_1 1))) $x5559 $x2585 $x8533 $x4848 (= (stack_s x_0 x_1 x_2 w_1 3 ?x218) w_1) (= ?x760 (+ 3 ?x1042)) $x11123 $x7693 $x9751 $x5300 (= ?x1788 (stack_s x_0 x_1 x_2 w_1 3 (bvadd (_ bv62 6) ?x275))) (= ?x4650 (stack_s x_0 x_1 x_2 w_1 3 (bvadd (_ bv63 6) ?x275))) (= ?x7289 (+ 3 ?x760)) $x10167 $x10463 $x3532 $x2827 (= ?x9178 ?x9579) (= (stack_s x_0 x_1 x_2 w_1 5 ?x9006) ?x9579) (= (stack_s x_0 x_1 x_2 w_1 5 ?x9687) ?x4650) (= (stack_s x_0 x_1 x_2 w_1 5 ?x11384) ?x1788) (= ?x7304 (+ 3 ?x7289)) (= ?x4319 (bvadd (_ bv1 6) ?x4305)) $x8263 $x6547 (= $x11317 $x5772) $x5049 (= (stack_s x_0 x_1 x_2 w_1 6 (bvadd (_ bv62 6) ?x926)) ?x9178) $x3747 (= ?x926 ?x4319) $x11363 $x9717 $x10454 (= (stack_t x_0 x_1 x_2 w_1 1 ?x63) w_1) (= (used_gas_t x_0 x_1 x_2 w_1 1) (+ 3 ?x11378)) $x6360 $x7072 $x187 $x6851 $x6286 $x4328 (= ?x7969 (+ 3 (used_gas_t x_0 x_1 x_2 w_1 1))) (= ?x2714 ?x7154) $x1193 $x1919 $x9202 (= (stack_t x_0 x_1 x_2 w_1 3 (bvadd (_ bv63 6) ?x11304)) ?x5785) (= (stack_t x_0 x_1 x_2 w_1 3 ?x4909) ?x5785) (= ?x983 (+ 3 ?x7969)) $x6852 $x7632 $x2388 (= $x3614 (or $x2556 $x9287 $x9443)) (= ?x5780 (stack_t x_0 x_1 x_2 w_1 3 (bvadd (_ bv60 6) ?x11304))) (= ?x10997 (stack_t x_0 x_1 x_2 w_1 3 (bvadd (_ bv63 6) ?x11304))) (= ?x5728 (stack_t x_0 x_1 x_2 w_1 3 (bvadd (_ bv61 6) ?x11304))) (= ?x5542 (stack_t x_0 x_1 x_2 w_1 3 (bvadd (_ bv62 6) ?x11304))) (= ?x7835 (+ 3 ?x983)) $x2257 $x5381 $x3136 $x8209 $x3507 (= (stack_t x_0 x_1 x_2 w_1 5 (bvadd (_ bv59 6) ?x8961)) ?x5780) (= (stack_t x_0 x_1 x_2 w_1 5 (bvadd (_ bv60 6) ?x8961)) ?x10997) (= (stack_t x_0 x_1 x_2 w_1 5 (bvadd (_ bv61 6) ?x8961)) ?x5728) (= (stack_t x_0 x_1 x_2 w_1 5 (bvadd (_ bv62 6) ?x8961)) ?x5542) $x1636 $x3181 $x8089 $x5261 $x2969 $x73 $x6026 $x58 $x10930 $x5132 (not (and $x5889 $x11208 $x11108 $x9017))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
 )
(check-sat)
(get-proof)
