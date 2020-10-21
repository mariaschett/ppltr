; PUSH cw_1 SWAP2 SWAP1 MLOAD SWAP1 DUP2 => MLOAD DUP1 PUSH cw_1 SWAP3 SWAP1
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
 (exists ((x_0 (_ BitVec 256)) (x_1 (_ BitVec 256)) (x_MLOAD_0 (_ BitVec 256)) )(let (($x6082 (forall ((w (_ BitVec 256)) )(let ((?x2719 (storage_t x_0 x_1 w_1 x_MLOAD_0 5 w)))
 (let ((?x11959 (storage_s x_0 x_1 w_1 x_MLOAD_0 6 w)))
 (= ?x11959 ?x2719))))
 ))
 (let (($x1885 (exc_halt_t 5)))
 (let (($x772 (exc_halt_s 6)))
 (let (($x11108 (= $x772 $x1885)))
 (let (($x51 (forall ((n (_ BitVec 6)) )(let ((?x9903 (stack_t x_0 x_1 w_1 x_MLOAD_0 5 n)))
 (let ((?x8820 (stack_s x_0 x_1 w_1 x_MLOAD_0 6 n)))
 (let (($x3512 (= ?x8820 ?x9903)))
 (or (bvsle (sc_t 5) n) $x3512)))))
 ))
 (let ((?x8961 (sc_t 5)))
 (let ((?x926 (sc_s 6)))
 (let (($x5889 (= ?x926 ?x8961)))
 (let ((?x822 (used_gas_t x_0 x_1 w_1 x_MLOAD_0 0)))
 (let ((?x2019 (used_gas_s x_0 x_1 w_1 x_MLOAD_0 0)))
 (let (($x7403 (= ?x2019 ?x822)))
 (let (($x11101 (forall ((w (_ BitVec 256)) )(let ((?x8555 (storage_t x_0 x_1 w_1 x_MLOAD_0 0 w)))
 (let ((?x9977 (storage_s x_0 x_1 w_1 x_MLOAD_0 0 w)))
 (= ?x9977 ?x8555))))
 ))
 (let (($x56 (exc_halt_t 0)))
 (let (($x57 (exc_halt_s 0)))
 (let (($x58 (= $x57 $x56)))
 (let (($x8635 (forall ((n (_ BitVec 6)) )(let ((?x9760 (stack_t x_0 x_1 w_1 x_MLOAD_0 0 n)))
 (let ((?x4910 (stack_s x_0 x_1 w_1 x_MLOAD_0 0 n)))
 (let (($x7562 (= ?x4910 ?x9760)))
 (or (bvsle (sc_t 0) n) $x7562)))))
 ))
 (let ((?x63 (sc_t 0)))
 (let ((?x72 (sc_s 0)))
 (let (($x73 (= ?x72 ?x63)))
 (let (($x4118 (= $x1885 (or (exc_halt_t 4) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_t 4))))))))
 (let (($x5540 (forall ((w (_ BitVec 256)) )(let ((?x10829 (storage_t x_0 x_1 w_1 x_MLOAD_0 4 w)))
 (let ((?x2719 (storage_t x_0 x_1 w_1 x_MLOAD_0 5 w)))
 (= ?x2719 ?x10829))))
 ))
 (let (($x1328 (forall ((n (_ BitVec 6)) )(let ((?x7122 (stack_t x_0 x_1 w_1 x_MLOAD_0 4 n)))
 (let ((?x9903 (stack_t x_0 x_1 w_1 x_MLOAD_0 5 n)))
 (or (bvsle (bvadd (_ bv62 6) (sc_t 4)) n) (= ?x9903 ?x7122)))))
 ))
 (let (($x10774 (= (used_gas_t x_0 x_1 w_1 x_MLOAD_0 5) (+ 3 (used_gas_t x_0 x_1 w_1 x_MLOAD_0 4)))))
 (let ((?x5746 (stack_t x_0 x_1 w_1 x_MLOAD_0 4 (bvadd (_ bv63 6) (sc_t 4)))))
 (let ((?x4818 (sc_t 4)))
 (let ((?x1413 (bvadd (_ bv62 6) ?x4818)))
 (let ((?x6474 (stack_t x_0 x_1 w_1 x_MLOAD_0 4 ?x1413)))
 (let (($x7854 (exc_halt_t 4)))
 (let (($x8209 (= $x7854 (or (exc_halt_t 3) (not (bvsle (_ bv0 6) (bvadd (_ bv60 6) (sc_t 3))))))))
 (let (($x8932 (forall ((w (_ BitVec 256)) )(let ((?x1065 (storage_t x_0 x_1 w_1 x_MLOAD_0 3 w)))
 (let ((?x10829 (storage_t x_0 x_1 w_1 x_MLOAD_0 4 w)))
 (= ?x10829 ?x1065))))
 ))
 (let (($x1733 (forall ((n (_ BitVec 6)) )(let ((?x11703 (stack_t x_0 x_1 w_1 x_MLOAD_0 3 n)))
 (let ((?x7122 (stack_t x_0 x_1 w_1 x_MLOAD_0 4 n)))
 (or (= ?x7122 ?x11703) (bvsle (bvadd (_ bv60 6) (sc_t 3)) n)))))
 ))
 (let ((?x8770 (used_gas_t x_0 x_1 w_1 x_MLOAD_0 4)))
 (let (($x1352 (= ?x6474 (stack_t x_0 x_1 w_1 x_MLOAD_0 3 (bvadd (_ bv62 6) (sc_t 3))))))
 (let (($x4103 (= (stack_t x_0 x_1 w_1 x_MLOAD_0 4 (bvadd (_ bv61 6) ?x4818)) (stack_t x_0 x_1 w_1 x_MLOAD_0 3 (bvadd (_ bv61 6) (sc_t 3))))))
 (let (($x2238 (= (stack_t x_0 x_1 w_1 x_MLOAD_0 4 (bvadd (_ bv60 6) ?x4818)) (stack_t x_0 x_1 w_1 x_MLOAD_0 3 (bvadd (_ bv63 6) (sc_t 3))))))
 (let (($x2296 (= ?x5746 (stack_t x_0 x_1 w_1 x_MLOAD_0 3 (bvadd (_ bv60 6) (sc_t 3))))))
 (let (($x2556 (exc_halt_t 2)))
 (let (($x6720 (or $x2556 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_t 2)))) (_ bv0 1))))))
 (let (($x3614 (exc_halt_t 3)))
 (let (($x11100 (forall ((w (_ BitVec 256)) )(let ((?x9591 (storage_t x_0 x_1 w_1 x_MLOAD_0 2 w)))
 (let ((?x1065 (storage_t x_0 x_1 w_1 x_MLOAD_0 3 w)))
 (= ?x1065 ?x9591))))
 ))
 (let (($x2853 (forall ((n (_ BitVec 6)) )(let ((?x3907 (stack_t x_0 x_1 w_1 x_MLOAD_0 2 n)))
 (let ((?x11703 (stack_t x_0 x_1 w_1 x_MLOAD_0 3 n)))
 (or (= ?x11703 ?x3907) (bvsle (sc_t 2) n)))))
 ))
 (let ((?x10906 (used_gas_t x_0 x_1 w_1 x_MLOAD_0 3)))
 (let (($x8377 (exc_halt_t 1)))
 (let (($x6253 (or (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_t 1)))) (_ bv0 1))) $x8377 (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_t 1)))))))
 (let (($x7220 (forall ((w (_ BitVec 256)) )(let ((?x6453 (storage_t x_0 x_1 w_1 x_MLOAD_0 1 w)))
 (let ((?x9591 (storage_t x_0 x_1 w_1 x_MLOAD_0 2 w)))
 (= ?x9591 ?x6453))))
 ))
 (let (($x8474 (forall ((n (_ BitVec 6)) )(let ((?x10432 (stack_t x_0 x_1 w_1 x_MLOAD_0 1 n)))
 (let ((?x3907 (stack_t x_0 x_1 w_1 x_MLOAD_0 2 n)))
 (or (bvsle (bvadd (_ bv63 6) (sc_t 1)) n) (= ?x3907 ?x10432)))))
 ))
 (let ((?x4790 (used_gas_t x_0 x_1 w_1 x_MLOAD_0 2)))
 (let ((?x7154 (sc_t 1)))
 (let ((?x9462 (bvadd (_ bv63 6) ?x7154)))
 (let ((?x1953 (stack_t x_0 x_1 w_1 x_MLOAD_0 1 ?x9462)))
 (let (($x1408 (= (stack_t x_0 x_1 w_1 x_MLOAD_0 2 (bvadd (_ bv63 6) (sc_t 2))) ?x1953)))
 (let (($x4549 (forall ((w (_ BitVec 256)) )(let ((?x8555 (storage_t x_0 x_1 w_1 x_MLOAD_0 0 w)))
 (let ((?x6453 (storage_t x_0 x_1 w_1 x_MLOAD_0 1 w)))
 (= ?x6453 ?x8555))))
 ))
 (let (($x8506 (forall ((n (_ BitVec 6)) )(let ((?x9760 (stack_t x_0 x_1 w_1 x_MLOAD_0 0 n)))
 (let ((?x10432 (stack_t x_0 x_1 w_1 x_MLOAD_0 1 n)))
 (or (bvsle (bvadd (_ bv63 6) (sc_t 0)) n) (= ?x10432 ?x9760)))))
 ))
 (let ((?x1105 (f_MLOAD x_0 x_1 w_1 x_MLOAD_0 (stack_t x_0 x_1 w_1 x_MLOAD_0 0 (bvadd (_ bv63 6) ?x63)))))
 (let (($x11317 (exc_halt_s 5)))
 (let (($x10924 (or (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 5)))) (_ bv0 1))) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 5)))) $x11317)))
 (let (($x9725 (forall ((w (_ BitVec 256)) )(let ((?x4805 (storage_s x_0 x_1 w_1 x_MLOAD_0 5 w)))
 (let ((?x11959 (storage_s x_0 x_1 w_1 x_MLOAD_0 6 w)))
 (= ?x11959 ?x4805))))
 ))
 (let (($x2986 (forall ((n (_ BitVec 6)) )(let ((?x11093 (stack_s x_0 x_1 w_1 x_MLOAD_0 5 n)))
 (let ((?x8820 (stack_s x_0 x_1 w_1 x_MLOAD_0 6 n)))
 (or (bvsle (bvadd (_ bv62 6) (sc_s 5)) n) (= ?x8820 ?x11093)))))
 ))
 (let (($x11225 (= (used_gas_s x_0 x_1 w_1 x_MLOAD_0 6) (+ 3 (used_gas_s x_0 x_1 w_1 x_MLOAD_0 5)))))
 (let ((?x4319 (sc_s 5)))
 (let ((?x5044 (bvadd (_ bv63 6) ?x4319)))
 (let ((?x3732 (stack_s x_0 x_1 w_1 x_MLOAD_0 5 ?x5044)))
 (let ((?x10793 (bvadd (_ bv62 6) ?x4319)))
 (let ((?x4520 (stack_s x_0 x_1 w_1 x_MLOAD_0 5 ?x10793)))
 (let (($x4096 (= $x11317 (or (exc_halt_s 4) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 4))))))))
 (let (($x1553 (forall ((w (_ BitVec 256)) )(let ((?x6506 (storage_s x_0 x_1 w_1 x_MLOAD_0 4 w)))
 (let ((?x4805 (storage_s x_0 x_1 w_1 x_MLOAD_0 5 w)))
 (= ?x4805 ?x6506))))
 ))
 (let (($x8663 (forall ((n (_ BitVec 6)) )(let ((?x479 (stack_s x_0 x_1 w_1 x_MLOAD_0 4 n)))
 (let ((?x11093 (stack_s x_0 x_1 w_1 x_MLOAD_0 5 n)))
 (or (= ?x11093 ?x479) (bvsle (bvadd (_ bv62 6) (sc_s 4)) n)))))
 ))
 (let ((?x7378 (used_gas_s x_0 x_1 w_1 x_MLOAD_0 5)))
 (let ((?x286 (stack_s x_0 x_1 w_1 x_MLOAD_0 4 (bvadd (_ bv63 6) (sc_s 4)))))
 (let (($x3401 (= ?x3732 (stack_s x_0 x_1 w_1 x_MLOAD_0 4 (bvadd (_ bv62 6) (sc_s 4))))))
 (let (($x7172 (exc_halt_s 4)))
 (let (($x10935 (= $x7172 (or (exc_halt_s 3) (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_s 3))))))))
 (let (($x7902 (forall ((w (_ BitVec 256)) )(let ((?x4334 (storage_s x_0 x_1 w_1 x_MLOAD_0 3 w)))
 (let ((?x6506 (storage_s x_0 x_1 w_1 x_MLOAD_0 4 w)))
 (= ?x6506 ?x4334))))
 ))
 (let (($x5885 (forall ((n (_ BitVec 6)) )(let ((?x4301 (stack_s x_0 x_1 w_1 x_MLOAD_0 3 n)))
 (let ((?x479 (stack_s x_0 x_1 w_1 x_MLOAD_0 4 n)))
 (or (= ?x479 ?x4301) (bvsle (bvadd (_ bv63 6) (sc_s 3)) n)))))
 ))
 (let ((?x3471 (used_gas_s x_0 x_1 w_1 x_MLOAD_0 4)))
 (let ((?x275 (sc_s 3)))
 (let ((?x5611 (bvadd (_ bv63 6) ?x275)))
 (let ((?x2212 (stack_s x_0 x_1 w_1 x_MLOAD_0 3 ?x5611)))
 (let (($x292 (exc_halt_s 3)))
 (let (($x8129 (= $x292 (or (exc_halt_s 2) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 2))))))))
 (let (($x10106 (forall ((w (_ BitVec 256)) )(let ((?x6927 (storage_s x_0 x_1 w_1 x_MLOAD_0 2 w)))
 (let ((?x4334 (storage_s x_0 x_1 w_1 x_MLOAD_0 3 w)))
 (= ?x4334 ?x6927))))
 ))
 (let (($x3573 (forall ((n (_ BitVec 6)) )(let ((?x7133 (stack_s x_0 x_1 w_1 x_MLOAD_0 2 n)))
 (let ((?x4301 (stack_s x_0 x_1 w_1 x_MLOAD_0 3 n)))
 (or (= ?x4301 ?x7133) (bvsle (bvadd (_ bv62 6) (sc_s 2)) n)))))
 ))
 (let ((?x6627 (used_gas_s x_0 x_1 w_1 x_MLOAD_0 3)))
 (let ((?x2473 (stack_s x_0 x_1 w_1 x_MLOAD_0 2 (bvadd (_ bv63 6) (sc_s 2)))))
 (let ((?x218 (sc_s 2)))
 (let ((?x6221 (bvadd (_ bv62 6) ?x218)))
 (let ((?x4966 (stack_s x_0 x_1 w_1 x_MLOAD_0 2 ?x6221)))
 (let (($x247 (exc_halt_s 2)))
 (let (($x4848 (= $x247 (or (exc_halt_s 1) (not (bvsle (_ bv0 6) (bvadd (_ bv61 6) (sc_s 1))))))))
 (let (($x544 (forall ((w (_ BitVec 256)) )(let ((?x14 (storage_s x_0 x_1 w_1 x_MLOAD_0 1 w)))
 (let ((?x6927 (storage_s x_0 x_1 w_1 x_MLOAD_0 2 w)))
 (= ?x6927 ?x14))))
 ))
 (let (($x7633 (forall ((n (_ BitVec 6)) )(let ((?x1460 (stack_s x_0 x_1 w_1 x_MLOAD_0 1 n)))
 (let ((?x7133 (stack_s x_0 x_1 w_1 x_MLOAD_0 2 n)))
 (or (bvsle (bvadd (_ bv61 6) (sc_s 1)) n) (= ?x7133 ?x1460)))))
 ))
 (let ((?x11802 (used_gas_s x_0 x_1 w_1 x_MLOAD_0 2)))
 (let (($x6520 (= ?x4966 (stack_s x_0 x_1 w_1 x_MLOAD_0 1 (bvadd (_ bv62 6) (sc_s 1))))))
 (let (($x8919 (= (stack_s x_0 x_1 w_1 x_MLOAD_0 2 (bvadd (_ bv61 6) ?x218)) (stack_s x_0 x_1 w_1 x_MLOAD_0 1 (bvadd (_ bv63 6) (sc_s 1))))))
 (let (($x7184 (= ?x2473 (stack_s x_0 x_1 w_1 x_MLOAD_0 1 (bvadd (_ bv61 6) (sc_s 1))))))
 (let (($x189 (exc_halt_s 1)))
 (let (($x11129 (= $x189 (or $x57 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x72))) (_ bv0 1)))))))
 (let (($x9406 (forall ((w (_ BitVec 256)) )(let ((?x9977 (storage_s x_0 x_1 w_1 x_MLOAD_0 0 w)))
 (let ((?x14 (storage_s x_0 x_1 w_1 x_MLOAD_0 1 w)))
 (= ?x14 ?x9977))))
 ))
 (let (($x7461 (forall ((n (_ BitVec 6)) )(let ((?x4910 (stack_s x_0 x_1 w_1 x_MLOAD_0 0 n)))
 (let ((?x1460 (stack_s x_0 x_1 w_1 x_MLOAD_0 1 n)))
 (or (bvsle (sc_s 0) n) (= ?x1460 ?x4910)))))
 ))
 (let (($x11495 (forall ((w0 (_ BitVec 256)) )(let (($x11619 (= (stack_s x_0 x_1 w_1 x_MLOAD_0 3 (bvadd (_ bv63 6) (sc_s 3))) w0)))
 (let ((?x7772 (f_MLOAD x_0 x_1 w_1 x_MLOAD_0 w0)))
 (= ?x7772 (ite $x11619 x_MLOAD_0 (_ bv0 256))))))
 ))
 (let (($x4964 (forall ((w (_ BitVec 256)) )(let ((?x9977 (storage_s x_0 x_1 w_1 x_MLOAD_0 0 w)))
 (= ?x9977 (_ bv0 256))))
 ))
 (let (($x9455 (= ?x2019 0)))
 (let (($x6469 (= (stack_s x_0 x_1 w_1 x_MLOAD_0 0 (_ bv1 6)) x_1)))
 (let (($x9846 (= (stack_s x_0 x_1 w_1 x_MLOAD_0 0 (_ bv0 6)) x_0)))
 (let (($x11128 (= ?x72 (_ bv2 6))))
 (and $x11128 $x9846 $x6469 (not $x57) $x9455 $x4964 $x11495 (= (stack_s x_0 x_1 w_1 x_MLOAD_0 1 ?x72) w_1) (= (used_gas_s x_0 x_1 w_1 x_MLOAD_0 1) (+ 3 ?x2019)) (= (sc_s 1) (bvadd (_ bv1 6) ?x72)) $x7461 $x9406 $x11129 $x7184 $x8919 $x6520 (= ?x11802 (+ 3 (used_gas_s x_0 x_1 w_1 x_MLOAD_0 1))) (= ?x218 (sc_s 1)) $x7633 $x544 $x4848 (= ?x2212 ?x4966) (= (stack_s x_0 x_1 w_1 x_MLOAD_0 3 (bvadd (_ bv62 6) ?x275)) ?x2473) (= ?x6627 (+ 3 ?x11802)) (= ?x275 ?x218) $x3573 $x10106 $x8129 (= ?x286 (f_MLOAD x_0 x_1 w_1 x_MLOAD_0 ?x2212)) (= ?x3471 (+ 3 ?x6627)) (= (sc_s 4) ?x275) $x5885 $x7902 $x10935 $x3401 (= ?x4520 ?x286) (= ?x7378 (+ 3 ?x3471)) (= ?x4319 (sc_s 4)) $x8663 $x1553 $x4096 (= (stack_s x_0 x_1 w_1 x_MLOAD_0 6 (bvadd (_ bv63 6) ?x926)) ?x4520) (= (stack_s x_0 x_1 w_1 x_MLOAD_0 6 ?x10793) ?x4520) (= (stack_s x_0 x_1 w_1 x_MLOAD_0 6 ?x5044) ?x3732) $x11225 (= ?x926 (bvadd (_ bv1 6) ?x4319)) $x2986 $x9725 (= $x772 $x10924) (= ?x1953 ?x1105) (= (used_gas_t x_0 x_1 w_1 x_MLOAD_0 1) (+ 3 ?x822)) (= ?x7154 ?x63) $x8506 $x4549 (= $x8377 (or $x56 (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) ?x63))))) $x1408 (= (stack_t x_0 x_1 w_1 x_MLOAD_0 2 ?x9462) ?x1953) (= ?x4790 (+ 3 (used_gas_t x_0 x_1 w_1 x_MLOAD_0 1))) (= (sc_t 2) (bvadd (_ bv1 6) ?x7154)) $x8474 $x7220 (= $x2556 $x6253) (= (stack_t x_0 x_1 w_1 x_MLOAD_0 3 (sc_t 2)) w_1) (= ?x10906 (+ 3 ?x4790)) (= (sc_t 3) (bvadd (_ bv1 6) (sc_t 2))) $x2853 $x11100 (= $x3614 $x6720) $x2296 $x2238 $x4103 $x1352 (= ?x8770 (+ 3 ?x10906)) (= ?x4818 (sc_t 3)) $x1733 $x8932 $x8209 (= (stack_t x_0 x_1 w_1 x_MLOAD_0 5 (bvadd (_ bv63 6) ?x8961)) ?x6474) (= (stack_t x_0 x_1 w_1 x_MLOAD_0 5 (bvadd (_ bv62 6) ?x8961)) ?x5746) $x10774 (= ?x8961 ?x4818) $x1328 $x5540 $x4118 $x73 $x8635 $x58 $x11101 $x7403 (not (and $x5889 $x51 $x11108 $x6082)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
 )
(check-sat)
(get-proof)
