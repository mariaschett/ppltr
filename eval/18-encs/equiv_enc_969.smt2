; SWAP1 DUP2 SWAP1 DUP5 SWAP1 => DUP4 DUP2 SWAP3
(set-option :produce-proofs true)
; 
(set-info :status unknown)
(declare-fun storage_t ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 256)) (_ BitVec 256))
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
 (exists ((x_0 (_ BitVec 256)) (x_1 (_ BitVec 256)) (x_2 (_ BitVec 256)) (x_3 (_ BitVec 256)) )(let (($x3316 (forall ((w (_ BitVec 256)) )(let ((?x5991 (storage_t x_0 x_1 x_2 x_3 3 w)))
 (let ((?x3900 (storage_s x_0 x_1 x_2 x_3 5 w)))
 (= ?x3900 ?x5991))))
 ))
 (let (($x5429 (exc_halt_t 3)))
 (let (($x1862 (exc_halt_s 5)))
 (let (($x3022 (= $x1862 $x5429)))
 (let (($x529 (forall ((n (_ BitVec 6)) )(let ((?x10526 (stack_t x_0 x_1 x_2 x_3 3 n)))
 (let ((?x9017 (stack_s x_0 x_1 x_2 x_3 5 n)))
 (let (($x1723 (= ?x9017 ?x10526)))
 (let ((?x7621 (sc_t 3)))
 (let (($x438 (bvsle ?x7621 n)))
 (or $x438 $x1723)))))))
 ))
 (let ((?x7621 (sc_t 3)))
 (let ((?x4319 (sc_s 5)))
 (let (($x3981 (= ?x4319 ?x7621)))
 (let ((?x11593 (used_gas_t x_0 x_1 x_2 x_3 0)))
 (let ((?x4303 (used_gas_s x_0 x_1 x_2 x_3 0)))
 (let (($x3707 (= ?x4303 ?x11593)))
 (let (($x1470 (forall ((w (_ BitVec 256)) )(let ((?x2103 (storage_t x_0 x_1 x_2 x_3 0 w)))
 (let ((?x6743 (storage_s x_0 x_1 x_2 x_3 0 w)))
 (= ?x6743 ?x2103))))
 ))
 (let (($x56 (exc_halt_t 0)))
 (let (($x57 (exc_halt_s 0)))
 (let (($x58 (= $x57 $x56)))
 (let (($x11377 (forall ((n (_ BitVec 6)) )(let ((?x63 (sc_t 0)))
 (let (($x11318 (bvsle ?x63 n)))
 (let ((?x3962 (stack_t x_0 x_1 x_2 x_3 0 n)))
 (let ((?x7340 (stack_s x_0 x_1 x_2 x_3 0 n)))
 (let (($x11107 (= ?x7340 ?x3962)))
 (or $x11107 $x11318)))))))
 ))
 (let ((?x63 (sc_t 0)))
 (let ((?x72 (sc_s 0)))
 (let (($x73 (= ?x72 ?x63)))
 (let (($x5589 (= $x5429 (or (exc_halt_t 2) (not (bvsle (_ bv0 6) (bvadd (_ bv60 6) (sc_t 2))))))))
 (let (($x2986 (forall ((w (_ BitVec 256)) )(let ((?x4361 (storage_t x_0 x_1 x_2 x_3 2 w)))
 (let ((?x5991 (storage_t x_0 x_1 x_2 x_3 3 w)))
 (= ?x5991 ?x4361))))
 ))
 (let (($x1338 (forall ((n (_ BitVec 6)) )(let ((?x10979 (stack_t x_0 x_1 x_2 x_3 2 n)))
 (let ((?x10526 (stack_t x_0 x_1 x_2 x_3 3 n)))
 (let ((?x8890 (sc_t 2)))
 (let ((?x5041 (bvadd (_ bv60 6) ?x8890)))
 (let (($x6626 (bvsle ?x5041 n)))
 (or $x6626 (= ?x10526 ?x10979))))))))
 ))
 (let ((?x8890 (sc_t 2)))
 (let (($x5385 (= ?x7621 ?x8890)))
 (let (($x7262 (= (used_gas_t x_0 x_1 x_2 x_3 3) (+ 3 (used_gas_t x_0 x_1 x_2 x_3 2)))))
 (let (($x8308 (= (stack_t x_0 x_1 x_2 x_3 3 (bvadd (_ bv62 6) ?x7621)) (stack_t x_0 x_1 x_2 x_3 2 (bvadd (_ bv62 6) ?x8890)))))
 (let (($x10462 (= (stack_t x_0 x_1 x_2 x_3 3 (bvadd (_ bv61 6) ?x7621)) (stack_t x_0 x_1 x_2 x_3 2 (bvadd (_ bv61 6) ?x8890)))))
 (let ((?x6760 (bvadd (_ bv63 6) ?x8890)))
 (let ((?x11948 (stack_t x_0 x_1 x_2 x_3 2 ?x6760)))
 (let (($x1968 (= (stack_t x_0 x_1 x_2 x_3 3 (bvadd (_ bv63 6) ?x7621)) (stack_t x_0 x_1 x_2 x_3 2 (bvadd (_ bv60 6) ?x8890)))))
 (let (($x11985 (exc_halt_t 1)))
 (let (($x9060 (or (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_t 1)))) (_ bv0 1))) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_t 1)))) $x11985)))
 (let (($x7505 (exc_halt_t 2)))
 (let (($x3926 (= $x7505 $x9060)))
 (let (($x10211 (forall ((w (_ BitVec 256)) )(let ((?x548 (storage_t x_0 x_1 x_2 x_3 1 w)))
 (let ((?x4361 (storage_t x_0 x_1 x_2 x_3 2 w)))
 (= ?x4361 ?x548))))
 ))
 (let (($x2779 (forall ((n (_ BitVec 6)) )(let ((?x1280 (stack_t x_0 x_1 x_2 x_3 1 n)))
 (let ((?x10979 (stack_t x_0 x_1 x_2 x_3 2 n)))
 (let ((?x4554 (sc_t 1)))
 (let ((?x2315 (bvadd (_ bv62 6) ?x4554)))
 (let (($x8517 (bvsle ?x2315 n)))
 (or $x8517 (= ?x10979 ?x1280))))))))
 ))
 (let (($x422 (= ?x8890 (bvadd (_ bv1 6) (sc_t 1)))))
 (let ((?x10707 (used_gas_t x_0 x_1 x_2 x_3 2)))
 (let ((?x4554 (sc_t 1)))
 (let ((?x6802 (bvadd (_ bv63 6) ?x4554)))
 (let ((?x11471 (stack_t x_0 x_1 x_2 x_3 1 ?x6802)))
 (let ((?x2315 (bvadd (_ bv62 6) ?x4554)))
 (let ((?x3000 (stack_t x_0 x_1 x_2 x_3 1 ?x2315)))
 (let (($x921 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x63))) (_ bv0 1)))))
 (let (($x2703 (forall ((w (_ BitVec 256)) )(let ((?x2103 (storage_t x_0 x_1 x_2 x_3 0 w)))
 (let ((?x548 (storage_t x_0 x_1 x_2 x_3 1 w)))
 (= ?x548 ?x2103))))
 ))
 (let (($x8572 (forall ((n (_ BitVec 6)) )(let ((?x3962 (stack_t x_0 x_1 x_2 x_3 0 n)))
 (let ((?x1280 (stack_t x_0 x_1 x_2 x_3 1 n)))
 (or (bvsle (bvadd (_ bv60 6) (sc_t 0)) n) (= ?x1280 ?x3962)))))
 ))
 (let (($x2096 (= ?x4554 (bvadd (_ bv1 6) ?x63))))
 (let (($x4296 (= (stack_t x_0 x_1 x_2 x_3 1 (bvadd (_ bv63 6) ?x63)) (stack_t x_0 x_1 x_2 x_3 0 (bvadd (_ bv63 6) ?x63)))))
 (let (($x6015 (= (stack_t x_0 x_1 x_2 x_3 1 (bvadd (_ bv62 6) ?x63)) (stack_t x_0 x_1 x_2 x_3 0 (bvadd (_ bv62 6) ?x63)))))
 (let (($x1729 (= (stack_t x_0 x_1 x_2 x_3 1 (bvadd (_ bv61 6) ?x63)) (stack_t x_0 x_1 x_2 x_3 0 (bvadd (_ bv61 6) ?x63)))))
 (let ((?x10848 (bvadd (_ bv60 6) ?x63)))
 (let ((?x4482 (stack_t x_0 x_1 x_2 x_3 0 ?x10848)))
 (let (($x9525 (= $x1862 (or (exc_halt_s 4) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 4))))))))
 (let (($x4902 (forall ((w (_ BitVec 256)) )(let ((?x9821 (storage_s x_0 x_1 x_2 x_3 4 w)))
 (let ((?x3900 (storage_s x_0 x_1 x_2 x_3 5 w)))
 (= ?x3900 ?x9821))))
 ))
 (let (($x10499 (forall ((n (_ BitVec 6)) )(let ((?x9550 (stack_s x_0 x_1 x_2 x_3 4 n)))
 (let ((?x9017 (stack_s x_0 x_1 x_2 x_3 5 n)))
 (let ((?x9433 (sc_s 4)))
 (let ((?x11364 (bvadd (_ bv62 6) ?x9433)))
 (let (($x6445 (bvsle ?x11364 n)))
 (or $x6445 (= ?x9017 ?x9550))))))))
 ))
 (let (($x2218 (= (used_gas_s x_0 x_1 x_2 x_3 5) (+ 3 (used_gas_s x_0 x_1 x_2 x_3 4)))))
 (let ((?x9433 (sc_s 4)))
 (let ((?x3395 (bvadd (_ bv63 6) ?x9433)))
 (let ((?x11385 (stack_s x_0 x_1 x_2 x_3 4 ?x3395)))
 (let (($x8676 (= (stack_s x_0 x_1 x_2 x_3 5 (bvadd (_ bv63 6) ?x4319)) (stack_s x_0 x_1 x_2 x_3 4 (bvadd (_ bv62 6) ?x9433)))))
 (let (($x8103 (exc_halt_s 3)))
 (let (($x1963 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 3)))) (_ bv0 1)))))
 (let (($x9175 (exc_halt_s 4)))
 (let (($x400 (forall ((w (_ BitVec 256)) )(let ((?x10424 (storage_s x_0 x_1 x_2 x_3 3 w)))
 (let ((?x9821 (storage_s x_0 x_1 x_2 x_3 4 w)))
 (= ?x9821 ?x10424))))
 ))
 (let (($x4680 (forall ((n (_ BitVec 6)) )(let ((?x3575 (stack_s x_0 x_1 x_2 x_3 3 n)))
 (let ((?x9550 (stack_s x_0 x_1 x_2 x_3 4 n)))
 (or (bvsle (bvadd (_ bv59 6) (sc_s 3)) n) (= ?x9550 ?x3575)))))
 ))
 (let (($x2843 (= ?x9433 (bvadd (_ bv1 6) (sc_s 3)))))
 (let ((?x7118 (used_gas_s x_0 x_1 x_2 x_3 4)))
 (let ((?x3851 (sc_s 3)))
 (let ((?x1738 (bvadd (_ bv63 6) ?x3851)))
 (let ((?x7409 (stack_s x_0 x_1 x_2 x_3 3 ?x1738)))
 (let ((?x8089 (bvadd (_ bv62 6) ?x3851)))
 (let ((?x11659 (stack_s x_0 x_1 x_2 x_3 3 ?x8089)))
 (let (($x5049 (= (stack_s x_0 x_1 x_2 x_3 4 (bvadd (_ bv61 6) ?x3851)) (stack_s x_0 x_1 x_2 x_3 3 (bvadd (_ bv61 6) ?x3851)))))
 (let (($x5994 (= (stack_s x_0 x_1 x_2 x_3 4 (bvadd (_ bv60 6) ?x3851)) (stack_s x_0 x_1 x_2 x_3 3 (bvadd (_ bv60 6) ?x3851)))))
 (let ((?x10710 (bvadd (_ bv59 6) ?x3851)))
 (let ((?x7452 (stack_s x_0 x_1 x_2 x_3 3 ?x10710)))
 (let (($x4380 (= $x8103 (or (exc_halt_s 2) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 2))))))))
 (let (($x5352 (forall ((w (_ BitVec 256)) )(let ((?x1967 (storage_s x_0 x_1 x_2 x_3 2 w)))
 (let ((?x10424 (storage_s x_0 x_1 x_2 x_3 3 w)))
 (= ?x10424 ?x1967))))
 ))
 (let (($x5195 (forall ((n (_ BitVec 6)) )(let ((?x2272 (sc_s 2)))
 (let ((?x7947 (bvadd (_ bv62 6) ?x2272)))
 (let (($x7062 (bvsle ?x7947 n)))
 (let ((?x5322 (stack_s x_0 x_1 x_2 x_3 2 n)))
 (let ((?x3575 (stack_s x_0 x_1 x_2 x_3 3 n)))
 (or (= ?x3575 ?x5322) $x7062)))))))
 ))
 (let ((?x2272 (sc_s 2)))
 (let (($x5381 (= ?x3851 ?x2272)))
 (let ((?x10754 (used_gas_s x_0 x_1 x_2 x_3 3)))
 (let (($x8780 (exc_halt_s 1)))
 (let (($x3429 (or (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 1)))) (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 1)))) (_ bv0 1))) $x8780)))
 (let (($x10052 (exc_halt_s 2)))
 (let (($x7373 (forall ((w (_ BitVec 256)) )(let ((?x4803 (storage_s x_0 x_1 x_2 x_3 1 w)))
 (let ((?x1967 (storage_s x_0 x_1 x_2 x_3 2 w)))
 (= ?x1967 ?x4803))))
 ))
 (let (($x3048 (forall ((n (_ BitVec 6)) )(let ((?x6880 (stack_s x_0 x_1 x_2 x_3 1 n)))
 (let ((?x5322 (stack_s x_0 x_1 x_2 x_3 2 n)))
 (or (= ?x5322 ?x6880) (bvsle (bvadd (_ bv62 6) (sc_s 1)) n)))))
 ))
 (let ((?x2941 (used_gas_s x_0 x_1 x_2 x_3 2)))
 (let ((?x154 (sc_s 1)))
 (let ((?x1791 (bvadd (_ bv63 6) ?x154)))
 (let ((?x11275 (stack_s x_0 x_1 x_2 x_3 1 ?x1791)))
 (let ((?x8244 (bvadd (_ bv62 6) ?x154)))
 (let ((?x1790 (stack_s x_0 x_1 x_2 x_3 1 ?x8244)))
 (let (($x7140 (= $x8780 (or $x57 (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) ?x72)))))))
 (let (($x1118 (forall ((w (_ BitVec 256)) )(let ((?x6743 (storage_s x_0 x_1 x_2 x_3 0 w)))
 (let ((?x4803 (storage_s x_0 x_1 x_2 x_3 1 w)))
 (= ?x4803 ?x6743))))
 ))
 (let (($x7278 (forall ((n (_ BitVec 6)) )(let ((?x7340 (stack_s x_0 x_1 x_2 x_3 0 n)))
 (let ((?x6880 (stack_s x_0 x_1 x_2 x_3 1 n)))
 (let ((?x72 (sc_s 0)))
 (let ((?x10468 (bvadd (_ bv62 6) ?x72)))
 (let (($x2129 (bvsle ?x10468 n)))
 (or $x2129 (= ?x6880 ?x7340))))))))
 ))
 (let (($x9780 (= ?x154 ?x72)))
 (let (($x2229 (forall ((w (_ BitVec 256)) )(let ((?x6743 (storage_s x_0 x_1 x_2 x_3 0 w)))
 (= ?x6743 (_ bv0 256))))
 ))
 (let (($x1717 (= ?x4303 0)))
 (let (($x3971 (not $x57)))
 (let (($x3572 (= (stack_s x_0 x_1 x_2 x_3 0 (_ bv3 6)) x_3)))
 (let (($x11059 (= (stack_s x_0 x_1 x_2 x_3 0 (_ bv2 6)) x_2)))
 (let (($x9341 (= (stack_s x_0 x_1 x_2 x_3 0 (_ bv1 6)) x_1)))
 (let (($x10662 (= (stack_s x_0 x_1 x_2 x_3 0 (_ bv0 6)) x_0)))
 (let (($x11523 (= ?x72 (_ bv4 6))))
 (and $x11523 $x10662 $x9341 $x11059 $x3572 $x3971 $x1717 $x2229 (= ?x11275 (stack_s x_0 x_1 x_2 x_3 0 (bvadd (_ bv62 6) ?x72))) (= ?x1790 (stack_s x_0 x_1 x_2 x_3 0 (bvadd (_ bv63 6) ?x72))) (= (used_gas_s x_0 x_1 x_2 x_3 1) (+ 3 ?x4303)) $x9780 $x7278 $x1118 $x7140 (= (stack_s x_0 x_1 x_2 x_3 2 (bvadd (_ bv63 6) ?x2272)) ?x1790) (= (stack_s x_0 x_1 x_2 x_3 2 ?x8244) ?x1790) (= (stack_s x_0 x_1 x_2 x_3 2 ?x1791) ?x11275) (= ?x2941 (+ 3 (used_gas_s x_0 x_1 x_2 x_3 1))) (= ?x2272 (bvadd (_ bv1 6) ?x154)) $x3048 $x7373 (= $x10052 $x3429) (= ?x7409 (stack_s x_0 x_1 x_2 x_3 2 (bvadd (_ bv62 6) ?x2272))) (= ?x11659 (stack_s x_0 x_1 x_2 x_3 2 (bvadd (_ bv63 6) ?x2272))) (= ?x10754 (+ 3 ?x2941)) $x5381 $x5195 $x5352 $x4380 (= ?x11385 ?x7452) (= (stack_s x_0 x_1 x_2 x_3 4 ?x10710) ?x7452) $x5994 $x5049 (= (stack_s x_0 x_1 x_2 x_3 4 ?x8089) ?x11659) (= (stack_s x_0 x_1 x_2 x_3 4 ?x1738) ?x7409) (= ?x7118 (+ 3 ?x10754)) $x2843 $x4680 $x400 (= $x9175 (or $x1963 (not (bvsle (_ bv0 6) ?x10710)) $x8103)) $x8676 (= (stack_s x_0 x_1 x_2 x_3 5 (bvadd (_ bv62 6) ?x4319)) ?x11385) $x2218 (= ?x4319 ?x9433) $x10499 $x4902 $x9525 (= ?x11471 ?x4482) (= (stack_t x_0 x_1 x_2 x_3 1 ?x10848) ?x4482) $x1729 $x6015 $x4296 (= (used_gas_t x_0 x_1 x_2 x_3 1) (+ 3 ?x11593)) $x2096 $x8572 $x2703 (= $x11985 (or $x56 $x921 (not (bvsle (_ bv0 6) ?x10848)))) (= ?x11948 ?x3000) (= (stack_t x_0 x_1 x_2 x_3 2 ?x2315) ?x3000) (= (stack_t x_0 x_1 x_2 x_3 2 ?x6802) ?x11471) (= ?x10707 (+ 3 (used_gas_t x_0 x_1 x_2 x_3 1))) $x422 $x2779 $x10211 $x3926 $x1968 (= (stack_t x_0 x_1 x_2 x_3 3 (bvadd (_ bv60 6) ?x7621)) ?x11948) $x10462 $x8308 $x7262 $x5385 $x1338 $x2986 $x5589 $x73 $x11377 $x58 $x1470 $x3707 (not (and $x3981 $x529 $x3022 $x3316)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
 )
(check-sat)
(get-proof)
