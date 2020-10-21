; SWAP3 SWAP3 DUP4 DUP5 => DUP4 DUP1
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
 (exists ((x_0 (_ BitVec 256)) (x_1 (_ BitVec 256)) (x_2 (_ BitVec 256)) (x_3 (_ BitVec 256)) )(let (($x9757 (forall ((w (_ BitVec 256)) )(let ((?x1645 (storage_t x_0 x_1 x_2 x_3 2 w)))
 (let ((?x11762 (storage_s x_0 x_1 x_2 x_3 4 w)))
 (= ?x11762 ?x1645))))
 ))
 (let (($x903 (exc_halt_t 2)))
 (let (($x64 (exc_halt_s 4)))
 (let (($x11638 (= $x64 $x903)))
 (let (($x3003 (forall ((n (_ BitVec 6)) )(let ((?x4056 (sc_t 2)))
 (let (($x10759 (bvsle ?x4056 n)))
 (let ((?x6846 (stack_t x_0 x_1 x_2 x_3 2 n)))
 (let ((?x1467 (stack_s x_0 x_1 x_2 x_3 4 n)))
 (let (($x5535 (= ?x1467 ?x6846)))
 (or $x5535 $x10759)))))))
 ))
 (let ((?x4056 (sc_t 2)))
 (let ((?x4305 (sc_s 4)))
 (let (($x10778 (= ?x4305 ?x4056)))
 (let ((?x8609 (used_gas_t x_0 x_1 x_2 x_3 0)))
 (let ((?x8264 (used_gas_s x_0 x_1 x_2 x_3 0)))
 (let (($x7157 (= ?x8264 ?x8609)))
 (let (($x9761 (forall ((w (_ BitVec 256)) )(let ((?x6543 (storage_t x_0 x_1 x_2 x_3 0 w)))
 (let ((?x2843 (storage_s x_0 x_1 x_2 x_3 0 w)))
 (= ?x2843 ?x6543))))
 ))
 (let (($x56 (exc_halt_t 0)))
 (let (($x57 (exc_halt_s 0)))
 (let (($x58 (= $x57 $x56)))
 (let (($x8915 (forall ((n (_ BitVec 6)) )(let ((?x8206 (stack_t x_0 x_1 x_2 x_3 0 n)))
 (let ((?x5908 (stack_s x_0 x_1 x_2 x_3 0 n)))
 (let (($x5414 (= ?x5908 ?x8206)))
 (let ((?x63 (sc_t 0)))
 (let (($x515 (bvsle ?x63 n)))
 (or $x515 $x5414)))))))
 ))
 (let ((?x63 (sc_t 0)))
 (let ((?x72 (sc_s 0)))
 (let (($x73 (= ?x72 ?x63)))
 (let (($x5051 (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_t 1))))))
 (let (($x1920 (exc_halt_t 1)))
 (let (($x487 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_t 1)))) (_ bv0 1)))))
 (let (($x6401 (= $x903 (or $x487 $x1920 $x5051))))
 (let (($x2605 (forall ((w (_ BitVec 256)) )(let ((?x1735 (storage_t x_0 x_1 x_2 x_3 1 w)))
 (let ((?x1645 (storage_t x_0 x_1 x_2 x_3 2 w)))
 (= ?x1645 ?x1735))))
 ))
 (let (($x7878 (forall ((n (_ BitVec 6)) )(let ((?x997 (stack_t x_0 x_1 x_2 x_3 1 n)))
 (let ((?x6846 (stack_t x_0 x_1 x_2 x_3 2 n)))
 (let ((?x4023 (sc_t 1)))
 (let ((?x10675 (bvadd (_ bv63 6) ?x4023)))
 (let (($x2171 (bvsle ?x10675 n)))
 (or $x2171 (= ?x6846 ?x997))))))))
 ))
 (let (($x7940 (= ?x4056 (bvadd (_ bv1 6) (sc_t 1)))))
 (let (($x9482 (= (used_gas_t x_0 x_1 x_2 x_3 2) (+ 3 (used_gas_t x_0 x_1 x_2 x_3 1)))))
 (let ((?x4023 (sc_t 1)))
 (let ((?x10675 (bvadd (_ bv63 6) ?x4023)))
 (let ((?x502 (stack_t x_0 x_1 x_2 x_3 1 ?x10675)))
 (let (($x4496 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x63))) (_ bv0 1)))))
 (let (($x5930 (forall ((w (_ BitVec 256)) )(let ((?x6543 (storage_t x_0 x_1 x_2 x_3 0 w)))
 (let ((?x1735 (storage_t x_0 x_1 x_2 x_3 1 w)))
 (= ?x1735 ?x6543))))
 ))
 (let (($x7720 (forall ((n (_ BitVec 6)) )(let ((?x8206 (stack_t x_0 x_1 x_2 x_3 0 n)))
 (let ((?x997 (stack_t x_0 x_1 x_2 x_3 1 n)))
 (or (bvsle (bvadd (_ bv60 6) (sc_t 0)) n) (= ?x997 ?x8206)))))
 ))
 (let (($x6031 (= ?x4023 (bvadd (_ bv1 6) ?x63))))
 (let (($x1323 (= (stack_t x_0 x_1 x_2 x_3 1 (bvadd (_ bv63 6) ?x63)) (stack_t x_0 x_1 x_2 x_3 0 (bvadd (_ bv63 6) ?x63)))))
 (let (($x5619 (= (stack_t x_0 x_1 x_2 x_3 1 (bvadd (_ bv62 6) ?x63)) (stack_t x_0 x_1 x_2 x_3 0 (bvadd (_ bv62 6) ?x63)))))
 (let (($x10605 (= (stack_t x_0 x_1 x_2 x_3 1 (bvadd (_ bv61 6) ?x63)) (stack_t x_0 x_1 x_2 x_3 0 (bvadd (_ bv61 6) ?x63)))))
 (let ((?x2107 (bvadd (_ bv60 6) ?x63)))
 (let ((?x4624 (stack_t x_0 x_1 x_2 x_3 0 ?x2107)))
 (let (($x7122 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 3)))) (_ bv0 1)))))
 (let (($x292 (exc_halt_s 3)))
 (let (($x11718 (forall ((w (_ BitVec 256)) )(let ((?x2896 (storage_s x_0 x_1 x_2 x_3 3 w)))
 (let ((?x11762 (storage_s x_0 x_1 x_2 x_3 4 w)))
 (= ?x11762 ?x2896))))
 ))
 (let (($x2266 (forall ((n (_ BitVec 6)) )(let ((?x5767 (stack_s x_0 x_1 x_2 x_3 3 n)))
 (let ((?x1467 (stack_s x_0 x_1 x_2 x_3 4 n)))
 (or (= ?x1467 ?x5767) (bvsle (bvadd (_ bv59 6) (sc_s 3)) n)))))
 ))
 (let (($x3882 (= ?x4305 (bvadd (_ bv1 6) (sc_s 3)))))
 (let (($x1029 (= (used_gas_s x_0 x_1 x_2 x_3 4) (+ 3 (used_gas_s x_0 x_1 x_2 x_3 3)))))
 (let ((?x275 (sc_s 3)))
 (let ((?x7729 (bvadd (_ bv63 6) ?x275)))
 (let ((?x3892 (stack_s x_0 x_1 x_2 x_3 3 ?x7729)))
 (let (($x881 (= (stack_s x_0 x_1 x_2 x_3 4 (bvadd (_ bv62 6) ?x275)) (stack_s x_0 x_1 x_2 x_3 3 (bvadd (_ bv62 6) ?x275)))))
 (let (($x4324 (= (stack_s x_0 x_1 x_2 x_3 4 (bvadd (_ bv61 6) ?x275)) (stack_s x_0 x_1 x_2 x_3 3 (bvadd (_ bv61 6) ?x275)))))
 (let (($x9718 (= (stack_s x_0 x_1 x_2 x_3 4 (bvadd (_ bv60 6) ?x275)) (stack_s x_0 x_1 x_2 x_3 3 (bvadd (_ bv60 6) ?x275)))))
 (let ((?x8458 (bvadd (_ bv59 6) ?x275)))
 (let ((?x8252 (stack_s x_0 x_1 x_2 x_3 3 ?x8458)))
 (let (($x7239 (not (bvsle (_ bv0 6) (bvadd (_ bv60 6) (sc_s 2))))))
 (let (($x1186 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 2)))) (_ bv0 1)))))
 (let (($x247 (exc_halt_s 2)))
 (let (($x11337 (forall ((w (_ BitVec 256)) )(let ((?x8384 (storage_s x_0 x_1 x_2 x_3 2 w)))
 (let ((?x2896 (storage_s x_0 x_1 x_2 x_3 3 w)))
 (= ?x2896 ?x8384))))
 ))
 (let (($x5845 (forall ((n (_ BitVec 6)) )(let ((?x341 (stack_s x_0 x_1 x_2 x_3 2 n)))
 (let ((?x5767 (stack_s x_0 x_1 x_2 x_3 3 n)))
 (let ((?x218 (sc_s 2)))
 (let ((?x8351 (bvadd (_ bv60 6) ?x218)))
 (let (($x367 (bvsle ?x8351 n)))
 (or $x367 (= ?x5767 ?x341))))))))
 ))
 (let (($x10093 (= ?x275 (bvadd (_ bv1 6) (sc_s 2)))))
 (let ((?x4946 (used_gas_s x_0 x_1 x_2 x_3 3)))
 (let ((?x218 (sc_s 2)))
 (let ((?x5628 (bvadd (_ bv63 6) ?x218)))
 (let ((?x3089 (stack_s x_0 x_1 x_2 x_3 2 ?x5628)))
 (let ((?x2547 (bvadd (_ bv62 6) ?x218)))
 (let ((?x695 (stack_s x_0 x_1 x_2 x_3 2 ?x2547)))
 (let ((?x431 (bvadd (_ bv61 6) ?x218)))
 (let ((?x106 (stack_s x_0 x_1 x_2 x_3 2 ?x431)))
 (let ((?x8351 (bvadd (_ bv60 6) ?x218)))
 (let ((?x11257 (stack_s x_0 x_1 x_2 x_3 2 ?x8351)))
 (let (($x10902 (= $x247 (or (exc_halt_s 1) (not (bvsle (_ bv0 6) (bvadd (_ bv60 6) (sc_s 1))))))))
 (let (($x1508 (forall ((w (_ BitVec 256)) )(let ((?x8608 (storage_s x_0 x_1 x_2 x_3 1 w)))
 (let ((?x8384 (storage_s x_0 x_1 x_2 x_3 2 w)))
 (= ?x8384 ?x8608))))
 ))
 (let (($x1407 (forall ((n (_ BitVec 6)) )(let ((?x639 (stack_s x_0 x_1 x_2 x_3 1 n)))
 (let ((?x341 (stack_s x_0 x_1 x_2 x_3 2 n)))
 (or (bvsle (bvadd (_ bv60 6) (sc_s 1)) n) (= ?x341 ?x639)))))
 ))
 (let ((?x154 (sc_s 1)))
 (let (($x1121 (= ?x218 ?x154)))
 (let ((?x4489 (used_gas_s x_0 x_1 x_2 x_3 2)))
 (let (($x189 (exc_halt_s 1)))
 (let (($x6271 (= $x189 (or $x57 (not (bvsle (_ bv0 6) (bvadd (_ bv60 6) ?x72)))))))
 (let (($x4759 (forall ((w (_ BitVec 256)) )(let ((?x2843 (storage_s x_0 x_1 x_2 x_3 0 w)))
 (let ((?x8608 (storage_s x_0 x_1 x_2 x_3 1 w)))
 (= ?x8608 ?x2843))))
 ))
 (let (($x9911 (forall ((n (_ BitVec 6)) )(let ((?x5908 (stack_s x_0 x_1 x_2 x_3 0 n)))
 (let ((?x639 (stack_s x_0 x_1 x_2 x_3 1 n)))
 (let ((?x72 (sc_s 0)))
 (let ((?x3390 (bvadd (_ bv60 6) ?x72)))
 (let (($x3251 (bvsle ?x3390 n)))
 (or $x3251 (= ?x639 ?x5908))))))))
 ))
 (let (($x6532 (= ?x154 ?x72)))
 (let ((?x1354 (bvadd (_ bv62 6) ?x154)))
 (let ((?x8793 (stack_s x_0 x_1 x_2 x_3 1 ?x1354)))
 (let ((?x5002 (bvadd (_ bv61 6) ?x154)))
 (let ((?x1973 (stack_s x_0 x_1 x_2 x_3 1 ?x5002)))
 (let ((?x2939 (bvadd (_ bv60 6) ?x154)))
 (let ((?x4583 (stack_s x_0 x_1 x_2 x_3 1 ?x2939)))
 (let ((?x4815 (bvadd (_ bv63 6) ?x154)))
 (let ((?x4343 (stack_s x_0 x_1 x_2 x_3 1 ?x4815)))
 (let (($x5251 (forall ((w (_ BitVec 256)) )(let ((?x2843 (storage_s x_0 x_1 x_2 x_3 0 w)))
 (= ?x2843 (_ bv0 256))))
 ))
 (let (($x78 (= ?x8264 0)))
 (let (($x2460 (not $x57)))
 (let (($x2287 (= (stack_s x_0 x_1 x_2 x_3 0 (_ bv3 6)) x_3)))
 (let (($x6617 (= (stack_s x_0 x_1 x_2 x_3 0 (_ bv2 6)) x_2)))
 (let (($x7191 (= (stack_s x_0 x_1 x_2 x_3 0 (_ bv1 6)) x_1)))
 (let (($x6214 (= (stack_s x_0 x_1 x_2 x_3 0 (_ bv0 6)) x_0)))
 (let (($x7736 (= ?x72 (_ bv4 6))))
 (and $x7736 $x6214 $x7191 $x6617 $x2287 $x2460 $x78 $x5251 (= ?x4343 (stack_s x_0 x_1 x_2 x_3 0 (bvadd (_ bv60 6) ?x72))) (= ?x4583 (stack_s x_0 x_1 x_2 x_3 0 (bvadd (_ bv63 6) ?x72))) (= ?x1973 (stack_s x_0 x_1 x_2 x_3 0 (bvadd (_ bv61 6) ?x72))) (= ?x8793 (stack_s x_0 x_1 x_2 x_3 0 (bvadd (_ bv62 6) ?x72))) (= (used_gas_s x_0 x_1 x_2 x_3 1) (+ 3 ?x8264)) $x6532 $x9911 $x4759 $x6271 (= ?x3089 ?x4583) (= ?x11257 ?x4343) (= ?x106 ?x1973) (= ?x695 ?x8793) (= ?x4489 (+ 3 (used_gas_s x_0 x_1 x_2 x_3 1))) $x1121 $x1407 $x1508 $x10902 (= ?x3892 ?x11257) (= (stack_s x_0 x_1 x_2 x_3 3 ?x8351) ?x11257) (= (stack_s x_0 x_1 x_2 x_3 3 ?x431) ?x106) (= (stack_s x_0 x_1 x_2 x_3 3 ?x2547) ?x695) (= (stack_s x_0 x_1 x_2 x_3 3 ?x5628) ?x3089) (= ?x4946 (+ 3 ?x4489)) $x10093 $x5845 $x11337 (= $x292 (or $x247 $x1186 $x7239)) (= (stack_s x_0 x_1 x_2 x_3 4 (bvadd (_ bv63 6) ?x4305)) ?x8252) (= (stack_s x_0 x_1 x_2 x_3 4 ?x8458) ?x8252) $x9718 $x4324 $x881 (= (stack_s x_0 x_1 x_2 x_3 4 ?x7729) ?x3892) $x1029 $x3882 $x2266 $x11718 (= $x64 (or $x292 $x7122 (not (bvsle (_ bv0 6) ?x8458)))) (= ?x502 ?x4624) (= (stack_t x_0 x_1 x_2 x_3 1 ?x2107) ?x4624) $x10605 $x5619 $x1323 (= (used_gas_t x_0 x_1 x_2 x_3 1) (+ 3 ?x8609)) $x6031 $x7720 $x5930 (= $x1920 (or $x56 (not (bvsle (_ bv0 6) ?x2107)) $x4496)) (= (stack_t x_0 x_1 x_2 x_3 2 (bvadd (_ bv63 6) ?x4056)) ?x502) (= (stack_t x_0 x_1 x_2 x_3 2 ?x10675) ?x502) $x9482 $x7940 $x7878 $x2605 $x6401 $x73 $x8915 $x58 $x9761 $x7157 (not (and $x10778 $x3003 $x11638 $x9757))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
 )
(check-sat)
(get-proof)
