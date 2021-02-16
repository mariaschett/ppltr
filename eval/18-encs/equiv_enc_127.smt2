; PUSH cw_2 DUP1 PUSH cw_2 DUP1 POP => PUSH cw_2 DUP1 DUP1
(set-option :produce-proofs true)
; 
(set-info :status unknown)
(declare-fun storage_t ((_ BitVec 256) Int (_ BitVec 256)) (_ BitVec 256))
(declare-fun w_2 () (_ BitVec 256))
(declare-fun storage_s ((_ BitVec 256) Int (_ BitVec 256)) (_ BitVec 256))
(declare-fun exc_halt_t (Int) Bool)
(declare-fun exc_halt_s (Int) Bool)
(declare-fun sc_t (Int) (_ BitVec 6))
(declare-fun stack_t ((_ BitVec 256) Int (_ BitVec 6)) (_ BitVec 256))
(declare-fun stack_s ((_ BitVec 256) Int (_ BitVec 6)) (_ BitVec 256))
(declare-fun sc_s (Int) (_ BitVec 6))
(declare-fun used_gas_t ((_ BitVec 256) Int) Int)
(declare-fun used_gas_s ((_ BitVec 256) Int) Int)
(assert
 (let (($x8693 (forall ((w (_ BitVec 256)) )(let ((?x10351 (storage_t w_2 3 w)))
 (let ((?x9395 (storage_s w_2 5 w)))
 (= ?x9395 ?x10351))))
 ))
 (let (($x10336 (exc_halt_t 3)))
 (let (($x3979 (exc_halt_s 5)))
 (let (($x8847 (= $x3979 $x10336)))
 (let (($x8696 (forall ((n (_ BitVec 6)) )(let ((?x2012 (sc_t 3)))
 (let (($x8881 (bvsle ?x2012 n)))
 (let ((?x8985 (stack_t w_2 3 n)))
 (let ((?x904 (stack_s w_2 5 n)))
 (let (($x2016 (= ?x904 ?x8985)))
 (or $x2016 $x8881)))))))
 ))
 (let ((?x2012 (sc_t 3)))
 (let ((?x805 (sc_s 5)))
 (let (($x2955 (= ?x805 ?x2012)))
 (let ((?x6823 (used_gas_t w_2 0)))
 (let ((?x7453 (used_gas_s w_2 0)))
 (let (($x8987 (= ?x7453 ?x6823)))
 (let (($x1760 (forall ((w (_ BitVec 256)) )(let ((?x986 (storage_t w_2 0 w)))
 (let ((?x9151 (storage_s w_2 0 w)))
 (= ?x9151 ?x986))))
 ))
 (let (($x56 (exc_halt_t 0)))
 (let (($x57 (exc_halt_s 0)))
 (let (($x58 (= $x57 $x56)))
 (let (($x4779 (forall ((n (_ BitVec 6)) )(let ((?x8535 (stack_t w_2 0 n)))
 (let ((?x8869 (stack_s w_2 0 n)))
 (let (($x9606 (= ?x8869 ?x8535)))
 (let ((?x63 (sc_t 0)))
 (let (($x2733 (bvsle ?x63 n)))
 (or $x2733 $x9606)))))))
 ))
 (let ((?x63 (sc_t 0)))
 (let ((?x72 (sc_s 0)))
 (let (($x73 (= ?x72 ?x63)))
 (let (($x9944 (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_t 2))))))
 (let (($x8512 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_t 2)))) (_ bv0 1)))))
 (let (($x903 (exc_halt_t 2)))
 (let (($x9722 (forall ((w (_ BitVec 256)) )(let ((?x9643 (storage_t w_2 2 w)))
 (let ((?x10351 (storage_t w_2 3 w)))
 (= ?x10351 ?x9643))))
 ))
 (let (($x7491 (forall ((n (_ BitVec 6)) )(let ((?x4056 (sc_t 2)))
 (let ((?x6881 (bvadd (_ bv63 6) ?x4056)))
 (let (($x4901 (bvsle ?x6881 n)))
 (or $x4901 (= (stack_t w_2 3 n) (stack_t w_2 2 n)))))))
 ))
 (let (($x5064 (= ?x2012 (bvadd (_ bv1 6) (sc_t 2)))))
 (let ((?x4056 (sc_t 2)))
 (let ((?x6881 (bvadd (_ bv63 6) ?x4056)))
 (let ((?x2203 (stack_t w_2 2 ?x6881)))
 (let (($x10874 (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_t 1))))))
 (let (($x10002 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_t 1)))) (_ bv0 1)))))
 (let (($x1920 (exc_halt_t 1)))
 (let (($x3615 (forall ((w (_ BitVec 256)) )(let ((?x6209 (storage_t w_2 1 w)))
 (let ((?x9643 (storage_t w_2 2 w)))
 (= ?x9643 ?x6209))))
 ))
 (let (($x3213 (forall ((n (_ BitVec 6)) )(let ((?x4023 (sc_t 1)))
 (let ((?x6958 (bvadd (_ bv63 6) ?x4023)))
 (let (($x2645 (bvsle ?x6958 n)))
 (or (= (stack_t w_2 2 n) (stack_t w_2 1 n)) $x2645)))))
 ))
 (let (($x423 (= ?x4056 (bvadd (_ bv1 6) (sc_t 1)))))
 (let ((?x4023 (sc_t 1)))
 (let ((?x6958 (bvadd (_ bv63 6) ?x4023)))
 (let ((?x2928 (stack_t w_2 1 ?x6958)))
 (let (($x1160 (= $x1920 (or $x56 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x63))) (_ bv0 1)))))))
 (let (($x4694 (forall ((w (_ BitVec 256)) )(let ((?x986 (storage_t w_2 0 w)))
 (let ((?x6209 (storage_t w_2 1 w)))
 (= ?x6209 ?x986))))
 ))
 (let (($x9639 (forall ((n (_ BitVec 6)) )(let ((?x63 (sc_t 0)))
 (let (($x2733 (bvsle ?x63 n)))
 (or $x2733 (= (stack_t w_2 1 n) (stack_t w_2 0 n))))))
 ))
 (let (($x7084 (= ?x4023 (bvadd (_ bv1 6) ?x63))))
 (let (($x7980 (= $x3979 (or (exc_halt_s 4) (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_s 4))))))))
 (let (($x2914 (forall ((w (_ BitVec 256)) )(let ((?x8756 (storage_s w_2 4 w)))
 (let ((?x9395 (storage_s w_2 5 w)))
 (= ?x9395 ?x8756))))
 ))
 (let (($x2716 (forall ((n (_ BitVec 6)) )(let ((?x4305 (sc_s 4)))
 (let ((?x10083 (bvadd (_ bv63 6) ?x4305)))
 (let (($x6463 (bvsle ?x10083 n)))
 (or $x6463 (= (stack_s w_2 5 n) (stack_s w_2 4 n)))))))
 ))
 (let (($x6601 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 3)))) (_ bv0 1)))))
 (let (($x292 (exc_halt_s 3)))
 (let (($x99 (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_s 3))))))
 (let (($x64 (exc_halt_s 4)))
 (let (($x985 (= $x64 (or $x99 $x292 $x6601))))
 (let (($x2170 (forall ((w (_ BitVec 256)) )(let ((?x5729 (storage_s w_2 3 w)))
 (let ((?x8756 (storage_s w_2 4 w)))
 (= ?x8756 ?x5729))))
 ))
 (let (($x3512 (forall ((n (_ BitVec 6)) )(let ((?x275 (sc_s 3)))
 (let ((?x2861 (bvadd (_ bv63 6) ?x275)))
 (let (($x9361 (bvsle ?x2861 n)))
 (or (= (stack_s w_2 4 n) (stack_s w_2 3 n)) $x9361)))))
 ))
 (let ((?x4305 (sc_s 4)))
 (let (($x2321 (= ?x4305 (bvadd (_ bv1 6) (sc_s 3)))))
 (let ((?x275 (sc_s 3)))
 (let ((?x2861 (bvadd (_ bv63 6) ?x275)))
 (let ((?x6779 (stack_s w_2 3 ?x2861)))
 (let (($x247 (exc_halt_s 2)))
 (let (($x7906 (or $x247 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 2)))) (_ bv0 1))))))
 (let (($x1359 (forall ((w (_ BitVec 256)) )(let ((?x3411 (storage_s w_2 2 w)))
 (let ((?x5729 (storage_s w_2 3 w)))
 (= ?x5729 ?x3411))))
 ))
 (let (($x7531 (forall ((n (_ BitVec 6)) )(or (= (stack_s w_2 3 n) (stack_s w_2 2 n)) (bvsle (sc_s 2) n)))
 ))
 (let (($x6277 (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_s 1))))))
 (let (($x3417 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 1)))) (_ bv0 1)))))
 (let (($x189 (exc_halt_s 1)))
 (let (($x6338 (= $x247 (or $x189 $x3417 $x6277))))
 (let (($x9813 (forall ((w (_ BitVec 256)) )(let ((?x10287 (storage_s w_2 1 w)))
 (let ((?x3411 (storage_s w_2 2 w)))
 (= ?x3411 ?x10287))))
 ))
 (let (($x9300 (forall ((n (_ BitVec 6)) )(let ((?x154 (sc_s 1)))
 (let ((?x5240 (bvadd (_ bv63 6) ?x154)))
 (let (($x2656 (bvsle ?x5240 n)))
 (or (= (stack_s w_2 2 n) (stack_s w_2 1 n)) $x2656)))))
 ))
 (let ((?x218 (sc_s 2)))
 (let (($x3670 (= ?x218 (bvadd (_ bv1 6) (sc_s 1)))))
 (let ((?x154 (sc_s 1)))
 (let ((?x5240 (bvadd (_ bv63 6) ?x154)))
 (let ((?x8973 (stack_s w_2 1 ?x5240)))
 (let (($x4891 (= $x189 (or $x57 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x72))) (_ bv0 1)))))))
 (let (($x9240 (forall ((w (_ BitVec 256)) )(let ((?x9151 (storage_s w_2 0 w)))
 (let ((?x10287 (storage_s w_2 1 w)))
 (= ?x10287 ?x9151))))
 ))
 (let (($x9384 (forall ((n (_ BitVec 6)) )(let ((?x72 (sc_s 0)))
 (let (($x1656 (bvsle ?x72 n)))
 (or $x1656 (= (stack_s w_2 1 n) (stack_s w_2 0 n))))))
 ))
 (let (($x10428 (= ?x154 (bvadd (_ bv1 6) ?x72))))
 (let (($x1218 (forall ((w (_ BitVec 256)) )(let ((?x9151 (storage_s w_2 0 w)))
 (= ?x9151 (_ bv0 256))))
 ))
 (let (($x773 (= ?x7453 0)))
 (let (($x3117 (not $x57)))
 (let (($x136 (= ?x72 (_ bv0 6))))
 (and $x136 $x3117 $x773 $x1218 (= (stack_s w_2 1 ?x72) w_2) (= (used_gas_s w_2 1) (+ 3 ?x7453)) $x10428 $x9384 $x9240 $x4891 (= (stack_s w_2 2 (bvadd (_ bv63 6) ?x218)) ?x8973) (= (stack_s w_2 2 ?x5240) ?x8973) (= (used_gas_s w_2 2) (+ 3 (used_gas_s w_2 1))) $x3670 $x9300 $x9813 $x6338 (= (stack_s w_2 3 ?x218) w_2) (= (used_gas_s w_2 3) (+ 3 (used_gas_s w_2 2))) (= ?x275 (bvadd (_ bv1 6) ?x218)) $x7531 $x1359 (= $x292 $x7906) (= (stack_s w_2 4 (bvadd (_ bv63 6) ?x4305)) ?x6779) (= (stack_s w_2 4 ?x2861) ?x6779) (= (used_gas_s w_2 4) (+ 3 (used_gas_s w_2 3))) $x2321 $x3512 $x2170 $x985 (= (used_gas_s w_2 5) (+ 2 (used_gas_s w_2 4))) (= ?x805 (bvadd (_ bv63 6) ?x4305)) $x2716 $x2914 $x7980 (= (stack_t w_2 1 ?x63) w_2) (= (used_gas_t w_2 1) (+ 3 ?x6823)) $x7084 $x9639 $x4694 $x1160 (= ?x2203 ?x2928) (= (stack_t w_2 2 ?x6958) ?x2928) (= (used_gas_t w_2 2) (+ 3 (used_gas_t w_2 1))) $x423 $x3213 $x3615 (= $x903 (or $x1920 $x10002 $x10874)) (= (stack_t w_2 3 (bvadd (_ bv63 6) ?x2012)) ?x2203) (= (stack_t w_2 3 ?x6881) ?x2203) (= (used_gas_t w_2 3) (+ 3 (used_gas_t w_2 2))) $x5064 $x7491 $x9722 (= $x10336 (or $x903 $x8512 $x9944)) $x73 $x4779 $x58 $x1760 $x8987 (not (and $x2955 $x8696 $x8847 $x8693)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
(check-sat)
(get-proof)