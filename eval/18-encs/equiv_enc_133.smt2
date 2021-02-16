; MLOAD DUP1 MLOAD SWAP2 LT POP => SWAP1 POP MLOAD MLOAD
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
(declare-fun f_MLOAD ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256)) (_ BitVec 256))
(assert
 (exists ((x_0 (_ BitVec 256)) (x_1 (_ BitVec 256)) (x_MLOAD_0 (_ BitVec 256)) (x_MLOAD_1 (_ BitVec 256)) )(let (($x10867 (forall ((w (_ BitVec 256)) )(let ((?x9520 (storage_t x_0 x_1 x_MLOAD_0 x_MLOAD_1 4 w)))
 (let ((?x9268 (storage_s x_0 x_1 x_MLOAD_0 x_MLOAD_1 6 w)))
 (= ?x9268 ?x9520))))
 ))
 (let (($x3723 (exc_halt_t 4)))
 (let (($x772 (exc_halt_s 6)))
 (let (($x4256 (= $x772 $x3723)))
 (let (($x2723 (forall ((n (_ BitVec 6)) )(let ((?x3757 (sc_t 4)))
 (let (($x7992 (bvsle ?x3757 n)))
 (let ((?x1065 (stack_t x_0 x_1 x_MLOAD_0 x_MLOAD_1 4 n)))
 (let ((?x953 (stack_s x_0 x_1 x_MLOAD_0 x_MLOAD_1 6 n)))
 (let (($x1537 (= ?x953 ?x1065)))
 (or $x1537 $x7992)))))))
 ))
 (let ((?x3757 (sc_t 4)))
 (let ((?x926 (sc_s 6)))
 (let (($x7941 (= ?x926 ?x3757)))
 (let ((?x4828 (used_gas_t x_0 x_1 x_MLOAD_0 x_MLOAD_1 0)))
 (let ((?x231 (used_gas_s x_0 x_1 x_MLOAD_0 x_MLOAD_1 0)))
 (let (($x525 (= ?x231 ?x4828)))
 (let (($x1024 (forall ((w (_ BitVec 256)) )(let ((?x1750 (storage_t x_0 x_1 x_MLOAD_0 x_MLOAD_1 0 w)))
 (let ((?x1979 (storage_s x_0 x_1 x_MLOAD_0 x_MLOAD_1 0 w)))
 (= ?x1979 ?x1750))))
 ))
 (let (($x56 (exc_halt_t 0)))
 (let (($x57 (exc_halt_s 0)))
 (let (($x58 (= $x57 $x56)))
 (let (($x1398 (forall ((n (_ BitVec 6)) )(let ((?x63 (sc_t 0)))
 (let (($x4379 (bvsle ?x63 n)))
 (let ((?x994 (stack_t x_0 x_1 x_MLOAD_0 x_MLOAD_1 0 n)))
 (let ((?x1502 (stack_s x_0 x_1 x_MLOAD_0 x_MLOAD_1 0 n)))
 (let (($x2570 (= ?x1502 ?x994)))
 (or $x2570 $x4379)))))))
 ))
 (let ((?x63 (sc_t 0)))
 (let ((?x72 (sc_s 0)))
 (let (($x73 (= ?x72 ?x63)))
 (let (($x9766 (= $x3723 (or (exc_halt_t 3) (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_t 3))))))))
 (let (($x9772 (forall ((w (_ BitVec 256)) )(let ((?x65 (storage_t x_0 x_1 x_MLOAD_0 x_MLOAD_1 3 w)))
 (let ((?x9520 (storage_t x_0 x_1 x_MLOAD_0 x_MLOAD_1 4 w)))
 (= ?x9520 ?x65))))
 ))
 (let (($x6007 (forall ((n (_ BitVec 6)) )(let ((?x4359 (stack_t x_0 x_1 x_MLOAD_0 x_MLOAD_1 3 n)))
 (let ((?x1065 (stack_t x_0 x_1 x_MLOAD_0 x_MLOAD_1 4 n)))
 (or (= ?x1065 ?x4359) (bvsle (bvadd (_ bv63 6) (sc_t 3)) n)))))
 ))
 (let (($x1721 (= (used_gas_t x_0 x_1 x_MLOAD_0 x_MLOAD_1 4) (+ 3 (used_gas_t x_0 x_1 x_MLOAD_0 x_MLOAD_1 3)))))
 (let ((?x2012 (sc_t 3)))
 (let ((?x5914 (bvadd (_ bv63 6) ?x2012)))
 (let ((?x2692 (stack_t x_0 x_1 x_MLOAD_0 x_MLOAD_1 3 ?x5914)))
 (let (($x6949 (= (stack_t x_0 x_1 x_MLOAD_0 x_MLOAD_1 4 (bvadd (_ bv63 6) ?x3757)) (f_MLOAD x_0 x_1 x_MLOAD_0 x_MLOAD_1 ?x2692))))
 (let (($x10336 (exc_halt_t 3)))
 (let (($x5923 (= $x10336 (or (exc_halt_t 2) (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_t 2))))))))
 (let (($x7527 (forall ((w (_ BitVec 256)) )(let ((?x4945 (storage_t x_0 x_1 x_MLOAD_0 x_MLOAD_1 2 w)))
 (let ((?x65 (storage_t x_0 x_1 x_MLOAD_0 x_MLOAD_1 3 w)))
 (= ?x65 ?x4945))))
 ))
 (let (($x6650 (forall ((n (_ BitVec 6)) )(let ((?x4056 (sc_t 2)))
 (let ((?x1464 (bvadd (_ bv63 6) ?x4056)))
 (let (($x5406 (bvsle ?x1464 n)))
 (let ((?x7359 (stack_t x_0 x_1 x_MLOAD_0 x_MLOAD_1 2 n)))
 (let ((?x4359 (stack_t x_0 x_1 x_MLOAD_0 x_MLOAD_1 3 n)))
 (or (= ?x4359 ?x7359) $x5406)))))))
 ))
 (let ((?x4056 (sc_t 2)))
 (let (($x1273 (= ?x2012 ?x4056)))
 (let ((?x1463 (used_gas_t x_0 x_1 x_MLOAD_0 x_MLOAD_1 3)))
 (let ((?x1034 (f_MLOAD x_0 x_1 x_MLOAD_0 x_MLOAD_1 (stack_t x_0 x_1 x_MLOAD_0 x_MLOAD_1 2 (bvadd (_ bv63 6) ?x4056)))))
 (let (($x903 (exc_halt_t 2)))
 (let (($x2478 (= $x903 (or (exc_halt_t 1) (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_t 1))))))))
 (let (($x10749 (forall ((w (_ BitVec 256)) )(let ((?x4485 (storage_t x_0 x_1 x_MLOAD_0 x_MLOAD_1 1 w)))
 (let ((?x4945 (storage_t x_0 x_1 x_MLOAD_0 x_MLOAD_1 2 w)))
 (= ?x4945 ?x4485))))
 ))
 (let (($x3329 (forall ((n (_ BitVec 6)) )(let ((?x2039 (stack_t x_0 x_1 x_MLOAD_0 x_MLOAD_1 1 n)))
 (let ((?x7359 (stack_t x_0 x_1 x_MLOAD_0 x_MLOAD_1 2 n)))
 (or (= ?x7359 ?x2039) (bvsle (bvadd (_ bv63 6) (sc_t 1)) n)))))
 ))
 (let ((?x7309 (used_gas_t x_0 x_1 x_MLOAD_0 x_MLOAD_1 2)))
 (let (($x6822 (forall ((w (_ BitVec 256)) )(let ((?x1750 (storage_t x_0 x_1 x_MLOAD_0 x_MLOAD_1 0 w)))
 (let ((?x4485 (storage_t x_0 x_1 x_MLOAD_0 x_MLOAD_1 1 w)))
 (= ?x4485 ?x1750))))
 ))
 (let (($x848 (forall ((n (_ BitVec 6)) )(let ((?x994 (stack_t x_0 x_1 x_MLOAD_0 x_MLOAD_1 0 n)))
 (let ((?x2039 (stack_t x_0 x_1 x_MLOAD_0 x_MLOAD_1 1 n)))
 (or (= ?x2039 ?x994) (bvsle (bvadd (_ bv62 6) (sc_t 0)) n)))))
 ))
 (let (($x8508 (= (stack_t x_0 x_1 x_MLOAD_0 x_MLOAD_1 1 (bvadd (_ bv62 6) (sc_t 1))) (stack_t x_0 x_1 x_MLOAD_0 x_MLOAD_1 0 (bvadd (_ bv63 6) ?x63)))))
 (let (($x562 (= (stack_t x_0 x_1 x_MLOAD_0 x_MLOAD_1 1 (bvadd (_ bv63 6) (sc_t 1))) (stack_t x_0 x_1 x_MLOAD_0 x_MLOAD_1 0 (bvadd (_ bv62 6) ?x63)))))
 (let (($x1976 (= $x772 (or (exc_halt_s 5) (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_s 5))))))))
 (let (($x6096 (forall ((w (_ BitVec 256)) )(let ((?x8796 (storage_s x_0 x_1 x_MLOAD_0 x_MLOAD_1 5 w)))
 (let ((?x9268 (storage_s x_0 x_1 x_MLOAD_0 x_MLOAD_1 6 w)))
 (= ?x9268 ?x8796))))
 ))
 (let (($x1834 (forall ((n (_ BitVec 6)) )(let ((?x805 (sc_s 5)))
 (let ((?x8276 (bvadd (_ bv63 6) ?x805)))
 (let (($x7252 (bvsle ?x8276 n)))
 (let ((?x8792 (stack_s x_0 x_1 x_MLOAD_0 x_MLOAD_1 5 n)))
 (let ((?x953 (stack_s x_0 x_1 x_MLOAD_0 x_MLOAD_1 6 n)))
 (or (= ?x953 ?x8792) $x7252)))))))
 ))
 (let (($x1521 (= (used_gas_s x_0 x_1 x_MLOAD_0 x_MLOAD_1 6) (+ 2 (used_gas_s x_0 x_1 x_MLOAD_0 x_MLOAD_1 5)))))
 (let (($x3979 (exc_halt_s 5)))
 (let (($x2352 (= $x3979 (or (exc_halt_s 4) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 4))))))))
 (let (($x842 (forall ((w (_ BitVec 256)) )(let ((?x201 (storage_s x_0 x_1 x_MLOAD_0 x_MLOAD_1 4 w)))
 (let ((?x8796 (storage_s x_0 x_1 x_MLOAD_0 x_MLOAD_1 5 w)))
 (= ?x8796 ?x201))))
 ))
 (let (($x1431 (forall ((n (_ BitVec 6)) )(let ((?x1784 (stack_s x_0 x_1 x_MLOAD_0 x_MLOAD_1 4 n)))
 (let ((?x8792 (stack_s x_0 x_1 x_MLOAD_0 x_MLOAD_1 5 n)))
 (or (= ?x8792 ?x1784) (bvsle (bvadd (_ bv62 6) (sc_s 4)) n)))))
 ))
 (let ((?x2385 (used_gas_s x_0 x_1 x_MLOAD_0 x_MLOAD_1 5)))
 (let ((?x4305 (sc_s 4)))
 (let ((?x1909 (bvadd (_ bv63 6) ?x4305)))
 (let ((?x1857 (stack_s x_0 x_1 x_MLOAD_0 x_MLOAD_1 4 ?x1909)))
 (let ((?x5889 (bvadd (_ bv62 6) ?x4305)))
 (let ((?x1951 (stack_s x_0 x_1 x_MLOAD_0 x_MLOAD_1 4 ?x5889)))
 (let (($x9463 (= (stack_s x_0 x_1 x_MLOAD_0 x_MLOAD_1 5 (bvadd (_ bv63 6) (sc_s 5))) (ite (bvule ?x1951 ?x1857) (_ bv0 256) (_ bv1 256)))))
 (let (($x64 (exc_halt_s 4)))
 (let (($x853 (= $x64 (or (exc_halt_s 3) (not (bvsle (_ bv0 6) (bvadd (_ bv61 6) (sc_s 3))))))))
 (let (($x6506 (forall ((w (_ BitVec 256)) )(let ((?x7139 (storage_s x_0 x_1 x_MLOAD_0 x_MLOAD_1 3 w)))
 (let ((?x201 (storage_s x_0 x_1 x_MLOAD_0 x_MLOAD_1 4 w)))
 (= ?x201 ?x7139))))
 ))
 (let (($x10268 (forall ((n (_ BitVec 6)) )(let ((?x4589 (stack_s x_0 x_1 x_MLOAD_0 x_MLOAD_1 3 n)))
 (let ((?x1784 (stack_s x_0 x_1 x_MLOAD_0 x_MLOAD_1 4 n)))
 (or (bvsle (bvadd (_ bv61 6) (sc_s 3)) n) (= ?x1784 ?x4589)))))
 ))
 (let ((?x275 (sc_s 3)))
 (let (($x1346 (= ?x4305 ?x275)))
 (let ((?x5473 (used_gas_s x_0 x_1 x_MLOAD_0 x_MLOAD_1 4)))
 (let ((?x300 (bvadd (_ bv63 6) ?x275)))
 (let ((?x7471 (stack_s x_0 x_1 x_MLOAD_0 x_MLOAD_1 3 ?x300)))
 (let (($x292 (exc_halt_s 3)))
 (let (($x192 (= $x292 (or (exc_halt_s 2) (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_s 2))))))))
 (let (($x8806 (forall ((w (_ BitVec 256)) )(let ((?x1885 (storage_s x_0 x_1 x_MLOAD_0 x_MLOAD_1 2 w)))
 (let ((?x7139 (storage_s x_0 x_1 x_MLOAD_0 x_MLOAD_1 3 w)))
 (= ?x7139 ?x1885))))
 ))
 (let (($x1697 (forall ((n (_ BitVec 6)) )(let ((?x10914 (stack_s x_0 x_1 x_MLOAD_0 x_MLOAD_1 2 n)))
 (let ((?x4589 (stack_s x_0 x_1 x_MLOAD_0 x_MLOAD_1 3 n)))
 (or (bvsle (bvadd (_ bv63 6) (sc_s 2)) n) (= ?x4589 ?x10914)))))
 ))
 (let ((?x6594 (used_gas_s x_0 x_1 x_MLOAD_0 x_MLOAD_1 3)))
 (let ((?x218 (sc_s 2)))
 (let ((?x8036 (bvadd (_ bv63 6) ?x218)))
 (let ((?x1378 (stack_s x_0 x_1 x_MLOAD_0 x_MLOAD_1 2 ?x8036)))
 (let (($x9933 (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_s 1))))))
 (let (($x189 (exc_halt_s 1)))
 (let (($x1038 (or $x189 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 1)))) (_ bv0 1))) $x9933)))
 (let (($x247 (exc_halt_s 2)))
 (let (($x5148 (forall ((w (_ BitVec 256)) )(let ((?x1996 (storage_s x_0 x_1 x_MLOAD_0 x_MLOAD_1 1 w)))
 (let ((?x1885 (storage_s x_0 x_1 x_MLOAD_0 x_MLOAD_1 2 w)))
 (= ?x1885 ?x1996))))
 ))
 (let (($x9202 (forall ((n (_ BitVec 6)) )(let ((?x154 (sc_s 1)))
 (let ((?x2733 (bvadd (_ bv63 6) ?x154)))
 (let (($x9668 (bvsle ?x2733 n)))
 (let ((?x4653 (stack_s x_0 x_1 x_MLOAD_0 x_MLOAD_1 1 n)))
 (let ((?x10914 (stack_s x_0 x_1 x_MLOAD_0 x_MLOAD_1 2 n)))
 (or (= ?x10914 ?x4653) $x9668)))))))
 ))
 (let ((?x3067 (used_gas_s x_0 x_1 x_MLOAD_0 x_MLOAD_1 2)))
 (let ((?x154 (sc_s 1)))
 (let ((?x2733 (bvadd (_ bv63 6) ?x154)))
 (let ((?x215 (stack_s x_0 x_1 x_MLOAD_0 x_MLOAD_1 1 ?x2733)))
 (let (($x2332 (forall ((w (_ BitVec 256)) )(let ((?x1979 (storage_s x_0 x_1 x_MLOAD_0 x_MLOAD_1 0 w)))
 (let ((?x1996 (storage_s x_0 x_1 x_MLOAD_0 x_MLOAD_1 1 w)))
 (= ?x1996 ?x1979))))
 ))
 (let (($x6247 (forall ((n (_ BitVec 6)) )(let ((?x1502 (stack_s x_0 x_1 x_MLOAD_0 x_MLOAD_1 0 n)))
 (let ((?x4653 (stack_s x_0 x_1 x_MLOAD_0 x_MLOAD_1 1 n)))
 (or (bvsle (bvadd (_ bv63 6) (sc_s 0)) n) (= ?x4653 ?x1502)))))
 ))
 (let ((?x419 (f_MLOAD x_0 x_1 x_MLOAD_0 x_MLOAD_1 (stack_s x_0 x_1 x_MLOAD_0 x_MLOAD_1 0 (bvadd (_ bv63 6) ?x72)))))
 (let (($x5553 (forall ((w0 (_ BitVec 256)) )(let (($x7778 (= (stack_s x_0 x_1 x_MLOAD_0 x_MLOAD_1 2 (bvadd (_ bv63 6) (sc_s 2))) w0)))
 (let (($x10802 (= (stack_s x_0 x_1 x_MLOAD_0 x_MLOAD_1 0 (bvadd (_ bv63 6) (sc_s 0))) w0)))
 (let ((?x5523 (f_MLOAD x_0 x_1 x_MLOAD_0 x_MLOAD_1 w0)))
 (= ?x5523 (ite $x10802 x_MLOAD_0 (ite $x7778 x_MLOAD_1 (_ bv0 256))))))))
 ))
 (let (($x10760 (forall ((w (_ BitVec 256)) )(let ((?x1979 (storage_s x_0 x_1 x_MLOAD_0 x_MLOAD_1 0 w)))
 (= ?x1979 (_ bv0 256))))
 ))
 (let (($x9582 (= ?x231 0)))
 (let (($x6677 (not $x57)))
 (let (($x2157 (= (stack_s x_0 x_1 x_MLOAD_0 x_MLOAD_1 0 (_ bv1 6)) x_1)))
 (let (($x3712 (= (stack_s x_0 x_1 x_MLOAD_0 x_MLOAD_1 0 (_ bv0 6)) x_0)))
 (let (($x1347 (= ?x72 (_ bv2 6))))
 (and $x1347 $x3712 $x2157 $x6677 $x9582 $x10760 $x5553 (= ?x215 ?x419) (= (used_gas_s x_0 x_1 x_MLOAD_0 x_MLOAD_1 1) (+ 3 ?x231)) (= ?x154 ?x72) $x6247 $x2332 (= $x189 (or $x57 (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) ?x72))))) (= ?x1378 ?x215) (= (stack_s x_0 x_1 x_MLOAD_0 x_MLOAD_1 2 ?x2733) ?x215) (= ?x3067 (+ 3 (used_gas_s x_0 x_1 x_MLOAD_0 x_MLOAD_1 1))) (= ?x218 (bvadd (_ bv1 6) ?x154)) $x9202 $x5148 (= $x247 $x1038) (= ?x7471 (f_MLOAD x_0 x_1 x_MLOAD_0 x_MLOAD_1 ?x1378)) (= ?x6594 (+ 3 ?x3067)) (= ?x275 ?x218) $x1697 $x8806 $x192 (= ?x1857 (stack_s x_0 x_1 x_MLOAD_0 x_MLOAD_1 3 (bvadd (_ bv61 6) ?x275))) (= (stack_s x_0 x_1 x_MLOAD_0 x_MLOAD_1 4 (bvadd (_ bv61 6) ?x4305)) ?x7471) (= ?x1951 (stack_s x_0 x_1 x_MLOAD_0 x_MLOAD_1 3 (bvadd (_ bv62 6) ?x275))) (= ?x5473 (+ 3 ?x6594)) $x1346 $x10268 $x6506 $x853 $x9463 (= ?x2385 (+ 3 ?x5473)) (= (sc_s 5) ?x1909) $x1431 $x842 $x2352 $x1521 (= ?x926 (bvadd (_ bv63 6) (sc_s 5))) $x1834 $x6096 $x1976 $x562 $x8508 (= (used_gas_t x_0 x_1 x_MLOAD_0 x_MLOAD_1 1) (+ 3 ?x4828)) (= (sc_t 1) ?x63) $x848 $x6822 (= (exc_halt_t 1) (or $x56 (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) ?x63))))) (= ?x7309 (+ 2 (used_gas_t x_0 x_1 x_MLOAD_0 x_MLOAD_1 1))) (= ?x4056 (bvadd (_ bv63 6) (sc_t 1))) $x3329 $x10749 $x2478 (= ?x2692 ?x1034) (= ?x1463 (+ 3 ?x7309)) $x1273 $x6650 $x7527 $x5923 $x6949 $x1721 (= ?x3757 ?x2012) $x6007 $x9772 $x9766 $x73 $x1398 $x58 $x1024 $x525 (not (and $x7941 $x2723 $x4256 $x10867))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
 )
(check-sat)
(get-proof)