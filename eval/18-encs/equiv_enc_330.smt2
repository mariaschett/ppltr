; PUSH 0x01 MUL PUSH 0x00 NOT AND => 
(set-option :produce-proofs true)
; 
(set-info :status unknown)
(declare-fun storage_t ((_ BitVec 256) Int (_ BitVec 256)) (_ BitVec 256))
(declare-fun storage_s ((_ BitVec 256) Int (_ BitVec 256)) (_ BitVec 256))
(declare-fun exc_halt_t (Int) Bool)
(declare-fun exc_halt_s (Int) Bool)
(declare-fun stack_t ((_ BitVec 256) Int (_ BitVec 6)) (_ BitVec 256))
(declare-fun stack_s ((_ BitVec 256) Int (_ BitVec 6)) (_ BitVec 256))
(declare-fun sc_t (Int) (_ BitVec 6))
(declare-fun sc_s (Int) (_ BitVec 6))
(declare-fun used_gas_t ((_ BitVec 256) Int) Int)
(declare-fun used_gas_s ((_ BitVec 256) Int) Int)
(assert
 (exists ((x_0 (_ BitVec 256)) )(let (($x9594 (forall ((w (_ BitVec 256)) )(let ((?x977 (storage_t x_0 0 w)))
 (let ((?x750 (storage_s x_0 5 w)))
 (= ?x750 ?x977))))
 ))
 (let (($x56 (exc_halt_t 0)))
 (let (($x3979 (exc_halt_s 5)))
 (let (($x9458 (= $x3979 $x56)))
 (let (($x4838 (forall ((n (_ BitVec 6)) )(let ((?x3733 (stack_t x_0 0 n)))
 (let ((?x6181 (stack_s x_0 5 n)))
 (let (($x11291 (= ?x6181 ?x3733)))
 (let ((?x63 (sc_t 0)))
 (let (($x2337 (bvsle ?x63 n)))
 (or $x2337 $x11291)))))))
 ))
 (let ((?x63 (sc_t 0)))
 (let ((?x805 (sc_s 5)))
 (let (($x11555 (= ?x805 ?x63)))
 (let ((?x6384 (used_gas_s x_0 0)))
 (let (($x4323 (= ?x6384 (used_gas_t x_0 0))))
 (let (($x1062 (forall ((w (_ BitVec 256)) )(let ((?x977 (storage_t x_0 0 w)))
 (let ((?x5977 (storage_s x_0 0 w)))
 (= ?x5977 ?x977))))
 ))
 (let (($x57 (exc_halt_s 0)))
 (let (($x58 (= $x57 $x56)))
 (let (($x3764 (forall ((n (_ BitVec 6)) )(let ((?x3733 (stack_t x_0 0 n)))
 (let ((?x11605 (stack_s x_0 0 n)))
 (let (($x7038 (= ?x11605 ?x3733)))
 (let ((?x63 (sc_t 0)))
 (let (($x2337 (bvsle ?x63 n)))
 (or $x2337 $x7038)))))))
 ))
 (let ((?x72 (sc_s 0)))
 (let (($x73 (= ?x72 ?x63)))
 (let (($x9010 (= $x3979 (or (exc_halt_s 4) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 4))))))))
 (let (($x1261 (forall ((w (_ BitVec 256)) )(let ((?x10573 (storage_s x_0 4 w)))
 (let ((?x750 (storage_s x_0 5 w)))
 (= ?x750 ?x10573))))
 ))
 (let (($x9932 (forall ((n (_ BitVec 6)) )(let ((?x4305 (sc_s 4)))
 (let ((?x8912 (bvadd (_ bv62 6) ?x4305)))
 (let (($x9203 (bvsle ?x8912 n)))
 (or (= (stack_s x_0 5 n) (stack_s x_0 4 n)) $x9203)))))
 ))
 (let ((?x4305 (sc_s 4)))
 (let ((?x10093 (bvadd (_ bv63 6) ?x4305)))
 (let (($x697 (= ?x805 ?x10093)))
 (let ((?x4333 (bvor (bvnot (stack_s x_0 4 ?x10093)) (bvnot (stack_s x_0 4 (bvadd (_ bv62 6) ?x4305))))))
 (let (($x7172 (exc_halt_s 4)))
 (let (($x8375 (= $x7172 (or (exc_halt_s 3) (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_s 3))))))))
 (let (($x6863 (forall ((w (_ BitVec 256)) )(let ((?x1204 (storage_s x_0 3 w)))
 (let ((?x10573 (storage_s x_0 4 w)))
 (= ?x10573 ?x1204))))
 ))
 (let (($x10795 (forall ((n (_ BitVec 6)) )(let ((?x275 (sc_s 3)))
 (let ((?x6666 (bvadd (_ bv63 6) ?x275)))
 (let (($x1955 (bvsle ?x6666 n)))
 (or $x1955 (= (stack_s x_0 4 n) (stack_s x_0 3 n)))))))
 ))
 (let ((?x275 (sc_s 3)))
 (let (($x5336 (= ?x4305 ?x275)))
 (let ((?x11437 (stack_s x_0 4 ?x10093)))
 (let (($x834 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 2)))) (_ bv0 1)))))
 (let (($x247 (exc_halt_s 2)))
 (let (($x292 (exc_halt_s 3)))
 (let (($x7327 (forall ((w (_ BitVec 256)) )(let ((?x9851 (storage_s x_0 2 w)))
 (let ((?x1204 (storage_s x_0 3 w)))
 (= ?x1204 ?x9851))))
 ))
 (let (($x9733 (forall ((n (_ BitVec 6)) )(or (bvsle (sc_s 2) n) (= (stack_s x_0 3 n) (stack_s x_0 2 n))))
 ))
 (let (($x4905 (= ?x275 (bvadd (_ bv1 6) (sc_s 2)))))
 (let (($x8572 (= $x247 (or (exc_halt_s 1) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 1))))))))
 (let (($x10456 (forall ((w (_ BitVec 256)) )(let ((?x10889 (storage_s x_0 1 w)))
 (let ((?x9851 (storage_s x_0 2 w)))
 (= ?x9851 ?x10889))))
 ))
 (let (($x3255 (forall ((n (_ BitVec 6)) )(or (bvsle (bvadd (_ bv62 6) (sc_s 1)) n) (= (stack_s x_0 2 n) (stack_s x_0 1 n))))
 ))
 (let ((?x10382 (bvmul (stack_s x_0 1 (bvadd (_ bv62 6) (sc_s 1))) (stack_s x_0 1 (bvadd (_ bv63 6) (sc_s 1))))))
 (let (($x189 (exc_halt_s 1)))
 (let (($x4214 (= $x189 (or $x57 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x72))) (_ bv0 1)))))))
 (let (($x507 (forall ((w (_ BitVec 256)) )(let ((?x5977 (storage_s x_0 0 w)))
 (let ((?x10889 (storage_s x_0 1 w)))
 (= ?x10889 ?x5977))))
 ))
 (let (($x923 (forall ((n (_ BitVec 6)) )(let ((?x72 (sc_s 0)))
 (let (($x8577 (bvsle ?x72 n)))
 (or $x8577 (= (stack_s x_0 1 n) (stack_s x_0 0 n))))))
 ))
 (let ((?x154 (sc_s 1)))
 (let (($x3297 (= ?x154 (bvadd (_ bv1 6) ?x72))))
 (let (($x6991 (forall ((w (_ BitVec 256)) )(let ((?x5977 (storage_s x_0 0 w)))
 (= ?x5977 (_ bv0 256))))
 ))
 (let (($x6871 (= ?x6384 0)))
 (let (($x4887 (not $x57)))
 (let (($x11444 (= (stack_s x_0 0 (_ bv0 6)) x_0)))
 (let (($x8685 (= ?x72 (_ bv1 6))))
 (and $x8685 $x11444 $x4887 $x6871 $x6991 (= (stack_s x_0 1 ?x72) (_ bv1 256)) (= (used_gas_s x_0 1) (+ 3 ?x6384)) $x3297 $x923 $x507 $x4214 (= (stack_s x_0 2 (bvadd (_ bv63 6) (sc_s 2))) ?x10382) (= (used_gas_s x_0 2) (+ 5 (used_gas_s x_0 1))) (= (sc_s 2) (bvadd (_ bv63 6) ?x154)) $x3255 $x10456 $x8572 (= (stack_s x_0 3 (sc_s 2)) (_ bv0 256)) (= (used_gas_s x_0 3) (+ 3 (used_gas_s x_0 2))) $x4905 $x9733 $x7327 (= $x292 (or $x247 $x834)) (= ?x11437 (bvnot (stack_s x_0 3 (bvadd (_ bv63 6) ?x275)))) (= (used_gas_s x_0 4) (+ 3 (used_gas_s x_0 3))) $x5336 $x10795 $x6863 $x8375 (= (stack_s x_0 5 (bvadd (_ bv63 6) ?x805)) (bvnot ?x4333)) (= (used_gas_s x_0 5) (+ 3 (used_gas_s x_0 4))) $x697 $x9932 $x1261 $x9010 $x73 $x3764 $x58 $x1062 $x4323 (not (and $x11555 $x4838 $x9458 $x9594)))))))))))))))))))))))))))))))))))))))))))))))))))))))
 )
(check-sat)
(get-proof)