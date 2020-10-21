; SWAP1 POP POP => POP POP
(set-option :produce-proofs true)
; 
(set-info :status unknown)
(declare-fun storage_t ((_ BitVec 256) (_ BitVec 256) Int (_ BitVec 256)) (_ BitVec 256))
(declare-fun storage_s ((_ BitVec 256) (_ BitVec 256) Int (_ BitVec 256)) (_ BitVec 256))
(declare-fun exc_halt_t (Int) Bool)
(declare-fun exc_halt_s (Int) Bool)
(declare-fun stack_t ((_ BitVec 256) (_ BitVec 256) Int (_ BitVec 6)) (_ BitVec 256))
(declare-fun stack_s ((_ BitVec 256) (_ BitVec 256) Int (_ BitVec 6)) (_ BitVec 256))
(declare-fun sc_t (Int) (_ BitVec 6))
(declare-fun sc_s (Int) (_ BitVec 6))
(declare-fun used_gas_t ((_ BitVec 256) (_ BitVec 256) Int) Int)
(declare-fun used_gas_s ((_ BitVec 256) (_ BitVec 256) Int) Int)
(assert
 (exists ((x_0 (_ BitVec 256)) (x_1 (_ BitVec 256)) )(let (($x608 (forall ((w (_ BitVec 256)) )(let ((?x10738 (storage_t x_0 x_1 2 w)))
 (let ((?x382 (storage_s x_0 x_1 3 w)))
 (= ?x382 ?x10738))))
 ))
 (let (($x2163 (exc_halt_t 2)))
 (let (($x292 (exc_halt_s 3)))
 (let (($x6078 (= $x292 $x2163)))
 (let (($x5260 (forall ((n (_ BitVec 6)) )(let ((?x6969 (stack_t x_0 x_1 2 n)))
 (let ((?x704 (stack_s x_0 x_1 3 n)))
 (let (($x3692 (= ?x704 ?x6969)))
 (let ((?x2714 (sc_t 2)))
 (let (($x1932 (bvsle ?x2714 n)))
 (or $x1932 $x3692)))))))
 ))
 (let ((?x2714 (sc_t 2)))
 (let ((?x275 (sc_s 3)))
 (let (($x8805 (= ?x275 ?x2714)))
 (let ((?x10660 (used_gas_t x_0 x_1 0)))
 (let ((?x2095 (used_gas_s x_0 x_1 0)))
 (let (($x3815 (= ?x2095 ?x10660)))
 (let (($x970 (forall ((w (_ BitVec 256)) )(let ((?x7036 (storage_t x_0 x_1 0 w)))
 (let ((?x99 (storage_s x_0 x_1 0 w)))
 (= ?x99 ?x7036))))
 ))
 (let (($x56 (exc_halt_t 0)))
 (let (($x57 (exc_halt_s 0)))
 (let (($x58 (= $x57 $x56)))
 (let (($x1039 (forall ((n (_ BitVec 6)) )(let ((?x4808 (stack_t x_0 x_1 0 n)))
 (let ((?x5807 (stack_s x_0 x_1 0 n)))
 (let (($x7810 (= ?x5807 ?x4808)))
 (let ((?x63 (sc_t 0)))
 (let (($x2855 (bvsle ?x63 n)))
 (or $x2855 $x7810)))))))
 ))
 (let ((?x63 (sc_t 0)))
 (let ((?x72 (sc_s 0)))
 (let (($x73 (= ?x72 ?x63)))
 (let (($x8830 (= $x2163 (or (exc_halt_t 1) (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_t 1))))))))
 (let (($x4078 (forall ((w (_ BitVec 256)) )(let ((?x9389 (storage_t x_0 x_1 1 w)))
 (let ((?x10738 (storage_t x_0 x_1 2 w)))
 (= ?x10738 ?x9389))))
 ))
 (let (($x3073 (forall ((n (_ BitVec 6)) )(let ((?x7154 (sc_t 1)))
 (let ((?x2239 (bvadd (_ bv63 6) ?x7154)))
 (let (($x2352 (bvsle ?x2239 n)))
 (or (= (stack_t x_0 x_1 2 n) (stack_t x_0 x_1 1 n)) $x2352)))))
 ))
 (let ((?x7154 (sc_t 1)))
 (let ((?x2239 (bvadd (_ bv63 6) ?x7154)))
 (let (($x1355 (= ?x2714 ?x2239)))
 (let (($x1356 (= (used_gas_t x_0 x_1 2) (+ 2 (used_gas_t x_0 x_1 1)))))
 (let (($x8377 (exc_halt_t 1)))
 (let (($x1851 (= $x8377 (or $x56 (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) ?x63)))))))
 (let (($x10011 (forall ((w (_ BitVec 256)) )(let ((?x7036 (storage_t x_0 x_1 0 w)))
 (let ((?x9389 (storage_t x_0 x_1 1 w)))
 (= ?x9389 ?x7036))))
 ))
 (let (($x8566 (forall ((n (_ BitVec 6)) )(let ((?x63 (sc_t 0)))
 (let ((?x1545 (bvadd (_ bv63 6) ?x63)))
 (let (($x9469 (bvsle ?x1545 n)))
 (or (= (stack_t x_0 x_1 1 n) (stack_t x_0 x_1 0 n)) $x9469)))))
 ))
 (let (($x9269 (= $x292 (or (exc_halt_s 2) (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_s 2))))))))
 (let (($x2597 (forall ((w (_ BitVec 256)) )(let ((?x1996 (storage_s x_0 x_1 2 w)))
 (let ((?x382 (storage_s x_0 x_1 3 w)))
 (= ?x382 ?x1996))))
 ))
 (let (($x5347 (forall ((n (_ BitVec 6)) )(or (bvsle (bvadd (_ bv63 6) (sc_s 2)) n) (= (stack_s x_0 x_1 3 n) (stack_s x_0 x_1 2 n))))
 ))
 (let ((?x218 (sc_s 2)))
 (let ((?x7113 (bvadd (_ bv63 6) ?x218)))
 (let (($x10468 (= ?x275 ?x7113)))
 (let ((?x7791 (used_gas_s x_0 x_1 3)))
 (let (($x247 (exc_halt_s 2)))
 (let (($x597 (= $x247 (or (exc_halt_s 1) (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_s 1))))))))
 (let (($x1516 (forall ((w (_ BitVec 256)) )(let ((?x627 (storage_s x_0 x_1 1 w)))
 (let ((?x1996 (storage_s x_0 x_1 2 w)))
 (= ?x1996 ?x627))))
 ))
 (let (($x6050 (forall ((n (_ BitVec 6)) )(let ((?x154 (sc_s 1)))
 (let ((?x11673 (bvadd (_ bv63 6) ?x154)))
 (let (($x10494 (bvsle ?x11673 n)))
 (let ((?x4086 (stack_s x_0 x_1 1 n)))
 (let ((?x437 (stack_s x_0 x_1 2 n)))
 (let (($x9479 (= ?x437 ?x4086)))
 (or $x9479 $x10494))))))))
 ))
 (let ((?x7451 (used_gas_s x_0 x_1 2)))
 (let (($x189 (exc_halt_s 1)))
 (let (($x1979 (= $x189 (or $x57 (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) ?x72)))))))
 (let (($x574 (forall ((w (_ BitVec 256)) )(let ((?x99 (storage_s x_0 x_1 0 w)))
 (let ((?x627 (storage_s x_0 x_1 1 w)))
 (= ?x627 ?x99))))
 ))
 (let (($x8114 (forall ((n (_ BitVec 6)) )(or (bvsle (bvadd (_ bv62 6) (sc_s 0)) n) (= (stack_s x_0 x_1 1 n) (stack_s x_0 x_1 0 n))))
 ))
 (let ((?x154 (sc_s 1)))
 (let (($x2802 (= ?x154 ?x72)))
 (let ((?x8841 (used_gas_s x_0 x_1 1)))
 (let (($x11272 (= ?x8841 (+ 3 ?x2095))))
 (let (($x3942 (= (stack_s x_0 x_1 1 (bvadd (_ bv62 6) ?x154)) (stack_s x_0 x_1 0 (bvadd (_ bv63 6) ?x72)))))
 (let (($x9806 (= (stack_s x_0 x_1 1 (bvadd (_ bv63 6) ?x154)) (stack_s x_0 x_1 0 (bvadd (_ bv62 6) ?x72)))))
 (let (($x2124 (forall ((w (_ BitVec 256)) )(let ((?x99 (storage_s x_0 x_1 0 w)))
 (= ?x99 (_ bv0 256))))
 ))
 (let (($x5846 (= ?x2095 0)))
 (let (($x10766 (not $x57)))
 (let (($x7379 (= (stack_s x_0 x_1 0 (_ bv1 6)) x_1)))
 (let (($x9816 (= (stack_s x_0 x_1 0 (_ bv0 6)) x_0)))
 (let (($x11128 (= ?x72 (_ bv2 6))))
 (and $x11128 $x9816 $x7379 $x10766 $x5846 $x2124 $x9806 $x3942 $x11272 $x2802 $x8114 $x574 $x1979 (= ?x7451 (+ 2 ?x8841)) (= ?x218 (bvadd (_ bv63 6) ?x154)) $x6050 $x1516 $x597 (= ?x7791 (+ 2 ?x7451)) $x10468 $x5347 $x2597 $x9269 (= (used_gas_t x_0 x_1 1) (+ 2 ?x10660)) (= ?x7154 (bvadd (_ bv63 6) ?x63)) $x8566 $x10011 $x1851 $x1356 $x1355 $x3073 $x4078 $x8830 $x73 $x1039 $x58 $x970 $x3815 (not (and $x8805 $x5260 $x6078 $x608))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
 )
(check-sat)
(get-proof)
