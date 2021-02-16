; PUSH 0x00 ADD DUP1 DUP1 => DUP1 DUP2
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
 (exists ((x_0 (_ BitVec 256)) )(let (($x8901 (forall ((w (_ BitVec 256)) )(let ((?x10991 (storage_t x_0 2 w)))
 (let ((?x5805 (storage_s x_0 4 w)))
 (= ?x5805 ?x10991))))
 ))
 (let (($x5252 (exc_halt_t 2)))
 (let (($x7172 (exc_halt_s 4)))
 (let (($x773 (= $x7172 $x5252)))
 (let (($x11019 (forall ((n (_ BitVec 6)) )(let ((?x5059 (stack_t x_0 2 n)))
 (let ((?x6249 (stack_s x_0 4 n)))
 (let (($x7215 (= ?x6249 ?x5059)))
 (or (bvsle (sc_t 2) n) $x7215)))))
 ))
 (let ((?x2714 (sc_t 2)))
 (let ((?x4305 (sc_s 4)))
 (let (($x8664 (= ?x4305 ?x2714)))
 (let ((?x6132 (used_gas_t x_0 0)))
 (let ((?x1095 (used_gas_s x_0 0)))
 (let (($x3862 (= ?x1095 ?x6132)))
 (let (($x7953 (forall ((w (_ BitVec 256)) )(let ((?x3836 (storage_t x_0 0 w)))
 (let ((?x10398 (storage_s x_0 0 w)))
 (= ?x10398 ?x3836))))
 ))
 (let (($x56 (exc_halt_t 0)))
 (let (($x57 (exc_halt_s 0)))
 (let (($x58 (= $x57 $x56)))
 (let (($x6571 (forall ((n (_ BitVec 6)) )(let ((?x752 (stack_t x_0 0 n)))
 (let ((?x11600 (stack_s x_0 0 n)))
 (let (($x5314 (= ?x11600 ?x752)))
 (or (bvsle (sc_t 0) n) $x5314)))))
 ))
 (let ((?x63 (sc_t 0)))
 (let ((?x72 (sc_s 0)))
 (let (($x73 (= ?x72 ?x63)))
 (let (($x3120 (exc_halt_t 1)))
 (let (($x8418 (or (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_t 1)))) $x3120 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_t 1)))) (_ bv0 1))))))
 (let (($x2338 (forall ((w (_ BitVec 256)) )(let ((?x3372 (storage_t x_0 1 w)))
 (let ((?x10991 (storage_t x_0 2 w)))
 (= ?x10991 ?x3372))))
 ))
 (let (($x3208 (forall ((n (_ BitVec 6)) )(or (= (stack_t x_0 2 n) (stack_t x_0 1 n)) (bvsle (bvadd (_ bv62 6) (sc_t 1)) n)))
 ))
 (let ((?x6052 (sc_t 1)))
 (let ((?x5428 (bvadd (_ bv63 6) ?x6052)))
 (let ((?x10950 (stack_t x_0 1 ?x5428)))
 (let ((?x11677 (bvadd (_ bv62 6) ?x6052)))
 (let ((?x4526 (stack_t x_0 1 ?x11677)))
 (let (($x10031 (or $x56 (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) ?x63))) (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x63))) (_ bv0 1))))))
 (let (($x3134 (forall ((w (_ BitVec 256)) )(let ((?x3836 (storage_t x_0 0 w)))
 (let ((?x3372 (storage_t x_0 1 w)))
 (= ?x3372 ?x3836))))
 ))
 (let (($x4728 (forall ((n (_ BitVec 6)) )(or (bvsle (bvadd (_ bv63 6) (sc_t 0)) n) (= (stack_t x_0 1 n) (stack_t x_0 0 n))))
 ))
 (let ((?x3437 (bvadd (_ bv63 6) ?x63)))
 (let ((?x4768 (stack_t x_0 0 ?x3437)))
 (let (($x292 (exc_halt_s 3)))
 (let (($x11197 (or $x292 (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_s 3)))) (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 3)))) (_ bv0 1))))))
 (let (($x3515 (forall ((w (_ BitVec 256)) )(let ((?x9644 (storage_s x_0 3 w)))
 (let ((?x5805 (storage_s x_0 4 w)))
 (= ?x5805 ?x9644))))
 ))
 (let (($x10736 (forall ((n (_ BitVec 6)) )(or (bvsle (bvadd (_ bv63 6) (sc_s 3)) n) (= (stack_s x_0 4 n) (stack_s x_0 3 n))))
 ))
 (let ((?x275 (sc_s 3)))
 (let ((?x7071 (bvadd (_ bv63 6) ?x275)))
 (let ((?x10477 (stack_s x_0 3 ?x7071)))
 (let (($x247 (exc_halt_s 2)))
 (let (($x11187 (or $x247 (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_s 2)))) (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 2)))) (_ bv0 1))))))
 (let (($x10772 (forall ((w (_ BitVec 256)) )(let ((?x5244 (storage_s x_0 2 w)))
 (let ((?x9644 (storage_s x_0 3 w)))
 (= ?x9644 ?x5244))))
 ))
 (let (($x6445 (forall ((n (_ BitVec 6)) )(or (= (stack_s x_0 3 n) (stack_s x_0 2 n)) (bvsle (bvadd (_ bv63 6) (sc_s 2)) n)))
 ))
 (let ((?x218 (sc_s 2)))
 (let ((?x5148 (bvadd (_ bv63 6) ?x218)))
 (let ((?x11403 (stack_s x_0 2 ?x5148)))
 (let (($x3125 (= $x247 (or (exc_halt_s 1) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 1))))))))
 (let (($x2129 (forall ((w (_ BitVec 256)) )(let ((?x11460 (storage_s x_0 1 w)))
 (let ((?x5244 (storage_s x_0 2 w)))
 (= ?x5244 ?x11460))))
 ))
 (let (($x2115 (forall ((n (_ BitVec 6)) )(or (= (stack_s x_0 2 n) (stack_s x_0 1 n)) (bvsle (bvadd (_ bv62 6) (sc_s 1)) n)))
 ))
 (let ((?x6700 (bvadd (stack_s x_0 1 (bvadd (_ bv63 6) (sc_s 1))) (stack_s x_0 1 (bvadd (_ bv62 6) (sc_s 1))))))
 (let (($x189 (exc_halt_s 1)))
 (let (($x1654 (= $x189 (or $x57 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x72))) (_ bv0 1)))))))
 (let (($x7848 (forall ((w (_ BitVec 256)) )(let ((?x10398 (storage_s x_0 0 w)))
 (let ((?x11460 (storage_s x_0 1 w)))
 (= ?x11460 ?x10398))))
 ))
 (let (($x5693 (forall ((n (_ BitVec 6)) )(or (bvsle (sc_s 0) n) (= (stack_s x_0 1 n) (stack_s x_0 0 n))))
 ))
 (let (($x4375 (forall ((w (_ BitVec 256)) )(let ((?x10398 (storage_s x_0 0 w)))
 (= ?x10398 (_ bv0 256))))
 ))
 (let (($x4059 (= ?x1095 0)))
 (let (($x774 (= (stack_s x_0 0 (_ bv0 6)) x_0)))
 (let (($x8685 (= ?x72 (_ bv1 6))))
 (and $x8685 $x774 (not $x57) $x4059 $x4375 (= (stack_s x_0 1 ?x72) (_ bv0 256)) (= (used_gas_s x_0 1) (+ 3 ?x1095)) (= (sc_s 1) (bvadd (_ bv1 6) ?x72)) $x5693 $x7848 $x1654 (= ?x11403 ?x6700) (= (used_gas_s x_0 2) (+ 3 (used_gas_s x_0 1))) (= ?x218 (bvadd (_ bv63 6) (sc_s 1))) $x2115 $x2129 $x3125 (= ?x10477 ?x11403) (= (stack_s x_0 3 ?x5148) ?x11403) (= (used_gas_s x_0 3) (+ 3 (used_gas_s x_0 2))) (= ?x275 (bvadd (_ bv1 6) ?x218)) $x6445 $x10772 (= $x292 $x11187) (= (stack_s x_0 4 (bvadd (_ bv63 6) ?x4305)) ?x10477) (= (stack_s x_0 4 ?x7071) ?x10477) (= (used_gas_s x_0 4) (+ 3 (used_gas_s x_0 3))) (= ?x4305 (bvadd (_ bv1 6) ?x275)) $x10736 $x3515 (= $x7172 $x11197) (= ?x10950 ?x4768) (= (stack_t x_0 1 ?x3437) ?x4768) (= (used_gas_t x_0 1) (+ 3 ?x6132)) (= ?x6052 (bvadd (_ bv1 6) ?x63)) $x4728 $x3134 (= $x3120 $x10031) (= (stack_t x_0 2 (bvadd (_ bv63 6) ?x2714)) ?x4526) (= (stack_t x_0 2 ?x11677) ?x4526) (= (stack_t x_0 2 ?x5428) ?x10950) (= (used_gas_t x_0 2) (+ 3 (used_gas_t x_0 1))) (= ?x2714 (bvadd (_ bv1 6) ?x6052)) $x3208 $x2338 (= $x5252 $x8418) $x73 $x6571 $x58 $x7953 $x3862 (not (and $x8664 $x11019 $x773 $x8901)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
 )
(check-sat)
(get-proof)