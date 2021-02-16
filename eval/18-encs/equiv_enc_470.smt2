; DUP2 PUSH 0x00 ADD DUP2 SWAP1 => DUP1 DUP3
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
 (exists ((x_0 (_ BitVec 256)) (x_1 (_ BitVec 256)) )(let (($x7465 (forall ((w (_ BitVec 256)) )(let ((?x2983 (storage_t x_0 x_1 2 w)))
 (let ((?x2177 (storage_s x_0 x_1 5 w)))
 (= ?x2177 ?x2983))))
 ))
 (let (($x2163 (exc_halt_t 2)))
 (let (($x11317 (exc_halt_s 5)))
 (let (($x6773 (= $x11317 $x2163)))
 (let (($x4992 (forall ((n (_ BitVec 6)) )(let ((?x8886 (stack_t x_0 x_1 2 n)))
 (let ((?x7826 (stack_s x_0 x_1 5 n)))
 (let (($x10047 (= ?x7826 ?x8886)))
 (let ((?x2714 (sc_t 2)))
 (let (($x6673 (bvsle ?x2714 n)))
 (or $x6673 $x10047)))))))
 ))
 (let ((?x2714 (sc_t 2)))
 (let ((?x4319 (sc_s 5)))
 (let (($x10201 (= ?x4319 ?x2714)))
 (let ((?x8373 (used_gas_t x_0 x_1 0)))
 (let ((?x9397 (used_gas_s x_0 x_1 0)))
 (let (($x974 (= ?x9397 ?x8373)))
 (let (($x5838 (forall ((w (_ BitVec 256)) )(let ((?x11475 (storage_t x_0 x_1 0 w)))
 (let ((?x10607 (storage_s x_0 x_1 0 w)))
 (= ?x10607 ?x11475))))
 ))
 (let (($x56 (exc_halt_t 0)))
 (let (($x57 (exc_halt_s 0)))
 (let (($x58 (= $x57 $x56)))
 (let (($x5475 (forall ((n (_ BitVec 6)) )(let ((?x8129 (stack_t x_0 x_1 0 n)))
 (let ((?x7406 (stack_s x_0 x_1 0 n)))
 (let (($x6423 (= ?x7406 ?x8129)))
 (let ((?x63 (sc_t 0)))
 (let (($x5499 (bvsle ?x63 n)))
 (or $x5499 $x6423)))))))
 ))
 (let ((?x63 (sc_t 0)))
 (let ((?x72 (sc_s 0)))
 (let (($x73 (= ?x72 ?x63)))
 (let (($x9396 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_t 1)))) (_ bv0 1)))))
 (let (($x3508 (exc_halt_t 1)))
 (let (($x7013 (forall ((w (_ BitVec 256)) )(let ((?x1194 (storage_t x_0 x_1 1 w)))
 (let ((?x2983 (storage_t x_0 x_1 2 w)))
 (= ?x2983 ?x1194))))
 ))
 (let (($x11318 (forall ((n (_ BitVec 6)) )(or (bvsle (bvadd (_ bv61 6) (sc_t 1)) n) (= (stack_t x_0 x_1 2 n) (stack_t x_0 x_1 1 n))))
 ))
 (let (($x134 (= ?x2714 (bvadd (_ bv1 6) (sc_t 1)))))
 (let (($x2632 (= (used_gas_t x_0 x_1 2) (+ 3 (used_gas_t x_0 x_1 1)))))
 (let ((?x8347 (sc_t 1)))
 (let ((?x1086 (bvadd (_ bv63 6) ?x8347)))
 (let ((?x8423 (stack_t x_0 x_1 1 ?x1086)))
 (let (($x11293 (= (stack_t x_0 x_1 2 (bvadd (_ bv62 6) ?x8347)) (stack_t x_0 x_1 1 (bvadd (_ bv62 6) ?x8347)))))
 (let ((?x5437 (bvadd (_ bv61 6) ?x8347)))
 (let ((?x2425 (stack_t x_0 x_1 1 ?x5437)))
 (let (($x3193 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x63))) (_ bv0 1)))))
 (let (($x7763 (= $x3508 (or $x56 $x3193 (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) ?x63)))))))
 (let (($x8556 (forall ((w (_ BitVec 256)) )(let ((?x11475 (storage_t x_0 x_1 0 w)))
 (let ((?x1194 (storage_t x_0 x_1 1 w)))
 (= ?x1194 ?x11475))))
 ))
 (let (($x9836 (forall ((n (_ BitVec 6)) )(let ((?x63 (sc_t 0)))
 (let ((?x6761 (bvadd (_ bv63 6) ?x63)))
 (let (($x7721 (bvsle ?x6761 n)))
 (or (= (stack_t x_0 x_1 1 n) (stack_t x_0 x_1 0 n)) $x7721)))))
 ))
 (let (($x11604 (= ?x8347 (bvadd (_ bv1 6) ?x63))))
 (let ((?x6761 (bvadd (_ bv63 6) ?x63)))
 (let ((?x2467 (stack_t x_0 x_1 0 ?x6761)))
 (let (($x705 (= $x11317 (or (exc_halt_s 4) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 4))))))))
 (let (($x11063 (forall ((w (_ BitVec 256)) )(let ((?x10160 (storage_s x_0 x_1 4 w)))
 (let ((?x2177 (storage_s x_0 x_1 5 w)))
 (= ?x2177 ?x10160))))
 ))
 (let (($x4644 (forall ((n (_ BitVec 6)) )(let ((?x4305 (sc_s 4)))
 (let ((?x3389 (bvadd (_ bv62 6) ?x4305)))
 (let (($x1231 (bvsle ?x3389 n)))
 (or $x1231 (= (stack_s x_0 x_1 5 n) (stack_s x_0 x_1 4 n)))))))
 ))
 (let ((?x4305 (sc_s 4)))
 (let (($x11171 (= ?x4319 ?x4305)))
 (let (($x7473 (= (used_gas_s x_0 x_1 5) (+ 3 (used_gas_s x_0 x_1 4)))))
 (let ((?x6745 (bvadd (_ bv63 6) ?x4305)))
 (let ((?x3113 (stack_s x_0 x_1 4 ?x6745)))
 (let (($x8131 (= (stack_s x_0 x_1 5 (bvadd (_ bv63 6) ?x4319)) (stack_s x_0 x_1 4 (bvadd (_ bv62 6) ?x4305)))))
 (let (($x4369 (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 3))))))
 (let (($x1137 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 3)))) (_ bv0 1)))))
 (let (($x292 (exc_halt_s 3)))
 (let (($x7172 (exc_halt_s 4)))
 (let (($x2293 (forall ((w (_ BitVec 256)) )(let ((?x4260 (storage_s x_0 x_1 3 w)))
 (let ((?x10160 (storage_s x_0 x_1 4 w)))
 (= ?x10160 ?x4260))))
 ))
 (let (($x9266 (forall ((n (_ BitVec 6)) )(let ((?x275 (sc_s 3)))
 (let ((?x1922 (bvadd (_ bv62 6) ?x275)))
 (let (($x2009 (bvsle ?x1922 n)))
 (or $x2009 (= (stack_s x_0 x_1 4 n) (stack_s x_0 x_1 3 n)))))))
 ))
 (let (($x6462 (= ?x4305 (bvadd (_ bv1 6) (sc_s 3)))))
 (let ((?x3213 (used_gas_s x_0 x_1 4)))
 (let ((?x275 (sc_s 3)))
 (let ((?x891 (bvadd (_ bv63 6) ?x275)))
 (let ((?x6136 (stack_s x_0 x_1 3 ?x891)))
 (let ((?x1922 (bvadd (_ bv62 6) ?x275)))
 (let ((?x7964 (stack_s x_0 x_1 3 ?x1922)))
 (let (($x7282 (= $x292 (or (exc_halt_s 2) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 2))))))))
 (let (($x9443 (forall ((w (_ BitVec 256)) )(let ((?x4458 (storage_s x_0 x_1 2 w)))
 (let ((?x4260 (storage_s x_0 x_1 3 w)))
 (= ?x4260 ?x4458))))
 ))
 (let (($x11581 (forall ((n (_ BitVec 6)) )(let ((?x218 (sc_s 2)))
 (let ((?x6986 (bvadd (_ bv62 6) ?x218)))
 (let (($x9254 (bvsle ?x6986 n)))
 (or $x9254 (= (stack_s x_0 x_1 3 n) (stack_s x_0 x_1 2 n)))))))
 ))
 (let ((?x218 (sc_s 2)))
 (let ((?x8766 (bvadd (_ bv63 6) ?x218)))
 (let (($x6914 (= ?x275 ?x8766)))
 (let ((?x2226 (used_gas_s x_0 x_1 3)))
 (let ((?x10292 (bvadd (stack_s x_0 x_1 2 ?x8766) (stack_s x_0 x_1 2 (bvadd (_ bv62 6) ?x218)))))
 (let (($x419 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 1)))) (_ bv0 1)))))
 (let (($x189 (exc_halt_s 1)))
 (let (($x247 (exc_halt_s 2)))
 (let (($x1121 (= $x247 (or $x189 $x419))))
 (let (($x2485 (forall ((w (_ BitVec 256)) )(let ((?x8273 (storage_s x_0 x_1 1 w)))
 (let ((?x4458 (storage_s x_0 x_1 2 w)))
 (= ?x4458 ?x8273))))
 ))
 (let (($x381 (forall ((n (_ BitVec 6)) )(let ((?x154 (sc_s 1)))
 (let (($x2887 (bvsle ?x154 n)))
 (or $x2887 (= (stack_s x_0 x_1 2 n) (stack_s x_0 x_1 1 n))))))
 ))
 (let (($x2550 (= ?x218 (bvadd (_ bv1 6) (sc_s 1)))))
 (let ((?x8926 (used_gas_s x_0 x_1 2)))
 (let (($x2150 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x72))) (_ bv0 1)))))
 (let (($x1744 (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) ?x72)))))
 (let (($x4500 (forall ((w (_ BitVec 256)) )(let ((?x10607 (storage_s x_0 x_1 0 w)))
 (let ((?x8273 (storage_s x_0 x_1 1 w)))
 (= ?x8273 ?x10607))))
 ))
 (let (($x5434 (forall ((n (_ BitVec 6)) )(let ((?x72 (sc_s 0)))
 (let ((?x4584 (bvadd (_ bv62 6) ?x72)))
 (let (($x9591 (bvsle ?x4584 n)))
 (or (= (stack_s x_0 x_1 1 n) (stack_s x_0 x_1 0 n)) $x9591)))))
 ))
 (let ((?x154 (sc_s 1)))
 (let (($x6368 (= ?x154 (bvadd (_ bv1 6) ?x72))))
 (let (($x6720 (= (stack_s x_0 x_1 1 (bvadd (_ bv63 6) ?x72)) (stack_s x_0 x_1 0 (bvadd (_ bv63 6) ?x72)))))
 (let ((?x4584 (bvadd (_ bv62 6) ?x72)))
 (let ((?x10752 (stack_s x_0 x_1 0 ?x4584)))
 (let (($x1748 (forall ((w (_ BitVec 256)) )(let ((?x10607 (storage_s x_0 x_1 0 w)))
 (= ?x10607 (_ bv0 256))))
 ))
 (let (($x2861 (= ?x9397 0)))
 (let (($x11443 (not $x57)))
 (let (($x7132 (= (stack_s x_0 x_1 0 (_ bv1 6)) x_1)))
 (let (($x10388 (= (stack_s x_0 x_1 0 (_ bv0 6)) x_0)))
 (let (($x11128 (= ?x72 (_ bv2 6))))
 (and $x11128 $x10388 $x7132 $x11443 $x2861 $x1748 (= (stack_s x_0 x_1 1 (bvadd (_ bv63 6) ?x154)) ?x10752) (= (stack_s x_0 x_1 1 ?x4584) ?x10752) $x6720 (= (used_gas_s x_0 x_1 1) (+ 3 ?x9397)) $x6368 $x5434 $x4500 (= $x189 (or $x57 $x1744 $x2150)) (= (stack_s x_0 x_1 2 ?x154) (_ bv0 256)) (= ?x8926 (+ 3 (used_gas_s x_0 x_1 1))) $x2550 $x381 $x2485 $x1121 (= ?x6136 ?x10292) (= ?x2226 (+ 3 ?x8926)) $x6914 $x11581 $x9443 $x7282 (= ?x3113 ?x7964) (= (stack_s x_0 x_1 4 ?x1922) ?x7964) (= (stack_s x_0 x_1 4 ?x891) ?x6136) (= ?x3213 (+ 3 ?x2226)) $x6462 $x9266 $x2293 (= $x7172 (or $x292 $x1137 $x4369)) $x8131 (= (stack_s x_0 x_1 5 (bvadd (_ bv62 6) ?x4319)) ?x3113) $x7473 $x11171 $x4644 $x11063 $x705 (= ?x8423 ?x2467) (= (stack_t x_0 x_1 1 ?x6761) ?x2467) (= (used_gas_t x_0 x_1 1) (+ 3 ?x8373)) $x11604 $x9836 $x8556 $x7763 (= (stack_t x_0 x_1 2 (bvadd (_ bv63 6) ?x2714)) ?x2425) (= (stack_t x_0 x_1 2 ?x5437) ?x2425) $x11293 (= (stack_t x_0 x_1 2 ?x1086) ?x8423) $x2632 $x134 $x11318 $x7013 (= $x2163 (or (not (bvsle (_ bv0 6) ?x5437)) $x3508 $x9396)) $x73 $x5475 $x58 $x5838 $x974 (not (and $x10201 $x4992 $x6773 $x7465)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
 )
(check-sat)
(get-proof)