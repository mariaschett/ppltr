; SWAP4 AND SWAP3 SWAP1 SWAP3 => SWAP2 SWAP4 AND
(set-option :produce-proofs true)
; 
(set-info :status unknown)
(declare-fun storage_t ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 256)) (_ BitVec 256))
(declare-fun storage_s ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 256)) (_ BitVec 256))
(declare-fun exc_halt_t (Int) Bool)
(declare-fun exc_halt_s (Int) Bool)
(declare-fun sc_t (Int) (_ BitVec 6))
(declare-fun stack_t ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 6)) (_ BitVec 256))
(declare-fun stack_s ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 6)) (_ BitVec 256))
(declare-fun sc_s (Int) (_ BitVec 6))
(declare-fun used_gas_t ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int) Int)
(declare-fun used_gas_s ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int) Int)
(assert
 (exists ((x_0 (_ BitVec 256)) (x_1 (_ BitVec 256)) (x_2 (_ BitVec 256)) (x_3 (_ BitVec 256)) (x_4 (_ BitVec 256)) )(let (($x2672 (forall ((w (_ BitVec 256)) )(let ((?x7083 (storage_t x_0 x_1 x_2 x_3 x_4 3 w)))
 (let ((?x10297 (storage_s x_0 x_1 x_2 x_3 x_4 5 w)))
 (= ?x10297 ?x7083))))
 ))
 (let (($x6783 (exc_halt_t 3)))
 (let (($x11317 (exc_halt_s 5)))
 (let (($x11613 (= $x11317 $x6783)))
 (let (($x444 (forall ((n (_ BitVec 6)) )(let ((?x3154 (stack_t x_0 x_1 x_2 x_3 x_4 3 n)))
 (let ((?x4633 (stack_s x_0 x_1 x_2 x_3 x_4 5 n)))
 (let (($x9517 (= ?x4633 ?x3154)))
 (or $x9517 (bvsle (sc_t 3) n))))))
 ))
 (let ((?x6438 (sc_t 3)))
 (let ((?x4319 (sc_s 5)))
 (let (($x7281 (= ?x4319 ?x6438)))
 (let ((?x3776 (used_gas_t x_0 x_1 x_2 x_3 x_4 0)))
 (let ((?x533 (used_gas_s x_0 x_1 x_2 x_3 x_4 0)))
 (let (($x6891 (= ?x533 ?x3776)))
 (let (($x6362 (forall ((w (_ BitVec 256)) )(let ((?x2463 (storage_t x_0 x_1 x_2 x_3 x_4 0 w)))
 (let ((?x10181 (storage_s x_0 x_1 x_2 x_3 x_4 0 w)))
 (= ?x10181 ?x2463))))
 ))
 (let (($x56 (exc_halt_t 0)))
 (let (($x57 (exc_halt_s 0)))
 (let (($x58 (= $x57 $x56)))
 (let (($x9472 (forall ((n (_ BitVec 6)) )(let ((?x6554 (stack_t x_0 x_1 x_2 x_3 x_4 0 n)))
 (let ((?x2223 (stack_s x_0 x_1 x_2 x_3 x_4 0 n)))
 (let (($x2293 (= ?x2223 ?x6554)))
 (or $x2293 (bvsle (sc_t 0) n))))))
 ))
 (let ((?x63 (sc_t 0)))
 (let ((?x72 (sc_s 0)))
 (let (($x73 (= ?x72 ?x63)))
 (let (($x5109 (= $x6783 (or (exc_halt_t 2) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_t 2))))))))
 (let (($x5301 (forall ((w (_ BitVec 256)) )(let ((?x8373 (storage_t x_0 x_1 x_2 x_3 x_4 2 w)))
 (let ((?x7083 (storage_t x_0 x_1 x_2 x_3 x_4 3 w)))
 (= ?x7083 ?x8373))))
 ))
 (let (($x6472 (forall ((n (_ BitVec 6)) )(let ((?x7451 (stack_t x_0 x_1 x_2 x_3 x_4 2 n)))
 (let ((?x3154 (stack_t x_0 x_1 x_2 x_3 x_4 3 n)))
 (or (= ?x3154 ?x7451) (bvsle (bvadd (_ bv62 6) (sc_t 2)) n)))))
 ))
 (let (($x3718 (= (used_gas_t x_0 x_1 x_2 x_3 x_4 3) (+ 3 (used_gas_t x_0 x_1 x_2 x_3 x_4 2)))))
 (let ((?x2714 (sc_t 2)))
 (let ((?x1024 (bvadd (_ bv62 6) ?x2714)))
 (let ((?x6721 (stack_t x_0 x_1 x_2 x_3 x_4 2 ?x1024)))
 (let ((?x9896 (bvor (bvnot (stack_t x_0 x_1 x_2 x_3 x_4 2 (bvadd (_ bv63 6) ?x2714))) (bvnot ?x6721))))
 (let (($x5756 (= (stack_t x_0 x_1 x_2 x_3 x_4 3 (bvadd (_ bv63 6) ?x6438)) (bvnot ?x9896))))
 (let (($x5252 (exc_halt_t 2)))
 (let (($x11549 (= $x5252 (or (exc_halt_t 1) (not (bvsle (_ bv0 6) (bvadd (_ bv59 6) (sc_t 1))))))))
 (let (($x9738 (forall ((w (_ BitVec 256)) )(let ((?x5863 (storage_t x_0 x_1 x_2 x_3 x_4 1 w)))
 (let ((?x8373 (storage_t x_0 x_1 x_2 x_3 x_4 2 w)))
 (= ?x8373 ?x5863))))
 ))
 (let (($x10927 (forall ((n (_ BitVec 6)) )(let ((?x2781 (stack_t x_0 x_1 x_2 x_3 x_4 1 n)))
 (let ((?x7451 (stack_t x_0 x_1 x_2 x_3 x_4 2 n)))
 (or (bvsle (bvadd (_ bv59 6) (sc_t 1)) n) (= ?x7451 ?x2781)))))
 ))
 (let ((?x5126 (used_gas_t x_0 x_1 x_2 x_3 x_4 2)))
 (let ((?x10269 (stack_t x_0 x_1 x_2 x_3 x_4 1 (bvadd (_ bv62 6) (sc_t 1)))))
 (let ((?x4677 (stack_t x_0 x_1 x_2 x_3 x_4 1 (bvadd (_ bv61 6) (sc_t 1)))))
 (let (($x9580 (= (stack_t x_0 x_1 x_2 x_3 x_4 2 (bvadd (_ bv60 6) ?x2714)) (stack_t x_0 x_1 x_2 x_3 x_4 1 (bvadd (_ bv60 6) (sc_t 1))))))
 (let ((?x3142 (stack_t x_0 x_1 x_2 x_3 x_4 1 (bvadd (_ bv63 6) (sc_t 1)))))
 (let ((?x9149 (bvadd (_ bv63 6) ?x2714)))
 (let ((?x1463 (stack_t x_0 x_1 x_2 x_3 x_4 2 ?x9149)))
 (let (($x1814 (= ?x1463 (stack_t x_0 x_1 x_2 x_3 x_4 1 (bvadd (_ bv59 6) (sc_t 1))))))
 (let (($x5809 (forall ((w (_ BitVec 256)) )(let ((?x2463 (storage_t x_0 x_1 x_2 x_3 x_4 0 w)))
 (let ((?x5863 (storage_t x_0 x_1 x_2 x_3 x_4 1 w)))
 (= ?x5863 ?x2463))))
 ))
 (let (($x503 (forall ((n (_ BitVec 6)) )(let ((?x6554 (stack_t x_0 x_1 x_2 x_3 x_4 0 n)))
 (let ((?x2781 (stack_t x_0 x_1 x_2 x_3 x_4 1 n)))
 (or (= ?x2781 ?x6554) (bvsle (bvadd (_ bv61 6) (sc_t 0)) n)))))
 ))
 (let (($x10610 (= $x11317 (or (exc_halt_s 4) (not (bvsle (_ bv0 6) (bvadd (_ bv60 6) (sc_s 4))))))))
 (let (($x5738 (forall ((w (_ BitVec 256)) )(let ((?x3456 (storage_s x_0 x_1 x_2 x_3 x_4 4 w)))
 (let ((?x10297 (storage_s x_0 x_1 x_2 x_3 x_4 5 w)))
 (= ?x10297 ?x3456))))
 ))
 (let (($x7899 (forall ((n (_ BitVec 6)) )(let ((?x707 (stack_s x_0 x_1 x_2 x_3 x_4 4 n)))
 (let ((?x4633 (stack_s x_0 x_1 x_2 x_3 x_4 5 n)))
 (or (bvsle (bvadd (_ bv60 6) (sc_s 4)) n) (= ?x4633 ?x707)))))
 ))
 (let (($x8911 (= (used_gas_s x_0 x_1 x_2 x_3 x_4 5) (+ 3 (used_gas_s x_0 x_1 x_2 x_3 x_4 4)))))
 (let ((?x8103 (stack_s x_0 x_1 x_2 x_3 x_4 4 (bvadd (_ bv62 6) (sc_s 4)))))
 (let (($x6129 (= (stack_s x_0 x_1 x_2 x_3 x_4 5 (bvadd (_ bv61 6) ?x4319)) (stack_s x_0 x_1 x_2 x_3 x_4 4 (bvadd (_ bv61 6) (sc_s 4))))))
 (let ((?x2484 (stack_s x_0 x_1 x_2 x_3 x_4 4 (bvadd (_ bv63 6) (sc_s 4)))))
 (let (($x10241 (= (stack_s x_0 x_1 x_2 x_3 x_4 5 (bvadd (_ bv63 6) ?x4319)) (stack_s x_0 x_1 x_2 x_3 x_4 4 (bvadd (_ bv60 6) (sc_s 4))))))
 (let (($x7172 (exc_halt_s 4)))
 (let (($x8085 (= $x7172 (or (exc_halt_s 3) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 3))))))))
 (let (($x1374 (forall ((w (_ BitVec 256)) )(let ((?x3187 (storage_s x_0 x_1 x_2 x_3 x_4 3 w)))
 (let ((?x3456 (storage_s x_0 x_1 x_2 x_3 x_4 4 w)))
 (= ?x3456 ?x3187))))
 ))
 (let (($x8340 (forall ((n (_ BitVec 6)) )(let ((?x3687 (stack_s x_0 x_1 x_2 x_3 x_4 3 n)))
 (let ((?x707 (stack_s x_0 x_1 x_2 x_3 x_4 4 n)))
 (or (bvsle (bvadd (_ bv62 6) (sc_s 3)) n) (= ?x707 ?x3687)))))
 ))
 (let ((?x3707 (used_gas_s x_0 x_1 x_2 x_3 x_4 4)))
 (let ((?x3953 (stack_s x_0 x_1 x_2 x_3 x_4 3 (bvadd (_ bv63 6) (sc_s 3)))))
 (let ((?x275 (sc_s 3)))
 (let ((?x4190 (bvadd (_ bv62 6) ?x275)))
 (let ((?x4186 (stack_s x_0 x_1 x_2 x_3 x_4 3 ?x4190)))
 (let (($x292 (exc_halt_s 3)))
 (let (($x6649 (= $x292 (or (exc_halt_s 2) (not (bvsle (_ bv0 6) (bvadd (_ bv60 6) (sc_s 2))))))))
 (let (($x869 (forall ((w (_ BitVec 256)) )(let ((?x1492 (storage_s x_0 x_1 x_2 x_3 x_4 2 w)))
 (let ((?x3187 (storage_s x_0 x_1 x_2 x_3 x_4 3 w)))
 (= ?x3187 ?x1492))))
 ))
 (let (($x10170 (forall ((n (_ BitVec 6)) )(let ((?x1910 (stack_s x_0 x_1 x_2 x_3 x_4 2 n)))
 (let ((?x3687 (stack_s x_0 x_1 x_2 x_3 x_4 3 n)))
 (or (= ?x3687 ?x1910) (bvsle (bvadd (_ bv60 6) (sc_s 2)) n)))))
 ))
 (let ((?x7298 (used_gas_s x_0 x_1 x_2 x_3 x_4 3)))
 (let (($x10366 (= ?x4186 (stack_s x_0 x_1 x_2 x_3 x_4 2 (bvadd (_ bv62 6) (sc_s 2))))))
 (let (($x8051 (= (stack_s x_0 x_1 x_2 x_3 x_4 3 (bvadd (_ bv61 6) ?x275)) (stack_s x_0 x_1 x_2 x_3 x_4 2 (bvadd (_ bv61 6) (sc_s 2))))))
 (let ((?x9952 (stack_s x_0 x_1 x_2 x_3 x_4 2 (bvadd (_ bv63 6) (sc_s 2)))))
 (let (($x6749 (= ?x3953 (stack_s x_0 x_1 x_2 x_3 x_4 2 (bvadd (_ bv60 6) (sc_s 2))))))
 (let (($x247 (exc_halt_s 2)))
 (let (($x8583 (= $x247 (or (exc_halt_s 1) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 1))))))))
 (let (($x9467 (forall ((w (_ BitVec 256)) )(let ((?x5748 (storage_s x_0 x_1 x_2 x_3 x_4 1 w)))
 (let ((?x1492 (storage_s x_0 x_1 x_2 x_3 x_4 2 w)))
 (= ?x1492 ?x5748))))
 ))
 (let (($x9734 (forall ((n (_ BitVec 6)) )(let ((?x486 (stack_s x_0 x_1 x_2 x_3 x_4 1 n)))
 (let ((?x1910 (stack_s x_0 x_1 x_2 x_3 x_4 2 n)))
 (or (= ?x1910 ?x486) (bvsle (bvadd (_ bv62 6) (sc_s 1)) n)))))
 ))
 (let ((?x2467 (used_gas_s x_0 x_1 x_2 x_3 x_4 2)))
 (let ((?x154 (sc_s 1)))
 (let ((?x9948 (bvadd (_ bv62 6) ?x154)))
 (let ((?x7964 (stack_s x_0 x_1 x_2 x_3 x_4 1 ?x9948)))
 (let ((?x7407 (bvor (bvnot (stack_s x_0 x_1 x_2 x_3 x_4 1 (bvadd (_ bv63 6) ?x154))) (bvnot ?x7964))))
 (let (($x765 (forall ((w (_ BitVec 256)) )(let ((?x10181 (storage_s x_0 x_1 x_2 x_3 x_4 0 w)))
 (let ((?x5748 (storage_s x_0 x_1 x_2 x_3 x_4 1 w)))
 (= ?x5748 ?x10181))))
 ))
 (let (($x9262 (forall ((n (_ BitVec 6)) )(let ((?x2223 (stack_s x_0 x_1 x_2 x_3 x_4 0 n)))
 (let ((?x486 (stack_s x_0 x_1 x_2 x_3 x_4 1 n)))
 (or (= ?x486 ?x2223) (bvsle (bvadd (_ bv59 6) (sc_s 0)) n)))))
 ))
 (let (($x6256 (= (stack_s x_0 x_1 x_2 x_3 x_4 1 (bvadd (_ bv61 6) ?x154)) (stack_s x_0 x_1 x_2 x_3 x_4 0 (bvadd (_ bv61 6) ?x72)))))
 (let (($x9251 (= (stack_s x_0 x_1 x_2 x_3 x_4 1 (bvadd (_ bv60 6) ?x154)) (stack_s x_0 x_1 x_2 x_3 x_4 0 (bvadd (_ bv60 6) ?x72)))))
 (let (($x5367 (= (stack_s x_0 x_1 x_2 x_3 x_4 1 (bvadd (_ bv59 6) ?x154)) (stack_s x_0 x_1 x_2 x_3 x_4 0 (bvadd (_ bv63 6) ?x72)))))
 (let ((?x9355 (bvadd (_ bv63 6) ?x154)))
 (let ((?x4003 (stack_s x_0 x_1 x_2 x_3 x_4 1 ?x9355)))
 (let (($x3006 (forall ((w (_ BitVec 256)) )(let ((?x10181 (storage_s x_0 x_1 x_2 x_3 x_4 0 w)))
 (= ?x10181 (_ bv0 256))))
 ))
 (let (($x3151 (= ?x533 0)))
 (let (($x4225 (= (stack_s x_0 x_1 x_2 x_3 x_4 0 (_ bv4 6)) x_4)))
 (let (($x9350 (= (stack_s x_0 x_1 x_2 x_3 x_4 0 (_ bv3 6)) x_3)))
 (let (($x8806 (= (stack_s x_0 x_1 x_2 x_3 x_4 0 (_ bv2 6)) x_2)))
 (let (($x1219 (= (stack_s x_0 x_1 x_2 x_3 x_4 0 (_ bv1 6)) x_1)))
 (let (($x737 (= (stack_s x_0 x_1 x_2 x_3 x_4 0 (_ bv0 6)) x_0)))
 (let (($x2791 (= ?x72 (_ bv5 6))))
 (and $x2791 $x737 $x1219 $x8806 $x9350 $x4225 (not $x57) $x3151 $x3006 (= ?x4003 (stack_s x_0 x_1 x_2 x_3 x_4 0 (bvadd (_ bv59 6) ?x72))) $x5367 $x9251 $x6256 (= ?x7964 (stack_s x_0 x_1 x_2 x_3 x_4 0 (bvadd (_ bv62 6) ?x72))) (= (used_gas_s x_0 x_1 x_2 x_3 x_4 1) (+ 3 ?x533)) (= ?x154 ?x72) $x9262 $x765 (= (exc_halt_s 1) (or $x57 (not (bvsle (_ bv0 6) (bvadd (_ bv59 6) ?x72))))) (= ?x9952 (bvnot ?x7407)) (= ?x2467 (+ 3 (used_gas_s x_0 x_1 x_2 x_3 x_4 1))) (= (sc_s 2) ?x9355) $x9734 $x9467 $x8583 $x6749 (= (stack_s x_0 x_1 x_2 x_3 x_4 3 (bvadd (_ bv60 6) ?x275)) ?x9952) $x8051 $x10366 (= ?x7298 (+ 3 ?x2467)) (= ?x275 (sc_s 2)) $x10170 $x869 $x6649 (= ?x2484 ?x4186) (= ?x8103 ?x3953) (= ?x3707 (+ 3 ?x7298)) (= (sc_s 4) ?x275) $x8340 $x1374 $x8085 $x10241 (= (stack_s x_0 x_1 x_2 x_3 x_4 5 (bvadd (_ bv60 6) ?x4319)) ?x2484) $x6129 (= (stack_s x_0 x_1 x_2 x_3 x_4 5 (bvadd (_ bv62 6) ?x4319)) ?x8103) $x8911 (= ?x4319 (sc_s 4)) $x7899 $x5738 $x10610 (= ?x3142 (stack_t x_0 x_1 x_2 x_3 x_4 0 (bvadd (_ bv61 6) ?x63))) (= ?x4677 (stack_t x_0 x_1 x_2 x_3 x_4 0 (bvadd (_ bv63 6) ?x63))) (= ?x10269 (stack_t x_0 x_1 x_2 x_3 x_4 0 (bvadd (_ bv62 6) ?x63))) (= (used_gas_t x_0 x_1 x_2 x_3 x_4 1) (+ 3 ?x3776)) (= (sc_t 1) ?x63) $x503 $x5809 (= (exc_halt_t 1) (or $x56 (not (bvsle (_ bv0 6) (bvadd (_ bv61 6) ?x63))))) $x1814 (= (stack_t x_0 x_1 x_2 x_3 x_4 2 (bvadd (_ bv59 6) ?x2714)) ?x3142) $x9580 (= (stack_t x_0 x_1 x_2 x_3 x_4 2 (bvadd (_ bv61 6) ?x2714)) ?x4677) (= ?x6721 ?x10269) (= ?x5126 (+ 3 (used_gas_t x_0 x_1 x_2 x_3 x_4 1))) (= ?x2714 (sc_t 1)) $x10927 $x9738 $x11549 $x5756 $x3718 (= ?x6438 ?x9149) $x6472 $x5301 $x5109 $x73 $x9472 $x58 $x6362 $x6891 (not (and $x7281 $x444 $x11613 $x2672))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
 )
(check-sat)
(get-proof)
