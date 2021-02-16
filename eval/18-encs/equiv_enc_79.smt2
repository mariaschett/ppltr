; PUSH cw_2 DUP1 POP CALLER PUSH cw_2 => PUSH cw_2 CALLER DUP2
(set-option :produce-proofs true)
; 
(set-info :status unknown)
(declare-fun storage_t ((_ BitVec 256) (_ BitVec 256) Int (_ BitVec 256)) (_ BitVec 256))
(declare-fun w_2 () (_ BitVec 256))
(declare-fun storage_s ((_ BitVec 256) (_ BitVec 256) Int (_ BitVec 256)) (_ BitVec 256))
(declare-fun exc_halt_t (Int) Bool)
(declare-fun exc_halt_s (Int) Bool)
(declare-fun sc_t (Int) (_ BitVec 6))
(declare-fun stack_t ((_ BitVec 256) (_ BitVec 256) Int (_ BitVec 6)) (_ BitVec 256))
(declare-fun stack_s ((_ BitVec 256) (_ BitVec 256) Int (_ BitVec 6)) (_ BitVec 256))
(declare-fun sc_s (Int) (_ BitVec 6))
(declare-fun used_gas_t ((_ BitVec 256) (_ BitVec 256) Int) Int)
(declare-fun used_gas_s ((_ BitVec 256) (_ BitVec 256) Int) Int)
(assert
 (exists ((x_CALLER (_ BitVec 256)) )(let (($x7594 (forall ((w (_ BitVec 256)) )(let ((?x1019 (storage_t w_2 x_CALLER 3 w)))
 (let ((?x1812 (storage_s w_2 x_CALLER 5 w)))
 (= ?x1812 ?x1019))))
 ))
 (let (($x10336 (exc_halt_t 3)))
 (let (($x3979 (exc_halt_s 5)))
 (let (($x5604 (= $x3979 $x10336)))
 (let (($x7204 (forall ((n (_ BitVec 6)) )(let ((?x2012 (sc_t 3)))
 (let (($x8942 (bvsle ?x2012 n)))
 (let ((?x8537 (stack_t w_2 x_CALLER 3 n)))
 (let ((?x6934 (stack_s w_2 x_CALLER 5 n)))
 (let (($x5513 (= ?x6934 ?x8537)))
 (or $x5513 $x8942)))))))
 ))
 (let ((?x2012 (sc_t 3)))
 (let ((?x805 (sc_s 5)))
 (let (($x2122 (= ?x805 ?x2012)))
 (let ((?x9221 (used_gas_t w_2 x_CALLER 0)))
 (let ((?x9531 (used_gas_s w_2 x_CALLER 0)))
 (let (($x6683 (= ?x9531 ?x9221)))
 (let (($x1133 (forall ((w (_ BitVec 256)) )(let ((?x6669 (storage_t w_2 x_CALLER 0 w)))
 (let ((?x6690 (storage_s w_2 x_CALLER 0 w)))
 (= ?x6690 ?x6669))))
 ))
 (let (($x56 (exc_halt_t 0)))
 (let (($x57 (exc_halt_s 0)))
 (let (($x58 (= $x57 $x56)))
 (let (($x10370 (forall ((n (_ BitVec 6)) )(let ((?x6269 (stack_t w_2 x_CALLER 0 n)))
 (let ((?x1592 (stack_s w_2 x_CALLER 0 n)))
 (let (($x7957 (= ?x1592 ?x6269)))
 (let ((?x63 (sc_t 0)))
 (let (($x7266 (bvsle ?x63 n)))
 (or $x7266 $x7957)))))))
 ))
 (let ((?x63 (sc_t 0)))
 (let ((?x72 (sc_s 0)))
 (let (($x73 (= ?x72 ?x63)))
 (let (($x2496 (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_t 2))))))
 (let (($x2326 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_t 2)))) (_ bv0 1)))))
 (let (($x903 (exc_halt_t 2)))
 (let (($x10278 (= $x10336 (or $x903 $x2326 $x2496))))
 (let (($x1590 (forall ((w (_ BitVec 256)) )(let ((?x7826 (storage_t w_2 x_CALLER 2 w)))
 (let ((?x1019 (storage_t w_2 x_CALLER 3 w)))
 (= ?x1019 ?x7826))))
 ))
 (let (($x10439 (forall ((n (_ BitVec 6)) )(let ((?x9057 (stack_t w_2 x_CALLER 2 n)))
 (let ((?x8537 (stack_t w_2 x_CALLER 3 n)))
 (let (($x9276 (= ?x8537 ?x9057)))
 (let ((?x4056 (sc_t 2)))
 (let ((?x8342 (bvadd (_ bv62 6) ?x4056)))
 (let (($x1004 (bvsle ?x8342 n)))
 (or $x1004 $x9276))))))))
 ))
 (let (($x8655 (= ?x2012 (bvadd (_ bv1 6) (sc_t 2)))))
 (let (($x6563 (= (used_gas_t w_2 x_CALLER 3) (+ 3 (used_gas_t w_2 x_CALLER 2)))))
 (let ((?x4056 (sc_t 2)))
 (let ((?x2158 (bvadd (_ bv63 6) ?x4056)))
 (let ((?x6821 (stack_t w_2 x_CALLER 2 ?x2158)))
 (let ((?x8342 (bvadd (_ bv62 6) ?x4056)))
 (let ((?x10379 (stack_t w_2 x_CALLER 2 ?x8342)))
 (let (($x1881 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_t 1)))) (_ bv0 1)))))
 (let (($x1920 (exc_halt_t 1)))
 (let (($x1699 (= $x903 (or $x1920 $x1881))))
 (let (($x1728 (forall ((w (_ BitVec 256)) )(let ((?x6481 (storage_t w_2 x_CALLER 1 w)))
 (let ((?x7826 (storage_t w_2 x_CALLER 2 w)))
 (= ?x7826 ?x6481))))
 ))
 (let (($x7386 (forall ((n (_ BitVec 6)) )(let ((?x4023 (sc_t 1)))
 (let (($x2724 (bvsle ?x4023 n)))
 (or $x2724 (= (stack_t w_2 x_CALLER 2 n) (stack_t w_2 x_CALLER 1 n))))))
 ))
 (let (($x2343 (= ?x4056 (bvadd (_ bv1 6) (sc_t 1)))))
 (let ((?x181 (used_gas_t w_2 x_CALLER 2)))
 (let (($x3239 (= $x1920 (or $x56 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x63))) (_ bv0 1)))))))
 (let (($x9729 (forall ((w (_ BitVec 256)) )(let ((?x6669 (storage_t w_2 x_CALLER 0 w)))
 (let ((?x6481 (storage_t w_2 x_CALLER 1 w)))
 (= ?x6481 ?x6669))))
 ))
 (let (($x6861 (forall ((n (_ BitVec 6)) )(let ((?x63 (sc_t 0)))
 (let (($x7266 (bvsle ?x63 n)))
 (or $x7266 (= (stack_t w_2 x_CALLER 1 n) (stack_t w_2 x_CALLER 0 n))))))
 ))
 (let ((?x4023 (sc_t 1)))
 (let (($x9398 (= ?x4023 (bvadd (_ bv1 6) ?x63))))
 (let ((?x1136 (used_gas_t w_2 x_CALLER 1)))
 (let (($x939 (= ?x1136 (+ 3 ?x9221))))
 (let (($x2395 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 4)))) (_ bv0 1)))))
 (let (($x64 (exc_halt_s 4)))
 (let (($x2678 (= $x3979 (or $x64 $x2395))))
 (let (($x1793 (forall ((w (_ BitVec 256)) )(let ((?x1610 (storage_s w_2 x_CALLER 4 w)))
 (let ((?x1812 (storage_s w_2 x_CALLER 5 w)))
 (= ?x1812 ?x1610))))
 ))
 (let (($x8159 (forall ((n (_ BitVec 6)) )(let ((?x4305 (sc_s 4)))
 (let (($x8533 (bvsle ?x4305 n)))
 (let ((?x1569 (stack_s w_2 x_CALLER 4 n)))
 (let ((?x6934 (stack_s w_2 x_CALLER 5 n)))
 (let (($x7336 (= ?x6934 ?x1569)))
 (or $x7336 $x8533)))))))
 ))
 (let (($x2789 (= ?x805 (bvadd (_ bv1 6) (sc_s 4)))))
 (let ((?x6497 (used_gas_s w_2 x_CALLER 5)))
 (let (($x1789 (= ?x6497 (+ 3 (used_gas_s w_2 x_CALLER 4)))))
 (let (($x2793 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 3)))) (_ bv0 1)))))
 (let (($x292 (exc_halt_s 3)))
 (let (($x8947 (= $x64 (or $x292 $x2793))))
 (let (($x6287 (forall ((w (_ BitVec 256)) )(let ((?x1707 (storage_s w_2 x_CALLER 3 w)))
 (let ((?x1610 (storage_s w_2 x_CALLER 4 w)))
 (= ?x1610 ?x1707))))
 ))
 (let (($x10353 (forall ((n (_ BitVec 6)) )(let ((?x275 (sc_s 3)))
 (let (($x9161 (bvsle ?x275 n)))
 (let ((?x7933 (stack_s w_2 x_CALLER 3 n)))
 (let ((?x1569 (stack_s w_2 x_CALLER 4 n)))
 (let (($x7551 (= ?x1569 ?x7933)))
 (or $x7551 $x9161)))))))
 ))
 (let ((?x4305 (sc_s 4)))
 (let (($x8496 (= ?x4305 (bvadd (_ bv1 6) (sc_s 3)))))
 (let ((?x6951 (used_gas_s w_2 x_CALLER 4)))
 (let (($x4327 (= $x292 (or (exc_halt_s 2) (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_s 2))))))))
 (let (($x7315 (forall ((w (_ BitVec 256)) )(let ((?x8833 (storage_s w_2 x_CALLER 2 w)))
 (let ((?x1707 (storage_s w_2 x_CALLER 3 w)))
 (= ?x1707 ?x8833))))
 ))
 (let (($x7208 (forall ((n (_ BitVec 6)) )(let ((?x218 (sc_s 2)))
 (let ((?x3563 (bvadd (_ bv63 6) ?x218)))
 (let (($x8136 (bvsle ?x3563 n)))
 (or $x8136 (= (stack_s w_2 x_CALLER 3 n) (stack_s w_2 x_CALLER 2 n)))))))
 ))
 (let ((?x218 (sc_s 2)))
 (let ((?x3563 (bvadd (_ bv63 6) ?x218)))
 (let ((?x275 (sc_s 3)))
 (let (($x8134 (= ?x275 ?x3563)))
 (let ((?x1559 (used_gas_s w_2 x_CALLER 3)))
 (let (($x8796 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 1)))) (_ bv0 1)))))
 (let (($x8428 (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_s 1))))))
 (let (($x189 (exc_halt_s 1)))
 (let (($x247 (exc_halt_s 2)))
 (let (($x2921 (= $x247 (or $x189 $x8428 $x8796))))
 (let (($x6761 (forall ((w (_ BitVec 256)) )(let ((?x7781 (storage_s w_2 x_CALLER 1 w)))
 (let ((?x8833 (storage_s w_2 x_CALLER 2 w)))
 (= ?x8833 ?x7781))))
 ))
 (let (($x6402 (forall ((n (_ BitVec 6)) )(let ((?x154 (sc_s 1)))
 (let ((?x3584 (bvadd (_ bv63 6) ?x154)))
 (let (($x3272 (bvsle ?x3584 n)))
 (or (= (stack_s w_2 x_CALLER 2 n) (stack_s w_2 x_CALLER 1 n)) $x3272)))))
 ))
 (let (($x6573 (= ?x218 (bvadd (_ bv1 6) (sc_s 1)))))
 (let ((?x7758 (used_gas_s w_2 x_CALLER 2)))
 (let (($x1841 (= ?x7758 (+ 3 (used_gas_s w_2 x_CALLER 1)))))
 (let ((?x154 (sc_s 1)))
 (let ((?x3584 (bvadd (_ bv63 6) ?x154)))
 (let ((?x8984 (stack_s w_2 x_CALLER 1 ?x3584)))
 (let (($x7947 (= $x189 (or $x57 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x72))) (_ bv0 1)))))))
 (let (($x1711 (forall ((w (_ BitVec 256)) )(let ((?x6690 (storage_s w_2 x_CALLER 0 w)))
 (let ((?x7781 (storage_s w_2 x_CALLER 1 w)))
 (= ?x7781 ?x6690))))
 ))
 (let (($x10337 (forall ((n (_ BitVec 6)) )(let ((?x72 (sc_s 0)))
 (let (($x8308 (bvsle ?x72 n)))
 (or $x8308 (= (stack_s w_2 x_CALLER 1 n) (stack_s w_2 x_CALLER 0 n))))))
 ))
 (let (($x3028 (= ?x154 (bvadd (_ bv1 6) ?x72))))
 (let ((?x2858 (used_gas_s w_2 x_CALLER 1)))
 (let (($x5481 (= ?x2858 (+ 3 ?x9531))))
 (let (($x6246 (= (stack_s w_2 x_CALLER 1 ?x72) w_2)))
 (let (($x9048 (forall ((w (_ BitVec 256)) )(let ((?x6690 (storage_s w_2 x_CALLER 0 w)))
 (= ?x6690 (_ bv0 256))))
 ))
 (let (($x9080 (= ?x9531 0)))
 (let (($x3253 (not $x57)))
 (let (($x136 (= ?x72 (_ bv0 6))))
 (and $x136 $x3253 $x9080 $x9048 $x6246 $x5481 $x3028 $x10337 $x1711 $x7947 (= (stack_s w_2 x_CALLER 2 ?x3563) ?x8984) (= (stack_s w_2 x_CALLER 2 ?x3584) ?x8984) $x1841 $x6573 $x6402 $x6761 $x2921 (= ?x1559 (+ 2 ?x7758)) $x8134 $x7208 $x7315 $x4327 (= (stack_s w_2 x_CALLER 4 ?x275) x_CALLER) (= ?x6951 (+ 2 ?x1559)) $x8496 $x10353 $x6287 $x8947 (= (stack_s w_2 x_CALLER 5 ?x4305) w_2) $x1789 $x2789 $x8159 $x1793 $x2678 (= (stack_t w_2 x_CALLER 1 ?x63) w_2) $x939 $x9398 $x6861 $x9729 $x3239 (= (stack_t w_2 x_CALLER 2 ?x4023) x_CALLER) (= ?x181 (+ 2 ?x1136)) $x2343 $x7386 $x1728 $x1699 (= (stack_t w_2 x_CALLER 3 (bvadd (_ bv63 6) ?x2012)) ?x10379) (= (stack_t w_2 x_CALLER 3 ?x8342) ?x10379) (= (stack_t w_2 x_CALLER 3 ?x2158) ?x6821) $x6563 $x8655 $x10439 $x1590 $x10278 $x73 $x10370 $x58 $x1133 $x6683 (not (and $x2122 $x7204 $x5604 $x7594))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
 )
(check-sat)
(get-proof)