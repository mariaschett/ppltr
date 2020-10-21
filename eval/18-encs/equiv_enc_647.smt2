; CALLER SWAP1 POP PUSH 0x00 DUP1 SWAP1 => POP CALLER PUSH 0x00 PUSH 0x00
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
 (exists ((x_0 (_ BitVec 256)) (x_CALLER (_ BitVec 256)) )(let (($x2513 (forall ((w (_ BitVec 256)) )(let ((?x11271 (storage_t x_0 x_CALLER 4 w)))
 (let ((?x8332 (storage_s x_0 x_CALLER 6 w)))
 (= ?x8332 ?x11271))))
 ))
 (let (($x7854 (exc_halt_t 4)))
 (let (($x772 (exc_halt_s 6)))
 (let (($x4404 (= $x772 $x7854)))
 (let (($x8368 (forall ((n (_ BitVec 6)) )(let ((?x2156 (stack_t x_0 x_CALLER 4 n)))
 (let ((?x953 (stack_s x_0 x_CALLER 6 n)))
 (let (($x560 (= ?x953 ?x2156)))
 (let ((?x4818 (sc_t 4)))
 (let (($x531 (bvsle ?x4818 n)))
 (or $x531 $x560)))))))
 ))
 (let ((?x4818 (sc_t 4)))
 (let ((?x926 (sc_s 6)))
 (let (($x7899 (= ?x926 ?x4818)))
 (let ((?x5103 (used_gas_t x_0 x_CALLER 0)))
 (let ((?x9021 (used_gas_s x_0 x_CALLER 0)))
 (let (($x9847 (= ?x9021 ?x5103)))
 (let (($x1767 (forall ((w (_ BitVec 256)) )(let ((?x11251 (storage_t x_0 x_CALLER 0 w)))
 (let ((?x10188 (storage_s x_0 x_CALLER 0 w)))
 (= ?x10188 ?x11251))))
 ))
 (let (($x56 (exc_halt_t 0)))
 (let (($x57 (exc_halt_s 0)))
 (let (($x58 (= $x57 $x56)))
 (let (($x2218 (forall ((n (_ BitVec 6)) )(let ((?x63 (sc_t 0)))
 (let (($x11489 (bvsle ?x63 n)))
 (let ((?x5181 (stack_t x_0 x_CALLER 0 n)))
 (let ((?x11182 (stack_s x_0 x_CALLER 0 n)))
 (let (($x1859 (= ?x11182 ?x5181)))
 (or $x1859 $x11489)))))))
 ))
 (let ((?x63 (sc_t 0)))
 (let ((?x72 (sc_s 0)))
 (let (($x73 (= ?x72 ?x63)))
 (let (($x3614 (exc_halt_t 3)))
 (let (($x7067 (or $x3614 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_t 3)))) (_ bv0 1))))))
 (let (($x1777 (= $x7854 $x7067)))
 (let (($x4624 (forall ((w (_ BitVec 256)) )(let ((?x4232 (storage_t x_0 x_CALLER 3 w)))
 (let ((?x11271 (storage_t x_0 x_CALLER 4 w)))
 (= ?x11271 ?x4232))))
 ))
 (let (($x9456 (forall ((n (_ BitVec 6)) )(let ((?x11304 (sc_t 3)))
 (let (($x5286 (bvsle ?x11304 n)))
 (or $x5286 (= (stack_t x_0 x_CALLER 4 n) (stack_t x_0 x_CALLER 3 n))))))
 ))
 (let (($x5791 (= ?x4818 (bvadd (_ bv1 6) (sc_t 3)))))
 (let (($x1686 (= (used_gas_t x_0 x_CALLER 4) (+ 3 (used_gas_t x_0 x_CALLER 3)))))
 (let (($x8115 (exc_halt_t 2)))
 (let (($x9562 (or $x8115 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_t 2)))) (_ bv0 1))))))
 (let (($x5357 (= $x3614 $x9562)))
 (let (($x5468 (forall ((w (_ BitVec 256)) )(let ((?x1284 (storage_t x_0 x_CALLER 2 w)))
 (let ((?x4232 (storage_t x_0 x_CALLER 3 w)))
 (= ?x4232 ?x1284))))
 ))
 (let (($x5806 (forall ((n (_ BitVec 6)) )(let ((?x6158 (sc_t 2)))
 (let (($x10359 (bvsle ?x6158 n)))
 (or $x10359 (= (stack_t x_0 x_CALLER 3 n) (stack_t x_0 x_CALLER 2 n))))))
 ))
 (let ((?x11304 (sc_t 3)))
 (let (($x1640 (= ?x11304 (bvadd (_ bv1 6) (sc_t 2)))))
 (let ((?x7292 (used_gas_t x_0 x_CALLER 3)))
 (let (($x6160 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_t 1)))) (_ bv0 1)))))
 (let (($x8377 (exc_halt_t 1)))
 (let (($x11320 (= $x8115 (or $x8377 $x6160))))
 (let (($x10194 (forall ((w (_ BitVec 256)) )(let ((?x1036 (storage_t x_0 x_CALLER 1 w)))
 (let ((?x1284 (storage_t x_0 x_CALLER 2 w)))
 (= ?x1284 ?x1036))))
 ))
 (let (($x11481 (forall ((n (_ BitVec 6)) )(let ((?x7154 (sc_t 1)))
 (let (($x3342 (bvsle ?x7154 n)))
 (or (= (stack_t x_0 x_CALLER 2 n) (stack_t x_0 x_CALLER 1 n)) $x3342))))
 ))
 (let ((?x6158 (sc_t 2)))
 (let (($x9815 (= ?x6158 (bvadd (_ bv1 6) (sc_t 1)))))
 (let ((?x1599 (used_gas_t x_0 x_CALLER 2)))
 (let (($x6503 (= ?x1599 (+ 2 (used_gas_t x_0 x_CALLER 1)))))
 (let (($x9297 (= (stack_t x_0 x_CALLER 2 (sc_t 1)) x_CALLER)))
 (let (($x3592 (= $x8377 (or $x56 (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) ?x63)))))))
 (let (($x6109 (forall ((w (_ BitVec 256)) )(let ((?x11251 (storage_t x_0 x_CALLER 0 w)))
 (let ((?x1036 (storage_t x_0 x_CALLER 1 w)))
 (= ?x1036 ?x11251))))
 ))
 (let (($x4897 (forall ((n (_ BitVec 6)) )(let ((?x5181 (stack_t x_0 x_CALLER 0 n)))
 (let ((?x4639 (stack_t x_0 x_CALLER 1 n)))
 (let (($x9350 (= ?x4639 ?x5181)))
 (let ((?x63 (sc_t 0)))
 (let ((?x3660 (bvadd (_ bv63 6) ?x63)))
 (let (($x7941 (bvsle ?x3660 n)))
 (or $x7941 $x9350))))))))
 ))
 (let ((?x380 (used_gas_t x_0 x_CALLER 1)))
 (let (($x3003 (= ?x380 (+ 2 ?x5103))))
 (let (($x914 (= $x772 (or (exc_halt_s 5) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 5))))))))
 (let (($x7471 (forall ((w (_ BitVec 256)) )(let ((?x1762 (storage_s x_0 x_CALLER 5 w)))
 (let ((?x8332 (storage_s x_0 x_CALLER 6 w)))
 (= ?x8332 ?x1762))))
 ))
 (let (($x6950 (forall ((n (_ BitVec 6)) )(or (bvsle (bvadd (_ bv62 6) (sc_s 5)) n) (= (stack_s x_0 x_CALLER 6 n) (stack_s x_0 x_CALLER 5 n))))
 ))
 (let (($x2593 (= (used_gas_s x_0 x_CALLER 6) (+ 3 (used_gas_s x_0 x_CALLER 5)))))
 (let ((?x4319 (sc_s 5)))
 (let ((?x1539 (bvadd (_ bv63 6) ?x4319)))
 (let ((?x10002 (stack_s x_0 x_CALLER 5 ?x1539)))
 (let (($x41 (= (stack_s x_0 x_CALLER 6 (bvadd (_ bv63 6) ?x926)) (stack_s x_0 x_CALLER 5 (bvadd (_ bv62 6) ?x4319)))))
 (let (($x7172 (exc_halt_s 4)))
 (let (($x8049 (or (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 4)))) (_ bv0 1))) $x7172 (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_s 4)))))))
 (let (($x11317 (exc_halt_s 5)))
 (let (($x6875 (= $x11317 $x8049)))
 (let (($x8239 (forall ((w (_ BitVec 256)) )(let ((?x5202 (storage_s x_0 x_CALLER 4 w)))
 (let ((?x1762 (storage_s x_0 x_CALLER 5 w)))
 (= ?x1762 ?x5202))))
 ))
 (let (($x736 (forall ((n (_ BitVec 6)) )(or (= (stack_s x_0 x_CALLER 5 n) (stack_s x_0 x_CALLER 4 n)) (bvsle (bvadd (_ bv63 6) (sc_s 4)) n)))
 ))
 (let (($x10051 (= ?x4319 (bvadd (_ bv1 6) (sc_s 4)))))
 (let ((?x5331 (used_gas_s x_0 x_CALLER 5)))
 (let (($x11052 (= ?x5331 (+ 3 (used_gas_s x_0 x_CALLER 4)))))
 (let ((?x4305 (sc_s 4)))
 (let ((?x2899 (bvadd (_ bv63 6) ?x4305)))
 (let ((?x7438 (stack_s x_0 x_CALLER 4 ?x2899)))
 (let (($x9509 (= (stack_s x_0 x_CALLER 5 ?x2899) ?x7438)))
 (let (($x7551 (= ?x10002 ?x7438)))
 (let (($x5855 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 3)))) (_ bv0 1)))))
 (let (($x292 (exc_halt_s 3)))
 (let (($x5835 (forall ((w (_ BitVec 256)) )(let ((?x1549 (storage_s x_0 x_CALLER 3 w)))
 (let ((?x5202 (storage_s x_0 x_CALLER 4 w)))
 (= ?x5202 ?x1549))))
 ))
 (let (($x11475 (forall ((n (_ BitVec 6)) )(let ((?x7341 (stack_s x_0 x_CALLER 3 n)))
 (let ((?x8752 (stack_s x_0 x_CALLER 4 n)))
 (let (($x3381 (= ?x8752 ?x7341)))
 (or $x3381 (bvsle (sc_s 3) n))))))
 ))
 (let (($x3643 (= ?x4305 (bvadd (_ bv1 6) (sc_s 3)))))
 (let ((?x6826 (used_gas_s x_0 x_CALLER 4)))
 (let (($x10211 (= $x292 (or (exc_halt_s 2) (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_s 2))))))))
 (let (($x6818 (forall ((w (_ BitVec 256)) )(let ((?x9432 (storage_s x_0 x_CALLER 2 w)))
 (let ((?x1549 (storage_s x_0 x_CALLER 3 w)))
 (= ?x1549 ?x9432))))
 ))
 (let (($x8687 (forall ((n (_ BitVec 6)) )(let ((?x218 (sc_s 2)))
 (let ((?x4018 (bvadd (_ bv63 6) ?x218)))
 (let (($x6278 (bvsle ?x4018 n)))
 (let ((?x534 (stack_s x_0 x_CALLER 2 n)))
 (let ((?x7341 (stack_s x_0 x_CALLER 3 n)))
 (let (($x3649 (= ?x7341 ?x534)))
 (or $x3649 $x6278))))))))
 ))
 (let ((?x218 (sc_s 2)))
 (let ((?x4018 (bvadd (_ bv63 6) ?x218)))
 (let ((?x275 (sc_s 3)))
 (let (($x8462 (= ?x275 ?x4018)))
 (let ((?x5436 (used_gas_s x_0 x_CALLER 3)))
 (let (($x247 (exc_halt_s 2)))
 (let (($x9840 (= $x247 (or (exc_halt_s 1) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 1))))))))
 (let (($x6397 (forall ((w (_ BitVec 256)) )(let ((?x10998 (storage_s x_0 x_CALLER 1 w)))
 (let ((?x9432 (storage_s x_0 x_CALLER 2 w)))
 (= ?x9432 ?x10998))))
 ))
 (let (($x3992 (forall ((n (_ BitVec 6)) )(let ((?x154 (sc_s 1)))
 (let ((?x9584 (bvadd (_ bv62 6) ?x154)))
 (let (($x1345 (bvsle ?x9584 n)))
 (let ((?x1612 (stack_s x_0 x_CALLER 1 n)))
 (let ((?x534 (stack_s x_0 x_CALLER 2 n)))
 (let (($x958 (= ?x534 ?x1612)))
 (or $x958 $x1345))))))))
 ))
 (let ((?x154 (sc_s 1)))
 (let (($x4858 (= ?x218 ?x154)))
 (let ((?x5000 (used_gas_s x_0 x_CALLER 2)))
 (let ((?x1096 (bvadd (_ bv63 6) ?x154)))
 (let ((?x6699 (stack_s x_0 x_CALLER 1 ?x1096)))
 (let ((?x1898 (bvadd (_ bv62 6) ?x218)))
 (let ((?x10726 (stack_s x_0 x_CALLER 2 ?x1898)))
 (let ((?x4699 (stack_s x_0 x_CALLER 2 ?x4018)))
 (let (($x189 (exc_halt_s 1)))
 (let (($x9826 (= $x189 (or $x57 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x72))) (_ bv0 1)))))))
 (let (($x8062 (forall ((w (_ BitVec 256)) )(let ((?x10188 (storage_s x_0 x_CALLER 0 w)))
 (let ((?x10998 (storage_s x_0 x_CALLER 1 w)))
 (= ?x10998 ?x10188))))
 ))
 (let (($x2480 (forall ((n (_ BitVec 6)) )(let ((?x11182 (stack_s x_0 x_CALLER 0 n)))
 (let ((?x1612 (stack_s x_0 x_CALLER 1 n)))
 (let (($x11513 (= ?x1612 ?x11182)))
 (let ((?x72 (sc_s 0)))
 (let (($x4372 (bvsle ?x72 n)))
 (or $x4372 $x11513)))))))
 ))
 (let (($x3182 (= ?x154 (bvadd (_ bv1 6) ?x72))))
 (let (($x6669 (forall ((w (_ BitVec 256)) )(let ((?x10188 (storage_s x_0 x_CALLER 0 w)))
 (= ?x10188 (_ bv0 256))))
 ))
 (let (($x9040 (= ?x9021 0)))
 (let (($x9420 (not $x57)))
 (let (($x10547 (= (stack_s x_0 x_CALLER 0 (_ bv0 6)) x_0)))
 (let (($x7315 (= ?x72 (_ bv1 6))))
 (and $x7315 $x10547 $x9420 $x9040 $x6669 (= (stack_s x_0 x_CALLER 1 ?x72) x_CALLER) (= (used_gas_s x_0 x_CALLER 1) (+ 2 ?x9021)) $x3182 $x2480 $x8062 $x9826 (= ?x4699 (stack_s x_0 x_CALLER 1 (bvadd (_ bv62 6) ?x154))) (= ?x10726 ?x6699) (= ?x5000 (+ 3 (used_gas_s x_0 x_CALLER 1))) $x4858 $x3992 $x6397 $x9840 (= ?x5436 (+ 2 ?x5000)) $x8462 $x8687 $x6818 $x10211 (= (stack_s x_0 x_CALLER 4 ?x275) (_ bv0 256)) (= ?x6826 (+ 3 ?x5436)) $x3643 $x11475 $x5835 (= $x7172 (or $x292 $x5855)) $x7551 $x9509 $x11052 $x10051 $x736 $x8239 $x6875 $x41 (= (stack_s x_0 x_CALLER 6 (bvadd (_ bv62 6) ?x926)) ?x10002) $x2593 (= ?x926 ?x4319) $x6950 $x7471 $x914 $x3003 (= (sc_t 1) (bvadd (_ bv63 6) ?x63)) $x4897 $x6109 $x3592 $x9297 $x6503 $x9815 $x11481 $x10194 $x11320 (= (stack_t x_0 x_CALLER 3 ?x6158) (_ bv0 256)) (= ?x7292 (+ 3 ?x1599)) $x1640 $x5806 $x5468 $x5357 (= (stack_t x_0 x_CALLER 4 ?x11304) (_ bv0 256)) $x1686 $x5791 $x9456 $x4624 $x1777 $x73 $x2218 $x58 $x1767 $x9847 (not (and $x7899 $x8368 $x4404 $x2513)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
 )
(check-sat)
(get-proof)
