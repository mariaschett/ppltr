; DUP2 DUP1 MLOAD PUSH cw_2 SWAP2 => PUSH cw_2 DUP3 MLOAD DUP4
(set-option :produce-proofs true)
; 
(set-info :status unknown)
(declare-fun storage_t ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 256)) (_ BitVec 256))
(declare-fun w_2 () (_ BitVec 256))
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
 (exists ((x_0 (_ BitVec 256)) (x_1 (_ BitVec 256)) (x_MLOAD_0 (_ BitVec 256)) )(let (($x2738 (forall ((w (_ BitVec 256)) )(let ((?x1918 (storage_t x_0 x_1 w_2 x_MLOAD_0 4 w)))
 (let ((?x8595 (storage_s x_0 x_1 w_2 x_MLOAD_0 5 w)))
 (= ?x8595 ?x1918))))
 ))
 (let (($x3723 (exc_halt_t 4)))
 (let (($x3979 (exc_halt_s 5)))
 (let (($x7567 (= $x3979 $x3723)))
 (let (($x3382 (forall ((n (_ BitVec 6)) )(let ((?x8529 (stack_t x_0 x_1 w_2 x_MLOAD_0 4 n)))
 (let ((?x1702 (stack_s x_0 x_1 w_2 x_MLOAD_0 5 n)))
 (let (($x1615 (= ?x1702 ?x8529)))
 (or $x1615 (bvsle (sc_t 4) n))))))
 ))
 (let ((?x3757 (sc_t 4)))
 (let ((?x805 (sc_s 5)))
 (let (($x9143 (= ?x805 ?x3757)))
 (let ((?x2063 (used_gas_t x_0 x_1 w_2 x_MLOAD_0 0)))
 (let ((?x2524 (used_gas_s x_0 x_1 w_2 x_MLOAD_0 0)))
 (let (($x7008 (= ?x2524 ?x2063)))
 (let (($x5839 (forall ((w (_ BitVec 256)) )(let ((?x5369 (storage_t x_0 x_1 w_2 x_MLOAD_0 0 w)))
 (let ((?x8649 (storage_s x_0 x_1 w_2 x_MLOAD_0 0 w)))
 (= ?x8649 ?x5369))))
 ))
 (let (($x56 (exc_halt_t 0)))
 (let (($x57 (exc_halt_s 0)))
 (let (($x58 (= $x57 $x56)))
 (let (($x7093 (forall ((n (_ BitVec 6)) )(let ((?x7516 (stack_t x_0 x_1 w_2 x_MLOAD_0 0 n)))
 (let ((?x2672 (stack_s x_0 x_1 w_2 x_MLOAD_0 0 n)))
 (let (($x8396 (= ?x2672 ?x7516)))
 (let ((?x63 (sc_t 0)))
 (let (($x2733 (bvsle ?x63 n)))
 (or $x2733 $x8396)))))))
 ))
 (let ((?x63 (sc_t 0)))
 (let ((?x72 (sc_s 0)))
 (let (($x73 (= ?x72 ?x63)))
 (let (($x10336 (exc_halt_t 3)))
 (let (($x7660 (or (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_t 3)))) (_ bv0 1))) $x10336 (not (bvsle (_ bv0 6) (bvadd (_ bv60 6) (sc_t 3)))))))
 (let (($x10696 (forall ((w (_ BitVec 256)) )(let ((?x5627 (storage_t x_0 x_1 w_2 x_MLOAD_0 3 w)))
 (let ((?x1918 (storage_t x_0 x_1 w_2 x_MLOAD_0 4 w)))
 (= ?x1918 ?x5627))))
 ))
 (let (($x3764 (forall ((n (_ BitVec 6)) )(let ((?x7446 (stack_t x_0 x_1 w_2 x_MLOAD_0 3 n)))
 (let ((?x8529 (stack_t x_0 x_1 w_2 x_MLOAD_0 4 n)))
 (or (bvsle (bvadd (_ bv60 6) (sc_t 3)) n) (= ?x8529 ?x7446)))))
 ))
 (let (($x7164 (= (used_gas_t x_0 x_1 w_2 x_MLOAD_0 4) (+ 3 (used_gas_t x_0 x_1 w_2 x_MLOAD_0 3)))))
 (let ((?x2012 (sc_t 3)))
 (let ((?x7459 (bvadd (_ bv63 6) ?x2012)))
 (let ((?x6560 (stack_t x_0 x_1 w_2 x_MLOAD_0 3 ?x7459)))
 (let (($x8697 (= (stack_t x_0 x_1 w_2 x_MLOAD_0 4 (bvadd (_ bv62 6) ?x2012)) (stack_t x_0 x_1 w_2 x_MLOAD_0 3 (bvadd (_ bv62 6) ?x2012)))))
 (let (($x6962 (= (stack_t x_0 x_1 w_2 x_MLOAD_0 4 (bvadd (_ bv61 6) ?x2012)) (stack_t x_0 x_1 w_2 x_MLOAD_0 3 (bvadd (_ bv61 6) ?x2012)))))
 (let ((?x6782 (bvadd (_ bv60 6) ?x2012)))
 (let ((?x7512 (stack_t x_0 x_1 w_2 x_MLOAD_0 3 ?x6782)))
 (let (($x6765 (= $x10336 (or (exc_halt_t 2) (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_t 2))))))))
 (let (($x7647 (forall ((w (_ BitVec 256)) )(let ((?x8317 (storage_t x_0 x_1 w_2 x_MLOAD_0 2 w)))
 (let ((?x5627 (storage_t x_0 x_1 w_2 x_MLOAD_0 3 w)))
 (= ?x5627 ?x8317))))
 ))
 (let (($x6150 (forall ((n (_ BitVec 6)) )(let ((?x6475 (stack_t x_0 x_1 w_2 x_MLOAD_0 2 n)))
 (let ((?x7446 (stack_t x_0 x_1 w_2 x_MLOAD_0 3 n)))
 (or (bvsle (bvadd (_ bv63 6) (sc_t 2)) n) (= ?x7446 ?x6475)))))
 ))
 (let ((?x3008 (used_gas_t x_0 x_1 w_2 x_MLOAD_0 3)))
 (let ((?x4056 (sc_t 2)))
 (let ((?x6881 (bvadd (_ bv63 6) ?x4056)))
 (let ((?x347 (stack_t x_0 x_1 w_2 x_MLOAD_0 2 ?x6881)))
 (let (($x1920 (exc_halt_t 1)))
 (let (($x5365 (or $x1920 (not (bvsle (_ bv0 6) (bvadd (_ bv61 6) (sc_t 1)))) (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_t 1)))) (_ bv0 1))))))
 (let (($x903 (exc_halt_t 2)))
 (let (($x9836 (forall ((w (_ BitVec 256)) )(let ((?x8200 (storage_t x_0 x_1 w_2 x_MLOAD_0 1 w)))
 (let ((?x8317 (storage_t x_0 x_1 w_2 x_MLOAD_0 2 w)))
 (= ?x8317 ?x8200))))
 ))
 (let (($x3593 (forall ((n (_ BitVec 6)) )(let ((?x8706 (stack_t x_0 x_1 w_2 x_MLOAD_0 1 n)))
 (let ((?x6475 (stack_t x_0 x_1 w_2 x_MLOAD_0 2 n)))
 (or (= ?x6475 ?x8706) (bvsle (bvadd (_ bv61 6) (sc_t 1)) n)))))
 ))
 (let ((?x3233 (used_gas_t x_0 x_1 w_2 x_MLOAD_0 2)))
 (let (($x7571 (= (stack_t x_0 x_1 w_2 x_MLOAD_0 2 (bvadd (_ bv63 6) (sc_t 1))) (stack_t x_0 x_1 w_2 x_MLOAD_0 1 (bvadd (_ bv63 6) (sc_t 1))))))
 (let (($x5910 (= (stack_t x_0 x_1 w_2 x_MLOAD_0 2 (bvadd (_ bv62 6) (sc_t 1))) (stack_t x_0 x_1 w_2 x_MLOAD_0 1 (bvadd (_ bv62 6) (sc_t 1))))))
 (let ((?x4023 (sc_t 1)))
 (let ((?x6888 (bvadd (_ bv61 6) ?x4023)))
 (let ((?x7441 (stack_t x_0 x_1 w_2 x_MLOAD_0 1 ?x6888)))
 (let (($x1160 (= $x1920 (or $x56 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x63))) (_ bv0 1)))))))
 (let (($x7302 (forall ((w (_ BitVec 256)) )(let ((?x5369 (storage_t x_0 x_1 w_2 x_MLOAD_0 0 w)))
 (let ((?x8200 (storage_t x_0 x_1 w_2 x_MLOAD_0 1 w)))
 (= ?x8200 ?x5369))))
 ))
 (let (($x7598 (forall ((n (_ BitVec 6)) )(let ((?x7516 (stack_t x_0 x_1 w_2 x_MLOAD_0 0 n)))
 (let ((?x8706 (stack_t x_0 x_1 w_2 x_MLOAD_0 1 n)))
 (let ((?x63 (sc_t 0)))
 (let (($x2733 (bvsle ?x63 n)))
 (or $x2733 (= ?x8706 ?x7516)))))))
 ))
 (let (($x6839 (= $x3979 (or (exc_halt_s 4) (not (bvsle (_ bv0 6) (bvadd (_ bv61 6) (sc_s 4))))))))
 (let (($x3491 (forall ((w (_ BitVec 256)) )(let ((?x3321 (storage_s x_0 x_1 w_2 x_MLOAD_0 4 w)))
 (let ((?x8595 (storage_s x_0 x_1 w_2 x_MLOAD_0 5 w)))
 (= ?x8595 ?x3321))))
 ))
 (let (($x9470 (forall ((n (_ BitVec 6)) )(let ((?x7976 (stack_s x_0 x_1 w_2 x_MLOAD_0 4 n)))
 (let ((?x1702 (stack_s x_0 x_1 w_2 x_MLOAD_0 5 n)))
 (or (= ?x1702 ?x7976) (bvsle (bvadd (_ bv61 6) (sc_s 4)) n)))))
 ))
 (let (($x7399 (= (used_gas_s x_0 x_1 w_2 x_MLOAD_0 5) (+ 3 (used_gas_s x_0 x_1 w_2 x_MLOAD_0 4)))))
 (let (($x5306 (= (stack_s x_0 x_1 w_2 x_MLOAD_0 5 (bvadd (_ bv62 6) ?x805)) (stack_s x_0 x_1 w_2 x_MLOAD_0 4 (bvadd (_ bv62 6) (sc_s 4))))))
 (let (($x6465 (= (stack_s x_0 x_1 w_2 x_MLOAD_0 5 (bvadd (_ bv61 6) ?x805)) (stack_s x_0 x_1 w_2 x_MLOAD_0 4 (bvadd (_ bv63 6) (sc_s 4))))))
 (let (($x3204 (= (stack_s x_0 x_1 w_2 x_MLOAD_0 5 (bvadd (_ bv63 6) ?x805)) (stack_s x_0 x_1 w_2 x_MLOAD_0 4 (bvadd (_ bv61 6) (sc_s 4))))))
 (let (($x292 (exc_halt_s 3)))
 (let (($x4773 (or $x292 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 3)))) (_ bv0 1))))))
 (let (($x64 (exc_halt_s 4)))
 (let (($x9323 (forall ((w (_ BitVec 256)) )(let ((?x8633 (storage_s x_0 x_1 w_2 x_MLOAD_0 3 w)))
 (let ((?x3321 (storage_s x_0 x_1 w_2 x_MLOAD_0 4 w)))
 (= ?x3321 ?x8633))))
 ))
 (let (($x613 (forall ((n (_ BitVec 6)) )(let ((?x7233 (stack_s x_0 x_1 w_2 x_MLOAD_0 3 n)))
 (let ((?x7976 (stack_s x_0 x_1 w_2 x_MLOAD_0 4 n)))
 (or (bvsle (sc_s 3) n) (= ?x7976 ?x7233)))))
 ))
 (let ((?x5775 (used_gas_s x_0 x_1 w_2 x_MLOAD_0 4)))
 (let (($x7790 (= $x292 (or (exc_halt_s 2) (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_s 2))))))))
 (let (($x1110 (forall ((w (_ BitVec 256)) )(let ((?x6912 (storage_s x_0 x_1 w_2 x_MLOAD_0 2 w)))
 (let ((?x8633 (storage_s x_0 x_1 w_2 x_MLOAD_0 3 w)))
 (= ?x8633 ?x6912))))
 ))
 (let (($x9523 (forall ((n (_ BitVec 6)) )(let ((?x10688 (stack_s x_0 x_1 w_2 x_MLOAD_0 2 n)))
 (let ((?x7233 (stack_s x_0 x_1 w_2 x_MLOAD_0 3 n)))
 (or (bvsle (bvadd (_ bv63 6) (sc_s 2)) n) (= ?x7233 ?x10688)))))
 ))
 (let ((?x420 (used_gas_s x_0 x_1 w_2 x_MLOAD_0 3)))
 (let ((?x218 (sc_s 2)))
 (let ((?x6327 (bvadd (_ bv63 6) ?x218)))
 (let ((?x8604 (stack_s x_0 x_1 w_2 x_MLOAD_0 2 ?x6327)))
 (let (($x3879 (= (stack_s x_0 x_1 w_2 x_MLOAD_0 3 (bvadd (_ bv63 6) (sc_s 3))) (f_MLOAD x_0 x_1 w_2 x_MLOAD_0 ?x8604))))
 (let (($x6277 (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_s 1))))))
 (let (($x189 (exc_halt_s 1)))
 (let (($x10062 (or $x189 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 1)))) (_ bv0 1))) $x6277)))
 (let (($x247 (exc_halt_s 2)))
 (let (($x10522 (forall ((w (_ BitVec 256)) )(let ((?x3602 (storage_s x_0 x_1 w_2 x_MLOAD_0 1 w)))
 (let ((?x6912 (storage_s x_0 x_1 w_2 x_MLOAD_0 2 w)))
 (= ?x6912 ?x3602))))
 ))
 (let (($x9634 (forall ((n (_ BitVec 6)) )(let ((?x9580 (stack_s x_0 x_1 w_2 x_MLOAD_0 1 n)))
 (let ((?x10688 (stack_s x_0 x_1 w_2 x_MLOAD_0 2 n)))
 (let ((?x154 (sc_s 1)))
 (let ((?x5240 (bvadd (_ bv63 6) ?x154)))
 (let (($x2656 (bvsle ?x5240 n)))
 (or $x2656 (= ?x10688 ?x9580))))))))
 ))
 (let ((?x7361 (used_gas_s x_0 x_1 w_2 x_MLOAD_0 2)))
 (let ((?x154 (sc_s 1)))
 (let ((?x5240 (bvadd (_ bv63 6) ?x154)))
 (let ((?x8351 (stack_s x_0 x_1 w_2 x_MLOAD_0 1 ?x5240)))
 (let (($x3120 (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) ?x72)))))
 (let (($x10647 (or $x57 $x3120 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x72))) (_ bv0 1))))))
 (let (($x10638 (forall ((w (_ BitVec 256)) )(let ((?x8649 (storage_s x_0 x_1 w_2 x_MLOAD_0 0 w)))
 (let ((?x3602 (storage_s x_0 x_1 w_2 x_MLOAD_0 1 w)))
 (= ?x3602 ?x8649))))
 ))
 (let (($x8103 (forall ((n (_ BitVec 6)) )(let ((?x2672 (stack_s x_0 x_1 w_2 x_MLOAD_0 0 n)))
 (let ((?x9580 (stack_s x_0 x_1 w_2 x_MLOAD_0 1 n)))
 (let ((?x72 (sc_s 0)))
 (let ((?x1529 (bvadd (_ bv62 6) ?x72)))
 (let (($x7006 (bvsle ?x1529 n)))
 (or $x7006 (= ?x9580 ?x2672))))))))
 ))
 (let (($x10311 (= (stack_s x_0 x_1 w_2 x_MLOAD_0 1 (bvadd (_ bv63 6) ?x72)) (stack_s x_0 x_1 w_2 x_MLOAD_0 0 (bvadd (_ bv63 6) ?x72)))))
 (let ((?x1529 (bvadd (_ bv62 6) ?x72)))
 (let ((?x326 (stack_s x_0 x_1 w_2 x_MLOAD_0 0 ?x1529)))
 (let (($x10464 (forall ((w0 (_ BitVec 256)) )(let (($x9554 (= (stack_s x_0 x_1 w_2 x_MLOAD_0 2 (bvadd (_ bv63 6) (sc_s 2))) w0)))
 (let ((?x667 (f_MLOAD x_0 x_1 w_2 x_MLOAD_0 w0)))
 (= ?x667 (ite $x9554 x_MLOAD_0 (_ bv0 256))))))
 ))
 (let (($x5489 (forall ((w (_ BitVec 256)) )(let ((?x8649 (storage_s x_0 x_1 w_2 x_MLOAD_0 0 w)))
 (= ?x8649 (_ bv0 256))))
 ))
 (let (($x8457 (= ?x2524 0)))
 (let (($x3117 (not $x57)))
 (let (($x129 (= (stack_s x_0 x_1 w_2 x_MLOAD_0 0 (_ bv1 6)) x_1)))
 (let (($x1832 (= (stack_s x_0 x_1 w_2 x_MLOAD_0 0 (_ bv0 6)) x_0)))
 (let (($x3159 (= ?x72 (_ bv2 6))))
 (and $x3159 $x1832 $x129 $x3117 $x8457 $x5489 $x10464 (= ?x8351 ?x326) (= (stack_s x_0 x_1 w_2 x_MLOAD_0 1 ?x1529) ?x326) $x10311 (= (used_gas_s x_0 x_1 w_2 x_MLOAD_0 1) (+ 3 ?x2524)) (= ?x154 (bvadd (_ bv1 6) ?x72)) $x8103 $x10638 (= $x189 $x10647) (= ?x8604 ?x8351) (= (stack_s x_0 x_1 w_2 x_MLOAD_0 2 ?x5240) ?x8351) (= ?x7361 (+ 3 (used_gas_s x_0 x_1 w_2 x_MLOAD_0 1))) (= ?x218 (bvadd (_ bv1 6) ?x154)) $x9634 $x10522 (= $x247 $x10062) $x3879 (= ?x420 (+ 3 ?x7361)) (= (sc_s 3) ?x218) $x9523 $x1110 $x7790 (= (stack_s x_0 x_1 w_2 x_MLOAD_0 4 (sc_s 3)) w_2) (= ?x5775 (+ 3 ?x420)) (= (sc_s 4) (bvadd (_ bv1 6) (sc_s 3))) $x613 $x9323 (= $x64 $x4773) $x3204 $x6465 $x5306 $x7399 (= ?x805 (sc_s 4)) $x9470 $x3491 $x6839 (= (stack_t x_0 x_1 w_2 x_MLOAD_0 1 ?x63) w_2) (= (used_gas_t x_0 x_1 w_2 x_MLOAD_0 1) (+ 3 ?x2063)) (= ?x4023 (bvadd (_ bv1 6) ?x63)) $x7598 $x7302 $x1160 (= ?x347 ?x7441) (= (stack_t x_0 x_1 w_2 x_MLOAD_0 2 ?x6888) ?x7441) $x5910 $x7571 (= ?x3233 (+ 3 (used_gas_t x_0 x_1 w_2 x_MLOAD_0 1))) (= ?x4056 (bvadd (_ bv1 6) ?x4023)) $x3593 $x9836 (= $x903 $x5365) (= ?x6560 (f_MLOAD x_0 x_1 w_2 x_MLOAD_0 ?x347)) (= ?x3008 (+ 3 ?x3233)) (= ?x2012 ?x4056) $x6150 $x7647 $x6765 (= (stack_t x_0 x_1 w_2 x_MLOAD_0 4 (bvadd (_ bv63 6) ?x3757)) ?x7512) (= (stack_t x_0 x_1 w_2 x_MLOAD_0 4 ?x6782) ?x7512) $x6962 $x8697 (= (stack_t x_0 x_1 w_2 x_MLOAD_0 4 ?x7459) ?x6560) $x7164 (= ?x3757 (bvadd (_ bv1 6) ?x2012)) $x3764 $x10696 (= $x3723 $x7660) $x73 $x7093 $x58 $x5839 $x7008 (not (and $x9143 $x3382 $x7567 $x2738)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
 )
(check-sat)
(get-proof)
