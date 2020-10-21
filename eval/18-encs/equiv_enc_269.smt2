; POP SWAP1 POP DUP1 PUSH 0x00 ADD => DUP2 SWAP3 POP POP
(set-option :produce-proofs true)
; 
(set-info :status unknown)
(declare-fun storage_t ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 256)) (_ BitVec 256))
(declare-fun storage_s ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 256)) (_ BitVec 256))
(declare-fun exc_halt_t (Int) Bool)
(declare-fun exc_halt_s (Int) Bool)
(declare-fun stack_t ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 6)) (_ BitVec 256))
(declare-fun stack_s ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 6)) (_ BitVec 256))
(declare-fun sc_t (Int) (_ BitVec 6))
(declare-fun sc_s (Int) (_ BitVec 6))
(declare-fun used_gas_t ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int) Int)
(declare-fun used_gas_s ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int) Int)
(assert
 (exists ((x_0 (_ BitVec 256)) (x_1 (_ BitVec 256)) (x_2 (_ BitVec 256)) )(let (($x8921 (forall ((w (_ BitVec 256)) )(let ((?x4118 (storage_t x_0 x_1 x_2 4 w)))
 (let ((?x4864 (storage_s x_0 x_1 x_2 6 w)))
 (= ?x4864 ?x4118))))
 ))
 (let (($x3723 (exc_halt_t 4)))
 (let (($x772 (exc_halt_s 6)))
 (let (($x1357 (= $x772 $x3723)))
 (let (($x1449 (forall ((n (_ BitVec 6)) )(let ((?x9557 (stack_t x_0 x_1 x_2 4 n)))
 (let ((?x978 (stack_s x_0 x_1 x_2 6 n)))
 (let (($x11366 (= ?x978 ?x9557)))
 (let ((?x3757 (sc_t 4)))
 (let (($x8252 (bvsle ?x3757 n)))
 (or $x8252 $x11366)))))))
 ))
 (let ((?x3757 (sc_t 4)))
 (let ((?x926 (sc_s 6)))
 (let (($x5103 (= ?x926 ?x3757)))
 (let ((?x9308 (used_gas_t x_0 x_1 x_2 0)))
 (let ((?x345 (used_gas_s x_0 x_1 x_2 0)))
 (let (($x6908 (= ?x345 ?x9308)))
 (let (($x3936 (forall ((w (_ BitVec 256)) )(let ((?x7835 (storage_t x_0 x_1 x_2 0 w)))
 (let ((?x9173 (storage_s x_0 x_1 x_2 0 w)))
 (= ?x9173 ?x7835))))
 ))
 (let (($x56 (exc_halt_t 0)))
 (let (($x57 (exc_halt_s 0)))
 (let (($x58 (= $x57 $x56)))
 (let (($x9815 (forall ((n (_ BitVec 6)) )(let ((?x6390 (stack_t x_0 x_1 x_2 0 n)))
 (let ((?x1978 (stack_s x_0 x_1 x_2 0 n)))
 (let (($x4543 (= ?x1978 ?x6390)))
 (let ((?x63 (sc_t 0)))
 (let (($x4036 (bvsle ?x63 n)))
 (or $x4036 $x4543)))))))
 ))
 (let ((?x63 (sc_t 0)))
 (let ((?x72 (sc_s 0)))
 (let (($x73 (= ?x72 ?x63)))
 (let (($x3775 (= $x3723 (or (exc_halt_t 3) (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_t 3))))))))
 (let (($x9913 (forall ((w (_ BitVec 256)) )(let ((?x10000 (storage_t x_0 x_1 x_2 3 w)))
 (let ((?x4118 (storage_t x_0 x_1 x_2 4 w)))
 (= ?x4118 ?x10000))))
 ))
 (let (($x308 (forall ((n (_ BitVec 6)) )(let ((?x3388 (stack_t x_0 x_1 x_2 3 n)))
 (let ((?x9557 (stack_t x_0 x_1 x_2 4 n)))
 (let ((?x6378 (sc_t 3)))
 (let ((?x8958 (bvadd (_ bv63 6) ?x6378)))
 (let (($x7546 (bvsle ?x8958 n)))
 (or $x7546 (= ?x9557 ?x3388))))))))
 ))
 (let (($x10279 (= (used_gas_t x_0 x_1 x_2 4) (+ 2 (used_gas_t x_0 x_1 x_2 3)))))
 (let (($x8009 (exc_halt_t 3)))
 (let (($x2732 (= $x8009 (or (exc_halt_t 2) (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_t 2))))))))
 (let (($x4133 (forall ((w (_ BitVec 256)) )(let ((?x8271 (storage_t x_0 x_1 x_2 2 w)))
 (let ((?x10000 (storage_t x_0 x_1 x_2 3 w)))
 (= ?x10000 ?x8271))))
 ))
 (let (($x9675 (forall ((n (_ BitVec 6)) )(let ((?x4056 (sc_t 2)))
 (let ((?x2982 (bvadd (_ bv63 6) ?x4056)))
 (let (($x9723 (bvsle ?x2982 n)))
 (let ((?x5328 (stack_t x_0 x_1 x_2 2 n)))
 (let ((?x3388 (stack_t x_0 x_1 x_2 3 n)))
 (or (= ?x3388 ?x5328) $x9723)))))))
 ))
 (let ((?x7342 (used_gas_t x_0 x_1 x_2 3)))
 (let (($x903 (exc_halt_t 2)))
 (let (($x9242 (= $x903 (or (exc_halt_t 1) (not (bvsle (_ bv0 6) (bvadd (_ bv60 6) (sc_t 1))))))))
 (let (($x11309 (forall ((w (_ BitVec 256)) )(let ((?x2606 (storage_t x_0 x_1 x_2 1 w)))
 (let ((?x8271 (storage_t x_0 x_1 x_2 2 w)))
 (= ?x8271 ?x2606))))
 ))
 (let (($x10803 (forall ((n (_ BitVec 6)) )(let ((?x3931 (stack_t x_0 x_1 x_2 1 n)))
 (let ((?x5328 (stack_t x_0 x_1 x_2 2 n)))
 (or (bvsle (bvadd (_ bv60 6) (sc_t 1)) n) (= ?x5328 ?x3931)))))
 ))
 (let ((?x2430 (used_gas_t x_0 x_1 x_2 2)))
 (let (($x8864 (= (stack_t x_0 x_1 x_2 2 (bvadd (_ bv62 6) (sc_t 2))) (stack_t x_0 x_1 x_2 1 (bvadd (_ bv62 6) (sc_t 1))))))
 (let (($x5804 (= (stack_t x_0 x_1 x_2 2 (bvadd (_ bv61 6) (sc_t 2))) (stack_t x_0 x_1 x_2 1 (bvadd (_ bv61 6) (sc_t 1))))))
 (let ((?x4023 (sc_t 1)))
 (let ((?x7819 (bvadd (_ bv63 6) ?x4023)))
 (let ((?x7376 (stack_t x_0 x_1 x_2 1 ?x7819)))
 (let (($x11644 (= (stack_t x_0 x_1 x_2 2 (bvadd (_ bv63 6) (sc_t 2))) (stack_t x_0 x_1 x_2 1 (bvadd (_ bv60 6) ?x4023)))))
 (let (($x5983 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x63))) (_ bv0 1)))))
 (let (($x1920 (exc_halt_t 1)))
 (let (($x10573 (forall ((w (_ BitVec 256)) )(let ((?x7835 (storage_t x_0 x_1 x_2 0 w)))
 (let ((?x2606 (storage_t x_0 x_1 x_2 1 w)))
 (= ?x2606 ?x7835))))
 ))
 (let (($x5758 (forall ((n (_ BitVec 6)) )(let ((?x6390 (stack_t x_0 x_1 x_2 0 n)))
 (let ((?x3931 (stack_t x_0 x_1 x_2 1 n)))
 (or (bvsle (bvadd (_ bv62 6) (sc_t 0)) n) (= ?x3931 ?x6390)))))
 ))
 (let (($x7805 (= ?x4023 (bvadd (_ bv1 6) ?x63))))
 (let (($x7485 (= (stack_t x_0 x_1 x_2 1 (bvadd (_ bv63 6) ?x63)) (stack_t x_0 x_1 x_2 0 (bvadd (_ bv63 6) ?x63)))))
 (let ((?x5842 (bvadd (_ bv62 6) ?x63)))
 (let ((?x3925 (stack_t x_0 x_1 x_2 0 ?x5842)))
 (let (($x2362 (= $x772 (or (exc_halt_s 5) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 5))))))))
 (let (($x9256 (forall ((w (_ BitVec 256)) )(let ((?x8202 (storage_s x_0 x_1 x_2 5 w)))
 (let ((?x4864 (storage_s x_0 x_1 x_2 6 w)))
 (= ?x4864 ?x8202))))
 ))
 (let (($x6391 (forall ((n (_ BitVec 6)) )(let ((?x805 (sc_s 5)))
 (let ((?x798 (bvadd (_ bv62 6) ?x805)))
 (let (($x10739 (bvsle ?x798 n)))
 (let ((?x10097 (stack_s x_0 x_1 x_2 5 n)))
 (let ((?x978 (stack_s x_0 x_1 x_2 6 n)))
 (or (= ?x978 ?x10097) $x10739)))))))
 ))
 (let (($x9254 (= (used_gas_s x_0 x_1 x_2 6) (+ 3 (used_gas_s x_0 x_1 x_2 5)))))
 (let ((?x6321 (bvadd (stack_s x_0 x_1 x_2 5 (bvadd (_ bv63 6) (sc_s 5))) (stack_s x_0 x_1 x_2 5 (bvadd (_ bv62 6) (sc_s 5))))))
 (let (($x2721 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 4)))) (_ bv0 1)))))
 (let (($x64 (exc_halt_s 4)))
 (let (($x3979 (exc_halt_s 5)))
 (let (($x11623 (= $x3979 (or $x64 $x2721))))
 (let (($x3045 (forall ((w (_ BitVec 256)) )(let ((?x11483 (storage_s x_0 x_1 x_2 4 w)))
 (let ((?x8202 (storage_s x_0 x_1 x_2 5 w)))
 (= ?x8202 ?x11483))))
 ))
 (let (($x3550 (forall ((n (_ BitVec 6)) )(let ((?x4305 (sc_s 4)))
 (let (($x6116 (bvsle ?x4305 n)))
 (let ((?x5221 (stack_s x_0 x_1 x_2 4 n)))
 (let ((?x10097 (stack_s x_0 x_1 x_2 5 n)))
 (or (= ?x10097 ?x5221) $x6116))))))
 ))
 (let ((?x805 (sc_s 5)))
 (let (($x7084 (= ?x805 (bvadd (_ bv1 6) (sc_s 4)))))
 (let ((?x11375 (used_gas_s x_0 x_1 x_2 5)))
 (let (($x1360 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 3)))) (_ bv0 1)))))
 (let (($x292 (exc_halt_s 3)))
 (let (($x10288 (= $x64 (or $x292 $x1360 (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_s 3))))))))
 (let (($x3247 (forall ((w (_ BitVec 256)) )(let ((?x336 (storage_s x_0 x_1 x_2 3 w)))
 (let ((?x11483 (storage_s x_0 x_1 x_2 4 w)))
 (= ?x11483 ?x336))))
 ))
 (let (($x5563 (forall ((n (_ BitVec 6)) )(let ((?x9567 (stack_s x_0 x_1 x_2 3 n)))
 (let ((?x5221 (stack_s x_0 x_1 x_2 4 n)))
 (let ((?x275 (sc_s 3)))
 (let ((?x7617 (bvadd (_ bv63 6) ?x275)))
 (let (($x5853 (bvsle ?x7617 n)))
 (or $x5853 (= ?x5221 ?x9567))))))))
 ))
 (let ((?x4305 (sc_s 4)))
 (let (($x6010 (= ?x4305 (bvadd (_ bv1 6) (sc_s 3)))))
 (let ((?x8536 (used_gas_s x_0 x_1 x_2 4)))
 (let ((?x275 (sc_s 3)))
 (let ((?x7617 (bvadd (_ bv63 6) ?x275)))
 (let ((?x11353 (stack_s x_0 x_1 x_2 3 ?x7617)))
 (let (($x10500 (= $x292 (or (exc_halt_s 2) (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_s 2))))))))
 (let (($x7699 (forall ((w (_ BitVec 256)) )(let ((?x10859 (storage_s x_0 x_1 x_2 2 w)))
 (let ((?x336 (storage_s x_0 x_1 x_2 3 w)))
 (= ?x336 ?x10859))))
 ))
 (let (($x7598 (forall ((n (_ BitVec 6)) )(let ((?x218 (sc_s 2)))
 (let ((?x3337 (bvadd (_ bv63 6) ?x218)))
 (let (($x10223 (bvsle ?x3337 n)))
 (let ((?x9217 (stack_s x_0 x_1 x_2 2 n)))
 (let ((?x9567 (stack_s x_0 x_1 x_2 3 n)))
 (or (= ?x9567 ?x9217) $x10223)))))))
 ))
 (let ((?x218 (sc_s 2)))
 (let ((?x3337 (bvadd (_ bv63 6) ?x218)))
 (let (($x4124 (= ?x275 ?x3337)))
 (let ((?x7399 (used_gas_s x_0 x_1 x_2 3)))
 (let (($x247 (exc_halt_s 2)))
 (let (($x6805 (= $x247 (or (exc_halt_s 1) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 1))))))))
 (let (($x11712 (forall ((w (_ BitVec 256)) )(let ((?x6765 (storage_s x_0 x_1 x_2 1 w)))
 (let ((?x10859 (storage_s x_0 x_1 x_2 2 w)))
 (= ?x10859 ?x6765))))
 ))
 (let (($x1709 (forall ((n (_ BitVec 6)) )(let ((?x154 (sc_s 1)))
 (let ((?x3772 (bvadd (_ bv62 6) ?x154)))
 (let (($x10311 (bvsle ?x3772 n)))
 (let ((?x7655 (stack_s x_0 x_1 x_2 1 n)))
 (let ((?x9217 (stack_s x_0 x_1 x_2 2 n)))
 (or (= ?x9217 ?x7655) $x10311)))))))
 ))
 (let ((?x154 (sc_s 1)))
 (let (($x2333 (= ?x218 ?x154)))
 (let ((?x10496 (used_gas_s x_0 x_1 x_2 2)))
 (let (($x8275 (= (stack_s x_0 x_1 x_2 2 (bvadd (_ bv62 6) ?x218)) (stack_s x_0 x_1 x_2 1 (bvadd (_ bv63 6) ?x154)))))
 (let (($x6632 (= (stack_s x_0 x_1 x_2 2 ?x3337) (stack_s x_0 x_1 x_2 1 (bvadd (_ bv62 6) ?x154)))))
 (let (($x2636 (forall ((w (_ BitVec 256)) )(let ((?x9173 (storage_s x_0 x_1 x_2 0 w)))
 (let ((?x6765 (storage_s x_0 x_1 x_2 1 w)))
 (= ?x6765 ?x9173))))
 ))
 (let (($x10333 (forall ((n (_ BitVec 6)) )(let ((?x72 (sc_s 0)))
 (let ((?x5742 (bvadd (_ bv63 6) ?x72)))
 (let (($x9929 (bvsle ?x5742 n)))
 (let ((?x1978 (stack_s x_0 x_1 x_2 0 n)))
 (let ((?x7655 (stack_s x_0 x_1 x_2 1 n)))
 (or (= ?x7655 ?x1978) $x9929)))))))
 ))
 (let (($x4711 (forall ((w (_ BitVec 256)) )(let ((?x9173 (storage_s x_0 x_1 x_2 0 w)))
 (= ?x9173 (_ bv0 256))))
 ))
 (let (($x8840 (= ?x345 0)))
 (let (($x4562 (not $x57)))
 (let (($x6611 (= (stack_s x_0 x_1 x_2 0 (_ bv2 6)) x_2)))
 (let (($x4645 (= (stack_s x_0 x_1 x_2 0 (_ bv1 6)) x_1)))
 (let (($x6042 (= (stack_s x_0 x_1 x_2 0 (_ bv0 6)) x_0)))
 (let (($x6735 (= ?x72 (_ bv3 6))))
 (and $x6735 $x6042 $x4645 $x6611 $x4562 $x8840 $x4711 (= (used_gas_s x_0 x_1 x_2 1) (+ 2 ?x345)) (= ?x154 (bvadd (_ bv63 6) ?x72)) $x10333 $x2636 (= (exc_halt_s 1) (or $x57 (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) ?x72))))) $x6632 $x8275 (= ?x10496 (+ 3 (used_gas_s x_0 x_1 x_2 1))) $x2333 $x1709 $x11712 $x6805 (= ?x7399 (+ 2 ?x10496)) $x4124 $x7598 $x7699 $x10500 (= (stack_s x_0 x_1 x_2 4 (bvadd (_ bv63 6) ?x4305)) ?x11353) (= (stack_s x_0 x_1 x_2 4 ?x7617) ?x11353) (= ?x8536 (+ 3 ?x7399)) $x6010 $x5563 $x3247 $x10288 (= (stack_s x_0 x_1 x_2 5 ?x4305) (_ bv0 256)) (= ?x11375 (+ 3 ?x8536)) $x7084 $x3550 $x3045 $x11623 (= (stack_s x_0 x_1 x_2 6 (bvadd (_ bv63 6) ?x926)) ?x6321) $x9254 (= ?x926 (bvadd (_ bv63 6) ?x805)) $x6391 $x9256 $x2362 (= ?x7376 ?x3925) (= (stack_t x_0 x_1 x_2 1 ?x5842) ?x3925) $x7485 (= (used_gas_t x_0 x_1 x_2 1) (+ 3 ?x9308)) $x7805 $x5758 $x10573 (= $x1920 (or $x56 $x5983 (not (bvsle (_ bv0 6) ?x5842)))) $x11644 (= (stack_t x_0 x_1 x_2 2 (bvadd (_ bv60 6) (sc_t 2))) ?x7376) $x5804 $x8864 (= ?x2430 (+ 3 (used_gas_t x_0 x_1 x_2 1))) (= (sc_t 2) ?x4023) $x10803 $x11309 $x9242 (= ?x7342 (+ 2 ?x2430)) (= (sc_t 3) (bvadd (_ bv63 6) (sc_t 2))) $x9675 $x4133 $x2732 $x10279 (= ?x3757 (bvadd (_ bv63 6) (sc_t 3))) $x308 $x9913 $x3775 $x73 $x9815 $x58 $x3936 $x6908 (not (and $x5103 $x1449 $x1357 $x8921)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
 )
(check-sat)
(get-proof)
