; PUSH 0x00 ISZERO ISZERO PUSH cw_4 PUSH 0x00 => PUSH 0x00 PUSH cw_4 DUP2
(set-option :produce-proofs true)
; 
(set-info :status unknown)
(declare-fun storage_t ((_ BitVec 256) Int (_ BitVec 256)) (_ BitVec 256))
(declare-fun w_4 () (_ BitVec 256))
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
 (let (($x1996 (forall ((w (_ BitVec 256)) )(let ((?x3900 (storage_t w_4 3 w)))
 (let ((?x3553 (storage_s w_4 5 w)))
 (= ?x3553 ?x3900))))
 ))
 (let (($x3892 (exc_halt_t 3)))
 (let (($x3979 (exc_halt_s 5)))
 (let (($x6095 (= $x3979 $x3892)))
 (let (($x8838 (forall ((n (_ BitVec 6)) )(let ((?x6137 (stack_t w_4 3 n)))
 (let ((?x9848 (stack_s w_4 5 n)))
 (let (($x10253 (= ?x9848 ?x6137)))
 (let ((?x9893 (sc_t 3)))
 (let (($x4335 (bvsle ?x9893 n)))
 (or $x4335 $x10253)))))))
 ))
 (let ((?x9893 (sc_t 3)))
 (let ((?x805 (sc_s 5)))
 (let (($x5622 (= ?x805 ?x9893)))
 (let ((?x9992 (used_gas_t w_4 0)))
 (let ((?x3190 (used_gas_s w_4 0)))
 (let (($x4281 (= ?x3190 ?x9992)))
 (let (($x8478 (forall ((w (_ BitVec 256)) )(let ((?x10165 (storage_t w_4 0 w)))
 (let ((?x7937 (storage_s w_4 0 w)))
 (= ?x7937 ?x10165))))
 ))
 (let (($x56 (exc_halt_t 0)))
 (let (($x57 (exc_halt_s 0)))
 (let (($x58 (= $x57 $x56)))
 (let (($x10191 (forall ((n (_ BitVec 6)) )(let ((?x2174 (stack_t w_4 0 n)))
 (let ((?x47 (stack_s w_4 0 n)))
 (let (($x6575 (= ?x47 ?x2174)))
 (let ((?x63 (sc_t 0)))
 (let (($x4861 (bvsle ?x63 n)))
 (or $x4861 $x6575)))))))
 ))
 (let ((?x63 (sc_t 0)))
 (let ((?x72 (sc_s 0)))
 (let (($x73 (= ?x72 ?x63)))
 (let (($x10960 (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_t 2))))))
 (let (($x9517 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_t 2)))) (_ bv0 1)))))
 (let (($x903 (exc_halt_t 2)))
 (let (($x11804 (= $x3892 (or $x903 $x9517 $x10960))))
 (let (($x607 (forall ((w (_ BitVec 256)) )(let ((?x725 (storage_t w_4 2 w)))
 (let ((?x3900 (storage_t w_4 3 w)))
 (= ?x3900 ?x725))))
 ))
 (let (($x2326 (forall ((n (_ BitVec 6)) )(let ((?x4056 (sc_t 2)))
 (let ((?x7917 (bvadd (_ bv62 6) ?x4056)))
 (let (($x8862 (bvsle ?x7917 n)))
 (or (= (stack_t w_4 3 n) (stack_t w_4 2 n)) $x8862)))))
 ))
 (let (($x7792 (= ?x9893 (bvadd (_ bv1 6) (sc_t 2)))))
 (let (($x5370 (= (stack_t w_4 3 (bvadd (_ bv63 6) (sc_t 2))) (stack_t w_4 2 (bvadd (_ bv63 6) (sc_t 2))))))
 (let ((?x4056 (sc_t 2)))
 (let ((?x7917 (bvadd (_ bv62 6) ?x4056)))
 (let ((?x7012 (stack_t w_4 2 ?x7917)))
 (let (($x9991 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_t 1)))) (_ bv0 1)))))
 (let (($x1920 (exc_halt_t 1)))
 (let (($x11317 (= $x903 (or $x1920 $x9991))))
 (let (($x7222 (forall ((w (_ BitVec 256)) )(let ((?x9898 (storage_t w_4 1 w)))
 (let ((?x725 (storage_t w_4 2 w)))
 (= ?x725 ?x9898))))
 ))
 (let (($x6683 (forall ((n (_ BitVec 6)) )(let ((?x4023 (sc_t 1)))
 (let (($x3531 (bvsle ?x4023 n)))
 (or $x3531 (= (stack_t w_4 2 n) (stack_t w_4 1 n))))))
 ))
 (let (($x4596 (= ?x4056 (bvadd (_ bv1 6) (sc_t 1)))))
 (let (($x7107 (= $x1920 (or $x56 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x63))) (_ bv0 1)))))))
 (let (($x2308 (forall ((w (_ BitVec 256)) )(let ((?x10165 (storage_t w_4 0 w)))
 (let ((?x9898 (storage_t w_4 1 w)))
 (= ?x9898 ?x10165))))
 ))
 (let (($x10431 (forall ((n (_ BitVec 6)) )(let ((?x63 (sc_t 0)))
 (let (($x4861 (bvsle ?x63 n)))
 (or $x4861 (= (stack_t w_4 1 n) (stack_t w_4 0 n))))))
 ))
 (let ((?x4023 (sc_t 1)))
 (let (($x7726 (= ?x4023 (bvadd (_ bv1 6) ?x63))))
 (let (($x8120 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 4)))) (_ bv0 1)))))
 (let (($x64 (exc_halt_s 4)))
 (let (($x8297 (= $x3979 (or $x64 $x8120))))
 (let (($x6824 (forall ((w (_ BitVec 256)) )(let ((?x188 (storage_s w_4 4 w)))
 (let ((?x3553 (storage_s w_4 5 w)))
 (= ?x3553 ?x188))))
 ))
 (let (($x893 (forall ((n (_ BitVec 6)) )(let ((?x4305 (sc_s 4)))
 (let (($x5821 (bvsle ?x4305 n)))
 (or (= (stack_s w_4 5 n) (stack_s w_4 4 n)) $x5821))))
 ))
 (let (($x8155 (= ?x805 (bvadd (_ bv1 6) (sc_s 4)))))
 (let (($x292 (exc_halt_s 3)))
 (let (($x659 (or $x292 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 3)))) (_ bv0 1))))))
 (let (($x1207 (forall ((w (_ BitVec 256)) )(let ((?x8688 (storage_s w_4 3 w)))
 (let ((?x188 (storage_s w_4 4 w)))
 (= ?x188 ?x8688))))
 ))
 (let (($x1680 (forall ((n (_ BitVec 6)) )(or (bvsle (sc_s 3) n) (= (stack_s w_4 4 n) (stack_s w_4 3 n))))
 ))
 (let (($x1340 (= $x292 (or (exc_halt_s 2) (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_s 2))))))))
 (let (($x9925 (forall ((w (_ BitVec 256)) )(let ((?x9814 (storage_s w_4 2 w)))
 (let ((?x8688 (storage_s w_4 3 w)))
 (= ?x8688 ?x9814))))
 ))
 (let (($x9904 (forall ((n (_ BitVec 6)) )(let ((?x218 (sc_s 2)))
 (let ((?x9274 (bvadd (_ bv63 6) ?x218)))
 (let (($x11847 (bvsle ?x9274 n)))
 (or (= (stack_s w_4 3 n) (stack_s w_4 2 n)) $x11847)))))
 ))
 (let ((?x218 (sc_s 2)))
 (let ((?x275 (sc_s 3)))
 (let (($x1939 (= ?x275 ?x218)))
 (let (($x2423 (= (stack_s w_4 3 (bvadd (_ bv63 6) ?x275)) (ite (= (stack_s w_4 2 (bvadd (_ bv63 6) ?x218)) (_ bv0 256)) (_ bv1 256) (_ bv0 256)))))
 (let (($x247 (exc_halt_s 2)))
 (let (($x9953 (= $x247 (or (exc_halt_s 1) (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_s 1))))))))
 (let (($x1371 (forall ((w (_ BitVec 256)) )(let ((?x1816 (storage_s w_4 1 w)))
 (let ((?x9814 (storage_s w_4 2 w)))
 (= ?x9814 ?x1816))))
 ))
 (let (($x5727 (forall ((n (_ BitVec 6)) )(let ((?x154 (sc_s 1)))
 (let ((?x2046 (bvadd (_ bv63 6) ?x154)))
 (let (($x3117 (bvsle ?x2046 n)))
 (or (= (stack_s w_4 2 n) (stack_s w_4 1 n)) $x3117)))))
 ))
 (let ((?x154 (sc_s 1)))
 (let (($x4713 (= ?x218 ?x154)))
 (let ((?x9274 (bvadd (_ bv63 6) ?x218)))
 (let ((?x5753 (stack_s w_4 2 ?x9274)))
 (let (($x10159 (= ?x5753 (ite (= (stack_s w_4 1 (bvadd (_ bv63 6) ?x154)) (_ bv0 256)) (_ bv1 256) (_ bv0 256)))))
 (let (($x189 (exc_halt_s 1)))
 (let (($x7022 (= $x189 (or $x57 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x72))) (_ bv0 1)))))))
 (let (($x19 (forall ((w (_ BitVec 256)) )(let ((?x7937 (storage_s w_4 0 w)))
 (let ((?x1816 (storage_s w_4 1 w)))
 (= ?x1816 ?x7937))))
 ))
 (let (($x1956 (forall ((n (_ BitVec 6)) )(let ((?x72 (sc_s 0)))
 (let (($x8487 (bvsle ?x72 n)))
 (or (= (stack_s w_4 1 n) (stack_s w_4 0 n)) $x8487))))
 ))
 (let (($x7777 (= ?x154 (bvadd (_ bv1 6) ?x72))))
 (let (($x6510 (forall ((w (_ BitVec 256)) )(let ((?x7937 (storage_s w_4 0 w)))
 (= ?x7937 (_ bv0 256))))
 ))
 (let (($x10278 (= ?x3190 0)))
 (let (($x5326 (not $x57)))
 (let (($x136 (= ?x72 (_ bv0 6))))
 (and $x136 $x5326 $x10278 $x6510 (= (stack_s w_4 1 ?x72) (_ bv0 256)) (= (used_gas_s w_4 1) (+ 3 ?x3190)) $x7777 $x1956 $x19 $x7022 $x10159 (= (used_gas_s w_4 2) (+ 3 (used_gas_s w_4 1))) $x4713 $x5727 $x1371 $x9953 $x2423 (= (used_gas_s w_4 3) (+ 3 (used_gas_s w_4 2))) $x1939 $x9904 $x9925 $x1340 (= (stack_s w_4 4 ?x275) w_4) (= (used_gas_s w_4 4) (+ 3 (used_gas_s w_4 3))) (= (sc_s 4) (bvadd (_ bv1 6) ?x275)) $x1680 $x1207 (= $x64 $x659) (= (stack_s w_4 5 (sc_s 4)) (_ bv0 256)) (= (used_gas_s w_4 5) (+ 3 (used_gas_s w_4 4))) $x8155 $x893 $x6824 $x8297 (= (stack_t w_4 1 ?x63) (_ bv0 256)) (= (used_gas_t w_4 1) (+ 3 ?x9992)) $x7726 $x10431 $x2308 $x7107 (= (stack_t w_4 2 ?x4023) w_4) (= (used_gas_t w_4 2) (+ 3 (used_gas_t w_4 1))) $x4596 $x6683 $x7222 $x11317 (= (stack_t w_4 3 (bvadd (_ bv63 6) ?x9893)) ?x7012) (= (stack_t w_4 3 ?x7917) ?x7012) $x5370 (= (used_gas_t w_4 3) (+ 3 (used_gas_t w_4 2))) $x7792 $x2326 $x607 $x11804 $x73 $x10191 $x58 $x8478 $x4281 (not (and $x5622 $x8838 $x6095 $x1996))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
(check-sat)
(get-proof)