; DUP4 PUSH cw_1 ADD DUP2 SWAP1 => DUP1 DUP5 PUSH cw_1 ADD
(set-option :produce-proofs true)
; 
(set-info :status unknown)
(declare-fun storage_t ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 256)) (_ BitVec 256))
(declare-fun w_1 () (_ BitVec 256))
(declare-fun storage_s ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 256)) (_ BitVec 256))
(declare-fun exc_halt_t (Int) Bool)
(declare-fun exc_halt_s (Int) Bool)
(declare-fun stack_t ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 6)) (_ BitVec 256))
(declare-fun stack_s ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 6)) (_ BitVec 256))
(declare-fun sc_t (Int) (_ BitVec 6))
(declare-fun sc_s (Int) (_ BitVec 6))
(declare-fun used_gas_t ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int) Int)
(declare-fun used_gas_s ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int) Int)
(assert
 (exists ((x_0 (_ BitVec 256)) (x_1 (_ BitVec 256)) (x_2 (_ BitVec 256)) (x_3 (_ BitVec 256)) )(let (($x8069 (forall ((w (_ BitVec 256)) )(let ((?x8852 (storage_t x_0 x_1 x_2 x_3 w_1 4 w)))
 (let ((?x3756 (storage_s x_0 x_1 x_2 x_3 w_1 5 w)))
 (= ?x3756 ?x8852))))
 ))
 (let (($x3723 (exc_halt_t 4)))
 (let (($x11317 (exc_halt_s 5)))
 (let (($x7362 (= $x11317 $x3723)))
 (let (($x7353 (forall ((n (_ BitVec 6)) )(let ((?x5776 (stack_t x_0 x_1 x_2 x_3 w_1 4 n)))
 (let ((?x2472 (stack_s x_0 x_1 x_2 x_3 w_1 5 n)))
 (let (($x11285 (= ?x2472 ?x5776)))
 (let ((?x1098 (sc_t 4)))
 (let (($x8721 (bvsle ?x1098 n)))
 (or $x8721 $x11285)))))))
 ))
 (let ((?x1098 (sc_t 4)))
 (let ((?x4319 (sc_s 5)))
 (let (($x11451 (= ?x4319 ?x1098)))
 (let ((?x7655 (used_gas_t x_0 x_1 x_2 x_3 w_1 0)))
 (let ((?x2218 (used_gas_s x_0 x_1 x_2 x_3 w_1 0)))
 (let (($x6326 (= ?x2218 ?x7655)))
 (let (($x11223 (forall ((w (_ BitVec 256)) )(let ((?x3635 (storage_t x_0 x_1 x_2 x_3 w_1 0 w)))
 (let ((?x3882 (storage_s x_0 x_1 x_2 x_3 w_1 0 w)))
 (= ?x3882 ?x3635))))
 ))
 (let (($x56 (exc_halt_t 0)))
 (let (($x57 (exc_halt_s 0)))
 (let (($x58 (= $x57 $x56)))
 (let (($x3066 (forall ((n (_ BitVec 6)) )(let ((?x63 (sc_t 0)))
 (let (($x7453 (bvsle ?x63 n)))
 (let ((?x1158 (stack_t x_0 x_1 x_2 x_3 w_1 0 n)))
 (let ((?x5640 (stack_s x_0 x_1 x_2 x_3 w_1 0 n)))
 (let (($x3073 (= ?x5640 ?x1158)))
 (or $x3073 $x7453)))))))
 ))
 (let ((?x63 (sc_t 0)))
 (let ((?x72 (sc_s 0)))
 (let (($x73 (= ?x72 ?x63)))
 (let (($x7275 (= $x3723 (or (exc_halt_t 3) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_t 3))))))))
 (let (($x7910 (forall ((w (_ BitVec 256)) )(let ((?x348 (storage_t x_0 x_1 x_2 x_3 w_1 3 w)))
 (let ((?x8852 (storage_t x_0 x_1 x_2 x_3 w_1 4 w)))
 (= ?x8852 ?x348))))
 ))
 (let (($x5843 (forall ((n (_ BitVec 6)) )(let ((?x6438 (sc_t 3)))
 (let ((?x9210 (bvadd (_ bv62 6) ?x6438)))
 (let (($x6607 (bvsle ?x9210 n)))
 (let ((?x7423 (stack_t x_0 x_1 x_2 x_3 w_1 3 n)))
 (let ((?x5776 (stack_t x_0 x_1 x_2 x_3 w_1 4 n)))
 (or (= ?x5776 ?x7423) $x6607)))))))
 ))
 (let ((?x6438 (sc_t 3)))
 (let ((?x9822 (bvadd (_ bv63 6) ?x6438)))
 (let (($x5696 (= ?x1098 ?x9822)))
 (let (($x5269 (= (used_gas_t x_0 x_1 x_2 x_3 w_1 4) (+ 3 (used_gas_t x_0 x_1 x_2 x_3 w_1 3)))))
 (let ((?x1843 (bvadd (stack_t x_0 x_1 x_2 x_3 w_1 3 ?x9822) (stack_t x_0 x_1 x_2 x_3 w_1 3 (bvadd (_ bv62 6) ?x6438)))))
 (let (($x8259 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_t 2)))) (_ bv0 1)))))
 (let (($x5252 (exc_halt_t 2)))
 (let (($x6783 (exc_halt_t 3)))
 (let (($x8671 (= $x6783 (or $x5252 $x8259))))
 (let (($x279 (forall ((w (_ BitVec 256)) )(let ((?x11355 (storage_t x_0 x_1 x_2 x_3 w_1 2 w)))
 (let ((?x348 (storage_t x_0 x_1 x_2 x_3 w_1 3 w)))
 (= ?x348 ?x11355))))
 ))
 (let (($x3497 (forall ((n (_ BitVec 6)) )(let ((?x6417 (stack_t x_0 x_1 x_2 x_3 w_1 2 n)))
 (let ((?x7423 (stack_t x_0 x_1 x_2 x_3 w_1 3 n)))
 (let ((?x2714 (sc_t 2)))
 (let (($x552 (bvsle ?x2714 n)))
 (or $x552 (= ?x7423 ?x6417)))))))
 ))
 (let (($x8645 (= ?x6438 (bvadd (_ bv1 6) (sc_t 2)))))
 (let ((?x3498 (used_gas_t x_0 x_1 x_2 x_3 w_1 3)))
 (let (($x3508 (exc_halt_t 1)))
 (let (($x1340 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_t 1)))) (_ bv0 1)))))
 (let (($x806 (not (bvsle (_ bv0 6) (bvadd (_ bv59 6) (sc_t 1))))))
 (let (($x2903 (= $x5252 (or $x806 $x1340 $x3508))))
 (let (($x9410 (forall ((w (_ BitVec 256)) )(let ((?x1047 (storage_t x_0 x_1 x_2 x_3 w_1 1 w)))
 (let ((?x11355 (storage_t x_0 x_1 x_2 x_3 w_1 2 w)))
 (= ?x11355 ?x1047))))
 ))
 (let (($x7267 (forall ((n (_ BitVec 6)) )(let ((?x8347 (sc_t 1)))
 (let ((?x6800 (bvadd (_ bv59 6) ?x8347)))
 (let (($x6777 (bvsle ?x6800 n)))
 (let ((?x4332 (stack_t x_0 x_1 x_2 x_3 w_1 1 n)))
 (let ((?x6417 (stack_t x_0 x_1 x_2 x_3 w_1 2 n)))
 (or (= ?x6417 ?x4332) $x6777)))))))
 ))
 (let ((?x2714 (sc_t 2)))
 (let (($x2756 (= ?x2714 (bvadd (_ bv1 6) (sc_t 1)))))
 (let ((?x5376 (used_gas_t x_0 x_1 x_2 x_3 w_1 2)))
 (let ((?x8347 (sc_t 1)))
 (let ((?x5756 (bvadd (_ bv63 6) ?x8347)))
 (let ((?x2789 (stack_t x_0 x_1 x_2 x_3 w_1 1 ?x5756)))
 (let (($x1978 (= (stack_t x_0 x_1 x_2 x_3 w_1 2 (bvadd (_ bv62 6) ?x8347)) (stack_t x_0 x_1 x_2 x_3 w_1 1 (bvadd (_ bv62 6) ?x8347)))))
 (let (($x7798 (= (stack_t x_0 x_1 x_2 x_3 w_1 2 (bvadd (_ bv61 6) ?x8347)) (stack_t x_0 x_1 x_2 x_3 w_1 1 (bvadd (_ bv61 6) ?x8347)))))
 (let (($x8303 (= (stack_t x_0 x_1 x_2 x_3 w_1 2 (bvadd (_ bv60 6) ?x8347)) (stack_t x_0 x_1 x_2 x_3 w_1 1 (bvadd (_ bv60 6) ?x8347)))))
 (let ((?x6800 (bvadd (_ bv59 6) ?x8347)))
 (let ((?x6524 (stack_t x_0 x_1 x_2 x_3 w_1 1 ?x6800)))
 (let (($x3118 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x63))) (_ bv0 1)))))
 (let (($x11245 (= $x3508 (or $x56 $x3118 (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) ?x63)))))))
 (let (($x2017 (forall ((w (_ BitVec 256)) )(let ((?x3635 (storage_t x_0 x_1 x_2 x_3 w_1 0 w)))
 (let ((?x1047 (storage_t x_0 x_1 x_2 x_3 w_1 1 w)))
 (= ?x1047 ?x3635))))
 ))
 (let (($x9220 (forall ((n (_ BitVec 6)) )(let ((?x63 (sc_t 0)))
 (let ((?x1747 (bvadd (_ bv63 6) ?x63)))
 (let (($x7777 (bvsle ?x1747 n)))
 (let ((?x1158 (stack_t x_0 x_1 x_2 x_3 w_1 0 n)))
 (let ((?x4332 (stack_t x_0 x_1 x_2 x_3 w_1 1 n)))
 (or (= ?x4332 ?x1158) $x7777)))))))
 ))
 (let (($x6213 (= ?x8347 (bvadd (_ bv1 6) ?x63))))
 (let ((?x1747 (bvadd (_ bv63 6) ?x63)))
 (let ((?x8016 (stack_t x_0 x_1 x_2 x_3 w_1 0 ?x1747)))
 (let (($x5022 (= $x11317 (or (exc_halt_s 4) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 4))))))))
 (let (($x473 (forall ((w (_ BitVec 256)) )(let ((?x5701 (storage_s x_0 x_1 x_2 x_3 w_1 4 w)))
 (let ((?x3756 (storage_s x_0 x_1 x_2 x_3 w_1 5 w)))
 (= ?x3756 ?x5701))))
 ))
 (let (($x10496 (forall ((n (_ BitVec 6)) )(let ((?x4305 (sc_s 4)))
 (let ((?x9984 (bvadd (_ bv62 6) ?x4305)))
 (let (($x10287 (bvsle ?x9984 n)))
 (let ((?x10763 (stack_s x_0 x_1 x_2 x_3 w_1 4 n)))
 (let ((?x2472 (stack_s x_0 x_1 x_2 x_3 w_1 5 n)))
 (or (= ?x2472 ?x10763) $x10287)))))))
 ))
 (let ((?x4305 (sc_s 4)))
 (let (($x15 (= ?x4319 ?x4305)))
 (let (($x2094 (= (used_gas_s x_0 x_1 x_2 x_3 w_1 5) (+ 3 (used_gas_s x_0 x_1 x_2 x_3 w_1 4)))))
 (let ((?x805 (bvadd (_ bv63 6) ?x4305)))
 (let ((?x10438 (stack_s x_0 x_1 x_2 x_3 w_1 4 ?x805)))
 (let (($x1791 (= (stack_s x_0 x_1 x_2 x_3 w_1 5 (bvadd (_ bv63 6) ?x4319)) (stack_s x_0 x_1 x_2 x_3 w_1 4 (bvadd (_ bv62 6) ?x4305)))))
 (let (($x4383 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 3)))) (_ bv0 1)))))
 (let (($x837 (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 3))))))
 (let (($x292 (exc_halt_s 3)))
 (let (($x7172 (exc_halt_s 4)))
 (let (($x9721 (forall ((w (_ BitVec 256)) )(let ((?x1719 (storage_s x_0 x_1 x_2 x_3 w_1 3 w)))
 (let ((?x5701 (storage_s x_0 x_1 x_2 x_3 w_1 4 w)))
 (= ?x5701 ?x1719))))
 ))
 (let (($x4821 (forall ((n (_ BitVec 6)) )(let ((?x5303 (stack_s x_0 x_1 x_2 x_3 w_1 3 n)))
 (let ((?x10763 (stack_s x_0 x_1 x_2 x_3 w_1 4 n)))
 (let ((?x275 (sc_s 3)))
 (let ((?x9043 (bvadd (_ bv62 6) ?x275)))
 (let (($x6666 (bvsle ?x9043 n)))
 (or $x6666 (= ?x10763 ?x5303))))))))
 ))
 (let (($x11555 (= ?x4305 (bvadd (_ bv1 6) (sc_s 3)))))
 (let ((?x10248 (used_gas_s x_0 x_1 x_2 x_3 w_1 4)))
 (let ((?x275 (sc_s 3)))
 (let ((?x9812 (bvadd (_ bv63 6) ?x275)))
 (let ((?x8443 (stack_s x_0 x_1 x_2 x_3 w_1 3 ?x9812)))
 (let ((?x9043 (bvadd (_ bv62 6) ?x275)))
 (let ((?x4064 (stack_s x_0 x_1 x_2 x_3 w_1 3 ?x9043)))
 (let (($x6649 (= $x292 (or (exc_halt_s 2) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 2))))))))
 (let (($x7422 (forall ((w (_ BitVec 256)) )(let ((?x6806 (storage_s x_0 x_1 x_2 x_3 w_1 2 w)))
 (let ((?x1719 (storage_s x_0 x_1 x_2 x_3 w_1 3 w)))
 (= ?x1719 ?x6806))))
 ))
 (let (($x4089 (forall ((n (_ BitVec 6)) )(let ((?x218 (sc_s 2)))
 (let ((?x2513 (bvadd (_ bv62 6) ?x218)))
 (let (($x4614 (bvsle ?x2513 n)))
 (let ((?x4250 (stack_s x_0 x_1 x_2 x_3 w_1 2 n)))
 (let ((?x5303 (stack_s x_0 x_1 x_2 x_3 w_1 3 n)))
 (or (= ?x5303 ?x4250) $x4614)))))))
 ))
 (let ((?x218 (sc_s 2)))
 (let ((?x7855 (bvadd (_ bv63 6) ?x218)))
 (let (($x6590 (= ?x275 ?x7855)))
 (let ((?x8682 (used_gas_s x_0 x_1 x_2 x_3 w_1 3)))
 (let ((?x7347 (bvadd (stack_s x_0 x_1 x_2 x_3 w_1 2 ?x7855) (stack_s x_0 x_1 x_2 x_3 w_1 2 (bvadd (_ bv62 6) ?x218)))))
 (let (($x8154 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 1)))) (_ bv0 1)))))
 (let (($x189 (exc_halt_s 1)))
 (let (($x247 (exc_halt_s 2)))
 (let (($x961 (= $x247 (or $x189 $x8154))))
 (let (($x9527 (forall ((w (_ BitVec 256)) )(let ((?x10591 (storage_s x_0 x_1 x_2 x_3 w_1 1 w)))
 (let ((?x6806 (storage_s x_0 x_1 x_2 x_3 w_1 2 w)))
 (= ?x6806 ?x10591))))
 ))
 (let (($x8542 (forall ((n (_ BitVec 6)) )(let ((?x10889 (stack_s x_0 x_1 x_2 x_3 w_1 1 n)))
 (let ((?x4250 (stack_s x_0 x_1 x_2 x_3 w_1 2 n)))
 (let ((?x154 (sc_s 1)))
 (let (($x1556 (bvsle ?x154 n)))
 (or $x1556 (= ?x4250 ?x10889)))))))
 ))
 (let (($x1362 (= ?x218 (bvadd (_ bv1 6) (sc_s 1)))))
 (let ((?x1922 (used_gas_s x_0 x_1 x_2 x_3 w_1 2)))
 (let (($x10528 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x72))) (_ bv0 1)))))
 (let (($x7546 (not (bvsle (_ bv0 6) (bvadd (_ bv60 6) ?x72)))))
 (let (($x8307 (= $x189 (or $x57 $x7546 $x10528))))
 (let (($x5394 (forall ((w (_ BitVec 256)) )(let ((?x3882 (storage_s x_0 x_1 x_2 x_3 w_1 0 w)))
 (let ((?x10591 (storage_s x_0 x_1 x_2 x_3 w_1 1 w)))
 (= ?x10591 ?x3882))))
 ))
 (let (($x4140 (forall ((n (_ BitVec 6)) )(let ((?x5640 (stack_s x_0 x_1 x_2 x_3 w_1 0 n)))
 (let ((?x10889 (stack_s x_0 x_1 x_2 x_3 w_1 1 n)))
 (let ((?x72 (sc_s 0)))
 (let ((?x1360 (bvadd (_ bv60 6) ?x72)))
 (let (($x1879 (bvsle ?x1360 n)))
 (or $x1879 (= ?x10889 ?x5640))))))))
 ))
 (let ((?x154 (sc_s 1)))
 (let (($x11639 (= ?x154 (bvadd (_ bv1 6) ?x72))))
 (let (($x6104 (= (stack_s x_0 x_1 x_2 x_3 w_1 1 (bvadd (_ bv63 6) ?x72)) (stack_s x_0 x_1 x_2 x_3 w_1 0 (bvadd (_ bv63 6) ?x72)))))
 (let (($x9743 (= (stack_s x_0 x_1 x_2 x_3 w_1 1 (bvadd (_ bv62 6) ?x72)) (stack_s x_0 x_1 x_2 x_3 w_1 0 (bvadd (_ bv62 6) ?x72)))))
 (let (($x5236 (= (stack_s x_0 x_1 x_2 x_3 w_1 1 (bvadd (_ bv61 6) ?x72)) (stack_s x_0 x_1 x_2 x_3 w_1 0 (bvadd (_ bv61 6) ?x72)))))
 (let ((?x1360 (bvadd (_ bv60 6) ?x72)))
 (let ((?x2192 (stack_s x_0 x_1 x_2 x_3 w_1 0 ?x1360)))
 (let (($x10666 (forall ((w (_ BitVec 256)) )(let ((?x3882 (storage_s x_0 x_1 x_2 x_3 w_1 0 w)))
 (= ?x3882 (_ bv0 256))))
 ))
 (let (($x8629 (= ?x2218 0)))
 (let (($x6901 (not $x57)))
 (let (($x8518 (= (stack_s x_0 x_1 x_2 x_3 w_1 0 (_ bv3 6)) x_3)))
 (let (($x6448 (= (stack_s x_0 x_1 x_2 x_3 w_1 0 (_ bv2 6)) x_2)))
 (let (($x7478 (= (stack_s x_0 x_1 x_2 x_3 w_1 0 (_ bv1 6)) x_1)))
 (let (($x7228 (= (stack_s x_0 x_1 x_2 x_3 w_1 0 (_ bv0 6)) x_0)))
 (let (($x289 (= ?x72 (_ bv4 6))))
 (and $x289 $x7228 $x7478 $x6448 $x8518 $x6901 $x8629 $x10666 (= (stack_s x_0 x_1 x_2 x_3 w_1 1 (bvadd (_ bv63 6) ?x154)) ?x2192) (= (stack_s x_0 x_1 x_2 x_3 w_1 1 ?x1360) ?x2192) $x5236 $x9743 $x6104 (= (used_gas_s x_0 x_1 x_2 x_3 w_1 1) (+ 3 ?x2218)) $x11639 $x4140 $x5394 $x8307 (= (stack_s x_0 x_1 x_2 x_3 w_1 2 ?x154) w_1) (= ?x1922 (+ 3 (used_gas_s x_0 x_1 x_2 x_3 w_1 1))) $x1362 $x8542 $x9527 $x961 (= ?x8443 ?x7347) (= ?x8682 (+ 3 ?x1922)) $x6590 $x4089 $x7422 $x6649 (= ?x10438 ?x4064) (= (stack_s x_0 x_1 x_2 x_3 w_1 4 ?x9043) ?x4064) (= (stack_s x_0 x_1 x_2 x_3 w_1 4 ?x9812) ?x8443) (= ?x10248 (+ 3 ?x8682)) $x11555 $x4821 $x9721 (= $x7172 (or $x292 $x837 $x4383)) $x1791 (= (stack_s x_0 x_1 x_2 x_3 w_1 5 (bvadd (_ bv62 6) ?x4319)) ?x10438) $x2094 $x15 $x10496 $x473 $x5022 (= ?x2789 ?x8016) (= (stack_t x_0 x_1 x_2 x_3 w_1 1 ?x1747) ?x8016) (= (used_gas_t x_0 x_1 x_2 x_3 w_1 1) (+ 3 ?x7655)) $x6213 $x9220 $x2017 $x11245 (= (stack_t x_0 x_1 x_2 x_3 w_1 2 (bvadd (_ bv63 6) ?x2714)) ?x6524) (= (stack_t x_0 x_1 x_2 x_3 w_1 2 ?x6800) ?x6524) $x8303 $x7798 $x1978 (= (stack_t x_0 x_1 x_2 x_3 w_1 2 ?x5756) ?x2789) (= ?x5376 (+ 3 (used_gas_t x_0 x_1 x_2 x_3 w_1 1))) $x2756 $x7267 $x9410 $x2903 (= (stack_t x_0 x_1 x_2 x_3 w_1 3 ?x2714) w_1) (= ?x3498 (+ 3 ?x5376)) $x8645 $x3497 $x279 $x8671 (= (stack_t x_0 x_1 x_2 x_3 w_1 4 (bvadd (_ bv63 6) ?x1098)) ?x1843) $x5269 $x5696 $x5843 $x7910 $x7275 $x73 $x3066 $x58 $x11223 $x6326 (not (and $x11451 $x7353 $x7362 $x8069)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
 )
(check-sat)
(get-proof)