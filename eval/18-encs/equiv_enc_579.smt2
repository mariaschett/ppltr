; PUSH cw_3 DUP2 SWAP1 DUP1 => DUP1 PUSH cw_3 PUSH cw_3
(set-option :produce-proofs true)
; 
(set-info :status unknown)
(declare-fun storage_t ((_ BitVec 256) (_ BitVec 256) Int (_ BitVec 256)) (_ BitVec 256))
(declare-fun w_3 () (_ BitVec 256))
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
 (exists ((x_0 (_ BitVec 256)) )(let (($x9370 (forall ((w (_ BitVec 256)) )(let ((?x8129 (storage_t x_0 w_3 3 w)))
 (let ((?x6953 (storage_s x_0 w_3 4 w)))
 (= ?x6953 ?x8129))))
 ))
 (let (($x2670 (exc_halt_t 3)))
 (let (($x7172 (exc_halt_s 4)))
 (let (($x5551 (= $x7172 $x2670)))
 (let (($x9673 (forall ((n (_ BitVec 6)) )(let ((?x10926 (sc_t 3)))
 (let (($x7865 (bvsle ?x10926 n)))
 (let ((?x5500 (stack_t x_0 w_3 3 n)))
 (let ((?x600 (stack_s x_0 w_3 4 n)))
 (let (($x4383 (= ?x600 ?x5500)))
 (or $x4383 $x7865)))))))
 ))
 (let ((?x10926 (sc_t 3)))
 (let ((?x4305 (sc_s 4)))
 (let (($x10313 (= ?x4305 ?x10926)))
 (let ((?x763 (used_gas_t x_0 w_3 0)))
 (let ((?x1352 (used_gas_s x_0 w_3 0)))
 (let (($x3868 (= ?x1352 ?x763)))
 (let (($x11126 (forall ((w (_ BitVec 256)) )(let ((?x8759 (storage_t x_0 w_3 0 w)))
 (let ((?x6191 (storage_s x_0 w_3 0 w)))
 (= ?x6191 ?x8759))))
 ))
 (let (($x56 (exc_halt_t 0)))
 (let (($x57 (exc_halt_s 0)))
 (let (($x58 (= $x57 $x56)))
 (let (($x5319 (forall ((n (_ BitVec 6)) )(let ((?x63 (sc_t 0)))
 (let (($x4230 (bvsle ?x63 n)))
 (let ((?x10742 (stack_t x_0 w_3 0 n)))
 (let ((?x8347 (stack_s x_0 w_3 0 n)))
 (let (($x2443 (= ?x8347 ?x10742)))
 (or $x2443 $x4230)))))))
 ))
 (let ((?x63 (sc_t 0)))
 (let ((?x72 (sc_s 0)))
 (let (($x73 (= ?x72 ?x63)))
 (let (($x2163 (exc_halt_t 2)))
 (let (($x5873 (or $x2163 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_t 2)))) (_ bv0 1))))))
 (let (($x1149 (= $x2670 $x5873)))
 (let (($x5865 (forall ((w (_ BitVec 256)) )(let ((?x7555 (storage_t x_0 w_3 2 w)))
 (let ((?x8129 (storage_t x_0 w_3 3 w)))
 (= ?x8129 ?x7555))))
 ))
 (let (($x1270 (forall ((n (_ BitVec 6)) )(let ((?x2714 (sc_t 2)))
 (let (($x9222 (bvsle ?x2714 n)))
 (or (= (stack_t x_0 w_3 3 n) (stack_t x_0 w_3 2 n)) $x9222))))
 ))
 (let (($x8329 (= ?x10926 (bvadd (_ bv1 6) (sc_t 2)))))
 (let (($x6782 (= (used_gas_t x_0 w_3 3) (+ 3 (used_gas_t x_0 w_3 2)))))
 (let (($x8101 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_t 1)))) (_ bv0 1)))))
 (let (($x8377 (exc_halt_t 1)))
 (let (($x8626 (= $x2163 (or $x8377 $x8101))))
 (let (($x6454 (forall ((w (_ BitVec 256)) )(let ((?x8401 (storage_t x_0 w_3 1 w)))
 (let ((?x7555 (storage_t x_0 w_3 2 w)))
 (= ?x7555 ?x8401))))
 ))
 (let (($x10159 (forall ((n (_ BitVec 6)) )(let ((?x7154 (sc_t 1)))
 (let (($x8298 (bvsle ?x7154 n)))
 (or $x8298 (= (stack_t x_0 w_3 2 n) (stack_t x_0 w_3 1 n))))))
 ))
 (let ((?x2714 (sc_t 2)))
 (let (($x12000 (= ?x2714 (bvadd (_ bv1 6) (sc_t 1)))))
 (let ((?x1860 (used_gas_t x_0 w_3 2)))
 (let (($x546 (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) ?x63)))))
 (let (($x370 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x63))) (_ bv0 1)))))
 (let (($x2236 (forall ((w (_ BitVec 256)) )(let ((?x8759 (storage_t x_0 w_3 0 w)))
 (let ((?x8401 (storage_t x_0 w_3 1 w)))
 (= ?x8401 ?x8759))))
 ))
 (let (($x366 (forall ((n (_ BitVec 6)) )(let ((?x63 (sc_t 0)))
 (let ((?x1564 (bvadd (_ bv63 6) ?x63)))
 (let (($x10843 (bvsle ?x1564 n)))
 (or (= (stack_t x_0 w_3 1 n) (stack_t x_0 w_3 0 n)) $x10843)))))
 ))
 (let ((?x7154 (sc_t 1)))
 (let (($x3285 (= ?x7154 (bvadd (_ bv1 6) ?x63))))
 (let ((?x1564 (bvadd (_ bv63 6) ?x63)))
 (let ((?x4318 (stack_t x_0 w_3 0 ?x1564)))
 (let (($x2499 (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_s 3))))))
 (let (($x292 (exc_halt_s 3)))
 (let (($x18 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 3)))) (_ bv0 1)))))
 (let (($x9769 (forall ((w (_ BitVec 256)) )(let ((?x4228 (storage_s x_0 w_3 3 w)))
 (let ((?x6953 (storage_s x_0 w_3 4 w)))
 (= ?x6953 ?x4228))))
 ))
 (let (($x10247 (forall ((n (_ BitVec 6)) )(let ((?x275 (sc_s 3)))
 (let ((?x10876 (bvadd (_ bv63 6) ?x275)))
 (let (($x6531 (bvsle ?x10876 n)))
 (or $x6531 (= (stack_s x_0 w_3 4 n) (stack_s x_0 w_3 3 n)))))))
 ))
 (let (($x3017 (= ?x4305 (bvadd (_ bv1 6) (sc_s 3)))))
 (let (($x4974 (= (used_gas_s x_0 w_3 4) (+ 3 (used_gas_s x_0 w_3 3)))))
 (let ((?x275 (sc_s 3)))
 (let ((?x10876 (bvadd (_ bv63 6) ?x275)))
 (let ((?x454 (stack_s x_0 w_3 3 ?x10876)))
 (let (($x8310 (= $x292 (or (exc_halt_s 2) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 2))))))))
 (let (($x3736 (forall ((w (_ BitVec 256)) )(let ((?x2543 (storage_s x_0 w_3 2 w)))
 (let ((?x4228 (storage_s x_0 w_3 3 w)))
 (= ?x4228 ?x2543))))
 ))
 (let (($x7895 (forall ((n (_ BitVec 6)) )(let ((?x218 (sc_s 2)))
 (let ((?x11299 (bvadd (_ bv62 6) ?x218)))
 (let (($x9433 (bvsle ?x11299 n)))
 (or (= (stack_s x_0 w_3 3 n) (stack_s x_0 w_3 2 n)) $x9433)))))
 ))
 (let ((?x218 (sc_s 2)))
 (let (($x7483 (= ?x275 ?x218)))
 (let ((?x682 (used_gas_s x_0 w_3 3)))
 (let ((?x4853 (bvadd (_ bv63 6) ?x218)))
 (let ((?x1094 (stack_s x_0 w_3 2 ?x4853)))
 (let (($x7994 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 1)))) (_ bv0 1)))))
 (let (($x4864 (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 1))))))
 (let (($x189 (exc_halt_s 1)))
 (let (($x247 (exc_halt_s 2)))
 (let (($x6137 (forall ((w (_ BitVec 256)) )(let ((?x6739 (storage_s x_0 w_3 1 w)))
 (let ((?x2543 (storage_s x_0 w_3 2 w)))
 (= ?x2543 ?x6739))))
 ))
 (let (($x2509 (forall ((n (_ BitVec 6)) )(let ((?x154 (sc_s 1)))
 (let ((?x9486 (bvadd (_ bv62 6) ?x154)))
 (let (($x9595 (bvsle ?x9486 n)))
 (or $x9595 (= (stack_s x_0 w_3 2 n) (stack_s x_0 w_3 1 n)))))))
 ))
 (let (($x8356 (= ?x218 (bvadd (_ bv1 6) (sc_s 1)))))
 (let ((?x10555 (used_gas_s x_0 w_3 2)))
 (let (($x1307 (= (stack_s x_0 w_3 2 (bvadd (_ bv63 6) (sc_s 1))) (stack_s x_0 w_3 1 (bvadd (_ bv63 6) (sc_s 1))))))
 (let ((?x154 (sc_s 1)))
 (let ((?x9486 (bvadd (_ bv62 6) ?x154)))
 (let ((?x7708 (stack_s x_0 w_3 1 ?x9486)))
 (let (($x1815 (= $x189 (or $x57 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x72))) (_ bv0 1)))))))
 (let (($x4360 (forall ((w (_ BitVec 256)) )(let ((?x6191 (storage_s x_0 w_3 0 w)))
 (let ((?x6739 (storage_s x_0 w_3 1 w)))
 (= ?x6739 ?x6191))))
 ))
 (let (($x7578 (forall ((n (_ BitVec 6)) )(let ((?x72 (sc_s 0)))
 (let (($x4037 (bvsle ?x72 n)))
 (or $x4037 (= (stack_s x_0 w_3 1 n) (stack_s x_0 w_3 0 n))))))
 ))
 (let (($x1117 (= ?x154 (bvadd (_ bv1 6) ?x72))))
 (let (($x6747 (forall ((w (_ BitVec 256)) )(let ((?x6191 (storage_s x_0 w_3 0 w)))
 (= ?x6191 (_ bv0 256))))
 ))
 (let (($x7915 (= ?x1352 0)))
 (let (($x5536 (not $x57)))
 (let (($x8635 (= (stack_s x_0 w_3 0 (_ bv0 6)) x_0)))
 (let (($x7315 (= ?x72 (_ bv1 6))))
 (and $x7315 $x8635 $x5536 $x7915 $x6747 (= (stack_s x_0 w_3 1 ?x72) w_3) (= (used_gas_s x_0 w_3 1) (+ 3 ?x1352)) $x1117 $x7578 $x4360 $x1815 (= ?x1094 ?x7708) (= (stack_s x_0 w_3 2 ?x9486) ?x7708) $x1307 (= ?x10555 (+ 3 (used_gas_s x_0 w_3 1))) $x8356 $x2509 $x6137 (= $x247 (or $x189 $x4864 $x7994)) (= ?x454 (stack_s x_0 w_3 2 (bvadd (_ bv62 6) ?x218))) (= (stack_s x_0 w_3 3 (bvadd (_ bv62 6) ?x275)) ?x1094) (= ?x682 (+ 3 ?x10555)) $x7483 $x7895 $x3736 $x8310 (= (stack_s x_0 w_3 4 (bvadd (_ bv63 6) ?x4305)) ?x454) (= (stack_s x_0 w_3 4 ?x10876) ?x454) $x4974 $x3017 $x10247 $x9769 (= $x7172 (or $x18 $x292 $x2499)) (= (stack_t x_0 w_3 1 (bvadd (_ bv63 6) ?x7154)) ?x4318) (= (stack_t x_0 w_3 1 ?x1564) ?x4318) (= (used_gas_t x_0 w_3 1) (+ 3 ?x763)) $x3285 $x366 $x2236 (= $x8377 (or $x56 $x370 $x546)) (= (stack_t x_0 w_3 2 ?x7154) w_3) (= ?x1860 (+ 3 (used_gas_t x_0 w_3 1))) $x12000 $x10159 $x6454 $x8626 (= (stack_t x_0 w_3 3 ?x2714) w_3) $x6782 $x8329 $x1270 $x5865 $x1149 $x73 $x5319 $x58 $x11126 $x3868 (not (and $x10313 $x9673 $x5551 $x9370)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
 )
(check-sat)
(get-proof)
