; PUSH 0x00 DUP6 DUP5 SUB LT ISZERO => PUSH 0x01
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
 (exists ((x_0 (_ BitVec 256)) (x_1 (_ BitVec 256)) (x_2 (_ BitVec 256)) (x_3 (_ BitVec 256)) (x_4 (_ BitVec 256)) )(let (($x9500 (forall ((w (_ BitVec 256)) )(let ((?x11062 (storage_t x_0 x_1 x_2 x_3 x_4 1 w)))
 (let ((?x3192 (storage_s x_0 x_1 x_2 x_3 x_4 6 w)))
 (= ?x3192 ?x11062))))
 ))
 (let (($x3120 (exc_halt_t 1)))
 (let (($x772 (exc_halt_s 6)))
 (let (($x11034 (= $x772 $x3120)))
 (let (($x6506 (forall ((n (_ BitVec 6)) )(let ((?x6052 (sc_t 1)))
 (let (($x8432 (bvsle ?x6052 n)))
 (let ((?x11133 (stack_t x_0 x_1 x_2 x_3 x_4 1 n)))
 (let ((?x2112 (stack_s x_0 x_1 x_2 x_3 x_4 6 n)))
 (let (($x4864 (= ?x2112 ?x11133)))
 (or $x4864 $x8432)))))))
 ))
 (let ((?x6052 (sc_t 1)))
 (let ((?x926 (sc_s 6)))
 (let (($x3312 (= ?x926 ?x6052)))
 (let ((?x5835 (used_gas_t x_0 x_1 x_2 x_3 x_4 0)))
 (let ((?x8532 (used_gas_s x_0 x_1 x_2 x_3 x_4 0)))
 (let (($x11815 (= ?x8532 ?x5835)))
 (let (($x7366 (forall ((w (_ BitVec 256)) )(let ((?x9236 (storage_t x_0 x_1 x_2 x_3 x_4 0 w)))
 (let ((?x11153 (storage_s x_0 x_1 x_2 x_3 x_4 0 w)))
 (= ?x11153 ?x9236))))
 ))
 (let (($x56 (exc_halt_t 0)))
 (let (($x57 (exc_halt_s 0)))
 (let (($x58 (= $x57 $x56)))
 (let (($x3485 (forall ((n (_ BitVec 6)) )(let ((?x63 (sc_t 0)))
 (let (($x5025 (bvsle ?x63 n)))
 (let ((?x9272 (stack_t x_0 x_1 x_2 x_3 x_4 0 n)))
 (let ((?x8438 (stack_s x_0 x_1 x_2 x_3 x_4 0 n)))
 (let (($x3180 (= ?x8438 ?x9272)))
 (or $x3180 $x5025)))))))
 ))
 (let ((?x63 (sc_t 0)))
 (let ((?x72 (sc_s 0)))
 (let (($x73 (= ?x72 ?x63)))
 (let (($x4387 (= $x3120 (or $x56 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x63))) (_ bv0 1)))))))
 (let (($x10949 (forall ((w (_ BitVec 256)) )(let ((?x9236 (storage_t x_0 x_1 x_2 x_3 x_4 0 w)))
 (let ((?x11062 (storage_t x_0 x_1 x_2 x_3 x_4 1 w)))
 (= ?x11062 ?x9236))))
 ))
 (let (($x3541 (forall ((n (_ BitVec 6)) )(let ((?x9272 (stack_t x_0 x_1 x_2 x_3 x_4 0 n)))
 (let ((?x11133 (stack_t x_0 x_1 x_2 x_3 x_4 1 n)))
 (let ((?x63 (sc_t 0)))
 (let (($x5025 (bvsle ?x63 n)))
 (or $x5025 (= ?x11133 ?x9272)))))))
 ))
 (let (($x4139 (= ?x6052 (bvadd (_ bv1 6) ?x63))))
 (let (($x6497 (= $x772 (or (exc_halt_s 5) (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_s 5))))))))
 (let (($x1600 (forall ((w (_ BitVec 256)) )(let ((?x48 (storage_s x_0 x_1 x_2 x_3 x_4 5 w)))
 (let ((?x3192 (storage_s x_0 x_1 x_2 x_3 x_4 6 w)))
 (= ?x3192 ?x48))))
 ))
 (let (($x6350 (forall ((n (_ BitVec 6)) )(let ((?x7305 (stack_s x_0 x_1 x_2 x_3 x_4 5 n)))
 (let ((?x2112 (stack_s x_0 x_1 x_2 x_3 x_4 6 n)))
 (let ((?x805 (sc_s 5)))
 (let ((?x9431 (bvadd (_ bv63 6) ?x805)))
 (let (($x2706 (bvsle ?x9431 n)))
 (or $x2706 (= ?x2112 ?x7305))))))))
 ))
 (let ((?x805 (sc_s 5)))
 (let (($x6427 (= ?x926 ?x805)))
 (let (($x10820 (= (used_gas_s x_0 x_1 x_2 x_3 x_4 6) (+ 3 (used_gas_s x_0 x_1 x_2 x_3 x_4 5)))))
 (let ((?x5342 (ite (= (stack_s x_0 x_1 x_2 x_3 x_4 5 (bvadd (_ bv63 6) ?x805)) (_ bv0 256)) (_ bv1 256) (_ bv0 256))))
 (let (($x3979 (exc_halt_s 5)))
 (let (($x1289 (= $x3979 (or (exc_halt_s 4) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 4))))))))
 (let (($x7200 (forall ((w (_ BitVec 256)) )(let ((?x4232 (storage_s x_0 x_1 x_2 x_3 x_4 4 w)))
 (let ((?x48 (storage_s x_0 x_1 x_2 x_3 x_4 5 w)))
 (= ?x48 ?x4232))))
 ))
 (let (($x8437 (forall ((n (_ BitVec 6)) )(let ((?x11007 (stack_s x_0 x_1 x_2 x_3 x_4 4 n)))
 (let ((?x7305 (stack_s x_0 x_1 x_2 x_3 x_4 5 n)))
 (let ((?x4305 (sc_s 4)))
 (let ((?x5391 (bvadd (_ bv62 6) ?x4305)))
 (let (($x5921 (bvsle ?x5391 n)))
 (or $x5921 (= ?x7305 ?x11007))))))))
 ))
 (let ((?x4305 (sc_s 4)))
 (let ((?x1110 (bvadd (_ bv63 6) ?x4305)))
 (let (($x3594 (= ?x805 ?x1110)))
 (let ((?x7380 (used_gas_s x_0 x_1 x_2 x_3 x_4 5)))
 (let ((?x9619 (stack_s x_0 x_1 x_2 x_3 x_4 4 ?x1110)))
 (let (($x9694 (bvule (stack_s x_0 x_1 x_2 x_3 x_4 4 (bvadd (_ bv62 6) ?x4305)) ?x9619)))
 (let ((?x9431 (bvadd (_ bv63 6) ?x805)))
 (let ((?x4544 (stack_s x_0 x_1 x_2 x_3 x_4 5 ?x9431)))
 (let (($x7172 (exc_halt_s 4)))
 (let (($x8182 (= $x7172 (or (exc_halt_s 3) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 3))))))))
 (let (($x6299 (forall ((w (_ BitVec 256)) )(let ((?x5876 (storage_s x_0 x_1 x_2 x_3 x_4 3 w)))
 (let ((?x4232 (storage_s x_0 x_1 x_2 x_3 x_4 4 w)))
 (= ?x4232 ?x5876))))
 ))
 (let (($x2844 (forall ((n (_ BitVec 6)) )(let ((?x6185 (stack_s x_0 x_1 x_2 x_3 x_4 3 n)))
 (let ((?x11007 (stack_s x_0 x_1 x_2 x_3 x_4 4 n)))
 (let ((?x275 (sc_s 3)))
 (let ((?x7283 (bvadd (_ bv62 6) ?x275)))
 (let (($x49 (bvsle ?x7283 n)))
 (or $x49 (= ?x11007 ?x6185))))))))
 ))
 (let ((?x275 (sc_s 3)))
 (let ((?x7071 (bvadd (_ bv63 6) ?x275)))
 (let (($x10849 (= ?x4305 ?x7071)))
 (let ((?x9612 (used_gas_s x_0 x_1 x_2 x_3 x_4 4)))
 (let ((?x4294 (bvmul (_ bv115792089237316195423570985008687907853269984665640564039457584007913129639935 256) (stack_s x_0 x_1 x_2 x_3 x_4 3 (bvadd (_ bv62 6) ?x275)))))
 (let ((?x4161 (stack_s x_0 x_1 x_2 x_3 x_4 3 ?x7071)))
 (let (($x8482 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 2)))) (_ bv0 1)))))
 (let (($x247 (exc_halt_s 2)))
 (let (($x292 (exc_halt_s 3)))
 (let (($x9746 (forall ((w (_ BitVec 256)) )(let ((?x4430 (storage_s x_0 x_1 x_2 x_3 x_4 2 w)))
 (let ((?x5876 (storage_s x_0 x_1 x_2 x_3 x_4 3 w)))
 (= ?x5876 ?x4430))))
 ))
 (let (($x7921 (forall ((n (_ BitVec 6)) )(let ((?x2765 (stack_s x_0 x_1 x_2 x_3 x_4 2 n)))
 (let ((?x6185 (stack_s x_0 x_1 x_2 x_3 x_4 3 n)))
 (or (= ?x6185 ?x2765) (bvsle (bvadd (_ bv59 6) (sc_s 2)) n)))))
 ))
 (let (($x2652 (= ?x275 (bvadd (_ bv1 6) (sc_s 2)))))
 (let ((?x6781 (used_gas_s x_0 x_1 x_2 x_3 x_4 3)))
 (let ((?x218 (sc_s 2)))
 (let ((?x5148 (bvadd (_ bv63 6) ?x218)))
 (let ((?x3422 (stack_s x_0 x_1 x_2 x_3 x_4 2 ?x5148)))
 (let (($x445 (= (stack_s x_0 x_1 x_2 x_3 x_4 3 (bvadd (_ bv62 6) ?x218)) (stack_s x_0 x_1 x_2 x_3 x_4 2 (bvadd (_ bv62 6) ?x218)))))
 (let (($x3249 (= (stack_s x_0 x_1 x_2 x_3 x_4 3 (bvadd (_ bv61 6) ?x218)) (stack_s x_0 x_1 x_2 x_3 x_4 2 (bvadd (_ bv61 6) ?x218)))))
 (let (($x6624 (= (stack_s x_0 x_1 x_2 x_3 x_4 3 (bvadd (_ bv60 6) ?x218)) (stack_s x_0 x_1 x_2 x_3 x_4 2 (bvadd (_ bv60 6) ?x218)))))
 (let ((?x359 (bvadd (_ bv59 6) ?x218)))
 (let ((?x9092 (stack_s x_0 x_1 x_2 x_3 x_4 2 ?x359)))
 (let (($x2265 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 1)))) (_ bv0 1)))))
 (let (($x189 (exc_halt_s 1)))
 (let (($x8772 (forall ((w (_ BitVec 256)) )(let ((?x7933 (storage_s x_0 x_1 x_2 x_3 x_4 1 w)))
 (let ((?x4430 (storage_s x_0 x_1 x_2 x_3 x_4 2 w)))
 (= ?x4430 ?x7933))))
 ))
 (let (($x9530 (forall ((n (_ BitVec 6)) )(let ((?x11059 (stack_s x_0 x_1 x_2 x_3 x_4 1 n)))
 (let ((?x2765 (stack_s x_0 x_1 x_2 x_3 x_4 2 n)))
 (or (bvsle (bvadd (_ bv58 6) (sc_s 1)) n) (= ?x2765 ?x11059)))))
 ))
 (let (($x1555 (= ?x218 (bvadd (_ bv1 6) (sc_s 1)))))
 (let ((?x5766 (used_gas_s x_0 x_1 x_2 x_3 x_4 2)))
 (let (($x3911 (= (stack_s x_0 x_1 x_2 x_3 x_4 2 (bvadd (_ bv63 6) (sc_s 1))) (stack_s x_0 x_1 x_2 x_3 x_4 1 (bvadd (_ bv63 6) (sc_s 1))))))
 (let (($x9197 (= (stack_s x_0 x_1 x_2 x_3 x_4 2 (bvadd (_ bv62 6) (sc_s 1))) (stack_s x_0 x_1 x_2 x_3 x_4 1 (bvadd (_ bv62 6) (sc_s 1))))))
 (let (($x1293 (= (stack_s x_0 x_1 x_2 x_3 x_4 2 (bvadd (_ bv61 6) (sc_s 1))) (stack_s x_0 x_1 x_2 x_3 x_4 1 (bvadd (_ bv61 6) (sc_s 1))))))
 (let (($x1520 (= (stack_s x_0 x_1 x_2 x_3 x_4 2 (bvadd (_ bv60 6) (sc_s 1))) (stack_s x_0 x_1 x_2 x_3 x_4 1 (bvadd (_ bv60 6) (sc_s 1))))))
 (let (($x10355 (= (stack_s x_0 x_1 x_2 x_3 x_4 2 (bvadd (_ bv59 6) (sc_s 1))) (stack_s x_0 x_1 x_2 x_3 x_4 1 (bvadd (_ bv59 6) (sc_s 1))))))
 (let ((?x154 (sc_s 1)))
 (let ((?x6382 (bvadd (_ bv58 6) ?x154)))
 (let ((?x5770 (stack_s x_0 x_1 x_2 x_3 x_4 1 ?x6382)))
 (let (($x1654 (= $x189 (or $x57 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x72))) (_ bv0 1)))))))
 (let (($x6242 (forall ((w (_ BitVec 256)) )(let ((?x11153 (storage_s x_0 x_1 x_2 x_3 x_4 0 w)))
 (let ((?x7933 (storage_s x_0 x_1 x_2 x_3 x_4 1 w)))
 (= ?x7933 ?x11153))))
 ))
 (let (($x3874 (forall ((n (_ BitVec 6)) )(let ((?x8438 (stack_s x_0 x_1 x_2 x_3 x_4 0 n)))
 (let ((?x11059 (stack_s x_0 x_1 x_2 x_3 x_4 1 n)))
 (let ((?x72 (sc_s 0)))
 (let (($x3386 (bvsle ?x72 n)))
 (or $x3386 (= ?x11059 ?x8438)))))))
 ))
 (let (($x7749 (= ?x154 (bvadd (_ bv1 6) ?x72))))
 (let (($x11032 (forall ((w (_ BitVec 256)) )(let ((?x11153 (storage_s x_0 x_1 x_2 x_3 x_4 0 w)))
 (= ?x11153 (_ bv0 256))))
 ))
 (let (($x11026 (= ?x8532 0)))
 (let (($x5167 (not $x57)))
 (let (($x11048 (= (stack_s x_0 x_1 x_2 x_3 x_4 0 (_ bv4 6)) x_4)))
 (let (($x6119 (= (stack_s x_0 x_1 x_2 x_3 x_4 0 (_ bv3 6)) x_3)))
 (let (($x11078 (= (stack_s x_0 x_1 x_2 x_3 x_4 0 (_ bv2 6)) x_2)))
 (let (($x9605 (= (stack_s x_0 x_1 x_2 x_3 x_4 0 (_ bv1 6)) x_1)))
 (let (($x9317 (= (stack_s x_0 x_1 x_2 x_3 x_4 0 (_ bv0 6)) x_0)))
 (let (($x8002 (= ?x72 (_ bv5 6))))
 (and $x8002 $x9317 $x9605 $x11078 $x6119 $x11048 $x5167 $x11026 $x11032 (= (stack_s x_0 x_1 x_2 x_3 x_4 1 ?x72) (_ bv0 256)) (= (used_gas_s x_0 x_1 x_2 x_3 x_4 1) (+ 3 ?x8532)) $x7749 $x3874 $x6242 $x1654 (= ?x3422 ?x5770) (= (stack_s x_0 x_1 x_2 x_3 x_4 2 ?x6382) ?x5770) $x10355 $x1520 $x1293 $x9197 $x3911 (= ?x5766 (+ 3 (used_gas_s x_0 x_1 x_2 x_3 x_4 1))) $x1555 $x9530 $x8772 (= $x247 (or $x189 $x2265 (not (bvsle (_ bv0 6) ?x6382)))) (= ?x4161 ?x9092) (= (stack_s x_0 x_1 x_2 x_3 x_4 3 ?x359) ?x9092) $x6624 $x3249 $x445 (= (stack_s x_0 x_1 x_2 x_3 x_4 3 ?x5148) ?x3422) (= ?x6781 (+ 3 ?x5766)) $x2652 $x7921 $x9746 (= $x292 (or $x247 (not (bvsle (_ bv0 6) ?x359)) $x8482)) (= ?x9619 (bvadd ?x4161 ?x4294)) (= ?x9612 (+ 3 ?x6781)) $x10849 $x2844 $x6299 $x8182 (= ?x4544 (ite $x9694 (_ bv0 256) (_ bv1 256))) (= ?x7380 (+ 3 ?x9612)) $x3594 $x8437 $x7200 $x1289 (= (stack_s x_0 x_1 x_2 x_3 x_4 6 (bvadd (_ bv63 6) ?x926)) ?x5342) $x10820 $x6427 $x6350 $x1600 $x6497 (= (stack_t x_0 x_1 x_2 x_3 x_4 1 ?x63) (_ bv1 256)) (= (used_gas_t x_0 x_1 x_2 x_3 x_4 1) (+ 3 ?x5835)) $x4139 $x3541 $x10949 $x4387 $x73 $x3485 $x58 $x7366 $x11815 (not (and $x3312 $x6506 $x11034 $x9500))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
 )
(check-sat)
(get-proof)