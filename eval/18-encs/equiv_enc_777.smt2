; PUSH cw_4 DUP1 CALLDATALOAD SWAP1 => PUSH cw_4 CALLDATALOAD PUSH cw_4
(set-option :produce-proofs true)
; 
(set-info :status unknown)
(declare-fun storage_t ((_ BitVec 256) (_ BitVec 256) Int (_ BitVec 256)) (_ BitVec 256))
(declare-fun w_4 () (_ BitVec 256))
(declare-fun storage_s ((_ BitVec 256) (_ BitVec 256) Int (_ BitVec 256)) (_ BitVec 256))
(declare-fun exc_halt_t (Int) Bool)
(declare-fun exc_halt_s (Int) Bool)
(declare-fun sc_t (Int) (_ BitVec 6))
(declare-fun stack_t ((_ BitVec 256) (_ BitVec 256) Int (_ BitVec 6)) (_ BitVec 256))
(declare-fun stack_s ((_ BitVec 256) (_ BitVec 256) Int (_ BitVec 6)) (_ BitVec 256))
(declare-fun sc_s (Int) (_ BitVec 6))
(declare-fun used_gas_t ((_ BitVec 256) (_ BitVec 256) Int) Int)
(declare-fun used_gas_s ((_ BitVec 256) (_ BitVec 256) Int) Int)
(declare-fun f_CALLDATALOAD ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256)) (_ BitVec 256))
(assert
 (exists ((x_CALLDATALOAD_0 (_ BitVec 256)) )(let (($x2337 (forall ((w (_ BitVec 256)) )(let ((?x6551 (storage_t w_4 x_CALLDATALOAD_0 3 w)))
 (let ((?x6289 (storage_s w_4 x_CALLDATALOAD_0 4 w)))
 (= ?x6289 ?x6551))))
 ))
 (let (($x4112 (exc_halt_t 3)))
 (let (($x9175 (exc_halt_s 4)))
 (let (($x11099 (= $x9175 $x4112)))
 (let (($x6995 (forall ((n (_ BitVec 6)) )(let ((?x11964 (sc_t 3)))
 (let (($x7010 (bvsle ?x11964 n)))
 (let ((?x2208 (stack_t w_4 x_CALLDATALOAD_0 3 n)))
 (let ((?x9929 (stack_s w_4 x_CALLDATALOAD_0 4 n)))
 (let (($x2801 (= ?x9929 ?x2208)))
 (or $x2801 $x7010)))))))
 ))
 (let ((?x11964 (sc_t 3)))
 (let ((?x9433 (sc_s 4)))
 (let (($x3597 (= ?x9433 ?x11964)))
 (let ((?x7246 (used_gas_t w_4 x_CALLDATALOAD_0 0)))
 (let ((?x1482 (used_gas_s w_4 x_CALLDATALOAD_0 0)))
 (let (($x11057 (= ?x1482 ?x7246)))
 (let (($x4203 (forall ((w (_ BitVec 256)) )(let ((?x5190 (storage_t w_4 x_CALLDATALOAD_0 0 w)))
 (let ((?x3945 (storage_s w_4 x_CALLDATALOAD_0 0 w)))
 (= ?x3945 ?x5190))))
 ))
 (let (($x56 (exc_halt_t 0)))
 (let (($x57 (exc_halt_s 0)))
 (let (($x58 (= $x57 $x56)))
 (let (($x5680 (forall ((n (_ BitVec 6)) )(let ((?x152 (stack_t w_4 x_CALLDATALOAD_0 0 n)))
 (let ((?x671 (stack_s w_4 x_CALLDATALOAD_0 0 n)))
 (let (($x10309 (= ?x671 ?x152)))
 (let ((?x63 (sc_t 0)))
 (let (($x721 (bvsle ?x63 n)))
 (or $x721 $x10309)))))))
 ))
 (let ((?x63 (sc_t 0)))
 (let ((?x72 (sc_s 0)))
 (let (($x73 (= ?x72 ?x63)))
 (let (($x8382 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_t 2)))) (_ bv0 1)))))
 (let (($x3706 (exc_halt_t 2)))
 (let (($x6032 (= $x4112 (or $x3706 $x8382))))
 (let (($x2301 (forall ((w (_ BitVec 256)) )(let ((?x9376 (storage_t w_4 x_CALLDATALOAD_0 2 w)))
 (let ((?x6551 (storage_t w_4 x_CALLDATALOAD_0 3 w)))
 (= ?x6551 ?x9376))))
 ))
 (let (($x7719 (forall ((n (_ BitVec 6)) )(let ((?x11248 (sc_t 2)))
 (let (($x3309 (bvsle ?x11248 n)))
 (or $x3309 (= (stack_t w_4 x_CALLDATALOAD_0 3 n) (stack_t w_4 x_CALLDATALOAD_0 2 n))))))
 ))
 (let (($x6697 (= ?x11964 (bvadd (_ bv1 6) (sc_t 2)))))
 (let (($x1218 (= (used_gas_t w_4 x_CALLDATALOAD_0 3) (+ 3 (used_gas_t w_4 x_CALLDATALOAD_0 2)))))
 (let (($x7247 (= $x3706 (or (exc_halt_t 1) (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_t 1))))))))
 (let (($x10078 (forall ((w (_ BitVec 256)) )(let ((?x1049 (storage_t w_4 x_CALLDATALOAD_0 1 w)))
 (let ((?x9376 (storage_t w_4 x_CALLDATALOAD_0 2 w)))
 (= ?x9376 ?x1049))))
 ))
 (let (($x5270 (forall ((n (_ BitVec 6)) )(let ((?x9666 (sc_t 1)))
 (let ((?x10947 (bvadd (_ bv63 6) ?x9666)))
 (let (($x2892 (bvsle ?x10947 n)))
 (or $x2892 (= (stack_t w_4 x_CALLDATALOAD_0 2 n) (stack_t w_4 x_CALLDATALOAD_0 1 n)))))))
 ))
 (let ((?x5471 (used_gas_t w_4 x_CALLDATALOAD_0 2)))
 (let ((?x2777 (f_CALLDATALOAD w_4 x_CALLDATALOAD_0 (stack_t w_4 x_CALLDATALOAD_0 1 (bvadd (_ bv63 6) (sc_t 1))))))
 (let (($x4852 (exc_halt_t 1)))
 (let (($x2020 (= $x4852 (or $x56 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x63))) (_ bv0 1)))))))
 (let (($x10698 (forall ((w (_ BitVec 256)) )(let ((?x5190 (storage_t w_4 x_CALLDATALOAD_0 0 w)))
 (let ((?x1049 (storage_t w_4 x_CALLDATALOAD_0 1 w)))
 (= ?x1049 ?x5190))))
 ))
 (let (($x1726 (forall ((n (_ BitVec 6)) )(let ((?x63 (sc_t 0)))
 (let (($x721 (bvsle ?x63 n)))
 (or $x721 (= (stack_t w_4 x_CALLDATALOAD_0 1 n) (stack_t w_4 x_CALLDATALOAD_0 0 n))))))
 ))
 (let ((?x9666 (sc_t 1)))
 (let (($x7504 (= ?x9666 (bvadd (_ bv1 6) ?x63))))
 (let (($x7542 (= $x9175 (or (exc_halt_s 3) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 3))))))))
 (let (($x7188 (forall ((w (_ BitVec 256)) )(let ((?x7963 (storage_s w_4 x_CALLDATALOAD_0 3 w)))
 (let ((?x6289 (storage_s w_4 x_CALLDATALOAD_0 4 w)))
 (= ?x6289 ?x7963))))
 ))
 (let (($x9476 (forall ((n (_ BitVec 6)) )(let ((?x3851 (sc_s 3)))
 (let ((?x6476 (bvadd (_ bv62 6) ?x3851)))
 (let (($x2289 (bvsle ?x6476 n)))
 (or $x2289 (= (stack_s w_4 x_CALLDATALOAD_0 4 n) (stack_s w_4 x_CALLDATALOAD_0 3 n)))))))
 ))
 (let ((?x3851 (sc_s 3)))
 (let (($x11584 (= ?x9433 ?x3851)))
 (let (($x10548 (= (used_gas_s w_4 x_CALLDATALOAD_0 4) (+ 3 (used_gas_s w_4 x_CALLDATALOAD_0 3)))))
 (let ((?x39 (bvadd (_ bv63 6) ?x3851)))
 (let ((?x11721 (stack_s w_4 x_CALLDATALOAD_0 3 ?x39)))
 (let (($x10563 (= (stack_s w_4 x_CALLDATALOAD_0 4 (bvadd (_ bv63 6) ?x9433)) (stack_s w_4 x_CALLDATALOAD_0 3 (bvadd (_ bv62 6) ?x3851)))))
 (let (($x8103 (exc_halt_s 3)))
 (let (($x8941 (= $x8103 (or (exc_halt_s 2) (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_s 2))))))))
 (let (($x4843 (forall ((w (_ BitVec 256)) )(let ((?x9670 (storage_s w_4 x_CALLDATALOAD_0 2 w)))
 (let ((?x7963 (storage_s w_4 x_CALLDATALOAD_0 3 w)))
 (= ?x7963 ?x9670))))
 ))
 (let (($x11906 (forall ((n (_ BitVec 6)) )(let ((?x2272 (sc_s 2)))
 (let ((?x7721 (bvadd (_ bv63 6) ?x2272)))
 (let (($x2092 (bvsle ?x7721 n)))
 (or $x2092 (= (stack_s w_4 x_CALLDATALOAD_0 3 n) (stack_s w_4 x_CALLDATALOAD_0 2 n)))))))
 ))
 (let ((?x2272 (sc_s 2)))
 (let (($x11383 (= ?x3851 ?x2272)))
 (let ((?x2755 (used_gas_s w_4 x_CALLDATALOAD_0 3)))
 (let ((?x7721 (bvadd (_ bv63 6) ?x2272)))
 (let ((?x9276 (stack_s w_4 x_CALLDATALOAD_0 2 ?x7721)))
 (let (($x10953 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 1)))) (_ bv0 1)))))
 (let (($x9515 (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_s 1))))))
 (let (($x8780 (exc_halt_s 1)))
 (let (($x10052 (exc_halt_s 2)))
 (let (($x9454 (forall ((w (_ BitVec 256)) )(let ((?x10402 (storage_s w_4 x_CALLDATALOAD_0 1 w)))
 (let ((?x9670 (storage_s w_4 x_CALLDATALOAD_0 2 w)))
 (= ?x9670 ?x10402))))
 ))
 (let (($x5327 (forall ((n (_ BitVec 6)) )(let ((?x154 (sc_s 1)))
 (let ((?x7012 (bvadd (_ bv63 6) ?x154)))
 (let (($x9070 (bvsle ?x7012 n)))
 (or (= (stack_s w_4 x_CALLDATALOAD_0 2 n) (stack_s w_4 x_CALLDATALOAD_0 1 n)) $x9070)))))
 ))
 (let (($x7440 (= ?x2272 (bvadd (_ bv1 6) (sc_s 1)))))
 (let ((?x10456 (used_gas_s w_4 x_CALLDATALOAD_0 2)))
 (let ((?x154 (sc_s 1)))
 (let ((?x7012 (bvadd (_ bv63 6) ?x154)))
 (let ((?x7001 (stack_s w_4 x_CALLDATALOAD_0 1 ?x7012)))
 (let (($x7229 (= $x8780 (or $x57 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x72))) (_ bv0 1)))))))
 (let (($x4302 (forall ((w (_ BitVec 256)) )(let ((?x3945 (storage_s w_4 x_CALLDATALOAD_0 0 w)))
 (let ((?x10402 (storage_s w_4 x_CALLDATALOAD_0 1 w)))
 (= ?x10402 ?x3945))))
 ))
 (let (($x7996 (forall ((n (_ BitVec 6)) )(let ((?x72 (sc_s 0)))
 (let (($x10877 (bvsle ?x72 n)))
 (or (= (stack_s w_4 x_CALLDATALOAD_0 1 n) (stack_s w_4 x_CALLDATALOAD_0 0 n)) $x10877))))
 ))
 (let (($x6284 (= ?x154 (bvadd (_ bv1 6) ?x72))))
 (let (($x4612 (forall ((w0 (_ BitVec 256)) )(let ((?x4199 (ite (= (stack_s w_4 x_CALLDATALOAD_0 2 (bvadd (_ bv63 6) (sc_s 2))) w0) x_CALLDATALOAD_0 (_ bv0 256))))
 (let ((?x10162 (f_CALLDATALOAD w_4 x_CALLDATALOAD_0 w0)))
 (= ?x10162 ?x4199))))
 ))
 (let (($x1486 (forall ((w (_ BitVec 256)) )(let ((?x3945 (storage_s w_4 x_CALLDATALOAD_0 0 w)))
 (= ?x3945 (_ bv0 256))))
 ))
 (let (($x5739 (= ?x1482 0)))
 (let (($x3720 (not $x57)))
 (let (($x4875 (= ?x72 (_ bv0 6))))
 (and $x4875 $x3720 $x5739 $x1486 $x4612 (= (stack_s w_4 x_CALLDATALOAD_0 1 ?x72) w_4) (= (used_gas_s w_4 x_CALLDATALOAD_0 1) (+ 3 ?x1482)) $x6284 $x7996 $x4302 $x7229 (= ?x9276 ?x7001) (= (stack_s w_4 x_CALLDATALOAD_0 2 ?x7012) ?x7001) (= ?x10456 (+ 3 (used_gas_s w_4 x_CALLDATALOAD_0 1))) $x7440 $x5327 $x9454 (= $x10052 (or $x8780 $x9515 $x10953)) (= ?x11721 (f_CALLDATALOAD w_4 x_CALLDATALOAD_0 ?x9276)) (= ?x2755 (+ 3 ?x10456)) $x11383 $x11906 $x4843 $x8941 $x10563 (= (stack_s w_4 x_CALLDATALOAD_0 4 (bvadd (_ bv62 6) ?x9433)) ?x11721) $x10548 $x11584 $x9476 $x7188 $x7542 (= (stack_t w_4 x_CALLDATALOAD_0 1 ?x63) w_4) (= (used_gas_t w_4 x_CALLDATALOAD_0 1) (+ 3 ?x7246)) $x7504 $x1726 $x10698 $x2020 (= (stack_t w_4 x_CALLDATALOAD_0 2 (bvadd (_ bv63 6) (sc_t 2))) ?x2777) (= ?x5471 (+ 3 (used_gas_t w_4 x_CALLDATALOAD_0 1))) (= (sc_t 2) ?x9666) $x5270 $x10078 $x7247 (= (stack_t w_4 x_CALLDATALOAD_0 3 (sc_t 2)) w_4) $x1218 $x6697 $x7719 $x2301 $x6032 $x73 $x5680 $x58 $x4203 $x11057 (not (and $x3597 $x6995 $x11099 $x2337)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
 )
(check-sat)
(get-proof)