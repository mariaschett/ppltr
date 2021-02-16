; PUSH cw_2 CALLDATASIZE SWAP1 DUP1 => CALLDATASIZE PUSH cw_2 PUSH cw_2
(set-option :produce-proofs true)
; 
(set-info :status unknown)
(declare-fun storage_t ((_ BitVec 256) (_ BitVec 256) Int (_ BitVec 256)) (_ BitVec 256))
(declare-fun w_2 () (_ BitVec 256))
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
 (exists ((x_CALLDATASIZE (_ BitVec 256)) )(let (($x8004 (forall ((w (_ BitVec 256)) )(let ((?x3750 (storage_t w_2 x_CALLDATASIZE 3 w)))
 (let ((?x10716 (storage_s w_2 x_CALLDATASIZE 4 w)))
 (= ?x10716 ?x3750))))
 ))
 (let (($x10336 (exc_halt_t 3)))
 (let (($x64 (exc_halt_s 4)))
 (let (($x6232 (= $x64 $x10336)))
 (let (($x5578 (forall ((n (_ BitVec 6)) )(let ((?x10698 (stack_t w_2 x_CALLDATASIZE 3 n)))
 (let ((?x10695 (stack_s w_2 x_CALLDATASIZE 4 n)))
 (let (($x10701 (= ?x10695 ?x10698)))
 (or (bvsle (sc_t 3) n) $x10701)))))
 ))
 (let ((?x2012 (sc_t 3)))
 (let ((?x4305 (sc_s 4)))
 (let (($x2319 (= ?x4305 ?x2012)))
 (let ((?x4963 (used_gas_t w_2 x_CALLDATASIZE 0)))
 (let ((?x4409 (used_gas_s w_2 x_CALLDATASIZE 0)))
 (let (($x10693 (= ?x4409 ?x4963)))
 (let (($x8459 (forall ((w (_ BitVec 256)) )(let ((?x10690 (storage_t w_2 x_CALLDATASIZE 0 w)))
 (let ((?x8342 (storage_s w_2 x_CALLDATASIZE 0 w)))
 (= ?x8342 ?x10690))))
 ))
 (let (($x56 (exc_halt_t 0)))
 (let (($x57 (exc_halt_s 0)))
 (let (($x58 (= $x57 $x56)))
 (let (($x8156 (forall ((n (_ BitVec 6)) )(let ((?x63 (sc_t 0)))
 (let (($x2733 (bvsle ?x63 n)))
 (let ((?x1865 (stack_t w_2 x_CALLDATASIZE 0 n)))
 (let ((?x8546 (stack_s w_2 x_CALLDATASIZE 0 n)))
 (let (($x1425 (= ?x8546 ?x1865)))
 (or $x1425 $x2733)))))))
 ))
 (let ((?x63 (sc_t 0)))
 (let ((?x72 (sc_s 0)))
 (let (($x73 (= ?x72 ?x63)))
 (let (($x903 (exc_halt_t 2)))
 (let (($x2423 (or $x903 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_t 2)))) (_ bv0 1))))))
 (let (($x6614 (forall ((w (_ BitVec 256)) )(let ((?x8032 (storage_t w_2 x_CALLDATASIZE 2 w)))
 (let ((?x3750 (storage_t w_2 x_CALLDATASIZE 3 w)))
 (= ?x3750 ?x8032))))
 ))
 (let (($x8222 (forall ((n (_ BitVec 6)) )(let ((?x4056 (sc_t 2)))
 (let (($x8328 (bvsle ?x4056 n)))
 (or $x8328 (= (stack_t w_2 x_CALLDATASIZE 3 n) (stack_t w_2 x_CALLDATASIZE 2 n))))))
 ))
 (let (($x3949 (= (used_gas_t w_2 x_CALLDATASIZE 3) (+ 3 (used_gas_t w_2 x_CALLDATASIZE 2)))))
 (let (($x10002 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_t 1)))) (_ bv0 1)))))
 (let (($x1920 (exc_halt_t 1)))
 (let (($x5051 (forall ((w (_ BitVec 256)) )(let ((?x7147 (storage_t w_2 x_CALLDATASIZE 1 w)))
 (let ((?x8032 (storage_t w_2 x_CALLDATASIZE 2 w)))
 (= ?x8032 ?x7147))))
 ))
 (let (($x7428 (forall ((n (_ BitVec 6)) )(or (bvsle (sc_t 1) n) (= (stack_t w_2 x_CALLDATASIZE 2 n) (stack_t w_2 x_CALLDATASIZE 1 n))))
 ))
 (let ((?x4056 (sc_t 2)))
 (let (($x423 (= ?x4056 (bvadd (_ bv1 6) (sc_t 1)))))
 (let ((?x1310 (used_gas_t w_2 x_CALLDATASIZE 2)))
 (let (($x1160 (= $x1920 (or $x56 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x63))) (_ bv0 1)))))))
 (let (($x3881 (forall ((w (_ BitVec 256)) )(let ((?x10690 (storage_t w_2 x_CALLDATASIZE 0 w)))
 (let ((?x7147 (storage_t w_2 x_CALLDATASIZE 1 w)))
 (= ?x7147 ?x10690))))
 ))
 (let (($x3930 (forall ((n (_ BitVec 6)) )(let ((?x63 (sc_t 0)))
 (let (($x2733 (bvsle ?x63 n)))
 (or (= (stack_t w_2 x_CALLDATASIZE 1 n) (stack_t w_2 x_CALLDATASIZE 0 n)) $x2733))))
 ))
 (let ((?x4023 (sc_t 1)))
 (let (($x7084 (= ?x4023 (bvadd (_ bv1 6) ?x63))))
 (let (($x6601 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 3)))) (_ bv0 1)))))
 (let (($x292 (exc_halt_s 3)))
 (let (($x1568 (forall ((w (_ BitVec 256)) )(let ((?x9465 (storage_s w_2 x_CALLDATASIZE 3 w)))
 (let ((?x10716 (storage_s w_2 x_CALLDATASIZE 4 w)))
 (= ?x10716 ?x9465))))
 ))
 (let (($x7806 (forall ((n (_ BitVec 6)) )(or (= (stack_s w_2 x_CALLDATASIZE 4 n) (stack_s w_2 x_CALLDATASIZE 3 n)) (bvsle (bvadd (_ bv63 6) (sc_s 3)) n)))
 ))
 (let (($x2321 (= ?x4305 (bvadd (_ bv1 6) (sc_s 3)))))
 (let (($x7079 (= (used_gas_s w_2 x_CALLDATASIZE 4) (+ 3 (used_gas_s w_2 x_CALLDATASIZE 3)))))
 (let ((?x275 (sc_s 3)))
 (let ((?x2861 (bvadd (_ bv63 6) ?x275)))
 (let ((?x3015 (stack_s w_2 x_CALLDATASIZE 3 ?x2861)))
 (let (($x9263 (= $x292 (or (exc_halt_s 2) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 2))))))))
 (let (($x4892 (forall ((w (_ BitVec 256)) )(let ((?x4684 (storage_s w_2 x_CALLDATASIZE 2 w)))
 (let ((?x9465 (storage_s w_2 x_CALLDATASIZE 3 w)))
 (= ?x9465 ?x4684))))
 ))
 (let (($x2191 (forall ((n (_ BitVec 6)) )(or (= (stack_s w_2 x_CALLDATASIZE 3 n) (stack_s w_2 x_CALLDATASIZE 2 n)) (bvsle (bvadd (_ bv62 6) (sc_s 2)) n)))
 ))
 (let ((?x218 (sc_s 2)))
 (let (($x9085 (= ?x275 ?x218)))
 (let ((?x302 (used_gas_s w_2 x_CALLDATASIZE 3)))
 (let (($x2049 (= (stack_s w_2 x_CALLDATASIZE 3 (bvadd (_ bv62 6) ?x275)) (stack_s w_2 x_CALLDATASIZE 2 (bvadd (_ bv63 6) ?x218)))))
 (let (($x3417 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 1)))) (_ bv0 1)))))
 (let (($x189 (exc_halt_s 1)))
 (let (($x247 (exc_halt_s 2)))
 (let (($x9072 (forall ((w (_ BitVec 256)) )(let ((?x6491 (storage_s w_2 x_CALLDATASIZE 1 w)))
 (let ((?x4684 (storage_s w_2 x_CALLDATASIZE 2 w)))
 (= ?x4684 ?x6491))))
 ))
 (let (($x3465 (forall ((n (_ BitVec 6)) )(or (= (stack_s w_2 x_CALLDATASIZE 2 n) (stack_s w_2 x_CALLDATASIZE 1 n)) (bvsle (sc_s 1) n)))
 ))
 (let (($x3670 (= ?x218 (bvadd (_ bv1 6) (sc_s 1)))))
 (let ((?x8764 (used_gas_s w_2 x_CALLDATASIZE 2)))
 (let (($x4891 (= $x189 (or $x57 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x72))) (_ bv0 1)))))))
 (let (($x1575 (forall ((w (_ BitVec 256)) )(let ((?x8342 (storage_s w_2 x_CALLDATASIZE 0 w)))
 (let ((?x6491 (storage_s w_2 x_CALLDATASIZE 1 w)))
 (= ?x6491 ?x8342))))
 ))
 (let (($x4347 (forall ((n (_ BitVec 6)) )(or (bvsle (sc_s 0) n) (= (stack_s w_2 x_CALLDATASIZE 1 n) (stack_s w_2 x_CALLDATASIZE 0 n))))
 ))
 (let ((?x154 (sc_s 1)))
 (let (($x10428 (= ?x154 (bvadd (_ bv1 6) ?x72))))
 (let (($x10733 (forall ((w (_ BitVec 256)) )(let ((?x8342 (storage_s w_2 x_CALLDATASIZE 0 w)))
 (= ?x8342 (_ bv0 256))))
 ))
 (let (($x3096 (= ?x4409 0)))
 (let (($x3117 (not $x57)))
 (let (($x136 (= ?x72 (_ bv0 6))))
 (and $x136 $x3117 $x3096 $x10733 (= (stack_s w_2 x_CALLDATASIZE 1 ?x72) w_2) (= (used_gas_s w_2 x_CALLDATASIZE 1) (+ 3 ?x4409)) $x10428 $x4347 $x1575 $x4891 (= (stack_s w_2 x_CALLDATASIZE 2 ?x154) x_CALLDATASIZE) (= ?x8764 (+ 2 (used_gas_s w_2 x_CALLDATASIZE 1))) $x3670 $x3465 $x9072 (= $x247 (or $x189 $x3417)) (= ?x3015 (stack_s w_2 x_CALLDATASIZE 2 (bvadd (_ bv62 6) ?x218))) $x2049 (= ?x302 (+ 3 ?x8764)) $x9085 $x2191 $x4892 $x9263 (= (stack_s w_2 x_CALLDATASIZE 4 (bvadd (_ bv63 6) ?x4305)) ?x3015) (= (stack_s w_2 x_CALLDATASIZE 4 ?x2861) ?x3015) $x7079 $x2321 $x7806 $x1568 (= $x64 (or (not (bvsle (_ bv0 6) ?x2861)) $x292 $x6601)) (= (stack_t w_2 x_CALLDATASIZE 1 ?x63) x_CALLDATASIZE) (= (used_gas_t w_2 x_CALLDATASIZE 1) (+ 2 ?x4963)) $x7084 $x3930 $x3881 $x1160 (= (stack_t w_2 x_CALLDATASIZE 2 ?x4023) w_2) (= ?x1310 (+ 3 (used_gas_t w_2 x_CALLDATASIZE 1))) $x423 $x7428 $x5051 (= $x903 (or $x1920 $x10002)) (= (stack_t w_2 x_CALLDATASIZE 3 ?x4056) w_2) $x3949 (= ?x2012 (bvadd (_ bv1 6) ?x4056)) $x8222 $x6614 (= $x10336 $x2423) $x73 $x8156 $x58 $x8459 $x10693 (not (and $x2319 $x5578 $x6232 $x8004))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
 )
(check-sat)
(get-proof)