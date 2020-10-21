; DUP1 MLOAD SWAP2 LT POP ISZERO => MLOAD ISZERO SWAP1 POP
(set-option :produce-proofs true)
; 
(set-info :status unknown)
(declare-fun storage_t ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 256)) (_ BitVec 256))
(declare-fun storage_s ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 256)) (_ BitVec 256))
(declare-fun exc_halt_t (Int) Bool)
(declare-fun exc_halt_s (Int) Bool)
(declare-fun sc_t (Int) (_ BitVec 6))
(declare-fun stack_t ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 6)) (_ BitVec 256))
(declare-fun stack_s ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 6)) (_ BitVec 256))
(declare-fun sc_s (Int) (_ BitVec 6))
(declare-fun used_gas_t ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int) Int)
(declare-fun used_gas_s ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int) Int)
(declare-fun f_MLOAD ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256)) (_ BitVec 256))
(assert
 (exists ((x_0 (_ BitVec 256)) (x_1 (_ BitVec 256)) (x_MLOAD_0 (_ BitVec 256)) )(let (($x10634 (forall ((w (_ BitVec 256)) )(let ((?x800 (storage_t x_0 x_1 x_MLOAD_0 4 w)))
 (let ((?x935 (storage_s x_0 x_1 x_MLOAD_0 6 w)))
 (= ?x935 ?x800))))
 ))
 (let (($x3723 (exc_halt_t 4)))
 (let (($x772 (exc_halt_s 6)))
 (let (($x9051 (= $x772 $x3723)))
 (let (($x829 (forall ((n (_ BitVec 6)) )(let ((?x3757 (sc_t 4)))
 (let (($x5939 (bvsle ?x3757 n)))
 (let ((?x6915 (stack_t x_0 x_1 x_MLOAD_0 4 n)))
 (let ((?x530 (stack_s x_0 x_1 x_MLOAD_0 6 n)))
 (let (($x81 (= ?x530 ?x6915)))
 (or $x81 $x5939)))))))
 ))
 (let ((?x3757 (sc_t 4)))
 (let ((?x926 (sc_s 6)))
 (let (($x3561 (= ?x926 ?x3757)))
 (let ((?x6038 (used_gas_t x_0 x_1 x_MLOAD_0 0)))
 (let ((?x4761 (used_gas_s x_0 x_1 x_MLOAD_0 0)))
 (let (($x7547 (= ?x4761 ?x6038)))
 (let (($x8641 (forall ((w (_ BitVec 256)) )(let ((?x8486 (storage_t x_0 x_1 x_MLOAD_0 0 w)))
 (let ((?x9401 (storage_s x_0 x_1 x_MLOAD_0 0 w)))
 (= ?x9401 ?x8486))))
 ))
 (let (($x56 (exc_halt_t 0)))
 (let (($x57 (exc_halt_s 0)))
 (let (($x58 (= $x57 $x56)))
 (let (($x3288 (forall ((n (_ BitVec 6)) )(let ((?x3557 (stack_t x_0 x_1 x_MLOAD_0 0 n)))
 (let ((?x9599 (stack_s x_0 x_1 x_MLOAD_0 0 n)))
 (let (($x7895 (= ?x9599 ?x3557)))
 (let ((?x63 (sc_t 0)))
 (let (($x1361 (bvsle ?x63 n)))
 (or $x1361 $x7895)))))))
 ))
 (let ((?x63 (sc_t 0)))
 (let ((?x72 (sc_s 0)))
 (let (($x73 (= ?x72 ?x63)))
 (let (($x2085 (= $x3723 (or (exc_halt_t 3) (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_t 3))))))))
 (let (($x1697 (forall ((w (_ BitVec 256)) )(let ((?x8846 (storage_t x_0 x_1 x_MLOAD_0 3 w)))
 (let ((?x800 (storage_t x_0 x_1 x_MLOAD_0 4 w)))
 (= ?x800 ?x8846))))
 ))
 (let (($x10723 (forall ((n (_ BitVec 6)) )(let ((?x2012 (sc_t 3)))
 (let ((?x7653 (bvadd (_ bv63 6) ?x2012)))
 (let (($x6928 (bvsle ?x7653 n)))
 (let ((?x3396 (stack_t x_0 x_1 x_MLOAD_0 3 n)))
 (let ((?x6915 (stack_t x_0 x_1 x_MLOAD_0 4 n)))
 (or (= ?x6915 ?x3396) $x6928)))))))
 ))
 (let (($x5855 (= (used_gas_t x_0 x_1 x_MLOAD_0 4) (+ 2 (used_gas_t x_0 x_1 x_MLOAD_0 3)))))
 (let (($x10336 (exc_halt_t 3)))
 (let (($x7502 (= $x10336 (or (exc_halt_t 2) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_t 2))))))))
 (let (($x10036 (forall ((w (_ BitVec 256)) )(let ((?x6109 (storage_t x_0 x_1 x_MLOAD_0 2 w)))
 (let ((?x8846 (storage_t x_0 x_1 x_MLOAD_0 3 w)))
 (= ?x8846 ?x6109))))
 ))
 (let (($x1430 (forall ((n (_ BitVec 6)) )(let ((?x4056 (sc_t 2)))
 (let ((?x3734 (bvadd (_ bv62 6) ?x4056)))
 (let (($x3782 (bvsle ?x3734 n)))
 (let ((?x3011 (stack_t x_0 x_1 x_MLOAD_0 2 n)))
 (let ((?x3396 (stack_t x_0 x_1 x_MLOAD_0 3 n)))
 (let (($x3416 (= ?x3396 ?x3011)))
 (or $x3416 $x3782))))))))
 ))
 (let ((?x4056 (sc_t 2)))
 (let ((?x2012 (sc_t 3)))
 (let (($x9482 (= ?x2012 ?x4056)))
 (let ((?x7269 (used_gas_t x_0 x_1 x_MLOAD_0 3)))
 (let (($x9468 (= ?x7269 (+ 3 (used_gas_t x_0 x_1 x_MLOAD_0 2)))))
 (let ((?x1438 (bvadd (_ bv63 6) ?x4056)))
 (let ((?x6711 (stack_t x_0 x_1 x_MLOAD_0 2 ?x1438)))
 (let ((?x10379 (bvadd (_ bv62 6) ?x2012)))
 (let ((?x4116 (stack_t x_0 x_1 x_MLOAD_0 3 ?x10379)))
 (let ((?x3734 (bvadd (_ bv62 6) ?x4056)))
 (let ((?x7533 (stack_t x_0 x_1 x_MLOAD_0 2 ?x3734)))
 (let ((?x7653 (bvadd (_ bv63 6) ?x2012)))
 (let ((?x736 (stack_t x_0 x_1 x_MLOAD_0 3 ?x7653)))
 (let (($x903 (exc_halt_t 2)))
 (let (($x2492 (= $x903 (or (exc_halt_t 1) (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_t 1))))))))
 (let (($x627 (forall ((w (_ BitVec 256)) )(let ((?x7154 (storage_t x_0 x_1 x_MLOAD_0 1 w)))
 (let ((?x6109 (storage_t x_0 x_1 x_MLOAD_0 2 w)))
 (= ?x6109 ?x7154))))
 ))
 (let (($x1631 (forall ((n (_ BitVec 6)) )(let ((?x4023 (sc_t 1)))
 (let ((?x1843 (bvadd (_ bv63 6) ?x4023)))
 (let (($x10022 (bvsle ?x1843 n)))
 (let ((?x10159 (stack_t x_0 x_1 x_MLOAD_0 1 n)))
 (let ((?x3011 (stack_t x_0 x_1 x_MLOAD_0 2 n)))
 (let (($x3897 (= ?x3011 ?x10159)))
 (or $x3897 $x10022))))))))
 ))
 (let ((?x4023 (sc_t 1)))
 (let (($x6083 (= ?x4056 ?x4023)))
 (let ((?x9781 (used_gas_t x_0 x_1 x_MLOAD_0 2)))
 (let (($x14 (= ?x9781 (+ 3 (used_gas_t x_0 x_1 x_MLOAD_0 1)))))
 (let ((?x7398 (ite (= (stack_t x_0 x_1 x_MLOAD_0 1 (bvadd (_ bv63 6) ?x4023)) (_ bv0 256)) (_ bv1 256) (_ bv0 256))))
 (let (($x3278 (forall ((w (_ BitVec 256)) )(let ((?x8486 (storage_t x_0 x_1 x_MLOAD_0 0 w)))
 (let ((?x7154 (storage_t x_0 x_1 x_MLOAD_0 1 w)))
 (= ?x7154 ?x8486))))
 ))
 (let (($x9669 (forall ((n (_ BitVec 6)) )(let ((?x63 (sc_t 0)))
 (let ((?x6326 (bvadd (_ bv63 6) ?x63)))
 (let (($x8483 (bvsle ?x6326 n)))
 (let ((?x3557 (stack_t x_0 x_1 x_MLOAD_0 0 n)))
 (let ((?x10159 (stack_t x_0 x_1 x_MLOAD_0 1 n)))
 (or (= ?x10159 ?x3557) $x8483)))))))
 ))
 (let ((?x4400 (used_gas_t x_0 x_1 x_MLOAD_0 1)))
 (let (($x3733 (= ?x4400 (+ 3 ?x6038))))
 (let ((?x6326 (bvadd (_ bv63 6) ?x63)))
 (let ((?x5309 (stack_t x_0 x_1 x_MLOAD_0 0 ?x6326)))
 (let ((?x1843 (bvadd (_ bv63 6) ?x4023)))
 (let ((?x742 (stack_t x_0 x_1 x_MLOAD_0 1 ?x1843)))
 (let (($x9463 (= $x772 (or (exc_halt_s 5) (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_s 5))))))))
 (let (($x6855 (forall ((w (_ BitVec 256)) )(let ((?x1254 (storage_s x_0 x_1 x_MLOAD_0 5 w)))
 (let ((?x935 (storage_s x_0 x_1 x_MLOAD_0 6 w)))
 (= ?x935 ?x1254))))
 ))
 (let (($x1372 (forall ((n (_ BitVec 6)) )(let ((?x3472 (stack_s x_0 x_1 x_MLOAD_0 5 n)))
 (let ((?x530 (stack_s x_0 x_1 x_MLOAD_0 6 n)))
 (or (= ?x530 ?x3472) (bvsle (bvadd (_ bv63 6) (sc_s 5)) n)))))
 ))
 (let ((?x805 (sc_s 5)))
 (let (($x8328 (= ?x926 ?x805)))
 (let (($x7258 (= (used_gas_s x_0 x_1 x_MLOAD_0 6) (+ 3 (used_gas_s x_0 x_1 x_MLOAD_0 5)))))
 (let ((?x8715 (ite (= (stack_s x_0 x_1 x_MLOAD_0 5 (bvadd (_ bv63 6) ?x805)) (_ bv0 256)) (_ bv1 256) (_ bv0 256))))
 (let (($x3979 (exc_halt_s 5)))
 (let (($x1664 (= $x3979 (or (exc_halt_s 4) (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_s 4))))))))
 (let (($x2101 (forall ((w (_ BitVec 256)) )(let ((?x3104 (storage_s x_0 x_1 x_MLOAD_0 4 w)))
 (let ((?x1254 (storage_s x_0 x_1 x_MLOAD_0 5 w)))
 (= ?x1254 ?x3104))))
 ))
 (let (($x5475 (forall ((n (_ BitVec 6)) )(let ((?x9073 (stack_s x_0 x_1 x_MLOAD_0 4 n)))
 (let ((?x3472 (stack_s x_0 x_1 x_MLOAD_0 5 n)))
 (let ((?x4305 (sc_s 4)))
 (let ((?x3595 (bvadd (_ bv63 6) ?x4305)))
 (let (($x3093 (bvsle ?x3595 n)))
 (or $x3093 (= ?x3472 ?x9073))))))))
 ))
 (let ((?x4305 (sc_s 4)))
 (let ((?x3595 (bvadd (_ bv63 6) ?x4305)))
 (let (($x2634 (= ?x805 ?x3595)))
 (let ((?x1977 (used_gas_s x_0 x_1 x_MLOAD_0 5)))
 (let (($x64 (exc_halt_s 4)))
 (let (($x2731 (= $x64 (or (exc_halt_s 3) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 3))))))))
 (let (($x2823 (forall ((w (_ BitVec 256)) )(let ((?x6359 (storage_s x_0 x_1 x_MLOAD_0 3 w)))
 (let ((?x3104 (storage_s x_0 x_1 x_MLOAD_0 4 w)))
 (= ?x3104 ?x6359))))
 ))
 (let (($x6181 (forall ((n (_ BitVec 6)) )(let ((?x8988 (stack_s x_0 x_1 x_MLOAD_0 3 n)))
 (let ((?x9073 (stack_s x_0 x_1 x_MLOAD_0 4 n)))
 (let (($x6909 (= ?x9073 ?x8988)))
 (let ((?x275 (sc_s 3)))
 (let ((?x11136 (bvadd (_ bv62 6) ?x275)))
 (let (($x6437 (bvsle ?x11136 n)))
 (or $x6437 $x6909))))))))
 ))
 (let ((?x10498 (used_gas_s x_0 x_1 x_MLOAD_0 4)))
 (let (($x3530 (= ?x10498 (+ 3 (used_gas_s x_0 x_1 x_MLOAD_0 3)))))
 (let ((?x275 (sc_s 3)))
 (let ((?x9192 (bvadd (_ bv63 6) ?x275)))
 (let ((?x7561 (stack_s x_0 x_1 x_MLOAD_0 3 ?x9192)))
 (let ((?x11136 (bvadd (_ bv62 6) ?x275)))
 (let ((?x2004 (stack_s x_0 x_1 x_MLOAD_0 3 ?x11136)))
 (let ((?x9475 (stack_s x_0 x_1 x_MLOAD_0 4 ?x3595)))
 (let (($x292 (exc_halt_s 3)))
 (let (($x9751 (= $x292 (or (exc_halt_s 2) (not (bvsle (_ bv0 6) (bvadd (_ bv61 6) (sc_s 2))))))))
 (let (($x6641 (forall ((w (_ BitVec 256)) )(let ((?x5376 (storage_s x_0 x_1 x_MLOAD_0 2 w)))
 (let ((?x6359 (storage_s x_0 x_1 x_MLOAD_0 3 w)))
 (= ?x6359 ?x5376))))
 ))
 (let (($x9056 (forall ((n (_ BitVec 6)) )(let ((?x6849 (stack_s x_0 x_1 x_MLOAD_0 2 n)))
 (let ((?x8988 (stack_s x_0 x_1 x_MLOAD_0 3 n)))
 (let (($x2623 (= ?x8988 ?x6849)))
 (or $x2623 (bvsle (bvadd (_ bv61 6) (sc_s 2)) n))))))
 ))
 (let ((?x218 (sc_s 2)))
 (let (($x1433 (= ?x275 ?x218)))
 (let ((?x3667 (used_gas_s x_0 x_1 x_MLOAD_0 3)))
 (let (($x392 (= ?x3667 (+ 3 (used_gas_s x_0 x_1 x_MLOAD_0 2)))))
 (let ((?x2961 (bvadd (_ bv63 6) ?x218)))
 (let ((?x5792 (stack_s x_0 x_1 x_MLOAD_0 2 ?x2961)))
 (let ((?x7590 (bvadd (_ bv61 6) ?x275)))
 (let ((?x749 (stack_s x_0 x_1 x_MLOAD_0 3 ?x7590)))
 (let (($x247 (exc_halt_s 2)))
 (let (($x9992 (= $x247 (or (exc_halt_s 1) (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_s 1))))))))
 (let (($x10312 (forall ((w (_ BitVec 256)) )(let ((?x1015 (storage_s x_0 x_1 x_MLOAD_0 1 w)))
 (let ((?x5376 (storage_s x_0 x_1 x_MLOAD_0 2 w)))
 (= ?x5376 ?x1015))))
 ))
 (let (($x8002 (forall ((n (_ BitVec 6)) )(let ((?x154 (sc_s 1)))
 (let ((?x11219 (bvadd (_ bv63 6) ?x154)))
 (let (($x5020 (bvsle ?x11219 n)))
 (let ((?x971 (stack_s x_0 x_1 x_MLOAD_0 1 n)))
 (let ((?x6849 (stack_s x_0 x_1 x_MLOAD_0 2 n)))
 (let (($x1478 (= ?x6849 ?x971)))
 (or $x1478 $x5020))))))))
 ))
 (let ((?x154 (sc_s 1)))
 (let (($x5668 (= ?x218 ?x154)))
 (let ((?x9148 (used_gas_s x_0 x_1 x_MLOAD_0 2)))
 (let (($x9693 (= ?x9148 (+ 3 (used_gas_s x_0 x_1 x_MLOAD_0 1)))))
 (let ((?x11219 (bvadd (_ bv63 6) ?x154)))
 (let ((?x8996 (stack_s x_0 x_1 x_MLOAD_0 1 ?x11219)))
 (let (($x11374 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x72))) (_ bv0 1)))))
 (let (($x189 (exc_halt_s 1)))
 (let (($x6373 (= $x189 (or $x57 (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) ?x72))) $x11374))))
 (let (($x3513 (forall ((w (_ BitVec 256)) )(let ((?x9401 (storage_s x_0 x_1 x_MLOAD_0 0 w)))
 (let ((?x1015 (storage_s x_0 x_1 x_MLOAD_0 1 w)))
 (= ?x1015 ?x9401))))
 ))
 (let (($x7576 (forall ((n (_ BitVec 6)) )(let ((?x72 (sc_s 0)))
 (let ((?x6683 (bvadd (_ bv63 6) ?x72)))
 (let (($x8026 (bvsle ?x6683 n)))
 (let ((?x9599 (stack_s x_0 x_1 x_MLOAD_0 0 n)))
 (let ((?x971 (stack_s x_0 x_1 x_MLOAD_0 1 n)))
 (let (($x7422 (= ?x971 ?x9599)))
 (or $x7422 $x8026))))))))
 ))
 (let (($x1901 (= ?x154 (bvadd (_ bv1 6) ?x72))))
 (let ((?x5325 (used_gas_s x_0 x_1 x_MLOAD_0 1)))
 (let (($x5690 (= ?x5325 (+ 3 ?x4761))))
 (let ((?x6683 (bvadd (_ bv63 6) ?x72)))
 (let ((?x9212 (stack_s x_0 x_1 x_MLOAD_0 0 ?x6683)))
 (let (($x2075 (forall ((w0 (_ BitVec 256)) )(let ((?x1190 (ite (= (stack_s x_0 x_1 x_MLOAD_0 1 (bvadd (_ bv63 6) (sc_s 1))) w0) x_MLOAD_0 (_ bv0 256))))
 (let ((?x2019 (f_MLOAD x_0 x_1 x_MLOAD_0 w0)))
 (= ?x2019 ?x1190))))
 ))
 (let (($x921 (forall ((w (_ BitVec 256)) )(let ((?x9401 (storage_s x_0 x_1 x_MLOAD_0 0 w)))
 (= ?x9401 (_ bv0 256))))
 ))
 (let (($x6188 (= ?x4761 0)))
 (let (($x11411 (not $x57)))
 (let (($x3761 (= (stack_s x_0 x_1 x_MLOAD_0 0 (_ bv1 6)) x_1)))
 (let (($x9661 (= (stack_s x_0 x_1 x_MLOAD_0 0 (_ bv0 6)) x_0)))
 (let (($x3907 (= ?x72 (_ bv2 6))))
 (and $x3907 $x9661 $x3761 $x11411 $x6188 $x921 $x2075 (= ?x8996 ?x9212) (= (stack_s x_0 x_1 x_MLOAD_0 1 ?x6683) ?x9212) $x5690 $x1901 $x7576 $x3513 $x6373 (= ?x5792 (f_MLOAD x_0 x_1 x_MLOAD_0 ?x8996)) $x9693 $x5668 $x8002 $x10312 $x9992 (= ?x7561 (stack_s x_0 x_1 x_MLOAD_0 2 (bvadd (_ bv61 6) ?x218))) (= ?x749 ?x5792) (= ?x2004 (stack_s x_0 x_1 x_MLOAD_0 2 (bvadd (_ bv62 6) ?x218))) $x392 $x1433 $x9056 $x6641 $x9751 (= ?x9475 (ite (bvule ?x2004 ?x7561) (_ bv0 256) (_ bv1 256))) $x3530 (= ?x4305 ?x9192) $x6181 $x2823 $x2731 (= ?x1977 (+ 2 ?x10498)) $x2634 $x5475 $x2101 $x1664 (= (stack_s x_0 x_1 x_MLOAD_0 6 (bvadd (_ bv63 6) ?x926)) ?x8715) $x7258 $x8328 $x1372 $x6855 $x9463 (= ?x742 (f_MLOAD x_0 x_1 x_MLOAD_0 ?x5309)) $x3733 (= ?x4023 ?x63) $x9669 $x3278 (= (exc_halt_t 1) (or $x56 (not (bvsle (_ bv0 6) ?x6326)))) (= ?x6711 ?x7398) $x14 $x6083 $x1631 $x627 $x2492 (= ?x736 ?x7533) (= ?x4116 ?x6711) $x9468 $x9482 $x1430 $x10036 $x7502 $x5855 (= ?x3757 ?x7653) $x10723 $x1697 $x2085 $x73 $x3288 $x58 $x8641 $x7547 (not (and $x3561 $x829 $x9051 $x10634)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
 )
(check-sat)
(get-proof)
