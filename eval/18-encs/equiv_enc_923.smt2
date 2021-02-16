; PUSH cw_1 SWAP4 SWAP1 SWAP3 SWAP1 => SWAP2 PUSH cw_1 SWAP4
(set-option :produce-proofs true)
; 
(set-info :status unknown)
(declare-fun storage_t ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 256)) (_ BitVec 256))
(declare-fun w_1 () (_ BitVec 256))
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
 (exists ((x_0 (_ BitVec 256)) (x_1 (_ BitVec 256)) (x_2 (_ BitVec 256)) (x_3 (_ BitVec 256)) )(let (($x3913 (forall ((w (_ BitVec 256)) )(let ((?x9303 (storage_t x_0 x_1 x_2 x_3 w_1 3 w)))
 (let ((?x3850 (storage_s x_0 x_1 x_2 x_3 w_1 5 w)))
 (= ?x3850 ?x9303))))
 ))
 (let (($x9131 (exc_halt_t 3)))
 (let (($x1862 (exc_halt_s 5)))
 (let (($x5461 (= $x1862 $x9131)))
 (let (($x10729 (forall ((n (_ BitVec 6)) )(let ((?x5248 (stack_t x_0 x_1 x_2 x_3 w_1 3 n)))
 (let ((?x3532 (stack_s x_0 x_1 x_2 x_3 w_1 5 n)))
 (let (($x3669 (= ?x3532 ?x5248)))
 (or $x3669 (bvsle (sc_t 3) n))))))
 ))
 (let ((?x10013 (sc_t 3)))
 (let ((?x4319 (sc_s 5)))
 (let (($x8334 (= ?x4319 ?x10013)))
 (let ((?x4742 (used_gas_t x_0 x_1 x_2 x_3 w_1 0)))
 (let ((?x7669 (used_gas_s x_0 x_1 x_2 x_3 w_1 0)))
 (let (($x10981 (= ?x7669 ?x4742)))
 (let (($x3036 (forall ((w (_ BitVec 256)) )(let ((?x9944 (storage_t x_0 x_1 x_2 x_3 w_1 0 w)))
 (let ((?x11678 (storage_s x_0 x_1 x_2 x_3 w_1 0 w)))
 (= ?x11678 ?x9944))))
 ))
 (let (($x56 (exc_halt_t 0)))
 (let (($x57 (exc_halt_s 0)))
 (let (($x58 (= $x57 $x56)))
 (let (($x6323 (forall ((n (_ BitVec 6)) )(let ((?x63 (sc_t 0)))
 (let (($x4809 (bvsle ?x63 n)))
 (let ((?x6054 (stack_t x_0 x_1 x_2 x_3 w_1 0 n)))
 (let ((?x2712 (stack_s x_0 x_1 x_2 x_3 w_1 0 n)))
 (let (($x464 (= ?x2712 ?x6054)))
 (or $x464 $x4809)))))))
 ))
 (let ((?x63 (sc_t 0)))
 (let ((?x72 (sc_s 0)))
 (let (($x73 (= ?x72 ?x63)))
 (let (($x3338 (= $x9131 (or (exc_halt_t 2) (not (bvsle (_ bv0 6) (bvadd (_ bv59 6) (sc_t 2))))))))
 (let (($x7044 (forall ((w (_ BitVec 256)) )(let ((?x3672 (storage_t x_0 x_1 x_2 x_3 w_1 2 w)))
 (let ((?x9303 (storage_t x_0 x_1 x_2 x_3 w_1 3 w)))
 (= ?x9303 ?x3672))))
 ))
 (let (($x3550 (forall ((n (_ BitVec 6)) )(let ((?x5407 (stack_t x_0 x_1 x_2 x_3 w_1 2 n)))
 (let ((?x5248 (stack_t x_0 x_1 x_2 x_3 w_1 3 n)))
 (or (= ?x5248 ?x5407) (bvsle (bvadd (_ bv59 6) (sc_t 2)) n)))))
 ))
 (let (($x5834 (= (used_gas_t x_0 x_1 x_2 x_3 w_1 3) (+ 3 (used_gas_t x_0 x_1 x_2 x_3 w_1 2)))))
 (let (($x2314 (= (stack_t x_0 x_1 x_2 x_3 w_1 3 (bvadd (_ bv62 6) ?x10013)) (stack_t x_0 x_1 x_2 x_3 w_1 2 (bvadd (_ bv62 6) (sc_t 2))))))
 (let (($x2020 (= (stack_t x_0 x_1 x_2 x_3 w_1 3 (bvadd (_ bv61 6) ?x10013)) (stack_t x_0 x_1 x_2 x_3 w_1 2 (bvadd (_ bv61 6) (sc_t 2))))))
 (let (($x8166 (= (stack_t x_0 x_1 x_2 x_3 w_1 3 (bvadd (_ bv60 6) ?x10013)) (stack_t x_0 x_1 x_2 x_3 w_1 2 (bvadd (_ bv60 6) (sc_t 2))))))
 (let (($x1474 (= (stack_t x_0 x_1 x_2 x_3 w_1 3 (bvadd (_ bv59 6) ?x10013)) (stack_t x_0 x_1 x_2 x_3 w_1 2 (bvadd (_ bv63 6) (sc_t 2))))))
 (let (($x521 (= (stack_t x_0 x_1 x_2 x_3 w_1 3 (bvadd (_ bv63 6) ?x10013)) (stack_t x_0 x_1 x_2 x_3 w_1 2 (bvadd (_ bv59 6) (sc_t 2))))))
 (let (($x7008 (exc_halt_t 1)))
 (let (($x1552 (or $x7008 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_t 1)))) (_ bv0 1))))))
 (let (($x8194 (exc_halt_t 2)))
 (let (($x8566 (= $x8194 $x1552)))
 (let (($x11704 (forall ((w (_ BitVec 256)) )(let ((?x4986 (storage_t x_0 x_1 x_2 x_3 w_1 1 w)))
 (let ((?x3672 (storage_t x_0 x_1 x_2 x_3 w_1 2 w)))
 (= ?x3672 ?x4986))))
 ))
 (let (($x8563 (forall ((n (_ BitVec 6)) )(let ((?x1740 (stack_t x_0 x_1 x_2 x_3 w_1 1 n)))
 (let ((?x5407 (stack_t x_0 x_1 x_2 x_3 w_1 2 n)))
 (let ((?x6360 (sc_t 1)))
 (let (($x2555 (bvsle ?x6360 n)))
 (or $x2555 (= ?x5407 ?x1740)))))))
 ))
 (let ((?x6535 (sc_t 2)))
 (let (($x7414 (= ?x6535 (bvadd (_ bv1 6) (sc_t 1)))))
 (let ((?x9599 (used_gas_t x_0 x_1 x_2 x_3 w_1 2)))
 (let (($x653 (forall ((w (_ BitVec 256)) )(let ((?x9944 (storage_t x_0 x_1 x_2 x_3 w_1 0 w)))
 (let ((?x4986 (storage_t x_0 x_1 x_2 x_3 w_1 1 w)))
 (= ?x4986 ?x9944))))
 ))
 (let (($x4456 (forall ((n (_ BitVec 6)) )(let ((?x6054 (stack_t x_0 x_1 x_2 x_3 w_1 0 n)))
 (let ((?x1740 (stack_t x_0 x_1 x_2 x_3 w_1 1 n)))
 (or (bvsle (bvadd (_ bv61 6) (sc_t 0)) n) (= ?x1740 ?x6054)))))
 ))
 (let (($x9986 (= (stack_t x_0 x_1 x_2 x_3 w_1 1 (bvadd (_ bv62 6) (sc_t 1))) (stack_t x_0 x_1 x_2 x_3 w_1 0 (bvadd (_ bv62 6) ?x63)))))
 (let (($x11065 (= (stack_t x_0 x_1 x_2 x_3 w_1 1 (bvadd (_ bv61 6) (sc_t 1))) (stack_t x_0 x_1 x_2 x_3 w_1 0 (bvadd (_ bv63 6) ?x63)))))
 (let (($x8835 (= (stack_t x_0 x_1 x_2 x_3 w_1 1 (bvadd (_ bv63 6) (sc_t 1))) (stack_t x_0 x_1 x_2 x_3 w_1 0 (bvadd (_ bv61 6) ?x63)))))
 (let (($x3380 (= $x1862 (or (exc_halt_s 4) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 4))))))))
 (let (($x6459 (forall ((w (_ BitVec 256)) )(let ((?x8385 (storage_s x_0 x_1 x_2 x_3 w_1 4 w)))
 (let ((?x3850 (storage_s x_0 x_1 x_2 x_3 w_1 5 w)))
 (= ?x3850 ?x8385))))
 ))
 (let (($x7484 (forall ((n (_ BitVec 6)) )(let ((?x9433 (sc_s 4)))
 (let ((?x9158 (bvadd (_ bv62 6) ?x9433)))
 (let (($x3986 (bvsle ?x9158 n)))
 (let ((?x7568 (stack_s x_0 x_1 x_2 x_3 w_1 4 n)))
 (let ((?x3532 (stack_s x_0 x_1 x_2 x_3 w_1 5 n)))
 (or (= ?x3532 ?x7568) $x3986)))))))
 ))
 (let ((?x9433 (sc_s 4)))
 (let (($x1526 (= ?x4319 ?x9433)))
 (let (($x1838 (= (used_gas_s x_0 x_1 x_2 x_3 w_1 5) (+ 3 (used_gas_s x_0 x_1 x_2 x_3 w_1 4)))))
 (let ((?x2904 (bvadd (_ bv63 6) ?x9433)))
 (let ((?x1138 (stack_s x_0 x_1 x_2 x_3 w_1 4 ?x2904)))
 (let ((?x9158 (bvadd (_ bv62 6) ?x9433)))
 (let ((?x5312 (stack_s x_0 x_1 x_2 x_3 w_1 4 ?x9158)))
 (let (($x9175 (exc_halt_s 4)))
 (let (($x3272 (= $x9175 (or (exc_halt_s 3) (not (bvsle (_ bv0 6) (bvadd (_ bv60 6) (sc_s 3))))))))
 (let (($x2766 (forall ((w (_ BitVec 256)) )(let ((?x8230 (storage_s x_0 x_1 x_2 x_3 w_1 3 w)))
 (let ((?x8385 (storage_s x_0 x_1 x_2 x_3 w_1 4 w)))
 (= ?x8385 ?x8230))))
 ))
 (let (($x10917 (forall ((n (_ BitVec 6)) )(let ((?x4446 (stack_s x_0 x_1 x_2 x_3 w_1 3 n)))
 (let ((?x7568 (stack_s x_0 x_1 x_2 x_3 w_1 4 n)))
 (or (= ?x7568 ?x4446) (bvsle (bvadd (_ bv60 6) (sc_s 3)) n)))))
 ))
 (let ((?x3851 (sc_s 3)))
 (let (($x10493 (= ?x9433 ?x3851)))
 (let ((?x2165 (used_gas_s x_0 x_1 x_2 x_3 w_1 4)))
 (let (($x11582 (= (stack_s x_0 x_1 x_2 x_3 w_1 4 (bvadd (_ bv61 6) ?x9433)) (stack_s x_0 x_1 x_2 x_3 w_1 3 (bvadd (_ bv61 6) ?x3851)))))
 (let ((?x8926 (bvadd (_ bv63 6) ?x3851)))
 (let ((?x11732 (stack_s x_0 x_1 x_2 x_3 w_1 3 ?x8926)))
 (let (($x8103 (exc_halt_s 3)))
 (let (($x1596 (= $x8103 (or (exc_halt_s 2) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 2))))))))
 (let (($x536 (forall ((w (_ BitVec 256)) )(let ((?x11628 (storage_s x_0 x_1 x_2 x_3 w_1 2 w)))
 (let ((?x8230 (storage_s x_0 x_1 x_2 x_3 w_1 3 w)))
 (= ?x8230 ?x11628))))
 ))
 (let (($x3399 (forall ((n (_ BitVec 6)) )(let ((?x11484 (stack_s x_0 x_1 x_2 x_3 w_1 2 n)))
 (let ((?x4446 (stack_s x_0 x_1 x_2 x_3 w_1 3 n)))
 (let ((?x2272 (sc_s 2)))
 (let ((?x6884 (bvadd (_ bv62 6) ?x2272)))
 (let (($x368 (bvsle ?x6884 n)))
 (or $x368 (= ?x4446 ?x11484))))))))
 ))
 (let ((?x2272 (sc_s 2)))
 (let (($x10097 (= ?x3851 ?x2272)))
 (let ((?x86 (used_gas_s x_0 x_1 x_2 x_3 w_1 3)))
 (let ((?x4463 (bvadd (_ bv63 6) ?x2272)))
 (let ((?x7234 (stack_s x_0 x_1 x_2 x_3 w_1 2 ?x4463)))
 (let ((?x2944 (bvadd (_ bv62 6) ?x3851)))
 (let ((?x7894 (stack_s x_0 x_1 x_2 x_3 w_1 3 ?x2944)))
 (let (($x10052 (exc_halt_s 2)))
 (let (($x9849 (= $x10052 (or (exc_halt_s 1) (not (bvsle (_ bv0 6) (bvadd (_ bv59 6) (sc_s 1))))))))
 (let (($x10077 (forall ((w (_ BitVec 256)) )(let ((?x3797 (storage_s x_0 x_1 x_2 x_3 w_1 1 w)))
 (let ((?x11628 (storage_s x_0 x_1 x_2 x_3 w_1 2 w)))
 (= ?x11628 ?x3797))))
 ))
 (let (($x6317 (forall ((n (_ BitVec 6)) )(let ((?x6653 (stack_s x_0 x_1 x_2 x_3 w_1 1 n)))
 (let ((?x11484 (stack_s x_0 x_1 x_2 x_3 w_1 2 n)))
 (or (= ?x11484 ?x6653) (bvsle (bvadd (_ bv59 6) (sc_s 1)) n)))))
 ))
 (let ((?x103 (used_gas_s x_0 x_1 x_2 x_3 w_1 2)))
 (let ((?x6884 (bvadd (_ bv62 6) ?x2272)))
 (let ((?x1090 (stack_s x_0 x_1 x_2 x_3 w_1 2 ?x6884)))
 (let (($x7524 (= ?x1090 (stack_s x_0 x_1 x_2 x_3 w_1 1 (bvadd (_ bv62 6) (sc_s 1))))))
 (let (($x10581 (= (stack_s x_0 x_1 x_2 x_3 w_1 2 (bvadd (_ bv61 6) ?x2272)) (stack_s x_0 x_1 x_2 x_3 w_1 1 (bvadd (_ bv61 6) (sc_s 1))))))
 (let (($x10398 (= (stack_s x_0 x_1 x_2 x_3 w_1 2 (bvadd (_ bv60 6) ?x2272)) (stack_s x_0 x_1 x_2 x_3 w_1 1 (bvadd (_ bv60 6) (sc_s 1))))))
 (let (($x5837 (= (stack_s x_0 x_1 x_2 x_3 w_1 2 (bvadd (_ bv59 6) ?x2272)) (stack_s x_0 x_1 x_2 x_3 w_1 1 (bvadd (_ bv63 6) (sc_s 1))))))
 (let (($x8404 (= ?x7234 (stack_s x_0 x_1 x_2 x_3 w_1 1 (bvadd (_ bv59 6) (sc_s 1))))))
 (let (($x8780 (exc_halt_s 1)))
 (let (($x8019 (= $x8780 (or $x57 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x72))) (_ bv0 1)))))))
 (let (($x10319 (forall ((w (_ BitVec 256)) )(let ((?x11678 (storage_s x_0 x_1 x_2 x_3 w_1 0 w)))
 (let ((?x3797 (storage_s x_0 x_1 x_2 x_3 w_1 1 w)))
 (= ?x3797 ?x11678))))
 ))
 (let (($x3234 (forall ((n (_ BitVec 6)) )(let ((?x2712 (stack_s x_0 x_1 x_2 x_3 w_1 0 n)))
 (let ((?x6653 (stack_s x_0 x_1 x_2 x_3 w_1 1 n)))
 (let ((?x72 (sc_s 0)))
 (let (($x8226 (bvsle ?x72 n)))
 (or $x8226 (= ?x6653 ?x2712)))))))
 ))
 (let ((?x154 (sc_s 1)))
 (let (($x10114 (= ?x154 (bvadd (_ bv1 6) ?x72))))
 (let (($x9384 (forall ((w (_ BitVec 256)) )(let ((?x11678 (storage_s x_0 x_1 x_2 x_3 w_1 0 w)))
 (= ?x11678 (_ bv0 256))))
 ))
 (let (($x6250 (= ?x7669 0)))
 (let (($x7519 (not $x57)))
 (let (($x2311 (= (stack_s x_0 x_1 x_2 x_3 w_1 0 (_ bv3 6)) x_3)))
 (let (($x2676 (= (stack_s x_0 x_1 x_2 x_3 w_1 0 (_ bv2 6)) x_2)))
 (let (($x10933 (= (stack_s x_0 x_1 x_2 x_3 w_1 0 (_ bv1 6)) x_1)))
 (let (($x9666 (= (stack_s x_0 x_1 x_2 x_3 w_1 0 (_ bv0 6)) x_0)))
 (let (($x10832 (= ?x72 (_ bv4 6))))
 (and $x10832 $x9666 $x10933 $x2676 $x2311 $x7519 $x6250 $x9384 (= (stack_s x_0 x_1 x_2 x_3 w_1 1 ?x72) w_1) (= (used_gas_s x_0 x_1 x_2 x_3 w_1 1) (+ 3 ?x7669)) $x10114 $x3234 $x10319 $x8019 $x8404 $x5837 $x10398 $x10581 $x7524 (= ?x103 (+ 3 (used_gas_s x_0 x_1 x_2 x_3 w_1 1))) (= ?x2272 ?x154) $x6317 $x10077 $x9849 (= ?x11732 ?x1090) (= ?x7894 ?x7234) (= ?x86 (+ 3 ?x103)) $x10097 $x3399 $x536 $x1596 (= ?x1138 (stack_s x_0 x_1 x_2 x_3 w_1 3 (bvadd (_ bv60 6) ?x3851))) (= (stack_s x_0 x_1 x_2 x_3 w_1 4 (bvadd (_ bv60 6) ?x9433)) ?x11732) $x11582 (= ?x5312 ?x7894) (= ?x2165 (+ 3 ?x86)) $x10493 $x10917 $x2766 $x3272 (= (stack_s x_0 x_1 x_2 x_3 w_1 5 (bvadd (_ bv63 6) ?x4319)) ?x5312) (= (stack_s x_0 x_1 x_2 x_3 w_1 5 (bvadd (_ bv62 6) ?x4319)) ?x1138) $x1838 $x1526 $x7484 $x6459 $x3380 $x8835 $x11065 $x9986 (= (used_gas_t x_0 x_1 x_2 x_3 w_1 1) (+ 3 ?x4742)) (= (sc_t 1) ?x63) $x4456 $x653 (= $x7008 (or $x56 (not (bvsle (_ bv0 6) (bvadd (_ bv61 6) ?x63))))) (= (stack_t x_0 x_1 x_2 x_3 w_1 2 (sc_t 1)) w_1) (= ?x9599 (+ 3 (used_gas_t x_0 x_1 x_2 x_3 w_1 1))) $x7414 $x8563 $x11704 $x8566 $x521 $x1474 $x8166 $x2020 $x2314 $x5834 (= ?x10013 ?x6535) $x3550 $x7044 $x3338 $x73 $x6323 $x58 $x3036 $x10981 (not (and $x8334 $x10729 $x5461 $x3913)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
 )
(check-sat)
(get-proof)