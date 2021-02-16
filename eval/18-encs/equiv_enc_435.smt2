; SWAP2 SWAP1 SWAP2 OR DUP1 DUP3 => DUP2 SWAP3 OR DUP1 SWAP2
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
 (exists ((x_0 (_ BitVec 256)) (x_1 (_ BitVec 256)) (x_2 (_ BitVec 256)) )(let (($x10383 (forall ((w (_ BitVec 256)) )(let ((?x3794 (storage_t x_0 x_1 x_2 5 w)))
 (let ((?x4245 (storage_s x_0 x_1 x_2 6 w)))
 (= ?x4245 ?x3794))))
 ))
 (let (($x886 (exc_halt_t 5)))
 (let (($x772 (exc_halt_s 6)))
 (let (($x889 (= $x772 $x886)))
 (let (($x11074 (forall ((n (_ BitVec 6)) )(let ((?x2954 (stack_t x_0 x_1 x_2 5 n)))
 (let ((?x3258 (stack_s x_0 x_1 x_2 6 n)))
 (let (($x6187 (= ?x3258 ?x2954)))
 (let ((?x919 (sc_t 5)))
 (let (($x5906 (bvsle ?x919 n)))
 (or $x5906 $x6187)))))))
 ))
 (let ((?x919 (sc_t 5)))
 (let ((?x926 (sc_s 6)))
 (let (($x929 (= ?x926 ?x919)))
 (let ((?x1661 (used_gas_t x_0 x_1 x_2 0)))
 (let ((?x11801 (used_gas_s x_0 x_1 x_2 0)))
 (let (($x613 (= ?x11801 ?x1661)))
 (let (($x1630 (forall ((w (_ BitVec 256)) )(let ((?x8748 (storage_t x_0 x_1 x_2 0 w)))
 (let ((?x9643 (storage_s x_0 x_1 x_2 0 w)))
 (= ?x9643 ?x8748))))
 ))
 (let (($x56 (exc_halt_t 0)))
 (let (($x57 (exc_halt_s 0)))
 (let (($x58 (= $x57 $x56)))
 (let (($x7721 (forall ((n (_ BitVec 6)) )(let ((?x63 (sc_t 0)))
 (let (($x2525 (bvsle ?x63 n)))
 (let ((?x11216 (stack_t x_0 x_1 x_2 0 n)))
 (let ((?x4854 (stack_s x_0 x_1 x_2 0 n)))
 (let (($x2176 (= ?x4854 ?x11216)))
 (or $x2176 $x2525)))))))
 ))
 (let ((?x63 (sc_t 0)))
 (let ((?x72 (sc_s 0)))
 (let (($x73 (= ?x72 ?x63)))
 (let (($x5107 (= $x886 (or (exc_halt_t 4) (not (bvsle (_ bv0 6) (bvadd (_ bv61 6) (sc_t 4))))))))
 (let (($x4301 (forall ((w (_ BitVec 256)) )(let ((?x9314 (storage_t x_0 x_1 x_2 4 w)))
 (let ((?x3794 (storage_t x_0 x_1 x_2 5 w)))
 (= ?x3794 ?x9314))))
 ))
 (let (($x9324 (forall ((n (_ BitVec 6)) )(let ((?x7193 (stack_t x_0 x_1 x_2 4 n)))
 (let ((?x2954 (stack_t x_0 x_1 x_2 5 n)))
 (or (= ?x2954 ?x7193) (bvsle (bvadd (_ bv61 6) (sc_t 4)) n)))))
 ))
 (let ((?x11631 (sc_t 4)))
 (let (($x2721 (= ?x919 ?x11631)))
 (let (($x4321 (= (used_gas_t x_0 x_1 x_2 5) (+ 3 (used_gas_t x_0 x_1 x_2 4)))))
 (let (($x7950 (= (stack_t x_0 x_1 x_2 5 (bvadd (_ bv62 6) ?x919)) (stack_t x_0 x_1 x_2 4 (bvadd (_ bv62 6) ?x11631)))))
 (let ((?x1883 (bvadd (_ bv63 6) ?x11631)))
 (let ((?x6849 (stack_t x_0 x_1 x_2 4 ?x1883)))
 (let (($x10023 (= (stack_t x_0 x_1 x_2 5 (bvadd (_ bv63 6) ?x919)) (stack_t x_0 x_1 x_2 4 (bvadd (_ bv61 6) ?x11631)))))
 (let (($x7763 (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_t 3))))))
 (let (($x6783 (exc_halt_t 3)))
 (let (($x6329 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_t 3)))) (_ bv0 1)))))
 (let (($x7722 (exc_halt_t 4)))
 (let (($x4600 (forall ((w (_ BitVec 256)) )(let ((?x4395 (storage_t x_0 x_1 x_2 3 w)))
 (let ((?x9314 (storage_t x_0 x_1 x_2 4 w)))
 (= ?x9314 ?x4395))))
 ))
 (let (($x9759 (forall ((n (_ BitVec 6)) )(let ((?x996 (stack_t x_0 x_1 x_2 3 n)))
 (let ((?x7193 (stack_t x_0 x_1 x_2 4 n)))
 (let ((?x6438 (sc_t 3)))
 (let ((?x1993 (bvadd (_ bv63 6) ?x6438)))
 (let (($x5009 (bvsle ?x1993 n)))
 (or $x5009 (= ?x7193 ?x996))))))))
 ))
 (let (($x6839 (= ?x11631 (bvadd (_ bv1 6) (sc_t 3)))))
 (let ((?x4390 (used_gas_t x_0 x_1 x_2 4)))
 (let ((?x6438 (sc_t 3)))
 (let ((?x1993 (bvadd (_ bv63 6) ?x6438)))
 (let ((?x6342 (stack_t x_0 x_1 x_2 3 ?x1993)))
 (let (($x10273 (= $x6783 (or (exc_halt_t 2) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_t 2))))))))
 (let (($x1635 (forall ((w (_ BitVec 256)) )(let ((?x7874 (storage_t x_0 x_1 x_2 2 w)))
 (let ((?x4395 (storage_t x_0 x_1 x_2 3 w)))
 (= ?x4395 ?x7874))))
 ))
 (let (($x553 (forall ((n (_ BitVec 6)) )(let ((?x2714 (sc_t 2)))
 (let ((?x2716 (bvadd (_ bv62 6) ?x2714)))
 (let (($x7852 (bvsle ?x2716 n)))
 (let ((?x866 (stack_t x_0 x_1 x_2 2 n)))
 (let ((?x996 (stack_t x_0 x_1 x_2 3 n)))
 (or (= ?x996 ?x866) $x7852)))))))
 ))
 (let ((?x2714 (sc_t 2)))
 (let ((?x5200 (bvadd (_ bv63 6) ?x2714)))
 (let (($x2167 (= ?x6438 ?x5200)))
 (let ((?x11499 (used_gas_t x_0 x_1 x_2 3)))
 (let ((?x2716 (bvadd (_ bv62 6) ?x2714)))
 (let ((?x1582 (stack_t x_0 x_1 x_2 2 ?x2716)))
 (let ((?x9776 (stack_t x_0 x_1 x_2 2 ?x5200)))
 (let (($x3071 (exc_halt_t 2)))
 (let (($x1829 (= $x3071 (or (exc_halt_t 1) (not (bvsle (_ bv0 6) (bvadd (_ bv60 6) (sc_t 1))))))))
 (let (($x3490 (forall ((w (_ BitVec 256)) )(let ((?x10621 (storage_t x_0 x_1 x_2 1 w)))
 (let ((?x7874 (storage_t x_0 x_1 x_2 2 w)))
 (= ?x7874 ?x10621))))
 ))
 (let (($x7787 (forall ((n (_ BitVec 6)) )(let ((?x7238 (stack_t x_0 x_1 x_2 1 n)))
 (let ((?x866 (stack_t x_0 x_1 x_2 2 n)))
 (or (bvsle (bvadd (_ bv60 6) (sc_t 1)) n) (= ?x866 ?x7238)))))
 ))
 (let ((?x4564 (used_gas_t x_0 x_1 x_2 2)))
 (let (($x4073 (= (stack_t x_0 x_1 x_2 2 (bvadd (_ bv61 6) ?x2714)) (stack_t x_0 x_1 x_2 1 (bvadd (_ bv61 6) (sc_t 1))))))
 (let ((?x8347 (sc_t 1)))
 (let ((?x9429 (bvadd (_ bv63 6) ?x8347)))
 (let ((?x6087 (stack_t x_0 x_1 x_2 1 ?x9429)))
 (let (($x3887 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x63))) (_ bv0 1)))))
 (let (($x2759 (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) ?x63)))))
 (let (($x3508 (exc_halt_t 1)))
 (let (($x8320 (forall ((w (_ BitVec 256)) )(let ((?x8748 (storage_t x_0 x_1 x_2 0 w)))
 (let ((?x10621 (storage_t x_0 x_1 x_2 1 w)))
 (= ?x10621 ?x8748))))
 ))
 (let (($x2324 (forall ((n (_ BitVec 6)) )(let ((?x63 (sc_t 0)))
 (let ((?x8027 (bvadd (_ bv62 6) ?x63)))
 (let (($x11443 (bvsle ?x8027 n)))
 (let ((?x11216 (stack_t x_0 x_1 x_2 0 n)))
 (let ((?x7238 (stack_t x_0 x_1 x_2 1 n)))
 (or (= ?x7238 ?x11216) $x11443)))))))
 ))
 (let (($x5258 (= ?x8347 (bvadd (_ bv1 6) ?x63))))
 (let (($x10139 (= (stack_t x_0 x_1 x_2 1 (bvadd (_ bv63 6) ?x63)) (stack_t x_0 x_1 x_2 0 (bvadd (_ bv63 6) ?x63)))))
 (let ((?x8027 (bvadd (_ bv62 6) ?x63)))
 (let ((?x2519 (stack_t x_0 x_1 x_2 0 ?x8027)))
 (let (($x11317 (exc_halt_s 5)))
 (let (($x7391 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 5)))) (_ bv0 1)))))
 (let (($x6612 (forall ((w (_ BitVec 256)) )(let ((?x9101 (storage_s x_0 x_1 x_2 5 w)))
 (let ((?x4245 (storage_s x_0 x_1 x_2 6 w)))
 (= ?x4245 ?x9101))))
 ))
 (let (($x52 (forall ((n (_ BitVec 6)) )(let ((?x5127 (stack_s x_0 x_1 x_2 5 n)))
 (let ((?x3258 (stack_s x_0 x_1 x_2 6 n)))
 (or (bvsle (bvadd (_ bv61 6) (sc_s 5)) n) (= ?x3258 ?x5127)))))
 ))
 (let (($x3981 (= ?x926 (bvadd (_ bv1 6) (sc_s 5)))))
 (let (($x5649 (= (used_gas_s x_0 x_1 x_2 6) (+ 3 (used_gas_s x_0 x_1 x_2 5)))))
 (let ((?x4319 (sc_s 5)))
 (let ((?x2994 (bvadd (_ bv63 6) ?x4319)))
 (let ((?x2170 (stack_s x_0 x_1 x_2 5 ?x2994)))
 (let (($x228 (= (stack_s x_0 x_1 x_2 6 (bvadd (_ bv62 6) ?x4319)) (stack_s x_0 x_1 x_2 5 (bvadd (_ bv62 6) ?x4319)))))
 (let ((?x3512 (bvadd (_ bv61 6) ?x4319)))
 (let ((?x5437 (stack_s x_0 x_1 x_2 5 ?x3512)))
 (let (($x9880 (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_s 4))))))
 (let (($x7172 (exc_halt_s 4)))
 (let (($x6340 (or (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 4)))) (_ bv0 1))) $x7172 $x9880)))
 (let (($x3167 (forall ((w (_ BitVec 256)) )(let ((?x4173 (storage_s x_0 x_1 x_2 4 w)))
 (let ((?x9101 (storage_s x_0 x_1 x_2 5 w)))
 (= ?x9101 ?x4173))))
 ))
 (let (($x909 (forall ((n (_ BitVec 6)) )(let ((?x4305 (sc_s 4)))
 (let ((?x11124 (bvadd (_ bv63 6) ?x4305)))
 (let (($x7397 (bvsle ?x11124 n)))
 (let ((?x4068 (stack_s x_0 x_1 x_2 4 n)))
 (let ((?x5127 (stack_s x_0 x_1 x_2 5 n)))
 (or (= ?x5127 ?x4068) $x7397)))))))
 ))
 (let ((?x9063 (used_gas_s x_0 x_1 x_2 5)))
 (let ((?x4305 (sc_s 4)))
 (let ((?x11124 (bvadd (_ bv63 6) ?x4305)))
 (let ((?x11323 (stack_s x_0 x_1 x_2 4 ?x11124)))
 (let (($x7783 (= $x7172 (or (exc_halt_s 3) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 3))))))))
 (let (($x11789 (forall ((w (_ BitVec 256)) )(let ((?x5848 (storage_s x_0 x_1 x_2 3 w)))
 (let ((?x4173 (storage_s x_0 x_1 x_2 4 w)))
 (= ?x4173 ?x5848))))
 ))
 (let (($x9557 (forall ((n (_ BitVec 6)) )(let ((?x275 (sc_s 3)))
 (let ((?x6337 (bvadd (_ bv62 6) ?x275)))
 (let (($x6005 (bvsle ?x6337 n)))
 (let ((?x6603 (stack_s x_0 x_1 x_2 3 n)))
 (let ((?x4068 (stack_s x_0 x_1 x_2 4 n)))
 (or (= ?x4068 ?x6603) $x6005)))))))
 ))
 (let ((?x275 (sc_s 3)))
 (let ((?x3684 (bvadd (_ bv63 6) ?x275)))
 (let (($x11684 (= ?x4305 ?x3684)))
 (let ((?x11579 (used_gas_s x_0 x_1 x_2 4)))
 (let ((?x6337 (bvadd (_ bv62 6) ?x275)))
 (let ((?x398 (stack_s x_0 x_1 x_2 3 ?x6337)))
 (let ((?x7678 (stack_s x_0 x_1 x_2 3 ?x3684)))
 (let (($x292 (exc_halt_s 3)))
 (let (($x8330 (= $x292 (or (exc_halt_s 2) (not (bvsle (_ bv0 6) (bvadd (_ bv61 6) (sc_s 2))))))))
 (let (($x9788 (forall ((w (_ BitVec 256)) )(let ((?x6117 (storage_s x_0 x_1 x_2 2 w)))
 (let ((?x5848 (storage_s x_0 x_1 x_2 3 w)))
 (= ?x5848 ?x6117))))
 ))
 (let (($x9921 (forall ((n (_ BitVec 6)) )(let ((?x218 (sc_s 2)))
 (let ((?x861 (bvadd (_ bv61 6) ?x218)))
 (let (($x1357 (bvsle ?x861 n)))
 (let ((?x5300 (stack_s x_0 x_1 x_2 2 n)))
 (let ((?x6603 (stack_s x_0 x_1 x_2 3 n)))
 (or (= ?x6603 ?x5300) $x1357)))))))
 ))
 (let ((?x218 (sc_s 2)))
 (let (($x7762 (= ?x275 ?x218)))
 (let ((?x5557 (used_gas_s x_0 x_1 x_2 3)))
 (let ((?x5784 (bvadd (_ bv63 6) ?x218)))
 (let ((?x10886 (stack_s x_0 x_1 x_2 2 ?x5784)))
 (let (($x247 (exc_halt_s 2)))
 (let (($x4535 (= $x247 (or (exc_halt_s 1) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 1))))))))
 (let (($x3832 (forall ((w (_ BitVec 256)) )(let ((?x9883 (storage_s x_0 x_1 x_2 1 w)))
 (let ((?x6117 (storage_s x_0 x_1 x_2 2 w)))
 (= ?x6117 ?x9883))))
 ))
 (let (($x10915 (forall ((n (_ BitVec 6)) )(let ((?x154 (sc_s 1)))
 (let ((?x1887 (bvadd (_ bv62 6) ?x154)))
 (let (($x3587 (bvsle ?x1887 n)))
 (let ((?x855 (stack_s x_0 x_1 x_2 1 n)))
 (let ((?x5300 (stack_s x_0 x_1 x_2 2 n)))
 (or (= ?x5300 ?x855) $x3587)))))))
 ))
 (let ((?x154 (sc_s 1)))
 (let (($x1088 (= ?x218 ?x154)))
 (let ((?x10854 (used_gas_s x_0 x_1 x_2 2)))
 (let ((?x5786 (bvadd (_ bv63 6) ?x154)))
 (let ((?x5077 (stack_s x_0 x_1 x_2 1 ?x5786)))
 (let ((?x5210 (bvadd (_ bv62 6) ?x218)))
 (let ((?x359 (stack_s x_0 x_1 x_2 2 ?x5210)))
 (let (($x11079 (forall ((w (_ BitVec 256)) )(let ((?x9643 (storage_s x_0 x_1 x_2 0 w)))
 (let ((?x9883 (storage_s x_0 x_1 x_2 1 w)))
 (= ?x9883 ?x9643))))
 ))
 (let (($x1603 (forall ((n (_ BitVec 6)) )(let ((?x4854 (stack_s x_0 x_1 x_2 0 n)))
 (let ((?x855 (stack_s x_0 x_1 x_2 1 n)))
 (or (bvsle (bvadd (_ bv61 6) (sc_s 0)) n) (= ?x855 ?x4854)))))
 ))
 (let ((?x1887 (bvadd (_ bv62 6) ?x154)))
 (let ((?x7794 (stack_s x_0 x_1 x_2 1 ?x1887)))
 (let (($x3431 (= (stack_s x_0 x_1 x_2 1 (bvadd (_ bv61 6) ?x154)) (stack_s x_0 x_1 x_2 0 (bvadd (_ bv63 6) ?x72)))))
 (let (($x4150 (forall ((w (_ BitVec 256)) )(let ((?x9643 (storage_s x_0 x_1 x_2 0 w)))
 (= ?x9643 (_ bv0 256))))
 ))
 (let (($x7069 (= ?x11801 0)))
 (let (($x4511 (not $x57)))
 (let (($x10172 (= (stack_s x_0 x_1 x_2 0 (_ bv2 6)) x_2)))
 (let (($x8925 (= (stack_s x_0 x_1 x_2 0 (_ bv1 6)) x_1)))
 (let (($x4146 (= (stack_s x_0 x_1 x_2 0 (_ bv0 6)) x_0)))
 (let (($x5315 (= ?x72 (_ bv3 6))))
 (and $x5315 $x4146 $x8925 $x10172 $x4511 $x7069 $x4150 (= ?x5077 (stack_s x_0 x_1 x_2 0 (bvadd (_ bv61 6) ?x72))) $x3431 (= ?x7794 (stack_s x_0 x_1 x_2 0 (bvadd (_ bv62 6) ?x72))) (= (used_gas_s x_0 x_1 x_2 1) (+ 3 ?x11801)) (= ?x154 ?x72) $x1603 $x11079 (= (exc_halt_s 1) (or $x57 (not (bvsle (_ bv0 6) (bvadd (_ bv61 6) ?x72))))) (= ?x10886 ?x7794) (= ?x359 ?x5077) (= ?x10854 (+ 3 (used_gas_s x_0 x_1 x_2 1))) $x1088 $x10915 $x3832 $x4535 (= ?x7678 (stack_s x_0 x_1 x_2 2 (bvadd (_ bv61 6) ?x218))) (= (stack_s x_0 x_1 x_2 3 (bvadd (_ bv61 6) ?x275)) ?x10886) (= ?x398 ?x359) (= ?x5557 (+ 3 ?x10854)) $x7762 $x9921 $x9788 $x8330 (= ?x11323 (bvor ?x7678 ?x398)) (= ?x11579 (+ 3 ?x5557)) $x11684 $x9557 $x11789 $x7783 (= ?x2170 ?x11323) (= (stack_s x_0 x_1 x_2 5 ?x11124) ?x11323) (= ?x9063 (+ 3 ?x11579)) (= ?x4319 (bvadd (_ bv1 6) ?x4305)) $x909 $x3167 (= $x11317 $x6340) (= (stack_s x_0 x_1 x_2 6 (bvadd (_ bv63 6) ?x926)) ?x5437) (= (stack_s x_0 x_1 x_2 6 ?x3512) ?x5437) $x228 (= (stack_s x_0 x_1 x_2 6 ?x2994) ?x2170) $x5649 $x3981 $x52 $x6612 (= $x772 (or (not (bvsle (_ bv0 6) ?x3512)) $x7391 $x11317)) (= ?x6087 ?x2519) (= (stack_t x_0 x_1 x_2 1 ?x8027) ?x2519) $x10139 (= (used_gas_t x_0 x_1 x_2 1) (+ 3 ?x1661)) $x5258 $x2324 $x8320 (= $x3508 (or $x56 $x2759 $x3887)) (= ?x9776 (stack_t x_0 x_1 x_2 1 (bvadd (_ bv60 6) ?x8347))) (= (stack_t x_0 x_1 x_2 2 (bvadd (_ bv60 6) ?x2714)) ?x6087) $x4073 (= ?x1582 (stack_t x_0 x_1 x_2 1 (bvadd (_ bv62 6) ?x8347))) (= ?x4564 (+ 3 (used_gas_t x_0 x_1 x_2 1))) (= ?x2714 ?x8347) $x7787 $x3490 $x1829 (= ?x6342 (bvor ?x9776 ?x1582)) (= ?x11499 (+ 3 ?x4564)) $x2167 $x553 $x1635 $x10273 (= ?x6849 ?x6342) (= (stack_t x_0 x_1 x_2 4 ?x1993) ?x6342) (= ?x4390 (+ 3 ?x11499)) $x6839 $x9759 $x4600 (= $x7722 (or $x6329 $x6783 $x7763)) $x10023 (= (stack_t x_0 x_1 x_2 5 (bvadd (_ bv61 6) ?x919)) ?x6849) $x7950 $x4321 $x2721 $x9324 $x4301 $x5107 $x73 $x7721 $x58 $x1630 $x613 (not (and $x929 $x11074 $x889 $x10383)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
 )
(check-sat)
(get-proof)