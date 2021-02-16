; DUP3 DUP4 SWAP3 DUP2 SWAP1 => DUP3 DUP4 DUP1 SWAP4
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
(assert
 (exists ((x_0 (_ BitVec 256)) (x_1 (_ BitVec 256)) (x_2 (_ BitVec 256)) )(let (($x6097 (forall ((w (_ BitVec 256)) )(let ((?x6926 (storage_t x_0 x_1 x_2 4 w)))
 (let ((?x1193 (storage_s x_0 x_1 x_2 5 w)))
 (= ?x1193 ?x6926))))
 ))
 (let (($x3723 (exc_halt_t 4)))
 (let (($x3979 (exc_halt_s 5)))
 (let (($x7567 (= $x3979 $x3723)))
 (let (($x8219 (forall ((n (_ BitVec 6)) )(let ((?x3757 (sc_t 4)))
 (let (($x8481 (bvsle ?x3757 n)))
 (let ((?x5407 (stack_t x_0 x_1 x_2 4 n)))
 (let ((?x2514 (stack_s x_0 x_1 x_2 5 n)))
 (let (($x5860 (= ?x2514 ?x5407)))
 (or $x5860 $x8481)))))))
 ))
 (let ((?x3757 (sc_t 4)))
 (let ((?x805 (sc_s 5)))
 (let (($x9143 (= ?x805 ?x3757)))
 (let ((?x8026 (used_gas_t x_0 x_1 x_2 0)))
 (let ((?x2969 (used_gas_s x_0 x_1 x_2 0)))
 (let (($x2958 (= ?x2969 ?x8026)))
 (let (($x8060 (forall ((w (_ BitVec 256)) )(let ((?x2965 (storage_t x_0 x_1 x_2 0 w)))
 (let ((?x6346 (storage_s x_0 x_1 x_2 0 w)))
 (= ?x6346 ?x2965))))
 ))
 (let (($x56 (exc_halt_t 0)))
 (let (($x57 (exc_halt_s 0)))
 (let (($x58 (= $x57 $x56)))
 (let (($x9573 (forall ((n (_ BitVec 6)) )(let ((?x3301 (stack_t x_0 x_1 x_2 0 n)))
 (let ((?x5924 (stack_s x_0 x_1 x_2 0 n)))
 (let (($x6728 (= ?x5924 ?x3301)))
 (let ((?x63 (sc_t 0)))
 (let (($x2788 (bvsle ?x63 n)))
 (or $x2788 $x6728)))))))
 ))
 (let ((?x63 (sc_t 0)))
 (let ((?x72 (sc_s 0)))
 (let (($x73 (= ?x72 ?x63)))
 (let (($x8995 (= $x3723 (or (exc_halt_t 3) (not (bvsle (_ bv0 6) (bvadd (_ bv59 6) (sc_t 3))))))))
 (let (($x1015 (forall ((w (_ BitVec 256)) )(let ((?x8949 (storage_t x_0 x_1 x_2 3 w)))
 (let ((?x6926 (storage_t x_0 x_1 x_2 4 w)))
 (= ?x6926 ?x8949))))
 ))
 (let (($x7161 (forall ((n (_ BitVec 6)) )(let ((?x10276 (stack_t x_0 x_1 x_2 3 n)))
 (let ((?x5407 (stack_t x_0 x_1 x_2 4 n)))
 (or (bvsle (bvadd (_ bv59 6) (sc_t 3)) n) (= ?x5407 ?x10276)))))
 ))
 (let ((?x2012 (sc_t 3)))
 (let (($x632 (= ?x3757 ?x2012)))
 (let (($x8771 (= (used_gas_t x_0 x_1 x_2 4) (+ 3 (used_gas_t x_0 x_1 x_2 3)))))
 (let (($x8952 (= (stack_t x_0 x_1 x_2 4 (bvadd (_ bv62 6) ?x3757)) (stack_t x_0 x_1 x_2 3 (bvadd (_ bv62 6) ?x2012)))))
 (let (($x8490 (= (stack_t x_0 x_1 x_2 4 (bvadd (_ bv61 6) ?x3757)) (stack_t x_0 x_1 x_2 3 (bvadd (_ bv61 6) ?x2012)))))
 (let (($x8913 (= (stack_t x_0 x_1 x_2 4 (bvadd (_ bv60 6) ?x3757)) (stack_t x_0 x_1 x_2 3 (bvadd (_ bv60 6) ?x2012)))))
 (let ((?x5224 (bvadd (_ bv63 6) ?x2012)))
 (let ((?x7760 (stack_t x_0 x_1 x_2 3 ?x5224)))
 (let (($x5859 (= (stack_t x_0 x_1 x_2 4 (bvadd (_ bv63 6) ?x3757)) (stack_t x_0 x_1 x_2 3 (bvadd (_ bv59 6) ?x2012)))))
 (let (($x4476 (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_t 2))))))
 (let (($x1917 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_t 2)))) (_ bv0 1)))))
 (let (($x903 (exc_halt_t 2)))
 (let (($x10336 (exc_halt_t 3)))
 (let (($x7786 (forall ((w (_ BitVec 256)) )(let ((?x11250 (storage_t x_0 x_1 x_2 2 w)))
 (let ((?x8949 (storage_t x_0 x_1 x_2 3 w)))
 (= ?x8949 ?x11250))))
 ))
 (let (($x8321 (forall ((n (_ BitVec 6)) )(let ((?x5927 (stack_t x_0 x_1 x_2 2 n)))
 (let ((?x10276 (stack_t x_0 x_1 x_2 3 n)))
 (let ((?x4056 (sc_t 2)))
 (let ((?x8500 (bvadd (_ bv63 6) ?x4056)))
 (let (($x4544 (bvsle ?x8500 n)))
 (or $x4544 (= ?x10276 ?x5927))))))))
 ))
 (let (($x9325 (= ?x2012 (bvadd (_ bv1 6) (sc_t 2)))))
 (let ((?x6299 (used_gas_t x_0 x_1 x_2 3)))
 (let ((?x4056 (sc_t 2)))
 (let ((?x8500 (bvadd (_ bv63 6) ?x4056)))
 (let ((?x6548 (stack_t x_0 x_1 x_2 2 ?x8500)))
 (let (($x5482 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_t 1)))) (_ bv0 1)))))
 (let (($x1920 (exc_halt_t 1)))
 (let (($x7763 (forall ((w (_ BitVec 256)) )(let ((?x6377 (storage_t x_0 x_1 x_2 1 w)))
 (let ((?x11250 (storage_t x_0 x_1 x_2 2 w)))
 (= ?x11250 ?x6377))))
 ))
 (let (($x7779 (forall ((n (_ BitVec 6)) )(let ((?x7811 (stack_t x_0 x_1 x_2 1 n)))
 (let ((?x5927 (stack_t x_0 x_1 x_2 2 n)))
 (or (bvsle (bvadd (_ bv60 6) (sc_t 1)) n) (= ?x5927 ?x7811)))))
 ))
 (let (($x2529 (= ?x4056 (bvadd (_ bv1 6) (sc_t 1)))))
 (let ((?x5713 (used_gas_t x_0 x_1 x_2 2)))
 (let ((?x4023 (sc_t 1)))
 (let ((?x151 (bvadd (_ bv63 6) ?x4023)))
 (let ((?x1786 (stack_t x_0 x_1 x_2 1 ?x151)))
 (let (($x8105 (= (stack_t x_0 x_1 x_2 2 (bvadd (_ bv62 6) ?x4023)) (stack_t x_0 x_1 x_2 1 (bvadd (_ bv62 6) ?x4023)))))
 (let (($x8733 (= (stack_t x_0 x_1 x_2 2 (bvadd (_ bv61 6) ?x4023)) (stack_t x_0 x_1 x_2 1 (bvadd (_ bv61 6) ?x4023)))))
 (let ((?x8840 (bvadd (_ bv60 6) ?x4023)))
 (let ((?x2966 (stack_t x_0 x_1 x_2 1 ?x8840)))
 (let (($x1611 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x63))) (_ bv0 1)))))
 (let (($x7776 (forall ((w (_ BitVec 256)) )(let ((?x2965 (storage_t x_0 x_1 x_2 0 w)))
 (let ((?x6377 (storage_t x_0 x_1 x_2 1 w)))
 (= ?x6377 ?x2965))))
 ))
 (let (($x7769 (forall ((n (_ BitVec 6)) )(let ((?x3301 (stack_t x_0 x_1 x_2 0 n)))
 (let ((?x7811 (stack_t x_0 x_1 x_2 1 n)))
 (let (($x6056 (= ?x7811 ?x3301)))
 (or $x6056 (bvsle (bvadd (_ bv61 6) (sc_t 0)) n))))))
 ))
 (let (($x7517 (= ?x4023 (bvadd (_ bv1 6) ?x63))))
 (let ((?x8895 (bvadd (_ bv63 6) ?x63)))
 (let ((?x2509 (stack_t x_0 x_1 x_2 0 ?x8895)))
 (let ((?x6511 (bvadd (_ bv62 6) ?x63)))
 (let ((?x10915 (stack_t x_0 x_1 x_2 0 ?x6511)))
 (let ((?x9045 (bvadd (_ bv61 6) ?x63)))
 (let ((?x2940 (stack_t x_0 x_1 x_2 0 ?x9045)))
 (let (($x6033 (= $x3979 (or (exc_halt_s 4) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 4))))))))
 (let (($x7256 (forall ((w (_ BitVec 256)) )(let ((?x7627 (storage_s x_0 x_1 x_2 4 w)))
 (let ((?x1193 (storage_s x_0 x_1 x_2 5 w)))
 (= ?x1193 ?x7627))))
 ))
 (let (($x1179 (forall ((n (_ BitVec 6)) )(let ((?x4305 (sc_s 4)))
 (let ((?x9282 (bvadd (_ bv62 6) ?x4305)))
 (let (($x1651 (bvsle ?x9282 n)))
 (let ((?x9672 (stack_s x_0 x_1 x_2 4 n)))
 (let ((?x2514 (stack_s x_0 x_1 x_2 5 n)))
 (or (= ?x2514 ?x9672) $x1651)))))))
 ))
 (let ((?x4305 (sc_s 4)))
 (let (($x445 (= ?x805 ?x4305)))
 (let ((?x6010 (used_gas_s x_0 x_1 x_2 5)))
 (let ((?x8635 (bvadd (_ bv63 6) ?x4305)))
 (let ((?x6359 (stack_s x_0 x_1 x_2 4 ?x8635)))
 (let ((?x9282 (bvadd (_ bv62 6) ?x4305)))
 (let ((?x7420 (stack_s x_0 x_1 x_2 4 ?x9282)))
 (let (($x6939 (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 3))))))
 (let (($x3191 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 3)))) (_ bv0 1)))))
 (let (($x292 (exc_halt_s 3)))
 (let (($x64 (exc_halt_s 4)))
 (let (($x5888 (forall ((w (_ BitVec 256)) )(let ((?x6825 (storage_s x_0 x_1 x_2 3 w)))
 (let ((?x7627 (storage_s x_0 x_1 x_2 4 w)))
 (= ?x7627 ?x6825))))
 ))
 (let (($x6564 (forall ((n (_ BitVec 6)) )(let ((?x694 (stack_s x_0 x_1 x_2 3 n)))
 (let ((?x9672 (stack_s x_0 x_1 x_2 4 n)))
 (or (= ?x9672 ?x694) (bvsle (bvadd (_ bv62 6) (sc_s 3)) n)))))
 ))
 (let (($x6906 (= ?x4305 (bvadd (_ bv1 6) (sc_s 3)))))
 (let ((?x9009 (used_gas_s x_0 x_1 x_2 4)))
 (let (($x941 (= ?x9009 (+ 3 (used_gas_s x_0 x_1 x_2 3)))))
 (let ((?x275 (sc_s 3)))
 (let ((?x8615 (bvadd (_ bv63 6) ?x275)))
 (let ((?x3519 (stack_s x_0 x_1 x_2 3 ?x8615)))
 (let ((?x7606 (bvadd (_ bv62 6) ?x275)))
 (let ((?x512 (stack_s x_0 x_1 x_2 3 ?x7606)))
 (let (($x3575 (= ?x6359 ?x512)))
 (let (($x8447 (= $x292 (or (exc_halt_s 2) (not (bvsle (_ bv0 6) (bvadd (_ bv60 6) (sc_s 2))))))))
 (let (($x6272 (forall ((w (_ BitVec 256)) )(let ((?x5921 (storage_s x_0 x_1 x_2 2 w)))
 (let ((?x6825 (storage_s x_0 x_1 x_2 3 w)))
 (= ?x6825 ?x5921))))
 ))
 (let (($x9012 (forall ((n (_ BitVec 6)) )(let ((?x6868 (stack_s x_0 x_1 x_2 2 n)))
 (let ((?x694 (stack_s x_0 x_1 x_2 3 n)))
 (let (($x516 (= ?x694 ?x6868)))
 (or $x516 (bvsle (bvadd (_ bv60 6) (sc_s 2)) n))))))
 ))
 (let ((?x218 (sc_s 2)))
 (let (($x9415 (= ?x275 ?x218)))
 (let ((?x6035 (used_gas_s x_0 x_1 x_2 3)))
 (let (($x9391 (= ?x6035 (+ 3 (used_gas_s x_0 x_1 x_2 2)))))
 (let (($x7382 (= (stack_s x_0 x_1 x_2 3 (bvadd (_ bv61 6) ?x275)) (stack_s x_0 x_1 x_2 2 (bvadd (_ bv61 6) ?x218)))))
 (let ((?x11283 (bvadd (_ bv63 6) ?x218)))
 (let ((?x2766 (stack_s x_0 x_1 x_2 2 ?x11283)))
 (let (($x2107 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 1)))) (_ bv0 1)))))
 (let (($x189 (exc_halt_s 1)))
 (let (($x247 (exc_halt_s 2)))
 (let (($x2806 (forall ((w (_ BitVec 256)) )(let ((?x10310 (storage_s x_0 x_1 x_2 1 w)))
 (let ((?x5921 (storage_s x_0 x_1 x_2 2 w)))
 (= ?x5921 ?x10310))))
 ))
 (let (($x8513 (forall ((n (_ BitVec 6)) )(let ((?x7497 (stack_s x_0 x_1 x_2 1 n)))
 (let ((?x6868 (stack_s x_0 x_1 x_2 2 n)))
 (let (($x3016 (= ?x6868 ?x7497)))
 (or $x3016 (bvsle (bvadd (_ bv60 6) (sc_s 1)) n))))))
 ))
 (let (($x8657 (= ?x218 (bvadd (_ bv1 6) (sc_s 1)))))
 (let ((?x1302 (used_gas_s x_0 x_1 x_2 2)))
 (let (($x3848 (= ?x1302 (+ 3 (used_gas_s x_0 x_1 x_2 1)))))
 (let ((?x154 (sc_s 1)))
 (let ((?x7444 (bvadd (_ bv63 6) ?x154)))
 (let ((?x2662 (stack_s x_0 x_1 x_2 1 ?x7444)))
 (let (($x8732 (= (stack_s x_0 x_1 x_2 2 (bvadd (_ bv62 6) ?x154)) (stack_s x_0 x_1 x_2 1 (bvadd (_ bv62 6) ?x154)))))
 (let (($x3297 (= (stack_s x_0 x_1 x_2 2 (bvadd (_ bv61 6) ?x154)) (stack_s x_0 x_1 x_2 1 (bvadd (_ bv61 6) ?x154)))))
 (let ((?x8699 (bvadd (_ bv60 6) ?x154)))
 (let ((?x8948 (stack_s x_0 x_1 x_2 1 ?x8699)))
 (let (($x2400 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x72))) (_ bv0 1)))))
 (let (($x7900 (forall ((w (_ BitVec 256)) )(let ((?x6346 (storage_s x_0 x_1 x_2 0 w)))
 (let ((?x10310 (storage_s x_0 x_1 x_2 1 w)))
 (= ?x10310 ?x6346))))
 ))
 (let (($x9147 (forall ((n (_ BitVec 6)) )(let ((?x5924 (stack_s x_0 x_1 x_2 0 n)))
 (let ((?x7497 (stack_s x_0 x_1 x_2 1 n)))
 (let (($x1782 (= ?x7497 ?x5924)))
 (or $x1782 (bvsle (bvadd (_ bv61 6) (sc_s 0)) n))))))
 ))
 (let (($x8455 (= ?x154 (bvadd (_ bv1 6) ?x72))))
 (let ((?x3699 (used_gas_s x_0 x_1 x_2 1)))
 (let (($x7198 (= ?x3699 (+ 3 ?x2969))))
 (let (($x3765 (= (stack_s x_0 x_1 x_2 1 (bvadd (_ bv63 6) ?x72)) (stack_s x_0 x_1 x_2 0 (bvadd (_ bv63 6) ?x72)))))
 (let ((?x3359 (bvadd (_ bv62 6) ?x72)))
 (let ((?x1017 (stack_s x_0 x_1 x_2 0 ?x3359)))
 (let (($x3632 (= (stack_s x_0 x_1 x_2 1 ?x3359) ?x1017)))
 (let ((?x9273 (bvadd (_ bv61 6) ?x72)))
 (let ((?x8126 (stack_s x_0 x_1 x_2 0 ?x9273)))
 (let (($x6880 (forall ((w (_ BitVec 256)) )(let ((?x6346 (storage_s x_0 x_1 x_2 0 w)))
 (= ?x6346 (_ bv0 256))))
 ))
 (let (($x1020 (= ?x2969 0)))
 (let (($x2233 (not $x57)))
 (let (($x2449 (= (stack_s x_0 x_1 x_2 0 (_ bv2 6)) x_2)))
 (let (($x863 (= (stack_s x_0 x_1 x_2 0 (_ bv1 6)) x_1)))
 (let (($x10543 (= (stack_s x_0 x_1 x_2 0 (_ bv0 6)) x_0)))
 (let (($x96 (= ?x72 (_ bv3 6))))
 (and $x96 $x10543 $x863 $x2449 $x2233 $x1020 $x6880 (= ?x2662 ?x8126) (= (stack_s x_0 x_1 x_2 1 ?x9273) ?x8126) $x3632 $x3765 $x7198 $x8455 $x9147 $x7900 (= $x189 (or $x57 $x2400 (not (bvsle (_ bv0 6) ?x9273)))) (= ?x2766 ?x8948) (= (stack_s x_0 x_1 x_2 2 ?x8699) ?x8948) $x3297 $x8732 (= (stack_s x_0 x_1 x_2 2 ?x7444) ?x2662) $x3848 $x8657 $x8513 $x2806 (= $x247 (or $x189 $x2107 (not (bvsle (_ bv0 6) ?x8699)))) (= ?x3519 (stack_s x_0 x_1 x_2 2 (bvadd (_ bv60 6) ?x218))) (= (stack_s x_0 x_1 x_2 3 (bvadd (_ bv60 6) ?x275)) ?x2766) $x7382 (= ?x512 (stack_s x_0 x_1 x_2 2 (bvadd (_ bv62 6) ?x218))) $x9391 $x9415 $x9012 $x6272 $x8447 $x3575 (= (stack_s x_0 x_1 x_2 4 ?x7606) ?x512) (= (stack_s x_0 x_1 x_2 4 ?x8615) ?x3519) $x941 $x6906 $x6564 $x5888 (= $x64 (or $x292 $x3191 $x6939)) (= (stack_s x_0 x_1 x_2 5 (bvadd (_ bv63 6) ?x805)) ?x7420) (= (stack_s x_0 x_1 x_2 5 (bvadd (_ bv62 6) ?x805)) ?x6359) (= ?x6010 (+ 3 ?x9009)) $x445 $x1179 $x7256 $x6033 (= ?x1786 ?x2940) (= (stack_t x_0 x_1 x_2 1 ?x9045) ?x2940) (= (stack_t x_0 x_1 x_2 1 ?x6511) ?x10915) (= (stack_t x_0 x_1 x_2 1 ?x8895) ?x2509) (= (used_gas_t x_0 x_1 x_2 1) (+ 3 ?x8026)) $x7517 $x7769 $x7776 (= $x1920 (or $x56 $x1611 (not (bvsle (_ bv0 6) ?x9045)))) (= ?x6548 ?x2966) (= (stack_t x_0 x_1 x_2 2 ?x8840) ?x2966) $x8733 $x8105 (= (stack_t x_0 x_1 x_2 2 ?x151) ?x1786) (= ?x5713 (+ 3 (used_gas_t x_0 x_1 x_2 1))) $x2529 $x7779 $x7763 (= $x903 (or (not (bvsle (_ bv0 6) ?x8840)) $x1920 $x5482)) (= ?x7760 ?x6548) (= (stack_t x_0 x_1 x_2 3 ?x8500) ?x6548) (= ?x6299 (+ 3 ?x5713)) $x9325 $x8321 $x7786 (= $x10336 (or $x903 $x1917 $x4476)) $x5859 (= (stack_t x_0 x_1 x_2 4 (bvadd (_ bv59 6) ?x3757)) ?x7760) $x8913 $x8490 $x8952 $x8771 $x632 $x7161 $x1015 $x8995 $x73 $x9573 $x58 $x8060 $x2958 (not (and $x9143 $x8219 $x7567 $x6097))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
 )
(check-sat)
(get-proof)