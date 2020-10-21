; POP POP PUSH cw_1 SWAP2 SWAP1 SWAP2 => POP POP SWAP1 PUSH cw_1
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
 (exists ((x_0 (_ BitVec 256)) (x_1 (_ BitVec 256)) (x_2 (_ BitVec 256)) (x_3 (_ BitVec 256)) )(let (($x8932 (forall ((w (_ BitVec 256)) )(let ((?x994 (storage_t x_0 x_1 x_2 x_3 w_1 4 w)))
 (let ((?x11387 (storage_s x_0 x_1 x_2 x_3 w_1 6 w)))
 (= ?x11387 ?x994))))
 ))
 (let (($x7854 (exc_halt_t 4)))
 (let (($x772 (exc_halt_s 6)))
 (let (($x4404 (= $x772 $x7854)))
 (let (($x4349 (forall ((n (_ BitVec 6)) )(let ((?x10468 (sc_t 4)))
 (let (($x10525 (bvsle ?x10468 n)))
 (let ((?x8880 (stack_t x_0 x_1 x_2 x_3 w_1 4 n)))
 (let ((?x1272 (stack_s x_0 x_1 x_2 x_3 w_1 6 n)))
 (let (($x307 (= ?x1272 ?x8880)))
 (or $x307 $x10525)))))))
 ))
 (let ((?x10468 (sc_t 4)))
 (let ((?x926 (sc_s 6)))
 (let (($x5134 (= ?x926 ?x10468)))
 (let ((?x2321 (used_gas_t x_0 x_1 x_2 x_3 w_1 0)))
 (let ((?x7642 (used_gas_s x_0 x_1 x_2 x_3 w_1 0)))
 (let (($x2434 (= ?x7642 ?x2321)))
 (let (($x11327 (forall ((w (_ BitVec 256)) )(let ((?x6684 (storage_t x_0 x_1 x_2 x_3 w_1 0 w)))
 (let ((?x9185 (storage_s x_0 x_1 x_2 x_3 w_1 0 w)))
 (= ?x9185 ?x6684))))
 ))
 (let (($x56 (exc_halt_t 0)))
 (let (($x57 (exc_halt_s 0)))
 (let (($x58 (= $x57 $x56)))
 (let (($x4931 (forall ((n (_ BitVec 6)) )(let ((?x63 (sc_t 0)))
 (let (($x11230 (bvsle ?x63 n)))
 (let ((?x916 (stack_t x_0 x_1 x_2 x_3 w_1 0 n)))
 (let ((?x11300 (stack_s x_0 x_1 x_2 x_3 w_1 0 n)))
 (let (($x9567 (= ?x11300 ?x916)))
 (or $x9567 $x11230)))))))
 ))
 (let ((?x63 (sc_t 0)))
 (let ((?x72 (sc_s 0)))
 (let (($x73 (= ?x72 ?x63)))
 (let (($x7621 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_t 3)))) (_ bv0 1)))))
 (let (($x4112 (exc_halt_t 3)))
 (let (($x6985 (= $x7854 (or $x4112 $x7621))))
 (let (($x10643 (forall ((w (_ BitVec 256)) )(let ((?x4752 (storage_t x_0 x_1 x_2 x_3 w_1 3 w)))
 (let ((?x994 (storage_t x_0 x_1 x_2 x_3 w_1 4 w)))
 (= ?x994 ?x4752))))
 ))
 (let (($x638 (forall ((n (_ BitVec 6)) )(let ((?x11964 (sc_t 3)))
 (let (($x8602 (bvsle ?x11964 n)))
 (let ((?x11538 (stack_t x_0 x_1 x_2 x_3 w_1 3 n)))
 (let ((?x8880 (stack_t x_0 x_1 x_2 x_3 w_1 4 n)))
 (let (($x5782 (= ?x8880 ?x11538)))
 (or $x5782 $x8602)))))))
 ))
 (let (($x9174 (= ?x10468 (bvadd (_ bv1 6) (sc_t 3)))))
 (let ((?x5766 (used_gas_t x_0 x_1 x_2 x_3 w_1 4)))
 (let (($x2542 (= ?x5766 (+ 3 (used_gas_t x_0 x_1 x_2 x_3 w_1 3)))))
 (let (($x10850 (= $x4112 (or (exc_halt_t 2) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_t 2))))))))
 (let (($x10600 (forall ((w (_ BitVec 256)) )(let ((?x8604 (storage_t x_0 x_1 x_2 x_3 w_1 2 w)))
 (let ((?x4752 (storage_t x_0 x_1 x_2 x_3 w_1 3 w)))
 (= ?x4752 ?x8604))))
 ))
 (let (($x7519 (forall ((n (_ BitVec 6)) )(let ((?x5269 (sc_t 2)))
 (let ((?x9343 (bvadd (_ bv62 6) ?x5269)))
 (let (($x10917 (bvsle ?x9343 n)))
 (let ((?x6929 (stack_t x_0 x_1 x_2 x_3 w_1 2 n)))
 (let ((?x11538 (stack_t x_0 x_1 x_2 x_3 w_1 3 n)))
 (let (($x5762 (= ?x11538 ?x6929)))
 (or $x5762 $x10917))))))))
 ))
 (let ((?x5269 (sc_t 2)))
 (let ((?x11964 (sc_t 3)))
 (let (($x4398 (= ?x11964 ?x5269)))
 (let ((?x4384 (used_gas_t x_0 x_1 x_2 x_3 w_1 3)))
 (let (($x9345 (= ?x4384 (+ 3 (used_gas_t x_0 x_1 x_2 x_3 w_1 2)))))
 (let ((?x4748 (bvadd (_ bv63 6) ?x5269)))
 (let ((?x7685 (stack_t x_0 x_1 x_2 x_3 w_1 2 ?x4748)))
 (let ((?x11398 (bvadd (_ bv62 6) ?x11964)))
 (let ((?x1784 (stack_t x_0 x_1 x_2 x_3 w_1 3 ?x11398)))
 (let ((?x9343 (bvadd (_ bv62 6) ?x5269)))
 (let ((?x10151 (stack_t x_0 x_1 x_2 x_3 w_1 2 ?x9343)))
 (let ((?x1119 (bvadd (_ bv63 6) ?x11964)))
 (let ((?x5667 (stack_t x_0 x_1 x_2 x_3 w_1 3 ?x1119)))
 (let (($x4675 (exc_halt_t 2)))
 (let (($x6434 (= $x4675 (or (exc_halt_t 1) (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_t 1))))))))
 (let (($x266 (forall ((w (_ BitVec 256)) )(let ((?x4733 (storage_t x_0 x_1 x_2 x_3 w_1 1 w)))
 (let ((?x8604 (storage_t x_0 x_1 x_2 x_3 w_1 2 w)))
 (= ?x8604 ?x4733))))
 ))
 (let (($x7110 (forall ((n (_ BitVec 6)) )(let ((?x2841 (stack_t x_0 x_1 x_2 x_3 w_1 1 n)))
 (let ((?x6929 (stack_t x_0 x_1 x_2 x_3 w_1 2 n)))
 (let (($x11833 (= ?x6929 ?x2841)))
 (let ((?x7154 (sc_t 1)))
 (let ((?x1746 (bvadd (_ bv63 6) ?x7154)))
 (let (($x4225 (bvsle ?x1746 n)))
 (or $x4225 $x11833))))))))
 ))
 (let ((?x1331 (used_gas_t x_0 x_1 x_2 x_3 w_1 2)))
 (let (($x8377 (exc_halt_t 1)))
 (let (($x9616 (= $x8377 (or $x56 (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) ?x63)))))))
 (let (($x9442 (forall ((w (_ BitVec 256)) )(let ((?x6684 (storage_t x_0 x_1 x_2 x_3 w_1 0 w)))
 (let ((?x4733 (storage_t x_0 x_1 x_2 x_3 w_1 1 w)))
 (= ?x4733 ?x6684))))
 ))
 (let (($x8735 (forall ((n (_ BitVec 6)) )(let ((?x916 (stack_t x_0 x_1 x_2 x_3 w_1 0 n)))
 (let ((?x2841 (stack_t x_0 x_1 x_2 x_3 w_1 1 n)))
 (let (($x2300 (= ?x2841 ?x916)))
 (let ((?x63 (sc_t 0)))
 (let ((?x10160 (bvadd (_ bv63 6) ?x63)))
 (let (($x785 (bvsle ?x10160 n)))
 (or $x785 $x2300))))))))
 ))
 (let ((?x10160 (bvadd (_ bv63 6) ?x63)))
 (let ((?x7154 (sc_t 1)))
 (let (($x10818 (= ?x7154 ?x10160)))
 (let (($x11414 (= $x772 (or (exc_halt_s 5) (not (bvsle (_ bv0 6) (bvadd (_ bv61 6) (sc_s 5))))))))
 (let (($x2864 (forall ((w (_ BitVec 256)) )(let ((?x1526 (storage_s x_0 x_1 x_2 x_3 w_1 5 w)))
 (let ((?x11387 (storage_s x_0 x_1 x_2 x_3 w_1 6 w)))
 (= ?x11387 ?x1526))))
 ))
 (let (($x3583 (forall ((n (_ BitVec 6)) )(let ((?x10310 (stack_s x_0 x_1 x_2 x_3 w_1 5 n)))
 (let ((?x1272 (stack_s x_0 x_1 x_2 x_3 w_1 6 n)))
 (let ((?x4319 (sc_s 5)))
 (let ((?x3650 (bvadd (_ bv61 6) ?x4319)))
 (let (($x4913 (bvsle ?x3650 n)))
 (or $x4913 (= ?x1272 ?x10310))))))))
 ))
 (let ((?x4319 (sc_s 5)))
 (let (($x7923 (= ?x926 ?x4319)))
 (let (($x4878 (= (used_gas_s x_0 x_1 x_2 x_3 w_1 6) (+ 3 (used_gas_s x_0 x_1 x_2 x_3 w_1 5)))))
 (let ((?x9508 (bvadd (_ bv62 6) ?x4319)))
 (let ((?x3626 (stack_s x_0 x_1 x_2 x_3 w_1 5 ?x9508)))
 (let (($x8097 (= (stack_s x_0 x_1 x_2 x_3 w_1 6 (bvadd (_ bv62 6) ?x926)) ?x3626)))
 (let ((?x6605 (bvadd (_ bv63 6) ?x4319)))
 (let ((?x4794 (stack_s x_0 x_1 x_2 x_3 w_1 5 ?x6605)))
 (let (($x9879 (= (stack_s x_0 x_1 x_2 x_3 w_1 6 (bvadd (_ bv61 6) ?x926)) ?x4794)))
 (let (($x9484 (= (stack_s x_0 x_1 x_2 x_3 w_1 6 (bvadd (_ bv63 6) ?x926)) (stack_s x_0 x_1 x_2 x_3 w_1 5 (bvadd (_ bv61 6) ?x4319)))))
 (let (($x1862 (exc_halt_s 5)))
 (let (($x7458 (= $x1862 (or (exc_halt_s 4) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 4))))))))
 (let (($x9493 (forall ((w (_ BitVec 256)) )(let ((?x9949 (storage_s x_0 x_1 x_2 x_3 w_1 4 w)))
 (let ((?x1526 (storage_s x_0 x_1 x_2 x_3 w_1 5 w)))
 (= ?x1526 ?x9949))))
 ))
 (let (($x125 (forall ((n (_ BitVec 6)) )(let ((?x10619 (stack_s x_0 x_1 x_2 x_3 w_1 4 n)))
 (let ((?x10310 (stack_s x_0 x_1 x_2 x_3 w_1 5 n)))
 (let ((?x4305 (sc_s 4)))
 (let ((?x5150 (bvadd (_ bv62 6) ?x4305)))
 (let (($x6839 (bvsle ?x5150 n)))
 (or $x6839 (= ?x10310 ?x10619))))))))
 ))
 (let ((?x4305 (sc_s 4)))
 (let (($x8598 (= ?x4319 ?x4305)))
 (let ((?x2651 (used_gas_s x_0 x_1 x_2 x_3 w_1 5)))
 (let (($x592 (= ?x2651 (+ 3 (used_gas_s x_0 x_1 x_2 x_3 w_1 4)))))
 (let ((?x10368 (bvadd (_ bv63 6) ?x4305)))
 (let ((?x3175 (stack_s x_0 x_1 x_2 x_3 w_1 4 ?x10368)))
 (let (($x655 (= ?x3626 ?x3175)))
 (let ((?x5150 (bvadd (_ bv62 6) ?x4305)))
 (let ((?x8420 (stack_s x_0 x_1 x_2 x_3 w_1 4 ?x5150)))
 (let (($x3440 (= ?x4794 ?x8420)))
 (let (($x7172 (exc_halt_s 4)))
 (let (($x8088 (= $x7172 (or (exc_halt_s 3) (not (bvsle (_ bv0 6) (bvadd (_ bv61 6) (sc_s 3))))))))
 (let (($x5027 (forall ((w (_ BitVec 256)) )(let ((?x9322 (storage_s x_0 x_1 x_2 x_3 w_1 3 w)))
 (let ((?x9949 (storage_s x_0 x_1 x_2 x_3 w_1 4 w)))
 (= ?x9949 ?x9322))))
 ))
 (let (($x1060 (forall ((n (_ BitVec 6)) )(let ((?x275 (sc_s 3)))
 (let ((?x2689 (bvadd (_ bv61 6) ?x275)))
 (let (($x8701 (bvsle ?x2689 n)))
 (let ((?x754 (stack_s x_0 x_1 x_2 x_3 w_1 3 n)))
 (let ((?x10619 (stack_s x_0 x_1 x_2 x_3 w_1 4 n)))
 (let (($x4841 (= ?x10619 ?x754)))
 (or $x4841 $x8701))))))))
 ))
 (let ((?x275 (sc_s 3)))
 (let (($x3925 (= ?x4305 ?x275)))
 (let ((?x7469 (used_gas_s x_0 x_1 x_2 x_3 w_1 4)))
 (let (($x7521 (= ?x7469 (+ 3 (used_gas_s x_0 x_1 x_2 x_3 w_1 3)))))
 (let (($x7736 (= (stack_s x_0 x_1 x_2 x_3 w_1 4 (bvadd (_ bv61 6) ?x4305)) (stack_s x_0 x_1 x_2 x_3 w_1 3 (bvadd (_ bv63 6) ?x275)))))
 (let (($x2376 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 2)))) (_ bv0 1)))))
 (let (($x247 (exc_halt_s 2)))
 (let (($x292 (exc_halt_s 3)))
 (let (($x3156 (= $x292 (or $x247 $x2376))))
 (let (($x10935 (forall ((w (_ BitVec 256)) )(let ((?x2303 (storage_s x_0 x_1 x_2 x_3 w_1 2 w)))
 (let ((?x9322 (storage_s x_0 x_1 x_2 x_3 w_1 3 w)))
 (= ?x9322 ?x2303))))
 ))
 (let (($x8011 (forall ((n (_ BitVec 6)) )(let ((?x8090 (stack_s x_0 x_1 x_2 x_3 w_1 2 n)))
 (let ((?x754 (stack_s x_0 x_1 x_2 x_3 w_1 3 n)))
 (let (($x4403 (= ?x754 ?x8090)))
 (let ((?x218 (sc_s 2)))
 (let (($x1340 (bvsle ?x218 n)))
 (or $x1340 $x4403)))))))
 ))
 (let (($x7581 (= ?x275 (bvadd (_ bv1 6) (sc_s 2)))))
 (let ((?x1968 (used_gas_s x_0 x_1 x_2 x_3 w_1 3)))
 (let (($x1657 (= $x247 (or (exc_halt_s 1) (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_s 1))))))))
 (let (($x6255 (forall ((w (_ BitVec 256)) )(let ((?x9078 (storage_s x_0 x_1 x_2 x_3 w_1 1 w)))
 (let ((?x2303 (storage_s x_0 x_1 x_2 x_3 w_1 2 w)))
 (= ?x2303 ?x9078))))
 ))
 (let (($x8036 (forall ((n (_ BitVec 6)) )(let ((?x154 (sc_s 1)))
 (let ((?x3077 (bvadd (_ bv63 6) ?x154)))
 (let (($x8034 (bvsle ?x3077 n)))
 (let ((?x3589 (stack_s x_0 x_1 x_2 x_3 w_1 1 n)))
 (let ((?x8090 (stack_s x_0 x_1 x_2 x_3 w_1 2 n)))
 (let (($x2911 (= ?x8090 ?x3589)))
 (or $x2911 $x8034))))))))
 ))
 (let ((?x154 (sc_s 1)))
 (let ((?x3077 (bvadd (_ bv63 6) ?x154)))
 (let ((?x218 (sc_s 2)))
 (let (($x6943 (= ?x218 ?x3077)))
 (let ((?x1069 (used_gas_s x_0 x_1 x_2 x_3 w_1 2)))
 (let (($x1756 (forall ((w (_ BitVec 256)) )(let ((?x9185 (storage_s x_0 x_1 x_2 x_3 w_1 0 w)))
 (let ((?x9078 (storage_s x_0 x_1 x_2 x_3 w_1 1 w)))
 (= ?x9078 ?x9185))))
 ))
 (let (($x8450 (forall ((n (_ BitVec 6)) )(let ((?x11300 (stack_s x_0 x_1 x_2 x_3 w_1 0 n)))
 (let ((?x3589 (stack_s x_0 x_1 x_2 x_3 w_1 1 n)))
 (let (($x3307 (= ?x3589 ?x11300)))
 (let ((?x72 (sc_s 0)))
 (let ((?x10927 (bvadd (_ bv63 6) ?x72)))
 (let (($x2656 (bvsle ?x10927 n)))
 (or $x2656 $x3307))))))))
 ))
 (let ((?x10927 (bvadd (_ bv63 6) ?x72)))
 (let (($x4308 (= ?x154 ?x10927)))
 (let (($x5199 (forall ((w (_ BitVec 256)) )(let ((?x9185 (storage_s x_0 x_1 x_2 x_3 w_1 0 w)))
 (= ?x9185 (_ bv0 256))))
 ))
 (let (($x5331 (= ?x7642 0)))
 (let (($x5230 (not $x57)))
 (let (($x5143 (= (stack_s x_0 x_1 x_2 x_3 w_1 0 (_ bv3 6)) x_3)))
 (let (($x3816 (= (stack_s x_0 x_1 x_2 x_3 w_1 0 (_ bv2 6)) x_2)))
 (let (($x1050 (= (stack_s x_0 x_1 x_2 x_3 w_1 0 (_ bv1 6)) x_1)))
 (let (($x3313 (= (stack_s x_0 x_1 x_2 x_3 w_1 0 (_ bv0 6)) x_0)))
 (let (($x11199 (= ?x72 (_ bv4 6))))
 (and $x11199 $x3313 $x1050 $x3816 $x5143 $x5230 $x5331 $x5199 (= (used_gas_s x_0 x_1 x_2 x_3 w_1 1) (+ 2 ?x7642)) $x4308 $x8450 $x1756 (= (exc_halt_s 1) (or $x57 (not (bvsle (_ bv0 6) ?x10927)))) (= ?x1069 (+ 2 (used_gas_s x_0 x_1 x_2 x_3 w_1 1))) $x6943 $x8036 $x6255 $x1657 (= (stack_s x_0 x_1 x_2 x_3 w_1 3 ?x218) w_1) (= ?x1968 (+ 3 ?x1069)) $x7581 $x8011 $x10935 $x3156 (= ?x3175 (stack_s x_0 x_1 x_2 x_3 w_1 3 (bvadd (_ bv61 6) ?x275))) $x7736 (= ?x8420 (stack_s x_0 x_1 x_2 x_3 w_1 3 (bvadd (_ bv62 6) ?x275))) $x7521 $x3925 $x1060 $x5027 $x8088 $x3440 $x655 $x592 $x8598 $x125 $x9493 $x7458 $x9484 $x9879 $x8097 $x4878 $x7923 $x3583 $x2864 $x11414 (= (used_gas_t x_0 x_1 x_2 x_3 w_1 1) (+ 2 ?x2321)) $x10818 $x8735 $x9442 $x9616 (= ?x1331 (+ 2 (used_gas_t x_0 x_1 x_2 x_3 w_1 1))) (= ?x5269 (bvadd (_ bv63 6) ?x7154)) $x7110 $x266 $x6434 (= ?x5667 ?x10151) (= ?x1784 ?x7685) $x9345 $x4398 $x7519 $x10600 $x10850 (= (stack_t x_0 x_1 x_2 x_3 w_1 4 ?x11964) w_1) $x2542 $x9174 $x638 $x10643 $x6985 $x73 $x4931 $x58 $x11327 $x2434 (not (and $x5134 $x4349 $x4404 $x8932)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
 )
(check-sat)
(get-proof)
