; DUP1 PUSH cw_3 DUP1 SWAP2 POP PUSH cw_3 => PUSH cw_3 PUSH cw_3 DUP2
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
 (exists ((x_0 (_ BitVec 256)) )(let (($x7288 (forall ((w (_ BitVec 256)) )(let ((?x932 (storage_t x_0 w_3 3 w)))
 (let ((?x9518 (storage_s x_0 w_3 6 w)))
 (= ?x9518 ?x932))))
 ))
 (let (($x6783 (exc_halt_t 3)))
 (let (($x772 (exc_halt_s 6)))
 (let (($x3870 (= $x772 $x6783)))
 (let (($x5142 (forall ((n (_ BitVec 6)) )(let ((?x6438 (sc_t 3)))
 (let (($x9479 (bvsle ?x6438 n)))
 (let ((?x10909 (stack_t x_0 w_3 3 n)))
 (let ((?x11321 (stack_s x_0 w_3 6 n)))
 (let (($x5346 (= ?x11321 ?x10909)))
 (or $x5346 $x9479)))))))
 ))
 (let ((?x6438 (sc_t 3)))
 (let ((?x926 (sc_s 6)))
 (let (($x5324 (= ?x926 ?x6438)))
 (let ((?x3632 (used_gas_t x_0 w_3 0)))
 (let ((?x11541 (used_gas_s x_0 w_3 0)))
 (let (($x10519 (= ?x11541 ?x3632)))
 (let (($x7049 (forall ((w (_ BitVec 256)) )(let ((?x6948 (storage_t x_0 w_3 0 w)))
 (let ((?x2791 (storage_s x_0 w_3 0 w)))
 (= ?x2791 ?x6948))))
 ))
 (let (($x56 (exc_halt_t 0)))
 (let (($x57 (exc_halt_s 0)))
 (let (($x58 (= $x57 $x56)))
 (let (($x7656 (forall ((n (_ BitVec 6)) )(let ((?x63 (sc_t 0)))
 (let (($x9737 (bvsle ?x63 n)))
 (let ((?x8213 (stack_t x_0 w_3 0 n)))
 (let ((?x3219 (stack_s x_0 w_3 0 n)))
 (let (($x7997 (= ?x3219 ?x8213)))
 (or $x7997 $x9737)))))))
 ))
 (let ((?x63 (sc_t 0)))
 (let ((?x72 (sc_s 0)))
 (let (($x73 (= ?x72 ?x63)))
 (let (($x8185 (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_t 2))))))
 (let (($x5252 (exc_halt_t 2)))
 (let (($x4830 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_t 2)))) (_ bv0 1)))))
 (let (($x3570 (= $x6783 (or $x4830 $x5252 $x8185))))
 (let (($x1191 (forall ((w (_ BitVec 256)) )(let ((?x6087 (storage_t x_0 w_3 2 w)))
 (let ((?x932 (storage_t x_0 w_3 3 w)))
 (= ?x932 ?x6087))))
 ))
 (let (($x6401 (forall ((n (_ BitVec 6)) )(let ((?x2714 (sc_t 2)))
 (let ((?x1614 (bvadd (_ bv62 6) ?x2714)))
 (let (($x6008 (bvsle ?x1614 n)))
 (or $x6008 (= (stack_t x_0 w_3 3 n) (stack_t x_0 w_3 2 n)))))))
 ))
 (let (($x8875 (= ?x6438 (bvadd (_ bv1 6) (sc_t 2)))))
 (let (($x2940 (= (used_gas_t x_0 w_3 3) (+ 3 (used_gas_t x_0 w_3 2)))))
 (let (($x9155 (= (stack_t x_0 w_3 3 (bvadd (_ bv63 6) (sc_t 2))) (stack_t x_0 w_3 2 (bvadd (_ bv63 6) (sc_t 2))))))
 (let ((?x2714 (sc_t 2)))
 (let ((?x1614 (bvadd (_ bv62 6) ?x2714)))
 (let ((?x5162 (stack_t x_0 w_3 2 ?x1614)))
 (let (($x8039 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_t 1)))) (_ bv0 1)))))
 (let (($x6122 (exc_halt_t 1)))
 (let (($x8048 (= $x5252 (or $x6122 $x8039))))
 (let (($x7864 (forall ((w (_ BitVec 256)) )(let ((?x5384 (storage_t x_0 w_3 1 w)))
 (let ((?x6087 (storage_t x_0 w_3 2 w)))
 (= ?x6087 ?x5384))))
 ))
 (let (($x8947 (forall ((n (_ BitVec 6)) )(let ((?x5151 (sc_t 1)))
 (let (($x5798 (bvsle ?x5151 n)))
 (or (= (stack_t x_0 w_3 2 n) (stack_t x_0 w_3 1 n)) $x5798))))
 ))
 (let (($x4440 (= ?x2714 (bvadd (_ bv1 6) (sc_t 1)))))
 (let ((?x4337 (used_gas_t x_0 w_3 2)))
 (let (($x1753 (= $x6122 (or $x56 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x63))) (_ bv0 1)))))))
 (let (($x5097 (forall ((w (_ BitVec 256)) )(let ((?x6948 (storage_t x_0 w_3 0 w)))
 (let ((?x5384 (storage_t x_0 w_3 1 w)))
 (= ?x5384 ?x6948))))
 ))
 (let (($x10087 (forall ((n (_ BitVec 6)) )(let ((?x63 (sc_t 0)))
 (let (($x9737 (bvsle ?x63 n)))
 (or $x9737 (= (stack_t x_0 w_3 1 n) (stack_t x_0 w_3 0 n))))))
 ))
 (let ((?x5151 (sc_t 1)))
 (let (($x10883 (= ?x5151 (bvadd (_ bv1 6) ?x63))))
 (let (($x4635 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 5)))) (_ bv0 1)))))
 (let (($x11317 (exc_halt_s 5)))
 (let (($x1378 (= $x772 (or $x11317 $x4635))))
 (let (($x3183 (forall ((w (_ BitVec 256)) )(let ((?x9321 (storage_s x_0 w_3 5 w)))
 (let ((?x9518 (storage_s x_0 w_3 6 w)))
 (= ?x9518 ?x9321))))
 ))
 (let (($x4504 (forall ((n (_ BitVec 6)) )(let ((?x4319 (sc_s 5)))
 (let (($x9945 (bvsle ?x4319 n)))
 (or (= (stack_s x_0 w_3 6 n) (stack_s x_0 w_3 5 n)) $x9945))))
 ))
 (let (($x7083 (= ?x926 (bvadd (_ bv1 6) (sc_s 5)))))
 (let (($x1330 (= (used_gas_s x_0 w_3 6) (+ 3 (used_gas_s x_0 w_3 5)))))
 (let (($x6442 (= $x11317 (or (exc_halt_s 4) (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_s 4))))))))
 (let (($x1700 (forall ((w (_ BitVec 256)) )(let ((?x3339 (storage_s x_0 w_3 4 w)))
 (let ((?x9321 (storage_s x_0 w_3 5 w)))
 (= ?x9321 ?x3339))))
 ))
 (let (($x7503 (forall ((n (_ BitVec 6)) )(or (bvsle (bvadd (_ bv63 6) (sc_s 4)) n) (= (stack_s x_0 w_3 5 n) (stack_s x_0 w_3 4 n))))
 ))
 (let ((?x4305 (sc_s 4)))
 (let ((?x4445 (bvadd (_ bv63 6) ?x4305)))
 (let ((?x4319 (sc_s 5)))
 (let (($x6275 (= ?x4319 ?x4445)))
 (let ((?x8107 (used_gas_s x_0 w_3 5)))
 (let (($x7172 (exc_halt_s 4)))
 (let (($x3052 (= $x7172 (or (exc_halt_s 3) (not (bvsle (_ bv0 6) (bvadd (_ bv61 6) (sc_s 3))))))))
 (let (($x1979 (forall ((w (_ BitVec 256)) )(let ((?x4938 (storage_s x_0 w_3 3 w)))
 (let ((?x3339 (storage_s x_0 w_3 4 w)))
 (= ?x3339 ?x4938))))
 ))
 (let (($x3786 (forall ((n (_ BitVec 6)) )(let ((?x275 (sc_s 3)))
 (let ((?x4982 (bvadd (_ bv61 6) ?x275)))
 (let (($x8945 (bvsle ?x4982 n)))
 (or (= (stack_s x_0 w_3 4 n) (stack_s x_0 w_3 3 n)) $x8945)))))
 ))
 (let ((?x275 (sc_s 3)))
 (let (($x8378 (= ?x4305 ?x275)))
 (let ((?x2672 (used_gas_s x_0 w_3 4)))
 (let (($x7428 (= (stack_s x_0 w_3 4 (bvadd (_ bv62 6) ?x4305)) (stack_s x_0 w_3 3 (bvadd (_ bv62 6) ?x275)))))
 (let ((?x8928 (bvadd (_ bv63 6) ?x275)))
 (let ((?x8467 (stack_s x_0 w_3 3 ?x8928)))
 (let (($x9517 (= (stack_s x_0 w_3 4 ?x4445) (stack_s x_0 w_3 3 (bvadd (_ bv61 6) ?x275)))))
 (let (($x10077 (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_s 2))))))
 (let (($x8138 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 2)))) (_ bv0 1)))))
 (let (($x247 (exc_halt_s 2)))
 (let (($x292 (exc_halt_s 3)))
 (let (($x10072 (forall ((w (_ BitVec 256)) )(let ((?x5645 (storage_s x_0 w_3 2 w)))
 (let ((?x4938 (storage_s x_0 w_3 3 w)))
 (= ?x4938 ?x5645))))
 ))
 (let (($x7413 (forall ((n (_ BitVec 6)) )(let ((?x218 (sc_s 2)))
 (let ((?x9157 (bvadd (_ bv63 6) ?x218)))
 (let (($x10288 (bvsle ?x9157 n)))
 (or (= (stack_s x_0 w_3 3 n) (stack_s x_0 w_3 2 n)) $x10288)))))
 ))
 (let (($x1061 (= ?x275 (bvadd (_ bv1 6) (sc_s 2)))))
 (let ((?x11506 (used_gas_s x_0 w_3 3)))
 (let ((?x218 (sc_s 2)))
 (let ((?x9157 (bvadd (_ bv63 6) ?x218)))
 (let ((?x9697 (stack_s x_0 w_3 2 ?x9157)))
 (let (($x5228 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 1)))) (_ bv0 1)))))
 (let (($x189 (exc_halt_s 1)))
 (let (($x10837 (= $x247 (or $x189 $x5228))))
 (let (($x6692 (forall ((w (_ BitVec 256)) )(let ((?x11442 (storage_s x_0 w_3 1 w)))
 (let ((?x5645 (storage_s x_0 w_3 2 w)))
 (= ?x5645 ?x11442))))
 ))
 (let (($x3472 (forall ((n (_ BitVec 6)) )(let ((?x154 (sc_s 1)))
 (let (($x1373 (bvsle ?x154 n)))
 (or $x1373 (= (stack_s x_0 w_3 2 n) (stack_s x_0 w_3 1 n))))))
 ))
 (let (($x9893 (= ?x218 (bvadd (_ bv1 6) (sc_s 1)))))
 (let ((?x7404 (used_gas_s x_0 w_3 2)))
 (let (($x6640 (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) ?x72)))))
 (let (($x4977 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x72))) (_ bv0 1)))))
 (let (($x2463 (= $x189 (or $x57 $x4977 $x6640))))
 (let (($x3532 (forall ((w (_ BitVec 256)) )(let ((?x2791 (storage_s x_0 w_3 0 w)))
 (let ((?x11442 (storage_s x_0 w_3 1 w)))
 (= ?x11442 ?x2791))))
 ))
 (let (($x8733 (forall ((n (_ BitVec 6)) )(let ((?x72 (sc_s 0)))
 (let ((?x10558 (bvadd (_ bv63 6) ?x72)))
 (let (($x10181 (bvsle ?x10558 n)))
 (or (= (stack_s x_0 w_3 1 n) (stack_s x_0 w_3 0 n)) $x10181)))))
 ))
 (let ((?x154 (sc_s 1)))
 (let (($x3074 (= ?x154 (bvadd (_ bv1 6) ?x72))))
 (let ((?x10558 (bvadd (_ bv63 6) ?x72)))
 (let ((?x1175 (stack_s x_0 w_3 0 ?x10558)))
 (let (($x7579 (forall ((w (_ BitVec 256)) )(let ((?x2791 (storage_s x_0 w_3 0 w)))
 (= ?x2791 (_ bv0 256))))
 ))
 (let (($x1961 (= ?x11541 0)))
 (let (($x3082 (not $x57)))
 (let (($x6103 (= (stack_s x_0 w_3 0 (_ bv0 6)) x_0)))
 (let (($x8685 (= ?x72 (_ bv1 6))))
 (and $x8685 $x6103 $x3082 $x1961 $x7579 (= (stack_s x_0 w_3 1 (bvadd (_ bv63 6) ?x154)) ?x1175) (= (stack_s x_0 w_3 1 ?x10558) ?x1175) (= (used_gas_s x_0 w_3 1) (+ 3 ?x11541)) $x3074 $x8733 $x3532 $x2463 (= (stack_s x_0 w_3 2 ?x154) w_3) (= ?x7404 (+ 3 (used_gas_s x_0 w_3 1))) $x9893 $x3472 $x6692 $x10837 (= ?x8467 ?x9697) (= (stack_s x_0 w_3 3 ?x9157) ?x9697) (= ?x11506 (+ 3 ?x7404)) $x1061 $x7413 $x10072 (= $x292 (or $x247 $x8138 $x10077)) $x9517 (= (stack_s x_0 w_3 4 (bvadd (_ bv61 6) ?x4305)) ?x8467) $x7428 (= ?x2672 (+ 3 ?x11506)) $x8378 $x3786 $x1979 $x3052 (= ?x8107 (+ 2 ?x2672)) $x6275 $x7503 $x1700 $x6442 (= (stack_s x_0 w_3 6 ?x4319) w_3) $x1330 $x7083 $x4504 $x3183 $x1378 (= (stack_t x_0 w_3 1 ?x63) w_3) (= (used_gas_t x_0 w_3 1) (+ 3 ?x3632)) $x10883 $x10087 $x5097 $x1753 (= (stack_t x_0 w_3 2 ?x5151) w_3) (= ?x4337 (+ 3 (used_gas_t x_0 w_3 1))) $x4440 $x8947 $x7864 $x8048 (= (stack_t x_0 w_3 3 (bvadd (_ bv63 6) ?x6438)) ?x5162) (= (stack_t x_0 w_3 3 ?x1614) ?x5162) $x9155 $x2940 $x8875 $x6401 $x1191 $x3570 $x73 $x7656 $x58 $x7049 $x10519 (not (and $x5324 $x5142 $x3870 $x7288)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
 )
(check-sat)
(get-proof)
