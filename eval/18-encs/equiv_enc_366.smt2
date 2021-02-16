; SWAP1 SWAP3 SWAP2 SWAP1 POP POP => POP SWAP1 POP SWAP1
(set-option :produce-proofs true)
; 
(set-info :status unknown)
(declare-fun storage_t ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 256)) (_ BitVec 256))
(declare-fun storage_s ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 256)) (_ BitVec 256))
(declare-fun exc_halt_t (Int) Bool)
(declare-fun exc_halt_s (Int) Bool)
(declare-fun stack_t ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 6)) (_ BitVec 256))
(declare-fun stack_s ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 6)) (_ BitVec 256))
(declare-fun sc_t (Int) (_ BitVec 6))
(declare-fun sc_s (Int) (_ BitVec 6))
(declare-fun used_gas_t ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int) Int)
(declare-fun used_gas_s ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int) Int)
(assert
 (exists ((x_0 (_ BitVec 256)) (x_1 (_ BitVec 256)) (x_2 (_ BitVec 256)) (x_3 (_ BitVec 256)) )(let (($x3303 (forall ((w (_ BitVec 256)) )(let ((?x5303 (storage_t x_0 x_1 x_2 x_3 4 w)))
 (let ((?x3374 (storage_s x_0 x_1 x_2 x_3 6 w)))
 (= ?x3374 ?x5303))))
 ))
 (let (($x3723 (exc_halt_t 4)))
 (let (($x772 (exc_halt_s 6)))
 (let (($x10342 (= $x772 $x3723)))
 (let (($x4140 (forall ((n (_ BitVec 6)) )(let ((?x8260 (stack_t x_0 x_1 x_2 x_3 4 n)))
 (let ((?x4800 (stack_s x_0 x_1 x_2 x_3 6 n)))
 (let (($x3397 (= ?x4800 ?x8260)))
 (let ((?x1098 (sc_t 4)))
 (let (($x1484 (bvsle ?x1098 n)))
 (or $x1484 $x3397)))))))
 ))
 (let ((?x1098 (sc_t 4)))
 (let ((?x926 (sc_s 6)))
 (let (($x7019 (= ?x926 ?x1098)))
 (let ((?x10988 (used_gas_t x_0 x_1 x_2 x_3 0)))
 (let ((?x6387 (used_gas_s x_0 x_1 x_2 x_3 0)))
 (let (($x2784 (= ?x6387 ?x10988)))
 (let (($x10844 (forall ((w (_ BitVec 256)) )(let ((?x8413 (storage_t x_0 x_1 x_2 x_3 0 w)))
 (let ((?x6539 (storage_s x_0 x_1 x_2 x_3 0 w)))
 (= ?x6539 ?x8413))))
 ))
 (let (($x56 (exc_halt_t 0)))
 (let (($x57 (exc_halt_s 0)))
 (let (($x58 (= $x57 $x56)))
 (let (($x5183 (forall ((n (_ BitVec 6)) )(let ((?x2456 (stack_t x_0 x_1 x_2 x_3 0 n)))
 (let ((?x5579 (stack_s x_0 x_1 x_2 x_3 0 n)))
 (let (($x6479 (= ?x5579 ?x2456)))
 (let ((?x63 (sc_t 0)))
 (let (($x4866 (bvsle ?x63 n)))
 (or $x4866 $x6479)))))))
 ))
 (let ((?x63 (sc_t 0)))
 (let ((?x72 (sc_s 0)))
 (let (($x73 (= ?x72 ?x63)))
 (let (($x342 (= $x3723 (or (exc_halt_t 3) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_t 3))))))))
 (let (($x2812 (forall ((w (_ BitVec 256)) )(let ((?x9527 (storage_t x_0 x_1 x_2 x_3 3 w)))
 (let ((?x5303 (storage_t x_0 x_1 x_2 x_3 4 w)))
 (= ?x5303 ?x9527))))
 ))
 (let (($x2835 (forall ((n (_ BitVec 6)) )(let ((?x10866 (stack_t x_0 x_1 x_2 x_3 3 n)))
 (let ((?x8260 (stack_t x_0 x_1 x_2 x_3 4 n)))
 (let ((?x6438 (sc_t 3)))
 (let ((?x3065 (bvadd (_ bv62 6) ?x6438)))
 (let (($x755 (bvsle ?x3065 n)))
 (or $x755 (= ?x8260 ?x10866))))))))
 ))
 (let (($x8925 (= (used_gas_t x_0 x_1 x_2 x_3 4) (+ 3 (used_gas_t x_0 x_1 x_2 x_3 3)))))
 (let ((?x6438 (sc_t 3)))
 (let ((?x10141 (bvadd (_ bv63 6) ?x6438)))
 (let ((?x1873 (stack_t x_0 x_1 x_2 x_3 3 ?x10141)))
 (let (($x8499 (= (stack_t x_0 x_1 x_2 x_3 4 (bvadd (_ bv63 6) ?x1098)) (stack_t x_0 x_1 x_2 x_3 3 (bvadd (_ bv62 6) ?x6438)))))
 (let (($x6783 (exc_halt_t 3)))
 (let (($x5088 (= $x6783 (or (exc_halt_t 2) (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_t 2))))))))
 (let (($x2416 (forall ((w (_ BitVec 256)) )(let ((?x10711 (storage_t x_0 x_1 x_2 x_3 2 w)))
 (let ((?x9527 (storage_t x_0 x_1 x_2 x_3 3 w)))
 (= ?x9527 ?x10711))))
 ))
 (let (($x6059 (forall ((n (_ BitVec 6)) )(let ((?x10175 (stack_t x_0 x_1 x_2 x_3 2 n)))
 (let ((?x10866 (stack_t x_0 x_1 x_2 x_3 3 n)))
 (let (($x11526 (= ?x10866 ?x10175)))
 (or (bvsle (bvadd (_ bv63 6) (sc_t 2)) n) $x11526)))))
 ))
 (let ((?x2714 (sc_t 2)))
 (let ((?x9149 (bvadd (_ bv63 6) ?x2714)))
 (let (($x495 (= ?x6438 ?x9149)))
 (let ((?x10704 (used_gas_t x_0 x_1 x_2 x_3 3)))
 (let (($x5252 (exc_halt_t 2)))
 (let (($x5567 (= $x5252 (or (exc_halt_t 1) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_t 1))))))))
 (let (($x6811 (forall ((w (_ BitVec 256)) )(let ((?x1950 (storage_t x_0 x_1 x_2 x_3 1 w)))
 (let ((?x10711 (storage_t x_0 x_1 x_2 x_3 2 w)))
 (= ?x10711 ?x1950))))
 ))
 (let (($x10248 (forall ((n (_ BitVec 6)) )(let ((?x4934 (stack_t x_0 x_1 x_2 x_3 1 n)))
 (let ((?x10175 (stack_t x_0 x_1 x_2 x_3 2 n)))
 (let (($x7179 (= ?x10175 ?x4934)))
 (or (bvsle (bvadd (_ bv62 6) (sc_t 1)) n) $x7179)))))
 ))
 (let ((?x8347 (sc_t 1)))
 (let (($x11805 (= ?x2714 ?x8347)))
 (let ((?x418 (used_gas_t x_0 x_1 x_2 x_3 2)))
 (let (($x5004 (= ?x418 (+ 3 (used_gas_t x_0 x_1 x_2 x_3 1)))))
 (let ((?x4288 (bvadd (_ bv63 6) ?x8347)))
 (let ((?x3981 (stack_t x_0 x_1 x_2 x_3 1 ?x4288)))
 (let ((?x1024 (bvadd (_ bv62 6) ?x2714)))
 (let ((?x3234 (stack_t x_0 x_1 x_2 x_3 2 ?x1024)))
 (let ((?x10292 (bvadd (_ bv62 6) ?x8347)))
 (let ((?x8566 (stack_t x_0 x_1 x_2 x_3 1 ?x10292)))
 (let ((?x4539 (stack_t x_0 x_1 x_2 x_3 2 ?x9149)))
 (let (($x10305 (forall ((w (_ BitVec 256)) )(let ((?x8413 (storage_t x_0 x_1 x_2 x_3 0 w)))
 (let ((?x1950 (storage_t x_0 x_1 x_2 x_3 1 w)))
 (= ?x1950 ?x8413))))
 ))
 (let (($x5701 (forall ((n (_ BitVec 6)) )(let ((?x2456 (stack_t x_0 x_1 x_2 x_3 0 n)))
 (let ((?x4934 (stack_t x_0 x_1 x_2 x_3 1 n)))
 (let (($x6020 (= ?x4934 ?x2456)))
 (or (bvsle (bvadd (_ bv63 6) (sc_t 0)) n) $x6020)))))
 ))
 (let ((?x2902 (bvadd (_ bv63 6) ?x63)))
 (let (($x6944 (= ?x8347 ?x2902)))
 (let (($x6089 (= $x772 (or (exc_halt_s 5) (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_s 5))))))))
 (let (($x5092 (forall ((w (_ BitVec 256)) )(let ((?x5463 (storage_s x_0 x_1 x_2 x_3 5 w)))
 (let ((?x3374 (storage_s x_0 x_1 x_2 x_3 6 w)))
 (= ?x3374 ?x5463))))
 ))
 (let (($x7512 (forall ((n (_ BitVec 6)) )(let ((?x8211 (stack_s x_0 x_1 x_2 x_3 5 n)))
 (let ((?x4800 (stack_s x_0 x_1 x_2 x_3 6 n)))
 (let ((?x4319 (sc_s 5)))
 (let ((?x3898 (bvadd (_ bv63 6) ?x4319)))
 (let (($x2856 (bvsle ?x3898 n)))
 (or $x2856 (= ?x4800 ?x8211))))))))
 ))
 (let (($x8459 (= (used_gas_s x_0 x_1 x_2 x_3 6) (+ 2 (used_gas_s x_0 x_1 x_2 x_3 5)))))
 (let (($x11317 (exc_halt_s 5)))
 (let (($x3093 (= $x11317 (or (exc_halt_s 4) (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_s 4))))))))
 (let (($x3939 (forall ((w (_ BitVec 256)) )(let ((?x6619 (storage_s x_0 x_1 x_2 x_3 4 w)))
 (let ((?x5463 (storage_s x_0 x_1 x_2 x_3 5 w)))
 (= ?x5463 ?x6619))))
 ))
 (let (($x3059 (forall ((n (_ BitVec 6)) )(let ((?x4404 (stack_s x_0 x_1 x_2 x_3 4 n)))
 (let ((?x8211 (stack_s x_0 x_1 x_2 x_3 5 n)))
 (let ((?x4305 (sc_s 4)))
 (let ((?x9783 (bvadd (_ bv63 6) ?x4305)))
 (let (($x4162 (bvsle ?x9783 n)))
 (or $x4162 (= ?x8211 ?x4404))))))))
 ))
 (let ((?x4305 (sc_s 4)))
 (let ((?x9783 (bvadd (_ bv63 6) ?x4305)))
 (let ((?x4319 (sc_s 5)))
 (let (($x10649 (= ?x4319 ?x9783)))
 (let ((?x10151 (used_gas_s x_0 x_1 x_2 x_3 5)))
 (let (($x7172 (exc_halt_s 4)))
 (let (($x8085 (= $x7172 (or (exc_halt_s 3) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 3))))))))
 (let (($x4877 (forall ((w (_ BitVec 256)) )(let ((?x10217 (storage_s x_0 x_1 x_2 x_3 3 w)))
 (let ((?x6619 (storage_s x_0 x_1 x_2 x_3 4 w)))
 (= ?x6619 ?x10217))))
 ))
 (let (($x8828 (forall ((n (_ BitVec 6)) )(let ((?x275 (sc_s 3)))
 (let ((?x4190 (bvadd (_ bv62 6) ?x275)))
 (let (($x4079 (bvsle ?x4190 n)))
 (let ((?x3542 (stack_s x_0 x_1 x_2 x_3 3 n)))
 (let ((?x4404 (stack_s x_0 x_1 x_2 x_3 4 n)))
 (or (= ?x4404 ?x3542) $x4079)))))))
 ))
 (let ((?x275 (sc_s 3)))
 (let (($x9884 (= ?x4305 ?x275)))
 (let ((?x6954 (used_gas_s x_0 x_1 x_2 x_3 4)))
 (let (($x11467 (= ?x6954 (+ 3 (used_gas_s x_0 x_1 x_2 x_3 3)))))
 (let ((?x314 (bvadd (_ bv63 6) ?x275)))
 (let ((?x8306 (stack_s x_0 x_1 x_2 x_3 3 ?x314)))
 (let ((?x4190 (bvadd (_ bv62 6) ?x275)))
 (let ((?x940 (stack_s x_0 x_1 x_2 x_3 3 ?x4190)))
 (let ((?x11322 (stack_s x_0 x_1 x_2 x_3 4 ?x9783)))
 (let (($x292 (exc_halt_s 3)))
 (let (($x743 (= $x292 (or (exc_halt_s 2) (not (bvsle (_ bv0 6) (bvadd (_ bv61 6) (sc_s 2))))))))
 (let (($x1121 (forall ((w (_ BitVec 256)) )(let ((?x1465 (storage_s x_0 x_1 x_2 x_3 2 w)))
 (let ((?x10217 (storage_s x_0 x_1 x_2 x_3 3 w)))
 (= ?x10217 ?x1465))))
 ))
 (let (($x7102 (forall ((n (_ BitVec 6)) )(let ((?x2688 (stack_s x_0 x_1 x_2 x_3 2 n)))
 (let ((?x3542 (stack_s x_0 x_1 x_2 x_3 3 n)))
 (let (($x3650 (= ?x3542 ?x2688)))
 (or $x3650 (bvsle (bvadd (_ bv61 6) (sc_s 2)) n))))))
 ))
 (let ((?x218 (sc_s 2)))
 (let (($x4933 (= ?x275 ?x218)))
 (let ((?x11784 (used_gas_s x_0 x_1 x_2 x_3 3)))
 (let (($x11553 (= ?x11784 (+ 3 (used_gas_s x_0 x_1 x_2 x_3 2)))))
 (let ((?x3067 (bvadd (_ bv62 6) ?x218)))
 (let ((?x6011 (stack_s x_0 x_1 x_2 x_3 2 ?x3067)))
 (let (($x9464 (= ?x940 ?x6011)))
 (let ((?x5789 (bvadd (_ bv63 6) ?x218)))
 (let ((?x250 (stack_s x_0 x_1 x_2 x_3 2 ?x5789)))
 (let ((?x6436 (bvadd (_ bv61 6) ?x275)))
 (let ((?x1563 (stack_s x_0 x_1 x_2 x_3 3 ?x6436)))
 (let (($x247 (exc_halt_s 2)))
 (let (($x794 (= $x247 (or (exc_halt_s 1) (not (bvsle (_ bv0 6) (bvadd (_ bv60 6) (sc_s 1))))))))
 (let (($x4022 (forall ((w (_ BitVec 256)) )(let ((?x9566 (storage_s x_0 x_1 x_2 x_3 1 w)))
 (let ((?x1465 (storage_s x_0 x_1 x_2 x_3 2 w)))
 (= ?x1465 ?x9566))))
 ))
 (let (($x3605 (forall ((n (_ BitVec 6)) )(let ((?x1595 (stack_s x_0 x_1 x_2 x_3 1 n)))
 (let ((?x2688 (stack_s x_0 x_1 x_2 x_3 2 n)))
 (let (($x4357 (= ?x2688 ?x1595)))
 (or $x4357 (bvsle (bvadd (_ bv60 6) (sc_s 1)) n))))))
 ))
 (let ((?x154 (sc_s 1)))
 (let (($x2182 (= ?x218 ?x154)))
 (let ((?x437 (used_gas_s x_0 x_1 x_2 x_3 2)))
 (let (($x3204 (= ?x437 (+ 3 (used_gas_s x_0 x_1 x_2 x_3 1)))))
 (let ((?x5690 (bvadd (_ bv61 6) ?x154)))
 (let ((?x4859 (stack_s x_0 x_1 x_2 x_3 1 ?x5690)))
 (let ((?x10194 (bvadd (_ bv61 6) ?x218)))
 (let ((?x10185 (stack_s x_0 x_1 x_2 x_3 2 ?x10194)))
 (let ((?x9355 (bvadd (_ bv63 6) ?x154)))
 (let ((?x224 (stack_s x_0 x_1 x_2 x_3 1 ?x9355)))
 (let ((?x6883 (bvadd (_ bv60 6) ?x218)))
 (let ((?x9506 (stack_s x_0 x_1 x_2 x_3 2 ?x6883)))
 (let (($x3763 (forall ((w (_ BitVec 256)) )(let ((?x6539 (storage_s x_0 x_1 x_2 x_3 0 w)))
 (let ((?x9566 (storage_s x_0 x_1 x_2 x_3 1 w)))
 (= ?x9566 ?x6539))))
 ))
 (let (($x10727 (forall ((n (_ BitVec 6)) )(let ((?x5579 (stack_s x_0 x_1 x_2 x_3 0 n)))
 (let ((?x1595 (stack_s x_0 x_1 x_2 x_3 1 n)))
 (let (($x10930 (= ?x1595 ?x5579)))
 (or (bvsle (bvadd (_ bv62 6) (sc_s 0)) n) $x10930)))))
 ))
 (let (($x10914 (= ?x154 ?x72)))
 (let ((?x11792 (used_gas_s x_0 x_1 x_2 x_3 1)))
 (let (($x7925 (= ?x11792 (+ 3 ?x6387))))
 (let ((?x1990 (bvadd (_ bv63 6) ?x72)))
 (let ((?x10816 (stack_s x_0 x_1 x_2 x_3 0 ?x1990)))
 (let ((?x9948 (bvadd (_ bv62 6) ?x154)))
 (let ((?x6371 (stack_s x_0 x_1 x_2 x_3 1 ?x9948)))
 (let (($x209 (forall ((w (_ BitVec 256)) )(let ((?x6539 (storage_s x_0 x_1 x_2 x_3 0 w)))
 (= ?x6539 (_ bv0 256))))
 ))
 (let (($x4136 (= ?x6387 0)))
 (let (($x1248 (not $x57)))
 (let (($x6120 (= (stack_s x_0 x_1 x_2 x_3 0 (_ bv3 6)) x_3)))
 (let (($x1519 (= (stack_s x_0 x_1 x_2 x_3 0 (_ bv2 6)) x_2)))
 (let (($x4087 (= (stack_s x_0 x_1 x_2 x_3 0 (_ bv1 6)) x_1)))
 (let (($x786 (= (stack_s x_0 x_1 x_2 x_3 0 (_ bv0 6)) x_0)))
 (let (($x289 (= ?x72 (_ bv4 6))))
 (and $x289 $x786 $x4087 $x1519 $x6120 $x1248 $x4136 $x209 (= ?x224 (stack_s x_0 x_1 x_2 x_3 0 (bvadd (_ bv62 6) ?x72))) (= ?x6371 ?x10816) $x7925 $x10914 $x10727 $x3763 (= (exc_halt_s 1) (or $x57 (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) ?x72))))) (= ?x250 (stack_s x_0 x_1 x_2 x_3 1 (bvadd (_ bv60 6) ?x154))) (= ?x9506 ?x224) (= ?x10185 ?x4859) (= ?x6011 ?x6371) $x3204 $x2182 $x3605 $x4022 $x794 (= ?x8306 ?x10185) (= ?x1563 ?x250) $x9464 $x11553 $x4933 $x7102 $x1121 $x743 (= ?x11322 ?x940) (= (stack_s x_0 x_1 x_2 x_3 4 (bvadd (_ bv62 6) ?x4305)) ?x8306) $x11467 $x9884 $x8828 $x4877 $x8085 (= ?x10151 (+ 2 ?x6954)) $x10649 $x3059 $x3939 $x3093 $x8459 (= ?x926 (bvadd (_ bv63 6) ?x4319)) $x7512 $x5092 $x6089 (= (used_gas_t x_0 x_1 x_2 x_3 1) (+ 2 ?x10988)) $x6944 $x5701 $x10305 (= (exc_halt_t 1) (or $x56 (not (bvsle (_ bv0 6) ?x2902)))) (= ?x4539 ?x8566) (= ?x3234 ?x3981) $x5004 $x11805 $x10248 $x6811 $x5567 (= ?x10704 (+ 2 ?x418)) $x495 $x6059 $x2416 $x5088 $x8499 (= (stack_t x_0 x_1 x_2 x_3 4 (bvadd (_ bv62 6) ?x1098)) ?x1873) $x8925 (= ?x1098 ?x6438) $x2835 $x2812 $x342 $x73 $x5183 $x58 $x10844 $x2784 (not (and $x7019 $x4140 $x10342 $x3303))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
 )
(check-sat)
(get-proof)