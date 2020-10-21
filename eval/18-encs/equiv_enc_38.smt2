; DUP1 SWAP2 PUSH 0x00 SWAP1 DUP2 LT => PUSH 0x00 DUP2 SWAP3 ISZERO ISZERO
(set-option :produce-proofs true)
; 
(set-info :status unknown)
(declare-fun storage_t ((_ BitVec 256) (_ BitVec 256) Int (_ BitVec 256)) (_ BitVec 256))
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
 (exists ((x_0 (_ BitVec 256)) (x_1 (_ BitVec 256)) )(let (($x2025 (forall ((w (_ BitVec 256)) )(let ((?x2182 (storage_t x_0 x_1 5 w)))
 (let ((?x2906 (storage_s x_0 x_1 6 w)))
 (= ?x2906 ?x2182))))
 ))
 (let (($x886 (exc_halt_t 5)))
 (let (($x772 (exc_halt_s 6)))
 (let (($x889 (= $x772 $x886)))
 (let (($x1776 (forall ((n (_ BitVec 6)) )(let ((?x919 (sc_t 5)))
 (let (($x3541 (bvsle ?x919 n)))
 (let ((?x2896 (stack_t x_0 x_1 5 n)))
 (let ((?x2890 (stack_s x_0 x_1 6 n)))
 (let (($x2892 (= ?x2890 ?x2896)))
 (or $x2892 $x3541)))))))
 ))
 (let ((?x919 (sc_t 5)))
 (let ((?x926 (sc_s 6)))
 (let (($x929 (= ?x926 ?x919)))
 (let ((?x4902 (used_gas_t x_0 x_1 0)))
 (let ((?x4896 (used_gas_s x_0 x_1 0)))
 (let (($x4897 (= ?x4896 ?x4902)))
 (let (($x2181 (forall ((w (_ BitVec 256)) )(let ((?x2886 (storage_t x_0 x_1 0 w)))
 (let ((?x2893 (storage_s x_0 x_1 0 w)))
 (= ?x2893 ?x2886))))
 ))
 (let (($x56 (exc_halt_t 0)))
 (let (($x57 (exc_halt_s 0)))
 (let (($x58 (= $x57 $x56)))
 (let (($x8784 (forall ((n (_ BitVec 6)) )(let ((?x63 (sc_t 0)))
 (let (($x3675 (bvsle ?x63 n)))
 (let ((?x2841 (stack_t x_0 x_1 0 n)))
 (let ((?x2840 (stack_s x_0 x_1 0 n)))
 (let (($x2843 (= ?x2840 ?x2841)))
 (or $x2843 $x3675)))))))
 ))
 (let ((?x63 (sc_t 0)))
 (let ((?x72 (sc_s 0)))
 (let (($x73 (= ?x72 ?x63)))
 (let (($x1397 (= $x886 (or (exc_halt_t 4) (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_t 4))))))))
 (let (($x1597 (forall ((w (_ BitVec 256)) )(let ((?x2879 (storage_t x_0 x_1 4 w)))
 (let ((?x2182 (storage_t x_0 x_1 5 w)))
 (= ?x2182 ?x2879))))
 ))
 (let (($x1407 (forall ((n (_ BitVec 6)) )(or (bvsle (bvadd (_ bv63 6) (sc_t 4)) n) (= (stack_t x_0 x_1 5 n) (stack_t x_0 x_1 4 n))))
 ))
 (let (($x1409 (= (used_gas_t x_0 x_1 5) (+ 3 (used_gas_t x_0 x_1 4)))))
 (let ((?x1612 (ite (= (stack_t x_0 x_1 4 (bvadd (_ bv63 6) (sc_t 4))) (_ bv0 256)) (_ bv1 256) (_ bv0 256))))
 (let (($x8747 (exc_halt_t 4)))
 (let (($x3274 (= $x8747 (or (exc_halt_t 3) (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) (sc_t 3))))))))
 (let (($x5833 (forall ((w (_ BitVec 256)) )(let ((?x3067 (storage_t x_0 x_1 3 w)))
 (let ((?x2879 (storage_t x_0 x_1 4 w)))
 (= ?x2879 ?x3067))))
 ))
 (let (($x5953 (forall ((n (_ BitVec 6)) )(let ((?x2012 (sc_t 3)))
 (let ((?x3673 (bvadd (_ bv63 6) ?x2012)))
 (let (($x3245 (bvsle ?x3673 n)))
 (or (= (stack_t x_0 x_1 4 n) (stack_t x_0 x_1 3 n)) $x3245)))))
 ))
 (let ((?x2012 (sc_t 3)))
 (let ((?x9469 (sc_t 4)))
 (let (($x3349 (= ?x9469 ?x2012)))
 (let ((?x2845 (used_gas_t x_0 x_1 4)))
 (let ((?x3637 (bvadd (_ bv63 6) ?x9469)))
 (let ((?x1622 (stack_t x_0 x_1 4 ?x3637)))
 (let (($x1960 (= ?x1622 (ite (= (stack_t x_0 x_1 3 (bvadd (_ bv63 6) ?x2012)) (_ bv0 256)) (_ bv1 256) (_ bv0 256)))))
 (let (($x8030 (exc_halt_t 3)))
 (let (($x1640 (= $x8030 (or (exc_halt_t 2) (not (bvsle (_ bv0 6) (bvadd (_ bv60 6) (sc_t 2))))))))
 (let (($x1638 (forall ((w (_ BitVec 256)) )(let ((?x1564 (storage_t x_0 x_1 2 w)))
 (let ((?x3067 (storage_t x_0 x_1 3 w)))
 (= ?x3067 ?x1564))))
 ))
 (let (($x1915 (forall ((n (_ BitVec 6)) )(let ((?x4056 (sc_t 2)))
 (let ((?x2153 (bvadd (_ bv60 6) ?x4056)))
 (let (($x6890 (bvsle ?x2153 n)))
 (or (= (stack_t x_0 x_1 3 n) (stack_t x_0 x_1 2 n)) $x6890)))))
 ))
 (let ((?x4056 (sc_t 2)))
 (let (($x3319 (= ?x2012 ?x4056)))
 (let ((?x1561 (used_gas_t x_0 x_1 3)))
 (let (($x1924 (= (stack_t x_0 x_1 3 (bvadd (_ bv62 6) ?x2012)) (stack_t x_0 x_1 2 (bvadd (_ bv62 6) ?x4056)))))
 (let (($x1662 (= (stack_t x_0 x_1 3 (bvadd (_ bv61 6) ?x2012)) (stack_t x_0 x_1 2 (bvadd (_ bv61 6) ?x4056)))))
 (let ((?x7929 (bvadd (_ bv63 6) ?x4056)))
 (let ((?x2105 (stack_t x_0 x_1 2 ?x7929)))
 (let ((?x3673 (bvadd (_ bv63 6) ?x2012)))
 (let ((?x1328 (stack_t x_0 x_1 3 ?x3673)))
 (let (($x3572 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_t 1)))) (_ bv0 1)))))
 (let (($x1920 (exc_halt_t 1)))
 (let (($x903 (exc_halt_t 2)))
 (let (($x1791 (forall ((w (_ BitVec 256)) )(let ((?x997 (storage_t x_0 x_1 1 w)))
 (let ((?x1564 (storage_t x_0 x_1 2 w)))
 (= ?x1564 ?x997))))
 ))
 (let (($x990 (forall ((n (_ BitVec 6)) )(or (bvsle (bvadd (_ bv62 6) (sc_t 1)) n) (= (stack_t x_0 x_1 2 n) (stack_t x_0 x_1 1 n))))
 ))
 (let (($x462 (= ?x4056 (bvadd (_ bv1 6) (sc_t 1)))))
 (let ((?x1554 (used_gas_t x_0 x_1 2)))
 (let (($x1063 (= (stack_t x_0 x_1 2 (bvadd (_ bv63 6) (sc_t 1))) (stack_t x_0 x_1 1 (bvadd (_ bv63 6) (sc_t 1))))))
 (let ((?x4023 (sc_t 1)))
 (let ((?x4102 (bvadd (_ bv62 6) ?x4023)))
 (let ((?x1155 (stack_t x_0 x_1 1 ?x4102)))
 (let (($x420 (= $x1920 (or $x56 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x63))) (_ bv0 1)))))))
 (let (($x1160 (forall ((w (_ BitVec 256)) )(let ((?x2886 (storage_t x_0 x_1 0 w)))
 (let ((?x997 (storage_t x_0 x_1 1 w)))
 (= ?x997 ?x2886))))
 ))
 (let (($x1168 (forall ((n (_ BitVec 6)) )(let ((?x63 (sc_t 0)))
 (let (($x3675 (bvsle ?x63 n)))
 (or (= (stack_t x_0 x_1 1 n) (stack_t x_0 x_1 0 n)) $x3675))))
 ))
 (let (($x3747 (= ?x4023 (bvadd (_ bv1 6) ?x63))))
 (let (($x1258 (= $x772 (or (exc_halt_s 5) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 5))))))))
 (let (($x1334 (forall ((w (_ BitVec 256)) )(let ((?x1157 (storage_s x_0 x_1 5 w)))
 (let ((?x2906 (storage_s x_0 x_1 6 w)))
 (= ?x2906 ?x1157))))
 ))
 (let (($x1331 (forall ((n (_ BitVec 6)) )(or (= (stack_s x_0 x_1 6 n) (stack_s x_0 x_1 5 n)) (bvsle (bvadd (_ bv62 6) (sc_s 5)) n)))
 ))
 (let (($x1404 (= (used_gas_s x_0 x_1 6) (+ 3 (used_gas_s x_0 x_1 5)))))
 (let ((?x805 (sc_s 5)))
 (let ((?x930 (bvadd (_ bv63 6) ?x805)))
 (let ((?x1961 (stack_s x_0 x_1 5 ?x930)))
 (let ((?x2043 (ite (bvule (stack_s x_0 x_1 5 (bvadd (_ bv62 6) ?x805)) ?x1961) (_ bv0 256) (_ bv1 256))))
 (let (($x7478 (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 4))))))
 (let (($x64 (exc_halt_s 4)))
 (let (($x795 (or $x64 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 4)))) (_ bv0 1))) $x7478)))
 (let (($x3979 (exc_halt_s 5)))
 (let (($x785 (forall ((w (_ BitVec 256)) )(let ((?x2075 (storage_s x_0 x_1 4 w)))
 (let ((?x1157 (storage_s x_0 x_1 5 w)))
 (= ?x1157 ?x2075))))
 ))
 (let (($x957 (forall ((n (_ BitVec 6)) )(let ((?x4305 (sc_s 4)))
 (let ((?x3773 (bvadd (_ bv62 6) ?x4305)))
 (let (($x7341 (bvsle ?x3773 n)))
 (or (= (stack_s x_0 x_1 5 n) (stack_s x_0 x_1 4 n)) $x7341)))))
 ))
 (let ((?x1516 (used_gas_s x_0 x_1 5)))
 (let ((?x4305 (sc_s 4)))
 (let ((?x374 (bvadd (_ bv63 6) ?x4305)))
 (let ((?x1628 (stack_s x_0 x_1 4 ?x374)))
 (let ((?x3773 (bvadd (_ bv62 6) ?x4305)))
 (let ((?x9812 (stack_s x_0 x_1 4 ?x3773)))
 (let (($x2838 (= $x64 (or (exc_halt_s 3) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 3))))))))
 (let (($x1958 (forall ((w (_ BitVec 256)) )(let ((?x1337 (storage_s x_0 x_1 3 w)))
 (let ((?x2075 (storage_s x_0 x_1 4 w)))
 (= ?x2075 ?x1337))))
 ))
 (let (($x1956 (forall ((n (_ BitVec 6)) )(let ((?x275 (sc_s 3)))
 (let ((?x2605 (bvadd (_ bv62 6) ?x275)))
 (let (($x3177 (bvsle ?x2605 n)))
 (or (= (stack_s x_0 x_1 4 n) (stack_s x_0 x_1 3 n)) $x3177)))))
 ))
 (let ((?x275 (sc_s 3)))
 (let (($x1228 (= ?x4305 ?x275)))
 (let ((?x1423 (used_gas_s x_0 x_1 4)))
 (let (($x247 (exc_halt_s 2)))
 (let (($x9214 (or $x247 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 2)))) (_ bv0 1))))))
 (let (($x292 (exc_halt_s 3)))
 (let (($x3529 (= $x292 $x9214)))
 (let (($x1710 (forall ((w (_ BitVec 256)) )(let ((?x1387 (storage_s x_0 x_1 2 w)))
 (let ((?x1337 (storage_s x_0 x_1 3 w)))
 (= ?x1337 ?x1387))))
 ))
 (let (($x1825 (forall ((n (_ BitVec 6)) )(let ((?x218 (sc_s 2)))
 (let (($x4122 (bvsle ?x218 n)))
 (or (= (stack_s x_0 x_1 3 n) (stack_s x_0 x_1 2 n)) $x4122))))
 ))
 (let (($x3819 (= ?x275 (bvadd (_ bv1 6) (sc_s 2)))))
 (let ((?x1385 (used_gas_s x_0 x_1 3)))
 (let (($x1091 (= $x247 (or (exc_halt_s 1) (not (bvsle (_ bv0 6) (bvadd (_ bv61 6) (sc_s 1))))))))
 (let (($x1187 (forall ((w (_ BitVec 256)) )(let ((?x1746 (storage_s x_0 x_1 1 w)))
 (let ((?x1387 (storage_s x_0 x_1 2 w)))
 (= ?x1387 ?x1746))))
 ))
 (let (($x1235 (forall ((n (_ BitVec 6)) )(or (bvsle (bvadd (_ bv61 6) (sc_s 1)) n) (= (stack_s x_0 x_1 2 n) (stack_s x_0 x_1 1 n))))
 ))
 (let ((?x154 (sc_s 1)))
 (let ((?x218 (sc_s 2)))
 (let (($x3835 (= ?x218 ?x154)))
 (let ((?x2048 (used_gas_s x_0 x_1 2)))
 (let (($x1698 (= (stack_s x_0 x_1 2 (bvadd (_ bv62 6) ?x218)) (stack_s x_0 x_1 1 (bvadd (_ bv62 6) ?x154)))))
 (let ((?x3842 (bvadd (_ bv63 6) ?x154)))
 (let ((?x1957 (stack_s x_0 x_1 1 ?x3842)))
 (let (($x1932 (= (stack_s x_0 x_1 2 (bvadd (_ bv63 6) ?x218)) (stack_s x_0 x_1 1 (bvadd (_ bv61 6) ?x154)))))
 (let (($x3827 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x72))) (_ bv0 1)))))
 (let (($x189 (exc_halt_s 1)))
 (let (($x2666 (= $x189 (or $x57 (not (bvsle (_ bv0 6) (bvadd (_ bv63 6) ?x72))) $x3827))))
 (let (($x1954 (forall ((w (_ BitVec 256)) )(let ((?x2893 (storage_s x_0 x_1 0 w)))
 (let ((?x1746 (storage_s x_0 x_1 1 w)))
 (= ?x1746 ?x2893))))
 ))
 (let (($x1949 (forall ((n (_ BitVec 6)) )(let ((?x72 (sc_s 0)))
 (let ((?x3338 (bvadd (_ bv63 6) ?x72)))
 (let (($x3207 (bvsle ?x3338 n)))
 (or (= (stack_s x_0 x_1 1 n) (stack_s x_0 x_1 0 n)) $x3207)))))
 ))
 (let (($x3843 (= ?x154 (bvadd (_ bv1 6) ?x72))))
 (let ((?x3338 (bvadd (_ bv63 6) ?x72)))
 (let ((?x1952 (stack_s x_0 x_1 0 ?x3338)))
 (let (($x1996 (forall ((w (_ BitVec 256)) )(let ((?x2893 (storage_s x_0 x_1 0 w)))
 (= ?x2893 (_ bv0 256))))
 ))
 (let (($x2000 (= ?x4896 0)))
 (let (($x3853 (not $x57)))
 (let (($x915 (= (stack_s x_0 x_1 0 (_ bv1 6)) x_1)))
 (let (($x1975 (= (stack_s x_0 x_1 0 (_ bv0 6)) x_0)))
 (let (($x1839 (= ?x72 (_ bv2 6))))
 (and $x1839 $x1975 $x915 $x3853 $x2000 $x1996 (= ?x1957 ?x1952) (= (stack_s x_0 x_1 1 ?x3338) ?x1952) (= (used_gas_s x_0 x_1 1) (+ 3 ?x4896)) $x3843 $x1949 $x1954 $x2666 $x1932 (= (stack_s x_0 x_1 2 (bvadd (_ bv61 6) ?x218)) ?x1957) $x1698 (= ?x2048 (+ 3 (used_gas_s x_0 x_1 1))) $x3835 $x1235 $x1187 $x1091 (= (stack_s x_0 x_1 3 ?x218) (_ bv0 256)) (= ?x1385 (+ 3 ?x2048)) $x3819 $x1825 $x1710 $x3529 (= ?x1628 (stack_s x_0 x_1 3 (bvadd (_ bv62 6) ?x275))) (= ?x9812 (stack_s x_0 x_1 3 (bvadd (_ bv63 6) ?x275))) (= ?x1423 (+ 3 ?x1385)) $x1228 $x1956 $x1958 $x2838 (= ?x1961 ?x9812) (= (stack_s x_0 x_1 5 ?x3773) ?x9812) (= (stack_s x_0 x_1 5 ?x374) ?x1628) (= ?x1516 (+ 3 ?x1423)) (= ?x805 (bvadd (_ bv1 6) ?x4305)) $x957 $x785 (= $x3979 $x795) (= (stack_s x_0 x_1 6 (bvadd (_ bv63 6) ?x926)) ?x2043) $x1404 (= ?x926 ?x930) $x1331 $x1334 $x1258 (= (stack_t x_0 x_1 1 ?x63) (_ bv0 256)) (= (used_gas_t x_0 x_1 1) (+ 3 ?x4902)) $x3747 $x1168 $x1160 $x420 (= ?x2105 ?x1155) (= (stack_t x_0 x_1 2 ?x4102) ?x1155) $x1063 (= ?x1554 (+ 3 (used_gas_t x_0 x_1 1))) $x462 $x990 $x1791 (= $x903 (or (not (bvsle (_ bv0 6) ?x4102)) $x1920 $x3572)) (= ?x1328 (stack_t x_0 x_1 2 (bvadd (_ bv60 6) ?x4056))) (= (stack_t x_0 x_1 3 (bvadd (_ bv60 6) ?x2012)) ?x2105) $x1662 $x1924 (= ?x1561 (+ 3 ?x1554)) $x3319 $x1915 $x1638 $x1640 $x1960 (= ?x2845 (+ 3 ?x1561)) $x3349 $x5953 $x5833 $x3274 (= (stack_t x_0 x_1 5 (bvadd (_ bv63 6) ?x919)) ?x1612) $x1409 (= ?x919 ?x9469) $x1407 $x1597 $x1397 $x73 $x8784 $x58 $x2181 $x4897 (not (and $x929 $x1776 $x889 $x2025))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
 )
(check-sat)
(get-proof)
