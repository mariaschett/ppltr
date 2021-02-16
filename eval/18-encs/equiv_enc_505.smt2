; PUSH cw_4 SHA3 PUSH 0x00 ADD PUSH cw_3 SWAP1 => PUSH cw_3 SWAP1 PUSH cw_4 SHA3
(set-option :produce-proofs true)
; 
(set-info :status unknown)
(declare-fun storage_t ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 256)) (_ BitVec 256))
(declare-fun w_3 () (_ BitVec 256))
(declare-fun w_4 () (_ BitVec 256))
(declare-fun storage_s ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 256)) (_ BitVec 256))
(declare-fun exc_halt_t (Int) Bool)
(declare-fun exc_halt_s (Int) Bool)
(declare-fun stack_t ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 6)) (_ BitVec 256))
(declare-fun stack_s ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 6)) (_ BitVec 256))
(declare-fun sc_t (Int) (_ BitVec 6))
(declare-fun sc_s (Int) (_ BitVec 6))
(declare-fun used_gas_t ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int) Int)
(declare-fun used_gas_s ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int) Int)
(declare-fun f_SHA3 ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256)) (_ BitVec 256))
(assert
 (exists ((x_0 (_ BitVec 256)) (x_SHA3_0 (_ BitVec 256)) )(let (($x7882 (forall ((w (_ BitVec 256)) )(let ((?x3139 (storage_t x_0 w_4 w_3 x_SHA3_0 4 w)))
 (let ((?x10886 (storage_s x_0 w_4 w_3 x_SHA3_0 6 w)))
 (= ?x10886 ?x3139))))
 ))
 (let (($x7854 (exc_halt_t 4)))
 (let (($x772 (exc_halt_s 6)))
 (let (($x4404 (= $x772 $x7854)))
 (let (($x5822 (forall ((n (_ BitVec 6)) )(let ((?x2077 (stack_t x_0 w_4 w_3 x_SHA3_0 4 n)))
 (let ((?x7878 (stack_s x_0 w_4 w_3 x_SHA3_0 6 n)))
 (let (($x4302 (= ?x7878 ?x2077)))
 (or (bvsle (sc_t 4) n) $x4302)))))
 ))
 (let ((?x4818 (sc_t 4)))
 (let ((?x926 (sc_s 6)))
 (let (($x7899 (= ?x926 ?x4818)))
 (let ((?x4511 (used_gas_t x_0 w_4 w_3 x_SHA3_0 0)))
 (let ((?x6462 (used_gas_s x_0 w_4 w_3 x_SHA3_0 0)))
 (let (($x8134 (= ?x6462 ?x4511)))
 (let (($x11716 (forall ((w (_ BitVec 256)) )(let ((?x6259 (storage_t x_0 w_4 w_3 x_SHA3_0 0 w)))
 (let ((?x2084 (storage_s x_0 w_4 w_3 x_SHA3_0 0 w)))
 (= ?x2084 ?x6259))))
 ))
 (let (($x56 (exc_halt_t 0)))
 (let (($x57 (exc_halt_s 0)))
 (let (($x58 (= $x57 $x56)))
 (let (($x1737 (forall ((n (_ BitVec 6)) )(let ((?x11604 (stack_t x_0 w_4 w_3 x_SHA3_0 0 n)))
 (let ((?x4512 (stack_s x_0 w_4 w_3 x_SHA3_0 0 n)))
 (let (($x5339 (= ?x4512 ?x11604)))
 (let ((?x63 (sc_t 0)))
 (let (($x1710 (bvsle ?x63 n)))
 (or $x1710 $x5339)))))))
 ))
 (let ((?x63 (sc_t 0)))
 (let ((?x72 (sc_s 0)))
 (let (($x73 (= ?x72 ?x63)))
 (let (($x2850 (= $x7854 (or (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_t 3)))) (exc_halt_t 3)))))
 (let (($x2786 (forall ((w (_ BitVec 256)) )(let ((?x8519 (storage_t x_0 w_4 w_3 x_SHA3_0 3 w)))
 (let ((?x3139 (storage_t x_0 w_4 w_3 x_SHA3_0 4 w)))
 (= ?x3139 ?x8519))))
 ))
 (let (($x3037 (forall ((n (_ BitVec 6)) )(let ((?x7484 (stack_t x_0 w_4 w_3 x_SHA3_0 3 n)))
 (let ((?x2077 (stack_t x_0 w_4 w_3 x_SHA3_0 4 n)))
 (or (bvsle (bvadd (_ bv62 6) (sc_t 3)) n) (= ?x2077 ?x7484)))))
 ))
 (let (($x3890 (= (used_gas_t x_0 w_4 w_3 x_SHA3_0 4) (+ 30 (used_gas_t x_0 w_4 w_3 x_SHA3_0 3)))))
 (let ((?x8079 (f_SHA3 x_0 w_4 w_3 x_SHA3_0 (stack_t x_0 w_4 w_3 x_SHA3_0 3 (bvadd (_ bv63 6) (sc_t 3))) (stack_t x_0 w_4 w_3 x_SHA3_0 3 (bvadd (_ bv62 6) (sc_t 3))))))
 (let (($x2163 (exc_halt_t 2)))
 (let (($x2797 (or $x2163 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_t 2)))) (_ bv0 1))))))
 (let (($x6783 (exc_halt_t 3)))
 (let (($x10401 (forall ((w (_ BitVec 256)) )(let ((?x4298 (storage_t x_0 w_4 w_3 x_SHA3_0 2 w)))
 (let ((?x8519 (storage_t x_0 w_4 w_3 x_SHA3_0 3 w)))
 (= ?x8519 ?x4298))))
 ))
 (let (($x8348 (forall ((n (_ BitVec 6)) )(let ((?x10445 (stack_t x_0 w_4 w_3 x_SHA3_0 2 n)))
 (let ((?x7484 (stack_t x_0 w_4 w_3 x_SHA3_0 3 n)))
 (or (= ?x7484 ?x10445) (bvsle (sc_t 2) n)))))
 ))
 (let ((?x8075 (used_gas_t x_0 w_4 w_3 x_SHA3_0 3)))
 (let (($x9271 (= $x2163 (or (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_t 1)))) (exc_halt_t 1)))))
 (let (($x4183 (forall ((w (_ BitVec 256)) )(let ((?x1806 (storage_t x_0 w_4 w_3 x_SHA3_0 1 w)))
 (let ((?x4298 (storage_t x_0 w_4 w_3 x_SHA3_0 2 w)))
 (= ?x4298 ?x1806))))
 ))
 (let (($x8847 (forall ((n (_ BitVec 6)) )(let ((?x6177 (stack_t x_0 w_4 w_3 x_SHA3_0 1 n)))
 (let ((?x10445 (stack_t x_0 w_4 w_3 x_SHA3_0 2 n)))
 (or (bvsle (bvadd (_ bv62 6) (sc_t 1)) n) (= ?x10445 ?x6177)))))
 ))
 (let ((?x2455 (used_gas_t x_0 w_4 w_3 x_SHA3_0 2)))
 (let (($x11738 (= (stack_t x_0 w_4 w_3 x_SHA3_0 2 (bvadd (_ bv62 6) (sc_t 2))) (stack_t x_0 w_4 w_3 x_SHA3_0 1 (bvadd (_ bv63 6) (sc_t 1))))))
 (let (($x8273 (= (stack_t x_0 w_4 w_3 x_SHA3_0 2 (bvadd (_ bv63 6) (sc_t 2))) (stack_t x_0 w_4 w_3 x_SHA3_0 1 (bvadd (_ bv62 6) (sc_t 1))))))
 (let (($x8377 (exc_halt_t 1)))
 (let (($x5048 (= $x8377 (or $x56 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x63))) (_ bv0 1)))))))
 (let (($x2221 (forall ((w (_ BitVec 256)) )(let ((?x6259 (storage_t x_0 w_4 w_3 x_SHA3_0 0 w)))
 (let ((?x1806 (storage_t x_0 w_4 w_3 x_SHA3_0 1 w)))
 (= ?x1806 ?x6259))))
 ))
 (let (($x5645 (forall ((n (_ BitVec 6)) )(let ((?x11604 (stack_t x_0 w_4 w_3 x_SHA3_0 0 n)))
 (let ((?x6177 (stack_t x_0 w_4 w_3 x_SHA3_0 1 n)))
 (let ((?x63 (sc_t 0)))
 (let (($x1710 (bvsle ?x63 n)))
 (or $x1710 (= ?x6177 ?x11604)))))))
 ))
 (let (($x1129 (= $x772 (or (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 5)))) (exc_halt_s 5)))))
 (let (($x6601 (forall ((w (_ BitVec 256)) )(let ((?x9713 (storage_s x_0 w_4 w_3 x_SHA3_0 5 w)))
 (let ((?x10886 (storage_s x_0 w_4 w_3 x_SHA3_0 6 w)))
 (= ?x10886 ?x9713))))
 ))
 (let (($x9277 (forall ((n (_ BitVec 6)) )(let ((?x7399 (stack_s x_0 w_4 w_3 x_SHA3_0 5 n)))
 (let ((?x7878 (stack_s x_0 w_4 w_3 x_SHA3_0 6 n)))
 (or (= ?x7878 ?x7399) (bvsle (bvadd (_ bv62 6) (sc_s 5)) n)))))
 ))
 (let (($x11617 (= (used_gas_s x_0 w_4 w_3 x_SHA3_0 6) (+ 3 (used_gas_s x_0 w_4 w_3 x_SHA3_0 5)))))
 (let (($x10429 (= (stack_s x_0 w_4 w_3 x_SHA3_0 6 (bvadd (_ bv62 6) ?x926)) (stack_s x_0 w_4 w_3 x_SHA3_0 5 (bvadd (_ bv63 6) (sc_s 5))))))
 (let (($x1726 (= (stack_s x_0 w_4 w_3 x_SHA3_0 6 (bvadd (_ bv63 6) ?x926)) (stack_s x_0 w_4 w_3 x_SHA3_0 5 (bvadd (_ bv62 6) (sc_s 5))))))
 (let (($x7172 (exc_halt_s 4)))
 (let (($x6443 (or $x7172 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 4)))) (_ bv0 1))))))
 (let (($x11317 (exc_halt_s 5)))
 (let (($x9835 (forall ((w (_ BitVec 256)) )(let ((?x7029 (storage_s x_0 w_4 w_3 x_SHA3_0 4 w)))
 (let ((?x9713 (storage_s x_0 w_4 w_3 x_SHA3_0 5 w)))
 (= ?x9713 ?x7029))))
 ))
 (let (($x1503 (forall ((n (_ BitVec 6)) )(let ((?x9927 (stack_s x_0 w_4 w_3 x_SHA3_0 4 n)))
 (let ((?x7399 (stack_s x_0 w_4 w_3 x_SHA3_0 5 n)))
 (or (bvsle (sc_s 4) n) (= ?x7399 ?x9927)))))
 ))
 (let ((?x9918 (used_gas_s x_0 w_4 w_3 x_SHA3_0 5)))
 (let (($x976 (= $x7172 (or (exc_halt_s 3) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 3))))))))
 (let (($x8600 (forall ((w (_ BitVec 256)) )(let ((?x7283 (storage_s x_0 w_4 w_3 x_SHA3_0 3 w)))
 (let ((?x7029 (storage_s x_0 w_4 w_3 x_SHA3_0 4 w)))
 (= ?x7029 ?x7283))))
 ))
 (let (($x8683 (forall ((n (_ BitVec 6)) )(let ((?x6085 (stack_s x_0 w_4 w_3 x_SHA3_0 3 n)))
 (let ((?x9927 (stack_s x_0 w_4 w_3 x_SHA3_0 4 n)))
 (or (bvsle (bvadd (_ bv62 6) (sc_s 3)) n) (= ?x9927 ?x6085)))))
 ))
 (let ((?x5808 (used_gas_s x_0 w_4 w_3 x_SHA3_0 4)))
 (let ((?x9293 (bvadd (stack_s x_0 w_4 w_3 x_SHA3_0 3 (bvadd (_ bv63 6) (sc_s 3))) (stack_s x_0 w_4 w_3 x_SHA3_0 3 (bvadd (_ bv62 6) (sc_s 3))))))
 (let (($x264 (= (stack_s x_0 w_4 w_3 x_SHA3_0 4 (bvadd (_ bv63 6) (sc_s 4))) ?x9293)))
 (let (($x247 (exc_halt_s 2)))
 (let (($x3515 (or $x247 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) (sc_s 2)))) (_ bv0 1))))))
 (let (($x292 (exc_halt_s 3)))
 (let (($x5684 (forall ((w (_ BitVec 256)) )(let ((?x5210 (storage_s x_0 w_4 w_3 x_SHA3_0 2 w)))
 (let ((?x7283 (storage_s x_0 w_4 w_3 x_SHA3_0 3 w)))
 (= ?x7283 ?x5210))))
 ))
 (let (($x2334 (forall ((n (_ BitVec 6)) )(let ((?x5665 (stack_s x_0 w_4 w_3 x_SHA3_0 2 n)))
 (let ((?x6085 (stack_s x_0 w_4 w_3 x_SHA3_0 3 n)))
 (or (bvsle (sc_s 2) n) (= ?x6085 ?x5665)))))
 ))
 (let ((?x8033 (used_gas_s x_0 w_4 w_3 x_SHA3_0 3)))
 (let (($x6121 (= $x247 (or (exc_halt_s 1) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 1))))))))
 (let (($x7992 (forall ((w (_ BitVec 256)) )(let ((?x3723 (storage_s x_0 w_4 w_3 x_SHA3_0 1 w)))
 (let ((?x5210 (storage_s x_0 w_4 w_3 x_SHA3_0 2 w)))
 (= ?x5210 ?x3723))))
 ))
 (let (($x9238 (forall ((n (_ BitVec 6)) )(let ((?x4521 (stack_s x_0 w_4 w_3 x_SHA3_0 1 n)))
 (let ((?x5665 (stack_s x_0 w_4 w_3 x_SHA3_0 2 n)))
 (or (= ?x5665 ?x4521) (bvsle (bvadd (_ bv62 6) (sc_s 1)) n)))))
 ))
 (let ((?x1881 (used_gas_s x_0 w_4 w_3 x_SHA3_0 2)))
 (let ((?x2498 (f_SHA3 x_0 w_4 w_3 x_SHA3_0 (stack_s x_0 w_4 w_3 x_SHA3_0 1 (bvadd (_ bv63 6) (sc_s 1))) (stack_s x_0 w_4 w_3 x_SHA3_0 1 (bvadd (_ bv62 6) (sc_s 1))))))
 (let (($x2855 (= (stack_s x_0 w_4 w_3 x_SHA3_0 2 (bvadd (_ bv63 6) (sc_s 2))) ?x2498)))
 (let (($x189 (exc_halt_s 1)))
 (let (($x10045 (= $x189 (or $x57 (not (= ((_ extract 6 6) (bvadd (_ bv1 7) (concat (_ bv0 1) ?x72))) (_ bv0 1)))))))
 (let (($x1776 (forall ((w (_ BitVec 256)) )(let ((?x2084 (storage_s x_0 w_4 w_3 x_SHA3_0 0 w)))
 (let ((?x3723 (storage_s x_0 w_4 w_3 x_SHA3_0 1 w)))
 (= ?x3723 ?x2084))))
 ))
 (let (($x5797 (forall ((n (_ BitVec 6)) )(let ((?x4512 (stack_s x_0 w_4 w_3 x_SHA3_0 0 n)))
 (let ((?x4521 (stack_s x_0 w_4 w_3 x_SHA3_0 1 n)))
 (or (bvsle (sc_s 0) n) (= ?x4521 ?x4512)))))
 ))
 (let (($x6173 (forall ((w0 (_ BitVec 256)) (w1 (_ BitVec 256)) )(let (($x3451 (= (stack_s x_0 w_4 w_3 x_SHA3_0 1 (bvadd (_ bv62 6) (sc_s 1))) w1)))
 (let (($x7704 (= (stack_s x_0 w_4 w_3 x_SHA3_0 1 (bvadd (_ bv63 6) (sc_s 1))) w0)))
 (let ((?x10520 (f_SHA3 x_0 w_4 w_3 x_SHA3_0 w0 w1)))
 (= ?x10520 (ite (and $x7704 $x3451) x_SHA3_0 (_ bv0 256)))))))
 ))
 (let (($x5704 (forall ((w (_ BitVec 256)) )(let ((?x2084 (storage_s x_0 w_4 w_3 x_SHA3_0 0 w)))
 (= ?x2084 (_ bv0 256))))
 ))
 (let (($x1187 (= ?x6462 0)))
 (let (($x488 (= (stack_s x_0 w_4 w_3 x_SHA3_0 0 (_ bv0 6)) x_0)))
 (let (($x7865 (= ?x72 (_ bv1 6))))
 (and $x7865 $x488 (not $x57) $x1187 $x5704 $x6173 (= (stack_s x_0 w_4 w_3 x_SHA3_0 1 ?x72) w_4) (= (used_gas_s x_0 w_4 w_3 x_SHA3_0 1) (+ 3 ?x6462)) (= (sc_s 1) (bvadd (_ bv1 6) ?x72)) $x5797 $x1776 $x10045 $x2855 (= ?x1881 (+ 30 (used_gas_s x_0 w_4 w_3 x_SHA3_0 1))) (= (sc_s 2) (bvadd (_ bv63 6) (sc_s 1))) $x9238 $x7992 $x6121 (= (stack_s x_0 w_4 w_3 x_SHA3_0 3 (sc_s 2)) (_ bv0 256)) (= ?x8033 (+ 3 ?x1881)) (= (sc_s 3) (bvadd (_ bv1 6) (sc_s 2))) $x2334 $x5684 (= $x292 $x3515) $x264 (= ?x5808 (+ 3 ?x8033)) (= (sc_s 4) (bvadd (_ bv63 6) (sc_s 3))) $x8683 $x8600 $x976 (= (stack_s x_0 w_4 w_3 x_SHA3_0 5 (sc_s 4)) w_3) (= ?x9918 (+ 3 ?x5808)) (= (sc_s 5) (bvadd (_ bv1 6) (sc_s 4))) $x1503 $x9835 (= $x11317 $x6443) $x1726 $x10429 $x11617 (= ?x926 (sc_s 5)) $x9277 $x6601 $x1129 (= (stack_t x_0 w_4 w_3 x_SHA3_0 1 ?x63) w_3) (= (used_gas_t x_0 w_4 w_3 x_SHA3_0 1) (+ 3 ?x4511)) (= (sc_t 1) (bvadd (_ bv1 6) ?x63)) $x5645 $x2221 $x5048 $x8273 $x11738 (= ?x2455 (+ 3 (used_gas_t x_0 w_4 w_3 x_SHA3_0 1))) (= (sc_t 2) (sc_t 1)) $x8847 $x4183 $x9271 (= (stack_t x_0 w_4 w_3 x_SHA3_0 3 (sc_t 2)) w_4) (= ?x8075 (+ 3 ?x2455)) (= (sc_t 3) (bvadd (_ bv1 6) (sc_t 2))) $x8348 $x10401 (= $x6783 $x2797) (= (stack_t x_0 w_4 w_3 x_SHA3_0 4 (bvadd (_ bv63 6) ?x4818)) ?x8079) $x3890 (= ?x4818 (bvadd (_ bv63 6) (sc_t 3))) $x3037 $x2786 $x2850 $x73 $x1737 $x58 $x11716 $x8134 (not (and $x7899 $x5822 $x4404 $x7882)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
 )
(check-sat)
(get-proof)