; SWAP2 SWAP3 SWAP1 SWAP2 SWAP1 => SWAP1 SWAP2 SWAP3
(set-option :produce-proofs true)
; 
(set-info :status unknown)
(declare-fun storage_t ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 256)) (_ BitVec 256))
(declare-fun storage_s ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 256)) (_ BitVec 256))
(declare-fun exc_halt_t (Int) Bool)
(declare-fun exc_halt_s (Int) Bool)
(declare-fun sc_t (Int) (_ BitVec 6))
(declare-fun stack_t ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 6)) (_ BitVec 256))
(declare-fun stack_s ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int (_ BitVec 6)) (_ BitVec 256))
(declare-fun sc_s (Int) (_ BitVec 6))
(declare-fun used_gas_t ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int) Int)
(declare-fun used_gas_s ((_ BitVec 256) (_ BitVec 256) (_ BitVec 256) (_ BitVec 256) Int) Int)
(assert
 (exists ((x_0 (_ BitVec 256)) (x_1 (_ BitVec 256)) (x_2 (_ BitVec 256)) (x_3 (_ BitVec 256)) )(let (($x2257 (forall ((w (_ BitVec 256)) )(let ((?x10735 (storage_t x_0 x_1 x_2 x_3 3 w)))
 (let ((?x9286 (storage_s x_0 x_1 x_2 x_3 5 w)))
 (= ?x9286 ?x10735))))
 ))
 (let (($x3614 (exc_halt_t 3)))
 (let (($x11317 (exc_halt_s 5)))
 (let (($x3494 (= $x11317 $x3614)))
 (let (($x4118 (forall ((n (_ BitVec 6)) )(let ((?x11304 (sc_t 3)))
 (let (($x2531 (bvsle ?x11304 n)))
 (let ((?x7242 (stack_t x_0 x_1 x_2 x_3 3 n)))
 (let ((?x3526 (stack_s x_0 x_1 x_2 x_3 5 n)))
 (let (($x408 (= ?x3526 ?x7242)))
 (or $x408 $x2531)))))))
 ))
 (let ((?x11304 (sc_t 3)))
 (let ((?x4319 (sc_s 5)))
 (let (($x1839 (= ?x4319 ?x11304)))
 (let (($x6860 (not (and $x1839 $x4118 $x3494 $x2257))))
 (let ((?x6404 (used_gas_t x_0 x_1 x_2 x_3 0)))
 (let ((?x4495 (used_gas_s x_0 x_1 x_2 x_3 0)))
 (let (($x2902 (= ?x4495 ?x6404)))
 (let (($x5657 (forall ((w (_ BitVec 256)) )(let ((?x11145 (storage_t x_0 x_1 x_2 x_3 0 w)))
 (let ((?x4115 (storage_s x_0 x_1 x_2 x_3 0 w)))
 (= ?x4115 ?x11145))))
 ))
 (let (($x56 (exc_halt_t 0)))
 (let (($x57 (exc_halt_s 0)))
 (let (($x58 (= $x57 $x56)))
 (let (($x6928 (forall ((n (_ BitVec 6)) )(let ((?x4392 (stack_t x_0 x_1 x_2 x_3 0 n)))
 (let ((?x2427 (stack_s x_0 x_1 x_2 x_3 0 n)))
 (let (($x11125 (= ?x2427 ?x4392)))
 (let ((?x63 (sc_t 0)))
 (let (($x4912 (bvsle ?x63 n)))
 (or $x4912 $x11125)))))))
 ))
 (let ((?x63 (sc_t 0)))
 (let ((?x72 (sc_s 0)))
 (let (($x73 (= ?x72 ?x63)))
 (let (($x11973 (= $x3614 (or (exc_halt_t 2) (not (bvsle (_ bv0 6) (bvadd (_ bv60 6) (sc_t 2))))))))
 (let (($x4258 (forall ((w (_ BitVec 256)) )(let ((?x10631 (storage_t x_0 x_1 x_2 x_3 2 w)))
 (let ((?x10735 (storage_t x_0 x_1 x_2 x_3 3 w)))
 (= ?x10735 ?x10631))))
 ))
 (let (($x6823 (forall ((n (_ BitVec 6)) )(let ((?x4252 (stack_t x_0 x_1 x_2 x_3 2 n)))
 (let ((?x7242 (stack_t x_0 x_1 x_2 x_3 3 n)))
 (let (($x10493 (= ?x7242 ?x4252)))
 (let ((?x6158 (sc_t 2)))
 (let ((?x1786 (bvadd (_ bv60 6) ?x6158)))
 (let (($x10162 (bvsle ?x1786 n)))
 (or $x10162 $x10493))))))))
 ))
 (let ((?x6158 (sc_t 2)))
 (let (($x10184 (= ?x11304 ?x6158)))
 (let ((?x6487 (used_gas_t x_0 x_1 x_2 x_3 3)))
 (let (($x8221 (= ?x6487 (+ 3 (used_gas_t x_0 x_1 x_2 x_3 2)))))
 (let ((?x5327 (bvadd (_ bv62 6) ?x6158)))
 (let ((?x9814 (stack_t x_0 x_1 x_2 x_3 2 ?x5327)))
 (let ((?x9790 (bvadd (_ bv62 6) ?x11304)))
 (let ((?x8677 (stack_t x_0 x_1 x_2 x_3 3 ?x9790)))
 (let (($x10852 (= ?x8677 ?x9814)))
 (let ((?x495 (bvadd (_ bv61 6) ?x6158)))
 (let ((?x5739 (stack_t x_0 x_1 x_2 x_3 2 ?x495)))
 (let ((?x8581 (bvadd (_ bv61 6) ?x11304)))
 (let ((?x112 (stack_t x_0 x_1 x_2 x_3 3 ?x8581)))
 (let (($x8676 (= ?x112 ?x5739)))
 (let ((?x4024 (bvadd (_ bv63 6) ?x6158)))
 (let ((?x10126 (stack_t x_0 x_1 x_2 x_3 2 ?x4024)))
 (let (($x6685 (= (stack_t x_0 x_1 x_2 x_3 3 (bvadd (_ bv60 6) ?x11304)) ?x10126)))
 (let ((?x2559 (bvadd (_ bv63 6) ?x11304)))
 (let ((?x11925 (stack_t x_0 x_1 x_2 x_3 3 ?x2559)))
 (let (($x10164 (= ?x11925 (stack_t x_0 x_1 x_2 x_3 2 (bvadd (_ bv60 6) ?x6158)))))
 (let (($x4045 (exc_halt_t 2)))
 (let (($x5956 (= $x4045 (or (exc_halt_t 1) (not (bvsle (_ bv0 6) (bvadd (_ bv61 6) (sc_t 1))))))))
 (let (($x11265 (forall ((w (_ BitVec 256)) )(let ((?x8952 (storage_t x_0 x_1 x_2 x_3 1 w)))
 (let ((?x10631 (storage_t x_0 x_1 x_2 x_3 2 w)))
 (= ?x10631 ?x8952))))
 ))
 (let (($x4402 (forall ((n (_ BitVec 6)) )(let ((?x2435 (stack_t x_0 x_1 x_2 x_3 1 n)))
 (let ((?x4252 (stack_t x_0 x_1 x_2 x_3 2 n)))
 (let (($x5674 (= ?x4252 ?x2435)))
 (or (bvsle (bvadd (_ bv61 6) (sc_t 1)) n) $x5674)))))
 ))
 (let ((?x7154 (sc_t 1)))
 (let (($x3248 (= ?x6158 ?x7154)))
 (let ((?x3278 (used_gas_t x_0 x_1 x_2 x_3 2)))
 (let (($x843 (= ?x3278 (+ 3 (used_gas_t x_0 x_1 x_2 x_3 1)))))
 (let ((?x4452 (bvadd (_ bv62 6) ?x7154)))
 (let ((?x2025 (stack_t x_0 x_1 x_2 x_3 1 ?x4452)))
 (let (($x4051 (= ?x9814 ?x2025)))
 (let ((?x11261 (bvadd (_ bv63 6) ?x7154)))
 (let ((?x3608 (stack_t x_0 x_1 x_2 x_3 1 ?x11261)))
 (let (($x10872 (= ?x5739 ?x3608)))
 (let ((?x248 (bvadd (_ bv61 6) ?x7154)))
 (let ((?x5877 (stack_t x_0 x_1 x_2 x_3 1 ?x248)))
 (let (($x8387 (= ?x10126 ?x5877)))
 (let (($x8377 (exc_halt_t 1)))
 (let (($x5868 (= $x8377 (or $x56 (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) ?x63)))))))
 (let (($x5124 (forall ((w (_ BitVec 256)) )(let ((?x11145 (storage_t x_0 x_1 x_2 x_3 0 w)))
 (let ((?x8952 (storage_t x_0 x_1 x_2 x_3 1 w)))
 (= ?x8952 ?x11145))))
 ))
 (let (($x7029 (forall ((n (_ BitVec 6)) )(let ((?x4392 (stack_t x_0 x_1 x_2 x_3 0 n)))
 (let ((?x2435 (stack_t x_0 x_1 x_2 x_3 1 n)))
 (let (($x2467 (= ?x2435 ?x4392)))
 (or $x2467 (bvsle (bvadd (_ bv62 6) (sc_t 0)) n))))))
 ))
 (let (($x606 (= ?x7154 ?x63)))
 (let ((?x3203 (used_gas_t x_0 x_1 x_2 x_3 1)))
 (let (($x11689 (= ?x3203 (+ 3 ?x6404))))
 (let ((?x5841 (bvadd (_ bv63 6) ?x63)))
 (let ((?x9513 (stack_t x_0 x_1 x_2 x_3 0 ?x5841)))
 (let (($x2925 (= ?x2025 ?x9513)))
 (let ((?x5779 (bvadd (_ bv62 6) ?x63)))
 (let ((?x11131 (stack_t x_0 x_1 x_2 x_3 0 ?x5779)))
 (let (($x9849 (= ?x3608 ?x11131)))
 (let (($x10609 (= $x11317 (or (exc_halt_s 4) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 4))))))))
 (let (($x11796 (forall ((w (_ BitVec 256)) )(let ((?x601 (storage_s x_0 x_1 x_2 x_3 4 w)))
 (let ((?x9286 (storage_s x_0 x_1 x_2 x_3 5 w)))
 (= ?x9286 ?x601))))
 ))
 (let (($x6664 (forall ((n (_ BitVec 6)) )(let ((?x3752 (stack_s x_0 x_1 x_2 x_3 4 n)))
 (let ((?x3526 (stack_s x_0 x_1 x_2 x_3 5 n)))
 (let (($x4373 (= ?x3526 ?x3752)))
 (or $x4373 (bvsle (bvadd (_ bv62 6) (sc_s 4)) n))))))
 ))
 (let ((?x4305 (sc_s 4)))
 (let (($x11146 (= ?x4319 ?x4305)))
 (let ((?x10013 (used_gas_s x_0 x_1 x_2 x_3 5)))
 (let (($x563 (= ?x10013 (+ 3 (used_gas_s x_0 x_1 x_2 x_3 4)))))
 (let ((?x4799 (bvadd (_ bv63 6) ?x4305)))
 (let ((?x2818 (stack_s x_0 x_1 x_2 x_3 4 ?x4799)))
 (let ((?x4237 (bvadd (_ bv62 6) ?x4319)))
 (let ((?x2990 (stack_s x_0 x_1 x_2 x_3 5 ?x4237)))
 (let ((?x2590 (bvadd (_ bv62 6) ?x4305)))
 (let ((?x8022 (stack_s x_0 x_1 x_2 x_3 4 ?x2590)))
 (let ((?x8736 (bvadd (_ bv63 6) ?x4319)))
 (let ((?x9842 (stack_s x_0 x_1 x_2 x_3 5 ?x8736)))
 (let (($x7963 (= ?x9842 ?x8022)))
 (let (($x7172 (exc_halt_s 4)))
 (let (($x372 (= $x7172 (or (exc_halt_s 3) (not (bvsle (_ bv0 6) (bvadd (_ bv61 6) (sc_s 3))))))))
 (let (($x1320 (forall ((w (_ BitVec 256)) )(let ((?x10048 (storage_s x_0 x_1 x_2 x_3 3 w)))
 (let ((?x601 (storage_s x_0 x_1 x_2 x_3 4 w)))
 (= ?x601 ?x10048))))
 ))
 (let (($x518 (forall ((n (_ BitVec 6)) )(let ((?x275 (sc_s 3)))
 (let ((?x2289 (bvadd (_ bv61 6) ?x275)))
 (let (($x11555 (bvsle ?x2289 n)))
 (let ((?x3390 (stack_s x_0 x_1 x_2 x_3 3 n)))
 (let ((?x3752 (stack_s x_0 x_1 x_2 x_3 4 n)))
 (let (($x8032 (= ?x3752 ?x3390)))
 (or $x8032 $x11555))))))))
 ))
 (let ((?x275 (sc_s 3)))
 (let (($x3757 (= ?x4305 ?x275)))
 (let ((?x179 (used_gas_s x_0 x_1 x_2 x_3 4)))
 (let (($x10718 (= ?x179 (+ 3 (used_gas_s x_0 x_1 x_2 x_3 3)))))
 (let ((?x8389 (bvadd (_ bv62 6) ?x275)))
 (let ((?x9495 (stack_s x_0 x_1 x_2 x_3 3 ?x8389)))
 (let (($x1218 (= ?x8022 ?x9495)))
 (let ((?x2261 (bvadd (_ bv63 6) ?x275)))
 (let ((?x751 (stack_s x_0 x_1 x_2 x_3 3 ?x2261)))
 (let ((?x5422 (bvadd (_ bv61 6) ?x4305)))
 (let ((?x4574 (stack_s x_0 x_1 x_2 x_3 4 ?x5422)))
 (let (($x5187 (= ?x4574 ?x751)))
 (let ((?x2289 (bvadd (_ bv61 6) ?x275)))
 (let ((?x644 (stack_s x_0 x_1 x_2 x_3 3 ?x2289)))
 (let (($x11670 (= ?x2818 ?x644)))
 (let (($x292 (exc_halt_s 3)))
 (let (($x5172 (= $x292 (or (exc_halt_s 2) (not (bvsle (_ bv0 6) (bvadd (_ bv62 6) (sc_s 2))))))))
 (let (($x2865 (forall ((w (_ BitVec 256)) )(let ((?x7488 (storage_s x_0 x_1 x_2 x_3 2 w)))
 (let ((?x10048 (storage_s x_0 x_1 x_2 x_3 3 w)))
 (= ?x10048 ?x7488))))
 ))
 (let (($x5927 (forall ((n (_ BitVec 6)) )(let ((?x218 (sc_s 2)))
 (let ((?x4042 (bvadd (_ bv62 6) ?x218)))
 (let (($x11693 (bvsle ?x4042 n)))
 (let ((?x8410 (stack_s x_0 x_1 x_2 x_3 2 n)))
 (let ((?x3390 (stack_s x_0 x_1 x_2 x_3 3 n)))
 (let (($x2908 (= ?x3390 ?x8410)))
 (or $x2908 $x11693))))))))
 ))
 (let ((?x218 (sc_s 2)))
 (let (($x681 (= ?x275 ?x218)))
 (let ((?x6732 (used_gas_s x_0 x_1 x_2 x_3 3)))
 (let (($x3681 (= ?x6732 (+ 3 (used_gas_s x_0 x_1 x_2 x_3 2)))))
 (let ((?x4042 (bvadd (_ bv62 6) ?x218)))
 (let ((?x9843 (stack_s x_0 x_1 x_2 x_3 2 ?x4042)))
 (let (($x9872 (= ?x751 ?x9843)))
 (let (($x247 (exc_halt_s 2)))
 (let (($x5533 (= $x247 (or (exc_halt_s 1) (not (bvsle (_ bv0 6) (bvadd (_ bv60 6) (sc_s 1))))))))
 (let (($x6078 (forall ((w (_ BitVec 256)) )(let ((?x11262 (storage_s x_0 x_1 x_2 x_3 1 w)))
 (let ((?x7488 (storage_s x_0 x_1 x_2 x_3 2 w)))
 (= ?x7488 ?x11262))))
 ))
 (let (($x121 (forall ((n (_ BitVec 6)) )(let ((?x10513 (stack_s x_0 x_1 x_2 x_3 1 n)))
 (let ((?x8410 (stack_s x_0 x_1 x_2 x_3 2 n)))
 (let (($x10262 (= ?x8410 ?x10513)))
 (let ((?x154 (sc_s 1)))
 (let ((?x4453 (bvadd (_ bv60 6) ?x154)))
 (let (($x995 (bvsle ?x4453 n)))
 (or $x995 $x10262))))))))
 ))
 (let ((?x154 (sc_s 1)))
 (let (($x11030 (= ?x218 ?x154)))
 (let ((?x4524 (used_gas_s x_0 x_1 x_2 x_3 2)))
 (let (($x10227 (= ?x4524 (+ 3 (used_gas_s x_0 x_1 x_2 x_3 1)))))
 (let ((?x3159 (bvadd (_ bv62 6) ?x154)))
 (let ((?x2967 (stack_s x_0 x_1 x_2 x_3 1 ?x3159)))
 (let (($x2028 (= ?x9843 ?x2967)))
 (let ((?x98 (bvadd (_ bv61 6) ?x154)))
 (let ((?x5652 (stack_s x_0 x_1 x_2 x_3 1 ?x98)))
 (let ((?x9067 (bvadd (_ bv61 6) ?x218)))
 (let ((?x2219 (stack_s x_0 x_1 x_2 x_3 2 ?x9067)))
 (let (($x1300 (= ?x2219 ?x5652)))
 (let ((?x11015 (bvadd (_ bv63 6) ?x154)))
 (let ((?x11160 (stack_s x_0 x_1 x_2 x_3 1 ?x11015)))
 (let (($x2311 (= (stack_s x_0 x_1 x_2 x_3 2 (bvadd (_ bv60 6) ?x218)) ?x11160)))
 (let ((?x453 (bvadd (_ bv63 6) ?x218)))
 (let ((?x10753 (stack_s x_0 x_1 x_2 x_3 2 ?x453)))
 (let (($x8890 (= ?x10753 (stack_s x_0 x_1 x_2 x_3 1 (bvadd (_ bv60 6) ?x154)))))
 (let (($x189 (exc_halt_s 1)))
 (let (($x208 (= $x189 (or $x57 (not (bvsle (_ bv0 6) (bvadd (_ bv61 6) ?x72)))))))
 (let (($x6291 (forall ((w (_ BitVec 256)) )(let ((?x4115 (storage_s x_0 x_1 x_2 x_3 0 w)))
 (let ((?x11262 (storage_s x_0 x_1 x_2 x_3 1 w)))
 (= ?x11262 ?x4115))))
 ))
 (let (($x5137 (forall ((n (_ BitVec 6)) )(let ((?x2427 (stack_s x_0 x_1 x_2 x_3 0 n)))
 (let ((?x10513 (stack_s x_0 x_1 x_2 x_3 1 n)))
 (let (($x9101 (= ?x10513 ?x2427)))
 (let ((?x72 (sc_s 0)))
 (let ((?x6111 (bvadd (_ bv61 6) ?x72)))
 (let (($x3068 (bvsle ?x6111 n)))
 (or $x3068 $x9101))))))))
 ))
 (let (($x2517 (= ?x154 ?x72)))
 (let ((?x503 (used_gas_s x_0 x_1 x_2 x_3 1)))
 (let (($x7135 (= ?x503 (+ 3 ?x4495))))
 (let ((?x6111 (bvadd (_ bv61 6) ?x72)))
 (let ((?x10851 (stack_s x_0 x_1 x_2 x_3 0 ?x6111)))
 (let (($x9073 (= ?x11160 ?x10851)))
 (let (($x5618 (forall ((w (_ BitVec 256)) )(let ((?x4115 (storage_s x_0 x_1 x_2 x_3 0 w)))
 (= ?x4115 (_ bv0 256))))
 ))
 (let (($x9019 (= ?x4495 0)))
 (let (($x2084 (not $x57)))
 (let (($x2198 (= (stack_s x_0 x_1 x_2 x_3 0 (_ bv3 6)) x_3)))
 (let (($x8277 (= (stack_s x_0 x_1 x_2 x_3 0 (_ bv2 6)) x_2)))
 (let (($x3356 (= (stack_s x_0 x_1 x_2 x_3 0 (_ bv1 6)) x_1)))
 (let (($x2338 (= (stack_s x_0 x_1 x_2 x_3 0 (_ bv0 6)) x_0)))
 (let (($x11199 (= ?x72 (_ bv4 6))))
 (and $x11199 $x2338 $x3356 $x8277 $x2198 $x2084 $x9019 $x5618 $x9073 (= ?x5652 (stack_s x_0 x_1 x_2 x_3 0 (bvadd (_ bv63 6) ?x72))) (= ?x2967 (stack_s x_0 x_1 x_2 x_3 0 (bvadd (_ bv62 6) ?x72))) $x7135 $x2517 $x5137 $x6291 $x208 $x8890 $x2311 $x1300 $x2028 $x10227 $x11030 $x121 $x6078 $x5533 $x9872 (= ?x9495 ?x10753) $x3681 $x681 $x5927 $x2865 $x5172 $x11670 $x5187 $x1218 $x10718 $x3757 $x518 $x1320 $x372 $x7963 (= ?x2990 ?x2818) $x563 $x11146 $x6664 $x11796 $x10609 $x9849 $x2925 $x11689 $x606 $x7029 $x5124 $x5868 $x8387 $x10872 $x4051 $x843 $x3248 $x4402 $x11265 $x5956 $x10164 $x6685 $x8676 $x10852 $x8221 $x10184 $x6823 $x4258 $x11973 $x73 $x6928 $x58 $x5657 $x2902 $x6860)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
 )
(check-sat)
(get-proof)