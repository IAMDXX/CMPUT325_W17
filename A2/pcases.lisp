(setq t1 '(fl-interp '(+ 10 5) nil))
(setq t2 '(fl-interp '(- 12 8) nil))
(setq t3 '(fl-interp '(* 5 9) nil))
(setq t4 '(fl-interp '(> 2 3) nil) )
(setq t5 '(fl-interp '(< 1 131) nil) )
(setq t6 ' (fl-interp '(= 88 88) nil))
(setq t7 '(fl-interp '(and nil t) nil) )
(setq t8 '(fl-interp '(or t nil) nil) )
(setq t9 '(fl-interp '(not t) nil) )
(setq t10 '(fl-interp '(isnumber 354) nil))
(setq t11 '(fl-interp '(equal (3 4 1) (3 4 1)) nil))
(setq t12 ' (fl-interp '(if nil 2 3) nil) )
(setq t13 '(fl-interp '(null ()) nil))
(setq t14 ' (fl-interp '(atom (3)) nil))
(setq t15 ' (fl-interp '(eq x x) nil))
(setq t16 ' (fl-interp '(first (8 5 16)) nil))
(setq t17 ' (fl-interp '(rest (8 5 16)) nil))
(setq t18 ' (fl-interp '(cons 6 3) nil))
(setq t19 ' (fl-interp '(+ (* 2 2) (* 2 (- (+ 2 (+ 1 (- 7 4))) 2))) nil) )
(setq t20 ' (fl-interp '(and (> (+ 3 2) (- 4 2)) (or (< 3 (* 2 2))) (not (= 3 2))) nil))
(setq t21 ' (fl-interp '(fl-interp '(or (= 5 (- 4 2)) (and (not (> 2 2)) (< 3 2))) nil))
(setq t22 ' (fl-interp '(if (not (null (first (a c e)))) (if (isnumber (first (a c e))) (first (a c e)) (cons (a c e) d)) (rest (a c e))) nil))

#|(eval t1)
(eval t2)
(eval t3)
(eval t4)
(eval t5)
(eval t6)
(eval t7)
(eval t8)
(eval t9)
(eval t10)
(eval t11)
(eval t12)
(eval t13)
(eval t14)
(eval t15)
(eval t16)
(eval t17)
(eval t18)
(eval t19)
(eval t20)
(eval t21)
(eval t22)
|#