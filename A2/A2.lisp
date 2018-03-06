#|CMPUT 325 Assignment#2   Yiting Dong   2017.3.2|#

#| Ass#2 Overview

A program P in FL is a list of function definitions. The FL interpreter takes such a program, together with a function application, and returns the result of evaluating the application. This evaluation is based on the principle of "replacing equals by equals". Your interpreter should be defined as a Lisp function

(fl-interp E P)

which, given a program P and an expression E, returns the result of evaluating E with respect to P.

The language FL includes a number of primitive functions that should be implemented in your interpreter. FL, as specified below, resembles a subset of Lisp without an environment -- FL does not have a special form "defun" to create permanent named functions.

|#
#|--------------------- Helper functions -----------------------
 
EX: U2:(fl-interp '(square 4) '((square x = (* x x))))
	f = square
	fbody = (* x x)
	farg = (x)
	func = (farg fbody) = ((x) (* x x))
	comb = ((N,V)) = ((x 4))
	get_
|#

#|Getting the function body part from P, which is the part after "=";
i.e: func_body ((square x = (* x x))) ----> output = (*x x)
|# 

(defun func_body (body)
	( if (eq (car body) '=) (cdr body) 
				(func_body (cdr body))
	)
)

#|Getting arguments of a user defined function from P, which is the part in between function name and  "=";
i.e: func_arg ((square x = (* x x))) ----> output = (x)
|# 

(defun func_arg (arg)
	( if (eq '= (car arg)) nil 
		(cons (car arg) (func_arg (cdr arg)) )
	)
)
 
#|Combining the arguments and body of a user defined function
i.e: func ('square ((square x = (* x x))) ----> output = ((x) (*x x))
|# 

(defun func (f P)
	(cond
		((null P) nil)
		((eq '= (caar P)) nil)      ;find function f before "="
		((eq f (caar P)) (list (func_arg (cdar P)) (func_body (car P))) )
		(t (func f (cdr P)))
				  
	)
)

#|Trying to create a context which combines argumrnts and their corresponding value;
i.e: comb ((x y)(2 3)) ----> output = ((x 2)(y 3))
|# 

(defun comb (names vals) 
	(if (null names) nil
		(mapcar #'(lambda (n v) (list n v)) names vals)      
		;(cons (list (car names) (car vals)) (comb (cdr names) (cdr vals)))
	)
)

#|Getting the value from a Context;
i.e:	C = ((x 3) (y 4)) 
	get_value (x C) ----> output = 3
|# 

(defun get_value (E C)
	(cond
		((null C) nil)
		((eq E (caar C)) (cadar C))
		(t (get_value E (cdr C)))
	)
)

#|Checking if a variable is bounded;
i.e:	C = ((x 3) (y 4)) 
	exist (x C) ----> output = t
|# 

(defun exist (E C)
	(cond
		((null C) nil)
		((eq E (caar C)) 't)
		(t (exist E (cdr C)))
	)
)

#|Evaluating the vals;
i.e:	for v in vals, if v is a atom, return v
	if v is a expression (primitive functions like (rest l) ) then we need to evaluate it first 
	eval_arg (3 (rest '(a b c))) ----> output = (3 (b c))
|# 

(defun eval_arg (vals P C)
		(if (null vals) nil
			(cons (ske-interp (car vals) P C)  (eval_arg (cdr vals) P C))
		)
)

#|skeleton program of fl-interp(E P); Handling all primitive evaluation and user defined function.
	 
given a program P, an expression E and start with a empty context C ; returns the result of evaluating E with respect to P.

EX: 1.Primitive functions (without program):  (fl-interp '(or t nil) nil) ; > 't
			(fl-interp '(not t) nil) ; > 'nil
			(fl-interp '(isnumber 354) nil) ; > 't
    2. Simple self-define-function program : (fl-interp '(+ (* 2 2) (* 2 (- (+ 2 (+ 1 (- 7 4))) 2))) nil) ; > '12
						 (fl-interp '(and (> (+ 3 2) (- 4 2)) (or (< 3 (* 2 2))) (not (= 3 2))) nil) ; > 't

    3. self-define-function with recursion :(fl-interp '(push (1 2 3) 4) '((push x y = (if (null x) (cons y nil) (cons (first x) (push (rest x) y)))))) ; > '(1 2 3 4)
						(fl-interp '(pop (1 2 3)) '((pop x = (if (atom (rest (rest x))) (cons (first x) nil) (cons (first x)(pop (rest x))))))) ; > '(1 2)

|# 

(defun ske-interp (E P C)
	(cond 
		; check if atom E is a bounded variable, and get its value
		((atom E) (if (exist E C) (get_value E C) E)  )   
		(t
			 ; f = function name  arg = function arguments f_arg_body = user-defined-function ((arguments) (body))
			(let ((f (car E)) (arg (cdr E)) (f_arg_body (func (car E) P))) 
			   (cond 
				;primitive functions
				((eq f 'if) (if (ske-interp (car arg) P C) (ske-interp (cadr arg) P C) (ske-interp (caddr arg) P C)))
				((eq f 'null) (null (ske-interp (car arg) P C)))
				((eq f 'atom) (atom (ske-interp (car arg) P C)))
				((eq f 'eq) (eq (ske-interp (car arg) P C) (ske-interp (cadr arg) P C)))
				((eq f 'first) (car (ske-interp (car arg) P C)))
				((eq f 'rest) (cdr (ske-interp (car arg) P C)))
				((eq f 'cons) (cons (ske-interp (car arg) P C) (ske-interp (cadr arg) P C)))
				((eq f 'equal) (equal (ske-interp (car arg) P C) (ske-interp (cadr arg) P C)))
				((eq f 'isnumber) (numberp (ske-interp (car arg) P C)))
				((eq f '+) (+ (ske-interp (car arg) P C) (ske-interp (cadr arg) P C)))
				((eq f '-) (- (ske-interp (car arg) P C) (ske-interp (cadr arg) P C)))
				((eq f '*) (* (ske-interp (car arg) P C) (ske-interp (cadr arg) P C)))
				((eq f '>) (> (ske-interp (car arg) P C) (ske-interp (cadr arg) P C)))
				((eq f '<) (< (ske-interp (car arg) P C) (ske-interp (cadr arg) P C)))
				((eq f '=) (= (ske-interp (car arg) P C) (ske-interp (cadr arg) P C)))
				((eq f 'and) (and (ske-interp (car arg) P C) (ske-interp (cadr arg) P C)))
				((eq f 'or) (or (ske-interp (car arg) P C) (ske-interp (cadr arg) P C)))
				((eq f 'not) (not (ske-interp (car arg) P C)))   	
				
				;user-defined-function	
								
				((eq (null f_arg_body) nil)       
						( let ( (fargs (car f_arg_body)) 
							(fbody (caadr f_arg_body)) )
							
							; extend context
							(ske-interp fbody P (append (comb fargs (eval_arg arg P C)) C))	
						)
				)	
				(t E)
								
								
				)
			)
		)
	)

)

#|Final called function, start with an empty context|#
(defun fl-interp (E P)
	(ske-interp E P nil)
)
