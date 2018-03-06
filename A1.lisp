#|QUESTION 1

(xmember X Y)
It returns T if argument Y is a member of the argument list X and NIL otherwise. 
This should also test for lists being members of lists. 
Both the list X and the argument Y may be NIL or lists containing NIL. 

Test case: 
>(xmember '((1) 2 3) '1)
NIL

>(xmember nil nil)
NIL

>(xmember '((1) 2 3) '(1))
T

|#

(defun xmember (x y) 
	(if (and (equal (car x) y) x)
		T
		(if (equal (cdr x) NIL) 
			NIL
			(xmember (cdr x) y)
		)
	)
)

#|QUESTION 2 

(flatten x)
the result of (flatten x) is just a list of atoms with the property 
that all the atoms appearing in x also appear in (flatten x) and in the same order.

comment: check (null x) and (atom x) at the begining

Test case:

>(flatten '(a (b c) d))
(a b c d)

>(flatten '((((a))))) 
(a)

>(flatten '(a (b c) (d ((e)) f)))
(a b c d e f)

|#

(defun flatten (x)
	(if (atom x)
	    (if (null x) x
		(list x)
		)
	    (append (flatten (car x)) (flatten (cdr x)))
	)

) 

#|QUESTION 3

(mix L2 L1) 
mixes the elements of L1 and L2 into a single list, by choosing elements from L1 and L2 alternatingly. 
If one list is shorter than the other, then append all elements from the longer list at the end.

Test cases:

>(mix '(d e f) '(a b c))
(a d b e c f)

>(mix '(d e f g h) '((a) (b c))) 
((a) d (b c) e f g h)

>(mix '(nil) '(1 2 3))
(1 nil 2 3)

|#

(defun mix (l1 l2)
	(cond   ((null l1) l2)
		((null l2) l1)
		(T (append (list (car l2)) (mix (cdr l2) l1)) )
	)
)

#|QUESTION 4

(split L) 
splits the elements of L into a list of two sublists (L1 L2), by putting elements from L into L1 and L2 alternatingly.

comment: split function will try to separate L into evenly spread L1 L2

Test cases:

>(split '(1 2 3 4 5 6))
((1 3 5) (2 4 6))

>(split '((a) (b c) (d e f) g h)) 
(((a) (d e f) h) ((b c) g))

>(split '())
(nil nil)

|#

(defun split (L)
    (if (null L) (list l l)
        (if (null (cdr l)) (list (list (car l)) nil)
		(list (append (list (car L)) (car (split (cddr L)))) (append (list (cadr L)) (cadr (split (cddr L)))))
	)
    )
)

#|QUESTION 5

#5.1  Let L1 and L2 be lists. Is it always true that (split (mix L2 L1)) returns the list (L1 L2)? If yes, give a proof. If no, describe exactly for which pairs of lists L1, L2 the result is different from (L1 L2).

No. 
If length(L1) is not equal to length(L2), (split (mix L2 L1)) will not return (L1 L2). Since what mix function does is merging L1 and L2 together, the length of L1 and L2 won't bring any concern. However, when we using split function to seprate a list, split function tends to seprate the list into two evenly spread lists.
for example, (split (mix '(a b) '(c d e f g))) will return ((c d e g) (a b f)) which does not match ((c d e f g) (a b)).

#5.2
Let L be a list. Is it always true that (mix (cadr (split L)) (car (split L))) returns L? If yes, give a proof. If no, describe exactly for which lists L the result is different from L.

YES.
Proof: Let L = (x1 y1 x2 y2 x3 y3 .....x(k-1) y(k-1)) ----> split(L) ----> ( (x1 x2 ....x(k-1)) (y1 y2 .... y(k-1)) )  
	Then (cadr (split (L)) ) ---> (y1 y2 ... y(k-1)) and (car (split (L))) ---> (x1 x2 .... x(k-1))
	Therefore  (mix (cadr (split L)) (car (split L))) ----> (mix (y1 y2 ... y(k-1)) (x1 x2 ... x(k-1))) ---> (x1 y1 x2 y2 ..... x(k-1) y(k-1)) ----> L

|#

#|QUESTION 6

(subsetsum S L)
Given a list of numbers L and a sum S, find a subset of the numbers in L that sums up to S. 
Each number in L can only be used once. 
The result should be a list (n1 n2 ... nk) such that n1 + n2 + ... nk = S and all numbers n1,...,nk are in L.

Note: 
1.The numbers n1,...,nk should be in the same order as they appear in L;  
2.The result should be nil if the problem has no solution.
3. The problem may have more than one solution. Any valid solution is OK. 
4. 1. The input list L contains only positive integers.The sum S will also be a positive integer.

Optimization:
1. checking if s is less than the smallest element in the list
2. checking if the sum of the subL (which contains all the element from L less than s)
is less than s

Test cases:

>(subsetsum 2 '(1 5 3))
nil

>(subsetsum 29 '(1 16 2 8 4))
(1 16 8 4)

>(subsetsum  17849968430  '(3053682294 989689934 2274318726 2295906206 460832278 3045242478 1751748646 1515966654 1430721462 2177852046 3190353094 3227033566 4051115862 2784992430 3881439078 3549318398 2881716470 213746894 3109001222 4271503390 3644709014 2673949422 46278310 2333524286 703448630 235986702 1953520454 750635102 1618012630 1709216046 2411648486 2860920190 2677728118 2498916686 995636870 3318026398 3994374934 4237729646 2647465254 2135790014))

|#

(defun mysum (L)
	(if (null L) 0
		(+ (car L) (mysum (cdr L)))
	)
) 


(defun subsetsum (s L)
	(cond   ( (null L) L)
		( (= s (car L)) (list (car L)) )
		( (< s (car (sort (copy-list L) #'<))) nil)
		( (< (mysum L) s) nil)
		( t (let ((e (subsetsum (- s (car L)) (cdr L))))
				(if e (cons (car L) e)
					(subsetsum s (cdr L))
				) 
			)    
		)
	)
)

