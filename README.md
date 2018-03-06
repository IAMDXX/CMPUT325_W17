## CMPUT325_W17

## Ass#1




## Ass#2 Overview

A program P in FL is a list of function definitions. The FL interpreter takes such a program, together with a function application, and returns the result of evaluating the application. This evaluation is based on the principle of "replacing equals by equals". Your interpreter should be defined as a Lisp function

(fl-interp E P)

which, given a program P and an expression E, returns the result of evaluating E with respect to P.

The language FL includes a number of primitive functions that should be implemented in your interpreter. FL, as specified below, resembles a subset of Lisp without an environment -- FL does not have a special form "defun" to create permanent named functions.


## Ass#3 Overview
    1) xreverse(+L, ?R)
    2) xunique(+L, ?O)
    3) xdiff(+L1, +L2, -L)
    4) removeLast(+L, ?L1, ?Last)
    5) clique(L) :- findall(X,node(X),Nodes), xsubset(L,Nodes), allConnected(L).


## Ass#4 Overview
    1) Four Square
    2) War and Peace
