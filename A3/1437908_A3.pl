/* 
----------------------------------- #1 xreverse(+L, ?R)------------------------------------

Define the predicate xreverse(+L, ?R) to reverse a list, where L is a given list and R is either a variable or another given list.

Examples: 
xreverse([7,3,4],[4,3,7]) should return true,
xreverse([7,3,4],[4,3,5]) should return false,
xreverse([7,3,4], R) should return R = [4,3,7].
---------------------------------------------------------------------------------------
append(l1,l2,l3): Succeeds if List3 is the result of appending List2 to List1. Unifies List3 to the result of appending List2 to List1 

base case: reverse a empty list [] is still a empty list.
--->append(N,[A],B) gives a reversed order of list. 
----------------------------------------------------------------------------------------
*/

xreverse([],[]).
xreverse([A|R],B):- xreverse(R,N),append(N,[A],B).

/*
----------------------------------- #2 xunique(+L, ?O) -------------------------------------

Define the predicate xunique(+L, ?O) where L is a given list of atoms and O is a copy of L where all the duplicates have been removed. O can be either a variable or a given list. The elements of O should be in the order in which they first appear in L.

Examples: 
xunique([a,c,a,d], O) should return O = [a,c,d], 
xunique([a,c,a,d], [a,c,d]) should return true, 
xunique([a,c,a,d], [c,a,d]) should return false (because of wrong order), 
xunique([a,a,a,a,a,b,b,b,b,b,c,c,c,c,b,a], O) should return O = [a,b,c], 
xunique([], O) should return O = [].

------------------------------------------------------------------------------------------
Helper function: notamembebr(list1,list2)-->true if for e in list1 is not a member of the list; false otherwise.
xunique(A,B,C), C is for storing the unique elemnts in A but not in B.
base case: [] has no unique element---> retuen []
append head of list1 in L if head is not a member of L, and passing L to to check the rest of the list1.  

----------------------------------------------------------------------------------------
*/

notamember(_,[]).
notamember(X,[Y|R]):- X \== Y, notamember(X,R). 

xunique([],X,X).
xunique([H|T],O,L):- member(H,L), xunique(T,O,L).
xunique([H|T],O,L):- notamember(H,L),append(L,[H],R),xunique(T,O,R).
xunique(A,B) :-xunique(A,B,[]).

/*
----------------------------------- #3 xdiff(+L1, +L2, -L)----------------------------------

Define the predicate xdiff(+L1, +L2, -L) where L1 and L2 are given lists of atoms, and L contains the elements that are contained in L1 but not L2 (set difference of L1 and L2). 

Examples: 

xdiff([a,b,f,c,d],[e,b,a,c],L) should return L=[f,d], 
xdiff([p,u,e,r,k,l,o,a,g],[n,k,e,a,b,u,t],L) should return L = [p,r,l,o,g], 
xdiff([],[e,b,a,c],L) should return L = [].
----------------------------------------------------------------------------------------
if L1 is [], return [] directly.
else, checking if the head of L1 is in L2, if the head is unique, the pass it to L, and also check if every element in L is unique by a helper function xunique.
----------------------------------------------------------------------------------------
*/

xdiff([],_,[]).
xdiff([A|R],B,L):- member(A,B), xdiff(R,B,L).
xdiff([A|R],B,L):- notamember(A,B),xunique(R,U),xdiff(U,B,N),append([A],N,L).

/*
------------------------------ #4 removeLast(+L, ?L1, ?Last)-------------------------------

Define the predicate removeLast(+L, ?L1, ?Last) where L is a given non-empty list, L1 is the result of removing the last element from L, and Last is that last element. L1 and Last can be either variables or given values.

Examples: 
removeLast([a,c,a,d], L1, Last) should return L1 = [a,c,a], Last = d, 
removeLast([a,c,a,d], L1, d) should return L1 = [a,c,a], 
removeLast([a,c,a,d], L1, [d]) should return false (why?), --->Last(== d) != [d]-->false
removeLast([a], L1, Last) should return L1 = [], Last = a, 
removeLast([[a,b,c]], L1, Last) should return L1 = [], Last = [a,b,c].
----------------------------------------------------------------------------------------
using xreverse(+L,?R) as helper function, the last element of L is the 1st element of its reversed list. ---> Last = F, and for the rest in reversed list, we need to transfer the order back to get L1.
*/

removeLast(L,L1,LAST):- xreverse(L,[F|R]),LAST=F,xreverse(R,L1).

/*
--------------------------------- #5 clique -------------------------------------------

The clique problem is a graph-theoretic problem of finding a subset of nodes where each node is connected to every other node in the subset. In this problem, a graph will be represented by a collection of predicates, node(A) and edge(A,B), where A and B are constants. Edges are undirected but only written once, so edge(A,B) also implies edge(B,A).

The set of nodes [a,b,c] is a clique, and so is every subset of it such as [a,c] or [b]. The set [a,d] and its subsets [a] and [d] are also a clique, etc. The empty set [] is also a clique.

To solve this problem, first, by using the built-in predicate findall (see lecture notes), one can find all nodes of a graph. Thus, the clique problem can be solved as follows.

clique(L) :- findall(X,node(X),Nodes), xsubset(L,Nodes), allConnected(L).

pre-defined xsubset,xappend
----------------------------------------------------------------------------------------
#5.1  allConnected(L)

define the predicate allConnected(L) to test if each node in L is connected to each other node in L. A node A is connected to another node B if either edge(A,B) or edge(B,A) is true.
----------------------------------------------------------------------------------------
*/

xsubset([], _).
xsubset([X|Xs], Set) :- xappend(_, [X|Set1], Set), xsubset(Xs, Set1).

xappend([], L, L).
xappend([H|T], L, [H|R]) :- xappend(T, L, R).

connected(_,[]).

% undirected edges ,need to check double sides.
connected(X,[Y|Z]):-edge(X,Y), connected(X,Z).
connected(X,[Y|Z]):-edge(Y,X), connected(X,Z).

% true for an empty list, L= []
allConnected([]).
% if A is connected to every node in L and allConnected(L)
allConnected([A|L]):- connected(A,L),allConnected(L).

clique(L) :- findall(X,node(X),Nodes), xsubset(L,Nodes), allConnected(L).

/*
----------------------------------------------------------------------------------------
#5.2  maxclique(+N, -Cliques)

To compute all the maximal cliques of size N. N is the given input, Cliques is the output: a list of cliques. A clique is maximal if there is no larger clique that contains it. In the example above, cliques [a,b,c] and [a,d] are maximal, but [a,b] is not, since it is contained in [a,b,c].

Examples (using the graph above): 
maxclique(2,Cliques) returns Cliques = [[a,d],[a,e]] 
maxclique(3,Cliques) returns Cliques = [[a,b,c]] 
maxclique(1,Cliques) returns Cliques = [] 
maxclique(0,Cliques) returns Cliques = []
----------------------------------------------------------------------------------------
helper function xlenclique(Num, L), longclique(N,L), returnsubset(A,L),xdiff(L1,L2,L3)
----------------------------------------------------------------------------------------
*/
% find clique of L and with length of N.
xlenclique(N,L) :- clique(L), length(L,N).

% find clique of L and with length Len bigger than N.
longclique(N,L) :- clique(L), length(L,Len), Len > N.

% if A is a subset of any element in List --> T 
returnsubset(A, [B|_]) :- xsubset(A, B).
returnsubset(A, [_|C]) :- returnsubset(A, C).

% base case: length of cliques must larger than 1

maxclique(0,[]).
maxclique(1,[]).
maxclique(N, Cliques) :- findall(X, xlenclique(N,X), L1), findall(X, longclique(N,X), L2) , findall(S, returnsubset(S, L2), L3), xdiff(L1, L3, Cliques).







