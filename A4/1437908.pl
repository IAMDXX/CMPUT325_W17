/* CMPUT 325 Assignment #4
Yiting Dong   1437908
*/

:- use_module(library(clpfd)).

/*------------------------------ Q1. Four Square -----------------------------
fourSquares(+N, [-S1, -S2, -S3, -S4]) 
Given any positive integer N greater than 0, 
it returns a list of non-negative integers [S1,S2,S3,S4] 
such that N = S1*S1 + S2*S2 + S3*S3 + S4*S4.
it is also required that S1 <= S2 <= S3 <= S4. Note that some of S1...S3 may be zero.

ex. 20 = 2*2 + 4*4 and 20 = 1*1 + 1*1 + 3*3 + 3*3
------------------------------------------------------------------------------*/

% fourSquares(0,[0, 0, 0, 0]).
% fourSquares(1,[0, 0, 0, 1]).
fourSquares(N,[S1,S2,S3,S4]) :-  
			[S1,S2,S3,S4] ins 0..N, 
			S1 #=< S2, S2 #=< S3, S3 #=< S4,
			N #= S1*S1 + S2*S2 + S3*S3 + S4*S4,
			label([S1,S2,S3,S4]).

/*------------------------------ Q2. War and Peace -----------------------------
disarm(+Adivisions, +Bdivisions,-Solution)
Each element of Solution represents one dismantlement,
where a dismantlement is a list containing two elements: 
	the 1st element is a list of country A's dismantlements; 
	the 2nd is a list of country B's dismantlements.
Such that sum(A'dismantlement) = sum(B's dismantlement)
Note: Start with small dismantlements first. 
	Make sure that the total strength of one month's dismantlement is 
	less than or equal to the total strength of next month's dismantlement
      If there is no solution, then your program should return false.

ex. Country A: 1, 3, 3, 4, 6, 10, 12  Country B: 3, 4, 7, 9, 16
    Solution = [[[1,3],[4]], [[3,4],[7]], [[12],[3,9]], [[6,10],[16]]]

----------------------- Helper Function: find_sol() ----------------------------------------
find_sol(L1,L2,LS,PSUM) : L1 and L2 are sorted list of +Adivisions and +Bdivisions;
			LS is a result list used to save results;
			PSUM is the sum of previous achieved result, which is used to order the result 					list in a ascending order.
Base case: if +Adivisions and +Bdivisions are both empty, the there is no result.

------------------------ Build-in Function -----------------------------------------
search(?Elem,?List1,?list2): List2 is a list of List1 with Elem removed.
append(L1,Temp,LS): LS is the concatenation of L1 and Temp.
msort(+L,-Sorted): Return Sorted, which is a sorted list of L without removing duplicates in L.
------------------------------------------------------------------------------*/

find_sol([],[],[],_).

find_sol(L1,L2,LS,PSUM):-
		select(A1,L1,RL1),select(A2,RL1,RRL1),select(B1,L2,RL2),
		A1+A2 #= B1,  B1 #>=PSUM,
		append([[[A1,A2],[B1]]],Temp,LS), find_sol(RRL1,RL2,Temp,B1),
		label([A1,A2,B1]).

% either the sum of two elements in A equal to one of the element in B 
% or the sum of two elements of B equal to one of the element in A.
 
find_sol(L1,L2,LS,PSUM):-
		select(A1,L1,RL1),select(B1,L2,RL2),select(B2,RL2,RRL2),
		B1+B2 #= A1, A1 #>=PSUM,
		append([[[A1],[B1,B2]]],Temp,LS), find_sol(RL1,RRL2,Temp,A1),
		label([A1,B1,B2]).

% stop when find the first solution.
disarm(A, B, SOL):-
	msort(A,LA), msort(B,LB),
	find_sol(LA,LB,SOL,0),!.


	 


