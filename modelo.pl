:- use_module(library(lists)).

1/6::dado1(1); 1/6::dado1(2); 1/6::dado1(3); 1/6::dado1(4); 1/6::dado1(5); 1/6::dado1(6).
1/6::dado2(1); 1/6::dado2(2); 1/6::dado2(3); 1/6::dado2(4); 1/6::dado2(5); 1/6::dado2(6).
1/6::dado3(1); 1/6::dado3(2); 1/6::dado3(3); 1/6::dado3(4); 1/6::dado3(5); 1/6::dado3(6).
1/6::dado4(1); 1/6::dado4(2); 1/6::dado4(3); 1/6::dado4(4); 1/6::dado4(5); 1/6::dado4(6).
1/6::dado5(1); 1/6::dado5(2); 1/6::dado5(3); 1/6::dado5(4); 1/6::dado5(5); 1/6::dado5(6).

es_full_house(Lista) :- 
    sort(Lista, [A,B]), 
    (count(Lista, A, 3); count(Lista, B, 3)).

count([], _, 0).
count([X|T], X, N) :- N1 is N - 1, count(T, X, N1).
count([Y|T], X, N) :- count(T, X, N).

es_small_straight(A) :- 
    sort(A, B),
    (B = [A2, B2, C2, D2, _]; B = [A2, B2, C2, D2]),
    ((A2 = 1, B2 = 2, C2 = 3, D2 = 4); (A2 = 2, B2 = 3, C2 = 4, D2 = 5); (A2 = 3, B2 = 4, C2 = 5, D2 = 6)).

es_large_straight([A, B, C, D, E]) :- 
    permutation([A, B, C, D, E], [1, 2, 3, 4, 5]); permutation([A, B, C, D, E], [2, 3, 4, 5, 6]).

es_yahtzee([A,A,A,A,A]).

full_house:-
    dado1(C1), dado2(C2), dado3(C3), dado4(C4), dado5(C5),
    es_full_house([C1, C2, C3, C4, C5]).

small_straight:-
    dado1(C1), dado2(C2), dado3(C3), dado4(C4), dado5(C5),
    es_small_straight([C1, C2, C3, C4, C5]).

large_straight:-
    dado1(C1), dado2(C2), dado3(C3), dado4(C4), dado5(C5),
    es_large_straight([C1, C2, C3, C4, C5]).

yahtzee:-
    dado1(C1), dado2(C2), dado3(C3), dado4(C4), dado5(C5),
    es_yahtzee([C1, C2, C3, C4, C5]).


