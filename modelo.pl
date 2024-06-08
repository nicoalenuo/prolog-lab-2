:- use_module(library(lists)).
:- use_module(library(assert)).

1/6::dado1(1); 1/6::dado1(2); 1/6::dado1(3); 1/6::dado1(4); 1/6::dado1(5); 1/6::dado1(6).
1/6::dado2(1); 1/6::dado2(2); 1/6::dado2(3); 1/6::dado2(4); 1/6::dado2(5); 1/6::dado2(6).
1/6::dado3(1); 1/6::dado3(2); 1/6::dado3(3); 1/6::dado3(4); 1/6::dado3(5); 1/6::dado3(6).
1/6::dado4(1); 1/6::dado4(2); 1/6::dado4(3); 1/6::dado4(4); 1/6::dado4(5); 1/6::dado4(6).
1/6::dado5(1); 1/6::dado5(2); 1/6::dado5(3); 1/6::dado5(4); 1/6::dado5(5); 1/6::dado5(6).

full_house([A,A,A,B,B]) :- A \= B.
full_house([A,A,B,A,B]) :- A \= B.
full_house([A,B,A,A,B]) :- A \= B.
full_house([B,A,A,A,B]) :- A \= B.
full_house([A,A,B,B,A]) :- A \= B.
full_house([A,B,A,B,A]) :- A \= B.
full_house([B,A,A,B,A]) :- A \= B.
full_house([A,B,B,A,A]) :- A \= B.
full_house([B,A,B,A,A]) :- A \= B.
full_house([B,B,A,A,A]) :- A \= B.

escalera_pequenia(A) :- 
    sort(A, B),
    (B = [A2, B2, C2, D2, _]; B = [A2, B2, C2, D2]),
    ((A2 = 1, B2 = 2, C2 = 3, D2 = 4); (A2 = 2, B2 = 3, C2 = 4, D2 = 5); (A2 = 3, B2 = 4, C2 = 5, D2 = 6)).

escalera_grande([A, B, C, D, E]) :- 
    permutation([A, B, C, D, E], [1, 2, 3, 4, 5]); permutation([A, B, C, D, E], [2, 3, 4, 5, 6]).

yahtzee([A,A,A,A,A]).

me_da_fullhouse:-
    dados([A1, A2, A3, A4, A5]), cambios([X1, X2, X3, X4, X5]),
    (X1 = 1, dado1(C1); X1=0, C1 = A1),
    (X2 = 1, dado2(C2); X2=0, C2 = A2),
    (X3 = 1, dado3(C3); X3=0, C3 = A3),
    (X4 = 1, dado4(C4); X4=0, C4 = A4),
    (X5 = 1, dado5(C5); X5=0, C5 = A5),
    full_house([C1, C2, C3, C4, C5]).


me_da_escalera_pequenia:-
    dados([A1, A2, A3, A4, A5]), cambios([X1, X2, X3, X4, X5]),
    (X1 = 1, dado1(C1); X1=0, C1 = A1),
    (X2 = 1, dado2(C2); X2=0, C2 = A2),
    (X3 = 1, dado3(C3); X3=0, C3 = A3),
    (X4 = 1, dado4(C4); X4=0, C4 = A4),
    (X5 = 1, dado5(C5); X5=0, C5 = A5),
    escalera_pequenia([C1, C2, C3, C4, C5]).

me_da_escalera_grande:-
    dados([A1, A2, A3, A4, A5]), cambios([X1, X2, X3, X4, X5]),
    (X1 = 1, dado1(C1); X1=0, C1 = A1),
    (X2 = 1, dado2(C2); X2=0, C2 = A2),
    (X3 = 1, dado3(C3); X3=0, C3 = A3),
    (X4 = 1, dado4(C4); X4=0, C4 = A4),
    (X5 = 1, dado5(C5); X5=0, C5 = A5),
    escalera_grande([C1, C2, C3, C4, C5]).

me_da_yahtzee:-
    dados([A1, A2, A3, A4, A5]), cambios([X1, X2, X3, X4, X5]),
    (X1 = 1, dado1(C1); X1=0, C1 = A1),
    (X2 = 1, dado2(C2); X2=0, C2 = A2),
    (X3 = 1, dado3(C3); X3=0, C3 = A3),
    (X4 = 1, dado4(C4); X4=0, C4 = A4),
    (X5 = 1, dado5(C5); X5=0, C5 = A5),
    yahtzee([C1, C2, C3, C4, C5]).

:-
cmd_args([Categoria, D1, D2, D3, D4, D5, X1, X2, X3, X4, X5]), 
assertz(dados([D1, D2, D3, D4, D5])),
assertz(cambios([X1, X2, X3, X4, X5])),
(
(Categoria = 'full_house', assertz(query(me_da_fullhouse)));
(Categoria = 'small_straight', assertz(query(me_da_escalera_pequenia)));
(Categoria = 'large_straight', assertz(query(me_da_escalera_grande)));
(Categoria = 'yahtzee', assertz(query(me_da_yahtzee)))
).

