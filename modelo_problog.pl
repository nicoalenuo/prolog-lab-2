:- use_module(library(lists)).
:- use_module(library(assert)).

caster(D, S):-
    (D = '1', S = 1);
    (D = '2', S = 2);
    (D = '3', S = 3);
    (D = '4', S = 4);
    (D = '5', S = 5);
    (D = '6', S = 6).

1/6::dado1(1); 1/6::dado1(2); 1/6::dado1(3); 1/6::dado1(4); 1/6::dado1(5); 1/6::dado1(6).
1/6::dado2(1); 1/6::dado2(2); 1/6::dado2(3); 1/6::dado2(4); 1/6::dado2(5); 1/6::dado2(6).
1/6::dado3(1); 1/6::dado3(2); 1/6::dado3(3); 1/6::dado3(4); 1/6::dado3(5); 1/6::dado3(6).
1/6::dado4(1); 1/6::dado4(2); 1/6::dado4(3); 1/6::dado4(4); 1/6::dado4(5); 1/6::dado4(6).
1/6::dado5(1); 1/6::dado5(2); 1/6::dado5(3); 1/6::dado5(4); 1/6::dado5(5); 1/6::dado5(6).

full_house([A,A,A,B,B]) :- between(1, 6, A), between(1, 6, B), A \= B.
full_house([A,A,B,A,B]) :- between(1, 6, A), between(1, 6, B), A \= B.
full_house([A,B,A,A,B]) :- between(1, 6, A), between(1, 6, B), A \= B.
full_house([B,A,A,A,B]) :- between(1, 6, A), between(1, 6, B), A \= B.
full_house([A,A,B,B,A]) :- between(1, 6, A), between(1, 6, B), A \= B.
full_house([A,B,A,B,A]) :- between(1, 6, A), between(1, 6, B), A \= B.
full_house([B,A,A,B,A]) :- between(1, 6, A), between(1, 6, B), A \= B.
full_house([A,B,B,A,A]) :- between(1, 6, A), between(1, 6, B), A \= B.
full_house([B,A,B,A,A]) :- between(1, 6, A), between(1, 6, B), A \= B.
full_house([B,B,A,A,A]) :- between(1, 6, A), between(1, 6, B), A \= B.

escalera_pequenia([A, B, C, D, E]) :- 
    between(1, 6, A), between(1, 6, B), between(1, 6, C), between(1, 6, D), between(1, 6, E),
    sort([A, B, C, D, E], [A2, B2, C2, D2, _]), ((A2 = 1, B2 = 2, C2 = 3, D2 = 4); (A2 = 2, B2 = 3, C2 = 4, D2 = 5); (A2 = 3, B2 = 4, C2 = 5, D2 = 6)).

yahtzee([A,A,A,A,A]) :- between(1, 6, A).

me_da_fullhouse([X1, X2, X3, X4, X5]):-
    cmd_args([Arg1, Arg2, Arg3, Arg4, Arg5]), caster(Arg1, A1), caster(Arg2, A2), caster(Arg3, A3), caster(Arg4, A4), caster(Arg5, A5),
    full_house([C1, C2, C3, C4, C5]),
    between(0, 1, X1), between(0, 1, X2), between(0, 1, X3), between(0, 1, X4), between(0, 1, X5),
    (X1 = 1, dado1(C1); X1=0, C1 = A1),
    (X2 = 1, dado2(C2); X2=0, C2 = A2),
    (X3 = 1, dado3(C3); X3=0, C3 = A3),
    (X4 = 1, dado4(C4); X4=0, C4 = A4),
    (X5 = 1, dado5(C5); X5=0, C5 = A5).

me_da_escalera_pequenia([X1, X2, X3, X4, X5]):-
    cmd_args([Arg1, Arg2, Arg3, Arg4, Arg5]), caster(Arg1, A1), caster(Arg2, A2), caster(Arg3, A3), caster(Arg4, A4), caster(Arg5, A5),
    escalera_pequenia([C1, C2, C3, C4, C5]),
    between(0, 1, X1), between(0, 1, X2), between(0, 1, X3), between(0, 1, X4), between(0, 1, X5),
    (X1 = 1, dado1(C1); X1=0, C1 = A1),
    (X2 = 1, dado2(C2); X2=0, C2 = A2),
    (X3 = 1, dado3(C3); X3=0, C3 = A3),
    (X4 = 1, dado4(C4); X4=0, C4 = A4),
    (X5 = 1, dado5(C5); X5=0, C5 = A5).

me_da_escalera_grande([X1, X2, X3, X4, X5]):-
    cmd_args([Arg1, Arg2, Arg3, Arg4, Arg5]), caster(Arg1, A1), caster(Arg2, A2), caster(Arg3, A3), caster(Arg4, A4), caster(Arg5, A5),
    (permutation([C1, C2, C3, C4, C5], [1, 2, 3, 4, 5]); permutation([C1, C2, C3, C4, C5], [2, 3, 4, 5, 6])),
    between(0, 1, X1), between(0, 1, X2), between(0, 1, X3), between(0, 1, X4), between(0, 1, X5),
    (X1 = 1, dado1(C1); X1=0, C1 = A1),
    (X2 = 1, dado2(C2); X2=0, C2 = A2),
    (X3 = 1, dado3(C3); X3=0, C3 = A3),
    (X4 = 1, dado4(C4); X4=0, C4 = A4),
    (X5 = 1, dado5(C5); X5=0, C5 = A5).

me_da_yahtzee([X1, X2, X3, X4, X5]):-
    cmd_args([Arg1, Arg2, Arg3, Arg4, Arg5]), caster(Arg1, A1), caster(Arg2, A2), caster(Arg3, A3), caster(Arg4, A4), caster(Arg5, A5),
    yahtzee([C1, C2, C3, C4, C5]),
    between(0, 1, X1), between(0, 1, X2), between(0, 1, X3), between(0, 1, X4), between(0, 1, X5),
    (X1 = 1, dado1(C1); X1=0, C1 = A1),
    (X2 = 1, dado2(C2); X2=0, C2 = A2),
    (X3 = 1, dado3(C3); X3=0, C3 = A3),
    (X4 = 1, dado4(C4); X4=0, C4 = A4),
    (X5 = 1, dado5(C5); X5=0, C5 = A5).


:-
cmd_args([Categoria, Arg1, Arg2, Arg3, Arg4, Arg5]), caster(Arg1, A1), caster(Arg2, A2), caster(Arg3, A3), caster(Arg4, A4), caster(Arg5, A5), assertz(args([A1, A2, A3, A4, A5])),
Categoria = 'escalera_pequenia',
(
(Categoria = 'yahtzee', assertz(query(me_da_yahtzee(X))));
(Categoria = 'escalera_grande', assertz(query(me_da_escalera_grande(X))));
(Categoria = 'escalera_pequenia', assertz(query(me_da_escalera_pequenia(X))));
(Categoria = 'fullhouse', assertz(query(me_da_fullhouse(X))))
).

