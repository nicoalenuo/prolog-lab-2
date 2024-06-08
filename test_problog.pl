1/6::dado1(1); 1/6::dado1(2); 1/6::dado1(3); 1/6::dado1(4); 1/6::dado1(5); 1/6::dado1(6).
1/6::dado2(1); 1/6::dado2(2); 1/6::dado2(3); 1/6::dado2(4); 1/6::dado2(5); 1/6::dado2(6).
1/6::dado3(1); 1/6::dado3(2); 1/6::dado3(3); 1/6::dado3(4); 1/6::dado3(5); 1/6::dado3(6).
1/6::dado4(1); 1/6::dado4(2); 1/6::dado4(3); 1/6::dado4(4); 1/6::dado4(5); 1/6::dado4(6).
1/6::dado5(1); 1/6::dado5(2); 1/6::dado5(3); 1/6::dado5(4); 1/6::dado5(5); 1/6::dado5(6).


tres_iguales([A, A, A, B, C]):- A\=B,A\=C.
tres_iguales([A, A, B, A, C]):- A\=B,A\=C.
tres_iguales([A, A, B, C, A]):- A\=B,A\=C.
tres_iguales([A, B, A, A, C]):- A\=B,A\=C.
tres_iguales([A, B, A, C, A]):- A\=B,A\=C.
tres_iguales([A, B, C, A, A]):- A\=B,A\=C.
tres_iguales([B, A, A, A, C]):- A\=B,A\=C.
tres_iguales([B, A, A, C, A]):- A\=B,A\=C.
tres_iguales([B, A, C, A, A]):- A\=B,A\=C.
tres_iguales([B, C, A, A, A]):- A\=B,A\=C.

tres_iguales_aux(X):-
    dado1(C1),
    dado2(C2),
    dado3(C3),
    dado4(C4),
    dado5(C5),
    tres_iguales([C1, C2, C3, C4, C5]),
    X = [C1, C2, C3, C4, C5].

