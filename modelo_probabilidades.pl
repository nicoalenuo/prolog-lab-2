:- use_module(library(assert)).

obtener_full_house(1) :-
    assertz((1/6::dado(1, Dado, 1); 1/6::dado(2, Dado, 1); 1/6::dado(3, Dado, 1); 1/6::dado(4, Dado, 1); 1/6::dado(5, Dado, 1); 1/6::dado(6, Dado, 1))),
    (   (dado(X, 1, 1), dado(X, 2, 1), dado(X, 3, 1), dado(Y, 4, 1), dado(Y, 5, 1), X \= Y);
        (dado(X, 1, 1), dado(X, 2, 1), dado(X, 4, 1), dado(Y, 3, 1), dado(Y, 5, 1), X \= Y);
        (dado(X, 1, 1), dado(X, 2, 1), dado(X, 5, 1), dado(Y, 3, 1), dado(Y, 4, 1), X \= Y);
        (dado(X, 1, 1), dado(X, 3, 1), dado(X, 4, 1), dado(Y, 2, 1), dado(Y, 5, 1), X \= Y);
        (dado(X, 1, 1), dado(X, 3, 1), dado(X, 5, 1), dado(Y, 2, 1), dado(Y, 4, 1), X \= Y);
        (dado(X, 1, 1), dado(X, 4, 1), dado(X, 5, 1), dado(Y, 2, 1), dado(Y, 3, 1), X \= Y);
        (dado(X, 2, 1), dado(X, 3, 1), dado(X, 4, 1), dado(Y, 1, 1), dado(Y, 5, 1), X \= Y);
        (dado(X, 2, 1), dado(X, 3, 1), dado(X, 5, 1), dado(Y, 1, 1), dado(Y, 4, 1), X \= Y);
        (dado(X, 2, 1), dado(X, 4, 1), dado(X, 5, 1), dado(Y, 1, 1), dado(Y, 3, 1), X \= Y);
        (dado(X, 3, 1), dado(X, 4, 1), dado(X, 5, 1), dado(Y, 1, 1), dado(Y, 2, 1), X \= Y)
    ).

obtener_full_house(CantidadLanzamientos) :-
    CantidadLanzamientos > 1,
    assertz((1/6::dado(1, Dado, CantidadLanzamientos); 1/6::dado(2, Dado, CantidadLanzamientos); 1/6::dado(3, Dado, CantidadLanzamientos); 1/6::dado(4, Dado, CantidadLanzamientos); 1/6::dado(5, Dado, CantidadLanzamientos); 1/6::dado(6, Dado, CantidadLanzamientos))),
    CantAux is CantidadLanzamientos - 1,
    (   (dado(X, 1, CantidadLanzamientos), dado(X, 2, CantidadLanzamientos), dado(X, 3, CantidadLanzamientos), dado(Y, 4, CantidadLanzamientos), dado(Y, 5, CantidadLanzamientos), X \= Y);
        (dado(X, 1, CantidadLanzamientos), dado(X, 2, CantidadLanzamientos), dado(X, 4, CantidadLanzamientos), dado(Y, 3, CantidadLanzamientos), dado(Y, 5, CantidadLanzamientos), X \= Y);
        (dado(X, 1, CantidadLanzamientos), dado(X, 2, CantidadLanzamientos), dado(X, 5, CantidadLanzamientos), dado(Y, 3, CantidadLanzamientos), dado(Y, 4, CantidadLanzamientos), X \= Y);
        (dado(X, 1, CantidadLanzamientos), dado(X, 3, CantidadLanzamientos), dado(X, 4, CantidadLanzamientos), dado(Y, 2, CantidadLanzamientos), dado(Y, 5, CantidadLanzamientos), X \= Y);
        (dado(X, 1, CantidadLanzamientos), dado(X, 3, CantidadLanzamientos), dado(X, 5, CantidadLanzamientos), dado(Y, 2, CantidadLanzamientos), dado(Y, 4, CantidadLanzamientos), X \= Y);
        (dado(X, 1, CantidadLanzamientos), dado(X, 4, CantidadLanzamientos), dado(X, 5, CantidadLanzamientos), dado(Y, 2, CantidadLanzamientos), dado(Y, 3, CantidadLanzamientos), X \= Y);
        (dado(X, 2, CantidadLanzamientos), dado(X, 3, CantidadLanzamientos), dado(X, 4, CantidadLanzamientos), dado(Y, 1, CantidadLanzamientos), dado(Y, 5, CantidadLanzamientos), X \= Y);
        (dado(X, 2, CantidadLanzamientos), dado(X, 3, CantidadLanzamientos), dado(X, 5, CantidadLanzamientos), dado(Y, 1, CantidadLanzamientos), dado(Y, 4, CantidadLanzamientos), X \= Y);
        (dado(X, 2, CantidadLanzamientos), dado(X, 4, CantidadLanzamientos), dado(X, 5, CantidadLanzamientos), dado(Y, 1, CantidadLanzamientos), dado(Y, 3, CantidadLanzamientos), X \= Y);
        (dado(X, 3, CantidadLanzamientos), dado(X, 4, CantidadLanzamientos), dado(X, 5, CantidadLanzamientos), dado(Y, 1, CantidadLanzamientos), dado(Y, 2, CantidadLanzamientos), X \= Y);
        obtener_full_house(CantAux)
    ),

query(obtener_full_house(2)).