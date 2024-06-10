:- use_module(library(lists)).


1/6::dado(1, 1); 1/6::dado(1, 2); 1/6::dado(1, 3); 1/6::dado(1, 4); 1/6::dado(1, 5); 1/6::dado(1, 6).
1/6::dado(2, 1); 1/6::dado(2, 2); 1/6::dado(2, 3); 1/6::dado(2, 4); 1/6::dado(2, 5); 1/6::dado(2, 6).
1/6::dado(3, 1); 1/6::dado(3, 2); 1/6::dado(3, 3); 1/6::dado(3, 4); 1/6::dado(3, 5); 1/6::dado(3, 6).
1/6::dado(4, 1); 1/6::dado(4, 2); 1/6::dado(4, 3); 1/6::dado(4, 4); 1/6::dado(4, 5); 1/6::dado(4, 6).
1/6::dado(5, 1); 1/6::dado(5, 2); 1/6::dado(5, 3); 1/6::dado(5, 4); 1/6::dado(5, 5); 1/6::dado(5, 6).

% La implementacion se hace asi para reducir en gran medida la cantidad de chequeos ( y por lo tanto el tiempo de ejecucion )
probabilidad_de_Numero_Cant_veces(Numero, Cant) :-
    findall(D, dado(D, Numero), Ds),
    length(Ds, Cant).

three_of_a_kind(Tirada):-
    (
        (dado(1,C1), dado(2,C1), dado(3,C1),dado(4,C2),dado(5,C3)),Tirada = [C1,C1,C1,C2,C3];
        (dado(1,C2), dado(2,C1), dado(3,C1),dado(4,C1),dado(5,C3)),C1\=C2,Tirada = [C2,C1,C1,C1,C3];
        (dado(1,C2), dado(2,C3), dado(3,C1),dado(4,C1),dado(5,C1)),C1\=C2,C1\=C3,Tirada = [C2,C3,C1,C1,C1]
    ).

four_of_a_kind(Tirada):-
    (
        (dado(1,C1), dado(2,C1), dado(3,C1),dado(4,C1),dado(5,C2)),Tirada = [C1,C1,C1,C1,C2];
        (dado(1,C2), dado(2,C1), dado(3,C1),dado(4,C2),dado(5,C1)),C1\=C2,Tirada = [C1,C1,C1,C2,C1]
    ).

full_house:-
    (
    (dado(1,C1), dado(2,C1), dado(3,C2), dado(4,C2), dado(5,C2));
    (dado(1,C1), dado(2,C2), dado(3,C1), dado(4,C2), dado(5,C2));
    (dado(1,C1), dado(2,C2), dado(3,C2), dado(4,C1), dado(5,C2));
    (dado(1,C1), dado(2,C2), dado(3,C2), dado(4,C2), dado(5,C1));
    (dado(1,C2), dado(2,C1), dado(3,C1), dado(4,C2), dado(5,C2));
    (dado(1,C2), dado(2,C1), dado(3,C2), dado(4,C1), dado(5,C2));
    (dado(1,C2), dado(2,C1), dado(3,C2), dado(4,C2), dado(5,C1));
    (dado(1,C2), dado(2,C2), dado(3,C1), dado(4,C1), dado(5,C2));
    (dado(1,C2), dado(2,C2), dado(3,C1), dado(4,C2), dado(5,C1));
    (dado(1,C2), dado(2,C2), dado(3,C2), dado(4,C1), dado(5,C1))
    ),
    C1 \= C2.

    
small_straight:-
    (
    (dado(1,1); dado(2,1);dado(3,1);dado(4,1);dado(5,1)),
    (dado(1,2); dado(2,2);dado(3,2);dado(4,2);dado(5,2)),
    (dado(1,3); dado(2,3);dado(3,3);dado(4,3);dado(5,3)),
    (dado(1,4); dado(2,4);dado(3,4);dado(4,4);dado(5,4))
    );
    (
    (dado(1,2); dado(2,2);dado(3,2);dado(4,2);dado(5,2)),
    (dado(1,3); dado(2,3);dado(3,3);dado(4,3);dado(5,3)),
    (dado(1,4); dado(2,4);dado(3,4);dado(4,4);dado(5,4)),
    (dado(1,5); dado(2,5);dado(3,5);dado(4,5);dado(5,5))
    );
    (
    (dado(1,3); dado(2,3);dado(3,3);dado(4,3);dado(5,3)),
    (dado(1,4); dado(2,4);dado(3,4);dado(4,4);dado(5,4)),
    (dado(1,5); dado(2,5);dado(3,5);dado(4,5);dado(5,5)),
    (dado(1,6); dado(2,6);dado(3,6);dado(4,6);dado(5,6))
    ).

large_straight:-
    (
    (dado(1,1); dado(2,1);dado(3,1);dado(4,1);dado(5,1)),
    (dado(1,2); dado(2,2);dado(3,2);dado(4,2);dado(5,2)),
    (dado(1,3); dado(2,3);dado(3,3);dado(4,3);dado(5,3)),
    (dado(1,4); dado(2,4);dado(3,4);dado(4,4);dado(5,4)),
    (dado(1,5); dado(2,5);dado(3,5);dado(4,5);dado(5,5))
    );
    (
    (dado(1,2); dado(2,2);dado(3,2);dado(4,2);dado(5,2)),
    (dado(1,3); dado(2,3);dado(3,3);dado(4,3);dado(5,3)),
    (dado(1,4); dado(2,4);dado(3,4);dado(4,4);dado(5,4)),
    (dado(1,5); dado(2,5);dado(3,5);dado(4,5);dado(5,5)),
    (dado(1,6); dado(2,6);dado(3,6);dado(4,6);dado(5,6))   
    ).

yahtzee:-
    dado(1,C1), dado(2,C1), dado(3,C1), dado(4,C1), dado(5,C1).
