:- use_module(library(lists)).

1/6::dado1(1); 1/6::dado1(2); 1/6::dado1(3); 1/6::dado1(4); 1/6::dado1(5); 1/6::dado1(6).
1/6::dado2(1); 1/6::dado2(2); 1/6::dado2(3); 1/6::dado2(4); 1/6::dado2(5); 1/6::dado2(6).
1/6::dado3(1); 1/6::dado3(2); 1/6::dado3(3); 1/6::dado3(4); 1/6::dado3(5); 1/6::dado3(6).
1/6::dado4(1); 1/6::dado4(2); 1/6::dado4(3); 1/6::dado4(4); 1/6::dado4(5); 1/6::dado4(6).
1/6::dado5(1); 1/6::dado5(2); 1/6::dado5(3); 1/6::dado5(4); 1/6::dado5(5); 1/6::dado5(6).

% La implementacion se hace asi para reducir en gran medida la cantidad de chequeos ( y por lo tanto el tiempo de ejecucion )

three_of_a_kind:-
    (
    (dado1(C1), dado2(C1), dado3(C1));
    (dado1(C1), dado2(C1), dado4(C1));
    (dado1(C1), dado2(C1), dado5(C1));
    (dado1(C1), dado3(C1), dado4(C1));
    (dado1(C1), dado3(C1), dado5(C1));
    (dado1(C1), dado4(C1), dado5(C1));
    (dado2(C1), dado3(C1), dado4(C1));
    (dado2(C1), dado3(C1), dado5(C1));
    (dado2(C1), dado4(C1), dado5(C1));
    (dado3(C1), dado4(C1), dado5(C1))
    ).

four_of_a_kind:-
    (
    (dado1(C1), dado2(C1), dado3(C1), dado4(C1));
    (dado1(C1), dado2(C1), dado3(C1), dado5(C1));
    (dado1(C1), dado2(C1), dado4(C1), dado5(C1));
    (dado1(C1), dado3(C1), dado4(C1), dado5(C1));
    (dado2(C1), dado3(C1), dado4(C1), dado5(C1))
    ).

full_house:-
    (
    (dado1(C1), dado2(C1), dado3(C2), dado4(C2), dado5(C2));
    (dado1(C1), dado2(C2), dado3(C1), dado4(C2), dado5(C2));
    (dado1(C1), dado2(C2), dado3(C2), dado4(C1), dado5(C2));
    (dado1(C1), dado2(C2), dado3(C2), dado4(C2), dado5(C1));
    (dado1(C2), dado2(C1), dado3(C1), dado4(C2), dado5(C2));
    (dado1(C2), dado2(C1), dado3(C2), dado4(C1), dado5(C2));
    (dado1(C2), dado2(C1), dado3(C2), dado4(C2), dado5(C1));
    (dado1(C2), dado2(C2), dado3(C1), dado4(C1), dado5(C2));
    (dado1(C2), dado2(C2), dado3(C1), dado4(C2), dado5(C1));
    (dado1(C2), dado2(C2), dado3(C2), dado4(C1), dado5(C1))
    ),
    C1 \= C2.

    
small_straight:-
    (
    (dado1(1); dado2(1);dado3(1);dado4(1);dado5(1)),
    (dado1(2); dado2(2);dado3(2);dado4(2);dado5(2)),
    (dado1(3); dado2(3);dado3(3);dado4(3);dado5(3)),
    (dado1(4); dado2(4);dado3(4);dado4(4);dado5(4))
    );
    (
    (dado1(2); dado2(2);dado3(2);dado4(2);dado5(2)),
    (dado1(3); dado2(3);dado3(3);dado4(3);dado5(3)),
    (dado1(4); dado2(4);dado3(4);dado4(4);dado5(4)),
    (dado1(5); dado2(5);dado3(5);dado4(5);dado5(5))
    );
    (
    (dado1(3); dado2(3);dado3(3);dado4(3);dado5(3)),
    (dado1(4); dado2(4);dado3(4);dado4(4);dado5(4)),
    (dado1(5); dado2(5);dado3(5);dado4(5);dado5(5)),
    (dado1(6); dado2(6);dado3(6);dado4(6);dado5(6))
    ).

large_straight:-
    (
    (dado1(1); dado2(1);dado3(1);dado4(1);dado5(1)),
    (dado1(2); dado2(2);dado3(2);dado4(2);dado5(2)),
    (dado1(3); dado2(3);dado3(3);dado4(3);dado5(3)),
    (dado1(4); dado2(4);dado3(4);dado4(4);dado5(4)),
    (dado1(5); dado2(5);dado3(5);dado4(5);dado5(5))
    );
    (
    (dado1(2); dado2(2);dado3(2);dado4(2);dado5(2)),
    (dado1(3); dado2(3);dado3(3);dado4(3);dado5(3)),
    (dado1(4); dado2(4);dado3(4);dado4(4);dado5(4)),
    (dado1(5); dado2(5);dado3(5);dado4(5);dado5(5)),
    (dado1(6); dado2(6);dado3(6);dado4(6);dado5(6))   
    ).

yahtzee:-
    dado1(C1), dado2(C1), dado3(C1), dado4(C1), dado5(C1).

query(three_of_a_kind).