1/6::dado1(1); 1/6::dado1(2); 1/6::dado1(3); 1/6::dado1(4); 1/6::dado1(5); 1/6::dado1(6).
1/6::dado2(1); 1/6::dado2(2); 1/6::dado2(3); 1/6::dado2(4); 1/6::dado2(5); 1/6::dado2(6).
1/6::dado3(1); 1/6::dado3(2); 1/6::dado3(3); 1/6::dado3(4); 1/6::dado3(5); 1/6::dado3(6).
1/6::dado4(1); 1/6::dado4(2); 1/6::dado4(3); 1/6::dado4(4); 1/6::dado4(5); 1/6::dado4(6).
1/6::dado5(1); 1/6::dado5(2); 1/6::dado5(3); 1/6::dado5(4); 1/6::dado5(5); 1/6::dado5(6).

probabilidad_tirada5([A,B,C,D,E]) :- dado1(A), dado2(B), dado3(C), dado4(D), dado5(E).
probabilidad_tirada4([A,B,C,D])   :- dado1(A), dado2(B), dado3(C), dado4(D).
probabilidad_tirada3([A,B,C])     :- dado1(A), dado2(B), dado3(C).
probabilidad_tirada2([A,B])       :- dado1(A), dado2(B).
probabilidad_tirada1([A])         :- dado1(A).
/*
tres_iguales([A, A, A, B, C]):- A\=B,A\=C,B\=C.
tres_iguales([A, A, B, A, C]):- A\=B,A\=C,B\=C,writeln([A, A, B, A, C]).
tres_iguales([A, A, B, C, A]):- A\=B,A\=C,B\=C,writeln([A, A, B, C, A]).
tres_iguales([A, A, C, B, A]):- A\=B,A\=C,B\=C,writeln([A, A, C, B, A]).
tres_iguales([A, B, A, A, C]):- A\=B,A\=C,B\=C,writeln([A, B, A, A, C]).
tres_iguales([A, B, A, C, A]):- A\=B,A\=C,B\=C,writeln([A, B, A, C, A]).
tres_iguales([A, B, C, A, A]):- A\=B,A\=C,B\=C,writeln([A, B, C, A, A]).
tres_iguales([A, C, A, A, B]):- A\=B,A\=C,B\=C,writeln([A, C, A, A, B]).
tres_iguales([A, C, A, B, A]):- A\=B,A\=C,B\=C,writeln([A, C, A, B, A]).
tres_iguales([A, C, B, A, A]):- A\=B,A\=C,B\=C,writeln([A, C, B, A, A]).
tres_iguales([B, A, A, A, C]):- A\=B,A\=C,B\=C,writeln([B, A, A, A, C]).
tres_iguales([B, A, A, C, A]):- A\=B,A\=C,B\=C,writeln([B, A, A, C, A]).
tres_iguales([B, A, C, A, A]):- A\=B,A\=C,B\=C,writeln([B, A, C, A, A]).
tres_iguales([B, C, A, A, A]):- A\=B,A\=C,B\=C,writeln([B, C, A, A, A]).
tres_iguales([C, A, A, A, B]):- A\=B,A\=C,B\=C,writeln([C, A, A, A, B]).
tres_iguales([C, A, A, B, A]):- A\=B,A\=C,B\=C,writeln([C, A, A, B, A]).
tres_iguales([C, A, B, A, A]):- A\=B,A\=C,B\=C,writeln([C, A, B, A, A]).
tres_iguales([C, B, A, A, A]):- A\=B,A\=C,B\=C,writeln([C, B, A, A, A]).
*/

/*Tambien entran los casos donde hay FULL HOUSE*/
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


cuatro_iguales([A,A,A,A,B]):- A\=B.
cuatro_iguales([A,A,A,B,A]):- A\=B.
cuatro_iguales([A,A,B,A,A]):- A\=B.
cuatro_iguales([A,B,A,A,A]):- A\=B.
cuatro_iguales([B,A,A,A,A]):- A\=B.

cinco_iguales([A,A,A,A,A]).

caster(D,S):-
    (
        (D = '0',S is 0);
        (D = '1',S is 1);
        (D = '2',S is 2);
        (D = '3',S is 3);
        (D = '4',S is 4);
        (D = '5',S is 5)
    ).

solo_buenos([],[],[],0).
solo_buenos([1|RestoX],[Y|RestoY],[Y|RestoResultado],Cant):-
    solo_buenos(RestoX,RestoY,RestoResultado,Cant1),
    Cant is Cant1 + 1.
solo_buenos([0|RestoX],[Y|RestoY],RestoResultado,Cant):-
    solo_buenos(RestoX,RestoY,RestoResultado,Cant).

tres_iguales_aux([Ptr1, Ptr2, Ptr3, Ptr4, Ptr5],[Obj1, Obj2, Obj3, Obj4, Obj5],X):-
    (Ptr1 = 1, dado1(C1); Ptr1=0, C1 = Obj1),
    (Ptr2 = 1, dado2(C2); Ptr2=0, C2 = Obj2),
    (Ptr3 = 1, dado3(C3); Ptr3=0, C3 = Obj3),
    (Ptr4 = 1, dado4(C4); Ptr4=0, C4 = Obj4),
    (Ptr5 = 1, dado5(C5); Ptr5=0, C5 = Obj5),
    tres_iguales([C1, C2, C3, C4, C5]),
    X = [C1, C2, C3, C4, C5].

cuatro_iguales_aux([Ptr1, Ptr2, Ptr3, Ptr4, Ptr5], [Obj1, Obj2, Obj3, Obj4, Obj5],X):-
    (Ptr1 = 1, dado1(C1); Ptr1=0, C1 = Obj1),
    (Ptr2 = 1, dado2(C2); Ptr2=0, C2 = Obj2),
    (Ptr3 = 1, dado3(C3); Ptr3=0, C3 = Obj3),
    (Ptr4 = 1, dado4(C4); Ptr4=0, C4 = Obj4),
    (Ptr5 = 1, dado5(C5); Ptr5=0, C5 = Obj5),
    cuatro_iguales([C1, C2, C3, C4, C5]),
    X = [C1, C2, C3, C4, C5].

cinco_iguales_aux([Ptr1, Ptr2, Ptr3, Ptr4, Ptr5], [Obj1, Obj2, Obj3, Obj4, Obj5],X):-
    (Ptr1 = 1, dado1(C1); Ptr1=0, C1 = Obj1),
    (Ptr2 = 1, dado2(C2); Ptr2=0, C2 = Obj2),
    (Ptr3 = 1, dado3(C3); Ptr3=0, C3 = Obj3),
    (Ptr4 = 1, dado4(C4); Ptr4=0, C4 = Obj4),
    (Ptr5 = 1, dado5(C5); Ptr5=0, C5 = Obj5),
    cinco_iguales([C1, C2, C3, C4, C5]),
    X = [C1, C2, C3, C4, C5].


llamada(Z):-
    cmd_args([Patron1,Patron2,Patron3,Patron4,Patron5,DadoObj1,DadoObj2,DadoObj3,DadoObj4,DadoObj5]),
    caster(Patron1,Ptr1),
    caster(Patron2,Ptr2),
    caster(Patron3,Ptr3),
    caster(Patron4,Ptr4),
    caster(Patron5,Ptr5),
    caster(DadoObj1,Obj1),
    caster(DadoObj2,Obj2),
    caster(DadoObj3,Obj3),
    caster(DadoObj4,Obj4),
    caster(DadoObj5,Obj5),
    X = [Ptr1,Ptr2,Ptr3,Ptr4,Ptr5],
    Y = [Obj1,Obj2,Obj3,Obj4,Obj5],
    (
        tres_iguales_aux(X,Y,Z);
        cuatro_iguales_aux(X,Y,Z);
        cinco_iguales_aux(X,Y,Z)
    ).

    
query(llamada(Z)).