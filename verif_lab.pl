:- begin_tests(yahtzee).

test(dado_lanzamiento1,[nondet]):-
    iniciar(42),
    lanzamiento([1,1,1,2,3],[0,0,0,1,1],[1,1,1,2,2]).

test(ob_puntaje1,[nondet]):-
    puntaje([2,1,2,3,5],aces,1).

test(ob_puntaje1,[nondet]):-
    puntaje([1,1,1,3,5],twos,0).

test(ob_puntaje2,[nondet]):-
    puntaje([2,3,2,3,5],threes,6).

test(ob_three_of_a_kind1,[nondet]):-
    puntaje([2,2,4,1,2],three_of_a_kind,11).

test(three_of_a_kind2,[nondet]):-
    puntaje([2,2,4,2,2],three_of_a_kind,12).

test(three_of_a_kind3,[nondet]):-
    puntaje([1,1,4,2,2],three_of_a_kind,0).

test(four_of_a_kind1,[nondet]):-
    puntaje([2,2,4,2,2],four_of_a_kind,12).

test(four_of_a_kind5,[nondet]):-
    puntaje([2,3,4,2,2],four_of_a_kind,0).

test(full_house3,[nondet]):-
    puntaje([2,3,2,3,3],full_house,25).
test(full_house,[nondet]):-
    puntaje([2,3,2,3,1],full_house,0).

test(full_house2,[nondet]):-
    puntaje([2,3,1,3,3],full_house,0).


test(small_straight,[nondet]):-
    puntaje([2,3,1,3,4],small_straight,30).
test(small_straight,[nondet]):-
    puntaje([1,4,1,3,4],small_straight,0).


test(small_straight2,[nondet]):-
    puntaje([2,3,1,5,4],small_straight,30).
test(small_straight2,[nondet]):-
    puntaje([1,3,1,5,4],small_straight,0).


test(large_straight,[nondet]):-
    puntaje([2,3,1,5,4],large_straight,40).
test(large_straight2,[nondet]):-
    puntaje([2,3,1,2,4],large_straight,0).

test(yahtzee,[nondet]):-
    puntaje([2,2,2,2,2],yahtzee,50).
test(yahtzee2,[nondet]):-
    puntaje([2,2,2,2,1],yahtzee,0).

test(chance,[nondet]):-
    puntaje([4,2,1,2,2],chance,11).


test(puntaje_tablero1,[nondet]):-
    puntaje_tablero([s(aces,4), s(twos,2),s(threes,9), s(fours,4), s(fives,10), s(sixes,18),
    s(three_of_a_kind,20), s(four_of_a_kind,22), s(full_house,0), s(small_straight,0), 
    s(large_straight,40), s(yahtzee,50),s(chance,10)],189).

test(puntaje_tablero2,[nondet]):-
    puntaje_tablero([s(aces,4), s(twos,2),s(threes,15), s(fours,16), s(fives,10), s(sixes,18),
    s(three_of_a_kind,20), s(four_of_a_kind,22), s(full_house,0), s(small_straight,0), 
    s(large_straight,40), s(yahtzee,50),s(chance,10)],242).

test(ajustar_tablero,[nondet]):-
    ajustar_tablero([s(aces,4), s(twos,2),s(threes,15), s(fours,16), s(fives,10), s(sixes,18),
    s(three_of_a_kind,20), s(four_of_a_kind,22), s(full_house,0), s(small_straight,0), 
    s(large_straight,40), s(yahtzee,50),s(chance,10)],full_house,15,
    [s(aces,4), s(twos,2),s(threes,15), s(fours,16), s(fives,10), s(sixes,18),
    s(three_of_a_kind,20), s(four_of_a_kind,22), s(full_house,15), s(small_straight,0), 
    s(large_straight,40), s(yahtzee,50),s(chance,10)]
    ).

:- end_tests(yahtzee).