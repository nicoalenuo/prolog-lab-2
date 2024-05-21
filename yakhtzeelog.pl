:- use_module(library(random)).

% Setea el estado inicial del generador de números aleatorios
iniciar(X):- set_random(seed(X)).

% Tabla con las trece categorías
categorias([aces,twos,threes,fours,fives,sixes,three_of_a_kind,four_of_a_kind,full_house,small_straight,large_straight,yahtzee,chance]).

% Tablero inicial
inicial([s(aces,nil),s(twos,nil),s(threes,nil),s(fours,nil),s(fives,nil),s(sixes,nil),s(three_of_a_kind,nil),s(four_of_a_kind,nil),s(full_house,nil),s(small_straight,nil),s(large_straight,nil),s(yahtzee,nil),s(chance,nil)]).

% Lanza los dados, según el mapa que le pasamos en el segundo argumento
% Si en el mapa hay un 0, mantiene lo que había; de lo contrario, vuelve a lanzar ese dado
lanzamiento([],[],[]).
lanzamiento([X|T],[0|T1],[X|T2]):-
    lanzamiento(T,T1,T2).
lanzamiento([_|T],[1|T1],[X1|T2]):-
    tiro_dado(X1),
    lanzamiento(T,T1,T2).

% Lanza un dado
tiro_dado(X):-
    random(1,7,X).

% Puntaje para cada categoria
puntaje(Dados, aces, Puntaje) :-
    contar(Dados, 1, Puntaje).

puntaje(Dados, twos, Puntaje) :-
    contar(Dados, 2, Puntaje).

puntaje(Dados, threes, Puntaje) :-
    contar(Dados, 3, Puntaje).

puntaje(Dados, fours, Puntaje) :-
    contar(Dados, 4, Puntaje).

puntaje(Dados, fives, Puntaje) :-
    contar(Dados, 5, Puntaje).

puntaje(Dados, sixes, Puntaje) :-
    contar(Dados, 6, Puntaje).

puntaje(Dados, three_of_a_kind, Puntaje) :-
    tiene_n_del_mismo_tipo(Dados, 3, _), 
    sumar_lista(Dados, Puntaje),
    !.
puntaje(_, three_of_a_kind, 0).

puntaje(Dados, four_of_a_kind, Puntaje) :-
    tiene_n_del_mismo_tipo(Dados, 4, _), 
    sumar_lista(Dados, Puntaje).
puntaje(_, four_of_a_kind, 0).

puntaje(Dados, full_house, 25) :-
    tiene_full_house(Dados).
puntaje(_, full_house, 0).

puntaje(Dados, small_straight, 30) :-
    tiene_escalera_pequeña(Dados).
puntaje(_, small_straight, 0).

puntaje(Dados, large_straight, 40) :-
    tiene_escalera_grande(Dados).
puntaje(_, large_straight, 0).

puntaje(Dados, yahtzee, 50) :-
    tiene_n_del_mismo_tipo(Dados, 5, _).
puntaje(_, yahtzee, 0).

puntaje(Dados, chance, Puntaje) :-
    sumar_lista(Dados, Puntaje).

% Contar los puntos para categorías de números específicos
contar([], _, 0).
contar([Dado | RestoDados], Num, Puntaje) :-
    Dado is Num,
    contar(RestoDados, Num, PuntajeAux),
    Puntaje is PuntajeAux + Dado, !.
contar([_ | RestoDados], Num, Puntaje):-
    contar(RestoDados, Num, Puntaje).

% Sumar los valores de la lista de dados
sumar_lista([], 0).
sumar_lista([Dado | RestoDados], Puntaje) :-
    sumar_lista(RestoDados, PuntajeAux),
    Puntaje is PuntajeAux + Dado.

% Verificar si hay al menos N dados del mismo tipo
tiene_n_del_mismo_tipo([Dado | RestoDados], N, Dado) :-
    N1 is N - 1,
    tiene_n_de_tipo(RestoDados, Dado, N1), !.
tiene_n_del_mismo_tipo([_ | RestoDados], N, Tipo):-
    tiene_n_del_mismo_tipo(RestoDados, N, Tipo).

tiene_n_de_tipo(_, _, 0) :- !.
tiene_n_de_tipo([Dado | RestoDados], Tipo, N):-
    Dado is Tipo,
    N1 is N - 1,
    tiene_n_de_tipo(RestoDados, Tipo, N1), !.
tiene_n_de_tipo([_ | RestoDados], Tipo, N):-
    tiene_n_de_tipo(RestoDados, Tipo, N).

% Verificar si hay un full house
tiene_full_house(Dados) :-
    tiene_n_del_mismo_tipo(Dados, 2, X),
    tiene_n_del_mismo_tipo(Dados, 3, Y),
    X =\= Y.

% Verificar si hay una escalera pequeña (4 consecutivos)
tiene_escalera_pequeña(Dados) :-
    sort_unicos(Dados, DadosUnicos),
    (append(_, [1,2,3,4|_], DadosUnicos) ;
     append(_, [2,3,4,5|_], DadosUnicos) ;
     append(_, [3,4,5,6|_], DadosUnicos)).

% Verificar si hay una escalera grande (5 consecutivos)
tiene_escalera_grande(Dados) :-
    sort_unicos(Dados, DadosUnicos),
    (DadosUnicos = [1,2,3,4,5] ;
     DadosUnicos = [2,3,4,5,6]).

% Sort the list
sort_unicos(List, Sorted) :-
    sort_unicos(List, [], Sorted).

sort_unicos([], Acc, Acc).
sort_unicos([H|T], Acc, Sorted) :-
    insert(H, Acc, NAcc),
    sort_unicos(T, NAcc, Sorted).

insert(X, [], [X]).
insert(X, [Y|T], [X,Y|T]) :- X < Y.
insert(X, [Y|T], [Y|NT]) :- X > Y, insert(X, T, NT).
insert(X, [X|T], [X|T]).

puntaje_tablero_seccion_superior([s(sixes | Puntaje) | _], Puntaje) :- !.
puntaje_tablero_seccion_superior([s(_, PuntajeCategoria) | RestoTablero], Puntaje):-
    puntaje_tablero_seccion_superior(RestoTablero, PuntajeAux),
    Puntaje is PuntajeAux + PuntajeCategoria.

puntaje_tablero_total([], 0).
puntaje_tablero_total([s(_, PuntajeCategoria) | RestoTablero], Puntaje):-
    puntaje_tablero_total(RestoTablero, PuntajeAux),
    Puntaje is PuntajeAux + PuntajeCategoria.

puntaje_tablero(Tablero, Puntaje):-
    puntaje_tablero_seccion_superior(Tablero, Puntaje2),
    Puntaje2 > 63,
    puntaje_tablero_total(Tablero, Puntaje3),
    Puntaje is Puntaje3 + 35,
    !.
puntaje_tablero(Tablero, Puntaje):-
    puntaje_tablero_total(Tablero, Puntaje).
    
ajustar_tablero([s(Categoria, _) | RestoTablero], Categoria, Puntaje, TableroSalida):-
    TableroSalida = [s(Categoria, Puntaje) | RestoTablero],
    !.
ajustar_tablero([PuntajeCategoria | RestoTablero], Categoria, Puntaje, TableroSalida):-
    ajustar_tablero(RestoTablero, Categoria, Puntaje, TableroSalidaAux),
    TableroSalida = [PuntajeCategoria | TableroSalidaAux].





