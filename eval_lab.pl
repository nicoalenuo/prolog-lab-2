:- use_module(library(random)).
:- use_module(library(filesex)).

tablero_test([s(aces,4), s(twos,2),s(threes,15), s(fours,16), s(fives,10), s(sixes,18),
    s(three_of_a_kind,20), s(four_of_a_kind,22), s(full_house,0), s(small_straight,0), 
    s(large_straight,40), s(yahtzee,50),s(chance,10)]).

% Setea el estado inicial del generador de números aleatorios
iniciar(X):- set_random(seed(X)).

% Tabla con las trece categorías
categorias([aces,twos,threes,fours,fives,sixes,three_of_a_kind,four_of_a_kind,full_house,small_straight,large_straight,yahtzee,chance]).

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

% Inicializo el tablero a partir de la lista de categorías
tablero_inicial([],[]).
tablero_inicial([Cat|Cats],[s(Cat,nil)|T1]):-
        tablero_inicial(Cats,T1).

% Jugador yahtzee
% Jugador puede ser humano o ia
yahtzeelog(Estrategia,Seed):-
    set_random(seed(Seed)),
    partida(Estrategia,TableroFinal),
    writeln('Termino el juego'),
    % Termina el juego, calculo los resultados.
    writeln(TableroFinal),
    puntaje_tablero(TableroFinal,PuntajeFinal),
    write('Puntaje obtenido:'),writeln(PuntajeFinal).

% Esto es simplemente para no utilizar ronda1 como sinónimo de juego
partida(ia_prob,TableroFinal):-
    categorias(C),
    tablero_inicial(C,Tablero),
    ronda(1,ia_prob,Tablero,TableroFinal).

% Ronda de juego
% NumRonda es el número de ronda
% Tablero es el Tablero hasta el momento
% TableroSalida es el Tablero una vez finalizada la ronda
ronda(L1,_,Tablero,Tablero):-
    categorias(C),
    length(C,L),
    L1 =:= L+1.

ronda(NumRonda,ia_prob,Tablero,TableroSalida):-
    categorias(C),length(C,L),
    NumRonda =< L,
    writeln('-----'),
    write('Ronda numero:'),
    writeln(NumRonda),
    writeln('Tablero actual:'),
    writeln(Tablero),
    lanzamiento([_,_,_,_,_],[1,1,1,1,1],Dados),
    write('Primer Lanzamiento:'),writeln(Dados),
    cambio_dados(Dados,Tablero,ia_prob,Patron),
    write('Patron sugerido:'),writeln(Patron),
    lanzamiento(Dados,Patron,Dados1),
    write('Segundo Lanzamiento:'),writeln(Dados1),
    cambio_dados(Dados1,Tablero,ia_prob,Patron1),
    write('Patron sugerido:'),writeln(Patron1),
    lanzamiento(Dados1,Patron1,Dados2),
    write('Tercer Lanzamiento:'),writeln(Dados2),
    eleccion_slot(Dados2,Tablero,ia_prob,Slot),
    write('Slot elegido:'),writeln(Slot),
    puntaje(Dados2,Slot,Punt),
    ajustar_tablero(Tablero,Slot,Punt,Tablero2),
    NumRonda1 is NumRonda +1, 
    writeln('Siguiente ronda...'),
    ronda(NumRonda1,ia_prob,Tablero2,TableroSalida).