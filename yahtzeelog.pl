:- use_module(library(random)).
:- use_module(library(readutil)).

% Setea el estado inicial del generador de números aleatorios
iniciar(X):- set_random(seed(X)).

% Tabla con las trece categorías
categorias([aces,twos,threes,fours,fives,sixes,three_of_a_kind,four_of_a_kind,full_house,small_straight,large_straight,yahtzee,chance]).

% Tabla con las tres estrategias
estrategias([humano,ia_det,ia_prob]).

% Tablero inicial
inicial([s(aces,nil),s(twos,nil),s(threes,nil),s(fours,nil),s(fives,nil),s(sixes,nil),s(three_of_a_kind,nil),s(four_of_a_kind,nil),s(full_house,nil),s(small_straight,nil),s(large_straight,nil),s(yahtzee,nil),s(chance,nil)]).

% Tabla con las categorias separadas
% La seccion superior mapea las categorias con el numero representado
categorias_seccion_superior([m(1, aces), m(2, twos), m(3, threes), m(4, fours), m(5, fives), m(6 ,sixes)]).
categorias_seccion_inferior([three_of_a_kind, four_of_a_kind, full_house, small_straight, large_straight , yahtzee, chance]).

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

% -------------------------------------------

% Puntaje para cada categoria
puntaje(Dados, Cat, Puntaje) :-
    categorias_seccion_superior(CategoriasSuperior), % Obtengo las categorias de la seccion superior
    member(m(Num, Cat), CategoriasSuperior), % Chequeo si la categoria pasada como argumento es de la seccion superior
    contar(Dados, Num, Puntaje), % Cuento el puntaje obtenido por el numero correposndiente a la categoria
    !.

puntaje(Dados, three_of_a_kind, Puntaje) :-
    tiene_n_del_mismo_tipo(Dados, 3, _),
    sumar_lista(Dados, Puntaje),
    !.
puntaje(_, three_of_a_kind, 0).

puntaje(Dados, four_of_a_kind, Puntaje) :-
    tiene_n_del_mismo_tipo(Dados, 4, _),
    sumar_lista(Dados, Puntaje),
    !.
puntaje(_, four_of_a_kind, 0).

puntaje(Dados, full_house, 25) :-
    tiene_n_del_mismo_tipo(Dados, 2, X),
    tiene_n_del_mismo_tipo(Dados, 3, Y),
    X =\= Y,
    !.
puntaje(_, full_house, 0).

puntaje(Dados, small_straight, 30) :-
    tiene_escalera_pequenia(Dados),
    !.
puntaje(_, small_straight, 0).

puntaje(Dados, large_straight, 40) :-
    tiene_escalera_grande(Dados),
    !.
puntaje(_, large_straight, 0).

puntaje(Dados, yahtzee, 50) :-
    tiene_n_del_mismo_tipo(Dados, 5, _),
    !.
puntaje(_, yahtzee, 0).

puntaje(Dados, chance, Puntaje) :-
    sumar_lista(Dados, Puntaje).

% Contar los puntos para categorías de números específicos
contar([], _, 0).
contar([Dado | RestoDados], Num, Puntaje) :-
    Dado is Num,
    contar(RestoDados, Num, PuntajeAux),
    Puntaje is PuntajeAux + Dado,
    !.
contar([_ | RestoDados], Num, Puntaje):-
    contar(RestoDados, Num, Puntaje).

% Sumar los valores de la lista de dados
sumar_lista([], 0).
sumar_lista([Dado | RestoDados], Puntaje) :-
    sumar_lista(RestoDados, PuntajeAux),
    Puntaje is PuntajeAux + Dado.

% Verificar si hay al menos N dados del mismo tipo, Dado devuelve cual es el numero que lo cumple
tiene_n_del_mismo_tipo([Dado | RestoDados], N, Dado) :-
    N1 is N - 1,
    tiene_n_de_tipo(RestoDados, Dado, N1).
tiene_n_del_mismo_tipo([_ | RestoDados], N, Tipo):-
    tiene_n_del_mismo_tipo(RestoDados, N, Tipo).

% Verifica si tiene N de un tipo ESPECIFICO, NO es igual a tiene_n_del_mismo_tipo
tiene_n_de_tipo(_, _, 0) :- !.
tiene_n_de_tipo([Dado | RestoDados], Tipo, N):-
    Dado is Tipo,
    N1 is N - 1,
    tiene_n_de_tipo(RestoDados, Tipo, N1).
tiene_n_de_tipo([Dado | RestoDados], Tipo, N):-
    Dado \= Tipo,
    tiene_n_de_tipo(RestoDados, Tipo, N).

% Verificar si hay una escalera pequeña (4 consecutivos)
tiene_escalera_pequenia(Dados) :-
    sort_unicos(Dados, DadosUnicos),
    member(Escalera, [[1,2,3,4], [2,3,4,5], [3,4,5,6]]),
    append(_, X, DadosUnicos),
    append(Escalera, _, X).

% Verificar si hay una escalera grande (5 consecutivos)
tiene_escalera_grande(Dados) :-
    sort_unicos(Dados, DadosUnicos),
    member(DadosUnicos, [[1,2,3,4,5],[2,3,4,5,6]]).

% Hace sort de una lista, y devuelve la lista sin repeticiones
sort_unicos(List, Sorted) :-
    sort_unicos(List, [], Sorted).

sort_unicos([], Acc, Acc).
sort_unicos([H|T], Acc, Sorted) :-
    insert(H, Acc, NAcc),
    sort_unicos(T, NAcc, Sorted).

% Inserta de forma ordenada, en caso de que el elemento no este en la lista
insert(X, [], [X]).
insert(X, [Y|T], [X,Y|T]) :- X < Y.
insert(X, [Y|T], [Y|NT]) :- X > Y, insert(X, T, NT).
insert(X, [X|T], [X|T]).

%---------------------------------------------

puntaje_tablero_seccion_superior([s(sixes | Puntaje) | _], Puntaje) :- !.
puntaje_tablero_seccion_superior([s(_, PuntajeCategoria) | RestoTablero], Puntaje):-
    puntaje_tablero_seccion_superior(RestoTablero, PuntajeAux),
    Puntaje is PuntajeAux + PuntajeCategoria.

puntaje_tablero_total([], 0).
puntaje_tablero_total([s(_, PuntajeCategoria) | RestoTablero], Puntaje):-
    puntaje_tablero_total(RestoTablero, PuntajeAux),
    Puntaje is PuntajeAux + PuntajeCategoria.

puntaje_tablero(Tablero, Puntaje):-
    puntaje_tablero_seccion_superior(Tablero, Puntaje2), % Obtengo el puntaje de la seccion superior
    Puntaje2 >= 63, %Si es mayor o igual a 63, agrega 35 puntos
    puntaje_tablero_total(Tablero, Puntaje3),
    Puntaje is Puntaje3 + 35,
    !.
puntaje_tablero(Tablero, Puntaje):- % Si el puntaje de la seccion superior no es mayor a 63 puntos, no agrega los 35 puntos extra
    puntaje_tablero_total(Tablero, Puntaje).

%---------------------------------------------

ajustar_tablero([s(Categoria, _) | RestoTablero], Categoria, Puntaje, [s(Categoria, Puntaje) | RestoTablero]):-
    !.
ajustar_tablero([PuntajeCategoria | RestoTablero], Categoria, Puntaje, [PuntajeCategoria | TableroSalidaAux]):-
    ajustar_tablero(RestoTablero, Categoria, Puntaje, TableroSalidaAux).

%---------------------------------------------

% Mapeo todas las categorias aun no elegidas a sus respectivos puntajes
map_puntajes([], _, []).
map_puntajes([s(Categoria, nil) | RestoPuntajes ], Dados, [s(Categoria, PuntajeCategoria) | RestoPuntajesCategoria]):-
    puntaje(Dados, Categoria, PuntajeCategoria),
    map_puntajes(RestoPuntajes, Dados, RestoPuntajesCategoria),
    !.
map_puntajes([_ | RestoPuntajes], Dados, PuntajesCategoria):-
    map_puntajes(RestoPuntajes, Dados, PuntajesCategoria).

% Inserta de forma ordenada el puntaje
insertar_puntaje(s(Cat1, Puntaje1), [], [s(Cat1, Puntaje1)]).
insertar_puntaje(s(Cat1, Puntaje1), [s(Cat2, Puntaje2)|Resto], [s(Cat1, Puntaje1),s(Cat2, Puntaje2)|Resto]) :-
    Puntaje1 >= Puntaje2.
insertar_puntaje(s(Cat1, Puntaje1), [s(Cat2, Puntaje2)|Resto], [s(Cat2, Puntaje2)|R]) :-
    Puntaje1 < Puntaje2,
    insertar_puntaje(s(Cat1, Puntaje1), Resto, R).

% Ordena de mayor a menor los puntajes
ordenar_por_puntaje([], []).
ordenar_por_puntaje([X|Resto], Ordenada) :-
    ordenar_por_puntaje(Resto, OrdenadaResto),
    insertar_puntaje(X, OrdenadaResto, Ordenada).

% Muestra en pantalla las posibles categorias a elegir, junto con los puntos que sumaria si se eligiera
mostrar_slots_disponibles([],_).
mostrar_slots_disponibles([s(Categ, Puntos)|Resto], Num) :-
    write('  '), write(Num), write(' - '), write(Categ), write(' - '), write(Puntos), nl,
    Num2 is Num + 1, % incrementa el identificador (el indice en la lista impresa)
    mostrar_slots_disponibles(Resto, Num2).

eleccion_slot(Dados, Tablero, humano, Categoria) :-
    map_puntajes(Tablero, Dados, PuntajesCategoria), 
    write('Categorias por completar: [id - categoria - puntos que suma] '), nl,
    mostrar_slots_disponibles(PuntajesCategoria, 1), nl,

    write('Seleccione una categoria indicando el numero id.'), nl,
    flush_output(current_output),
    read_line_to_string(user_input, Entrada),

    length(PuntajesCategoria, CantDisponibles),
    (
        atom_number(Entrada, NroElegido),
        between(1, CantDisponibles, NroElegido),
        nth1(NroElegido, PuntajesCategoria, s(Categoria, _))
    ;   
        nl, write('Entrada invalida, por favor ingrese un numero entre el rango [1 - '), write(CantDisponibles), write('].'), nl, nl,
        eleccion_slot(Dados, Tablero, humano, Categoria)
    ).
    
eleccion_slot(Dados, Tablero, ia_det, Categoria):-
     map_puntajes(Tablero, Dados, PuntajesCategoria),
     categorias_seccion_superior(CategoriasSuperior),
     member(m(X, Categoria), CategoriasSuperior),
     member(s(Categoria, PuntajeCat), PuntajesCategoria),
     PuntajeCat >= 3 * X,  % Le doy preferencia a la seccion superior por encima de three_of_a_kind y four_of_a_kind en caso de que hayan 3 del mismo valor
     \+ member(s(full_house, 25), PuntajesCategoria),
     \+ member(s(smaill_straight, 30), PuntajesCategoria),
     \+ member(s(large_straight, 40), PuntajesCategoria),
     \+ member(s(yahtzee, 50), PuntajesCategoria),
     !.

eleccion_slot(Dados, Tablero, ia_det, Categoria):- % Devuelvo la categoria que de el mayor puntaje en caso de que no se cumplan las conidiciones anteriores
     map_puntajes(Tablero, Dados, PuntajesCategoria),
     ordenar_por_puntaje(PuntajesCategoria, [s(Categoria, _) | _]).

% eleccion_slot(Dados, Tablero, ia_prob, Categoria):-

% --------------------------------------------------

cambio_dados(_, _, humano, Patron)  :-
    elegir_patron(Patron, 5).

elegir_patron(_, 0).
elegir_patron([Patron|RestoPatron], Repetir) :-
    Repetir > 0,
    write('Ingresar un numero para cambiar el dado: [0 = Mantener] , [1 = Cambiar] '), nl,
    get_single_char(Char),
    atom_chars(Atom, [Char]),  % Convertir el carácter a un átomo
    (   atom_number(Atom, Patron), % Convertir el átomo a un número
        between(0, 1, Patron) -> % Verificar que el número esté en el rango [0, 1]
        NuevoRepetir is Repetir - 1,
        elegir_patron(RestoPatron, NuevoRepetir)
    ;   write('Entrada invalida, por favor ingrese un numero entre el rango [0-1]: '), nl,
        elegir_patron([Patron|RestoPatron], Repetir) % Llamada recursiva si la entrada no es válida
    ).

mostrar_tablero([]) :- nl.

mostrar_tablero([s(Categoria, nil) | Resto]) :-
    write(' -'), write(Categoria), write(': _'), nl, % asignadon\'t
    mostrar_tablero(Resto).

mostrar_tablero([s(Categoria, Puntos) | Resto]) :-
    write(' -'), write(Categoria), write(': '), write(Puntos), write(' puntos.'), nl,
    mostrar_tablero(Resto).

% ------------------------------------------------

% Se llama a yahtzee para jugar con un humano
yahtzee(humano, Seed):-
     inicial(Tablero), % Genero tablero inicial
     iniciar(Seed),    % Coloco semilla para numeros aleatorios
     yahtzee(13, Tablero).

yahtzee(0, Tablero) :-
    puntaje_tablero(Tablero, Puntaje),
    write('Fin del Juego, se han conseguido '), write(Puntaje), write(' puntos.'),
    !.
yahtzee(Repetir, Tablero) :-
    lanzamiento(_, [1,1,1,1,1], Dados),
    write('Dados: '), write(Dados), nl,
    cambio_dados(Dados, Tablero, humano, Patron1),
    lanzamiento(Dados, Patron1, NuevosDados1),
    write('Dados: '), write(NuevosDados1), nl,
    cambio_dados(NuevosDados1, Tablero, humano, Patron2),
    lanzamiento(NuevosDados1, Patron2, NuevosDados2),
    write('Dados: '), write(NuevosDados2), nl, nl,
    eleccion_slot(NuevosDados2, Tablero, humano, CategoriaSlot),
    puntaje(NuevosDados2, CategoriaSlot, PuntosCategoriaSeleccionada),
    ajustar_tablero(Tablero, CategoriaSlot, PuntosCategoriaSeleccionada, NuevoTablero),
    NuevoRepetir is Repetir - 1,
    
    % Luego de actualizar el tablero, se muestra en pantalla para que el jugador vea como va su partida y decida en la proxima ronda
    nl, write('Estado actual del tablero:'), nl,
    mostrar_tablero(NuevoTablero),
    
    yahtzee(NuevoRepetir, NuevoTablero).

% ---------------------------------------
% Es igual que yahtzee(humano) pero sin mostrar los dados en cada turno, no entiendo bien la diferencia entre ambos excepto por eso
% Pero la letra pide que hayan dos diferentes

% Se llama a yahtzeelog para jugar con un bot (ia_det o ia_prob)
yahtzeelog(Estrategia, Seed):-
    estrategias(ests),
    member(Estrategia, ests),
    inicial(Tablero),
    iniciar(Seed),
    yahtzee(13, Estrategia, Tablero).

yahtzeelog(0, _, Tablero) :-
    puntaje_tablero(Tablero, Puntaje),
    write('Fin del Juego, se han conseguido '), write(Puntaje), write(' puntos.'),
    !.
yahtzeelog(Repetir, Estrategia, Tablero) :-
    lanzamiento(_, [1,1,1,1,1], Dados),
    cambio_dados(Dados, Tablero, Estrategia, Patron1),
    lanzamiento(Dados, Patron1, NuevosDados1),
    cambio_dados(NuevosDados1, Tablero, Estrategia, Patron2),
    lanzamiento(NuevosDados1, Patron2, NuevosDados2),
    eleccion_slot(NuevosDados2, Tablero, Estrategia, CategoriaSlot),
    puntaje(NuevosDados2, CategoriaSlot, PuntosCategoriaSeleccionada),
    ajustar_tablero(Tablero, CategoriaSlot, PuntosCategoriaSeleccionada, NuevoTablero),
    NuevoRepetir is Repetir - 1,
    yahtzeelog(NuevoRepetir, Estrategia, NuevoTablero).
