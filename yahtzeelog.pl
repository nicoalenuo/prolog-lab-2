:- use_module(library(random)).
:- use_module(library(readutil)).
:- use_module(library(filesex)).

consultar_probabilidad_unica(Categoria, [D1, D2, D3, D4, D5], [X1, X2, X3, X4, X5], Probabilidad):-
    absolute_file_name(path(problog),Problog,[access(exist),extensions([exe])]),
    absolute_file_name(modelo,Modelo,[file_type(prolog)]),
    process_create(Problog, [Modelo, '-a', Categoria, '-a', D1, '-a', D2,'-a', D3,'-a', D4,'-a', D5, '-a', X1, '-a', X2, '-a', X3, '-a', X4, '-a', X5], [stdout(pipe(In))]),
    read_string(In, _, Result),
    split_string(Result,"\n\t","\r ",L),
    append([_, Y], [_], L),
    number_string(Probabilidad, Y).

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

% Verificar si hay una semi escalera pequeña (3 consecutivos), se usa en el cambiar dados de ia_det
tiene_semi_escalera_pequenia(Dados) :-
    sort_unicos(Dados, DadosUnicos),
    member(Sublista,[[1,2,3],[2,3,4],[3,4,5],[4,5,6]]),
    sublist(Sublista, DadosUnicos).

sublist([], _).
sublist([X|Xs], Ys) :-
    append(_, [X|Suffix], Ys),
    sublist(Xs, Suffix).
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

    flush_output(current_output), %falta ignorar el salto de linea (actualmente hay que apretar dos veces enter)
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
     mostrar_slots_disponibles(PuntajesCategoria, 1), nl,
     categorias_seccion_superior(CategoriasSuperior),
     member(m(X, Categoria), CategoriasSuperior),
     member(s(Categoria, PuntajeCat), PuntajesCategoria),
     PuntajeCat >= 3 * X,  % Le doy preferencia a la seccion superior por encima de three_of_a_kind y four_of_a_kind en caso de que hayan 3 del mismo valor
     \+ member(s(full_house, 25), PuntajesCategoria),
     \+ member(s(smaill_straight, 30), PuntajesCategoria),
     \+ member(s(large_straight, 40), PuntajesCategoria),
     \+ member(s(yahtzee, 50), PuntajesCategoria),
     !.

eleccion_slot(Dados, Tablero, ia_det, Categoria):- % Devuelvo la categoria que de el mayor puntaje en caso de que no se cumplan las condiciones anteriores
     map_puntajes(Tablero, Dados, PuntajesCategoria),
     ordenar_por_puntaje(PuntajesCategoria, [s(Categoria, _) | _]).

% eleccion_slot(Dados, Tablero, ia_prob, Categoria):-

% ---------------------------------------------------

cambio_dados(Dados, Tablero, ia_det, Patron) :-
    tiene_n_del_mismo_tipo(Dados,5,_),
    member(s(yahtzee,nil),Tablero),
    Patron = [0,0,0,0,0].

cambio_dados(_, _, humano, Patron)  :-
    elegir_patron(Patron, 5).

cambio_dados(Dados, Tablero, ia_det, Patron) :-
    tiene_n_del_mismo_tipo(Dados,3,Tipo),
    tiene_n_del_mismo_tipo(Dados,2,Tipo2),
    member(s(full_house,nil),Tablero),
    posicionesRepetido(Dados,Tipo,Patron1),
    posicionesRepetido(Dados,Tipo2,Patron2),
    unificadorPatrones(Patron1,Patron2,Patron).

cambio_dados(Dados, Tablero, ia_det, Patron) :-
    tiene_n_del_mismo_tipo(Dados,2,Tipo),
    tiene_n_del_mismo_tipo(Dados,2,Tipo2),
    Tipo =\= Tipo2,
    member(s(full_house,nil),Tablero),
    posicionesRepetido(Dados,Tipo,Patron1),
    posicionesRepetido(Dados,Tipo2,Patron2),
    unificadorPatrones(Patron1,Patron2,Patron).

cambio_dados(Dados, Tablero, ia_det, Patron) :-
    member(s(large_straight,nil),Tablero),
    tiene_escalera_grande(Dados),
    Patron = [0,0,0,0,0].

cambio_dados(Dados, Tablero, ia_det, Patron) :-
    member(s(large_straight,nil),Tablero),
    tiene_escalera_pequenia(Dados),
    sort_unicos(Dados,ValoresOrdenados),
    creadorPatronEscalera(Dados,ValoresOrdenados,Patron).

cambio_dados(Dados, Tablero, ia_det, Patron) :-
    member(s(small_straight,nil),Tablero),
    tiene_escalera_pequenia(Dados),
    sort_unicos(Dados,ValoresOrdenados),
    creadorPatronEscalera(Dados,ValoresOrdenados,Patron).

cambio_dados(Dados, Tablero, ia_det, Patron) :-
    member(s(small_straight,nil),Tablero),
    tiene_semi_escalera_pequenia(Dados),
    member(Sublista,[[3,4,5],[1,2,3],[2,3,4],[4,5,6]]),
    sublist(Sublista, Dados),
    creadorPatronEscalera(Dados,Sublista,Patron).

cambio_dados(Dados, Tablero, ia_det, Patron) :-
    tiene_n_del_mismo_tipo(Dados,5,Tipo),
    categorias_seccion_superior(Lista),
    member(m(Tipo,Cat),Lista),
    member(s(Cat,nil),Tablero),
    posicionesRepetido(Dados,Tipo,Patron).

cambio_dados(Dados, Tablero, ia_det, Patron) :-
    tiene_n_del_mismo_tipo(Dados,4,Tipo),
    categorias_seccion_superior(Lista),
    member(m(Tipo,Cat),Lista),
    member(s(Cat,nil),Tablero),
    posicionesRepetido(Dados,1,Patron).

cambio_dados(Dados, Tablero, ia_det, Patron) :-
    tiene_n_del_mismo_tipo(Dados,3,Tipo),
    categorias_seccion_superior(Lista),
    member(m(Tipo,Cat),Lista),
    member(s(Cat,nil),Tablero),
    posicionesRepetido(Dados,Tipo,Patron).

cambio_dados(Dados, Tablero, ia_det, Patron) :-
    tiene_n_del_mismo_tipo(Dados,2,Tipo),
    categorias_seccion_superior(Lista),
    member(m(Tipo,Cat),Lista),
    member(s(Cat,nil),Tablero),
    posicionesRepetido(Dados,Tipo,Patron).


cambio_dados(Dados, Tablero, ia_det, Patron) :-
    tiene_n_del_mismo_tipo(Dados,5,_),
    member(s(four_of_a_kind,nil),Tablero),
    Patron = [0,0,0,0,0].

cambio_dados(Dados, Tablero, ia_det, Patron) :-
    tiene_n_del_mismo_tipo(Dados,4,Tipo),
    member(s(four_of_a_kind,nil),Tablero),
    posicionesRepetido(Dados,Tipo,Patron).

cambio_dados(Dados, Tablero, ia_det, Patron) :-
    tiene_n_del_mismo_tipo(Dados,3,Tipo),
    member(s(four_of_a_kind,nil),Tablero),
    posicionesRepetido(Dados,Tipo,Patron).

cambio_dados(Dados, Tablero, ia_det, Patron) :-
    tiene_n_del_mismo_tipo(Dados,2,Tipo),
    member(s(four_of_a_kind,nil),Tablero),
    posicionesRepetido(Dados,Tipo,Patron).

cambio_dados(Dados, Tablero, ia_det, Patron) :-
    tiene_n_del_mismo_tipo(Dados,3,Tipo),
    member(s(four_of_a_kind,nil),Tablero),
    posicionesRepetido(Dados,Tipo,Patron).

cambio_dados(Dados, Tablero, ia_det, Patron) :-
    tiene_n_del_mismo_tipo(Dados,3,Tipo),
    member(s(three_of_a_kind,nil),Tablero),
    posicionesRepetido(Dados,Tipo,Patron).

cambio_dados(Dados, Tablero, ia_det, Patron) :-
    tiene_n_del_mismo_tipo(Dados,2,Tipo),
    member(s(three_of_a_kind,nil),Tablero),
    posicionesRepetido(Dados,Tipo,Patron).

cambio_dados(_, _, ia_det, [1,1,1,1,1]).

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

posicionesRepetido([],_,[]).
posicionesRepetido([Dado|RestoDados],Num,[N|RestoLista]) :-
    Dado =:= Num,
    N is 0,
    posicionesRepetido(RestoDados,Num,RestoLista).
posicionesRepetido([Dado|RestoDados],Num,[N|RestoLista]):-
    Dado =\= Num,
    N is 1,
    posicionesRepetido(RestoDados,Num,RestoLista).

unificadorPatrones([],[],[]).

unificadorPatrones([0|RestoPatron1],[_|RestoPatron2],[0|Patron]):-
    unificadorPatrones(RestoPatron1,RestoPatron2,Patron).

unificadorPatrones([_|RestoPatron1],[0|RestoPatron2],[0|Patron]):-
    unificadorPatrones(RestoPatron1,RestoPatron2,Patron).

unificadorPatrones([1|RestoPatron1],[1|RestoPatron2],[1|Patron]):-
    unificadorPatrones(RestoPatron1,RestoPatron2,Patron).


creadorPatronEscalera([], _, []).
creadorPatronEscalera([Dado|RestoDados], DatosUnicos, [0|Patron]):-
    member(Dado, DatosUnicos), % Comprueba si Dado está en DatosUnicos
    !,
    subtract(DatosUnicos, [Dado], NuevosDatosUnicos), % Elimina una ocurrencia de Dado de DatosUnicos
    creadorPatronEscalera(RestoDados, NuevosDatosUnicos, Patron).

creadorPatronEscalera([_|RestoDados], DatosUnicos, [1|Patron]):-
    creadorPatronEscalera(RestoDados, DatosUnicos, Patron).

mostrar_tablero([]) :- nl.
mostrar_tablero([s(Categoria, nil) | Resto]) :-
    write(' -'), write(Categoria), write(': _'), nl, % asignadon\'t
    mostrar_tablero(Resto).

mostrar_tablero([s(Categoria, Puntos) | Resto]) :-
    write(' -'), write(Categoria), write(': '), write(Puntos), write(' puntos.'), nl,
    mostrar_tablero(Resto).

% ------------------------------------------------

% eleccion_slot(Dados, Tablero, ia_prob, Categoria):-

% Se lo llama como patron(5,X). Retorna todas las combinaciones posibles de patrones
patron(1,[0]).
patron(1,[1]).
patron(K,[0|Resto]):-
    K > 1,
    N is K - 1,
    patron(N,Resto).
patron(K,[1|Resto]):-
    K > 1,
    N is K - 1,
    patron(N,Resto).
    
patron_por_numero([],_,[]).
patron_por_numero([Numero|Dados],Numero,[0|Patron]):-
    patron_por_numero(Dados,Numero,Patron).    
patron_por_numero([Dado|Dados],Numero,[1|Patron]):-
    Dado \= Numero,
    patron_por_numero(Dados,Numero,Patron).

    patron_generales(Dados, MinimoEsperado, Patrones) :-
        maximo_dado_repetido(Dados, Maximo, Cantidad),
        (MinimoEsperado < Cantidad ->
            findall(PatronesBase, buscoPatronesConMinimo(Dados, MinimoEsperado, Maximo, PatronesBase), Patrones),!
        ;
            findall(PatronesBase, buscoPatronesConMinimo(Dados, Cantidad, Maximo, PatronesBase), Patrones),!
        ).
    

buscoPatronesConMinimo([NumeroObj|Dados],MinimoEsperado,NumeroObj,[0|Patrones]):-
    MinimoEsperado > 0,
    NuevoMin is MinimoEsperado - 1,
    buscoPatronesConMinimo(Dados,NuevoMin,NumeroObj,Patrones).
buscoPatronesConMinimo([NumeroObj|Dados],0,NumeroObj,[0|Patrones]):-
    buscoPatronesConMinimo(Dados,0,NumeroObj,Patrones).
buscoPatronesConMinimo([NumeroObj|Dados],0,NumeroObj,[1|Patrones]):-
    buscoPatronesConMinimo(Dados,0,NumeroObj,Patrones).
buscoPatronesConMinimo([Dado|Dados],MinimoEsperado,NumeroObj,[1|Patrones]):-
    Dado \= NumeroObj,
    buscoPatronesConMinimo(Dados,MinimoEsperado,NumeroObj,Patrones).
buscoPatronesConMinimo([Dado|Dados],MinimoEsperado,NumeroObj,[0|Patrones]):-
    Dado \= NumeroObj,
    buscoPatronesConMinimo(Dados,MinimoEsperado,NumeroObj,Patrones).
buscoPatronesConMinimo([],_,_,[]).

maximo_dado_repetido(Dados,Maximo,Cantidad):-
    sort_unicos(Dados,Res),
    cant_por_grupo(Dados,Res,Lista),
    mejor_grupo(Lista,Maximo,Cantidad,0,0).

cant_por_grupo(Dados,[Res|Resto],[(Res,Count)|Lista]):-
    cant_por_numero(Dados,Res,Count),  
    %ACA SE PODRIA OPTIMIZAR UN POCO SI SACAMOS LOS DADOS QUE ACABAMOS DE CONTAR, NO TIENE SENTIDO DEJARLOS PORQUE YA CONTASMOS ESOS DADOS, AL SACARLOS, PARA LA SIGUIENTE ITERACION TENDRIAMOS MENOS NUMEROS
    cant_por_grupo(Dados,Resto,Lista).
cant_por_grupo(_,[],[]).

cant_por_numero([NumObj|Dados],NumObj,Conteo):-
    cant_por_numero(Dados,NumObj,ConteoAux),
    Conteo is ConteoAux + 1.
cant_por_numero([Dado|Dados],NumObj,Conteo):-
    Dado \= NumObj,
    cant_por_numero(Dados,NumObj,Conteo).
cant_por_numero([],_,0).

mejor_grupo([(Numero,Cant)|RestoLista],Maximo,Cantidad,AuxCant,AuxMax):-
    (Cant > AuxCant,mejor_grupo(RestoLista,Maximo,Cantidad,Cant,Numero),!);
    (Cant = AuxCant, Numero > AuxMax,mejor_grupo(RestoLista,Maximo,Cantidad,Cant,Numero),!).
mejor_grupo([(Numero,Cant)|RestoLista],Maximo,Cantidad,AuxCant,AuxMax):-
    (Cant < AuxCant,mejor_grupo(RestoLista,Maximo,Cantidad,AuxCant,AuxMax),!);
    (Cant = AuxCant, Numero < AuxMax,mejor_grupo(RestoLista,Maximo,Cantidad,AuxCant,AuxMax),!).
mejor_grupo([],Maximo,Cantidad,Cantidad,Maximo).


%dependiendo de la categoria y los dados, obtiene la lista de los "mejores" patrones
%por el momento, obtiene toda la lista de patrones posibles
obtener_patrones(Dados,Categoria,Patrones):-
    (categorias_seccion_superior(Lista), (m(Tipo,Categoria),Lista), patron_por_numero(Dados,Tipo,Patrones),!); %Patrones para las categorias superiores
    (Categoria = three_of_a_kind, patron_three(Dados,Patrones));


%sabiendo los dados, la categoría y la lista de patrones, retorna la suma de las probabilidades de los patrones y el mejor patron de dicha lista
acumular_probabilidad_categoria(_,_,[],ProbFinal,ProbFinal,PatronFinal,PatronFinal).
acumular_probabilidad_categoria(Dados,Categoria,[Patron|RestoPatrones],MejorProb,ProbFinal,MejorPatron,PatronFinal):-
     consultar_probabilidad_unica(Categoria,Dados,Patron,Prob), %funcion implementada en problog
     (
     Prob =< MejorProb,
     acumular_probabilidad_categoria(Dados,Categoria,RestoPatrones,MejorProb,ProbFinal,MejorPatron,PatronFinal);
     Prob > MejorProb,
     acumular_probabilidad_categoria(Dados,Categoria,RestoPatrones,Prob,ProbFinal,Patron,PatronFinal)
     ).

copiar([], []). % Caso base: una lista vacía se copia como una lista vacía
copiar([Lista1|RestoLista1], [Lista1|RestoLista2]) :-
    copiar(RestoLista1, RestoLista2). % Caso recursivo: copiar la cola de la lista

%sabiendo los dados y la categoria, retorna el mejor patron posible para dicha categoria
mejor_prob_categoria(Dados,Categoria,Patrones,MejorProb,MejorPatron,ProbAnterior,PatronAnterior) :-
    acumular_probabilidad_categoria(Dados,Categoria,Patrones,0,ProbFinal,_,PatronFinal),
    (
    ProbFinal =< ProbAnterior,
    MejorProb is ProbAnterior,
    copiar(MejorPatron,PatronAnterior);
    ProbFinal > ProbAnterior,
    MejorProb is ProbFinal,
    copiar(MejorPatron,PatronFinal)
    ).

%sabiendo los dados y las categorias restantes, retorna el mejor patron posible para todas las categorias
mejor_categoria(_,[],X,Y,X,Y).
mejor_categoria(Dados,[Categoria|RestoCategoria],MejorProbAnterior,MejorPatronAnterior,ProbFinal,PatronFinal):-  %MejorPatron no se está cargando pero MejorProb si? XD
    obtener_patrones(Dados,Categoria,Patrones),
    mejor_prob_categoria(Dados,Categoria,Patrones,MejorProb,_,MejorProbAnterior,MejorPatronAnterior),
    MejorProb =< MejorProbAnterior,
    mejor_categoria(Dados,RestoCategoria,MejorProbAnterior,MejorPatronAnterior,ProbFinal,PatronFinal).
mejor_categoria(Dados,[Categoria|RestoCategoria],MejorProbAnterior,MejorPatronAnterior,ProbFinal,PatronFinal):-
    obtener_patrones(Dados,Categoria,Patrones),
    mejor_prob_categoria(Dados,Categoria,Patrones,MejorProb,MejorPatron,MejorProbAnterior,MejorPatronAnterior),
    MejorProb > MejorProbAnterior,
    mejor_categoria(Dados,RestoCategoria,MejorProb,MejorPatron,ProbFinal,PatronFinal).

obtener_categorias_disponibles(Tablero,Categorias):-
     findall(Categoria, member(s(Categoria,nil),Tablero),Categorias).

% cambio_dados([1,1,2,3,4],[s(full_house,nil)],ia_prob,Patron).
cambio_dados(Dados, Tablero, ia_prob, Patron) :-
     obtener_categorias_disponibles(Tablero,Categorias),
     mejor_categoria(Dados,Categorias,0,_,_,Patron).
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
    estrategias(Ests),
    member(Estrategia, Ests),
    inicial(Tablero),
    iniciar(Seed),
    yahtzeelog(13, Estrategia, Tablero).

yahtzeelog(0, _, Tablero) :-
    puntaje_tablero(Tablero, Puntaje),
    write('Fin del Juego, se han conseguido '), write(Puntaje), write(' puntos.'),
    !.
yahtzeelog(Repetir, Estrategia, Tablero) :-
    lanzamiento(_, [1,1,1,1,1], Dados),
    write('Dados: '), write(Dados), nl,
    cambio_dados(Dados, Tablero, Estrategia, Patron1),
    lanzamiento(Dados, Patron1, NuevosDados1),
    write('Dados: '), write(NuevosDados1), nl,
    cambio_dados(NuevosDados1, Tablero, Estrategia, Patron2),
    lanzamiento(NuevosDados1, Patron2, NuevosDados2),
    write('Dados: '), write(NuevosDados2), nl,
    eleccion_slot(NuevosDados2, Tablero, Estrategia, CategoriaSlot),
    puntaje(NuevosDados2, CategoriaSlot, PuntosCategoriaSeleccionada),
    ajustar_tablero(Tablero, CategoriaSlot, PuntosCategoriaSeleccionada, NuevoTablero),
    NuevoRepetir is Repetir - 1,
    yahtzeelog(NuevoRepetir, Estrategia, NuevoTablero).

% ---------------------------------------


eleccion_slot_test(Dados, Tablero, ia_det, Categoria):-
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

%La diferencia entre test y no test es que no estan las salidas intermedias de antes y ahora solo hay unas salidas al final
eleccion_slot_test(Dados, Tablero, ia_det, Categoria):- % Devuelvo la categoria que de el mayor puntaje en caso de que no se cumplan las condiciones anteriores
     map_puntajes(Tablero, Dados, PuntajesCategoria),
     ordenar_por_puntaje(PuntajesCategoria, [s(Categoria, _) | _]).


yahtzeelog_test(0, _, Tablero, Tablero).
yahtzeelog_test(Repetir, Estrategia, Tablero, UltimoTablero) :-
    lanzamiento(_, [1,1,1,1,1], Dados),
    cambio_dados(Dados, Tablero, Estrategia, Patron1),
    lanzamiento(Dados, Patron1, NuevosDados1),
    cambio_dados(NuevosDados1, Tablero, Estrategia, Patron2),
    lanzamiento(NuevosDados1, Patron2, NuevosDados2),
    eleccion_slot_test(NuevosDados2, Tablero, Estrategia, CategoriaSlot),
    puntaje(NuevosDados2, CategoriaSlot, PuntosCategoriaSeleccionada),
    ajustar_tablero(Tablero, CategoriaSlot, PuntosCategoriaSeleccionada, NuevoTablero),
    NuevoRepetir is Repetir - 1,
    yahtzeelog_test(NuevoRepetir, Estrategia, NuevoTablero, UltimoTablero).

suma_desviacion([],_,Suma,Suma).
suma_desviacion([Puntaje|Puntajes],Media,Suma,SumaDesviacion) :-
    NuevaSuma is (Suma + ((Puntaje - Media) * (Puntaje - Media))),
    suma_desviacion(Puntajes,Media,NuevaSuma,SumaDesviacion).
    
% Si se quiere, se puede imprimir todos los datos que se quieran evaluar, no solo la media y la desviación, además podría ser el puntaje de alguna categoría en especifico, cuantas veces se obtiene el bonus, el max y min puntos alcanzado, etc.
test_masivo(_,_,0,PuntajeTotal,Desviacion) :-
    length(Desviacion, N),
    Media is PuntajeTotal / N,
    suma_desviacion(Desviacion,Media,0,SumaDesviacion),
    Suma is SumaDesviacion / N,
    DesviacionFinal is sqrt(Suma),
    write('La media es de '), write(Media),write(' puntos.'), nl,
    write('La desviacion es de '), write(DesviacionFinal), write(' puntos'),
    !.
test_masivo(Estrategia,Seed,Cantidad,PuntajeTotal,Desviacion):-
    estrategias(Ests),
    member(Estrategia, Ests),
    inicial(Tablero),
    iniciar(Seed),
    yahtzeelog_test(13,Estrategia,Tablero,UltimoTablero),
    puntaje_tablero(UltimoTablero, Puntaje),
    NuevoPuntajeTotal is PuntajeTotal + Puntaje,
    NuevoSeed is Seed + 1,
    NuevaCantidad is Cantidad - 1,
    append([Puntaje],Desviacion,NuevaDesviacion),
    test_masivo(Estrategia,NuevoSeed,NuevaCantidad,NuevoPuntajeTotal,NuevaDesviacion).

%realiza muchos tests para diferentes seed para Estrategia = [ia_det,ia_prob]
test_masivo(Estrategia,Cantidad):-
    test_masivo(Estrategia,1,Cantidad,0,[]).

% ---------------------------
% Verifica si hay alguna categoría sin completar (indicada por `nil`)
categorias_por_completar([s(_, nil) | _]) :- 
    !.  
categorias_por_completar([_ | RestoTablero]) :-
    categorias_por_completar(RestoTablero).

esperanza_sin_dados(Tablero, 0):-
    \+categorias_por_completar(Tablero),
    !.
esperanza_sin_dados(Tablero, Esperanza) :-
    lista_combinaciones_dados(Combinaciones),
    esperanza_sin_dados_sumatoria(Tablero, Combinaciones, Esperanza).

esperanza_sin_dados_sumatoria(_, [], 0):-
    !.
esperanza_sin_dados_sumatoria(Tablero, [Combinacion | RestoCombinaciones], Esperanza) :-
    % P = Probabilidad de pasar de no tener ningun dado a Combinacion, debería ser una constante, 1/(6^5)
    P is 1/(6^5),
    esperanza_sin_dados_sumatoria(Tablero, RestoCombinaciones, EsperanzaResto),
    esperanza_inicio(Tablero, Combinacion, 2, EsperanzaNueva),
    Esperanza is EsperanzaResto + P * EsperanzaNueva.

esperanza_inicio(Tablero, Dados, 0, Maximo):-
    map_puntajes(Tablero, Dados, PuntajesCategoria),
    esperanza_final(Tablero, Dados, PuntajesCategoria, 0, Maximo),
    !.

esperanza_inicio(Tablero, Dados, N, Maximo):-
    lista_rerolls(Rerolls),
    esperanza_maximo(Tablero, Dados, N, Rerolls, 0, Maximo).

esperanza_maximo(_, _, _, [], Maximo, Maximo):-
    !.
esperanza_maximo(Tablero, DadosOriginales, N, [Reroll | RestoRerolls], MaximoActual, Maximo) :-
    lista_combinaciones_dados(Combinaciones),
    esperanza_sumatoria(Tablero, DadosOriginales, Reroll, N, Combinaciones, Esperanza),
    MaximoNuevo is max(MaximoActual, Esperanza),
    esperanza_maximo(Tablero, DadosOriginales, N, RestoRerolls, MaximoNuevo, Maximo).

esperanza_sumatoria(_, _, _, _, [], 0) :-
    !.
esperanza_sumatoria(Tablero, DadosOriginales, Reroll, N, [Combinacion | RestoCombinaciones], Esperanza) :-
    % P = Probabilidad de pasar de DadosOriginales a Combinacion usando Reroll
    P is 0, % Este valor debería ajustarse según el cálculo real de probabilidad
    (P > 0 ->
    NAux is N - 1,
    esperanza_inicio(Tablero, Combinacion, NAux, EsperanzaNueva),
    esperanza_sumatoria(Tablero, DadosOriginales, Reroll, N, RestoCombinaciones, EsperanzaSumatoria),
    Esperanza is EsperanzaSumatoria + EsperanzaNueva * P
    ;    
    esperanza_sumatoria(Tablero, DadosOriginales, Reroll, N, RestoCombinaciones, EsperanzaSumatoria),
    Esperanza is EsperanzaSumatoria
    ).

esperanza_final(_, _, [], Maximo, Maximo):-
    !.
esperanza_final(Tablero, Dados, [s(Categoria, PuntajeCategoria) | RestoCategorias], MaximoActual, Maximo):-
    ajustar_tablero(Tablero, Categoria, PuntajeCategoria, TableroNuevo),
    esperanza_sin_dados(TableroNuevo, EsperanzaNueva),
    EsperanzaAux is PuntajeCategoria + EsperanzaNueva,
    MaximoNuevo is max(MaximoActual, EsperanzaAux),
    esperanza_final(Tablero, Dados, RestoCategorias, MaximoNuevo, Maximo).

:- dynamic lista_rerolls/1.
:- dynamic lista_combinaciones_dados/1.

% Predicado para generar todas las listas de longitud N con elementos de un conjunto dado
listas_con_elementos(0, _, []).
listas_con_elementos(N, Elementos, [X|Xs]) :-
    N > 0,
    member(X, Elementos),
    N1 is N - 1,
    listas_con_elementos(N1, Elementos, Xs).

generar_lista_rerolls :-
    findall(Lista, listas_con_elementos(5, [0, 1], Lista), Listas),
    retractall(lista_rerolls(_)),
    asserta((lista_rerolls(Listas))).

generar_lista_combinaciones_dados :-
    findall(Lista, listas_con_elementos(5, [1,2,3,4,5,6], Lista), Listas),
    retractall(lista_combinaciones_dados(_)),
    asserta((lista_combinaciones_dados(Listas))).

:- generar_lista_rerolls.
:- generar_lista_combinaciones_dados.
