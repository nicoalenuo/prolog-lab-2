:- use_module(library(filesex)).
:- use_module(library(persistency)).

categorias_seccion_superior([m(1, aces), m(2, twos), m(3, threes), m(4, fours), m(5, fives), m(6 ,sixes)]).

 % Guardamos las probabilidades de obtener full_house, small_straight, large_straight, yahtzee al cambiar todos los dados
 % Puesto que son las que mas tiempo llevan calcular, y no varian segun los valores de los dados
:- dynamic probabilidad_al_cambiar_todos_los_dados/2.
 
% Ademas, guardamos todas las consultas hechas, para no tener que volver a calcularlas
:- dynamic probabilidad_consulta/4.

% Categorias que no interesa la probabilidad de cada posible resultado
categoria_probabilidad_unica(full_house).
categoria_probabilidad_unica(small_straight).
categoria_probabilidad_unica(large_straight).
categoria_probabilidad_unica(yahtzee).

% Llamadas a problog
% --------------------------------

% Para seccion superior
consultar_probabilidad_categoria_superior(Modelo, Probabilidad):-
    absolute_file_name(path(problog), Problog, [access(exist), extensions([exe])]),
    process_create(Problog, [Modelo], [stdout(pipe(In))]),
    read_string(In, _, Result),
    split_string(Result,"probabilidad_de_X_N_veces(,):\n\t", "probabilidad_de_X_N_veces(,):\n\t",L),
    lista_valores(L, Probabilidad).

lista_valores([X,Y,Z|T],[M|T1]):-
    M = (X,Y,Z),
    lista_valores2(T,T1).
lista_valores([],[]).

% Para full_house, small_straight, large_straight, yahtzee
consultar_probabilidad(Modelo, Probabilidad):-
    absolute_file_name(path(problog), Problog, [access(exist), extensions([exe])]),
    process_create(Problog, [Modelo], [stdout(pipe(In))]),
    read_string(In, _, Result),
    split_string(Result, "\n\t", "\r ", L),
    append([_, Y], [_], L),
    number_string(Probabilidad, Y).

% Para three_of_a_kind, four_of_a_kind
consultar_probabilidades(Modelo, Probabilidades):-
    absolute_file_name(path(problog), Problog, [access(exist), extensions([exe])]),
    process_create(Problog, [Modelo], [stdout(pipe(In))]),
    read_string(In, _, Result),
    split_string(Result, "\n\t", "\r ", L),
    append(L1,[_],L),
    parsear_probabilidades(L1, Probabilidades).

parsear_probabilidades([X, Y | T], [TermValor | T1]) :-
    split_string(X, "(", "):", [_, X1]),
    string_to_atom(X1, Atom),
    atom_to_term(Atom, ValoresStr, _),
    number_string(NumberY, Y),
    TermValor = probabilidad(ValoresStr, NumberY),
    lista_valores(T, T1).
parsear_probabilidades([], []).

% ---------------------------------

% Genera las evidencias que luego serán enviadas
conseguir_evidencias([0|RestoPatron], [Dado|RestoDados], N, [Evidencia|RestoEvidencias]):-
    (N =:= 1, Evidencia = evidence(dado(1,Dado), true), N1 is N + 1, conseguir_evidencias(RestoPatron, RestoDados, N1, RestoEvidencias), !);
    (N =:= 2, Evidencia = evidence(dado(2,Dado), true), N1 is N + 1, conseguir_evidencias(RestoPatron, RestoDados, N1, RestoEvidencias), !);
    (N =:= 3, Evidencia = evidence(dado(3,Dado), true), N1 is N + 1, conseguir_evidencias(RestoPatron, RestoDados, N1, RestoEvidencias), !);
    (N =:= 4, Evidencia = evidence(dado(4,Dado), true), N1 is N + 1, conseguir_evidencias(RestoPatron, RestoDados, N1, RestoEvidencias), !);
    (N =:= 5, Evidencia = evidence(dado(5,Dado), true), N1 is N + 1, conseguir_evidencias(RestoPatron, RestoDados, N1, RestoEvidencias), !).
conseguir_evidencias([1|RestoPatron], [_|RestoDados], N, Evidencias):-
    N1 is N + 1,
    conseguir_evidencias(RestoPatron, RestoDados, N1, Evidencias),
    !.
conseguir_evidencias([], [], _, []).

% Envia las evidencias al archivo
enviar_evidencias([], _).
enviar_evidencias([Evidencia|Evidencias], Stream):-
    writeln(Stream, ''),
    write(Stream, Evidencia),
    writeln(Stream, '.'),
    enviar_evidencias(Evidencias, Stream).

% Para seccion superior
enviar_query(Categoria, Stream):-
    categorias_seccion_superior(Lista),
    member(m(Tipo,Categoria),Lista),
    
    writeln(Stream, ''),

    write(Stream, 'query(probabilidad_de_X_N_veces('),
    write(Stream, Tipo),
    writeln(Stream, ',1)).'),

    write(Stream, 'query(probabilidad_de_X_N_veces('),
    write(Stream, Tipo),
    writeln(Stream, ',2)).'),

    write(Stream, 'query(probabilidad_de_X_N_veces('),
    write(Stream, Tipo),
    writeln(Stream, ',3)).'),
    
    write(Stream, 'query(probabilidad_de_X_N_veces('),
    write(Stream, Tipo),
    writeln(Stream, ',4)).'),

    write(Stream, 'query(probabilidad_de_X_N_veces('),
    write(Stream, Tipo),
    writeln(Stream, ',5)).').

% Envia la query al archivo, se usa para full_house, small_straight, large_straight, yahtzee
enviar_query(Categoria, Stream):-
    writeln(Stream, ''),
    write(Stream, 'query('),
    write(Stream, Categoria),
    writeln(Stream, ').').

% Envia la query al archivo, agrega un parametro para que devuelva las combinaciones y sus probabilidades
% Se usa para three_of_a_kind, four_of_a_kind
enviar_query_para_diferentes_combinaciones(Categoria, Stream):-
    writeln(Stream, ''),
    write(Stream, 'query('),
    write(Stream, Categoria),
    writeln(Stream, '(D)).').

% Consulto si ya tengo guardada la probabilidad al cambiar todos los dados
probabilidad(Categoria, _, [1,1,1,1,1], Probabilidad):-
    probabilidad_al_cambiar_todos_los_dados(Categoria, Probabilidad),
    !.

% Consulto si ya tengo guardada la consulta, asi se evita volver a calcularla
probabilidad(Categoria, Dados, Patron, Probabilidad):-
    probabilidad_consulta(Categoria, Dados, Patron, Probabilidad),
    !.

% Si calcula la probabilidad al cambiar todos los dados, la guarda
% Solo aplica a full_house, small_straight, large_straight, yahtzee
probabilidad(Categoria, Dados, Patron, Probabilidad):-
    Patron = [1,1,1,1,1],
    categoria_probabilidad_unica(Categoria),
    conseguir_evidencias(Patron, Dados, 1, Evidencias),
    absolute_file_name(modelo, Modelo, [file_type(prolog)]),
    tmp_file_stream(text, TmpFile, Stream),
    open(Modelo, read, ModeloStream),
    copy_stream_data(ModeloStream, Stream),
    close(ModeloStream),
    enviar_evidencias(Evidencias, Stream),
    enviar_query(Categoria, Stream),
    close(Stream),
    consultar_probabilidad(TmpFile, Probabilidad),
    asserta((probabilidad_al_cambiar_todos_los_dados(Categoria,Probabilidad))),
    asserta((probabilidad_consulta(Categoria, Dados, Patron, Probabilidad))),
    !.

% CASO CATEGORIA = FULL_HOUSE, SMALL_STRAIGHT, LARGE_STRAIGHT, YAHTZEE
probabilidad(Categoria, Dados, Patron, Probabilidad):-
    categoria_probabilidad_unica(Categoria),
    conseguir_evidencias(Patron, Dados, 1, Evidencias),
    absolute_file_name(modelo, Modelo, [file_type(prolog)]),
    tmp_file_stream(text, TmpFile, Stream),
    open(Modelo, read, ModeloStream),
    copy_stream_data(ModeloStream, Stream),
    close(ModeloStream),
    enviar_evidencias(Evidencias, Stream),
    enviar_query(Categoria, Stream),
    close(Stream),
    consultar_probabilidad(TmpFile, Probabilidad),
    asserta((probabilidad_consulta(Categoria, Dados, Patron, Probabilidad))),
    !.

% CASO CATEGOIRA SECCION SUPERIOR
probabilidad(Categoria, Dados, Patron, Probabilidad):-
    categorias_seccion_superior(Lista),
    member(m(_, Categoria), Lista),
    conseguir_evidencias(Patron, Dados, 1, Evidencias),
    absolute_file_name(modelo, Modelo, [file_type(prolog)]),
    tmp_file_stream(text, TmpFile, Stream),
    open(Modelo, read, ModeloStream),
    copy_stream_data(ModeloStream, Stream),
    close(ModeloStream),
    enviar_evidencias(Evidencias, Stream),
    enviar_query(Categoria, Stream),
    close(Stream),
    consultar_probabilidad_categoria_superior(TmpFile, Probabilidad),
    asserta((probabilidad_consulta(Categoria, Dados, Patron, Probabilidad))),
    !.

% CASO CATEGORIA = THREE_OF_A_KIND, FOUR_OF_A_KIND
probabilidad(Categoria, Dados, Patron, Probabilidad):-
    conseguir_evidencias(Patron, Dados, 1, Evidencias),
    absolute_file_name(modelo, Modelo, [file_type(prolog)]),
    tmp_file_stream(text, TmpFile, Stream),
    open(Modelo, read, ModeloStream),
    copy_stream_data(ModeloStream, Stream),
    close(ModeloStream),
    enviar_evidencias(Evidencias, Stream),
    enviar_query_para_diferentes_combinaciones(Categoria, Stream),
    close(Stream),
    consultar_probabilidades(TmpFile, Probabilidad),
    asserta((probabilidad_consulta(Categoria, Dados, Patron, Probabilidad))).
