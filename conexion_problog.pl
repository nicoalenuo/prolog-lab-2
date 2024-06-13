:- use_module(library(filesex)).
:- use_module(library(persistency)).

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

categorias_seccion_superior_aux([m(1, aces), m(2, twos), m(3, threes), m(4, fours), m(5, fives), m(6 ,sixes)]).

consultar_probabilidades(Modelo, Categoria, Probabilidad) :-
    absolute_file_name(path(problog), Problog, [access(exist), extensions([exe])]),
    process_create(Problog, [Modelo], [stdout(pipe(In))]),
    read_string(In, _, Result),
    close(In),
    (
        member(Categoria, [full_house, small_straight, large_straight, yahtzee]), !,
        split_string(Result, "\n\t", "\r ", L),
        append([_, Y], [_], L),
        number_string(Probabilidad, Y)
    ;
        member(Categoria, [three_of_a_kind, four_of_a_kind]), !,
        (
            Categoria = three_of_a_kind, split_string(Result, "three_of_a_kind():\n\t", "three_of_a_kind():\n\t", L), !
        ;
            split_string(Result, "four_of_a_kind():\n\t", "four_of_a_kind():\n\t", L)
        ),
        lista_valores(L, Probabilidad)
    ;
        member(Categoria, [aces, twos, threes, fours, fives, sixes]), !,
        split_string(Result, "probabilidad_de_X_N_veces(,):\n\t", "probabilidad_de_X_N_veces(,):\n\t", L),
        lista_valores2(L, Probabilidad)
    ).

lista_valores([X, Y | T], [M | T1]) :-
    normalize_space(atom(ProbStrAtom), Y),
    atom_number(ProbStrAtom, Prob),
    string_a_lista(X, Tirada),
    M = (Tirada, Prob),
    lista_valores(T, T1), !.
lista_valores([], []) :- !.
lista_valores(_, [([1, 1, 1, 1, 1], 0)]).

lista_valores2([X, Y, Z | T], [M | T1]) :-
    string(Z),
    normalize_space(atom(ProbStrAtom), Z),
    atom_number(ProbStrAtom, Prob),
    number_string(Cant, Y),
    number_string(Num, X),
    M = (Num, Cant, Prob),
    lista_valores2(T, T1).
lista_valores2([], []).

string_a_lista(String, Lista) :-
    % Quitar los corchetes
    sub_atom(String, 1, _, 1, SubString),
    % Dividir la cadena en elementos individuales
    split_string(SubString, ",", " ", ElementosString),
    % Convertir cada elemento a número
    maplist(atom_number, ElementosString, Lista).

% Predicado para conseguir evidencias
conseguir_evidencias([0 | RestoPatron], [Dado | RestoDados], N, [Evidencia | RestoEvidencias]) :-
    Evidencia = evidence(dado(N, Dado), true),
    N1 is N + 1,
    conseguir_evidencias(RestoPatron, RestoDados, N1, RestoEvidencias), 
    !.
conseguir_evidencias([1 | RestoPatron], [_ | RestoDados], N, Evidencias) :-
    N1 is N + 1,
    conseguir_evidencias(RestoPatron, RestoDados, N1, Evidencias).
conseguir_evidencias([], [], _, []).

enviar_evidencias([], _).
enviar_evidencias([Evidencia | Evidencias], Stream) :-
    writeln(Stream, ''),
    write(Stream, Evidencia),
    writeln(Stream, '.'),
    enviar_evidencias(Evidencias, Stream).

enviar_query(Categoria, Stream) :-
        writeln(Stream, ''),
    (   
        member(Categoria, [full_house, small_straight, large_straight, yahtzee]), !,
        write(Stream, 'query('),
        write(Stream, Categoria),
        writeln(Stream, ').')
    ;
        member(Categoria, [three_of_a_kind, four_of_a_kind]), !,
        write(Stream, 'query('),
        write(Stream, Categoria),
        writeln(Stream, '(Tirada)).')
    ;
        categorias_seccion_superior_aux(Lista), !,
        member(m(Tipo, Categoria), Lista),
        
        write(Stream, 'query(probabilidad_de_X_N_veces('),
        write(Stream, Tipo),
        writeln(Stream, ',1)).'),
        
        writeln(Stream, ''),
        write(Stream, 'query(probabilidad_de_X_N_veces('),
        write(Stream, Tipo),
        writeln(Stream, ',2)).'),

        writeln(Stream, ''),
        write(Stream, 'query(probabilidad_de_X_N_veces('),
        write(Stream, Tipo),
        writeln(Stream, ',3)).'),
        
        writeln(Stream, ''),
        write(Stream, 'query(probabilidad_de_X_N_veces('),
        write(Stream, Tipo),
        writeln(Stream, ',4)).'),

        writeln(Stream, ''),
        write(Stream, 'query(probabilidad_de_X_N_veces('),
        write(Stream, Tipo),
        writeln(Stream, ',5)).')
    ).

% Consulto si ya tengo guardada la probabilidad al cambiar todos los dados
probabilidad(Categoria, _, [1, 1, 1, 1, 1], Probabilidad) :-
    probabilidad_al_cambiar_todos_los_dados(Categoria, Probabilidad),
    !.

% Consulto si ya tengo guardada la consulta, asi se evita volver a calcularla
probabilidad(Categoria, Dados, Patron, Probabilidad) :-
    probabilidad_consulta(Categoria, Dados, Patron, Probabilidad),
    !.

% Si calcula la probabilidad al cambiar todos los dados, la guarda
% Solo aplica a full_house, small_straight, large_straight, yahtzee
probabilidad(Categoria, Dados, Patron, Probabilidad) :-
    Patron = [1, 1, 1, 1, 1],
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
    consultar_probabilidades(TmpFile, Categoria, Probabilidad),
    delete_file(TmpFile), 
    asserta((probabilidad_al_cambiar_todos_los_dados(Categoria, Probabilidad))),
    asserta((probabilidad_consulta(Categoria, Dados, Patron, Probabilidad))), !.

probabilidad(Categoria, Dados, Patron, Probabilidad) :-
    conseguir_evidencias(Patron, Dados, 1, Evidencias),
    absolute_file_name(modelo, Modelo, [file_type(prolog)]),
    tmp_file_stream(text, TmpFile, Stream),
    open(Modelo, read, ModeloStream),
    copy_stream_data(ModeloStream, Stream),
    close(ModeloStream),
    enviar_evidencias(Evidencias, Stream),
    enviar_query(Categoria, Stream),
    close(Stream),
    consultar_probabilidades(TmpFile, Categoria, Probabilidad),
    delete_file(TmpFile), 
    asserta((probabilidad_consulta(Categoria, Dados, Patron, Probabilidad))).
