:- use_module(library(filesex)).
:- use_module(library(persistency)).
:- dynamic prob/2.

categorias_seccion_superior([m(1, aces), m(2, twos), m(3, threes), m(4, fours), m(5, fives), m(6 ,sixes)]).

consultar_probabilidades(Modelo,Categoria, Probabilidad):-
    absolute_file_name(path(problog), Problog, [access(exist), extensions([exe])]),
    process_create(Problog, [Modelo], [stdout(pipe(In))]),
    read_string(In, _, Result),
    (
        member(Categoria,[full_house, small_straight, large_straight, yahtzee]),!,
        split_string(Result, "\n\t", "\r ", L),
        append([_, Y], [_], L),
        number_string(Probabilidad, Y);

        member(Categoria,[three_of_a_kind,four_of_a_kind]),!,
        (
            Categoria = three_of_a_kind,split_string(Result,"three_of_a_kind():\n\t", "three_of_a_kind():\n\t",L),!;
            Result, split_string(Result,"four_of_a_kind():\n\t", "four_of_a_kind():\n\t",L)
        ),
        lista_valores(L,Probabilidad);

        member(Categoria,[aces,twos,threes,fours,fives,sixes]),!,
        split_string(Result,"probabilidad_de_X_N_veces(,):\n\t", "probabilidad_de_X_N_veces(,):\n\t",L),
        lista_valores2(L,Probabilidad)
    ).

lista_valores([X,Y|T],[M|T1]):-
    M = (X,Y),
    lista_valores(T,T1).
lista_valores([],[]).
lista_valores2([X,Y,Z|T],[M|T1]):-
    M = (X,Y,Z),
    lista_valores2(T,T1).
lista_valores2([],[]).

% Predicado para conseguir evidencias
conseguir_evidencias([0|RestoPatron], [Dado|RestoDados], N, [Evidencia|RestoEvidencias]):-
    (N =:= 1, Evidencia = evidence(dado(1,Dado), true), N1 is N + 1, conseguir_evidencias(RestoPatron, RestoDados, N1, RestoEvidencias), !);
    (N =:= 2, Evidencia = evidence(dado(2,Dado), true), N1 is N + 1, conseguir_evidencias(RestoPatron, RestoDados, N1, RestoEvidencias), !);
    (N =:= 3, Evidencia = evidence(dado(3,Dado), true), N1 is N + 1, conseguir_evidencias(RestoPatron, RestoDados, N1, RestoEvidencias), !);
    (N =:= 4, Evidencia = evidence(dado(4,Dado), true), N1 is N + 1, conseguir_evidencias(RestoPatron, RestoDados, N1, RestoEvidencias), !);
    (N =:= 5, Evidencia = evidence(dado(5,Dado), true), N1 is N + 1, conseguir_evidencias(RestoPatron, RestoDados, N1, RestoEvidencias), !).
conseguir_evidencias([1|RestoPatron], [_|RestoDados], N, Evidencias):-
    N1 is N + 1,
    conseguir_evidencias(RestoPatron, RestoDados, N1, Evidencias).
conseguir_evidencias([], [], _, []).

enviar_evidencias([], _).
enviar_evidencias([Evidencia|Evidencias], Stream):-
    write(Stream, Evidencia),
    writeln(Stream, '.'),
    enviar_evidencias(Evidencias, Stream).

enviar_query(Categoria, Stream):-
    (   
        member(Categoria,[full_house, small_straight, large_straight, yahtzee]),
        write(Stream, 'query('),
        write(Stream, Categoria),
        writeln(Stream, ').');

        member(Categoria,[three_of_a_kind,four_of_a_kind]),
        write(Stream, 'query('),
        write(Stream, Categoria),
        write(Stream, '(Tirada)'),
        writeln(Stream, ').');

        categorias_seccion_superior(Lista),
        member(m(Tipo,Categoria),Lista),

        write(Stream, 'query('),
        write(Stream, 'probabilidad_de_X_N_veces'),
        write(Stream, '('),
        write(Stream, Tipo),
        write(Stream, ','),
        write(Stream, '1'),
        write(Stream, ')'),
        writeln(Stream, ').'),

        write(Stream, 'query('),
        write(Stream, 'probabilidad_de_X_N_veces'),
        write(Stream, '('),
        write(Stream, Tipo),
        write(Stream, ','),
        write(Stream, '2'),
        write(Stream, ')'),
        writeln(Stream, ').'),

        write(Stream, 'query('),
        write(Stream, 'probabilidad_de_X_N_veces'),
        write(Stream, '('),
        write(Stream, Tipo),
        write(Stream, ','),
        write(Stream, '3'),
        write(Stream, ')'),
        writeln(Stream, ').'),

        write(Stream, 'query('),
        write(Stream, 'probabilidad_de_X_N_veces'),
        write(Stream, '('),
        write(Stream, Tipo),
        write(Stream, ','),
        write(Stream, '4'),
        write(Stream, ')'),
        writeln(Stream, ').'),

        write(Stream, 'query('),
        write(Stream, 'probabilidad_de_X_N_veces'),
        write(Stream, '('),
        write(Stream, Tipo),
        write(Stream, ','),
        write(Stream, '5'),
        write(Stream, ')'),
        writeln(Stream, ').')

    ).

probabilidad(Categoria, Patron, Dados, Probabilidad):-
    conseguir_evidencias(Patron, Dados, 1, Evidencias),
    absolute_file_name(modelo, Modelo, [file_type(prolog)]),
    tmp_file_stream(text, TmpFile, Stream),
    open(Modelo, read, ModeloStream),
    copy_stream_data(ModeloStream, Stream),
    close(ModeloStream),
    enviar_evidencias(Evidencias, Stream),
    enviar_query(Categoria, Stream),
    close(Stream),
    time(consultar_probabilidades(TmpFile,Categoria, Probabilidad)).
