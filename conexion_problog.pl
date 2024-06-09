:- use_module(library(filesex)).
:- use_module(library(persistency)).

consultar_probabilidades(Modelo, Probabilidad):-
    absolute_file_name(path(problog), Problog, [access(exist), extensions([exe])]),
    process_create(Problog, [Modelo], [stdout(pipe(In))]),
    read_string(In, _, Result),
    split_string(Result, "\n\t", "\r ", L),
    append([_, Y], [_], L),
    number_string(Probabilidad, Y).

% Predicado para conseguir evidencias
conseguir_evidencias([0|RestoPatron], [Dado|RestoDados], N, [Evidencia|RestoEvidencias]):-
    (N =:= 1, Evidencia = evidence(dado1(Dado), true), N1 is N + 1, conseguir_evidencias(RestoPatron, RestoDados, N1, RestoEvidencias), !);
    (N =:= 2, Evidencia = evidence(dado2(Dado), true), N1 is N + 1, conseguir_evidencias(RestoPatron, RestoDados, N1, RestoEvidencias), !);
    (N =:= 3, Evidencia = evidence(dado3(Dado), true), N1 is N + 1, conseguir_evidencias(RestoPatron, RestoDados, N1, RestoEvidencias), !);
    (N =:= 4, Evidencia = evidence(dado4(Dado), true), N1 is N + 1, conseguir_evidencias(RestoPatron, RestoDados, N1, RestoEvidencias), !);
    (N =:= 5, Evidencia = evidence(dado5(Dado), true), N1 is N + 1, conseguir_evidencias(RestoPatron, RestoDados, N1, RestoEvidencias), !).
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
    write(Stream, 'query('),
    write(Stream, Categoria),
    writeln(Stream, ').').

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
    consultar_probabilidades(TmpFile, Probabilidad).
