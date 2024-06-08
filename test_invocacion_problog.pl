:- use_module(library(filesex)).
:- use_module(library(persistency)).

% Invoco a Problog a partir de un modelo 
% Y consulto el resultado para obtener 
% las consultas y su probabilidad

consultar_probabilidades(ListaValores):-
    % Problog debe estar en el path!
    absolute_file_name(path(problog),Problog,[access(exist),extensions([exe])]),
    % Nombre del modelo, que se supone está en el mismo directorio que el fuente
    absolute_file_name(test_problog,Modelo,[file_type(prolog)]),
    % Invoca a problog con el modelo como argumento, y envía la salida a un pipe
    process_create(Problog,[Modelo],[stdout(pipe(In))]),
    % Convierte la salida a un string
    read_string(In,_,Result),
    % Divide la salida
    writeln(Result),
    
    split_string(Result,"\n\t","\r  llamada ( ), :",L),
    % Escribo la salida
    % Quito último elemento de la lista
    append(L1,[_],L),
    lista_valores(L1,ListaValores),
    writeln(ListaValores).


% Predicado auxiliar para transformar a términos y a números, como se espera
lista_valores([X,Y|T],[M|T1]):-
    M = s(X,Y),
    lista_valores(T,T1).
lista_valores([],[]).

conseguir_evidencias([0|RestoPatron],[Dado|RestoDados],N,[Evidencia|RestoEvidencias]):-
    (N =:= 1, Evidencia = evidence(dado1(Dado),true), N1 is N + 1, conseguir_evidencias(RestoPatron,RestoDados,N1,RestoEvidencias), !);
    (N =:= 2, Evidencia = evidence(dado2(Dado),true), N1 is N + 1, conseguir_evidencias(RestoPatron,RestoDados,N1,RestoEvidencias), !);
    (N =:= 3, Evidencia = evidence(dado3(Dado),true), N1 is N + 1, conseguir_evidencias(RestoPatron,RestoDados,N1,RestoEvidencias), !);
    (N =:= 4, Evidencia = evidence(dado4(Dado),true), N1 is N + 1, conseguir_evidencias(RestoPatron,RestoDados,N1,RestoEvidencias), !);
    (N =:= 5, Evidencia = evidence(dado5(Dado),true), N1 is N + 1, conseguir_evidencias(RestoPatron,RestoDados,N1,RestoEvidencias), !).
conseguir_evidencias([1|RestoPatron],[_|RestoDados],N,Evidencia):-
    N1 is N + 1,
    conseguir_evidencias(RestoPatron,RestoDados,N1,Evidencia).
conseguir_evidencias([],[],_,[]).

enviar_evidencias([],_).
enviar_evidencias([Evidencia|Evidencias],Stream):-
    write(Stream,Evidencia),
    writeln(Stream, .),
    enviar_evidencias(Evidencias,Stream).

enviar_query(Stream) :-
    writeln(Stream, 'query(tres_iguales_aux(X)).').

borrar_evidencias_y_query(Stream):-
    writeln(Stream,'retractall(evidence(_,_)).'),
    writeln(Stream,'retractall(query(_)).').

tres_of_a_kind(Patron,Dados,L):-
    conseguir_evidencias(Patron,Dados,1,Evidencias),
    absolute_file_name(test_problog,Modelo,[file_type(prolog)]),
    open(Modelo, append, Stream),
    writeln(Stream,''),
    enviar_evidencias(Evidencias,Stream),
    enviar_query(Stream),
    borrar_evidencias_y_query(Stream),
    close(Stream),
    consultar_probabilidades(L).


