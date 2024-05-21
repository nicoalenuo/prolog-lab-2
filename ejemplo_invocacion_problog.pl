:- use_module(library(filesex)).

% Invoco a Problog a partir de un modelo 
% Y consulto el resultado para obtener 
% las consultas y su probabilidad

consultar_probabilidades(ListaValores):-
    % Problog debe estar en el path!
    absolute_file_name(path(problog),Problog,[access(exist),extensions([exe])]),
    % Nombre del modelo, que se supone está en el mismo directorio que el fuente
    absolute_file_name(modelo_problog,Modelo,[file_type(prolog)]),
    % Invoca a problog con el modelo como argumento, y envía la salida a un pipe
    process_create(Problog,[Modelo],[stdout(pipe(In))]),
    % Convierte la salida a un string
    read_string(In,_,Result),
    % Divide la salida
    split_string(Result,"\n\t","\r ",L),
    % Escribo la salida
    writeln(Result),
    % Quito último elemento de la lista
    append(L1,[_],L),
    lista_valores(L1,ListaValores).

% Predicado auxiliar para transformar a términos y a números, como se espera
lista_valores([X,Y|T],[TermValor|T1]):-
    % Saco los dos puntos del final
    split_string(X,"",":",[X1|_]),
    term_string(TermX,X1),
    TermX =.. [carta,Cat,Valor],
    number_string(NumberY,Y),
    TermValor =.. [p,Cat,Valor,NumberY],
    lista_valores(T,T1).
lista_valores([],[]).