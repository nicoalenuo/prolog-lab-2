:- use_module(library(filesex)).

% Invoco a Problog a partir de un modelo 
% Y consulto el resultado para obtener 
% las consultas y su probabilidad

consultar_probabilidades([Patron1,Patron2,Patron3,Patron4,Patron5],[Dado1,Dado2,Dado3,Dado4,Dado5],ListaValores):-
    % Problog debe estar en el path!
    absolute_file_name(path(problog),Problog,[access(exist),extensions([exe])]),
    % Nombre del modelo, que se supone está en el mismo directorio que el fuente
    absolute_file_name(pr,Modelo,[file_type(prolog)]),
    % Invoca a problog con el modelo como argumento, y envía la salida a un pipe
    process_create(Problog,[Modelo, '-a', Patron1, '-a', Patron2, '-a', Patron3, '-a', Patron4, '-a', Patron5, '-a', Dado1, '-a', Dado2, '-a', Dado3, '-a', Dado4, '-a', Dado5],[stdout(pipe(In))]),
    % Convierte la salida a un string
    read_string(In,_,Result),
    % Divide la salida
    
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