:- use_module(library(filesex)).

consultar_probabilidad_unica(Categoria, [D1, D2, D3, D4, D5], [X1, X2, X3, X4, X5], Probabilidad):-
    absolute_file_name(path(problog),Problog,[access(exist),extensions([exe])]),
    absolute_file_name(modelo,Modelo,[file_type(prolog)]),
    process_create(Problog, [Modelo, '-a', Categoria, '-a', D1, '-a', D2,'-a', D3,'-a', D4,'-a', D5, '-a', X1, '-a', X2, '-a', X3, '-a', X4, '-a', X5], [stdout(pipe(In))]),
    read_string(In, _, Result),
    split_string(Result,"\n\t","\r ",L),
    append([_, Y], [_], L),
    number_string(Probabilidad, Y).

probabilidades([D1, D2, D3, D4, D5], ListaValores):-
    % Problog debe estar en el path!
    absolute_file_name(path(problog),Problog,[access(exist),extensions([exe])]),
    % Nombre del modelo, que se supone está en el mismo directorio que el fuente
    absolute_file_name(modelo_problog,Modelo,[file_type(prolog)]),
    % Invoca a problog con el modelo como argumento, y envía la salida a un pipe
    process_create(Problog, [Modelo, '-a', D1, '-a', D2,'-a', D3,'-a', D4,'-a', D5], [stdout(pipe(In))]),
    % Convierte la salida a un string
    read_string(In,_,Result),
    % Divide la salida
    split_string(Result,"\n\t","\r ",L),
    % Quito último elemento de la lista
    append(L1,[_],L),
    lista_valores(L1, ListaValores).

lista_valores([X, Y | T], [TermValor | T1]) :-
    % Saco los dos puntos del final
    split_string(X, "(", "):", [_, X1]),
    string_to_atom(X1, Atom),
    atom_to_term(Atom, ValoresStr, _),
    number_string(NumberY, Y),
    TermValor = c(ValoresStr, NumberY),
    lista_valores(T, T1).
lista_valores([], []).

