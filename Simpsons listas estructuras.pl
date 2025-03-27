% Listas de personas con estructuras
personas([
    hombre(abraham), hombre(clancy), hombre(herbert), hombre(homero), hombre(bart),
    mujer(mona), mujer(marge), mujer(jacqueline), mujer(patty), mujer(selma), mujer(lisa), mujer(maggie), mujer(ling)
]).

% Relaciones de paternidad y maternidad usando listas
% Relaciones de paternidad con términos compuestos
es_padre_de(abraham, [hijo(homero), hijo(herbert)]).
es_padre_de(clancy, [hija(patty), hija(selma)]).
es_padre_de(homero, [hijo(bart), hija(lisa), hija(maggie)]).

% Relaciones de maternidad con términos compuestos
es_madre_de(mona, [hijo(homero), hijo(herbert)]).
es_madre_de(jacqueline, [hija(marge), hija(patty), hija(selma)]).
es_madre_de(marge, [hijo(bart), hija(lisa), hija(maggie)]).
es_madre_de(selma, [hija(ling)]).


% Reglas generales
padre_de(P, Hijo) :- 
    es_padre_de(P, Hijos), 
    (member(hijo(Hijo), Hijos); member(hija(Hijo), Hijos)).

madre_de(M, Hijo) :- 
    es_madre_de(M, Hijos), 
    (member(hijo(Hijo), Hijos); member(hija(Hijo), Hijos)).


abuelo_de(X, Y) :- 
    padre_de(X, Z), 
    (es_padre_de(Z, Hijos), (member(hijo(Y), Hijos); member(hija(Y), Hijos));
     es_madre_de(Z, Hijos), (member(hijo(Y), Hijos); member(hija(Y), Hijos))).

abuela_de(X, Y) :- 
    madre_de(X, Z), 
    (es_padre_de(Z, Hijos), (member(hijo(Y), Hijos); member(hija(Y), Hijos));
     es_madre_de(Z, Hijos), (member(hijo(Y), Hijos); member(hija(Y), Hijos))).

% Dos personas son hermanos si tienen el mismo padre o la misma madre.
hermanos(X, Y) :- 
    es_padre_de(_, Hijos), 
    member(hijo(X), Hijos), member(hijo(Y), Hijos), X \= Y.

hermanos(X, Y) :- 
    es_padre_de(_, Hijos), 
    member(hijo(X), Hijos), member(hija(Y), Hijos), X \= Y.

hermanos(X, Y) :- 
    es_padre_de(_, Hijos), 
    member(hija(X), Hijos), member(hijo(Y), Hijos), X \= Y.

hermanos(X, Y) :- 
    es_padre_de(_, Hijos), 
    member(hija(X), Hijos), member(hija(Y), Hijos), X \= Y.

hermanos(X, Y) :- 
    es_madre_de(_, Hijos), 
    member(hijo(X), Hijos), member(hijo(Y), Hijos), X \= Y.

hermanos(X, Y) :- 
    es_madre_de(_, Hijos), 
    member(hijo(X), Hijos), member(hija(Y), Hijos), X \= Y.

hermanos(X, Y) :- 
    es_madre_de(_, Hijos), 
    member(hija(X), Hijos), member(hijo(Y), Hijos), X \= Y.

hermanos(X, Y) :- 
    es_madre_de(_, Hijos), 
    member(hija(X), Hijos), member(hija(Y), Hijos), X \= Y.

% Hermano si es hombre y es hermano de alguien.
hermano_de(X, Y) :- 
    hermanos(X, Y), 
    personas(P), 
    member(hombre(X), P).

% Hermana si es mujer y es hermana de alguien.
hermana_de(X, Y) :- 
    hermanos(X, Y), 
    personas(P), 
    member(mujer(X), P).

% Tío si es hermano del padre o la madre de la persona.
tio_de(X, Y) :- 
    padre_de(Z, Y), 
    hermano_de(X, Z).

tio_de(X, Y) :- 
    madre_de(Z, Y), 
    hermano_de(X, Z).

% Tía si es hermana del padre o la madre de la persona.
tia_de(X, Y) :- 
    padre_de(Z, Y), 
    hermana_de(X, Z).

tia_de(X, Y) :- 
    madre_de(Z, Y), 
    hermana_de(X, Z).

% Primos si su padre o madre tienen un hermano/a que es padre/madre del otro.
primos(X, Y) :- 
    padre_de(Z, X), 
    tio_de(Z, Y).

primos(X, Y) :- 
    madre_de(Z, X), 
    tia_de(Z, Y).

% Primo si es hombre y es primo de alguien.
primo_de(X, Y) :- 
    primos(X, Y), 
    personas(P), 
    member(hombre(X), P).

% Prima si es mujer y es prima de alguien.
prima_de(X, Y) :- 
    primos(X, Y), 
    personas(P), 
    member(mujer(X), P).
