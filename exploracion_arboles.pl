:- module(_,_).

% menores(arbol,maximo) nodo(numero,Hijos) [Hijos = {nodo, hoja}] hoja(numero)

menores(nodo(N,[Hijos]),Max) :-
  less_or_equal(N,Max),
  menoresAux(Hijos,Max).

menoresAux([nodo(N,[Hijos])|Xs],Max) :-
  less_or_equal(N,Max),
  menoresAux(Hijos,Max),
  menoresAux(Xs,Max).

menoresAux([hoja(N)|Xs],Max) :-
  less_or_equal(N,Max),
  menoresAux(Xs,Max).

% Predicados Auxiliares

less_or_equal(0,X) :-
	nat(X).
less_or_equal(s(X),s(Y)) :-
	less_or_equal(X,Y).

nat(0).
nat(s(X)) :-
  	nat(X).
