:- module(_,_).

% menores/2: Predicado que comprueba que todos los nodos del
% árbol tienen un valor igual o menor a un número dado
menores([],[]).
menores(nodo(N,Hijos),Max) :-
  less_or_equal(N,Max),
  menoresAux(Hijos,Max).

% menoresAux/2: Predicado que comprueba que todos los nodos del
% árbol tienen un valor igual o menor a un número dado
menoresAux([],_).
menoresAux([nodo(N,Hijos)|Xs],Max) :-
  less_or_equal(N,Max),
  menoresAux(Hijos,Max),
  menoresAux(Xs,Max).
menoresAux([hoja(N)|Xs],Max) :-
  less_or_equal(N,Max),
  menoresAux(Xs,Max).

% suma/2: Predicado que dado un árbol, suma los valores de
% todos los nodos
suma([],0).
suma(nodo(N,Hijos),Suma) :-
  sumaAux(Hijos,Ns),
  plus(N,Ns,Suma).

% sumaAux/2: Predicado que dado un árbol, suma los valores de
% todos los nodos
sumaAux([],0).
sumaAux([nodo(N,Hijos)|Xs],Suma) :-
  sumaAux(Hijos,Ns),
  sumaAux(Xs,Nss),
  plus(N,Ns,NX),
  plus(Nss,NX,Suma).
sumaAux([hoja(N)|Xs],Suma) :-
  sumaAux(Xs,Ns),
  plus(N,Ns,Suma).

% PREDICADOS AUXILIARES

% less_or_equal/2: Predicado que comprueba si un elemento
% es menor o igual que otro
less_or_equal(0,X) :-
	nat(X).
less_or_equal(s(X),s(Y)) :-
	less_or_equal(X,Y).

% nat/1: Predicado de comprobación de los números naturales
% en notación de Peano
nat(0).
nat(s(X)) :- nat(X).

% plus/3: Predicado que realiza una suma en notación de Peano
plus(0,X,X) :- nat(X).
plus(s(X),Y,s(Z)) :- plus(X,Y,Z).
