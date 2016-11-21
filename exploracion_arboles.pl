:- module(_,_).

menores([],[]).
menores(nodo(N,Hijos),Max) :-
  less_or_equal(N,Max),
  menoresAux(Hijos,Max).

menoresAux([],_).
menoresAux([nodo(N,Hijos)|Xs],Max) :-
  less_or_equal(N,Max),
  menoresAux(Hijos,Max),
  menoresAux(Xs,Max).
menoresAux([hoja(N)|Xs],Max) :-
  less_or_equal(N,Max),
  menoresAux(Xs,Max).

suma([],0).
suma(nodo(N,Hijos),Suma) :-
  sumaAux(Hijos,Ns),
  plus(N,Ns,Suma).

sumaAux([],0).
sumaAux([nodo(N,Hijos)|Xs],Suma) :-
  sumaAux(Hijos,Ns),
  sumaAux(Xs,Nss),
  plus(N,Ns,NX),
  plus(Nss,NX,Suma).
sumaAux([hoja(N)|Xs],Suma) :-
  sumaAux(Xs,Ns),
  plus(N,Ns,Suma).

% Predicados Auxiliares

less_or_equal(0,X) :-
	nat(X).
less_or_equal(s(X),s(Y)) :-
	less_or_equal(X,Y).

nat(0).
nat(s(X)) :- nat(X).

plus(0,X,X) :- nat(X).
plus(s(X),Y,s(Z)) :- plus(X,Y,Z).
