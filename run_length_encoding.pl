:- module(_,_).

% comprimir/2: Predicado que comprime una lista de caracteres en una
% con contadores para cada elementos continuos con el predicado
% rlec(Elemento,Número)
comprimir([],[]).
comprimir([E|Es],F) :-
  comprimirAux(Es,E,[rlec(E,C)|Rs]),
  limpiar([rlec(E,s(C))|Rs],F).

% comprimirAux/3: Predicado que dada una lista de elementos la
% comprime sin diferenciar el caso en el que solo haya un elemento
comprimirAux([],_,[rlec(_,s(0))|[]]).
comprimirAux([E|Es],E,[rlec(E,s(C))|Rs]) :-
  comprimirAux(Es,E,[rlec(E,C)|Rs]).
comprimirAux([E|Es],X,[rlec(X,s(0)),rlec(E,C)|Rs]) :-
  E \= X,
  comprimirAux(Es,E,[rlec(E,C)|Rs]).

% limpiar/2: Predicado que dada una lista de rlecs comprueba si el
% contador es 1 para quitar la estructura rlec y pones solo el
% elemento
limpiar([],[]).
limpiar([rlec(E,s(0))|Xs],[E|Ys]) :-
  limpiar(Xs,Ys).
limpiar([rlec(E,C)|Xs],[rlec(E,C)|Ys]) :-
  C \= s(0),
  limpiar(Xs,Ys).

% descomprimir/2: Predicado que descomprime una lista de rlecs
% a una lista de elementos
descomprimir(X,Y) :-
  descomprimirAux(X,Y).

% descomprimirAux/2: Predicado que descomprime una lista de rlecs
% a una lista de elementos
descomprimirAux([],[]).
descomprimirAux([rlec(E,C)|Xs],F) :-
  escribir(E,C,L),
  descomprimirAux(Xs,Fs),
  myappend(L,Fs,F).
descomprimirAux([X|Xs],F) :-
  X \= rlec(_,_),
  descomprimirAux(Xs,Fs),
  myappend([X],Fs,F).

% PREDICADOS AUXILIARES

% escribir/3: Predicado que escribe n veces un número
% en una lista resultado
escribir(_,0,[]).
escribir(E,s(C),[E|Es]) :- escribir(E,C,Es).

% myappend/3: Predicado que concatena dos listas
myappend([],L,L).
myappend([H|T],L2,[H|L3]) :- myappend(T,L2,L3).
