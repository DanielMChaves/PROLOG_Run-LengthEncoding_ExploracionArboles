:- module(_,_).

comprimir([],[]).

comprimir([E|Es],F) :-
  comprimirAux(Es,E,[rlec(E,C)|Rs]),
  limpiar([rlec(E,s(C))|Rs],F).

comprimirAux([],_,[rlec(_,s(0))|[]]).

comprimirAux([E|Es],E,[rlec(E,s(C))|Rs]) :-
  comprimirAux(Es,E,[rlec(E,C)|Rs]).

comprimirAux([E|Es],X,[rlec(X,s(0)),rlec(E,C)|Rs]) :-
  E \= X,
  comprimirAux(Es,E,[rlec(E,C)|Rs]).

limpiar([],[]).
limpiar([rlec(E,s(0))|Xs],[E|Ys]) :-
  limpiar(Xs,Ys).
limpiar([rlec(E,C)|Xs],[rlec(E,C)|Ys]) :-
  C \= s(0),
  limpiar(Xs,Ys).
