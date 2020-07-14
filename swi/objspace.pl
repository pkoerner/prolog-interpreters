:- module(objspace, [add/3, sub/3, mul/3,
                     le/3, lt/3, ge/3, gt/3, eq/3,
                     not/2,
                     store/4, lookup/3,
                     create_integer/2,
                     int_to_varname/3,
                     is_truthy/1,
                     is_falsey/1,
                     assoc_size/2]).

add(int(X), int(Y), int(Res)) :-
    Res is X + Y.

sub(int(X), int(Y), int(Res)) :-
    Res is X - Y.

mul(int(X), int(Y), int(Res)) :-
    Res is X * Y.

mod(int(X), int(Y), int(Res)) :-
    Y > 0, %% TODO: B semantics?
    Res is X mod Y.

le(int(X), int(Y), Res) :-
    (X =< Y 
      -> Res = true
      ;  Res = false).

lt(int(X), int(Y), Res) :-
    (X < Y 
      -> Res = true
      ;  Res = false).

ge(int(X), int(Y), Res) :-
    (X >= Y 
      -> Res = true
      ;  Res = false).

gt(int(X), int(Y), Res) :-
    (X > Y 
      -> Res = true
      ;  Res = false).

eq(int(X), int(Y), Res) :-
    (X =:= Y 
      -> Res = true
      ;  Res = false).

not(R, Res) :- 
    (R == true ->
        Res = false;
        R == false, Res = true).

is_truthy(X) :-
    X == true.

is_falsey(X) :-
    X == false.

:- use_module(library(assoc)).

store(AVL, Key, Value, NewAVL) :-
    put_assoc(Key, AVL, Value, NewAVL).
lookup(Key, AVL, Value) :-
    get_assoc(Key, AVL, Value).



create_integer(I, int(I)).
    
int_to_varname(I, IdTable, Varname) :-
    get_assoc(I, IdTable, Varname).


assoc_size(Assoc, Size) :-
    assoc_to_list(Assoc, L),
    length(L, Size).
