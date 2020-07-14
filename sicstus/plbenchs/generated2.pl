:- module('benchmarks/generated2', [benchmark/2]).

:- use_module(library(random)).
:- use_module(library(avl)).

:- use_module('../generator', [gen_program/1]).

seed(9348442).

benchmark(AST, Env) :-
    seed(X), setrand(X),
    gen_program(AST), !,
    empty_avl(AVL),
    avl_store(a, AVL, int(1), AVL1),
    avl_store(b, AVL1, int(1), AVL2),
    avl_store(x, AVL2, int(1), AVL3),
    avl_store(y, AVL3, int(1), AVL4),
    avl_store(z, AVL4, int(1), Env).

write_program :-
    seed(X), setrand(X),
    gen_program(AST), !,
    tell(generated_ast2), write(AST), write('.'), told.

