:- module('benchmarks/generated2', [benchmark/2]).

:- use_module(library(assoc)).


benchmark(AST, Env) :-
    see(generated_ast2),
    read(AST),
    seen,
    empty_assoc(AVL),
    put_assoc(a, AVL, int(1), AVL1),
    put_assoc(b, AVL1, int(1), AVL2),
    put_assoc(x, AVL2, int(1), AVL3),
    put_assoc(y, AVL3, int(1), AVL4),
    put_assoc(z, AVL4, int(1), Env).
