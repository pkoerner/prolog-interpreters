:- module('benchmarks/fib', [benchmark/2]).
:- use_module('../parser', [parse/2]).

:- use_module(library(avl)).

benchmark(AST, Env) :-
    Program = "i := 1;                 \
               while i < n {           \
                   b := b + a; \
                   a := b - a; \
                   i := i + 1;          \
               }",
    parse(Program, AST), !,
    empty_avl(AVL0),
    avl_store(a, AVL0, int(0), AVL1),
    avl_store(b, AVL1, int(1), AVL2),
    avl_store(n, AVL2, int(400000), AVL3),
    Env = AVL3.
