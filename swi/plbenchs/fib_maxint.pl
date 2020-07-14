:- module('benchmarks/fib_maxint', [benchmark/2]).
:- use_module('../parser', [parse/2]).

:- use_module(library(assoc)).

benchmark(AST, Env) :-
    Program = "i := 1;                 \
               while i < n {           \
                   b := (b + a) mod 1000000; \
                   a := (b - a) mod 1000000; \
                   i := i + 1;          \
               }",
    parse(Program, AST), !,
    empty_assoc(AVL0),
    put_assoc(a, AVL0, int(0), AVL1),
    put_assoc(b, AVL1, int(1), AVL2),
    put_assoc(n, AVL2, int(10000000), AVL3),
    Env = AVL3.
