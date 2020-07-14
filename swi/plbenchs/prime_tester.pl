:- module('benchmarks/prime_tester', [benchmark/2]).
:- use_module(library(assoc)).

benchmark(AST, Env) :-
    AST = [while(lt(id(start), id(v)),
                [if(eq(mod(id(v), id(start)), int(0)),
                    [assign(id(is_prime), int(0))],
                    [assign(id(is_prime), id(is_prime))]),
                 assign(id(start), add(id(start), int(1)))], _, _)],
    empty_assoc(AVL),
    put_assoc(is_prime, AVL, int(1), AVL1),
    %put_assoc(v, AVL1, int(5), AVL2),
    %put_assoc(v, AVL1, int(34261), AVL2),
    %put_assoc(v, AVL1, int(800000), AVL2),
    %put_assoc(v, AVL1, int(34265341), AVL2),
    put_assoc(v, AVL1, int(34265351), AVL2), % this is a prime
    
    put_assoc(start, AVL2, int(2), Env).
