:- module('benchmarks/prime_tester', [benchmark/2]).
:- use_module(library(avl)).

benchmark(AST, Env) :-
    AST = [while(lt(id(start), id(v)),
                [if(eq(mod(id(v), id(start)), int(0)),
                    [assign(id(is_prime), int(0))],
                    [assign(id(is_prime), id(is_prime))]),
                 assign(id(start), add(id(start), int(1)))], _, _)],
    empty_avl(AVL),
    avl_store(is_prime, AVL, int(1), AVL1),
    avl_store(v, AVL1, int(34265351), AVL2), % this is a prime
    avl_store(start, AVL2, int(2), Env).
