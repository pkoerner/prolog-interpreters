:- module(benchmarker, [run_bench/0/*, benchs_to_files/0*/]).



:- use_module(ast_interpreter, [ast_int/4]).
:- use_module(bc_interpreter, [bc_int_prepare/2, run_bc_int/4]).
:- use_module(assert_bc, [prepare_assert_bc/1, fact_int/2]).
:- use_module(c_driven, [prepare_c_int/7, run_c_int/6, ast_to_bc_file/3]).
:- use_module(rational_trees, [thread_program/2, rt_int/4]).
:- use_module(parser, [parse/2]).

:- use_module(library(lists)).
:- use_module(library(assoc)).


%benchmark(generated).
%benchmark(prime_tester).
benchmark(fib).


assoc_equals(A, B) :-
    assoc_to_list(A, L),
    assoc_to_list(B, K),
    permutation(L, K).

run_bench :-
    findall(X, benchmark(X), L),
    maplist(run_bench, L).

run_bench(Benchmark) :-
    print(Benchmark),nl,
    Goal =.. [Benchmark, AST, Env],
    call(Goal),

    !, garbage_collect,
    statistics(walltime, _),
    ast_int(AST, Env, objspace, AEnv),
    statistics(walltime, [_,Time1]),
    !, garbage_collect,

    format('AST interpreter took ~d ms~n',  [Time1]),
    %print(AEnv),nl,
    
    print('compile to sbc'),nl,
    bc_int_prepare(AST, Codes),
    print('compilation succeeded'),nl,
    !, garbage_collect,
    statistics(walltime, _),
    run_bc_int(Codes, Env, objspace, BEnv),
    statistics(walltime, [_,Time2]),
    !, garbage_collect,
    format('BC interpreter took ~d ms~n',   [Time2]),
    
    print('compile to facts'),nl,
    prepare_assert_bc(AST),
    print('compilation succeeded'),nl,
    !, garbage_collect,
    statistics(walltime, _),
    fact_int(Env, CEnv),
    statistics(walltime, [_,Time3]),
    !, garbage_collect,
    format('fact interpreter took ~d ms~n', [Time3]),

    print('compile for c'),nl,
    prepare_c_int(AST, Env, RIdTable, CBC, Len, CE, Len2),
    print('compilation succeeded'),nl,
    statistics(walltime, _),
    print('starting'),nl,
    run_c_int(RIdTable, CBC, Len, CE, Len2, DEnv),
    statistics(walltime, [_,Time4]),
    !,
    format('C interpreter took ~d ms~n', [Time4]),

    print('transform to threaded'),nl,
    thread_program(AST, Threaded),
    print('transformation succeeded'),nl,
    !, garbage_collect,
    statistics(walltime, _),
    rt_int(Threaded, Env, objspace, EEnv),
    statistics(walltime, [_,Time5]),
    !, garbage_collect,
    format('rational tree interpreter took ~d ms~n', [Time5]),

    assoc_equals(AEnv, BEnv),
    assoc_equals(AEnv, CEnv),
    assoc_equals(AEnv, DEnv),
    assoc_equals(AEnv, EEnv).


to_bc_file(Benchmark) :-
    Goal =.. [Benchmark, AST, _Env],
    call(Goal),
    atom_codes(Benchmark, CBenchmark),
    append("benchmarks/", CBenchmark, CPath),
    atom_codes(Path, CPath),
    ast_to_bc_file(AST, _Env, Path).

%:- use_module(library(file_systems)).
%
%benchs_to_files :-
%    Dirname = 'benchmarks',
%    (directory_exists(Dirname) -> true
%                                ; make_directory(Dirname)),
%    findall(X, benchmark(X), L),
%    maplist(to_bc_file, L).

generated(AST, Env) :-
    see(generated_ast),
    read(AST),
    seen,
    empty_assoc(AVL),
    put_assoc(a, AVL, int(1), AVL1),
    put_assoc(b, AVL1, int(1), AVL2),
    put_assoc(x, AVL2, int(1), AVL3),
    put_assoc(y, AVL3, int(1), AVL4),
    put_assoc(z, AVL4, int(1), Env).



prime_tester(AST, Env) :-
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

    %
    % Env: [is_prime = 0, V = 34265341, start = 2]
    %
    % while (start < V)
    %     if (V mod start == 0)
    %         is_prime = 1
    %     start = start + 1
    %

fib(AST, Env) :-
    Program = "i := 1;                 \
               while i < n {           \
                   b := b + a; \
                   a := b - a; \
                   i := i + 1;          \
               }",
    parse(Program, AST), !,
    empty_assoc(AVL0),
    put_assoc(a, AVL0, int(0), AVL1),
    put_assoc(b, AVL1, int(1), AVL2),
    %put_assoc(n, AVL2, int(41), AVL3),
    put_assoc(n, AVL2, int(400000), AVL3),
    Env = AVL3.
