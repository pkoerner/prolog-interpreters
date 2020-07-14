:- module(benchmarker, [run_bench/0, benchs_to_files/0]).



:- use_module(ast_interpreter, [ast_int/4]).
:- use_module(bc_interpreter, [bc_int_prepare/2, run_bc_int/4]).
:- use_module(assert_bc, [prepare_assert_bc/1, fact_int/2]).
:- use_module(c_driven, [prepare_c_int/7, run_c_int/6, ast_to_bc_file/3]).
:- use_module(rational_trees, [thread_program/2, rt_int/4]).
:- use_module(parser, [parse/2]).
:- use_module(generator, [gen_program/1]).

:- use_module(library(lists)).
:- use_module(library(avl)).
:- use_module(library(random)).


benchmark(generated).
benchmark(generated2).
benchmark(generated3).
benchmark(prime_tester).
benchmark(fib).

run_bench :-
    findall(X, benchmark(X), L),
    maplist(run_bench, L).

run_bench(Benchmark) :-
    print(Benchmark),nl,
    Goal =.. [Benchmark, AST, Env],
    call(Goal),
    run_bench_with_ast(ast,AST,Env,AEnv),
    maplist(run_and_check_bench(AST,Env,AEnv),[bc,fact,c,rational]).

run_and_check_bench(AST,Env,AEnv,Kind) :-
    run_bench_with_ast(Kind,AST,Env,Res),
    avl_equals(Res,AEnv).

run_bench_with_ast(ast,AST,Env,AEnv) :-
    format('Starting AST interpreter~n',  []),
    !, garbage_collect,
    statistics(walltime, _),
    %% set_prolog_flag(profiling,on), %% comment in to get profile info
    ast_int(AST, Env, objspace, AEnv),
    statistics(walltime, [_,Time1]),
    %% print_profile,set_prolog_flag(profiling,off), %% comment in to get profile info
    !, garbage_collect,
    format('AST interpreter took ~d ms~n',  [Time1]).
    %print(AEnv),nl,

run_bench_with_ast(bc,AST,Env,BEnv) :-
    bc_int_prepare(AST, Codes),
    !, garbage_collect,
    statistics(walltime, _),
    run_bc_int(Codes, Env, objspace, BEnv),
    statistics(walltime, [_,Time2]),
    !, garbage_collect,
    format('BC interpreter took ~d ms~n',   [Time2]).
    
run_bench_with_ast(fact,AST,Env,CEnv) :-
    prepare_assert_bc(AST),
    !, garbage_collect,
    statistics(walltime, _),
    fact_int(Env, CEnv),
    statistics(walltime, [_,Time3]),
    !, garbage_collect,
    format('fact interpreter took ~d ms~n', [Time3]).

run_bench_with_ast(c,AST,Env,DEnv) :-
    prepare_c_int(AST, Env, RIdTable, CBC, Len, CE, Len2),
    statistics(walltime, _),
    print('starting'),nl,
    run_c_int(RIdTable, CBC, Len, CE, Len2, DEnv),
    statistics(walltime, [_,Time4]),
    !,
    format('C interpreter took ~d ms~n', [Time4]).

run_bench_with_ast(rational,AST,Env,EEnv) :-
    thread_program(AST, Threaded),
    !, garbage_collect,
    statistics(walltime, _),
    rt_int(Threaded, Env, objspace, EEnv),
    statistics(walltime, [_,Time5]),
    !, garbage_collect,
    format('rational tree interpreter took ~d ms~n', [Time5]).


to_bc_file(Benchmark) :-
    Goal =.. [Benchmark, AST, _Env],
    call(Goal),
    atom_codes(Benchmark, CBenchmark),
    append("benchmarks/", CBenchmark, CPath),
    atom_codes(Path, CPath),
    ast_to_bc_file(AST, _Env, Path).

avls_same_val(K, AVL1, AVL2) :-
    avl_fetch(K, AVL1, V),
    avl_fetch(K, AVL2, V).

avl_equals(AVL1, AVL2) :-
    avl_domain(AVL1, D),
    avl_domain(AVL2, D),
    avl_to_list(AVL1, L1),
    avl_to_list(AVL2, L2),
    permutation(L1, L2).



:- use_module(library(file_systems)).

benchs_to_files :-
    Dirname = 'benchmarks',
    (directory_exists(Dirname) -> true
                                ; make_directory(Dirname)),
    findall(X, benchmark(X), L),
    maplist(to_bc_file, L).

fib(AST, Env) :-
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

prime_tester(AST, Env) :-
    AST = [while(lt(id(start), id(v)),
                [if(eq(mod(id(v), id(start)), int(0)),
                    [assign(id(is_prime), int(0))],
                    [assign(id(is_prime), id(is_prime))]),
                 assign(id(start), add(id(start), int(1)))], _, _)],
    empty_avl(AVL),
    avl_store(is_prime, AVL, int(1), AVL1),
    %avl_store(v, AVL1, int(34261), AVL2),
    %avl_store(v, AVL1, int(800000), AVL2),
    %avl_store(v, AVL1, int(34265341), AVL2),
    avl_store(v, AVL1, int(34265351), AVL2), % this is a prime
    
    avl_store(start, AVL2, int(2), Env).

    %
    % Env: [is_prime = 0, V = 34265341, start = 2]
    %
    % while (start < V)
    %     if (V mod start == 0)
    %         is_prime = 1
    %     start = start + 1
    %
 
generated(AST, Env) :-
    setrand(1234),
    gen_program(AST), !,
    tell(generated_ast), write(AST), write('.'), told,
    empty_avl(AVL),
    avl_store(a, AVL, int(1), AVL1),
    avl_store(b, AVL1, int(1), AVL2),
    avl_store(x, AVL2, int(1), AVL3),
    avl_store(y, AVL3, int(1), AVL4),
    avl_store(z, AVL4, int(1), Env).
