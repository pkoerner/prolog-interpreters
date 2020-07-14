:- use_module(assert_bc, [prepare_assert_bc/1, fact_int/2]).

prepare(Benchmark) :-
    atom_concat('plbenchs/', Benchmark, Name),
    use_module(Name),
    benchmark(AST, _Env),
    prepare_assert_bc(AST).

run_prepared(Benchmark) :-
    print('running: '),
    print(Benchmark),
    nl,

    atom_concat('plbenchs/', Benchmark, Name),
    use_module(Name),
    benchmark(_AST, Env),

    use_module(generated, [bc2/3]),
    

    !, garbage_collect,
    statistics(walltime, _),
    fact_int(Env, _CEnv),
    statistics(walltime, [_,Time3]),
    !, garbage_collect,
    format('fact interpreter took ~d ms~n', [Time3]).
    

run(Benchmark) :-
    print('running: '),
    print(Benchmark),
    nl,

    atom_concat('plbenchs/', Benchmark, Name),
    use_module(Name),
    benchmark(AST, Env),

    prepare_assert_bc(AST),
    !, garbage_collect,
    statistics(walltime, _),
    fact_int(Env, _CEnv),
    statistics(walltime, [_,Time3]),
    !, garbage_collect,
    format('fact interpreter took ~d ms~n', [Time3]).
