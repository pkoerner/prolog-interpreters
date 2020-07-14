:- use_module(rational_trees, [thread_program/2, rt_int/4]).

prepare(Benchmark) :-
    atom_concat('plbenchs/', Benchmark, Name),
    use_module(Name),
    benchmark(AST, _Env),
    thread_program(AST, Threaded),

    atom_concat('tmp/rt_', Benchmark, Filename),
    open(Filename, write, Stream),
    tell(Stream),
    write_term(Threaded, [cycles(true)]), write('.'), nl,
    told.


run_prepared(Benchmark) :-
    print('running: '),
    print(Benchmark),
    nl,

    atom_concat('plbenchs/', Benchmark, Name),
    use_module(Name),
    benchmark(_AST, Env),

    atom_concat('tmp/rt_', Benchmark, Filename),
    open(Filename, read, Stream),
    see(Stream),
    read_term(Threaded, [cycles(true)]),
    seen,

    !, garbage_collect,
    statistics(walltime, _),
    rt_int(Threaded, Env, objspace, _EEnv),
    statistics(walltime, [_,Time5]),
    !, garbage_collect,
    format('rational tree interpreter took ~d ms~n', [Time5]).


run(Benchmark) :-
    print('running: '),
    print(Benchmark),
    nl,

    atom_concat('plbenchs/', Benchmark, Name),
    use_module(Name),
    benchmark(AST, Env),

    thread_program(AST, Threaded),
    !, garbage_collect,
    statistics(walltime, _),
    rt_int(Threaded, Env, objspace, EEnv),
    statistics(walltime, [_,Time5]),
    !, garbage_collect,
    format('rational tree interpreter took ~d ms~n', [Time5]).
