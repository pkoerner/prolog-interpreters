:- use_module(bc_interpreter, [bc_int_prepare/2, run_bc_int/4]).

prepare(Benchmark) :-
    atom_concat('plbenchs/', Benchmark, Name),
    use_module(Name),
    benchmark(AST, _Env),
    bc_int_prepare(AST, Codes),

    atom_concat('tmp/bc_', Benchmark, Filename),
    open(Filename, write, Stream),
    tell(Stream),
    write(Codes),
    write('.'), nl,
    told.
    

run_prepared(Benchmark) :-
    print('running: '),
    print(Benchmark),
    nl,

    atom_concat('plbenchs/', Benchmark, Name),
    use_module(Name),
    benchmark(_AST, Env),

    atom_concat('tmp/bc_', Benchmark, Filename),
    open(Filename, read, Stream),
    see(Stream),
    read(Codes),
    seen,
    use_module(generated_subbytecodes, [sbc/2]),

    !, garbage_collect,
    statistics(walltime, _),
    run_bc_int(Codes, Env, objspace, _BEnv),
    statistics(walltime, [_,Time2]),
    !, garbage_collect,
    format('BC interpreter took ~d ms~n',   [Time2]).


run(Benchmark) :-
    print('running: '),
    print(Benchmark),
    nl,

    atom_concat('plbenchs/', Benchmark, Name),
    use_module(Name),
    benchmark(AST, Env),

    bc_int_prepare(AST, Codes),
    !, garbage_collect,
    statistics(walltime, _),
    run_bc_int(Codes, Env, objspace, _BEnv),
    statistics(walltime, [_,Time2]),
    !, garbage_collect,
    format('BC interpreter took ~d ms~n',   [Time2]).
