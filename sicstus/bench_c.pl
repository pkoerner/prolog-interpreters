:- use_module(c_driven, [init/0, prepare_c_int/7, run_c_int/6, ast_to_bc_file/3, linear_bytecode/7, list_to_char_ptr/3]).

prepare(Benchmark) :-
    atom_concat('plbenchs/', Benchmark, Name),
    use_module(Name),
    benchmark(AST, Env),
    linear_bytecode(AST, Env, RIdTable, Len, ResBC, EnvBCLen, ResBCEnv),

    atom_concat('tmp/c_', Benchmark, Filename),
    open(Filename, write, Stream),
    tell(Stream),
    write_term(RIdTable, []), write_term('.', []), nl,
    write_term(Len, []), write_term('.', []), nl,
    write_term(ResBC, []), write_term('.', []), nl,
    write_term(EnvBCLen, []), write_term('.', []), nl,
    write_term(ResBCEnv, []), write_term('.', []), nl,
    told.


run_prepared(Benchmark) :-
    print('running: '),
    print(Benchmark),
    nl,

    atom_concat('plbenchs/', Benchmark, Name),
    use_module(Name),
    benchmark(_AST, _Env),

    atom_concat('tmp/c_', Benchmark, Filename),
    open(Filename, read, Stream),
    see(Stream),
    read_term(RIdTable, []),
    read_term(Len, []),
    read_term(ResBC, []),
    read_term(Len2, []),
    read_term(ResBCEnv, []),
    seen,

    init,

    list_to_char_ptr(ResBCEnv, Len2,CE),
    list_to_char_ptr(ResBC, Len, CBC),

    !, garbage_collect,
    statistics(walltime, _),
    run_c_int(RIdTable, CBC, Len, CE, Len2, _DEnv),
    statistics(walltime, [_,Time4]),
    !, garbage_collect,
    format('C interpreter took ~d ms~n', [Time4]).

run(Benchmark) :-
    print('running: '),
    print(Benchmark),
    nl,

    atom_concat('plbenchs/', Benchmark, Name),
    use_module(Name),
    benchmark(AST, Env),

    prepare_c_int(AST, Env, RIdTable, CBC, Len, CE, Len2),
    statistics(walltime, _),
    print('starting'),nl,
    run_c_int(RIdTable, CBC, Len, CE, Len2, DEnv),
    statistics(walltime, [_,Time4]),
    !,
    format('C interpreter took ~d ms~n', [Time4]).
