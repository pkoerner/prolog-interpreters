:- use_module(rt_bytecode, [thread_bc/3, rt_bc_int/6]).
:- use_module(compiler, [do_compile/3]).

prepare(Benchmark) :-
    atom_concat('plbenchs/', Benchmark, Name),
    use_module(Name),
    benchmark(AST, _Env),

    do_compile(AST, SubByteCodes, Codes), !,
    thread_bc(Codes, SubByteCodes, Tree), !,

    atom_concat('tmp/rt_bc', Benchmark, Filename),
    open(Filename, write, Stream),
    tell(Stream),
    write_term(Tree, []), write_term('.', []), nl,
    told.


run_prepared(Benchmark) :-
    print('running: '),
    print(Benchmark),
    nl,

    atom_concat('plbenchs/', Benchmark, Name),
    use_module(Name),
    benchmark(_AST, Env),

    atom_concat('tmp/rt_bc', Benchmark, Filename),
    open(Filename, read, Stream),
    see(Stream),
    read_term(Tree, [cycles(true)]),
    seen,

    !, garbage_collect,
    statistics(walltime, _),
    rt_bc_int(Tree, Env, [], objspace, REnv, []),
    statistics(walltime, [_,Time]),
    !, garbage_collect,
    format('rational tree interpreter with bytecodes took ~d ms~n', [Time]).


