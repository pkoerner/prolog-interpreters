:- use_module(ast_interpreter, [ast_int/4]).
:- use_module(parser, [parse/2]).

run(Benchmark) :-
    print('running: '),
    print(Benchmark),
    nl,

    atom_concat('plbenchs/', Benchmark, Name),
    use_module(Name),
    benchmark(AST, Env),

    format('Starting AST interpreter~n',  []),
    !, garbage_collect,
    statistics(walltime, _),
    ast_int(AST, Env, objspace, _AEnv),
    statistics(walltime, [_,Time1]),
    !, garbage_collect,

    format('AST interpreter took ~d ms~n',  [Time1]).
