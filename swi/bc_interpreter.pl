:- module(bc_interpreter, [compare_interpreter_results/2,
                           bc_int_prepare/2,
                           run_bc_int/4]).


:- use_module(ast_interpreter, [ast_int/4]).
:- use_module(compiler, [do_compile/3, decode_integer/2, retrieve_subbytecode/3]).
:- use_module(library(assoc)).
:- use_module(objspace, [assoc_size/2]).

%% TODO:
% it is slightly more performant to store the subbytecodes as a list than
% fetching it from an AVL tree

% bc_int(ByteCode, Env, Stack, ResultEnv, ResultStack)
bc_int([], Env, Stack, _Objspace, Env, Stack). 
% if
bc_int([H|R], Env, Stack, Objspace, REnv, RStack) :-
   bc_int2(H,R, Env, Stack, Objspace, REnv, RStack).

bc_int2(1,[ThenI, ElseI|R], Env, [Cond|Stack], Objspace, REnv, RStack) :-
    (Cond == true -> generated_subbytecodes:sbc(ThenI, Then),
                     h_bc_int(Then, [], Env, Objspace, TEnv)
                  ;  Cond == false,
                     generated_subbytecodes:sbc(ElseI, Else),
                     h_bc_int(Else, [], Env, Objspace, TEnv)),!,
    bc_int(R, TEnv, Stack, Objspace, REnv, RStack).
% while
bc_int2(2, [CondI, InstrI|R], Env, Stack, Objspace, REnv, RStack) :-
    generated_subbytecodes:sbc(CondI, Cond),
    bc_int(Cond, Env, [], Objspace, Env, [Res]),
    (Res == true -> generated_subbytecodes:sbc(InstrI, Instr),
                    h_bc_int(Instr, [], Env, Objspace, TEnv),!,
                    bc_int2(2, [CondI, InstrI|R], TEnv, Stack, Objspace, REnv, RStack)
                 ;  Res == false,!,
                    bc_int(R, Env, Stack, Objspace, REnv, RStack)).
% push
bc_int2(23, [I|R], Env, Stack, Objspace, REnv, RStack) :-
    Objspace:create_integer(I, X),!,
    bc_int(R, Env, [X|Stack], Objspace, REnv, RStack).
%% push1
%bc_int2(20, [Val|R], Env, Stack, Objspace, REnv, RStack) :-
%    decode_integer([Val], I),
%    Objspace:create_integer(I, X),!,
%    bc_int(R, Env, [X|Stack], Objspace, REnv, RStack).
%% push4
%bc_int2(21, [A,B,C,D|R], Env, Stack, Objspace, REnv, RStack) :-
%    decode_integer([A,B,C,D], I),
%    Objspace:create_integer(I, X),!,
%    bc_int(R, Env, [X|Stack], Objspace, REnv, RStack).

% load
bc_int2(40, [Varname|R], Env, Stack, Objspace, REnv, RStack) :-
    Objspace:lookup(Varname, Env, Val),!,
    bc_int(R, Env, [Val|Stack], Objspace, REnv, RStack).
% assign
bc_int2(45, [Varname|R], Env, [Val|Stack], Objspace, REnv, RStack) :-
    Objspace:store(Env, Varname, Val, NewEnv),!,
    bc_int(R, NewEnv, Stack, Objspace, REnv, RStack).

% add
bc_int2(200,R, Env, [Y, X|Stack], Objspace, REnv, RStack) :-
    Objspace:add(X, Y, Res),!,
    bc_int(R, Env, [Res|Stack], Objspace, REnv, RStack).
% sub
bc_int2(199,R, Env, [Y, X|Stack], Objspace, REnv, RStack) :-
    Objspace:sub(X, Y, Res),!,
    bc_int(R, Env, [Res|Stack], Objspace, REnv, RStack).
% mul
bc_int2(198,R, Env, [Y, X|Stack], Objspace, REnv, RStack) :-
    Objspace:mul(X, Y, Res),!,
    bc_int(R, Env, [Res|Stack], Objspace, REnv, RStack).
% mod
bc_int2(197,R, Env, [Y, X|Stack], Objspace, REnv, RStack) :-
    Objspace:mod(X, Y, Res),!,
    bc_int(R, Env, [Res|Stack], Objspace, REnv, RStack).

% not
bc_int2(240,R, Env, [X|Stack], Objspace, REnv, RStack) :-
    Objspace:not(X, Res),!,
    bc_int(R, Env, [Res|Stack], Objspace, REnv, RStack).
% eq
bc_int2(251,R, Env, [Y, X|Stack], Objspace, REnv, RStack) :-
    Objspace:eq(X, Y, Res),!,
    bc_int(R, Env, [Res|Stack], Objspace, REnv, RStack).
% le
bc_int2(252,R, Env, [Y, X|Stack], Objspace, REnv, RStack) :-
    Objspace:le(X, Y, Res),!,
    bc_int(R, Env, [Res|Stack], Objspace, REnv, RStack).
% lt
bc_int2(253,R, Env, [Y, X|Stack], Objspace, REnv, RStack) :-
    Objspace:lt(X, Y, Res),!,
    bc_int(R, Env, [Res|Stack], Objspace, REnv, RStack).
% ge
bc_int2(254,R, Env, [Y, X|Stack], Objspace, REnv, RStack) :-
    Objspace:ge(X, Y, Res),!,
    bc_int(R, Env, [Res|Stack], Objspace, REnv, RStack).
% gt
bc_int2(255,R, Env, [Y, X|Stack], Objspace, REnv, RStack) :-
    Objspace:gt(X, Y, Res),!,
    bc_int(R, Env, [Res|Stack], Objspace, REnv, RStack).




h_bc_int([], _Stack, Env, _Objspace, Env) :- !.
h_bc_int(BC, Stack, Env, Objspace, REnv) :-
    bc_int(BC, Env, Stack, Objspace, REnv, Stack).


write_sbc_as_fact(Index, SubByteCodes) :-
    Index >= 0, !, 
    retrieve_subbytecode(SubByteCodes, Index, Block),
    write_term(sbc(Index, Block), []),
    write_term('.', []),nl,
    Next is Index - 1,
    write_sbc_as_fact(Next, SubByteCodes).
write_sbc_as_fact(X, _SubByteCodes) :-
    X < 0.

generate_bytecode_file(SubByteCodes, Filename) :-
    assoc_size(SubByteCodes, Size),
    open(Filename, write, Stream),
    tell(Stream),
    write_term(':- module(generated_subbytecodes, [sbc/2]).', []),nl,
    (Size == 0 -> write_term('sbc(_, _).', []) ; true),
    Last is Size - 1,
    write_sbc_as_fact(Last, SubByteCodes),
    told.


compare_interpreter_results(AST, Env) :-
    ast_int(AST, Env, objspace, AEnv), !, 
    bc_int_prepare(AST, Codes),
    h_bc_int(Codes, [], Env, objspace, BEnv), !,
    AEnv == BEnv.

bc_int_prepare(AST, Codes) :-
    do_compile(AST, SubByteCodes, Codes),
    Filename = 'generated_subbytecodes.pl',
    generate_bytecode_file(SubByteCodes, Filename),
    unload_file(Filename),
    use_module(Filename, [sbc/2]).

run_bc_int(Codes, Env, Objspace, REnv) :-
    h_bc_int(Codes, [], Env, Objspace, REnv).



:- use_module(library(plunit)).

:- begin_tests(bc_interpreter).

test(push_int) :-
    empty_assoc(Env),
    bc_int([23, 5], Env, [], objspace, NewEnv, NewStack), !,
    NewEnv = t,
    NewStack = [int(5)].

test(add) :-
    empty_assoc(Env),
    bc_int([23, 5, 23, 2, 200], Env, [], objspace, NewEnv, NewStack), !,
    NewEnv = t,
    NewStack = [int(7)].

test(assignment) :-
    empty_assoc(Env),
    AST = [assign(id(x), int(2))],
    compare_interpreter_results(AST, Env).

test(ite) :-
    AST = [if(eq(id(x), int(1)),
              [assign(id(x), int(2)), assign(id(y), int(2))],
              [assign(id(x), int(3)), assign(id(y), int(3))])],
    list_to_assoc([x-int(1), y-int(0)], Env),
    compare_interpreter_results(AST, Env).


test(ite2) :-
    AST = [if(eq(id(x), int(1)),
              [assign(id(x), int(2)), assign(id(y), int(2))],
              [assign(id(x), id(y)), assign(id(y), int(3))])],
    list_to_assoc([x-int(2), y-int(0)], Env),
    compare_interpreter_results(AST, Env).

test(assignment2) :-
    AST = [assign(id(x), id(y))],
    list_to_assoc([x-int(1), y-int(2)], Env),
    compare_interpreter_results(AST, Env).

test(while) :-
    AST = [while(le(id(x), int(3)),
                 [assign(id(x), add(id(x), int(1)))], _, _)],
    list_to_assoc([x-int(0)], Env),
    compare_interpreter_results(AST, Env).

test(sub) :-
    AST = [assign(id(x), sub(int(3), int(2)))],
    compare_interpreter_results(AST, t).

test(mul) :-
    AST = [assign(id(x), mul(int(3), int(2)))],
    compare_interpreter_results(AST, t).

test(mod) :-
    AST = [assign(id(x), mod(int(5), int(2)))],
    compare_interpreter_results(AST, t).

test(not) :-
    AST = [if(not(eq(id(x), int(1))),
              [assign(id(x), int(2)), assign(id(y), int(2))],
              [assign(id(x), int(3)), assign(id(y), int(3))])],
    list_to_assoc([x-int(1), y-int(0)], Env),
    compare_interpreter_results(AST, Env).

test(lt) :-
    AST = [if(lt(id(x), int(1)),
              [assign(id(x), int(2)), assign(id(y), int(2))],
              [assign(id(x), int(3)), assign(id(y), int(3))])],
    list_to_assoc([x-int(1), y-int(0)], Env),
    compare_interpreter_results(AST, Env).

test(lt2) :-
    AST = [if(lt(id(x), int(0)),
              [assign(id(x), int(2)), assign(id(y), int(2))],
              [assign(id(x), int(3)), assign(id(y), int(3))])],
    list_to_assoc([x-int(1), y-int(0)], Env),
    compare_interpreter_results(AST, Env).

test(gt) :-
    AST = [if(gt(id(x), int(1)),
              [assign(id(x), int(2)), assign(id(y), int(2))],
              [assign(id(x), int(3)), assign(id(y), int(3))])],
    list_to_assoc([x-int(1), y-int(0)], Env),
    compare_interpreter_results(AST, Env).

test(gt2) :-
    AST = [if(lt(id(x), int(0)),
              [assign(id(x), int(2)), assign(id(y), int(2))],
              [assign(id(x), int(3)), assign(id(y), int(3))])],
    list_to_assoc([x-int(1), y-int(0)], Env),
    compare_interpreter_results(AST, Env).

test(ge) :-
    AST = [if(gt(id(x), int(2)),
              [assign(id(x), int(2)), assign(id(y), int(2))],
              [assign(id(x), int(3)), assign(id(y), int(3))])],
    list_to_assoc([x-int(1), y-int(0)], Env),
    compare_interpreter_results(AST, Env).

test(ge2) :-
    AST = [if(lt(id(x), int(1)),
              [assign(id(x), int(2)), assign(id(y), int(2))],
              [assign(id(x), int(3)), assign(id(y), int(3))])],
    list_to_assoc([x-int(1), y-int(0)], Env),
    compare_interpreter_results(AST, Env).

:- end_tests(bc_interpreter).
