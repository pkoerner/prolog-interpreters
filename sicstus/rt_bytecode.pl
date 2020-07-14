:- module(rt_bytecode, [thread_bc/3]).

:- use_module(ast_interpreter, [ast_int/4]).
:- use_module(compiler, [do_compile/3, retrieve_subbytecode/3]).
:- use_module(library(avl)).


thread_bc(ByteCode, SubByteCodes, Res) :-
    h_thread_bc(ByteCode, SubByteCodes, Res, end).

h_thread_bc([], _SubByteCodes, End, End).
h_thread_bc([1, ThenI, ElseI|T], SubByteCodes, if(ThenNext, ElseNext), End) :-
    retrieve_subbytecode(SubByteCodes, ThenI, Then),
    retrieve_subbytecode(SubByteCodes, ElseI, Else),
    h_thread_bc(Then, SubByteCodes, ThenNext, Next),
    h_thread_bc(Else, SubByteCodes, ElseNext, Next),
    h_thread_bc(T, SubByteCodes, Next, End).
h_thread_bc([2, CondI, InstrI|T], SubByteCodes, CondNext, End) :-
    retrieve_subbytecode(SubByteCodes, CondI, Cond),
    retrieve_subbytecode(SubByteCodes, InstrI, Instr),
    h_thread_bc(Cond, SubByteCodes, CondNext, if(InstrNext, Next)),
    h_thread_bc(Instr, SubByteCodes, InstrNext, CondNext),
    h_thread_bc(T, SubByteCodes, Next, End).

h_thread_bc([23, Arg|T], SubByteCodes, push(Arg, Next), End) :-
    h_thread_bc(T, SubByteCodes, Next, End).
h_thread_bc([40, Arg|T], SubByteCodes, load(Arg, Next), End) :-
    h_thread_bc(T, SubByteCodes, Next, End).
h_thread_bc([45, Arg|T], SubByteCodes, assign(Arg, Next), End) :-
    h_thread_bc(T, SubByteCodes, Next, End).
h_thread_bc([200|T], SubByteCodes, add(Next), End) :-
    h_thread_bc(T, SubByteCodes, Next, End).
h_thread_bc([199|T], SubByteCodes, sub(Next), End) :-
    h_thread_bc(T, SubByteCodes, Next, End).
h_thread_bc([198|T], SubByteCodes, mul(Next), End) :-
    h_thread_bc(T, SubByteCodes, Next, End).
h_thread_bc([197|T], SubByteCodes, mod(Next), End) :-
    h_thread_bc(T, SubByteCodes, Next, End).
h_thread_bc([240|T], SubByteCodes, not(Next), End) :-
    h_thread_bc(T, SubByteCodes, Next, End).
h_thread_bc([251|T], SubByteCodes, eq(Next), End) :-
    h_thread_bc(T, SubByteCodes, Next, End).
h_thread_bc([252|T], SubByteCodes, le(Next), End) :-
    h_thread_bc(T, SubByteCodes, Next, End).
h_thread_bc([253|T], SubByteCodes, lt(Next), End) :-
    h_thread_bc(T, SubByteCodes, Next, End).
h_thread_bc([254|T], SubByteCodes, ge(Next), End) :-
    h_thread_bc(T, SubByteCodes, Next, End).
h_thread_bc([255|T], SubByteCodes, gt(Next), End) :-
    h_thread_bc(T, SubByteCodes, Next, End).

rt_bc_int(end, Env, Stack, _Objspace, Env, Stack).
rt_bc_int(if(Then, Else), Env, [X|Stack], Objspace, REnv, RStack) :-
    (Objspace:is_truthy(X) -> !, rt_bc_int(Then, Env, Stack, Objspace, REnv, RStack)
                            ; !, rt_bc_int(Else, Env, Stack, Objspace, REnv, RStack)).
rt_bc_int(push(Arg, Next), Env, Stack, Objspace, REnv, RStack) :-
    Objspace:create_integer(Arg, Val),!,
    rt_bc_int(Next, Env, [Val|Stack], Objspace, REnv, RStack).
rt_bc_int(load(Arg, Next), Env, Stack, Objspace, REnv, RStack) :-
    Objspace:lookup(Arg, Env, Val), !,
    rt_bc_int(Next, Env, [Val|Stack], Objspace, REnv, RStack).
rt_bc_int(assign(Arg, Next), Env, [Val|Stack], Objspace, REnv, RStack) :-
    Objspace:store(Env, Arg, Val, NewEnv), !,
    rt_bc_int(Next, NewEnv, Stack, Objspace, REnv, RStack).
rt_bc_int(add(Next), Env, [Y, X|Stack], Objspace, REnv, RStack) :-
    Objspace:add(X, Y, Res), !,
    rt_bc_int(Next, Env, [Res|Stack], Objspace, REnv, RStack).
rt_bc_int(sub(Next), Env, [Y, X|Stack], Objspace, REnv, RStack) :-
    Objspace:sub(X, Y, Res), !,
    rt_bc_int(Next, Env, [Res|Stack], Objspace, REnv, RStack).
rt_bc_int(mul(Next), Env, [Y, X|Stack], Objspace, REnv, RStack) :-
    Objspace:mul(X, Y, Res), !,
    rt_bc_int(Next, Env, [Res|Stack], Objspace, REnv, RStack).
rt_bc_int(mod(Next), Env, [Y, X|Stack], Objspace, REnv, RStack) :-
    Objspace:mod(X, Y, Res), !,
    rt_bc_int(Next, Env, [Res|Stack], Objspace, REnv, RStack).
rt_bc_int(not(Next), Env, [X|Stack], Objspace, REnv, RStack) :-
    Objspace:not(X, Res), !,
    rt_bc_int(Next, Env, [Res|Stack], Objspace, REnv, RStack).
rt_bc_int(eq(Next), Env, [Y, X|Stack], Objspace, REnv, RStack) :-
    Objspace:eq(X, Y, Res), !,
    rt_bc_int(Next, Env, [Res|Stack], Objspace, REnv, RStack).
rt_bc_int(le(Next), Env, [Y, X|Stack], Objspace, REnv, RStack) :-
    Objspace:le(X, Y, Res), !,
    rt_bc_int(Next, Env, [Res|Stack], Objspace, REnv, RStack).
rt_bc_int(lt(Next), Env, [Y, X|Stack], Objspace, REnv, RStack) :-
    Objspace:lt(X, Y, Res), !,
    rt_bc_int(Next, Env, [Res|Stack], Objspace, REnv, RStack).
rt_bc_int(ge(Next), Env, [Y, X|Stack], Objspace, REnv, RStack) :-
    Objspace:ge(X, Y, Res), !,
    rt_bc_int(Next, Env, [Res|Stack], Objspace, REnv, RStack).
rt_bc_int(gt(Next), Env, [Y, X|Stack], Objspace, REnv, RStack) :-
    Objspace:gt(X, Y, Res), !,
    rt_bc_int(Next, Env, [Res|Stack], Objspace, REnv, RStack).


compare_interpreter_results(AST, Env) :-
    do_compile(AST, SubByteCodes, Codes), !,
    thread_bc(Codes, SubByteCodes, Tree), !,
    ast_int(AST, Env, objspace, AEnv), !, 
    rt_bc_int(Tree, Env, [], objspace, REnv, []),
    AEnv == REnv.




:- use_module(library(plunit)).

:- begin_tests(thread_bc).

test(empty) :-
    thread_bc([], empty, X), !, X == end.

test(simple_linear) :-
    thread_bc([23, 5, 23, 2, 200], empty, X), !,
    X == push(5, push(2, add(end))).

test(simple_if) :-
    ByteCode = [23, 1, 23, 1, 251, 1, 0, 1, 23, 42, 45, y],
    list_to_avl([0-[23, 11, 45, x], 1-[23, 22, 45, x]], SBC),
    thread_bc(ByteCode, SBC, X), !,
    X == push(1, push(1, eq(if(push(11, assign(x, push(42, assign(y, end)))),
                               push(22, assign(x, push(42, assign(y, end)))))))).

test(while) :-
    ByteCode = [2, 0, 1, 23, 0, 45, x],
    list_to_avl([0-[40, x, 23, 5, 253], 1-[23, 22, 45, x]], SBC),
    thread_bc(ByteCode, SBC, X), !,
    ground(X),
    X = load(x, push(5, lt(if(push(22, assign(x, load(x, push(5, lt(if(push(22, assign(x, load(x, _))), 
                                                                       push(0, assign(x, end)))))))),
                              push(0, assign(x, end)))))).


:- end_tests(thread_bc).


:- begin_tests(bc_interpreter).

test(assignment) :-
    empty_avl(Env),
    AST = [assign(id(x), int(2))],
    compare_interpreter_results(AST, Env).

test(ite) :-
    AST = [if(eq(id(x), int(1)),
              [assign(id(x), int(2)), assign(id(y), int(2))],
              [assign(id(x), int(3)), assign(id(y), int(3))])],
    list_to_avl([x-int(1), y-int(0)], Env),
    compare_interpreter_results(AST, Env).


test(ite2) :-
    AST = [if(eq(id(x), int(1)),
              [assign(id(x), int(2)), assign(id(y), int(2))],
              [assign(id(x), id(y)), assign(id(y), int(3))])],
    list_to_avl([x-int(2), y-int(0)], Env),
    compare_interpreter_results(AST, Env).

test(assignment2) :-
    AST = [assign(id(x), id(y))],
    list_to_avl([x-int(1), y-int(2)], Env),
    compare_interpreter_results(AST, Env).

test(while) :-
    AST = [while(le(id(x), int(3)),
                 [assign(id(x), add(id(x), int(1)))], _, _)],
    list_to_avl([x-int(0)], Env),
    compare_interpreter_results(AST, Env).

test(sub) :-
    AST = [assign(id(x), sub(int(3), int(2)))],
    compare_interpreter_results(AST, empty).

test(mul) :-
    AST = [assign(id(x), mul(int(3), int(2)))],
    compare_interpreter_results(AST, empty).

test(mod) :-
    AST = [assign(id(x), mod(int(5), int(2)))],
    compare_interpreter_results(AST, empty).

test(not) :-
    AST = [if(not(eq(id(x), int(1))),
              [assign(id(x), int(2)), assign(id(y), int(2))],
              [assign(id(x), int(3)), assign(id(y), int(3))])],
    list_to_avl([x-int(1), y-int(0)], Env),
    compare_interpreter_results(AST, Env).

test(lt) :-
    AST = [if(lt(id(x), int(1)),
              [assign(id(x), int(2)), assign(id(y), int(2))],
              [assign(id(x), int(3)), assign(id(y), int(3))])],
    list_to_avl([x-int(1), y-int(0)], Env),
    compare_interpreter_results(AST, Env).

test(lt2) :-
    AST = [if(lt(id(x), int(0)),
              [assign(id(x), int(2)), assign(id(y), int(2))],
              [assign(id(x), int(3)), assign(id(y), int(3))])],
    list_to_avl([x-int(1), y-int(0)], Env),
    compare_interpreter_results(AST, Env).

test(gt) :-
    AST = [if(gt(id(x), int(1)),
              [assign(id(x), int(2)), assign(id(y), int(2))],
              [assign(id(x), int(3)), assign(id(y), int(3))])],
    list_to_avl([x-int(1), y-int(0)], Env),
    compare_interpreter_results(AST, Env).

test(gt2) :-
    AST = [if(lt(id(x), int(0)),
              [assign(id(x), int(2)), assign(id(y), int(2))],
              [assign(id(x), int(3)), assign(id(y), int(3))])],
    list_to_avl([x-int(1), y-int(0)], Env),
    compare_interpreter_results(AST, Env).

test(ge) :-
    AST = [if(gt(id(x), int(2)),
              [assign(id(x), int(2)), assign(id(y), int(2))],
              [assign(id(x), int(3)), assign(id(y), int(3))])],
    list_to_avl([x-int(1), y-int(0)], Env),
    compare_interpreter_results(AST, Env).

test(ge2) :-
    AST = [if(lt(id(x), int(1)),
              [assign(id(x), int(2)), assign(id(y), int(2))],
              [assign(id(x), int(3)), assign(id(y), int(3))])],
    list_to_avl([x-int(1), y-int(0)], Env),
    compare_interpreter_results(AST, Env).

:- end_tests(bc_interpreter).
