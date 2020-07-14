:- module(rational_trees, [thread_program/2, rt_int/4]).
:- use_module(ast_interpreter, [ast_int/4, eval/4]).
:- use_module(library(avl)).

thread_program(Program, Threaded) :-
    h_thread_program(Program, Threaded-end).
h_thread_program([], A-A).
h_thread_program([Inst|Next], Threaded-X) :-
    thread_instruction(Inst, Threaded, RestThreaded),
    h_thread_program(Next, RestThreaded-X).

thread_instruction(assign(id(Var), Val), assign(id(Var), Val, Next), Next).
thread_instruction(if(Cond, Then, Else), if(Cond, TThen, TElse), Next) :-
    h_thread_program(Then, TThen-Next),
    h_thread_program(Else, TElse-Next).
thread_instruction(while(Cond, Instrs, _, _), while(Cond, TInstrs, Next), Next) :-
    h_thread_program(Instrs, TInstrs-TWhile),
    TWhile = while(Cond, TInstrs, Next).


rt_int(end, Env, _, Env) :- !.
rt_int(assign(id(Var), Expr, Next), Env, Objspace, REnv) :-
    eval(Expr, Env, Objspace, Res),
    Objspace:store(Env, Var, Res, EnvOut), !,
    rt_int(Next, EnvOut, Objspace, REnv).
rt_int(if(Cond, Then, Else), Env, Objspace, REnv) :-
    eval(Cond, Env, Objspace, V),
    (V == true -> !, rt_int(Then, Env, Objspace, REnv)
                ; !, rt_int(Else, Env, Objspace, REnv)).
rt_int(while(Cond, Instrs, Else), Env, Objspace, REnv) :-
    eval(Cond, Env, Objspace, V),
    (V == true -> !, rt_int(Instrs, Env, Objspace, REnv)
                ; !, rt_int(Else, Env, Objspace, REnv)).


:- use_module(library(plunit)).

:- begin_tests(to_rational_trees).

test(empty) :-
    thread_program([], X), !, ground(X),
    X == end.

test(one_assignment) :-
    thread_program([assign(id(x), int(1))], X),
    !, ground(X),
    X == assign(id(x), int(1), end).

test(two_assignments) :-
    thread_program([assign(id(x), int(1)),
                    assign(id(y), int(2))], X),
    !, ground(X),
    X == assign(id(x), int(1), assign(id(y), int(2), end)).

test(three_assignments) :-
    thread_program([assign(id(x), int(1)),
                    assign(id(y), int(2)),
                    assign(id(z), int(3))], X),
    !, ground(X),
    X == assign(id(x), int(1), assign(id(y), int(2), assign(id(z), int(3), end))).

test(one_if_statement) :-
    thread_program([if(eq(id(x), int(0)),
                       [assign(id(x), int(1))],
                       [assign(id(x), int(0))])], X),
    !, ground(X),
    X == if(eq(id(x), int(0)), assign(id(x), int(1), end), assign(id(x), int(0), end)).

test(if_and_assign) :-
    thread_program([if(eq(id(x), int(0)),
                       [assign(id(x), int(1))],
                       [assign(id(x), int(0))]),
                    assign(id(y), int(3))], X),
    !, ground(X),
    X == if(eq(id(x), int(0)), assign(id(x), int(1), assign(id(y), int(3), end)),
                               assign(id(x), int(0), assign(id(y), int(3), end))).

test(if_and_two_assigns) :-
    thread_program([if(eq(id(x), int(0)),
                       [assign(id(x), int(1))],
                       [assign(id(x), int(0))]),
                    assign(id(y), int(3)),
                    assign(id(z), int(4))], X),
    !, ground(X),
    X == if(eq(id(x), int(0)), assign(id(x), int(1), assign(id(y), int(3), assign(id(z), int(4), end))),
                               assign(id(x), int(0), assign(id(y), int(3), assign(id(z), int(4), end)))).

test(nested_ifs) :-
    thread_program([if(eq(id(x), int(0)),
                       [if(true,
                           [assign(id(y), int(0))],
                           [assign(id(y), int(1))])],
                       [if(true,
                           [assign(id(z), int(0))],
                           [assign(id(z), int(1))])])], X),
    !, ground(X),
    X == if(eq(id(x), int(0)), if(true, assign(id(y), int(0), end),
                                        assign(id(y), int(1), end)),
                               if(true, assign(id(z), int(0), end),
                                        assign(id(z), int(1), end))).

test(nested_ifs_assignment) :-
    thread_program([if(eq(id(x), int(0)),
                       [if(true,
                           [assign(id(y), int(0))],
                           [assign(id(y), int(1))])],
                       [if(true,
                           [assign(id(z), int(0))],
                           [assign(id(z), int(1))])]),
                    assign(id(x), int(42))], X),
    !, ground(X),
    X == if(eq(id(x), int(0)), if(true, assign(id(y), int(0), assign(id(x), int(42), end)),
                                        assign(id(y), int(1), assign(id(x), int(42), end))),
                               if(true, assign(id(z), int(0), assign(id(x), int(42), end)),
                                        assign(id(z), int(1), assign(id(x), int(42), end)))).

test(if_with_two_assignments_in_body) :-
    thread_program([if(eq(id(x), int(0)),
                       [assign(id(x), int(1)), assign(id(y), int(3))],
                       [assign(id(x), int(0))]),
                    assign(id(z), int(4))], X),
    !, ground(X),
    X == if(eq(id(x), int(0)), assign(id(x), int(1), assign(id(y), int(3), assign(id(z), int(4), end))),
                               assign(id(x), int(0), assign(id(z), int(4), end))).

test(while) :-
    thread_program([while(true, [assign(id(x), int(1))], _, _)], X),
    !, ground(X),
    X = while(true, assign(id(x), int(1),
                           while(true, assign(id(x), int(1),
                                              while(true, assign(id(x), int(1), _), end)),
                                 end)),
                    end).

test(while_and_assignment) :-
    thread_program([while(true, [assign(id(x), int(1))], _, _), assign(id(y), int(3))], X),
    !, ground(X),
    X = while(true, assign(id(x), int(1),
                          while(true, assign(id(x), int(1),
                                             while(true, assign(id(x), int(1), _),
                                                   assign(id(y), int(3), end))),
                                assign(id(y), int(3), end))),
              assign(id(y), int(3), end)).

:- end_tests(to_rational_trees).

compare_interpreter_results(AST, Env) :-
    ast_int(AST, Env, objspace, AEnv),
    thread_program(AST, Threaded),
    rt_int(Threaded, Env, objspace, BEnv), !,
    AEnv == BEnv.

:- begin_tests(rt_interpreter).


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

:- end_tests(rt_interpreter).
