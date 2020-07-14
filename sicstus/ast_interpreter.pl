:- module(ast_interpreter, [ast_int/4, eval/4]).

:- use_module(library(plunit)).
:- ensure_loaded(objspace).


%% AST interpreter for a simple language containing:
%    integers
%    arithmetic expressions
%    if-statements
%    while-loops
%    variables?

% interpreter code
ast_int([], Env, _Objspace, Env).
ast_int([H|T], EnvIn, Objspace, EnvOut) :-
    ast_int(H, EnvIn, Objspace, Env),
    ast_int(T, Env, Objspace, EnvOut).
ast_int(if(Cond, Then, Else), EnvIn, Objspace, EnvOut) :-
    eval(Cond, EnvIn, Objspace, X),
    (X == true -> ast_int(Then, EnvIn, Objspace, EnvOut)
               ;  X == false, % just a sanity check, should be removed later
                  ast_int(Else, EnvIn, Objspace, EnvOut)).

% this is a sequential assignment, not a parallel one.
ast_int(assign(id(Var), Expr), EnvIn, Objspace, EnvOut) :-
    eval(Expr, EnvIn, Objspace, Res),
    Objspace:store(EnvIn, Var, Res, EnvOut).

ast_int(while(Cond, Instr, _Invariant, _Variant), EnvIn, Objspace, EnvOut) :- 
    ast_while(Cond,Instr,EnvIn, Objspace, EnvOut).
/* ast_int(while(Cond, Instr, _Invariant, _Variant), EnvIn, Objspace, EnvOut) :- !,
    eval(Cond, EnvIn, Objspace, Res),
    (Res == false
      -> EnvIn = EnvOut
      ;  ast_int(Instr, EnvIn, Objspace, Env),
         ast_int(while(Cond, Instr, _Invariant, _Variant), Env, Objspace, EnvOut)).
         */

ast_while(Cond,Instr,EnvIn, Objspace, EnvOut) :-
    eval(Cond, EnvIn, Objspace, Res),
    (Res == false
      -> EnvIn = EnvOut
      ;  ast_int(Instr, EnvIn, Objspace, Env),
         ast_while(Cond, Instr, Env, Objspace, EnvOut)).

% evaluation predicates

eval(true, _, _Objspace, true) :- !.
eval(false, _, _Objspace, false) :- !.
eval(int(X), _Env, _Objspace, int(X)) :- !.
% should this be X or int(X)? 
% feels like it should be int(X) because this could be a result?

eval(add(LHS, RHS), Env, Objspace, Res) :- !,
    eval(LHS, Env, Objspace, LHSR),
    eval(RHS, Env, Objspace, RHSR),
    Objspace:add(LHSR, RHSR, Res).

eval(sub(LHS, RHS), Env, Objspace, Res) :- !,
    eval(LHS, Env, Objspace, LHSR),
    eval(RHS, Env, Objspace, RHSR),
    Objspace:sub(LHSR, RHSR, Res).

eval(mul(LHS, RHS), Env, Objspace, Res) :- !,
    eval(LHS, Env, Objspace, LHSR),
    eval(RHS, Env, Objspace, RHSR),
    Objspace:mul(LHSR, RHSR, Res).

eval(mod(LHS, RHS), Env, Objspace, Res) :- !,
    eval(LHS, Env, Objspace, LHSR),
    eval(RHS, Env, Objspace, RHSR),
    Objspace:mod(LHSR, RHSR, Res).

eval(le(LHS,RHS), Env, Objspace, Res) :- !,
    eval(LHS, Env, Objspace, LHSR),
    eval(RHS, Env, Objspace, RHSR),
    Objspace:le(LHSR, RHSR, Res).

eval(lt(LHS,RHS), Env, Objspace, Res) :- !,
    eval(LHS, Env, Objspace, LHSR),
    eval(RHS, Env, Objspace, RHSR),
    Objspace:lt(LHSR, RHSR, Res).

eval(ge(LHS,RHS), Env, Objspace, Res) :- !,
    eval(LHS, Env, Objspace, LHSR),
    eval(RHS, Env, Objspace, RHSR),
    Objspace:ge(LHSR, RHSR, Res).

eval(gt(LHS,RHS), Env, Objspace, Res) :- !,
    eval(LHS, Env, Objspace, LHSR),
    eval(RHS, Env, Objspace, RHSR),
    Objspace:gt(LHSR, RHSR, Res).

eval(eq(LHS,RHS), Env, Objspace, Res) :- !,
    eval(LHS, Env, Objspace, LHSR),
    eval(RHS, Env, Objspace, RHSR),
    Objspace:eq(LHSR, RHSR, Res).

eval(not(Expr), Env, Objspace, Res) :-
    eval(Expr, Env, Objspace, R), !,
    Objspace:not(R, Res).

eval(id(X), Env, Objspace, Res) :- !,
    Objspace:lookup(X, Env, Res).


:- begin_tests(eval).

test(integer) :-
    eval(int(3), empty, objspace, X), !, X == int(3).

test(add) :-
    eval(add(int(1), int(2)), empty, objspace, X), !, X == int(3).

test(sub) :-
    eval(sub(int(1), int(2)), empty, objspace, X), !, X == int(-1).

test(mul) :-
    eval(mul(int(3), int(4)), empty, objspace, X), !, X == int(12).

test(composition) :-
    % (2+3) * 4
    eval(mul(add(int(2), int(3)), int(4)), empty, objspace, X), !, X == int(20).

test(less_equal) :-
    eval(le(int(2), int(2)), empty, objspace, X), !, X == true.

test(less_equal2) :-
    eval(le(int(1), int(2)), empty, objspace, X), !, X == true.

test(less_equal3) :-
    eval(le(int(3), int(2)), empty, objspace, X), !, X == false.

test(equal) :-
    eval(eq(int(3), int(2)), empty, objspace, X), !, X == false.

test(equal2) :-
    eval(eq(int(2), int(2)), empty, objspace, X), !, X == true.

test(equal3) :-
    eval(eq(int(1), int(2)), empty, objspace, X), !, X == false.

test(identifier) :-
    list_to_avl([x-int(2)], Env),
    eval(id(x), Env, objspace, X), !, X == int(2).

test(greater_equal) :-
    eval(ge(int(3), int(2)), empty, objspace, X), !, X == true.

test(greater_equal2) :-
    eval(ge(int(2), int(2)), empty, objspace, X), !, X == true.

test(greater_equal3) :-
    eval(ge(int(1), int(2)), empty, objspace, X), !, X == false.

test(greater_than) :-
    eval(gt(int(3), int(2)), empty, objspace, X), !, X == true.

test(greater_than2) :-
    eval(gt(int(2), int(2)), empty, objspace, X), !, X == false.

test(greater_than3) :-
    eval(gt(int(1), int(2)), empty, objspace, X), !, X == false.

test(less_than) :-
    eval(lt(int(2), int(2)), empty, objspace, X), !, X == false.

test(less_than2) :-
    eval(lt(int(1), int(2)), empty, objspace, X), !, X == true.

test(less_than3) :-
    eval(lt(int(3), int(2)), empty, objspace, X), !, X == false.

test(not) :-
    eval(not(true), empty, objspace, X), !, X == false.

test(not2) :-
    eval(not(false), empty, objspace, X), !, X == true.

test(not3) :-
    list_to_avl([x-int(2)], Env),
    eval(not(eq(int(2), id(x))), Env, objspace, X), !, X == false.

test(mod) :-
    eval(mod(int(5), int(3)), empty, objspace, X), !, X == int(2).


:- end_tests(eval).

:- use_module(library(avl)).

:- begin_tests(ast_interpreter).


test(interprete_empty) :-
    empty_avl(Env),
    ast_int([], Env, objspace, Res), !, Res == Env.

test(assignment) :-
    % x := 2
    list_to_avl([a-int(2), x-int(3)], Env),
    ast_int(assign(id(x), int(2)), Env, objspace, Res), !,
    objspace:lookup(x, Res, int(2)),
    objspace:lookup(a, Res, int(2)),
    avl_size(Res, 2).

test(assignment2) :-
    % x := y
    list_to_avl([x-int(2), y-int(3)], Env),
    ast_int(assign(id(x), id(y)), Env, objspace, Res), !,
    objspace:lookup(x, Res, int(3)),
    objspace:lookup(y, Res, int(3)),
    avl_size(Res, 2).

test(assignment3) :-
    % x := y + 1
    list_to_avl([x-int(2), y-int(3)], Env),
    ast_int(assign(id(x), add(int(1), id(y))), Env, objspace, Res), !,
    objspace:lookup(x, Res, int(4)),
    objspace:lookup(y, Res, int(3)),
    avl_size(Res, 2).

test(ite_then) :-
    % if x == 1 then x := 2; y := 2 else x := 3; y := 3
    list_to_avl([x-int(1), y-int(0)], Env),
    ast_int(if(eq(id(x), int(1)),
               [assign(id(x), int(2)), assign(id(y), int(2))],
               [assign(id(x), int(3)), assign(id(y), int(3))]),
            Env, objspace, Res), !,
    objspace:lookup(x, Res, int(2)),
    objspace:lookup(y, Res, int(2)),
    avl_size(Res, 2).

test(ite_else) :-
    % if x == 1 then x := 2; y := 2 else x := 3; y := 3
    list_to_avl([x-int(0), y-int(0)], Env),
    ast_int(if(eq(id(x), int(1)),
               [assign(id(x), int(2)), assign(id(y), int(2))],
               [assign(id(x), int(3)), assign(id(y), int(3))]),
            Env,
            objspace,
            Res), !,
    objspace:lookup(x, Res, int(3)),
    objspace:lookup(y, Res, int(3)),
    avl_size(Res, 2).

test(while_no_execution) :-
    % while x <= 2 do x := x + 1
    % invariant and variant should be implemented later
    list_to_avl([x-int(42)], Env),
    ast_int(while(le(id(x), int(2)), [assign(id(x), add(id(x), int(1)))], _I, _V),
            Env, objspace, Res), !,
    objspace:lookup(x, Res, int(42)),
    avl_size(Res, 1).

test(while_loop) :-
    % while x <= 2 do x := x + 1
    % invariant and variant should be implemented later
    list_to_avl([x-int(-3)], Env),
    ast_int(while(le(id(x), int(2)), [assign(id(x), add(id(x), int(1)))], _I, _V),
            Env, objspace, Res), !,
    objspace:lookup(x, Res, int(3)),
    avl_size(Res, 1).

:- end_tests(ast_interpreter).
