:- module(generator, [gen_program/1]).

:- use_module(library(lists)).
:- use_module(library(random)).

val(int(0)).
val(int(1)).
val(int(2)).
val(int(3)).
val(int(-1)).
vals([int(-1), int(0), int(1), int(2), int(3)]).

identifier(id(x)).
identifier(id(y)).
identifier(id(z)).
identifier(id(a)).
identifier(id(b)).
identifiers([id(a), id(b), id(x), id(y), id(z)]).

comparison(lt).
comparison(gt).
comparison(ge).
comparison(le).
comparison(eq).
comparisons([lt, gt, ge, le, eq]).

arithmetic(add).
arithmetic(sub).
%arithmetic(mul).
%arithmetic(mod). % modulo is hard since mod 0 is undefined
arithmetics([add, sub]).

c_expr(X) :-
    comparisons(Cs),
    random_member(C, Cs),
    X =.. [C, Y, Z],
    a_expr(Y), a_expr(Z).
    
a_expr(X) :-
    vals(Vs),
    identifiers(Ids),
    arithmetics(As),
    append([Vs, Ids, As], Os),
    random_member(O, Os),
    (arithmetic(O) -> X =.. [O, Y, Z],
                      a_expr(Y),
                      a_expr(Z)
                    ; X = O).
    
statements([while, assign, if]).

statement(while, [assign(id(TmpVar), int(0)), while(Cond, Body, _, _)], Depth) :-
    % generate unique identifier
    number_chars(Depth, DepthC),
    atom_chars(DepthA, DepthC),
    atom_concat(loop_var, DepthA, TmpVar),
    Cond = lt(id(TmpVar), int(20)), % TODO: number of loop iterations
    %Cond = lt(id(TmpVar), int(100)), % TODO: number of loop iterations
    Depth1 is Depth + 1,
    gen_statements(S, Depth1),
    Body = [assign(id(TmpVar), add(id(TmpVar), int(1)))|S].
    
statement(if, if(Cond, Then, Else), Depth) :-
    c_expr(Cond),
    Depth1 is Depth+1,
    gen_statements(Then, Depth1),
    gen_statements(Else, Depth1).

statement(assign, assign(Var, Val), _) :-
    identifiers(Ids),
    random_member(Var, Ids),
    a_expr(Val).
    

gen_statements(_, Depth, _, [Res]) :-
    %Depth > 2, !,
    Depth >= 3, !,
    statement(assign, Res, Depth).
gen_statements(0, _, Res, Res).
gen_statements(Amount, Depth, Acc, Res) :-
    Amount > 0,
    statements(Ss),
    random_member(E, Ss),
    statement(E, X, Depth),
    Amount1 is Amount - 1,
    (is_list(X) -> X = [Assignment, While],
                   gen_statements(Amount1, Depth, [Assignment, While|Acc], Res)
                 ; gen_statements(Amount1, Depth, [X|Acc], Res)).

gen_statements(S, Depth) :-
    % random(1, 10, Amount),
    random(20, 50, Amount),
    gen_statements(Amount, Depth, [], S).
    
gen_program(Program) :-
    gen_statements(Program, 0).
