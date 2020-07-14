:- module(assert_bc, [bc_to_terms/4,
                      prepare_assert_bc/1,
                      fact_int/2]).

:- use_module(ast_interpreter, [ast_int/4]).
:- use_module(compiler, [do_compile/3, encode_integer/3, decode_integer/2, retrieve_subbytecode/3]).
:- use_module(library(assoc)).

% TODO:
% - should integers be converted back already?
% - does the PC actually matter?
%

%bc_to_terms(BC, SBC, Start, End, Acc, Res) :- !,
bc_to_terms([], _, End, End, Acc, Acc).
% if
bc_to_terms([1, ThenI, ElseI|R], SBC, Start, End, Acc-T, Res) :- !,
    retrieve_subbytecode(SBC, ThenI, Then),
    retrieve_subbytecode(SBC, ElseI, Else), !,
    T-ThenDL = [bc(Start, 11, ElseStart)|ThenDL]-ThenDL,
    ThenStart is Start + 5,
    bc_to_terms(Then, SBC, ThenStart, ThenEnd, A-A, ThenDL-JmpDL),
    JmpDL-ElseDL = [bc(ThenEnd, 10, ElseEnd)|ElseDL]-ElseDL,
    ElseStart is ThenEnd + 5,
    bc_to_terms(Else, SBC, ElseStart, ElseEnd, B-B, ElseDL-DL),
    bc_to_terms(R, SBC, ElseEnd, End, Acc-DL, Res).
% while
bc_to_terms([2, CondI, InstrI|R], SBC, Start, End, Acc-T, Res) :- !,
    retrieve_subbytecode(SBC, CondI, Cond),
    retrieve_subbytecode(SBC, InstrI, Instr), !,
    bc_to_terms(Cond, SBC, Start, CondEnd, A-A, T-JmpDL),
    JmpDL-InstrDL = [bc(CondEnd, 11, WhileEnd)|InstrDL]-InstrDL,
    InstrStart is CondEnd + 5,
    bc_to_terms(Instr, SBC, InstrStart, InstrEnd, B-B, InstrDL-JmpDL2),
    JmpDL2-DL = [bc(InstrEnd, 10, Start)|DL]-DL,
    WhileEnd is InstrEnd + 5,
    bc_to_terms(R, SBC, WhileEnd, End, Acc-DL, Res).
% push
bc_to_terms([23, Val|R], SBC, Start, End, Acc-T, Res) :- !,
    T-DL = [bc(Start, 23, Val)|DL]-DL,
    %encode_integer(Val, Len, _),
    Len = 1,
    NStart is Start + Len + 1,
    bc_to_terms(R, SBC, NStart, End, Acc-DL, Res).
%% push1
%bc_to_terms([20, I|R], SBC, Start, End, Acc-T, Res) :- !,
%    decode_integer([I], Val),
%    T-DL = [bc(Start, 20, Val)|DL]-DL,
%    NStart is Start + 2,
%    bc_to_terms(R, SBC, NStart, End, Acc-DL, Res).
%% push4
%bc_to_terms([21, A, B, C, D|R], SBC, Start, End, Acc-T, Res) :- !,
%    decode_integer([A, B, C, D], Val),
%    T-DL = [bc(Start, 21, Val)|DL]-DL,
%    NStart is Start + 5,
%    bc_to_terms(R, SBC, NStart, End, Acc-DL, Res).
% load
bc_to_terms([40, Name|R], SBC, Start, End, Acc-T, Res) :- !,
    T-DL = [bc(Start, 40, Name)|DL]-DL,
    NStart is Start + 5,
    bc_to_terms(R, SBC, NStart, End, Acc-DL, Res).
% assign
bc_to_terms([45, Name|R], SBC, Start, End, Acc-T, Res) :- !,
    T-DL = [bc(Start, 45, Name)|DL]-DL,
    NStart is Start + 5,
    bc_to_terms(R, SBC, NStart, End, Acc-DL, Res).
% arithmetic
bc_to_terms([Arithmetic|R], SBC, Start, End, Acc-T, Res) :-
    Arithmetic >= 197, Arithmetic =< 200,
    T-DL = [bc(Start, Arithmetic, [])|DL]-DL,
    NStart is Start + 1,
    bc_to_terms(R, SBC, NStart, End, Acc-DL, Res).

% not
bc_to_terms([240|R], SBC, Start, End, Acc-T, Res) :-
    T-DL = [bc(Start, 240, [])|DL]-DL,
    NStart is Start + 1,
    bc_to_terms(R, SBC, NStart, End, Acc-DL, Res).

% comparisons
bc_to_terms([Comparison|R], SBC, Start, End, Acc-T, Res) :-
    Comparison >= 251, Comparison =< 255,
    T-DL = [bc(Start, Comparison, [])|DL]-DL,
    NStart is Start + 1,
    bc_to_terms(R, SBC, NStart, End, Acc-DL, Res).


bc_to_terms(BC, SBC, End, Res) :-
    bc_to_terms(BC, SBC, 0, End, A-A, Res-[]).



assert_bcs(BC, SBC) :-
    bc_to_terms(BC, SBC, End, Res),
    Filename = 'generated.pl',
    open(Filename, write, Stream),
    tell(Stream),
    write_term(':- module(generated, [bc2/3]).', []), nl,
    assert_terms([bc(End, 0, [])|Res]),
    told,
    unload_file(generated),
    use_module(generated, [bc2/3]).

assert_terms([]).
assert_terms([bc(A,B,C)|T]) :-
    %assert(H),
    write_term(bc2(A,B,C), []),
    write_term('.', []),
    nl,
    assert_terms(T).


fact_int(Env, REnv) :-
    fact_int(0, objspace, Env, [], REnv).

fact_int(PC, Objspace, Env, Stack, REnv) :- 
    generated:bc2(PC, Instr, Args),
    fact_int(Instr, Args, PC, Stack, Env, Objspace, REnv).

fact_int(0, _NewPC, _PC, _Stack, Env, _Objspace, Env).
% jump
fact_int(10, NewPC, _PC, Stack, Env, Objspace, REnv) :-
    fact_int(NewPC, Objspace, Env, Stack, REnv).
% jump-if-false
fact_int(11, Target, PC, [X|Stack], Env, Objspace, REnv) :-
    (X == true -> NewPC is PC + 5
                ; NewPC = Target),
    fact_int(NewPC, Objspace, Env, Stack, REnv).
% push
fact_int(23, Val, PC, Stack, Env, Objspace, REnv) :-
    NewPC is PC + 2,
    Objspace:create_integer(Val, X),
    fact_int(NewPC, Objspace, Env, [X|Stack], REnv).
%% push1
%fact_int(20, Val, PC, Stack, Env, Objspace, REnv) :-
%    NewPC is PC + 2,
%    Objspace:create_integer(Val, X),
%    fact_int(NewPC, Objspace, Env, [X|Stack], REnv).
%% push4
%fact_int(21, Val, PC, Stack, Env, Objspace, REnv) :-
%    NewPC is PC + 5,
%    Objspace:create_integer(Val, X),
%    fact_int(NewPC, Objspace, Env, [X|Stack], REnv).
% load
fact_int(40, Name, PC, Stack, Env, Objspace, REnv) :-
    NewPC is PC + 5,
    Objspace:lookup(Name, Env, Val),
    fact_int(NewPC, Objspace, Env, [Val|Stack], REnv).
% assign
fact_int(45, Name, PC, [Val|Stack], Env, Objspace, REnv) :-
    NewPC is PC + 5,
    Objspace:store(Env, Name, Val, NewEnv),
    fact_int(NewPC, Objspace, NewEnv, Stack, REnv).
% mod
fact_int(197, _Args, PC, [Y, X|Stack], Env, Objspace, REnv) :-
    NewPC is PC + 1,
    Objspace:mod(X, Y, Res),
    fact_int(NewPC, Objspace, Env, [Res|Stack], REnv).
% mul
fact_int(198, _Args, PC, [Y, X|Stack], Env, Objspace, REnv) :-
    NewPC is PC + 1,
    Objspace:mul(X, Y, Res),
    fact_int(NewPC, Objspace, Env, [Res|Stack], REnv).
% sub
fact_int(199, _Args, PC, [Y, X|Stack], Env, Objspace, REnv) :-
    NewPC is PC + 1,
    Objspace:sub(X, Y, Res),
    fact_int(NewPC, Objspace, Env, [Res|Stack], REnv).
% add
fact_int(200, _Args, PC, [Y, X|Stack], Env, Objspace, REnv) :-
    NewPC is PC + 1,
    Objspace:add(X, Y, Res),
    fact_int(NewPC, Objspace, Env, [Res|Stack], REnv).
% not
fact_int(240, _Args, PC, [X|Stack], Env, Objspace, REnv) :-
    NewPC is PC + 1,
    Objspace:not(X, Res),
    fact_int(NewPC, Objspace, Env, [Res|Stack], REnv).
% eq
fact_int(251, _Args, PC, [Y, X|Stack], Env, Objspace, REnv) :-
    NewPC is PC + 1,
    Objspace:eq(X, Y, Res),
    fact_int(NewPC, Objspace, Env, [Res|Stack], REnv).
% le
fact_int(252, _Args, PC, [Y, X|Stack], Env, Objspace, REnv) :-
    NewPC is PC + 1,
    Objspace:le(X, Y, Res),
    fact_int(NewPC, Objspace, Env, [Res|Stack], REnv).
% lt
fact_int(253, _Args, PC, [Y, X|Stack], Env, Objspace, REnv) :-
    NewPC is PC + 1,
    Objspace:lt(X, Y, Res),
    fact_int(NewPC, Objspace, Env, [Res|Stack], REnv).
% ge
fact_int(254, _Args, PC, [Y, X|Stack], Env, Objspace, REnv) :-
    NewPC is PC + 1,
    Objspace:ge(X, Y, Res),
    fact_int(NewPC, Objspace, Env, [Res|Stack], REnv).
% gt
fact_int(255, _Args, PC, [Y, X|Stack], Env, Objspace, REnv) :-
    NewPC is PC + 1,
    Objspace:gt(X, Y, Res),
    fact_int(NewPC, Objspace, Env, [Res|Stack], REnv).


compare_interpreter_results(AST, Env) :-
    unload_file('generated.pl'),
    retractall(bc(_, _, _)),
    do_compile(AST, SubByteCodes, Codes),
    ast_int(AST, Env, objspace, AEnv), !,
    assert_bcs(Codes, SubByteCodes),
    fact_int(Env, BEnv),!,
    AEnv == BEnv.


prepare_assert_bc(AST) :-
    retractall(bc(_, _, _)),
    do_compile(AST, SubByteCodes, Codes),
    assert_bcs(Codes, SubByteCodes).

:- use_module(library(plunit)).

:- begin_tests(bc_to_terms).

test(only_push) :-
    bc_to_terms([23, 0], _, End, Res), !, nonvar(Res),
    End =:= 2, Res == [bc(0, 23, 0)].

test(assignment) :-
    do_compile(assign(id(x), int(2)), SBC, BC),!,
    bc_to_terms(BC, SBC, End, Res), !, nonvar(Res),
    End =:= 7,
    length(Res, 2),
    member(bc(0, 23, 2), Res), % push 2
    member(bc(2, 45, x), Res), !. % assign x

test(eq) :-
    do_compile(eq(int(1), int(2)), SBC, BC),!,
    bc_to_terms(BC, SBC, End, Res), !, nonvar(Res),
    End =:= 5,
    length(Res, 3),
    member(bc(0, 23, 1), Res),
    member(bc(2, 23, 2), Res),
    member(bc(4, 251, []), Res), !.

test(ite) :-
    do_compile(if(eq(int(1), int(2)),
                  [assign(id(x), int(1))],
                  [assign(id(x), int(2))]),
               SBC, BC), !,
    bc_to_terms(BC, SBC, End, Res),!,nonvar(Res),
    End =:= 29,
    length(Res, 9),
    member(bc(0, 23, 1), Res), % push 1
    member(bc(2, 23, 2), Res), % push 2
    member(bc(4, 251, []), Res), % eq
    member(bc(5, 11, 22), Res),  % jump-if-false 22 [begin of else]
    member(bc(10, 23, 1), Res), % push 1
    member(bc(12, 45, x), Res), % assign 0/x
    member(bc(17, 10, 29), Res), % jump
    member(bc(22, 23, 2), Res),
    member(bc(24, 45, x), Res), !.

test(while) :-
    do_compile(while(le(id(x), int(3)),
                     [assign(id(x), add(id(x), int(1)))], _, _),
               SBC, BC), !,
    bc_to_terms(BC, SBC, End, Res), !, nonvar(Res),
    End =:= 31,
    length(Res, 9),
    member(bc(0, 40, x), Res), % load 0/x
    member(bc(5, 23, 3), Res), % push 3
    member(bc(7, 252, []), Res), % le
    member(bc(8, 11, 31), Res),  % jump-if-false 31 [after while loop] 
    member(bc(13, 40, x), Res), % load 0/x
    member(bc(18, 23, 1), Res), % push 2
    member(bc(20, 200, []), Res), % add
    member(bc(21, 45, x), Res), % store 0/x
    member(bc(26, 10, 0), Res), !. % jmp 0


:- end_tests(bc_to_terms).


:- begin_tests(fact_int).

test(assignment) :-
    AST = [assign(id(x), int(2))],
    empty_assoc(Env),
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
    empty_assoc(Env),
    compare_interpreter_results(AST, Env).

test(mul) :-
    AST = [assign(id(x), mul(int(3), int(2)))],
    empty_assoc(Env),
    compare_interpreter_results(AST, Env).

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


:- end_tests(fact_int).
