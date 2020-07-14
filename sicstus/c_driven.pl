:- module(c_driven, [prepare_c_int/7,
                     run_c_int/6,
                     ast_to_bc_file/3,
                     linear_bytecode/7,
                     init/0,
                     list_to_char_ptr/3]).
:- use_module(assert_bc_for_c, [bc_to_terms/4]).
:- use_module(compiler, [do_compile/3,
                         convert_identifier/3, idlist_to_idtable/3,
                         encode_integer/3, decode_integer/2,
                         pre_process/3]).
:- use_module(ast_interpreter, [ast_int/4]).
:- use_module(library(avl)).


foreign_resource('driver', [run_bc, list_to_char_ptr, init_pred_table, free_char_ptr]).
foreign(run_bc, c, run_bc(+address(char), +integer, +address(char), +integer, [-term])).
foreign(list_to_char_ptr, c, list_to_char_ptr(+term, +integer, [-address(char)])).
foreign(init_pred_table, c, init_pred_table(+atom)).
foreign(free_char_ptr, c, free_char_ptr(+address(char))).


/* TODO:
 * encode initial environment into bytecode
 * i.e. prepend [assign(x, 0), assign(y, 42), ...]
 * reverse the identifiers (start with highest first)
 * so the c interpreter knows exactly how long it will be
 */

    
env_to_bc(Env, IdTableEnc, IdTableDec, EnvBCLen, ResBC) :-
    env_to_ast(IdTableDec, Env, AST),
    do_compile(AST, _, BC),
    bc_to_terms(BC, _, EnvBCLen, Res),
    terms_to_bc(Res, A-A, IdTableEnc, ResBC-[]).


linear_bytecode(AST, Env, IdTableDec, Len, ResBC, EnvBCLen, ResBCEnv) :-
    pre_process(AST, IdTableEnc, IdTableDec),
    do_compile(AST, SubByteCodes, BC),
    bc_to_terms(BC, SubByteCodes, Len, Res),
    terms_to_bc(Res, A-A, IdTableEnc, ResBC-[]),
    env_to_bc(Env, IdTableEnc, IdTableDec, EnvBCLen, ResBCEnv).

terms_to_bc([], Acc, _IdTableEnc, Acc).
terms_to_bc([bc(_, 23, X)|T], Acc-R, IdTableEnc, Res) :- !,
    encode_integer(X, Len, Enc-DL),
    (Len =:= 1 -> BC = 20
                ; Len =:= 4 -> BC = 21
                             ; !, print('integer too large'), nl, fail),
    R = [BC|Enc],
    terms_to_bc(T, Acc-DL, IdTableEnc, Res).
terms_to_bc([bc(_, BC, Info)|T], Acc-[BC|Int], IdTableEnc, Res) :-
    BC >= 10, BC =< 12,
    encode_integer(Info, 4, Int-IDL),
    terms_to_bc(T, Acc-IDL, IdTableEnc, Res).
terms_to_bc([bc(_, BC, Identifier)|T], Acc-R, IdTableEnc, Res) :- 
    BC >= 40, BC =< 45, !,
    convert_identifier(Identifier, IdTableEnc, Int-DL),
    R = [BC|Int],
    terms_to_bc(T, Acc-DL, IdTableEnc, Res).
terms_to_bc([bc(_, BC, _)|T], Acc-[BC|IDL], IdTableEnc, Res) :-
    BC >= 197,
    terms_to_bc(T, Acc-IDL, IdTableEnc, Res).


:- dynamic(initialised/0).
init :-
    initialised, !.
init :-
    load_foreign_resource(driver),
    init_pred_table(objspace),
    assert(initialised).

prepare_c_env(RIdTable, Env, L, CEnv) :-
    avl_size(RIdTable, L),
    L1 is L - 1,
    h_prepare_c_env(RIdTable, Env, L1, [], CEnv).

h_prepare_c_env(_, _, Id, Acc, Acc) :-
    Id < 0, !.
h_prepare_c_env(RIdTable, Env, L, Acc, CEnv) :-
    avl_fetch(L, RIdTable, Id),
    (objspace:lookup(Id, Env, Val) -> true
                                    ; Val = uninitialised), % TODO: this may be evil
    L1 is L - 1,
    h_prepare_c_env(RIdTable, Env, L1, [Val|Acc], CEnv).

env_to_ast(RIdTable, Env, AST) :-
    avl_size(RIdTable, L),
    env_to_ast2(RIdTable, Env, 0, L, [], AST).
env_to_ast2(_, _, L, L, AST, AST) :- !.
env_to_ast2(RIdTable, Env, N, L, Acc, AST) :-
    avl_fetch(N, RIdTable, Id),
    (objspace:lookup(Id, Env, Val) -> true ; Val = int(0)),
    N1 is N + 1,
    env_to_ast2(RIdTable, Env, N1, L, [assign(id(Id), Val)|Acc], AST).




c_int(AST, Env, NewEnv) :-
    init,
    linear_bytecode(AST, Env, RIdTable, Len, ResBC, EnvBCLen, ResBCEnv),
    list_to_char_ptr(ResBCEnv, EnvBCLen, CEnvBC),
    list_to_char_ptr(ResBC, Len, CBC),
    run_bc(CBC, Len, CEnvBC, EnvBCLen, NewCEnv),
    free_char_ptr(CBC),
    free_char_ptr(CEnvBC),
    process_resulting_env(NewCEnv, RIdTable, NewEnv).

prepare_c_int(AST, Env, RIdTable, CBC, Len, CEnvBC, EnvBCLen) :-
    init,
    linear_bytecode(AST, Env, RIdTable, Len, ResBC, EnvBCLen, ResBCEnv),
    list_to_char_ptr(ResBCEnv, EnvBCLen, CEnvBC),
    list_to_char_ptr(ResBC, Len, CBC).

run_c_int(RIdTable, CBC, Len, CEnvBC, EnvBCLen, REnv) :-
    run_bc(CBC, Len, CEnvBC, EnvBCLen, NewCEnv),
    free_char_ptr(CBC),
    free_char_ptr(CEnvBC),
    process_resulting_env(NewCEnv, RIdTable, REnv).



process_resulting_env(CEnv, IdTable, NewEnv) :-
    h_process_resulting_env(CEnv, IdTable, 0, [], Env),
    list_to_avl(Env, NewEnv).

h_process_resulting_env([], _, _, Acc, Acc).
h_process_resulting_env([H|T], IdTable, I, Acc, NewEnv) :-
    avl_fetch(I, IdTable, Id),
    I1 is I + 1,
    h_process_resulting_env(T, IdTable, I1, [Id-H|Acc], NewEnv).



write_bc([]).
write_bc([H|T]) :-
    format('~c', H),
    write_bc(T).


bytecode_to_file(BCList, Filename) :-
    open(Filename, write, Stream),
    tell(Stream),
    write_bc(BCList),
    told.


ast_to_bc_file(AST, Env, Filename) :-
    linear_bytecode(AST, Env, _, _, ResBC, _, ResBCEnv),
    atom_codes(Filename, CFilename),
    append(CFilename, "_env", CEnvFilename),
    atom_codes(EnvFilename, CEnvFilename),
    bytecode_to_file(ResBCEnv, EnvFilename),
    bytecode_to_file(ResBC, Filename).




:- use_module(library(plunit)).

compare_interpreter_results(AST, Env) :-
    ast_int(AST, Env, objspace, AEnv), !,
    c_int(AST, Env, BEnv), !,
    AEnv == BEnv.


:- begin_tests(terms_to_bc).

test(jmp) :-
    terms_to_bc([bc(0, 10, 20)], A-A, _, Res), !, nonvar(Res),
    Res = BC-[], BC == [10, 20, 0, 0, 0].

test(jmp2) :-
    terms_to_bc([bc(0, 10, 20), bc(5, 11, 0)], A-A, _, Res), !, nonvar(Res),
    Res = BC-[], BC == [10, 20, 0, 0, 0, 11, 0, 0, 0, 0].

test(load) :-
    idlist_to_idtable([x], IdTable, _),
    terms_to_bc([bc(0, 40, x)], A-A, IdTable, Res), !, nonvar(Res),
    Res = BC-[], BC == [40, 0, 0, 0, 0].

test(assignment) :-
    idlist_to_idtable([x, y], IdTable,  _),
    terms_to_bc([bc(0, 45, y)], A-A, IdTable, Res), !, nonvar(Res),
    Res = BC-[], BC == [45, 1, 0, 0, 0].

test(add) :-
    terms_to_bc([bc(0, 200, [])], A-A, _, Res), !, nonvar(Res),
    Res = BC-[], BC == [200].

test(while) :-
    AST = while(le(id(x), int(3)),
                 [assign(id(x), add(id(x), int(1)))], _, _),
    linear_bytecode(AST, empty, _, Len, ResBC, _, _), !, 
    Len =:= 31, length(ResBC, Len),
    % compare resulting BC with test(while) in assert_bc.pl
    ResBC == [40, 0, 0, 0, 0,
              20, 3,
              252,
              11, 31, 0, 0, 0,
              40, 0, 0, 0, 0,
              20, 1,
              200,
              45, 0, 0, 0, 0,
              10, 0, 0, 0, 0].

test(ite) :-
    AST = if(eq(int(1), int(2)),
             [assign(id(x), int(1))],
             [assign(id(x), int(2))]),
    linear_bytecode(AST, empty, _, Len, ResBC, _, _), !, 
    Len =:= 29, length(ResBC, Len),
    ResBC == [20, 1,
              20, 2,
              251,
              11, 22, 0, 0, 0,
              20, 1,
              45, 0, 0, 0, 0,
              10, 29, 0, 0, 0,
              20, 2,
              45, 0, 0, 0, 0].

:- end_tests(terms_to_bc).

:- begin_tests(c_int).

test(simple_assign) :-
    AST = assign(id(x), int(1)),
    c_int(AST, [], NewEnv), !, 
    objspace:lookup(x, NewEnv, int(1)),
    avl_size(NewEnv, 1).


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

:- end_tests(c_int).


:- begin_tests(c_env).

test(prepare_empty) :-
    prepare_c_env(empty, empty, 0, X), !, X == [].

test(prepare_nonempty) :-
    list_to_avl([1-y, 0-x], IdAVL),
    list_to_avl([x-int(2), y-int(7)], Env),
    prepare_c_env(IdAVL, Env, 2, X), !,
    X == [int(2), int(7)].

test(process_empty) :-
    process_resulting_env([], [], X), !,
    X == empty.

test(process_nonempty) :-
    list_to_avl([1-x, 0-y], IdAVL),
    process_resulting_env([int(2), int(5)], IdAVL, X), !,
    objspace:lookup(x, X, int(5)),
    objspace:lookup(y, X, int(2)),
    avl_size(X, 2).

:- end_tests(c_env).

:- begin_tests(env2ast).

test(empty) :-
    env_to_ast(empty, empty, X), !, X == [].

test(nonempty) :-
    empty_avl(Empty),
    avl_store(0, Empty, x, A1),
    avl_store(1, A1, y, A2),
    avl_store(2, A2, z, IdTable),

    avl_store(x, Empty, int(20), B1),
    avl_store(y, B1, int(21), B2),
    avl_store(z, B2, int(22), Env),
    env_to_ast(IdTable, Env, X), !,
    X == [assign(id(z), int(22)), assign(id(y), int(21)), assign(id(x), int(20))].

:- end_tests(env2ast).

:- begin_tests(env2bc).

test(nonempty) :-
    empty_avl(Empty),
    avl_store(0, Empty, x, A1),
    avl_store(1, A1, y, A2),
    avl_store(2, A2, z, IdTableDec),

    avl_store(x, Empty, int(20), B1),
    avl_store(y, B1, int(21), B2),
    avl_store(z, B2, int(22), Env),

    avl_store(x, Empty, 0, C1),
    avl_store(y, C1, 1, C2),
    avl_store(z, C2, 2, IdTableEnc),
    env_to_bc(Env, IdTableEnc, IdTableDec, EnvBCLen, ResBC), !,
    ground(EnvBCLen),
    length(ResBC, EnvBCLen),
    ResBC == [20, 22, 45, 2, 0, 0, 0, 20, 21, 45, 1, 0, 0, 0, 20, 20, 45, 0, 0, 0, 0].

:- end_tests(env2bc).
