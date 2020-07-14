:- module(compiler, [do_compile/3,
                     encode_integer/3, decode_integer/2,
                     pre_process/3,
                     convert_identifier/3,
                     idlist_to_idtable/3,
                     retrieve_subbytecode/3]).
:- use_module(objspace, [int_to_varname/3, assoc_size/2]).
:- use_module(library(plunit)).
:- use_module(library(lists)).
:- use_module(library(assoc)).

%% compiler that compiles ASTs to byte code
%  produces pretty much "real" byte code already


append_dl(I-M, M-O, I-O).


encode_integer(I, 1, [Res|DL]-DL) :-
    I >= -128, I < 128, !,
    Res is I /\ 255.
encode_integer(I, 4, [A, B, C, D|DL]-DL) :-
    I >= -(2**63), I < 2**63, !,
    A is I /\ 255,
    B is (I >> 8) /\ 255,
    C is (I >> 16) /\ 255,
    D is (I >> 24) /\ 255.
encode_integer(_, _, _) :-
    % TODO: build this for push-n
    fail.


decode_integer([E], I) :- !,
    (E >= 128
        -> I is - (((\E) /\ 255) + 1)
         ; I = E).
decode_integer([A, B, C, D], I) :-
    (D >= 128
        -> V is -(((\D) /\ 255) + 1) << 24
         ; V is D << 24),
    I is V + A + (B << 8) + (C << 16).


convert_identifier(ID, IdTable, Int-DL) :-
    get_assoc(ID, IdTable, Pos),
    encode_integer(Pos, 4, Int-DL).

add_identifier(ID, IdList, IdList) :-
    member(ID, IdList), !.
add_identifier(ID, IdList, [ID|IdList]).



idlist_to_idtable(IdList, IdTableEncode, IdTableDecode) :-
    empty_assoc(X),
    empty_assoc(Y),
    h_idlist_to_idtable(IdList, X, Y, 0, IdTableEncode, IdTableDecode).

h_idlist_to_idtable([], ResEncode, ResDecode, _, ResEncode, ResDecode).
h_idlist_to_idtable([H|T], AccEncode, AccDecode, Nr, ResEncode, ResDecode) :-
    Nr1 is Nr + 1,
    put_assoc(H, AccEncode, Nr, NewAccEncode),
    put_assoc(Nr, AccDecode, H, NewAccDecode),
    h_idlist_to_idtable(T, NewAccEncode, NewAccDecode, Nr1, ResEncode, ResDecode).




gather_identifiers([], IdList, IdList).
gather_identifiers([H|T], IdList, RIdList) :-
    gather_identifiers(H, IdList, TIdList),
    gather_identifiers(T, TIdList, RIdList).

gather_identifiers(id(X), IdList, RIdList) :- !,
    add_identifier(X, IdList, RIdList).
gather_identifiers(Term, IdList, RIdList) :-
    Term =.. [_Functor|Args],
    gather_identifiers(Args, IdList, RIdList).


pre_process(AST, IdTableEnc, IdTableDec) :-
    gather_identifiers(AST, [], IDs),
    idlist_to_idtable(IDs, IdTableEnc, IdTableDec).


swap(A-B, B-A).

do_compile(AST, SubByteCodes, BC) :-
    %pre_process(AST, IdAVL),
    %avl_to_list(IdAVL, IdTable),
    %maplist(swap, IdTable, RIdTable),
    %list_to_avl(RIdTable, RIdAVL),
    empty_assoc(SBCAcc),
    compile(AST, SBCAcc, SubByteCodes, BC-[]).



store_subbytecode(AVL, SBC, Id, NewAVL) :-
    assoc_size(AVL, Id),
    put_assoc(Id, AVL, SBC, NewAVL).
retrieve_subbytecode(AVL, Id, SBC) :-
    get_assoc(Id, AVL, SBC).


compile([], ByteCodes, ByteCodes, []-[]).
compile([H|T], ByteCodes, RByteCodes, CH-X) :-
    compile(H, ByteCodes, TByteCodes, CH-CT), !,
    compile(T, TByteCodes, RByteCodes, CT-X).

compile(true, ByteCodes, ByteCodes, [push, true|DL]-DL).
compile(false, ByteCodes, ByteCodes, [push, false|DL]-DL).
compile(int(X), ByteCodes, ByteCodes, [23, X|DL]-DL).
compile(assign(id(Var),Val), ByteCodes, ByteCodes, Res-DL) :- !,
    compile(Val, ByteCodes, ByteCodes, Res-EX),
    EX-DL = [45, Var|DL]-DL.
compile(not(X), ByteCodes, ByteCodes, XC-DL) :- !,
    compile(X, ByteCodes, ByteCodes, XC-EX),
    append_dl(XC-EX, [240|DL]-DL, XC-DL).
compile(id(X), ByteCodes, ByteCodes, [40, X|DL]-DL).
compile(if(Cond, Then, Else), ByteCodes, RByteCodes, If-DL) :- !,
    compile(Cond, ByteCodes, ByteCodes, CondC-Block),
    compile(Then, ByteCodes, SByteCodes, ThenC-[]),
    compile(Else, SByteCodes, TByteCodes, ElseC-[]),
    store_subbytecode(TByteCodes, ThenC, X, UByteCodes),
    store_subbytecode(UByteCodes, ElseC, X1, RByteCodes),
    append_dl(CondC-Block, [1, X, X1|DL]-DL, If-DL).
compile(while(Cond, Instr,_,_), ByteCodes, RByteCodes, [2, X, X1|DL]-DL) :- !,
    compile([Cond], ByteCodes, ByteCodes, CondC-[]), % CondC should be a bytecode that leaves exactly a boolean on the stack
    compile(Instr, ByteCodes, TByteCodes, InstrC-[]),
    store_subbytecode(TByteCodes, CondC, X, UByteCodes),
    store_subbytecode(UByteCodes, InstrC, X1, RByteCodes).


compile(add(LHS, RHS), ByteCodes, ByteCodes, Res-DL) :-
    compile_and_append(LHS, RHS, 200, Res-DL).
compile(sub(LHS, RHS), ByteCodes, ByteCodes, Res-DL) :-
    compile_and_append(LHS, RHS, 199, Res-DL).
compile(mul(LHS, RHS), ByteCodes, ByteCodes, Res-DL) :-
    compile_and_append(LHS, RHS, 198, Res-DL).
compile(mod(LHS, RHS), ByteCodes, ByteCodes, Res-DL) :-
    compile_and_append(LHS, RHS, 197, Res-DL).

compile(gt(LHS, RHS), ByteCodes, ByteCodes, Res-DL) :-
    compile_and_append(LHS, RHS, 255, Res-DL).
compile(ge(LHS, RHS), ByteCodes, ByteCodes, Res-DL) :-
    compile_and_append(LHS, RHS, 254, Res-DL).
compile(lt(LHS, RHS), ByteCodes, ByteCodes, Res-DL) :-
    compile_and_append(LHS, RHS, 253, Res-DL).
compile(le(LHS, RHS), ByteCodes, ByteCodes, Res-DL) :-
    compile_and_append(LHS, RHS, 252, Res-DL).
compile(eq(LHS, RHS), ByteCodes, ByteCodes, Res-DL) :-
    compile_and_append(LHS, RHS, 251, Res-DL).
compile(X, _, _, _, _) :-
    functor(X, Functor, _), !, % do all arity two bytecodes because I'm lazy
    print(Functor), print(' is not yet implemented'), nl, !, fail.
    
compile_and_append(LHS, RHS, BC, Res-DL) :-
    compile(LHS, [], [], LC-RC),
    compile(RHS, [], [], RC-EX),
    append_dl(LC-RC, RC-EX, LC-EX),
    append_dl(LC-EX, [BC|X]-X, Res-DL).



:- begin_tests(id_list).


test(gather_identifiers) :-
    gather_identifiers(while(le(id(x), int(3)), [assign(id(x), add(id(x), int(1)))], _, _), [], R), !, R == [x].

test(gather_identifiers2) :-
    gather_identifiers(while(le(id(x), int(3)), [assign(id(z), add(id(y), int(1)))], _, _), [], R), !, R == [y, z, x].

test(to_idtable) :-
    idlist_to_idtable([y,z,x], REncode, RDecode), !,
    get_assoc(x, REncode, 2),
    get_assoc(z, REncode, 1),
    get_assoc(y, REncode, 0),
    assoc_size(REncode, 3),
    get_assoc(2, RDecode, x),
    get_assoc(1, RDecode, z),
    get_assoc(0, RDecode, y),
    assoc_size(RDecode, 3).

:- end_tests(id_list).




:- begin_tests(compiler).

test(integer) :-
    do_compile(int(2), SBC, Res), !, ground(Res), ground(SBC),
    Res = [23, 2], SBC == t.

test(true) :-
    do_compile(true, SBC, Res), !, ground(Res), ground(SBC),
    Res = [push, true], SBC == t.

test(false) :-
    do_compile(false, SBC, Res), !, ground(Res), ground(SBC),
    Res = [push, false], SBC == t.

test(add) :-
    do_compile(add(int(2), int(3)), SBC, Res), !, ground(Res), ground(SBC),
    Res = [23, 2, 23, 3, 200], SBC == t.

test(sub) :-
    do_compile(sub(int(2), int(3)), SBC, Res), !, ground(Res), ground(SBC),
    Res = [23, 2, 23, 3, 199], SBC == t.

test(mod) :-
    do_compile(mod(int(2), int(3)), SBC, Res), !, ground(Res), ground(SBC),
    Res = [23, 2, 23, 3, 197], SBC == t.

test(not) :-
    do_compile(not(true), SBC, Res), !, ground(Res), ground(SBC),
    Res = [push, true, 240], SBC == t.

test(composition) :-
    do_compile(mul(add(int(2), int(3)), int(4)), SBC, Res), !, ground(Res), ground(SBC),
    Res = [23, 2, 23, 3, 200, 23, 4, 198], SBC == t.

test(identifier) :-
    do_compile(id(x), SBC, Res), !, ground(Res), ground(SBC),
    Res = [40, x], SBC == t.

test(assignment) :-
    do_compile(assign(id(x), add(int(2), id(y))), SBC, Res), !, ground(Res), ground(SBC),
    Res = [23, 2, 40, y, 200, 45, x], SBC == t.

test(ite) :-
    do_compile(if(eq(int(2), int(2)), [assign(id(x), int(1))], [assign(id(x), int(2))]), SBC, Res), !, ground(Res), ground(SBC),
    Res = [23, 2, 23, 2, 251, 1, 0, 1],
    retrieve_subbytecode(SBC, 0, [23, 1, 45, x]),
    retrieve_subbytecode(SBC, 1, [23, 2, 45, x]),
    assoc_size(SBC, 2).

test(while) :-
    do_compile(while(le(id(x), int(3)), [assign(id(x), add(id(x), int(1)))], _, _), SBC, Res), !, ground(Res), ground(SBC),
    Res = [2, 0, 1],
    retrieve_subbytecode(SBC, 0, [40, x, 23, 3, 252]),
    retrieve_subbytecode(SBC, 1, [40, x, 23, 1, 200, 45, x]),
    assoc_size(SBC, 2).

:- end_tests(compiler).

:- begin_tests(integer_representation).

test(one_byte_positive) :-
    encode_integer(42, Len, Res), !, nonvar(Res),
    Len =:= 1, Res = [C]-[],
    decode_integer([C], 42).

test(one_byte_negative) :-
    encode_integer(-42, Len, Res), !, nonvar(Res),
    Len =:= 1, Res = [C]-[],
    decode_integer([C], -42).

test(edge_case1) :-
    encode_integer(-128, Len, Res), !, nonvar(Res),
    Len =:= 1, Res = [C]-[],
    decode_integer([C], -128).

test(edge_case2) :-
    encode_integer(127, Len, Res), !, nonvar(Res),
    Len =:= 1, Res = [C]-[],
    decode_integer([C], 127).

test(edge_case3) :-
    encode_integer(0, Len, Res), !, nonvar(Res),
    Len =:= 1, Res = [C]-[],
    decode_integer([C], 0).

test(four_byte_positive) :-
    encode_integer(1000, Len, Res), !, nonvar(Res),
    Len =:= 4, Res = [A,B,C,D]-[],
    decode_integer([A,B,C,D], 1000).
    
test(four_byte_big_positive) :-
    encode_integer(452435423, Len, Res), !, nonvar(Res),
    Len =:= 4, Res = [A,B,C,D]-[],
    decode_integer([A,B,C,D], 452435423).

test(four_byte_negative) :-
    encode_integer(-1000, Len, Res), !, nonvar(Res),
    Len =:= 4, Res = [A,B,C,D]-[],
    decode_integer([A,B,C,D], -1000).

test(four_byte_small_negative) :-
    encode_integer(-452435423, Len, Res), !, nonvar(Res),
    Len =:= 4, Res = [A,B,C,D]-[],
    decode_integer([A,B,C,D], -452435423).


:- end_tests(integer_representation).
