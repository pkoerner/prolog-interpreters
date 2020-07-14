:- module(parser, [parse/2]).
:- use_module(library(lists)). % for reverse


parse(Program, AST) :-
    statements(AST, Program, "").

statements([X|R]) --> ws, statement(X), ws, statements(R), !.
statements([X]) --> ws, statement(X), ws.

statement(if(C, Then, Else)) --> "if", !, ws, expr(C), ws, "{",
                                           statements(Then),
                                        "}", ws, "else", ws, "{",
                                        statements(Else),
                                        "}".
statement(while(C, Instr, [], [])) --> "while", !, ws, expr(C), ws, "{",
                                     statements(Instr),
                                     "}".
statement(assign(id(Name), Val)) --> identifier(Name), !, ws, ":=", ws, expr(Val), ws, ";".


expr(R) --> expra(A), ws, exprl(A,R).
exprl(Acc,Res) --> "<=", ws, !, expra(B), ws, exprl(le(Acc,B),Res).
exprl(Acc,Res) --> ">=", ws, !, expra(B), ws, exprl(ge(Acc,B),Res).
exprl(Acc,Res) --> "<",  ws, !, expra(B), ws, exprl(lt(Acc,B),Res).
exprl(Acc,Res) --> ">",  ws, !, expra(B), ws, exprl(gt(Acc,B),Res).
exprl(Acc,Res) --> "==", ws, !, expra(B), ws, exprl(eq(Acc,B),Res).
exprl(A,A) --> [].

expra(R) --> term(A), ws, exprar(A,R).
exprar(Acc, Res) --> "+", ws, !, term(B), ws, exprar(add(Acc, B), Res).
exprar(Acc, Res) --> "-", ws, !, term(B), ws, exprar(sub(Acc, B), Res).
exprar(A, A) --> [].


term(R) --> factor(A), ws, termc(A,R).
termc(Acc,Res) --> "*",   ws, !, factor(B), ws, termc(mul(Acc,B),Res).
termc(Acc,Res) --> "mod", ws, !, factor(B), ws, termc(mod(Acc,B),Res).
termc(A,A) --> [].

factor(F) --> val(F), !.
factor(F) --> "(", !, expr(F), ")".


val(neg(Value)) --> "-", val(Value), !.
val(int(Value)) --> num(Value).
val(id(Name)) --> identifier(Name). 


num(V, [H|T], Rest) :-
    H >= 48, H =< 57,
    numa(V, T, [H], Rest),
    number(V).
numa(V, [H|T], Acc, Rest) :-
    H >= 48, H =< 57, !,
    numa(V, T, [H|Acc], Rest).
numa(V, Rest, Acc, Rest) :-
    reverse(Acc, VC),
    number_codes(V, VC).

identifier(Name, [H|T], Rest) :-
    ((H >= 65, H =< 90) ; (H >= 97, H =< 122)), % capital and lowercase
    identifiera(Name, T, [H], Rest).
identifiera(V, [H|T], Acc, Rest) :-
    ((H >= 65, H =< 90) ; (H >= 97, H =< 122) ; (H >= 48, H =< 57)), % capital, lowercase and number
    identifiera(V, T, [H|Acc], Rest).
identifiera(V, Rest, Acc, Rest) :-
    reverse(Acc, VC),
    atom_codes(V, VC).



ws --> " ", !, ws.
ws --> [9], !, ws.  % htab
ws --> [11], !, ws. % vtab
ws --> [10], !, ws. % lf
ws --> [13], !, ws. % cr
ws --> [].


:- use_module(library(plunit)).

:- begin_tests(num).

test(single_digit) :-
    num(X, "1", R), !,
    X == 1, R == [].

test(single_digit2) :-
    num(X, "0", R), !,
    X == 0, R == [].

test(single_digit3) :-
    num(X, "9", R), !,
    X == 9, R == [].

test(single_digit4) :-
    num(X, "9 ", R), !,
    X == 9, R == " ".

test(multi_digit) :-
    num(X, "11", R), !,
    X == 11, R == [].

test(multi_digit2) :-
    num(X, "01", R), !,
    X == 1, R == [].

test(multi_digit3) :-
    num(X, "01a", R), !,
    X == 1, R == "a".

:- end_tests(num).

:- begin_tests(identifier).

test(id) :-
    identifier(X, "abc", R), !,
    X == abc, R == [].

test(id2) :-
    identifier(X, "abc21a", R), !,
    X == 'abc21a', R == [].

test(id3) :-
    identifier(X, "abc21a!pf", R), !,
    X == 'abc21a', R == "!pf".

:- end_tests(identifier).

:- begin_tests(val).

test(number) :-
    val(X, "22", R), !,
    X == int(22), R == [].

test(number2) :-
    val(X, "22 ", R), !,
    X == int(22), R == " ".

test(neg_number) :-
    val(X, "-22 ", R), !,
    X == neg(int(22)), R == " ".

test(identifier) :-
    val(X, "x1 ", R), !,
    X == id(x1), R == " ".

:- end_tests(val).

:- begin_tests(term).

test(mul) :-
    term(X, "a*b", R), !,
    X == mul(id(a), id(b)), R == "".

test(mul_ws) :-
    term(X, "a * b", R), !,
    X == mul(id(a), id(b)), R == "".

test(mul_ws2) :-
    term(X, "a * 2", R), !,
    X == mul(id(a), int(2)), R == "".

test(mod) :-
    term(X, "a mod b", R), !,
    X == mod(id(a), id(b)), R == "".

:- end_tests(term).


:- begin_tests(expr).

test(add) :-
    expr(X, "a+b", R), !,
    X == add(id(a), id(b)), R == "".

test(sub) :-
    expr(X, "a-b", R), !,
    X == sub(id(a), id(b)), R == "".

test(add_and_mul1) :-
    expr(X, "a + b * c", R), !,
    X == add(id(a), mul(id(b), id(c))), R == "".

test(add_and_mul2) :-
    expr(X, "(a + b) * c", R), !,
    X == mul(add(id(a), id(b)), id(c)), R == "".

test(comparison) :-
    expr(X, "1 < 2 + 3", R), !,
    X == lt(int(1), add(int(2), int(3))), R == "".

:- end_tests(expr).

:- begin_tests(statements).

test(assign) :-
    statements(X, "a := 3;", R), !,
    X == [assign(id(a), int(3))], R == "".
     

test(ite) :-
    Program = "if x < 2 {   \
                   x := 3;  \
                   y := 0;  \
               } else {     \
                   x := 4;  \
               }            \
               z := 3;      \
               ",
    statements(X, Program, R), !,
    X == [if(lt(id(x), int(2)),
             [assign(id(x), int(3)), assign(id(y), int(0))],
             [assign(id(x), int(4))]),
          assign(id(z), int(3))],
    R == "".

test(while) :-
    Program = "while x < 3 {   \
                   x := x + 1; \
               }",
    statements(X, Program, R), !,
    nonvar(X),
    X = [while(lt(id(x), int(3)), [assign(id(x), add(id(x), int(1)))], _, _)],
    R == "".

test(fib) :-
    Program = "i := 1;                 \
               while i < n {           \
                   b := b + a; \
                   a := b - a; \
                   i := i + 1;          \
               }",
               trace,
    parse(Program, AST), !,
    nonvar(AST),
    AST = [assign(id(i), int(1)),
          while(lt(id(i), id(n)),
                [assign(id(b), add(id(b), id(a))),
                 assign(id(a), sub(id(b), id(a))),
                 assign(id(i), add(id(i), int(1)))], _, _)].


:- end_tests(statements).
