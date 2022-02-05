%% 9.2 Lexicographic order global constraint

%% N.B. For non-gound list, we need to simulate some sort of abstract semantics.
%% See 2.2.1 Minimum, Abstract semantics

%% We assuming with the same length.

%% N.B.
%% Because clpr constraints can not waken chr constraints, and the workaround of trig solution doesn't work 
%% (see 2_add_constraint_propagation_l4.pl). This directory simulates inequalities constraints. However this
%% simulation is not complete, and it is only for demonstrating idea of lexicographic order global constraint.

%% 1. Not support entailed/1.
%% Using kept constraints in head to simulate entailed in guard. It usually causes problems, for example,
%% if there is a bi(X=<5) in store, it imply X=<6, so you can use entailed(X=<6) in guard, but in this
%% simulation you can not match bi(X=<6) in head, because there is no bi(X=<6) in the store.

%% 2. Not support transitivity
%% Therefore [R1,R2,R3] lex [T1,T2,T3], bi(R2=T2), bi(R3>T4), bi(R4>T3). doesn't work as expected

%% TODO:
%% 1. Find a way to let clpr constraints waken chr constraints
%% e.g. is there any chr predicate can reactive a chr constraint manually?
%% 2. Implement partial order relation that supports entailed check? (still require a wake-up mechanism)
%% 3. Is it possible to implement the non-ground list version, but only allow = in query?
%% i.e. not allow bi(X>=Y), but allow X=Y.

:- use_module(library(chr)).

%% :- set_prolog_flag(double_quotes, codes).

:- op(700, xfx, lex).

:- chr_constraint lex/2, bi/1.

bi(A) \ bi(A) <=> true.
bi(X=Y) <=> X=Y.
bi(X=\=X) <=> fail.
bi(X>=X) <=> true.
bi(X=<X) <=> true.
bi(X>X) <=> fail.
bi(X<X) <=> fail.

bi(X=<Y) <=> ground(X), ground(Y) | X =< Y.
bi(X>=Y) <=> ground(X), ground(Y) | X >= Y.
bi(X<Y) <=> ground(X), ground(Y) | X < Y.
bi(X>Y) <=> ground(X), ground(Y) | X > Y.
bi(X=Y) <=> ground(X), ground(Y) | X =:= Y.

bi(X>=Y), bi(X=<Y) <=> X=Y.
bi(X>=Y), bi(Y>=X) <=> X=Y.
bi(X>=Y), bi(Y>X) <=> fail.
bi(X=<Y), bi(Y<X) <=> fail.

% for lexicographic constraint
% so that guard checking works
bi(X>Y) ==> bi(X>=Y),bi(X=\=Y).
bi(X<Y) ==> bi(X=<Y),bi(X=\=Y).
bi(X=Y) ==> bi(X>=Y),bi(X=<Y).

bi(X>=Y),bi(X=\=Y) ==> bi(X>Y).
bi(X=<Y),bi(X=\=Y) ==> bi(X<Y).
bi(X>=Y),bi(X=<Y) ==> bi(X=Y).


l1  @ [] lex [] <=> true.
l2g @ [X|L1] lex [Y|L2] <=> ground(X), ground(Y), X<Y | true.
l2  @ bi(X<Y) \ [X|L1] lex [Y|L2] <=> true.

l3  @ [X|L1] lex [Y|L2] <=>  X==Y | L1 lex L2.


%% ?- [2] lex [1].
%@ false.

%% ?- [1] lex [2].
%@ true.

%% ?- [X] lex [X].
%@ true.

%% ?- [X] lex [Y], bi(X<Y).
%@ bi($VAR(X)\== $VAR(Y)),
%@ bi($VAR(X)=< $VAR(Y)),
%@ bi($VAR(X)< $VAR(Y)).

%% no rules are applicable
%% ?- [X] lex [Y].
%@ [$VAR(X)]lex[$VAR(Y)].

%% no rules are applicable
%% ?- [X] lex [Y], bi(X>=Y). 
%@ [$VAR(X)]lex[$VAR(Y)],
%@ bi($VAR(X)>= $VAR(Y)).

%% no rules are applicable, but should fail
%% ?- [X] lex [Y], bi(X>Y). 
%@ [$VAR(X)]lex[$VAR(Y)],
%@ bi($VAR(X)\== $VAR(Y)),
%@ bi($VAR(X)>= $VAR(Y)),
%@ bi($VAR(X)> $VAR(Y)).

