%% 9.2 Lexicographic order global constraint

%% N.B. For non-gound list, we need to simulate some sort of abstract semantics.
%% See 2.2.1 Minimum, Abstract semantics

%% We assuming with the same length.

%% N.B.
%% In the current implementation, clpr constrains (e.g. the {X=<Y} in query) can not waken chr constraints,
%% see https://swi-prolog.discourse.group/t/interaction-between-chr-and-clp-r/4982

%% These directory try to use trig/0 to simulate wake, but not good.
%% So don't look at it...

:- use_module(library(chr)).
:- use_module(library(clpr)).

%% :- set_prolog_flag(double_quotes, codes).

:- op(700, xfx, lex).

:- chr_constraint lex/2, trig/0.

%% In the current implementation, clpr constrains (e.g. the {X=<Y} in query) can not waken chr constraints,
%% I attempt to use the `trig` to simulate. Notice that since body of l4 also send a constaint {X=<Y}, we
%% have to trig it again, but that will cause divergence and we have to fix, but the fix is not general...
%% See 3_add_constraint_propagation_l5l6.pl

l1  @ trig \ [] lex [] <=> true.
l2  @ trig \ [X|L1] lex [Y|L2] <=> entailed(X<Y) | true.

l3  @ trig \ [X|L1] lex [Y|L2] <=> entailed(X=Y)  | L1 lex L2.
trig \ trig <=> true.
l4  @ trig, [X|L1] lex [Y|L2] ==> {X=<Y}, trig.

%% Place trig in l5 is wrong!
%% Fixed point should be only reach when variables attribue constraint no longer changing.
l5 @ trig \ [X,U|L1] lex [Y,V|L2] <=> entailed(U>V) | {X<Y}, trig.
l6 @ trig \ [X,U|L1] lex [Y,V|L2] <=> entailed(U=V) | [X|L1] lex [Y|L2].

trig <=> true.

%% ?- [1] lex [2], trig.
%@ true.

%% ?- [X] lex [X], trig.
%@ true.

%% ?- [X] lex [Y], {X<Y}, trig.
%@ {$VAR(Y)= $VAR(X)+ $VAR(_A),$VAR(_A)>0.0}.

%% Fix the previous problem by l4
%% ?- [X] lex [Y], trig.
%@ [$VAR(X)]lex[$VAR(Y)],
%@ {$VAR(Y)= $VAR(X)+ $VAR(_A),$VAR(_A)>=0.0}.

%% Fix the previous problem by l4
%% ?- [X] lex [Y], {X>=Y}, trig.
%@ {$VAR(Y)= $VAR(X)}.

%% Fix the previous problem by l4
%% ?- [X] lex [Y], {X>Y}, trig.
%@ false.

%% Fix the previous problem by l4
%% ?- [R|Rs] lex [T|Ts], {R=\=T}, trig.
%@ {$VAR(_A)>=0.0,$VAR(T)= $VAR(R)+ $VAR(_A),$VAR(_)= - $VAR(_A),$VAR(_A)=\=0.0}.


%% ?- [R1,R2,R3] lex [T1,T2,T3], bi(R2=T2), bi(R3>T3).

