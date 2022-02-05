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

l1  @ trig \ [] lex [] <=> true.
l2  @ trig \ [X|L1] lex [Y|L2] <=> entailed(X<Y) | true.

l3  @ trig \ [X|L1] lex [Y|L2] <=>  X==Y | L1 lex L2.
trig <=> true.

%% ?- [1] lex [2], trig.
%@ true.

%% ?- [X] lex [X], trig.
%@ true.

%% ?- [X] lex [Y], {X<Y}, trig.
%@ {$VAR(Y)= $VAR(X)+ $VAR(_A),$VAR(_A)>0.0}.

%% no rules are applicable
%% ?- [X] lex [Y], trig.
%@ [$VAR(X)]lex[$VAR(Y)].

%% no rules are applicable
%% ?- [X] lex [Y], {X>=Y}, trig. 
%@ [$VAR(X)]lex[$VAR(Y)],
%@ {$VAR(Y)= $VAR(X)- $VAR(_A),$VAR(_A)>=0.0}.

%% no rules are applicable, but should fail
%% ?- [X] lex [Y], {X>Y}, trig. 
%@ [$VAR(X)]lex[$VAR(Y)],
%@ {$VAR(Y)= $VAR(X)- $VAR(_A),$VAR(_A)>0.0}.

