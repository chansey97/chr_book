%% 9.2 Lexicographic order global constraint

%% N.B. For non-gound list, we need to simulate some sort of abstract semantics.
%% See 2.2.1 Minimum, Abstract semantics

%% We assuming with the same length.

:- use_module(library(chr)).
:- use_module(library(clpr)).

%% :- set_prolog_flag(double_quotes, codes).

:- op(700, xfx, lex).

:- chr_constraint lex/2, trig/0.

l1  @ trig \ [] lex [] <=> true.
l2  @ trig \ [X|L1] lex [Y|L2] <=> entailed(X<Y) | true.
l3  @ trig \ [X|L1] lex [Y|L2] <=>  X==Y | L1 lex L2.
l4  @ trig, [X|L1] lex [Y|L2] ==> {X=<Y}.
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

%% It doesn't work as expected!
%% In the current implementation, clpr constrains (e.g. the {X=<Y} in query) can not waken chr constraints,
%% I attempt to use the `trig` to simulate, but doesn't work! Because body of l4 also send constaint {X=<Y} 
%% which can not wake the `lex`. (Adding `trig` to the body will diverge.)

%% TODO: any workaround?
%% e.g. write own inequalities constraint which supports entailed or modify clpr to adapt chr?
%% https://swi-prolog.discourse.group/t/interaction-between-chr-and-clp-r/4982

%% Has problem
%% ?- [X] lex [Y], {X>=Y}, trig.
%@ [$VAR(X)]lex[$VAR(Y)],
%@ {$VAR(Y)= $VAR(X)}.

%% Fix the previous problem by l4
%% ?- [X] lex [Y], {X>Y}, trig.
%@ false.

%% Has problem
%% ?- [R|Rs] lex [T|Ts], {R=\=T}, trig.
%@ [$VAR(R)|$VAR(Rs)]lex[$VAR(T)|$VAR(Ts)],
%@ {$VAR(_A)>=0.0,$VAR(T)= $VAR(R)+ $VAR(_A),$VAR(_)= - $VAR(_A),$VAR(_A)=\=0.0}.
