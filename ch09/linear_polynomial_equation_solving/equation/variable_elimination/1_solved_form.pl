%% 9.1 Linear polynomial equation solving
%% 9.1.1 Variable elimination

:- use_module(library(chr)).
user:library_directory('../../').
:- use_module(library(helpers)).

:- op(500,xfy,+).
:- op(650,xfx,eq).

:- chr_constraint eq/2.

eliminate @ A1*X+P1 eq 0 \ A2*X+P2 eq 0 <=> normalize(A2*(-P1/A1)+P2,P3), P3 eq 0.

constant @ B eq 0 <=> number(B) | zero(B).

zero(X) :- abs(X) =< 0.000001.

%% ?- 1*X+3*Y+5 eq 0, 3*X+2*Y+8 eq 0.
%@ -7* $VAR(Y)-7 eq 0,
%@ 1* $VAR(X)+3* $VAR(Y)+5 eq 0.

%% TODO: back substitution variables