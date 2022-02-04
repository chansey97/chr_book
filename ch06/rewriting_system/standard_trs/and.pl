%% 6.2.1 Term rewriting systems

:- use_module(library(chr)).

:- op(600, xfx, eq).

:- chr_constraint eq/2.

%% TRS
%% and(0,Y) -> 0.
%% and(X,0) -> 0.
%% and(1,Y) -> Y.
%% and(X,1) -> X.
%% and(X,X) -> X.

T eq and(T1,T2), T1 eq 0, T2 eq Y <=> T eq 0.
T eq and(T1,T2), T1 eq X, T2 eq 0 <=> T eq 0.
T eq and(T1,T2), T1 eq 1, T2 eq Y <=> T eq Y.
T eq and(T1,T2), T1 eq X, T2 eq 1 <=> T eq X.
T eq and(T1,T2), T1 eq X, T2 eq X <=> T eq X.

%% -- For linear rules --

%% TRS: and(1,1)
%?- T eq and(T1,T2), T1 eq 1, T2 eq 1.
%@ $VAR(T)eq 1.
%% N.B. The last rule fires

%% -- For non-linear rules --

%% TRS: and(and(0,1),and(0,1))
%?- T eq and(T1, T2),
%%  T1 eq and(T11,T12), T11 eq 0, T12 eq 1,
%%  T2 eq and(T21,T22), T21 eq 0, T22 eq 1.
%@ $VAR(T22)eq 1,
%@ $VAR(T21)eq 0,
%@ $VAR(T)eq 0.
%% N.B. Although the result is OK, but the last rule doesn't fire.


%% TRS: and(and(0,1),and(0,1))
%?- T eq and(T1, T1),
%%  T1 eq and(T11,T12), T11 eq 0, T12 eq 1.
%@ $VAR(T1)eq 0,
%@ $VAR(T)eq and($VAR(T1),$VAR(T1)).
%% N.B Doesn't support structure sharing (just like let optimization in cps)
%% see and_sharing.pl