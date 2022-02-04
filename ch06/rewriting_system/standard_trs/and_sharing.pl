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

fd @ X eq T \ Y eq T <=> X=Y.
T eq and(T1,T1), T1 eq X <=> T eq X.

T eq and(T1,T2), T1 eq 0, T2 eq Y <=> T eq 0.
T eq and(T1,T2), T1 eq X, T2 eq 0 <=> T eq 0.
T eq and(T1,T2), T1 eq 1, T2 eq Y <=> T eq Y.
T eq and(T1,T2), T1 eq X, T2 eq 1 <=> T eq X.
T eq and(T1,T2), T1 eq X, T2 eq X <=> T eq X.

%?- Z eq and(X,Y), W eq and(X,Y).
%@ Z = $VAR(W),
%@ $VAR(W)eq and($VAR(X),$VAR(Y)).

%% Q: and(and(0,1),and(0,1))
%% Now OK
%?- T eq and(T1, T1),
%%  T1 eq and(T11,T12), T11 eq 0, T12 eq 1.
%@ $VAR(T)eq 0.

