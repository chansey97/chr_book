%% 6.2.1 Term rewriting systems
%% Functional programming.

:- use_module(library(chr)).

:- op(600, xfx, eq).

:- chr_constraint eq/2.

%% FP
%% 0 + y -> y
%% s(x) + y -> s(x + y)

X eq T <=> datum(T) | X=T.
X eq T <=> builtin(T) | c(T,X).

T eq 0+Y <=> T eq Y.
T eq s(X)+Y <=> T=s(T4), T4 eq T5+T6, T5 eq X, T6 eq Y.

datum(0).
datum(s(X)).
builtin(T) :- fail.

%?- T eq s(s(0))+s(0).
%@ T = s(s(s(0))).
