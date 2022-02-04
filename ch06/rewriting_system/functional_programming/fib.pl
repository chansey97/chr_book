%% 6.2.1 Term rewriting systems
%% Functional programming.

:- use_module(library(chr)).

:- op(600, xfx, eq).

:- chr_constraint eq/2.

%% FP
%% fib(0) -> 1.
%% fib(1) -> 1.
%% fib(N) -> N>=2 | fib(N-1)+fib(N-2).

%% X eq T <=> datum(T) | X=T.
%% X eq T <=> builtin(T) | c(T,X).

T eq fib(0) <=> T=1.
T eq fib(1) <=> T=1.
T eq fib(N) <=> N>=2 | N1 is N-1,N2 is N-2, F1 eq fib(N1), F2 eq fib(N2), T is F1+F2.

%?- T eq fib(5).
%@ T = 8.

