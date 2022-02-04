%% 6.2.1 Term rewriting systems
%% Functional programming.

:- use_module(library(chr)).

:- op(600, xfx, eq).

:- chr_constraint eq/2.

%% FP
%% and(0,Y) -> 0.
%% and(X,0) -> 0.
%% and(1,Y) -> Y.
%% and(X,1) -> X.
%% and(X,X) -> X.

%% X eq T <=> datum(T) | X=T.
%% X eq T <=> builtin(T) | c(T,X).

T eq and(0,Y) <=> T=0.
T eq and(X,0) <=> T=0.
T eq and(1,Y) <=> T eq Y.
T eq and(X,1) <=> T eq X.
T eq and(X,Y) <=> T eq X.

%?- T eq and(and(0,1), and(0,1)).
%@ T = 0.

