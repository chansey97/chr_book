%% 6.2.3 Petri nets

%% Refined semantics is not fair...

%% TODO:
%% 1. Add printing
%% 2. test in Parallel-CHR?

:- use_module(library(chr)).

:- chr_constraint t/1, f/1, e/1.

%% Fig. 6.2. The three dining philosophers problem as a colored Petri net

te@ t(X), f(X), f(Y) <=> Y =:= (X + 1) mod 3 | e(X).
et@ e(X) <=> Y = (X + 1) mod 3 | t(X), f(X), f(Y).

%?- t(0), t(1), t(2), f(0), f(1), f(2).
%% loop
