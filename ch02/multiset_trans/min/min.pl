%% 2.2.1 Minimum

:- use_module(library(chr)).

:- chr_constraint min/1.

%% min(N) \ min(M) <=> N=<M | true.

min(N) \ min(M) <=> N<M | true.
%% min(N) \ min(M) <=> N=M | true.

%?- min(1), min(2), min(1), min(2), min(3).
%@ min(1).

%?-  min(2), min(4), min(2), min(5).
%@ min(2).

%?- min(A), min(B).
%@ ERROR: Arguments are not sufficiently instantiated

%?- min(A), min(B), A=<B
%@ ERROR: Arguments are not sufficiently instantiated



