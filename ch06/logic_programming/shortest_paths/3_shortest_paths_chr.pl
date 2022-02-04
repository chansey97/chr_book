%% 6.3.1 Prolog and constraint logic programming

:- use_module(library(chr)).

:- chr_constraint e/2, p/3.

p(X,Y,N) \ p(X,Y,M) <=> N=<M | true.
e(X,Y) ==> p(X,Y,1).
e(X,Y), p(Y,Z,N) ==> N1 is N+1, p(X,Z,N1).
