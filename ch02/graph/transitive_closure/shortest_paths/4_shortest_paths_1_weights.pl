%% 2.4.1 Transitive closure
%% Shortest paths.

:- use_module(library(chr)).

:- chr_constraint e/3, p/3.

p(X,Y,N) \ p(X,Y,M) <=> N=<M | true.
e(X,Y,D) ==> p(X,Y,D).
e(X,Y,D), p(Y,Z,N) ==> ND is N+D, p(X,Z,ND).

%% can support negative weight edges
%% ?- e(a,b,5),e(a,c,2),e(b,c,-10).
%% e(b,c,-10),
%% e(a,c,2),
%% e(a,b,5),
%% p(a,c,-5),
%% p(b,c,-10),
%% p(a,b,5).

%% can not support negative circle
%% e(a,b,-1), e(b,a,-2).
%% loop