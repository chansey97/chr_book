:- use_module(library(chr)).

:- chr_constraint e/2,e_in/2,e_out/2.

%% N.B.
%% 1. This problem is checking a graph is an Eulerian graph instead of check a graph having an Eulerian path.
%% 2. This is for directed graphs.

e(X,Y) ==> e_in(X,Y), e_out(X,Y).
e_in(X,Y), e_out(Y,Z) <=> true.

