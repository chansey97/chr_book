%% 10.4 Generalizing union-find

:- use_module(library(chr)).

:- chr_constraint bind/0, make/1, root/2, p/3, union/3, find/3, link/3.

% Based on optimized union-find according to TPLP programming pearl 2006

% bind binds variables after computations by propagating "1" down the tree
bind \ p(X,A/B,N) <=> number(N) | X is A*N+B.


make      @ make(X) <=> root(X,0).
union     @ union(X,XY,Y) <=> find(X,XA,A), find(Y,YB,B), combine(XA,YB,XY,AB), link(A,AB,B).

findNode  @ p(X,XY,Y), find(X,XR,R) <=> find(Y,YR,R), compose(XY,YR,XR), p(X,XR,R).
findRoot  @ root(X,_) \ find(X,XR,R) <=> equal(XR), X=R.

%% N.B. `equal` is GENERIC, which need to be implemented in specific applications. 
%% see 2_ufe_bool.pl and 3_ufe_linear_polynomial.pl
%% linkEq @ link(X,XX,X) <=> equal(XX).

linkLeft  @ link(X,XY,Y), root(X,RX), root(Y,RY) <=>
               RX >= RY |
               invert(XY,YX),
               p(Y,YX,X),
               NRX is max(RX,RY+1), root(X,NRX).
linkRight @ link(X,XY,Y), root(Y,RY), root(X,RX) <=>
               RY >= RX |
               p(X,XY,Y),
               NRY is max(RY,RX+1), root(Y,NRY).

%% Need to implement equal