%% 8.1.1 Boolean algebra

:- use_module(library(chr)).

:- chr_constraint neg/2, and/3, enum/1, indomain/1.

%% neg
neg(0,X) <=> X=1.
neg(X,0) <=> X=1.
neg(1,X) <=> X=0.
neg(X,1) <=> X=0.
neg(X,X) <=> fail.

%% propagating values
and(X,Y,Z) <=> X=0 | Z=0.
and(X,Y,Z) <=> Y=0 | Z=0.
and(X,Y,Z) <=> Z=1 | X=1,Y=1.

%% propagate equalities between variables
%% With equality propagation, the query and(1,Y,Z), neg(Y,Z) will reduce
%% to false, and this cannot be achieved by value propagation alone.
and(X,Y,Z) <=> X=1 | Y=Z.
and(X,Y,Z) <=> Y=1 | X=Z.
and(X,Y,Z) <=> X=Y | Y=Z.

%% labeling
enum([]) <=> true.
enum([X|L]) <=> indomain(X), enum(L).
indomain(X) <=> (X=0 ; X=1).

%?- and(1,Y,Z), neg(Y,Z).
%@ false.

%% The solver is incomplete. For example, the solver cannot detect
%% inconsistency of and(X,Y,Z),and(X,Y,W),neg(Z,W).

%?-and(X,Y,Z),and(X,Y,W),neg(Z,W).
%@ neg($VAR(Z),$VAR(W)),
%@ and($VAR(X),$VAR(Y),$VAR(W)),
%@ and($VAR(X),$VAR(Y),$VAR(Z)).

%? Consistency after labeling

%?- and(X,Y,Z),and(X,Y,W),neg(Z,W),enum([X,Y,Z,W]).
%@ false.

%?- and(X,Y,0),enum([X,Y]).
%@ X = Y, Y = 0 ;
%@ X = 0,
%@ Y = 1 ;
%@ X = 1,
%@ Y = 0 ;
%@ false.
