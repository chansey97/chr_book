%% 2.3.1 Maximum

:- use_module(library(chr)).

:- chr_constraint max/3.

max(X,Y,Z) <=> X=<Y | Z=Y.
max(X,Y,Z) <=> Y=<X | Z=X.

%?- max(1,2,M).
%@ M = 2.

%% N,.B. Although both simplification rules (i.e. overlapping) could be applicable,
%% in refined semantics, it is a committed choice language, so only 1st rule applied.

%?- max(1,1,M).
%@ M = 1.
