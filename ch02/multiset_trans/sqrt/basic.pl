%% 2.2.6 Newton’s method for square roots

:- use_module(library(chr)).

:- chr_constraint sqrt/2.

%% The simplification rule describe iter

sqrt(X,G) <=> abs(G*G/X-1)>0.01 | G2 is (G+X/G)/2, sqrt(X,G2).

%?- sqrt(2,5).
%@ sqrt(2,1.4144709813677712).

%?- sqrt(2, 1).
%@ sqrt(2,1.4166666666666665).


