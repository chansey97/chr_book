%% 2.2.6 Newton’s method for square roots

:- use_module(library(chr)).

:- chr_constraint sqrt/2, improve/1.

improve(sqrt(X)), sqrt(X,G) <=> G2 is (G+X/G)/2, sqrt(X,G2).

%?- sqrt(2, 1), improve(sqrt(2)).
%@ sqrt(2,1.5).

%?- sqrt(2, 1), improve(sqrt(2)), improve(sqrt(2)).
%@ sqrt(2,1.4166666666666665).

%?- sqrt(2, 1), improve(sqrt(2)), improve(sqrt(2)), improve(sqrt(2)).
%@ sqrt(2,1.4142156862745097).