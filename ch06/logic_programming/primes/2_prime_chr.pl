%% 6.3.1 Prolog and constraint logic programming

%% No need to explicitly maintain a list of prime candidates.

:- use_module(library(chr)).

:- chr_constraint upto/1, prime/1.

upto(N) <=> N>1 | M is N-1, upto(M), prime(N).
sift @ prime(I) \ prime(J) <=> J mod I =:= 0 | true.

%?- upto(10).
%@ upto(1),
%@ prime(7),
%@ prime(5),
%@ prime(3),
%@ prime(2).