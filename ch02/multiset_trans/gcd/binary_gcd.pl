%% 2.2.3 Greatest common divisor
%% Program variation: binary gcd.

:- use_module(library(chr)).

:- chr_constraint gcd/2.

even(X) :- 0 is mod(X, 2).
odd(X) :- 1 is mod(X, 2).


gcd(0,_) <=> true.
gcd(N,A) <=> 0<N,even(N) | V is N//2, gcd(V,A).
gcd(N,A) \ gcd(M,B) <=> 0<N,N=<M, odd(N),odd(M)| V is M-N,
                                                 Min is min(A,B),
                                                 gcd(V,Min).

%?- gcd(94017,94017), gcd(1155,1155), gcd(2035,2035).
%@ gcd(11,1155).

