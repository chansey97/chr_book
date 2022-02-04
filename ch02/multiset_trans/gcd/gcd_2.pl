%% 2.2.3 Greatest common divisor

:- use_module(library(chr)).

:- chr_constraint gcd/1.

gcd(N) \ gcd(M) <=> 0<N,N=<M | V is M mod N, gcd(V).
gcd(0) <=> true.

%?- gcd(94017), gcd(1155), gcd(2035).
%@ gcd(11).

