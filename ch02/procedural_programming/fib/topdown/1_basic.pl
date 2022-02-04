%% 2.3.2 Fibonacci
%% Top-down evaluation.

:- use_module(library(chr)).

:- chr_constraint fib/2.

f0 @ fib(0,M) <=> M=1.
f1 @ fib(1,M) <=> M=1.
fn @ fib(N,M) <=> N>=2 | N1 is N-1, fib(N1,M1), N2 is N-2, fib(N2,M2), M is M1+M2.

%?- fib(4,A).
%@ A = 5.

%?- fib(A,5).
%@ ERROR: Arguments are not sufficiently instantiated
