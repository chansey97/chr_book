%% 2.3.2 Fibonacci
%% Top-down evaluation.

:- use_module(library(chr)).

:- chr_constraint fib/2.

%% This called functional dependency.
mem @ fib(N,M1) \ fib(N,M2) <=> M1=M2. % memorization is trivial in CHR

f0 @ fib(0,M) ==> M=1.
f1 @ fib(1,M) ==> M=1.
fn @ fib(N,M) ==> N>=2 | N1 is N-1, fib(N1,M1), N2 is N-2, fib(N2,M2), M is M1+M2.

%?- fib(8, X).
%@ X = 34,
%@ fib(0,1),
%@ fib(1,1),
%@ fib(2,2),
%@ fib(3,3),
%@ fib(4,5),
%@ fib(5,8),
%@ fib(6,13),
%@ fib(7,21),
%@ fib(8,34).
