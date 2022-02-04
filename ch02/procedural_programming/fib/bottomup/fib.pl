%% 2.3.2 Fibonacci
%% Bottom-up evaluation.

:- use_module(library(chr)).

:- chr_constraint fib/2, upto/1.

f01 @ upto(Max) ==> fib(0,1), fib(1,1).
fn @  upto(Max), fib(N1,M1), fib(N2,M2) ==> Max>N2, N2=:=N1+1 | N is N2+1, M is M1+M2, fib(N,M).

%?- upto(8).
%@ fib(8,34),
%@ fib(7,21),
%@ fib(6,13),
%@ fib(5,8),
%@ fib(4,5),
%@ fib(3,3),
%@ fib(2,2),
%@ fib(1,1),
%@ fib(0,1),
%@ upto(8).

