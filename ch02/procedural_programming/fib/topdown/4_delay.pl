%% 2.3.2 Fibonacci
%% Top-down evaluation.

:- use_module(library(chr)).

:- chr_constraint fib/2.

f0 @ fib(N,M) <=> ground(N), N =:= 0 | M=1.
f1 @ fib(N,M) <=> ground(N), N =:= 1 | M=1.
fn @ fib(N,M) <=> ground(N), N>=2 | N1 is N-1, fib(N1,M1), N2 is N-2, fib(N2,M2), M is M1+M2.

%?- fib(10,OUT).
%@ OUT = 89.

%?- fib(N,233).
%@ fib($VAR(N),233).

%?- fib(N,233), N=12.
%@ N = 12.

%?- fib(N,Out).
%@ fib($VAR(N),$VAR(Out)).

%?- fib(N,Out), Out=233.
%@ Out = 233,
%@ fib($VAR(N),233).

%?- fib(N,Out), Out=233, N=5.
%@ false.

%?- fib(N,Out), N=12.
%@ N = 12,
%@ Out = 233.

%?- fib(12,233).
%@ true.
