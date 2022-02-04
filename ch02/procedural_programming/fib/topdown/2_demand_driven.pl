%% 2.3.2 Fibonacci
%% Top-down evaluation.

:- use_module(library(chr)).
:- use_module(library(clpfd)).

:- chr_constraint fib/2, demand/0.

f0 @ demand, fib(0,M) <=> M=1.
f1 @ demand, fib(1,M) <=> M=1.
%% fn @ demand, fib(N,M) <=> N>=2 | N1 is N-1, fib(N1,M1), N2 is N-2, fib(N2,M2), M is M1+M2.
fn @ demand, fib(N,M) <=> N>=2 | N1 is N-1, fib(N1,M1), N2 is N-2, fib(N2,M2), M #= M1+M2.

%% 如果想 demonad driven fib，则必须使用 clpfd，因为 M is M1+M2 里 M1和 M2 没有实例化。
%% fib(N1,M1) 和 fib(N2,M2) 可以被正常 delay 因为 demand 被删除，但是 M is M1+M2 也必须被 delay。

%?- fib(3,M), demand.
%@ fib(1,$VAR(_A)),
%@ fib(2,$VAR(_B)),
%@ $VAR(_B)+ $VAR(_A)#= $VAR(M).

%?- fib(3,M), demand, demand.
%@ fib(2,$VAR(_A)),
%@ $VAR(_A)+1#= $VAR(M).

%?- fib(3,M), demand, demand, demand.
%@ fib(0,$VAR(_A)),
%@ fib(1,$VAR(_B)),
%@ $VAR(_B)+ $VAR(_A)#= $VAR(_C),
%@ $VAR(_C)+1#= $VAR(M).

%?- fib(3,M), demand, demand, demand, demand.
%@ fib(1,$VAR(_A)),
%@ $VAR(_A)+1#= $VAR(_B),
%@ $VAR(_B)+1#= $VAR(M).

%?- fib(3,M), demand, demand, demand, demand, demand.
%@ M = 3.
