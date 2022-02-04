%% 2.4.1 Transitive closure
%% Shortest paths.

:- use_module(library(chr)).

:- chr_constraint e/2, p/3.

%% 这个算法推广自 reachability/single_source.pl
%% 每输入一个 e(X,Y)，得到对应 p(X,Y,1)，然后往前找到 e(Y,Z) => p(X,Z,N1)
%% 如果你的输入 edge 是向前的话，那基本是找不到的，
%% 但 e(X,Y) 仍然是 active，于是向后找一个p(W,X,N)，生成p(X,Z,N1)

p(X,Y,N) \ p(X,Y,M) <=> N=<M | true.
e(X,Y) ==> p(X,Y,1).
p(X,Y,N), e(Y,Z) ==> N1 is N+1, p(X,Z,N1).

%% ?- e(a,b),e(b,c), e(c,d).
%% e(c,d),
%% e(b,c),
%% e(a,b),
%% p(a,b,1).
%% p(a,c,2),
%% p(a,d,3),
%% p(b,c,1),
%% p(b,d,2),
%% p(c,d,1),

%% ?- e(a,b),e(b,c), e(c,d),e(d,e).
%% e(d,e),
%% e(c,d),
%% e(b,c),
%% e(a,b),
%% p(a,b,1).
%% p(a,c,2),
%% p(a,d,3),
%% p(a,e,4),
%% p(b,c,1),
%% p(b,d,2),
%% p(b,e,3),
%% p(c,d,1),
%% p(c,e,2),
%% p(d,e,1),

%% ?- e(a,b),e(b,c), e(c,d),e(d,e), e(a,c).
%% e(a,c),
%% e(d,e),
%% e(c,d),
%% e(b,c),
%% e(a,b),
%% p(a,b,1).
%% p(a,c,1),
%% p(a,d,2),
%% p(a,e,3),
%% p(b,c,1),
%% p(b,d,2),
%% p(b,e,3),
%% p(c,d,1),
%% p(c,e,2),
%% p(d,e,1),
