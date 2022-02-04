%% 2.4.1 Transitive closure
%% Shortest paths.

:- use_module(library(chr)).

:- chr_constraint e/2, p/4.

p(X,Y,N,_) \ p(X,Y,M,_) <=> N=<M | true.
e(X,Y) ==> p(X,Y,1,[e(X,Y)]).
e(X,Y), p(Y,Z,N,P) ==> N1 is N+1, P1 = [e(X,Y)|P], p(X,Z,N1,P1).

%% ?- e(a,b),e(a,c),e(b,d),e(b,e),e(e,x),e(x,z),e(y,z),e(u,y),e(v,y).
%% e(v,y),
%% e(u,y),
%% e(y,z),
%% e(x,z),
%% e(e,x),
%% e(b,e),
%% e(b,d),
%% e(a,c),
%% e(a,b),
%% p(v,z,2,[e(v,y),e(y,z)]),
%% p(v,y,1,[e(v,y)]),
%% p(u,z,2,[e(u,y),e(y,z)]),
%% p(u,y,1,[e(u,y)]),
%% p(y,z,1,[e(y,z)]),
%% p(a,z,4,[e(a,b),e(b,e),e(e,x),e(x,z)]),
%% p(b,z,3,[e(b,e),e(e,x),e(x,z)]),
%% p(e,z,2,[e(e,x),e(x,z)]),
%% p(x,z,1,[e(x,z)]),
%% p(a,x,3,[e(a,b),e(b,e),e(e,x)]),
%% p(b,x,2,[e(b,e),e(e,x)]),
%% p(e,x,1,[e(e,x)]),
%% p(a,e,2,[e(a,b),e(b,e)]),
%% p(b,e,1,[e(b,e)]),
%% p(a,d,2,[e(a,b),e(b,d)]),
%% p(b,d,1,[e(b,d)]),
%% p(a,c,1,[e(a,c)]),
%% p(a,b,1,[e(a,b)]).
