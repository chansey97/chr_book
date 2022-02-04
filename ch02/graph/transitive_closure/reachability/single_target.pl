%% 2.4.1 Transitive closure
%% Reachability: single-source and single-target paths.

:- use_module(library(chr)).

:- chr_constraint source/1, target/1, e/2, p/2.

%% single-source / single-target path is a specialized transitive closure
target(Y), e(X,Y) ==> p(X,Y).
target(Z), e(X,Y), p(Y,Z) ==> p(X,Z).

%% N.B. In refined semantics,
%% the order of inputs e/2 and source/1 reflect different search order. 

%% DFS
%% ?- e(a,b),e(a,c),e(b,d),e(b,e),e(e,x),e(x,z),e(y,z),e(u,y),e(v,y), target(z).
%% target(z),
%% e(v,y),
%% e(u,y),
%% e(y,z),
%% e(x,z),
%% e(e,x),
%% e(b,e),
%% e(b,d),
%% e(a,c),
%% e(a,b),
%% p(a,z),
%% p(b,z),
%% p(e,z),
%% p(x,z),
%% p(u,z),
%% p(v,z),
%% p(y,z).
