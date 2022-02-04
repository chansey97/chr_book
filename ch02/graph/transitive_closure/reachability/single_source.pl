%% 2.4.1 Transitive closure
%% Reachability: single-source and single-target paths.

:- use_module(library(chr)).

:- chr_constraint source/1, e/2, p/2.

%% single-source / single-target path is a specialized transitive closure
source(X), e(X,Y) ==> p(X,Y).
source(X), p(X,Y), e(Y,Z) ==> p(X,Z).

%% N.B. In refined semantics,
%% the order of inputs e/2 and source/1 reflect different search order. 

%% DFS
%?- e(a,b),e(b,c),e(c,d),source(a).
%@ source(a),
%@ e(c,d),
%@ e(b,c),
%@ e(a,b),
%@ p(a,d),
%@ p(a,c),
%@ p(a,b).

%% DFS
%?- e(a,b),e(b,c),e(c,d),e(a,z),e(a,y),source(a).
%@ source(a),
%@ e(a,y),
%@ e(a,z),
%@ e(c,d),
%@ e(b,c),
%@ e(a,b),
%@ p(a,d),
%@ p(a,c),
%@ p(a,b),
%@ p(a,z),
%@ p(a,y).

%% BFS
%?- source(a),e(a,b),e(a,z),e(a,y),e(b,c),e(c,d).
%@ source(a),
%@ e(c,d),
%@ e(b,c),
%@ e(a,y),
%@ e(a,z),
%@ e(a,b),
%@ p(a,d),
%@ p(a,c),
%@ p(a,y),
%@ p(a,z),
%@ p(a,b).
