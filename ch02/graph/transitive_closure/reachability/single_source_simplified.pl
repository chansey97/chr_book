%% 2.4.1 Transitive closure
%% Reachability: single-source and single-target paths.

:- use_module(library(chr)).

:- chr_constraint source/1, e/2, p/1.

%% single-source / single-target path is a specialized transitive closure

%% Note that any path constraint produced will have the same node as the
%% first argument, that given in the source constraint.
%% Thus, we can simplify the code as follows:

%% Good pattern for single source shortest paths algorithm (e.g. Dijkstra).

p(X) \ p(X) <=> true.
source(X), e(X,Y) ==> p(Y).
p(Y), e(Y,Z) ==> p(Z).

%% N.B. In refined semantics,
%% the order of inputs e/2 and source/1 reflect different search order. 

%% DFS
%?- e(a,b),e(b,c),e(c,d),source(a).
%@ source(a),
%@ e(c,d),
%@ e(b,c),
%@ e(a,b),
%@ p(d),
%@ p(c),
%@ p(b).

%% DFS
%?- e(a,b),e(b,c),e(c,d),e(a,z),e(a,y),source(a).
%@ source(a),
%@ e(a,y),
%@ e(a,z),
%@ e(c,d),
%@ e(b,c),
%@ e(a,b),
%@ p(d),
%@ p(c),
%@ p(b),
%@ p(z),
%@ p(y).

%% BFS
%?- source(a),e(a,b),e(a,z),e(a,y),e(b,c),e(c,d).
%@ source(a),
%@ e(c,d),
%@ e(b,c),
%@ e(a,y),
%@ e(a,z),
%@ e(a,b),
%@ p(d),
%@ p(c),
%@ p(y),
%@ p(z),
%@ p(b).
