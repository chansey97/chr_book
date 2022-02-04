%% 2.4.1 Transitive closure

:- use_module(library(chr)).

:- chr_constraint e/2, p/2.

%% set-based semantics for termination (no built-in tabling needed)
dp @ p(X,Y) \ p(X,Y) <=> true.

p1 @ e(X,Y) ==> p(X,Y).
pn @ e(X,Y), p(Y,Z) ==> p(X,Z).

%% ?- e(a,a).
%@ e(a,a),
%@ p(a,a).

%% ?- e(a,b),e(b,a).
%@ e(b,a),
%@ e(a,b),
%@ p(b,b),
%@ p(a,a),
%@ p(b,a),
%@ p(a,b).

%% ?- e(a,b),e(b,c).
%@ e(b,c),
%@ e(a,b),
%@ p(a,c),
%@ p(b,c),
%@ p(a,b).