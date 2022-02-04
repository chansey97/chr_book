%% 2.4.1 Transitive closure

:- use_module(library(chr)).

:- chr_constraint n/1, cp/2, e/2, p/2.

%% There is also a direct and declarative way for a terminating
%% transitive closure program that does not rely on rule order. Having the
%% nodes of the graph also at hand as unary constraint, n, one simply generates
%% all candidates for all possible paths, cp, and uses simpagation rules.

%% generate all candidates for all possible paths
n(X) ==> cp(X,X).
n(X), n(Y) ==> cp(X,Y).

%% compute transitive closure in candidates
e(X,Y) \ cp(X,Y) <=> p(X,Y).
e(X,Y), p(Y,Z) \ cp(X,Z) <=> p(X,Z).

%% The cp constraints remaining after exhaustive application of the rules are those for
%% which no path exists, so they form the negation of the path constraint.

%% ?- n(1), n(2), n(3), e(1,2), e(2,1).
%@ n(3),
%@ n(2),
%@ n(1),
%@ cp(1,3),
%@ cp(2,3),
%@ cp(3,1),
%@ cp(3,2),
%@ cp(3,3),
%@ e(2,1),
%@ e(1,2),
%@ p(2,2),
%@ p(1,1),
%@ p(2,1),
%@ p(1,2).
