%% 8.1.3 Clauses and resolution
%% Ordered resolution.

:- use_module(library(chr)).
:- use_module(library(lists)).
user:library_directory('../../../../common/').
:- use_module(library(ordering)). % need stable order on variables

:- chr_constraint cl/1.

%% Clause as ordered list of signed variables, e.g. ¬x ∨ y ∨ z as cl([-x,+y,+z])
%% N.B. propositional variables are just atom instead of prolog variables!

%% The rules for resolution
empty_clause @ cl([]) <=> false.
tautology @ cl(L) <=> member(+X,L), member(-X,L) | true.
subsumption @ cl(L1) \ cl(L2) <=> sublist(L1,L2) | true.
%% This is a restriction of resolution, where resolution is
%% only performed with the leftmost literals of each clause. This approach is still
%% complete, since a set of such ordered clauses where the leftmost variables are
%% all different is always satisfiable.
ordered_resolution @ cl([+X|L1]), cl([-X|L2]) ==> ounion(L1,L2,L), cl(L).

%% Auxiliary predicates

%% del(?Element, +List, -Rest)
%% del(A,L1,L2): list L1 without element A is L2. 
%% N.B. It is not guaranteed to remove all A in L1, but we have assumed no duplicates in L1.  
del(X, [X|L],  L).
del(Y, [X|Xs], [X|Xt]) :- del(Y, Xs, Xt).

%% ordered union of L1 and L2 is L.
ounion([], L, L) :- !.
ounion(L, [], L) :- !.

ounion([X|L1], [X|L2], [X|L3]) :- !, ounion(L1,L2,L3).
ounion([+X|L1], [-X|L2], [+X|L3]) :- !, ounion(L1,[-X|L2],L3).
ounion([-X|L1], [+X|L2], [+X|L3]) :- !, ounion([-X|L1],L2,L3).

ounion([+X1|L1], [+X2|L2], [+X1|L3]) :- X1 @< X2, !, ounion(L1,[+X2|L2],L3).
ounion([+X1|L1], [-X2|L2], [+X1|L3]) :- X1 @< X2, !, ounion(L1,[-X2|L2],L3).
ounion([-X1|L1], [+X2|L2], [-X1|L3]) :- X1 @< X2, !, ounion(L1,[+X2|L2],L3).
ounion([-X1|L1], [-X2|L2], [-X1|L3]) :- X1 @< X2, !, ounion(L1,[-X2|L2],L3).

ounion([+X1|L1], [+X2|L2], [+X2|L3]) :- X1 @> X2, !, ounion([+X1|L1],L2,L3).
ounion([+X1|L1], [-X2|L2], [-X2|L3]) :- X1 @> X2, !, ounion([+X1|L1],L2,L3).
ounion([-X1|L1], [+X2|L2], [+X2|L3]) :- X1 @> X2, !, ounion([-X1|L1],L2,L3).
ounion([-X1|L1], [-X2|L2], [-X2|L3]) :- X1 @> X2, !, ounion([-X1|L1],L2,L3).

%% sublist(L1,L2): all elements of L1 occur in L2.
sublist(Sub, List) :- subset(Sub, List).

%% Examples

%% unsat 
%% ?- cl([+p]), cl([-p]).
%@ false.

%% sat
%% ?- cl([+p]), cl([-p, +q]).
%@ cl([+q]),
%@ cl([+p]).

%% ?- cl([+p]), cl([+p, +q]).
%@ cl([+p]).
%% q doesn't appeared in the result store, so s can be any value, true/false.

%% still complete
%% ?- cl([+a, +p]), cl([+a, -p]).
%@ cl([+a,-p]),
%@ cl([+a,+p]).

%% still complete
%% ?- cl([+o, +r]), cl([+p, -r]).
%@ cl([+p,-r]),
%@ cl([+o,+r]).

%% https://www.cs.rochester.edu/~brown/173/lectures/logic/formal_logic/Resolution.html
%% convert all propositions in premises to CNF
%% premises: p, (p ∧ q) => r, (s ∨ t) => q, t
%% premises(CNF): p, ¬p ∨ ¬q ∨ r , ¬s ∨ q , ¬t ∨ q, t 

%% premises are consistency
%% ?- cl([+p]), cl([-p,-q,+r]), cl([+q,-s]), cl([+q,-t]), cl([+t]).
%@ cl([+t]),
%@ cl([+r,-t]),
%@ cl([+q,-t]),
%@ cl([+r,-s]),
%@ cl([+q,-s]),
%@ cl([-q,+r]),
%@ cl([+p]).
%% although complete, but result is more verbose, for example,
%% s can be any value, true/false, but still in the store.

%% %% To prove r, verify ¬r
%% ?- cl([+p]), cl([-p,-q,+r]), cl([+q,-s]), cl([+q,-t]), cl([+t]), cl([-r]).
%@ false.
%% so r is proved.
