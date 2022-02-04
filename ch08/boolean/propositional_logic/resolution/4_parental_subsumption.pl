%% 8.1.3 Clauses and resolution

:- use_module(library(chr)).
:- use_module(library(lists)).
user:library_directory('../../../../common/').
:- use_module(library(ordering)). % need stable order on variables

:- chr_constraint cl/1.

%% Clause as ordered list of signed variables, e.g. ¬x ∨ y ∨ z as cl([-x,+y,+z])
%% N.B.
%% 1. Propositional variables are just atom instead of prolog variables!
%% 2. The elements of the list must be different from each other. 
%% 3. The elements of the list must be variables lexicographical ordered.

%% The rules for resolution
empty_clause @ cl([]) <=> false.
tautology @ cl(L) <=> member(+X,L), member(-X,L) | true.
subsumption @ cl(L1) \ cl(L2) <=> sublist(L1,L2) | true.
%% parental_subsumption is redundant, but may be more effective.
parental_subsumption @ cl(L1) \ cl(L2) <=> del(A,L1,L3), del(B,L2,L4), compl(A,B), sublist(L3,L4) | cl(L4).
resolution @ cl(L1), cl(L2) ==> del(+X,L1,L3), del(-X,L2,L4) | ounion(L3,L4,L), cl(L).

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

%% compl(A,B) holds if literal A is the logical complement (negation) of B.
compl(+X, -X).
compl(-X, +X).

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

%% https://www.cs.rochester.edu/~brown/173/lectures/logic/formal_logic/Resolution.html
%% convert all propositions in premises to CNF
%% premises: p, (p ∧ q) => r, (s ∨ t) => q, t
%% premises(CNF): p, ¬p ∨ ¬q ∨ r , ¬s ∨ q , ¬t ∨ q, t 

%% premises are consistency
%% ?- cl([+p]), cl([-p,-q,+r]), cl([+q,-s]), cl([+q,-t]), cl([+t]).
%@ cl([+q]),
%@ cl([+r]),
%@ cl([+t]),
%@ cl([+p]).
%% s doesn't appeared in the result store, so s can be any value, true/false.

%% %% To prove r, verify ¬r
%% ?- cl([+p]), cl([-p,-q,+r]), cl([+q,-s]), cl([+q,-t]), cl([+t]), cl([-r]).
%@ false.
%% so r is proved.
