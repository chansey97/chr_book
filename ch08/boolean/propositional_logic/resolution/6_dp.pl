%% 8.1.3 Clauses and resolution
%% Davis–Putnam procedure (unit resolution).

%% https://en.wikipedia.org/wiki/Unit_propagation

:- use_module(library(chr)).
:- use_module(library(lists)).
user:library_directory('../../../../common/').
:- use_module(library(ordering)). % need stable order on variables

:- chr_constraint cl/1, enum/1, indomain/1.

%% Clause as ordered list of signed variables, e.g. ¬x ∨ y ∨ z as cl([-x,+y,+z])
%% N.B.
%% 1. Propositional variables are just atom instead of prolog variables!
%% 2. The elements of the list must be different from each other. 
%% 3. The elements of the list must be variables lexicographical ordered.

%% The rules for resolution
empty_clause @ cl([]) <=> false.
tautology @ cl(L) <=> member(+X,L), member(-X,L) | true.
unit_subsumption @ cl([A]) \ cl(L) <=> member(A,L) | true.
unit_propagation @ cl([A]) \ cl(L) <=> compl(A,B), del(B,L,L1)| cl(L1).

%% Unit resolution is clearly incomplete, because there are unsatisfiable
%% problems that do not have any unit clauses.

%% We retain completeness for unit resolution by labeling search.
enum([]) <=> true.
enum([X|L]) <=> indomain(X), enum(L).
indomain(X) <=> (cl([+X]) ; cl([-X])).

%% Alternatively, we label by the literals in the clauses using an auxiliary dummy
%% constraint label:
%% label, cl(L) <=> member(A,L), cl([A]), label.

%% Auxiliary predicates
  
%% del(?Element, +List, -Rest)
%% del(A,L1,L2): list L1 without element A is L2. 
%% N.B. It is not guaranteed to remove all A in L1, but we have assumed no duplicates in L1.  
del(X, [X|L],  L).
del(Y, [X|Xs], [X|Xt]) :- del(Y, Xs, Xt).

%% compl(A,B) holds if literal A is the logical complement (negation) of B.
compl(+X, -X).
compl(-X, +X).

%% Examples

%% unsat 
%% ?- cl([+p]), cl([-p]), enum([p]).
%@ false.

%% sat
%% ?- cl([+p]), cl([-p, +q]), enum([p,q]).
%@ cl([+q]),
%@ cl([+p]) ;
%@ false.

%% ?- cl([+p]), cl([+p, +q]), enum([p,q]).
%@ cl([+q]),
%@ cl([+p]) ;
%@ cl([-q]),
%@ cl([+p]) ;
%@ false.

%% https://www.cs.rochester.edu/~brown/173/lectures/logic/formal_logic/Resolution.html
%% convert all propositions in premises to CNF
%% premises: p, (p ∧ q) => r, (s ∨ t) => q, t
%% premises(CNF): p, ¬p ∨ ¬q ∨ r , ¬s ∨ q , ¬t ∨ q, t 

%% premises are consistency
%% ?- cl([+p]), cl([-p,-q,+r]), cl([+q,-s]), cl([+q,-t]), cl([+t]).
%@ cl([+r]),
%@ cl([+q]),
%@ cl([+t]),
%@ cl([+p]).
%% s doesn't appeared in the result store, so s can be any value, true/false.

%% %% To prove r, verify ¬r
%% ?- cl([+p]), cl([-p,-q,+r]), cl([+q,-s]), cl([+q,-t]), cl([+t]), cl([-r]).
%@ false.
%% so r is proved.

%% Boolean Schur triples
%% Does there exist a red/blue coloring of the numbers 1, ..., n such that
%% there is no monochromatic solution of a + b = c with a < b < c ≤ 𝑛?
%% Answer: when n=9, it doesn't exist.

%% ?- cl([+x1,+x2,+x3]),cl([-x1,-x2,-x3]),
%%    cl([+x1,+x3,+x4]),cl([-x1,-x3,-x4]),
%%    cl([+x1,+x4,+x5]),cl([-x1,-x4,-x5]),
%%    cl([+x2,+x3,+x5]),cl([-x2,-x3,-x5]),
%%    cl([+x1,+x5,+x6]),cl([-x1,-x5,-x6]),
%%    cl([+x2,+x4,+x6]),cl([-x2,-x4,-x6]),
%%    cl([+x1,+x6,+x7]),cl([-x1,-x6,-x7]),
%%    cl([+x2,+x5,+x7]),cl([-x2,-x5,-x7]),
%%    cl([+x3,+x4,+x7]),cl([-x3,-x4,-x7]),
%%    cl([+x1,+x7,+x8]),cl([-x1,-x7,-x8]),
%%    cl([+x2,+x6,+x8]),cl([-x2,-x6,-x8]),
%%    cl([+x3,+x5,+x8]),cl([-x3,-x5,-x8]),
%%    cl([+x1,+x8,+x9]),cl([-x1,-x8,-x9]),
%%    cl([+x2,+x7,+x9]),cl([-x2,-x7,-x9]),
%%    cl([+x3,+x6,+x9]),cl([-x3,-x6,-x9]),
%%    cl([+x4,+x5,+x9]),cl([-x4,-x5,-x9]),
%%    enum([x1,x2,x3,x4,x5,x6,x7,x8,x9]).
%@ false.
