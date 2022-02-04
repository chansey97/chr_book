%% 8.1.3 Clauses and resolution

:- use_module(library(chr)).
:- use_module(library(lists)).
user:library_directory('../../../../common/').
:- use_module(library(ordering)). % need stable order on variables

:- chr_constraint cl/1, eliminate/1.

%% Clause as ordered list of signed variables, e.g. ¬x ∨ y ∨ z as cl([-x,+y,+z])
%% N.B.
%% 1. Propositional variables are just atom instead of prolog variables!
%% 2. The elements of the list must be different from each other. 
%% 3. The elements of the list must be variables lexicographical ordered.

%% The rules for resolution
empty_clause @ cl([]) <=> false.
tautology @ cl(L) <=> member(+X,L), member(-X,L) | true.
duplicate @ cl(L) \ cl(L) <=> true.

%% Specialized Resolution Rule, simply add eliminate
eliminate(X), cl(L1), cl(L2) ==> del(+X,L1,L3), del(-X,L2,L4)| ounion(L3,L4,L), cl(L).
%% Variable removal eventually
eliminate(X) \ cl(L) <=> (member(+X,L) ; member(-X,L)) | true.
eliminate(X) <=> true.

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


%% Examples

%% unsat
%% Projection of ∃p∈Bool.p∧¬p onto ∅ is ⊥
%% ?- cl([+p]), cl([-p]), eliminate(p).
%@ false.

%% ?- cl([+p, +q]), cl([-p]), cl([+p]), eliminate(p).
%@ false.

%% sat
%% Projection of ∃p∈Bool.p∧(¬p∨q) onto {q} is q
%% ?- cl([+p]), cl([-p, +q]), eliminate(p).
%@ cl([+q]).

%% ?- cl([+p]), cl([-p, +q]), eliminate(q).
%@ cl([+p]).

%% ?- cl([+p]), cl([+p, +q]), eliminate(q).
%@ cl([+p]).

%% ?- cl([+p, +q]), eliminate(p).
%@ true.

%% Constraints in conjunctive normal form (CNF):
%% (Y ⊓ Z = X) ≡ (¬X ⊔ Y ) ⊓ (¬X ⊔ Z) ⊓ (X ⊔ ¬Y ⊔ ¬Z)

%% Projection onto X and Z:

%% Clauses:
%% C0 = {¬X ⊔ Z} Y does not appear
%% C+ = {¬X ⊔ Y } Y appears positively
%% C− = {X ⊔ ¬Y ⊔ ¬Z} Y appears negatively
%% Resolution between C+ and C− (eliminate Y): {¬X ⊔ ¬Z ⊔ X}

%% Result: (¬X ⊔ ¬Z ⊔ X) ⊓ (¬X ⊔ Z) ≡ ¬X ⊔ Z ≡ X → Z

%% ?- cl([-x,+y]),cl([-x,+z]),cl([+x,-y,-z]), eliminate(y).
%@ cl([-x,+z]).

%% ?- cl([-x,+y]),cl([-x,+z]),cl([+x,-y,-z]), eliminate(z).
%@ cl([-x,+y]).

%% ?- cl([-x,+y]),cl([-x,+z]),cl([+x,-y,-z]), eliminate(x), eliminate(y).
%@ true.

%% ?-cl([-x,+y]),cl([+x]),cl([-y]),eliminate(x),eliminate(y).
%@ false.

%% https://www.cs.rochester.edu/~brown/173/lectures/logic/formal_logic/Resolution.html
%% convert all propositions in premises to CNF
%% premises: p, (p ∧ q) => r, (s ∨ t) => q, t
%% premises(CNF): p, ¬p ∨ ¬q ∨ r , ¬s ∨ q , ¬t ∨ q, t 

%% premises are consistency
%% ?- cl([+p]), cl([-p,-q,+r]), cl([+q,-s]), cl([+q,-t]), cl([+t]),
%%    eliminate(p),eliminate(q),eliminate(r),eliminate(s),eliminate(t).
%@ true.

%% To prove r, eliminate all variables
%% ?- cl([+p]), cl([-p,-q,+r]), cl([+q,-s]), cl([+q,-t]), cl([+t]),
%%    eliminate(p),eliminate(q),eliminate(s),eliminate(t).
%@ cl([+r]).

%% ?- cl([+p]), cl([-p,-q,+r]), cl([+q,-s]), cl([+q,-t]), cl([+t]),
%%    eliminate(p),eliminate(q),eliminate(r),eliminate(t).
%@ true. %% whis means s can have any value, e.g. true/false

%% (run* (a)
%%       (fresh (p q r s t)
%%              (smt-typeo p 'Bool)
%%              (smt-typeo q 'Bool)
%%              (smt-typeo r 'Bool)
%%              (smt-typeo s 'Bool)
%%              (smt-typeo t 'Bool)
%%              (smt-asserto `(and ,p
%%                                 (or (not ,p) (not ,q) ,r)
%%                                 (or ,q (not ,s))
%%                                 (or ,q (not ,t))
%%                                 ,t))   
%%              (== a `(,p ,q ,r ,s ,t)))
%%       )
%% ((#t #t #t #f #t) (#t #t #t #t #t))