%% 9.5 Feature terms
%% Arity constraints (CFT).

:- use_module(library(chr)).

:- op(100,xfx,'::').      % Variable::Sort/Expression  sort constraint
:- op(100,xfx,'@@'). % Variable@@LabelList        arity/label constraint
:- op(450,xfy,'##'). % Variable##Feature##Value   feature constraint
% in X@@A assumes that A is a sorted list of ground features 
% in X##F##Y assumes that feature F is a ground term and Y stays a variable or is atomic

:- op(600,xfx,'eq').

:- chr_constraint eq/2, (::)/2, (@@)/2, (##)/2.

% CFT Term Dissolution
X::T <=> nonvar(T), \+ atomic(T) | dissolve(X,T).

dissolve(X,T):- 
  T=..[S|Ls], X::S, dissolve1(X,Ls,A), sort(A,As), X@@As.

dissolve1(X,[],[]).
dissolve1(X,[L1::T1|Ls],[L1|Ls1]):- 
  X##L1##TV, 
  (   nonvar(T1) -> dissolve(TV,T1) ; TV=T1),
  dissolve1(X,Ls,Ls1).

variable_elimination @ X eq Y <=> X=Y.

% (S) sort are pairwise disjoint
sort_intersection @ X::S1 \ X::S2 <=> S1=S2.

% (F) features are functional
feature_decomposition @ X##L##Y \ X##L##Z <=> Y=Z.

% (A2) arities are unique
% sorting removes duplicate features
X@@A1 \ X@@A2 <=> A1=A2.

% (A1) If X has arity A, exactly the features in A are defined on X
X@@A, X##F##Y ==> member(F,A).

member(X,[Y|L]):- X=Y ; member(X,L).

%% Examples

%% ?- P::person(office::Y,
%%              home::Y,
%%              name::id(first::leo, last::smith)).
%@ $VAR(_A)::smith,
%@ $VAR(_B)::leo,
%@ $VAR(_C)::id,
%@ $VAR(P)::person,
%@ $VAR(P)@@[home,name,office],
%@ $VAR(_C)@@[first,last],
%@ $VAR(_A)@@[],
%@ $VAR(_B)@@[],
%@ $VAR(_C)##last## $VAR(_A),
%@ $VAR(_C)##first## $VAR(_B),
%@ $VAR(P)##name## $VAR(_C),
%@ $VAR(P)##home## $VAR(Y),
%@ $VAR(P)##office## $VAR(Y) ;
%@ false.

%% ?- X::person(spouse::Y), Y::person(spouse::X).
%@ $VAR(Y)::person,
%@ $VAR(X)::person,
%@ $VAR(Y)@@[spouse],
%@ $VAR(X)@@[spouse],
%@ $VAR(Y)##spouse## $VAR(X),
%@ $VAR(X)##spouse## $VAR(Y) ;
%@ false.

%% ?- X::person(spouse::person(spouse::X)).
%@ $VAR(_A)::person,
%@ $VAR(X)::person,
%@ $VAR(X)@@[spouse],
%@ $VAR(_A)@@[spouse],
%@ $VAR(_A)##spouse## $VAR(X),
%@ $VAR(X)##spouse## $VAR(_A) ;
%@ false.



% EXAMPLES ---------------------------------------------------------------

% page 236, determinant
eg0([U,V,W]-[X,Y,Z]):-
	X::a(f::V,g::Y),
	Y::b(f::X,g::Z,h::u),
	Z::a(f::W,g::Y,h::Z).

% cyclic structure, adapted from page 1, DEC-PRL RR 32
eg1(P):- 
    P::person(name::id(first::_,
		  last::S),
	 age::30,
	 spouse::person(name::id(last::S),
                        spouse::P)).

% cyclic list, adapted from p. 3, DEC-PRL RR 32
eg2(X):-
X::cons(head::1,tail::X).
eg2a(X):-	% same result as eg2(X)
X::cons(head::1,tail::X), X::cons(head::1,tail::cons(head::1,tail::X)).

% adapted from p.17, DEC-PRL RR 32
eg3(X):-			
X::s1(l1::s),X::s2(l2::s).


%% ?- eg0(X); eg1(X) ; eg2(X) ; eg2a(X) ; eg3(X).
%@ X = [$VAR(_),$VAR(_A),$VAR(_B)]-[$VAR(_C),$VAR(_D),$VAR(_E)],
%@ $VAR(_E)::a,
%@ $VAR(_F)::u,
%@ $VAR(_D)::b,
%@ $VAR(_C)::a,
%@ $VAR(_E)@@[f,g,h],
%@ $VAR(_D)@@[f,g,h],
%@ $VAR(_F)@@[],
%@ $VAR(_C)@@[f,g],
%@ $VAR(_E)##h## $VAR(_E),
%@ $VAR(_E)##g## $VAR(_D),
%@ $VAR(_E)##f## $VAR(_B),
%@ $VAR(_D)##h## $VAR(_F),
%@ $VAR(_D)##g## $VAR(_E),
%@ $VAR(_D)##f## $VAR(_C),
%@ $VAR(_C)##g## $VAR(_D),
%@ $VAR(_C)##f## $VAR(_A) ;

%@ $VAR(_A)::id,
%@ $VAR(_B)::person,
%@ $VAR(_C)::30,
%@ $VAR(_D)::id,
%@ $VAR(X)::person,
%@ $VAR(X)@@[age,name,spouse],
%@ $VAR(_B)@@[name,spouse],
%@ $VAR(_A)@@[last],
%@ $VAR(_C)@@[],
%@ $VAR(_D)@@[first,last],
%@ $VAR(_B)##spouse## $VAR(X),
%@ $VAR(_A)##last## $VAR(_E),
%@ $VAR(_B)##name## $VAR(_A),
%@ $VAR(X)##spouse## $VAR(_B),
%@ $VAR(X)##age## $VAR(_C),
%@ $VAR(_D)##last## $VAR(_E),
%@ $VAR(_D)##first## $VAR(_),
%@ $VAR(X)##name## $VAR(_D) ;

%@ $VAR(_A)::1,
%@ $VAR(X)::cons,
%@ $VAR(X)@@[head,tail],
%@ $VAR(_A)@@[],
%@ $VAR(X)##tail## $VAR(X),
%@ $VAR(X)##head## $VAR(_A) ;

%@ $VAR(_A)::1,
%@ $VAR(X)::cons,
%@ $VAR(X)@@[head,tail],
%@ $VAR(_A)@@[],
%@ $VAR(X)##tail## $VAR(X),
%@ $VAR(X)##head## $VAR(_A) ;
%@ false.

% end of handler cft ----------------------------------------------------------




