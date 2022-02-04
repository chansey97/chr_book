%% 9.5 Feature terms

:- use_module(library(chr)).

:- op(100,xfx,'::').   % Variable::Sort/Expression  sort constraint
:- op(450,xfy,'##').   % Variable##Feature##Value   feature constraint
% in X##F##Y assumes that feature F is a ground term and Y stays a variable or is atomic

:- op(600,xfx,'eq').

:- chr_constraint eq/2, (::)/2, (##)/2.

variable_elimination @ X eq Y <=> X=Y.

% (S) sort are pairwise disjoint
sort_intersection @ X::S1 \ X::S2 <=> S1=S2.

% (F) features are functional
feature_decomposition @ X##L##Y \ X##L##Z <=> Y=Z.

%% Examples

%% Three feature terms

%% ?- P::person,
%%    P##office##Y,
%%    P##home##Y, 
%%    P##name##C,
%%    C::id,
%%    C##last##A,
%%    C##first##B,
%%    A::smith,
%%    B::leo.
%@ $VAR(B)::leo,
%@ $VAR(A)::smith,
%@ $VAR(C)::id,
%@ $VAR(P)::person,
%@ $VAR(C)##first## $VAR(B),
%@ $VAR(C)##last## $VAR(A),
%@ $VAR(P)##name## $VAR(C),
%@ $VAR(P)##home## $VAR(Y),
%@ $VAR(P)##office## $VAR(Y).

%% One feature term, which is a cyclic structure
%% person(X) ∧ X.spouse.Y ∧ person(Y ) ∧ Y.spouse.X.

%% ?- X::person, X##spouse##Y, Y::person, Y##spouse##X.
%@ $VAR(Y)::person,
%@ $VAR(X)::person,
%@ $VAR(Y)##spouse## $VAR(X),
%@ $VAR(X)##spouse## $VAR(Y).

%% ?- X::person, X##name##Y, Y eq leo, X##livesin##Z, Z::munich, X##name##sepp.
%@ false.
