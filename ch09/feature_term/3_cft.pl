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

%% ?- X::g, X##1##Y, X##2##Z.
%@ $VAR(X)::g,
%@ $VAR(X)##2## $VAR(Z),
%@ $VAR(X)##1## $VAR(Y).

%% ?- X::g, X##1##Y, X##2##Z, X@@[1,2].
%@ $VAR(X)::g,
%@ $VAR(X)@@[1,2],
%@ $VAR(X)##2## $VAR(Z),
%@ $VAR(X)##1## $VAR(Y) ;
%@ false.

%% ?- X::g, X##1##Y, X@@[1,2].
%@ $VAR(X)::g,
%@ $VAR(X)@@[1,2],
%@ $VAR(X)##1## $VAR(Y) ;
%@ false.

%% ?- X::g, X##1##Y, X##2##Z, X##3##V, X@@[1,2].
%@ false.

%% ?- X::g, X##1##Y, X##3##V, X@@[1,2].
%@ false.

