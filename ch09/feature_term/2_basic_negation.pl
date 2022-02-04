%% 9.5 Feature terms
%% Negation.

:- use_module(library(chr)).

:- op(100,xfx,'::').   % Variable::Sort/Expression  sort constraint
:- op(450,xfy,'##').   % Variable##Feature##Value   feature constraint
% in X##F##Y assumes that feature F is a ground term and Y stays a variable or is atomic

:- op(600,xfx,'eq').
:- op(600,xfx,'neq').
:- op(100,xfx,'::¬').
:- op(500,fx,'¬').

:- chr_constraint eq/2, neq/2, (::¬)/2, (¬)/1, (::)/2, (##)/2.

variable_elimination @ X eq Y <=> X=Y.

% (S) sort are pairwise disjoint
sort_intersection @ X::S1 \ X::S2 <=> S1=S2.

% (F) features are functional
feature_decomposition @ X##L##Y \ X##L##Z <=> Y=Z.

X neq X <=> false.
X::S, X::¬ S <=> false.
X##F##Z, ¬ X##F <=> false.

