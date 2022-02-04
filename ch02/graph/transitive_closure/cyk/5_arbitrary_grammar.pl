%% 2.4.3 Grammar parsing

%% Input a CNF grammar (→)/2 and a string as graph chain of terminal symbols e/3.
%% Output paths of the restricted transitive closure satisfied the grammar.

:- use_module(library(chr)).

:- op(600, xfx, →).

:- chr_constraint
  
  % an edge of a graph chain
  e/3,
  
  % a path of transitive closure
  p/3,
  
  % production  
  (→)/2.

%% If the grammar rules are fixed, we may compile them away.

%% Given an arbitrary grammar, we just need to chain the path and edge constraints
%% in the CHR rule that corresponds to the sequence of nonterminal
%% and terminal symbols in the grammar rule.

%% For example,

%% G → a*G
%% G → a*G

duplicate @ p(A,I,J) \ p(A,I,J) <=> true.
'G → a*G' @ e(a,I,J), p(s_G,J,K) ==> p(s_G,I,K).
'G → a' @ e(a,I,J) ==> p(s_G,I,J).

%?- e(a,0,1), e(a,1,2).
%@ e(a,1,2),
%@ e(a,0,1),
%@ p(s_G,0,2),
%@ p(s_G,1,2),
%@ p(s_G,0,1).
