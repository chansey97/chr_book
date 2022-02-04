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

%% We can generalize the algorithm to work with other types of grammars.
%% For example, regular grammars just allow grammar rules of the form A->T
%% or A->T*C, i.e. the first symbol on the RHS must be a terminal symbol. It
%% suffices to replace the first p by e in the nonterminal rule.

duplicate @ p(A,I,J) \ p(A,I,J) <=> true.
terminal @ A→T, e(T,I,J) ==> p(A,I,J).
nonterminal @ A→B*C, e(B,I,J), p(C,J,K) ==> p(A,I,K).


