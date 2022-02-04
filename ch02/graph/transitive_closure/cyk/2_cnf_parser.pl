%% 2.4.3 Grammar parsing

%% Input a CNF grammar (→)/2 and a string as graph chain of terminal symbols e/3.
%% Output paths of the restricted transitive closure satisfied the grammar.

%% Parse tree.

%% Since no subparses are ever removed, the parse tree information
%% from the subparses can be looked up in the subparses and there is
%% no need to store them in the new parse. (This corresponds to the
%% backpointer approach in imperative languages.) We just have to
%% store in an additional argument of p the arguments from the rule
%% head that do not occur in the rule body.  

:- use_module(library(chr)).

:- op(600, xfx, →).

:- chr_constraint
  
  % an edge of a graph chain
  e/3,
  
  % a path of transitive closure
  p/4,
  
  % production  
  (→)/2.

duplicate @ p(A,I,J,P) \ p(A,I,J,P) <=> true.
terminal @ A→T, e(T,I,J) ==> p(A,I,J,t(T)).
nonterminal @ A→B*C, p(B,I,J,_P1), p(C,J,K,_P2) ==> p(A,I,K,nt(B*C,J)).

%% Example 1.

%% CNF
%% G → BG
%% G → a
%% B → a

%?- s_G → s_B * s_G, s_G → a, s_B → a, e(a,0,1), e(a,1,2).
%@ e(a,1,2),
%@ e(a,0,1),
%@ p(s_G,0,2,nt(s_B*s_G,1)),
%@ p(s_G,1,2,t(a)),
%@ p(s_B,1,2,t(a)),
%@ p(s_G,0,1,t(a)),
%@ p(s_B,0,1,t(a)),
%@ s_B→a,
%@ s_G→a,
%@ s_G→s_B*s_G.

%?- s_G → s_B * s_G, s_G → a, s_B → a, e(a,0,1), e(b,1,2).
%@ e(b,1,2),
%@ e(a,0,1),
%@ p(s_G,0,1,t(a)),
%@ p(s_B,0,1,t(a)),
%@ s_B→a,
%@ s_G→a,
%@ s_G→s_B*s_G.

%% Example 2.
%% Theory of Computation Video 9 : More Lemmas and CYK Algorithm - YouTube
%% https://www.youtube.com/watch?v=Sg6rp4soFAE

%% ?- s_A → s_B * s_C,
%%    s_A → s_A * s_B,
%%    s_A → 1,
%%    s_B → s_A * s_A,
%%    s_B → 0,
%%    s_C → s_C * s_B,
%%    s_C → 1,
%%    s_C → 0,
%%    e(1,0,1),e(0,1,2),e(1,2,3),e(0,3,4),e(0,4,5).
%@ e(0,4,5),
%@ e(0,3,4),
%@ e(1,2,3),
%@ e(0,1,2),
%@ e(1,0,1),
%@ p(s_A,0,5,nt(s_A*s_B,4)),
%@ p(s_B,0,5,nt(s_A*s_A,2)),
%@ p(s_A,2,5,nt(s_A*s_B,4)),
%@ p(s_A,1,5,nt(s_A*s_B,4)),
%@ p(s_A,0,5,nt(s_B*s_C,3)),
%@ p(s_C,3,5,nt(s_C*s_B,4)),
%@ p(s_B,0,5,nt(s_A*s_A,1)),
%@ p(s_A,1,5,nt(s_B*s_C,2)),
%@ p(s_C,2,5,nt(s_C*s_B,4)),
%@ p(s_B,4,5,t(0)),
%@ p(s_A,0,5,nt(s_A*s_B,1)),
%@ p(s_C,0,5,nt(s_C*s_B,1)),
%@ p(s_B,1,5,nt(s_A*s_A,3)),
%@ p(s_A,0,5,nt(s_A*s_B,2)),
%@ p(s_C,1,5,nt(s_C*s_B,2)),
%@ p(s_C,0,5,nt(s_C*s_B,2)),
%@ p(s_B,2,5,nt(s_A*s_A,3)),
%@ p(s_A,3,5,nt(s_B*s_C,4)),
%@ p(s_A,0,5,nt(s_B*s_C,4)),
%@ p(s_C,4,5,t(0)),
%@ p(s_A,1,4,nt(s_A*s_B,3)),
%@ p(s_B,0,4,nt(s_A*s_A,2)),
%@ p(s_A,2,4,nt(s_A*s_B,3)),
%@ p(s_B,0,4,nt(s_A*s_A,1)),
%@ p(s_A,1,4,nt(s_B*s_C,2)),
%@ p(s_C,2,4,nt(s_C*s_B,3)),
%@ p(s_B,3,4,t(0)),
%@ p(s_A,0,4,nt(s_B*s_C,3)),
%@ p(s_C,3,4,t(0)),
%@ p(s_B,0,3,nt(s_A*s_A,2)),
%@ p(s_A,2,3,t(1)),
%@ p(s_B,0,3,nt(s_A*s_A,1)),
%@ p(s_A,1,3,nt(s_B*s_C,2)),
%@ p(s_C,2,3,t(1)),
%@ p(s_A,0,2,nt(s_A*s_B,1)),
%@ p(s_C,0,2,nt(s_C*s_B,1)),
%@ p(s_B,1,2,t(0)),
%@ p(s_C,1,2,t(0)),
%@ p(s_A,0,1,t(1)),
%@ p(s_C,0,1,t(1)),
%@ s_C→0,
%@ s_C→1,
%@ s_C→s_C*s_B,
%@ s_B→0,
%@ s_B→s_A*s_A,
%@ s_A→1,
%@ s_A→s_A*s_B,
%@ s_A→s_B*s_C.


