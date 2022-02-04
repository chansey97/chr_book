%% 2.4.3 Grammar parsing
%% Input a CNF grammar (→)/2 and a string as graph chain of terminal symbols e/3.
%% Output paths of the restricted transitive closure satisfied the grammar.

%% Parse tree.

%% Store subtrees in p. 

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
terminal @ A→T, e(T,I,J) ==> p(A,I,J,t(A,T)).
nonterminal @ A→B*C, p(B,I,J,P1), p(C,J,K,P2) ==> p(A,I,K,nt(A,P1*P2)).


%% Example 1.

%% CNF
%% G → BG
%% G → a
%% B → a

%?- s_G → s_B * s_G, s_G → a, s_B → a, e(a,0,1), e(a,1,2).
%@ e(a,1,2),
%@ e(a,0,1),
%@ p(s_G,0,2,nt(s_G,t(s_B,a)*t(s_G,a))),
%@ p(s_G,1,2,t(s_G,a)),
%@ p(s_B,1,2,t(s_B,a)),
%@ p(s_G,0,1,t(s_G,a)),
%@ p(s_B,0,1,t(s_B,a)),
%@ s_B→a,
%@ s_G→a,
%@ s_G→s_B*s_G.

%?- s_G → s_B * s_G, s_G → a, s_B → a, e(a,0,1), e(b,1,2).
%@ e(b,1,2),
%@ e(a,0,1),
%@ p(s_G,0,1,t(s_G,a)),
%@ p(s_B,0,1,t(s_B,a)),
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
%@ p(s_A,0,5,nt(s_A,nt(s_A,nt(s_B,nt(s_A,t(s_A,1)*t(s_B,0))*t(s_A,1))*t(s_C,0))*t(s_B,0))),
%@ p(s_A,0,5,nt(s_A,nt(s_A,nt(s_B,t(s_A,1)*nt(s_A,t(s_B,0)*t(s_C,1)))*t(s_C,0))*t(s_B,0))),
%@ p(s_B,0,5,nt(s_B,t(s_A,1)*nt(s_A,nt(s_A,t(s_B,0)*nt(s_C,t(s_C,1)*t(s_B,0)))*t(s_B,0)))),
%@ p(s_A,1,5,nt(s_A,nt(s_A,t(s_B,0)*nt(s_C,t(s_C,1)*t(s_B,0)))*t(s_B,0))),
%@ p(s_B,0,5,nt(s_B,nt(s_A,t(s_A,1)*t(s_B,0))*nt(s_A,nt(s_A,t(s_A,1)*t(s_B,0))*t(s_B,0)))),
%@ p(s_A,2,5,nt(s_A,nt(s_A,t(s_A,1)*t(s_B,0))*t(s_B,0))),
%@ p(s_B,0,5,nt(s_B,t(s_A,1)*nt(s_A,nt(s_A,nt(s_A,t(s_B,0)*t(s_C,1))*t(s_B,0))*t(s_B,0)))),
%@ p(s_A,1,5,nt(s_A,nt(s_A,nt(s_A,t(s_B,0)*t(s_C,1))*t(s_B,0))*t(s_B,0))),
%@ p(s_A,0,5,nt(s_A,nt(s_B,t(s_A,1)*nt(s_A,t(s_B,0)*t(s_C,1)))*nt(s_C,t(s_C,0)*t(s_B,0)))),
%@ p(s_A,0,5,nt(s_A,nt(s_B,nt(s_A,t(s_A,1)*t(s_B,0))*t(s_A,1))*nt(s_C,t(s_C,0)*t(s_B,0)))),
%@ p(s_C,3,5,nt(s_C,t(s_C,0)*t(s_B,0))),
%@ p(s_B,0,5,nt(s_B,t(s_A,1)*nt(s_A,t(s_B,0)*nt(s_C,nt(s_C,t(s_C,1)*t(s_B,0))*t(s_B,0))))),
%@ p(s_A,1,5,nt(s_A,t(s_B,0)*nt(s_C,nt(s_C,t(s_C,1)*t(s_B,0))*t(s_B,0)))),
%@ p(s_C,2,5,nt(s_C,nt(s_C,t(s_C,1)*t(s_B,0))*t(s_B,0))),
%@ p(s_B,4,5,t(s_B,0)),
%@ p(s_A,0,5,nt(s_A,t(s_A,1)*nt(s_B,nt(s_A,t(s_B,0)*t(s_C,1))*nt(s_A,t(s_B,0)*t(s_C,0))))),
%@ p(s_C,0,5,nt(s_C,t(s_C,1)*nt(s_B,nt(s_A,t(s_B,0)*t(s_C,1))*nt(s_A,t(s_B,0)*t(s_C,0))))),
%@ p(s_B,1,5,nt(s_B,nt(s_A,t(s_B,0)*t(s_C,1))*nt(s_A,t(s_B,0)*t(s_C,0)))),
%@ p(s_A,0,5,nt(s_A,nt(s_A,t(s_A,1)*t(s_B,0))*nt(s_B,t(s_A,1)*nt(s_A,t(s_B,0)*t(s_C,0))))),
%@ p(s_C,1,5,nt(s_C,t(s_C,0)*nt(s_B,t(s_A,1)*nt(s_A,t(s_B,0)*t(s_C,0))))),
%@ p(s_C,0,5,nt(s_C,nt(s_C,t(s_C,1)*t(s_B,0))*nt(s_B,t(s_A,1)*nt(s_A,t(s_B,0)*t(s_C,0))))),
%@ p(s_B,2,5,nt(s_B,t(s_A,1)*nt(s_A,t(s_B,0)*t(s_C,0)))),
%@ p(s_A,3,5,nt(s_A,t(s_B,0)*t(s_C,0))),
%@ p(s_A,0,5,nt(s_A,nt(s_B,t(s_A,1)*nt(s_A,t(s_B,0)*nt(s_C,t(s_C,1)*t(s_B,0))))*t(s_C,0))),
%@ p(s_A,0,5,nt(s_A,nt(s_B,nt(s_A,t(s_A,1)*t(s_B,0))*nt(s_A,t(s_A,1)*t(s_B,0)))*t(s_C,0))),
%@ p(s_A,0,5,nt(s_A,nt(s_B,t(s_A,1)*nt(s_A,nt(s_A,t(s_B,0)*t(s_C,1))*t(s_B,0)))*t(s_C,0))),
%@ p(s_C,4,5,t(s_C,0)),
%@ p(s_B,0,4,nt(s_B,t(s_A,1)*nt(s_A,nt(s_A,t(s_B,0)*t(s_C,1))*t(s_B,0)))),
%@ p(s_A,1,4,nt(s_A,nt(s_A,t(s_B,0)*t(s_C,1))*t(s_B,0))),
%@ p(s_B,0,4,nt(s_B,nt(s_A,t(s_A,1)*t(s_B,0))*nt(s_A,t(s_A,1)*t(s_B,0)))),
%@ p(s_A,2,4,nt(s_A,t(s_A,1)*t(s_B,0))),
%@ p(s_B,0,4,nt(s_B,t(s_A,1)*nt(s_A,t(s_B,0)*nt(s_C,t(s_C,1)*t(s_B,0))))),
%@ p(s_A,1,4,nt(s_A,t(s_B,0)*nt(s_C,t(s_C,1)*t(s_B,0)))),
%@ p(s_C,2,4,nt(s_C,t(s_C,1)*t(s_B,0))),
%@ p(s_B,3,4,t(s_B,0)),
%@ p(s_A,0,4,nt(s_A,nt(s_B,t(s_A,1)*nt(s_A,t(s_B,0)*t(s_C,1)))*t(s_C,0))),
%@ p(s_A,0,4,nt(s_A,nt(s_B,nt(s_A,t(s_A,1)*t(s_B,0))*t(s_A,1))*t(s_C,0))),
%@ p(s_C,3,4,t(s_C,0)),
%@ p(s_B,0,3,nt(s_B,nt(s_A,t(s_A,1)*t(s_B,0))*t(s_A,1))),
%@ p(s_A,2,3,t(s_A,1)),
%@ p(s_B,0,3,nt(s_B,t(s_A,1)*nt(s_A,t(s_B,0)*t(s_C,1)))),
%@ p(s_A,1,3,nt(s_A,t(s_B,0)*t(s_C,1))),
%@ p(s_C,2,3,t(s_C,1)),
%@ p(s_A,0,2,nt(s_A,t(s_A,1)*t(s_B,0))),
%@ p(s_C,0,2,nt(s_C,t(s_C,1)*t(s_B,0))),
%@ p(s_B,1,2,t(s_B,0)),
%@ p(s_C,1,2,t(s_C,0)),
%@ p(s_A,0,1,t(s_A,1)),
%@ p(s_C,0,1,t(s_C,1)),
%@ s_C→0,
%@ s_C→1,
%@ s_C→s_C*s_B,
%@ s_B→0,
%@ s_B→s_A*s_A,
%@ s_A→1,
%@ s_A→s_A*s_B,
%@ s_A→s_B*s_C.
