%% 10.4 Generalizing union-find
%% 10.4.2 Boolean equations

:- use_module(library(chr)).

:- chr_constraint bind/0, make/1, root/2, p/3, union/3, find/3, link/3.

% Based on optimized union-find according to TPLP programming pearl 2006

% bind binds variables after computations by propagating "1" down the tree
bind \ p(X,A/B,N) <=> number(N) | X is A*N+B.


make      @ make(X) <=> root(X,0).
union     @ union(X,XY,Y) <=> find(X,XA,A), find(Y,YB,B), combine(XA,YB,XY,AB), link(A,AB,B).

findNode  @ p(X,XY,Y), find(X,XR,R) <=> find(Y,YR,R), compose(XY,YR,XR), p(X,XR,R).
findRoot  @ root(X,_) \ find(X,XR,R) <=> equal(XR), X=R.

% GENERIC and for BOOL, but not for NUMBERS
linkEq @ link(X,R,X) <=> equal(R). 

linkLeft  @ link(X,XY,Y), root(X,RX), root(Y,RY) <=>
               RX >= RY |
               invert(XY,YX),
               p(Y,YX,X),
               NRX is max(RX,RY+1), root(X,NRX).
linkRight @ link(X,XY,Y), root(Y,RY), root(X,RX) <=>
               RY >= RX |
               p(X,XY,Y),
               NRY is max(RY,RX+1), root(Y,NRY).


% function tables -------------------------------------------------

% COULD ALSO BE RUN WITH compose and invert as constraints!

% BOOL ufe ---------------------
% instances for Boolean eq and ne

% compose(AX,XY,AY)
compose(eq,R,R):-!.
compose(R,eq,R):-!.
compose(ne,ne,eq).

% combine(XA,YB,XY,AB)
combine(XA,YB,XY,AB):-
  compose(XY,YB,XB),
  invert(XA,AX),
  compose(AX,XB,AB),
  !.

% invert(XY,YX)
invert(X,X). % invert(X,Y):-compose(X,Y,Z),equal(Z)

% equal(XY)
equal(eq).  % equal(X):-compose(X,R,R)


% for regression tests only
union(X,Y):- union(X,eq,Y).
find(X,Y):- find(X,XY,Y).

%----------------------------------------

% feature logic: several unrelated functions 
% functor, arity ok, but arguments not bijective!


% finite paired domains

% path consistency with ufe: keep original relations, i.e. unions
% without optimizations still quadratic instead of cubic?!?


%--------------------------------------------------------------------------

%===========================================================================================


% BOOL ufe ---------------------------------------------------------------------
% special ufe Boolean eq/ne tests
 
%?- make(A),make(B), union(A,eq,B),union(A,eq,B).
%@ root($VAR(A),1),
%@ p($VAR(B),eq,$VAR(A)).

%?- make(A),make(B), union(A,eq,B),union(A,ne,B).
%@ false.

%?- make(A),make(B), union(A,ne,B),union(A,eq,B).
%@ false.

%?- make(A),make(B), union(A,ne,B),union(A,ne,B).
%@ root($VAR(A),1),
%@ p($VAR(B),ne,$VAR(A)).

%?- make(A),make(B), union(A,eq,B),union(B,eq,A).  
%@ root($VAR(A),1),
%@ p($VAR(B),eq,$VAR(A)).

%?- make(A),make(B), union(A,eq,B),union(B,ne,A).
%@ false.

%?- make(A),make(B), union(A,ne,B),union(B,eq,A).
%@ false.

%?- make(A),make(B), union(A,ne,B),union(B,ne,A).
%@ root($VAR(A),1),
%@ p($VAR(B),ne,$VAR(A)).


%?- make(X),make(Y),make(Z),(R=eq;R=ne),union(X,R,Y),union(Y,R,Z),union(X,R,Z).
%@ R = eq,
%@ root($VAR(X),1),
%@ p($VAR(Z),eq,$VAR(X)),
%@ p($VAR(Y),eq,$VAR(X)) ;
%@ false.

%?- make(X),make(Y),make(Z),(R=eq;R=ne),union(X,R,Y),union(Y,R,Z),union(X,R1,Z).
%@ R = R1, R1 = eq,
%@ root($VAR(X),1),
%@ p($VAR(Z),eq,$VAR(X)),
%@ p($VAR(Y),eq,$VAR(X)) ;
%@ R = ne,
%@ R1 = eq,
%@ root($VAR(X),1),
%@ p($VAR(Z),eq,$VAR(X)),
%@ p($VAR(Y),ne,$VAR(X)).

%?- make(X),make(Y),make(Z),(R=eq;R=ne),union(X,R,Y),union(Y,R,Z),union(X,ne,Z).
%@ false.

%?- make(X),make(Y),make(Z),(R=eq;R=ne),union(X,R,Y),union(Y,R,Z),union(X,eq,Z).
%@ R = eq,
%@ root($VAR(X),1),
%@ p($VAR(Z),eq,$VAR(X)),
%@ p($VAR(Y),eq,$VAR(X)) ;
%@ R = ne,
%@ root($VAR(X),1),
%@ p($VAR(Z),eq,$VAR(X)),
%@ p($VAR(Y),ne,$VAR(X)).

%% 1 and 0 are easy!

%?- make(0),make(1),union(0,ne,1).
%@ root(0,1),
%@ p(1,ne,0).

%?- make(0),make(1),union(0,ne,1), make(A),make(B),union(A,eq,B),union(A,ne,0).
%@ root($VAR(A),2),
%@ p(0,ne,$VAR(A)),
%@ p($VAR(B),eq,$VAR(A)),
%@ p(1,ne,0).

%?- make(0),make(1),union(0,ne,1), make(A),make(B),union(A,eq,B),union(A,ne,0), union(A,ne,1).
%@ false.

%% ?- make(0),make(1),union(0,ne,1), make(A),make(B),union(A,eq,B),union(A,ne,0), union(B,ne,1).
%@ false.

%?- make(0),make(1),union(0,ne,1), make(A),make(B),union(A,eq,B),union(A,ne,0), union(B,eq,1).
%@ root($VAR(A),2),
%@ p(1,eq,$VAR(A)),
%@ p(0,ne,$VAR(A)),
%@ p($VAR(B),eq,$VAR(A)).

%?- make(0),make(1),union(0,ne,1), make(A),make(B),union(A,eq,B),union(A,ne,0), union(A,eq,1).
%@ root($VAR(A),2),
%@ p(1,eq,$VAR(A)),
%@ p(0,ne,$VAR(A)),
%@ p($VAR(B),eq,$VAR(A)).

% and(A,B,C),or(A,B,C)
%% (union(A,eq,B),union(B,eq,C) ; union(A,ne,B),union(0,eq,C)) %and
%% (union(A,eq,B),union(B,eq,C) ; union(A,ne,B),union(1,eq,C)) %or

%?- make(0),make(1),union(0,ne,1), make(A),make(B),make(C),
%%  (union(A,eq,B),union(B,eq,C) ; union(A,ne,B),union(0,eq,C)),
%%  (union(A,eq,B),union(B,eq,C) ; union(A,ne,B),union(1,eq,C)).
%@ root($VAR(A),1),
%@ root(0,1),
%@ p($VAR(C),eq,$VAR(A)),
%@ p($VAR(B),eq,$VAR(A)),
%@ p(1,ne,0) ;
%@ false.



