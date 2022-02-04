%% 10.4 Generalizing union-find
%% 10.4.3 Linear polynomials

:- use_module(library(chr)).

:- chr_constraint bind/0, make/1, root/2, p/3, union/3, find/3, link/3.

% Based on optimized union-find according to TPLP programming pearl 2006

% bind binds variables after computations by propagating "1" down the tree
bind \ p(X,A/B,N) <=> number(N) | X is A*N+B.


make      @ make(X) <=> root(X,0).
union     @ union(X,XY,Y) <=> find(X,XA,A), find(Y,YB,B), combine(XA,YB,XY,AB), link(A,AB,B).

findNode  @ p(X,XY,Y), find(X,XR,R) <=> find(Y,YR,R), compose(XY,YR,XR), p(X,XR,R).
findRoot  @ root(X,_) \ find(X,XR,R) <=> equal(XR), X=R.

% for NUMBERS only 2005-11-25
% "1" must stay root, so do not use make(1), but root(1,+inf). (+inf can be log n)
% otherwise must use find before link(X,1/D,1)
linkEqNum1   @ link(X,A/B,X) <=> A=:=1 | B=:=0.
linkEqNum2   @ link(X,A/B,X) <=> A=\=1 | D is B/(1-A)-1, link(X,1/D,1). % X may be 1
%% linkEqNum2Int@ link(X,A/B,X) <=> A=\=1 | D is B/(1-A)-1, is_of_type(integer,D), link(X,1/D,1). % X may be 1

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

% NUMBERS -------------------------------------
% polynomial equations X=Y^a*b+c mod k, b=\=0, a=\=0?, k fixed (24 hours, 360 degrees)?
% here linear, without exponent: X A/B Y means X=A*Y+B, hence (X-B)/A=Y

% compose(AX,XY,AY)
% X=A*Y+B, Y=C*Z+D, hence X=A*(C*Z+D)+B=A*C*Z+A*D+B
compose(A/B,C/D,E/F):- E is A*C, F is A*D+B.

% combine(XA,YB,XY,AB)
combine(XA,YB,XY,AB):-
  compose(XY,YB,XB),
  invert(XA,AX),
  compose(AX,XB,AB),
  !.

% invert(XY,YX)
invert(A/B,C/D):- C is 1/A, D is -B/A.  %A=\=0

% equal(XY)
equal(1/0):-!.
equal(A/B):- A=:=1,B=:=0. 


% for regression tests only
union(X,Y):- union(X,1/0,Y).
find(X,Y):- find(X,XY,Y).

%----------------------------------------

% feature logic: several unrelated functions 
% functor, arity ok, but arguments not bijective!


% finite paired domains

% path consistency with ufe: keep original relations, i.e. unions
% without optimizations still quadratic instead of cubic?!?


%--------------------------------------------------------------------------

%===========================================================================================


% NUMBERS ufe --------------------------------------------------------------

% adapted special ufe Boolean eq/ne tests, eq=1/0, ne=-1/1

%% N.B.
%% Must use linkEqNum2Int instead of linkEqNum2, otherwise 

%?- make(A),make(B), union(A,1/0,B),union(A,1/0,B).  
%@ root($VAR(A),1),
%@ p($VAR(B),1/0,$VAR(A)).

%?- make(A),make(B), union(A,1/0,B),union(A,-1/1,B).
%@ false.

%?- make(A),make(B), union(A,-1/1,B),union(A,1/0,B).
%@ false.

%?- make(A),make(B), union(A,-1/1,B),union(A,-1/1,B).
%@ root($VAR(A),1),
%@ p($VAR(B),-1/1,$VAR(A)).

%?- make(A),make(B), union(A,1/0,B),union(B,1/0,A).  
%@ root($VAR(A),1),
%@ p($VAR(B),1/0,$VAR(A)).

%?- make(A),make(B), union(A,1/0,B),union(B,-1/1,A).
%@ false.

%?- make(A),make(B), union(A,-1/1,B),union(B,1/0,A).
%@ false.

%?- make(A),make(B), union(A,-1/1,B),union(B,-1/1,A).
%@ root($VAR(A),1),
%@ p($VAR(B),-1/1,$VAR(A)).

%?- make(X),make(Y),make(Z),(R=1/0;R=(-1)/1),union(X,R,Y),union(Y,R,Z),union(X,R,Z).
%@ R = 1/0,
%@ root($VAR(X),1),
%@ p($VAR(Z),1/0,$VAR(X)),
%@ p($VAR(Y),1/0,$VAR(X)) ;
%@ false.

%?- make(X),make(Y),make(Z),(R=1/0;R=(-1)/1),union(X,R,Y),union(Y,R,Z),R1 = 1/0,union(X,R1,Z).
%@ R = R1, R1 = 1/0,
%@ root($VAR(X),1),
%@ p($VAR(Z),1/0,$VAR(X)),
%@ p($VAR(Y),1/0,$VAR(X)) ;
%@ R = -1/1,
%@ R1 = 1/0,
%@ root($VAR(X),1),
%@ p($VAR(Z),1/0,$VAR(X)),
%@ p($VAR(Y),-1/1,$VAR(X)).;

%?- make(X),make(Y),make(Z),(R=1/0;R=(-1)/1),union(X,R,Y),union(Y,R,Z),union(X,-1/1,Z).
%@ false.

%?- make(X),make(Y),make(Z),(R=1/0;R=(-1)/1),union(X,R,Y),union(Y,R,Z),union(X,1/0,Z).
%@ R = 1/0,
%@ root($VAR(X),1),
%@ p($VAR(Z),1/0,$VAR(X)),
%@ p($VAR(Y),1/0,$VAR(X)) ;
%@ R = -1/1,
%@ root($VAR(X),1),
%@ p($VAR(Z),1/0,$VAR(X)),
%@ p($VAR(Y),-1/1,$VAR(X)).

% 1 and 0 are easy!

%?- make(0),make(1),union(0,-1/1,1).
%@ root(0,1),
%@ p(1,-1/1,0).


%?- make(0),make(1),union(0,-1/1,1), make(A),make(B),union(A,1/0,B),union(A,-1/1,0).
%@ root($VAR(A),2),
%@ p(0,-1/1,$VAR(A)),
%@ p($VAR(B),1/0,$VAR(A)),
%@ p(1,-1/1,0).

%?- make(0),make(1),union(0,-1/1,1), make(A),make(B),union(A,1/0,B),union(A,-1/1,0), union(A,-1/1,1).
%@ false.

%?- make(0),make(1),union(0,-1/1,1), make(A),make(B),union(A,1/0,B),union(A,-1/1,0), union(B,-1/1,1).
%@ false.

%?- make(0),make(1),union(0,-1/1,1), make(A),make(B),union(A,1/0,B),union(A,-1/1,0), union(B,1/0,1).
%@ root($VAR(A),2),
%@ p(1,1/0,$VAR(A)),
%@ p(0,-1/1,$VAR(A)),
%@ p($VAR(B),1/0,$VAR(A)).

%?- make(0),make(1),union(0,-1/1,1), make(A),make(B),union(A,1/0,B),union(A,-1/1,0), union(A,1/0,1).
%@ root($VAR(A),2),
%@ p(1,1/0,$VAR(A)),
%@ p(0,-1/1,$VAR(A)),
%@ p($VAR(B),1/0,$VAR(A)).

%% and(A,B,C),or(A,B,C)
%% (union(A,1/0,B),union(B,1/0,C);union(A,-1/1,B),union(0,1/0,C)), %and
%% (union(A,1/0,B),union(B,1/0,C);union(A,-1/1,B),union(1,1/0,C)). %or
  
%?- make(0),make(1),union(0,-1/1,1), make(A),make(B),make(C),
%%  (union(A,1/0,B),union(B,1/0,C);union(A,-1/1,B),union(0,1/0,C)),
%%  (union(A,1/0,B),union(B,1/0,C);union(A,-1/1,B),union(1,1/0,C)).
%@ root($VAR(A),1),
%@ root(0,1),
%@ p($VAR(C),1/0,$VAR(A)),
%@ p($VAR(B),1/0,$VAR(A)),
%@ p(1,-1/1,0) ;
%@ false.

% specific NUMBER examples

%% N.B.
%% Must use linkEqNum2 instead of linkEqNum2Int, otherwise

%?- make(X),make(Y),make(Z),union(X,1/2,Y),union(Y,1/3,Z).
%@ root($VAR(X),1),
%@ p($VAR(Z),1/ -5,$VAR(X)),
%@ p($VAR(Y),1/ -2,$VAR(X)).

%?- make(X),make(Y),make(Z),union(X,1/2,Y),union(Y,1/3,Z),union(X,1/5,Z).
%@ root($VAR(X),1),
%@ p($VAR(Z),1/ -5,$VAR(X)),
%@ p($VAR(Y),1/ -2,$VAR(X)).

%?- make(X),make(Y),make(Z),union(X,1/2,Y),union(Y,1/3,Z),union(X,1/6,Z).
%@ false.

%?- make(X),make(Y),make(Z),make(W),union(X,2/3,Y),union(Y,0.5/2,Z),union(X,1/6,W).
%@ root($VAR(X),1),
%@ p($VAR(W),1/ -6,$VAR(X)),
%@ p($VAR(Z),1.0/ -7.0,$VAR(X)),
%@ p($VAR(Y),0.5/ -1.5,$VAR(X)).

%?- make(X),make(Y),make(Z),make(W),union(Y,0.5/2,Z),union(X,1/6,W),union(X,2/3,Y).
%@ root($VAR(X),2),
%@ p($VAR(Y),0.5/ -1.5,$VAR(X)),
%@ p($VAR(W),1/ -6,$VAR(X)),
%@ p($VAR(Z),2.0/ -4.0,$VAR(Y)).

%?- make(X),make(Y),make(Z),make(W),union(Y,0.5/2,Z),union(X,1/6,W),union(X,2/3,Y),find(Z,D,_).
%@ D = 1.0/ -7.0,
%@ root($VAR(X),2),
%@ p($VAR(Z),1.0/ -7.0,$VAR(X)),
%@ p($VAR(Y),0.5/ -1.5,$VAR(X)),
%@ p($VAR(W),1/ -6,$VAR(X)).

% with numbers, without special linking

%?- make(X),make(Y),make(Z),make(W),X=5,Y=1,union(X,2/3,Y).
%@ X = 5,
%@ Y = 1,
%@ root(5,1),
%@ root($VAR(W),0),
%@ root($VAR(Z),0),
%@ p(1,0.5/ -1.5,5).

%?- make(X),make(Y),make(Z),make(1),make(5),union(X,2/3,Y),union(X,1/0,5),union(Y,1/0,1).
%@ root($VAR(X),1),
%@ root($VAR(Z),0),
%@ p(1,0.5/ -1.5,$VAR(X)),
%@ p($VAR(Y),0.5/ -1.5,$VAR(X)),
%@ p(5,1/0,$VAR(X)).

%?- make(X),make(Y),make(Z),make(1),make(6),union(X,2/3,Y),union(X,1/0,6),union(Y,1/0,1).
%@ root($VAR(X),1),
%@ root($VAR(Z),0),
%@ p(1,0.5/ -1.5,$VAR(X)),
%@ p($VAR(Y),0.5/ -1.5,$VAR(X)),
%@ p(6,1/0,$VAR(X)).

%?- make(X),make(Y),make(Z),make(1),make(6),union(X,2/3,Y),union(X,1/0,6),union(Y,1/0,1), bind.
%@ bind,
%@ root($VAR(X),1),
%@ root($VAR(Z),0),
%@ p(1,0.5/ -1.5,$VAR(X)),
%@ p($VAR(Y),0.5/ -1.5,$VAR(X)),
%@ p(6,1/0,$VAR(X)).

%% root(Z,0),
%% root(X,1),
%% p(1,1.0/-0.0,X),
%% p(Y,0.5/ -1.5,X),
%% p(6,0.5/ -1.5,X) ? ;

%% FIXME: might be some problem here...

% with numbers, with special linking linkNum

%?- make(X),make(Y),make(Z),make(W),X=5,Y=1,union(X,2/3,Y).
%@ X = 5,
%@ Y = 1,
%@ root(5,1),
%@ root($VAR(W),0),
%@ root($VAR(Z),0),
%@ p(1,0.5/ -1.5,5).

%% X = 5,
%% Y = 1,
%% root(Z,0),
%% root(W,0),
%% root(1,1) ? ;
%% no

%?- make(X),make(Y),make(Z),make(1),make(5),union(X,2/3,Y),union(X,1/0,5),union(Y,1/0,1).
%@ root($VAR(X),1),
%@ root($VAR(Z),0),
%@ p(1,0.5/ -1.5,$VAR(X)),
%@ p($VAR(Y),0.5/ -1.5,$VAR(X)),
%@ p(5,1/0,$VAR(X)).

%% root(Z,0),
%% root(1,3),
%% p(X,1.0/0.0,5),
%% p(Y,0.5/ -1.5,5) ? ;
%% no

%?- make(X),make(Y),make(Z),make(1),make(6),union(X,2/3,Y),union(X,1/0,6),union(Y,1/0,1).
%@ root($VAR(X),1),
%@ root($VAR(Z),0),
%@ p(1,0.5/ -1.5,$VAR(X)),
%@ p($VAR(Y),0.5/ -1.5,$VAR(X)),
%@ p(6,1/0,$VAR(X)).

%% no

% with special linkEQ Numrule, without findNum, pointerNum, linkNum rules, 2005-11-24

%?- make(X),make(Y),make(Z),make(1),make(6),union(X,2/3,Y),union(X,1/0,Y).
%@ root($VAR(X),1),
%@ root(6,0),
%@ root($VAR(Z),0),
%@ p(1,1/4.0,$VAR(X)),
%@ p($VAR(Y),0.5/ -1.5,$VAR(X)).



%% X = -3.0,
%% root(Z,0),
%% root(1,0),
%% root(6,0),
%% root(-3.0,1),
%% p(Y,0.5/ -1.5,-3.0) ? ;
%% no
  
%?- make(X),make(Y),make(Z),make(1),make(6),union(X,2/3,Y),union(X,1/0,Z),union(Z,1/0,Y).
%% X = -3.0,
%% root(1,0),
%% root(6,0),
%% root(-3.0,1),
%% p(Z,1.0/0.0,-3.0),
%% p(Y,0.5/ -1.5,-3.0) ? ;
%% no
  
%?- make(X),make(Y),make(Z),make(1),make(6),union(X,2/3,Y),union(X,1/0,Z),union(Z,2/1,Z).
%% X = -1.0,
%% root(1,0),
%% root(6,0),
%% root(-1.0,1),
%% p(Y,0.5/ -1.5,-1.0),
%% p(Z,1.0/0.0,-1.0) ?
  
%?- make(X),make(Y),make(Z),make(1),make(6),union(X,2/3,Y),union(X,1/0,Z),union(Z,1/0,Y),union(Y,1/0,Z).
%@ root($VAR(X),1),
%@ root(6,0),
%@ p($VAR(Z),1/0,$VAR(X)),
%@ p($VAR(Y),0.5/ -1.5,$VAR(X)),
%@ p(1,1/4.0,$VAR(X)),
%@ link($VAR(X),1/ -4.0,1).


%% X = -3.0,
%% root(1,0),
%% root(6,0),
%% root(-3.0,1),
%% p(Y,0.5/ -1.5,-3.0),
%% p(Z,1.0/0.0,-3.0) ?
%% yes
  
%?- make(X),make(Y),make(Z),make(1),make(6),union(X,2/3,Y),union(X,1/0,Z),union(Z,1/0,Y),union(X,1/0,Z).
%@ root($VAR(X),1),
%@ root(6,0),
%@ p($VAR(Z),1/0,$VAR(X)),
%@ p(1,1/4.0,$VAR(X)),
%@ p($VAR(Y),0.5/ -1.5,$VAR(X)).



%% X = -3.0,
%% root(1,0),
%% root(6,0),
%% root(-3.0,1),
%% p(Y,0.5/ -1.5,-3.0),
%% p(Z,1.0/0.0,-3.0) ? ;
%% no

% with special linkEQNum rule, with findNum, pointerNum, without linkNum rules
% Still incomplete.
% simple solution? all numbers are the same, just one number "1" or "0"



