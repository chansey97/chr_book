%% 9.1.3 Fourier’s algorithm

:- use_module(library(chr)).
user:library_directory('../../').
:- use_module(library(helpers)).

:- op(500,xfy,+).
:- op(650,xfx,geq).
:- op(650,xfx,eq).

:- chr_constraint geq/2, eq/2.

propagate @ A1*X+P1 geq 0, A2*X+P2 geq 0 ==>
  S is A2/A1, S<0 |
  normalize(A2*(-P1/A1)+P2,P3),
  P3 geq 0.

redundant @ A1*X+P1 geq 0 \ A2*X+P2 geq 0 <=>
  S is A2/A1, S>0, normalize(A2*(-P1/A1)+P2,P3), number(P3), P3>=0 |
  true.

simplify @ A1*X+P1 geq 0 , A2*X+P2 geq 0 <=>
  S is A2/A1, S<0, normalize(A2*(-P1/A1)+P2,P3), zero(P3) |
  A1*X+P1 eq 0.

eliminate_geq @ A1*X+P1 eq 0 \ A2*X+P2 geq 0 <=>
  normalize(A2*(-P1/A1)+P2,P3),
  P3 geq 0.

zero(X) :- abs(X) =< 0.000001.

%% https://www.youtube.com/watch?v=3oywu1piPug
%% ?- 1*X+1*Y geq 0, 2*X+1*Y+(-2) geq 0, -1*X+1*Y+(-1) geq 0, -1*X+2*Y+1 geq 0.
%@ -1* $VAR(X)+2* $VAR(Y)+1 geq 0,
%@ 3* $VAR(Y)+ -4 geq 0,
%@ -1* $VAR(X)+1* $VAR(Y)+ -1 geq 0,
%@ 2* $VAR(X)+1* $VAR(Y)+ -2 geq 0,
%@ 1* $VAR(X)+1* $VAR(Y)geq 0.

%% https://www.youtube.com/watch?v=tIZCHvv_K1w
%% ?- 1*X+1*Y+(-1)*Z+(-2) geq 0, 2*X+(-1)*Y-3 geq 0, -1*X+2*Y+1*Z geq 0.
%@ 3* $VAR(Y)+ -2 geq 0,
%@ -1* $VAR(X)+2* $VAR(Y)+1* $VAR(Z)geq 0,
%@ 2* $VAR(X)+ -1* $VAR(Y)-3 geq 0,
%@ 1* $VAR(X)+1* $VAR(Y)+ -1* $VAR(Z)+ -2 geq 0.

%% TODO: back substitution variables