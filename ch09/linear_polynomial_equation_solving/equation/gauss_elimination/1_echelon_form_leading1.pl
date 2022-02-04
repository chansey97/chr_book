%% 9.1 Linear polynomial equation solving

%% Solves linear polynomial equations by Gaussian variable elimination.
%% N.B. This approach is different from the book which using variable elimination.

%% Get solved form, here using row echelon form
%% Some texts add the condition that the leading coefficient must be 1.

:- use_module(library(chr)).
:- use_module(library(lists)).
user:library_directory('../../../../common/').
:- use_module(library(ordering)).

:- op(600,xfx,eq).

:- chr_constraint eq/2.

%% Poly eq Const, where Poly is list of monomials Coefficient*Variable
%%    eq/2

%% 0=C
zero @      [] eq C <=> zero(C).

%% Gaussian elimination 
elimate @   [A1*X|P1] eq C \ [A2*X|P2] eq D <=>
    multpoly(-A2/A1, P1 eq C, E),
    addpoly(E, P2 eq D, Q0 eq D0), 
    Q0 eq D0.

%% normalize first coefficient to 1
norm1 @     [A*X|P] eq C <=> notzero(A-1) | 
    multpoly(1/A,P eq C,P2 eq C2), [1*X|P2] eq C2.

%% Auxiliary predicates

%% stable variable order based on chr/ordering by C. Holzbaur (var_order.pl)
%% compare variables with var_compare
ord_lss(X,Y) :- var_compare(<,X,Y).
%% otherwise use Prolog standard order
ord_lss(X,Y) :- nonvar(Y), X @< Y.

%% numerical stabilty
notzero(A) :- abs(A) >  0.000001.
zero(A)    :- abs(A) =< 0.000001.

%% safe_is(X,Exp): X is Exp; fails, if Exp is not an arith. expression
safe_is(X,Exp) :- on_exception(_, X is Exp, fail).

%% addpoly(E,F, E0): add polynomial equations
%% requires: polynomials ordered + duplicate free; retains properties
addpoly(P eq C, [] eq D, P eq C0) :- !, C0 is C+D.
addpoly([] eq C, Q eq D, Q eq C0) :- !, C0 is C+D.
addpoly([A*X|P] eq C, [B*Y|Q] eq D, E0) :- X==Y, !,
  A1 is A+B, addpoly(P eq C, Q eq D, P1 eq C1),
  (   zero(A1)
  ->  E0 = P1 eq C1
  ;   E0 = [A1*X|P1] eq C1).
addpoly([A*X|P] eq C, [B*Y|Q] eq D, [A*X|P0] eq C0) :- ord_lss(X,Y), !,
  addpoly(P eq C, [B*Y|Q] eq D, P0 eq C0).
addpoly([A*X|P] eq C, [B*Y|Q] eq D, [B*Y|P0] eq C0) :- ord_lss(Y,X), !,
  addpoly([A*X|P] eq C, Q eq D, P0 eq C0).

%% multpoly(M,E, E0): multiply polynomial equation with scalar
multpoly(M, [] eq C, [] eq C0) :- C0 is M*C.
multpoly(M, [A*X|P] eq C, E0) :- 
  A1 is M*A, multpoly(M,P eq C,P1 eq C1),
  (   zero(A1)
  ->  E0 = P1 eq C1
  ;   E0 = [A1*X|P1] eq C1).

%% Examples

%% 1*X+3*Y eq -5, 3*X+2*Y eq -8
%% N.B. variables are ordered by globalize (not lexicographical order of variables' name).

%% ?- globalize([X,Y]), [1*X,3*Y] eq -5, [3*X,2*Y] eq -8.
%@ [1* $VAR(Y)]eq-1.0,
%@ [1* $VAR(X),3* $VAR(Y)]eq-5 ;
%@ false.

%% https://youtu.be/2tlwSqblrvU
%% row echelon form is not unique.

%% ?- globalize([X,Y,Z]), [2*X,3*Y,4*Z] eq 6, [1*X,2*Y,3*Z] eq 4, [3*X,-4*Y] eq 10.
%@ [1* $VAR(Z)]eq 1.6363636363636365,
%@ [1* $VAR(Y),2.0* $VAR(Z)]eq 2.0,
%@ [1* $VAR(X),1.5* $VAR(Y),2.0* $VAR(Z)]eq 3.0 ;
%@ false.

%% https://youtu.be/bWvb0eoX064
%% No solution

%% ?- globalize([A,B,C]), [1*A,1*B,-1*C] eq 2, [2*A,3*B,-1*C] eq 0, [3*A,4*B,-2*C] eq 1.
%@ false.

%% https://youtu.be/RNUxh-hlLkU
%% infinite number of solutions

%% ?- globalize([X,Y,Z]), [1*X,1*Y,1*Z] eq 3, [2*X,4*Y,1*Z] eq 8, [6*X,10*Y,4*Z] eq 22.
%@ [1* $VAR(Y),-0.5* $VAR(Z)]eq 1.0,
%@ [1* $VAR(X),1* $VAR(Y),1* $VAR(Z)]eq 3 ;
%@ false.
