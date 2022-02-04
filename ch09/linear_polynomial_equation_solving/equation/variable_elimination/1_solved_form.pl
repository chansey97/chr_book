%% 9.1 Linear polynomial equation solving
%% 9.1.1 Variable elimination
%% TODO

:- use_module(library(chr)).
:- use_module(library(lists)).
user:library_directory('.').
:- use_module(library(ordering)).

:- op(600,xfx,eq).

:- chr_constraint eq/2.

%% Poly eq Const, where Poly is list of monomials Coefficient*Variable
%%    eq/2

%% N.B. (Doen't work)
%% The approach in 9.1.2 is not good as gauss_elimination.
%% 1. In gauss_elimination, we only need to normalize the input equation once,
%%    in variable_elimination, we need to normalize the equation each time we eliminate a variable.
%% 2. The normpoly predicate can only normalize equations instead of polynomial.
%%    It can be modified to support polynomials efficiently, but no time to do.

%% eliminate @ A1*X+P1 eq 0 \ A2*X+P2 eq 0 <=> normalize(A2*(-P1/A1)+P2,P3), P3 eq 0.

%% constant @ B eq 0 <=> number(B) | zero(B).



%% %% Auxiliary predicates

%% combine_monomials([M],M) :- !.
%% combine_monomials([M|Ms],O) :- O = M+O1, combine_monomials(Ms,O1).

%% normalize(P1,P2) :- normpoly([P1] eq 0, Ms eq C), combine_monomials(Ms, P3), P2 = P3+C.


%% %% stable variable order based on chr/ordering by C. Holzbaur (var_order.pl)
%% %% compare variables with var_compare
%% ord_lss(X,Y) :- var_compare(<,X,Y).
%% %% otherwise use Prolog standard order
%% ord_lss(X,Y) :- nonvar(Y), X @< Y.

%% %% numerical stabilty
%% notzero(A) :- abs(A) >  0.000001.
%% zero(A)    :- abs(A) =< 0.000001.

%% %% safe_is(X,Exp): X is Exp; fails, if Exp is not an arith. expression
%% safe_is(X,Exp) :- on_exception(_, X is Exp, fail).

%% %% normalize polynomial equation
%% normpoly([X] eq C, [1*X] eq C) :- var(X), !.
%% normpoly([X] eq C, [1*X] eq C) :- simple(X), \+ number(X).
%% normpoly([A] eq C, [] eq C0) :- number(A), C0 is C-A.
%% normpoly([A,B|P] eq C, E0) :- 
%%   normpoly([A] eq 0,E1), normpoly([B|P] eq C,E2), addpoly(E1,E2,E0).
%% normpoly([+A] eq 0, E0) :- 
%%   normpoly([A] eq 0, E0).
%% normpoly([-A] eq 0, E0) :- 
%%   normpoly([A] eq 0, E1), multpoly(-1, E1, E0).
%% normpoly([A+B] eq C, E0) :- 
%%   normpoly([A] eq C,E), normpoly([B] eq 0,F), addpoly(E,F,E0).
%% normpoly([A-B] eq C, E0) :- normpoly([A] eq C,E), normpoly([B] eq 0,F),
%%   multpoly(-1,F,F1), addpoly(E,F1,E0).
%% normpoly([A*X] eq 0, E0) :-
%%   safe_is(A1,A), normpoly([X] eq 0, E), multpoly(A1,E,E0).
%% normpoly([X*A] eq 0, E0) :-
%%   safe_is(A1,A), normpoly([X] eq 0, E), multpoly(A1,E,E0).
%% normpoly([X/A] eq 0, E0) :-
%%   safe_is(A1,1/A), normpoly([X] eq 0, E), multpoly(A1,E,E0).
%% normpoly([A] eq C, P0 eq C0) :- 
%%   C\==0, normpoly([A] eq 0, P0 eq C1), C0 is C+C1.
