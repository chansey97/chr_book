%% 9.1 Linear polynomial equation solving

%% Solves linear polynomial equations by Gaussian variable elimination.
%% N.B. This approach is different from the book which using variable elimination.

%% Get solved form, here using row echelon form.
%% Add back substitution (incremental approach).
%% Normalize input.

%% HOW TO USE
%% The handler solves a set of equations enclosed in curly brackets:#
%% *{PL1=PR1, PL2=PR2, .., PLn=PRn}.*  (PLi, PRi are linear polynomial terms)#
%% If no solution exists the handler fails.

%% Variables can either be expressed by Prolog variables (X) or by atoms (x).#
%% If Prolog variables are used, as soon as the handler has determined the value 
%% of a variable it is bound to this value, e.g.: X = 10.#
%% For Prolog variables, the termination of the handler requires a stable 
%% variable order that is implemented by the imported module 'var_order'.#
%% If you don't want to use this module, you can also use atoms for this purpose,
%% a variable binding is then represented as a normalized equation, e.g.: 
%% *[1*x] eq 10.*

%% :- module(equations_gauss, [eq/2, {}/1]).
:- use_module(library(chr)).
:- use_module(library(lists)).
user:library_directory('../../../../common/').
:- use_module(library(ordering)).

:- op(600,xfx,eq).

:- chr_constraint eq/2, {}/1.

%% Poly eq Const, where Poly is list of monomials Coefficient*Variable
%%    eq/2

%% curly brackets as wrapper to avoid name clash with built-in =
%%    {}/1   

%% split system of equations
split @     {E, F} <=> {E}, {F}.

%% convert arbitrary polynomial equation to eq-notation
normalize @ {A = B} <=> globalize([A-B]), 
    normpoly([A-B] eq 0, Poly eq Const), Poly eq Const.

%% 0=C
zero @      [] eq C <=> zero(C).

%% Back substitution
%% N.B. These two rules below for back substitution must before the elimate rule,
%% because the elimate rule assumes the equation constraints must have been normalized.
%% The bound rule could normalize A*X to C, if X has been a constant.

%% const on left side (because of bound variable)
bound @     P eq C <=> select(A*X, P, P0), number(X) | C0 is C-A*X, P0 eq C0.

%% A*X = C  =>  bind variable
bind @      [A*X] eq C <=> var(X) | X is C/A.

%% Gaussian elimination 
elimate @   [A1*X|P1] eq C \ [A2*X|P2] eq D <=>
    multpoly(-A2/A1, P1 eq C, E),
    addpoly(E, P2 eq D, Q0 eq D0), 
    Q0 eq D0.

%% normalize first coefficient to 1
%% uncomment, if require first coefficient is 1 when having infinite number of solutions.
%% norm1 @     [A*X|P] eq C <=> notzero(A-1) | 
%%     multpoly(1/A,P eq C,P2 eq C2), [1*X|P2] eq C2.

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

%% normalize polynomial equation
normpoly([X] eq C, [1*X] eq C) :- var(X), !.
normpoly([X] eq C, [1*X] eq C) :- simple(X), \+ number(X).
normpoly([A] eq C, [] eq C0) :- number(A), C0 is C-A.
normpoly([A,B|P] eq C, E0) :- 
    normpoly([A] eq 0,E1), normpoly([B|P] eq C,E2), addpoly(E1,E2,E0).
normpoly([+A] eq 0, E0) :- 
    normpoly([A] eq 0, E0).
normpoly([-A] eq 0, E0) :- 
    normpoly([A] eq 0, E1), multpoly(-1, E1, E0).
normpoly([A+B] eq C, E0) :- 
    normpoly([A] eq C,E), normpoly([B] eq 0,F), addpoly(E,F,E0).
normpoly([A-B] eq C, E0) :- normpoly([A] eq C,E), normpoly([B] eq 0,F),
    multpoly(-1,F,F1), addpoly(E,F1,E0).
normpoly([A*X] eq 0, E0) :-
    safe_is(A1,A), !, normpoly([X] eq 0, E), multpoly(A1,E,E0). % add !, fix a bug in old code
normpoly([X*A] eq 0, E0) :-
    safe_is(A1,A), normpoly([X] eq 0, E), multpoly(A1,E,E0).
normpoly([X/A] eq 0, E0) :-
    safe_is(A1,1/A), normpoly([X] eq 0, E), multpoly(A1,E,E0).
normpoly([A] eq C, P0 eq C0) :- 
    C\==0, normpoly([A] eq 0, P0 eq C1), C0 is C+C1.

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



%% Test normpoly

%% ?- globalize([X,Y]), normpoly([3*X+2*Y+1] eq 0, Poly eq Const).
%@ Poly = [3* $VAR(X),2* $VAR(Y)],
%@ Const = -1 ;
%@ false.

%% ?- globalize([Y,X]), normpoly([3*X+2*Y+1] eq 0, Poly eq Const).
%@ Poly = [2* $VAR(Y),3* $VAR(X)],
%@ Const = -1 ;
%@ false.

%% Test normpoly's old bug fix

%% ?- normpoly([3*1] eq 0, Poly eq Const).
%@ Poly = [],
%@ Const = -3 ;
%@ false.

%% SAMPLE QUERIES

%% ?- {x+y=10, x-y=6}.
%@ ERROR: Unhandled exception: not_globalized
%% A: [1*x]eq 8.0, [1*y]eq 2.0.

%% ?- {X+Y=10, X-Y=6}.
%@ X = 8.0,
%@ Y = 2.0 ;
%@ false.

%% ?- {X+Y=10, X+Y=6}.
%@ false.

%% ?- {3 * X + 2 * Y - 4 * (3 + Z) = 2 * (X - 3) + (Y + Z) * 7,
%%     2 * (X + Y + Z) = 3 * (X - Y - Z),
%%     5 * (X + Y) - 7 * X - Z = (2 + 1 + X) * 6}.
%@ X = -1.7142857142857144,
%@ Y = 0.6571428571428571,
%@ Z = -1 ;

%% ?- {3 * X + 2 * Y - 4 * (3 + Z) = 2 * (X - 3) + (Y + Z) * 7,
%%     2 * (X + Y + Z) = 3 * (X - Y - Z)}.
%@ Z = -1,
%@ [1* $VAR(X),-5* $VAR(Y)]eq-5 ;
%@ false.

%% ?- {2+2*X+4*Y+2*Z=0, 4+2*X+4*Y+4*Z=0, 1+2*X+3*Z=0}.
%@ X = 1,
%@ Y = -0.5,
%@ Z = -1 ;
%@ false.

%% ?- {-10+1*X+1*Y=0, 2+1*X+(-1)*Y=0}.
%@ X = 4,
%@ Y = 6 ;
%@ false.

%% ?- {-10+1*X+1*Y=0, 2+1*X+(-1)*Y=0, -4+1*X=0}.
%@ X = 4,
%@ Y = 6 ;
%@ false.

%% ?- {-10+1*X+1*Y=0, 2+1*X+(-1)*Y=0, -5+1*X=0}.
%@ false.

%% ?- {-1+1*X+1*Y=0, -2+2*X+2*Y=0}.
%@ [1* $VAR(X),1* $VAR(Y)]eq 1 ;
%@ false.

%% ?- {X=Y+Z, X+Z=10, Y=10}.
%@ X = Y, Y = 10,
%@ Z = 0 ;
%@ false.

%% ?- {X+Y=10, X-Z=20, 2*Y+Z=30}.
%@ X = -30,
%@ Y = 40,
%@ Z = -50 ;
%@ false.

%% ?- {-(X)+Y+Z+2*Y+X+3*X+2=12}.
%@ [3* $VAR(X),3* $VAR(Y),1* $VAR(Z)]eq 10 ;
%@ false.
%% A: [1*X,1.0*Y,0.3333333333333333*Z]eq 3.333333333333333.