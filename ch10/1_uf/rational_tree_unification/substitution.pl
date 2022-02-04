%% 10.2 Rational tree unification with union-find

%% We assume that equations are in flat normal
%% form, i.e. the l.h.s. is a variable and the r.h.s. is either a variable or a
%% function symbol applied to variables. Recall

:- use_module(library(chr)).

:- op(600, xfx, '~>').
:- op(600, xfx, 'eq').

:- chr_constraint
    %% uion find  
    make/1,
    find/2,
    union/2,
    (~>)/2,
    link/2,
    root/1,
    noroot/1,
    
    %% rational tree unification
    eq/2, eq_list/2.

%% uion find  
uf_make     @ make(A) <=> root(A).

uf_union    @ union(A,B) <=> find(A,X), find(B,Y), link(X,Y).

uf_findNode @ A ~> B \ find(A,X) <=> find(B,X).  
uf_findRoot @ root(A) \ find(A,X) <=> X=A.

uf_linkEq   @ link(A,A) <=> true.  
uf_link     @ link(A,B), root(A), root(B) <=>  B ~> A, root(A), noroot(B).

% two same-length lists must be equal, 
% i.e., every pair of elements must be equal
eq_list([],[])                <=> true.
eq_list([X|L1],[Y|L2])        <=> X eq Y, eq_list(L1,L2).

%% rational tree unification
eq_union            @ X eq Y <=> var(X),var(Y) | union(X,Y).
eq_con+dec          @ X eq T1 \ X eq T2 <=> var(X),nonvar(T1),nonvar(T2) | same_functions(T1,T2).
%% eq_root1            @ X ~> Y \ X eq T <=> Y eq T.
eq_root             @ noroot(X) \ X eq T <=> find(X,Y), Y eq T.

%% Auxiliary

% functions must be equal
same_functions(T1,T2) :-
  T1=..[F|L1], T2=..[F|L2], same_length(L1,L2), eq_list(L1,L2).

%% ?- make(X1), make(X2), make(Y), make(Z), X1 eq f(Z,Y), X2 eq f(a,b), X1 eq X2.
%@ $VAR(X2)~> $VAR(X1),
%@ root($VAR(X1)),
%@ root($VAR(Z)),
%@ root($VAR(Y)),
%@ noroot($VAR(X2)),
%@ $VAR(Y) eq b,
%@ $VAR(Z) eq a,
%@ $VAR(X1) eq f($VAR(Z),$VAR(Y)).


%% TODO: Add decomposition rule to support nested function terms