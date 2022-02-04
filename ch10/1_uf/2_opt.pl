%% 10.1.2 Optimized union-find

%% :- module(unionfind_opt, [make/1,union/2,find/2,root/2,(~>)/2]).
:- use_module(library(chr)).

:- op(700, xfx, '~>').

:- chr_constraint
    make(+element),
    find(?element,?element),
    union(+element,+element),
    (?element) ~> (+element),
    link(+element,?element),
    root(+element,?natural).

%:- chr_type element == dense_int.          % efficient: arrays
%% :- chr_type element == int.                % less efficient: hashtables
:- chr_type element == any.     % less efficient: hashtables

%% The following operations are supported:
%%   *make(A)*:    create a new set with single element A
%%   *find(A,B)*:  B is representative of the set containing A
%%   *union(A,B)*: join the two sets containing A and B
%%   *link(A,B)*:  join the two sets represented by A and B (internal)         

%% Data is represented as:
%%   *root(A,N)*:  A is the representative of a set (root of tree with depth N)
%%   *A ~> B*:     A and B are in the same set (edge indirectly pointing to root)
  
make     @ make(A) <=> root(A,0).

union    @ union(A,B) <=> find(A,X), find(B,Y), link(X,Y).

findNode @ A ~> B, find(A,X) <=> find(B,X), A ~> X.
findRoot @ root(A,_) \ find(A,X) <=> X=A.

linkEq   @ link(A,A) <=> true.  
linkLeft @ link(A,B), root(A,NA), root(B,NB) <=> NA>=NB | 
                B ~> A, NA1 is max(NA,NB+1), root(A,NA1).
linkRight@ link(B,A), root(A,NA), root(B,NB) <=> NA>=NB |
                B ~> A, NA1 is max(NA,NB+1), root(A,NA1).

%% Tests

%?- make(a), make(b), make(c), make(d), make(e), union(a,b), union(c,d), union(e,c).
%@ e~>d,
%@ c~>d,
%@ a~>b,
%@ root(d,1),
%@ root(b,1).

%?- make(a), make(b), make(c), make(d), make(e), union(a,b), union(c,d), union(e,c), find(b,X), find(d,Y).
%@ X = b,
%@ Y = d,
%@ e~>d,
%@ c~>d,
%@ a~>b,
%@ root(d,1),
%@ root(b,1).

%% Test union-by-rank opt, comparing with 1_basic.pl

%?- make(a), make(b), make(c), make(d), make(e), union(a,b), union(c,d), union(e,c).
%@ e~>d,
%@ c~>d,
%@ a~>b,
%@ root(d,1),
%@ root(b,1).

%% Test path compression opt

%?- make(a), make(b), make(c), make(d), make(e), union(a,b), union(c,d), union(e,c), union(c,a).
%@ b~>d,
%@ a~>b,
%@ c~>d,
%@ e~>d,
%@ root(d,2).

%?- make(a), make(b), make(c), make(d), make(e), union(a,b), union(c,d), union(e,c), union(c,a), find(a,X).
%@ X = d,
%@ a~>d, % compress
%@ b~>d,
%@ c~>d,
%@ e~>d,
%@ root(d,2).

