%% 10.1.1 Basic union-find

%% :- module(unionfind,[make/1,union/2,find/2,root/1,(~>)/2]).
:- use_module(library(chr)).

:- op(700, xfx, '~>').

:- chr_constraint 
    make/1,
    find/2,
    union/2,
    (~>)/2,
    link/2,
    root/1.

%% The following operations are supported:

%% *make(A)*:    create a new set with single element A   
%% *find(A,B)*:  B is representative of the set containing A
%% *union(A,B)*: join the two sets containing A and B
%% *link(A,B)*:  join the two sets represented by A and B (internal)

%% Data is represented as:                                                     #
%%   *root(A)*:    A is the representative of a set (root of tree)             #
%%   *A ~> B*:     A and B are in the same set (edge indirectly pointing to root)

make     @ make(A) <=> root(A).

union    @ union(A,B) <=> find(A,X), find(B,Y), link(X,Y).

findNode @ A ~> B \ find(A,X) <=> find(B,X).  
findRoot @ root(A) \ find(A,X) <=> X=A.

linkEq   @ link(A,A) <=> true.  
link     @ link(A,B), root(A), root(B) <=>  B ~> A, root(A).

%% Tests

%?- make(a), make(b), make(c), make(d), make(e), union(a,b), union(c,d), union(e,c).
%@ c~>e,
%@ d~>c,
%@ b~>a,
%@ root(e),
%@ root(a).

%?- make(a), make(b), make(c), make(d), make(e), union(a,b), union(c,d), union(e,c), find(b,X), find(d,Y).
%@ X = a,
%@ Y = e,
%@ c~>e,
%@ d~>c,
%@ b~>a,
%@ root(e),
%@ root(a).
