%% 8.1.2 Boolean cardinality

%% The cardinality constraint is a so-called global constraint. Syntactically,
%% a global constraint is one that admits an arbitrary number of variables as
%% argument.

%% • negation card(0,0,[C],1)
%% • exclusive or (xor) card(1,1,[C1,C2],2)
%% • nand (negated and) card(0,1,[C1,C2],2)
%% • at most one card(0,1,[C1,...,Cn],N)
%% • conjunction card(N,N,[C1,...,Cn],N)
%% • disjunction card(1,N,[C1,...,Cn],N).

:- use_module(library(chr)).

:- chr_constraint card/4, enum/1, indomain/1.

triv_sat @ card(A,B,L,N) <=> A=<0,N=<B | true.
pos_sat @ card(N,B,L,N) <=> set_to_ones(L).
neg_sat @ card(A,0,L,N) <=> set_to_zeros(L).
pos_red @ card(A,B,L,N) <=> delete(X,L,L1),X==1 | A1 is A-1, B1 is B-1, N1 is N-1, card(A1,B1,L1,N1).
neg_red @ card(A,B,L,N) <=> delete(X,L,L1),X==0 | N1 is N-1, card(A,B,L1,N1).

%% built-ins
set_to_ones([]).
set_to_ones([1|L]):- set_to_ones(L).

set_to_zeros([]).
set_to_zeros([0|L]):- set_to_zeros(L).

delete(X,[X|L],L).
delete(Y,[X|Xs],[X|Xt]) :- delete(Y,Xs,Xt).

%% labeling
enum([]) <=> true.
enum([X|L]) <=> indomain(X), enum(L).
indomain(X) <=> (X=0 ; X=1).


%% • negation card(0,0,[C],1)

%?- card(0,0,[1],1).
%@ false.

%?- card(0,0,[0],1).
%@ true.

%?- card(0,0,[C],1).
%@ C = 0.

%% • exclusive or (xor) card(1,1,[C1,C2],2)

%?- card(1,1,[0,0],2).
%@ false.

%?- card(1,1,[C1,C2],2).
%@ card(1,1,[$VAR(C1),$VAR(C2)],2).

%?- card(1,1,[C1,C2],2),enum([C1,C2]).
%@ C1 = 0,
%@ C2 = 1 ;
%@ C1 = 1,
%@ C2 = 0 ;
%@ false.

%% • nand (negated and) card(0,1,[C1,C2],2)

%?- card(0,1,[C1,C2],2), enum([C1,C2]).
%@ C1 = C2, C2 = 0 ;
%@ C1 = 0,
%@ C2 = 1 ;
%@ C1 = 1,
%@ C2 = 0 ;
%@ false.

%% • at most one card(0,1,[C1,...,Cn],N)

%?- card(0,1,[C1,C2],2), enum([C1,C2]).
%@ C1 = C2, C2 = 0 ;
%@ C1 = 0,
%@ C2 = 1 ;
%@ C1 = 1,
%@ C2 = 0 ;
%@ false.

%% • conjunction card(N,N,[C1,...,Cn],N)

%?- card(2,2,[C1,C2],2), enum([C1,C2]).
%@ C1 = C2, C2 = 1.

%% • disjunction card(1,N,[C1,...,Cn],N).

%?- card(1,2,[C1,C2],2), enum([C1,C2]).
%@ C1 = 0,
%@ C2 = 1 ;
%@ C1 = 1,
%@ C2 = 0 ;
%@ C1 = C2, C2 = 1.

%% later awake

%?- card(1,2,[C1,C2,C3],3).
%@ card(1,2,[$VAR(C1),$VAR(C2),$VAR(C3)],3).

%?- card(1,2,[C1,C2,C3],3), C1=0.
%@ C1 = 0,
%@ card(1,2,[$VAR(C2),$VAR(C3)],2).

%?- card(1,2,[C1,C2,C3],3), C1=0, C2=0.
%@ C1 = C2, C2 = 0,
%@ C3 = 1.

%% Example of Clause resolution

%% (X1 ∨ X2)  ∧ (X2 ∨ X3)
%?- card(1,2,[X1,X2],2), card(1,2,[X2,X3],2), enum([X1,X2]).
%@ X1 = 0,
%@ X2 = 1 ;
%@ X1 = X3, X3 = 1,
%@ X2 = 0 ;
%@ X1 = X2, X2 = 1.

