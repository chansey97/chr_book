%% 8.2.2 Path consistency

%% The primitive constraints in c/3 must be ordered (<,=,>).

%% To avoid technicalities, we assume a complete graph.
%% Thus, in a query, for each pair of variables, there is
%% a disjunctive binary constraint between them.

%% 8.2.4 Temporal reasoning with path consistency

%% N.B.
%% A PA network (X,C) is consistent if and only if the path consistency
%% algorithm does not make any constraint empty.

:- use_module(library(chr)).

:- chr_constraint c/3, complete/1, consistent/0, inv/0, trans/0, label/0.

complete(L) <=> ids(L), inv, trans.

%% Inverse direction
inv, c(X,Y,C) \ c(X,Y,C) <=> true.
inv, c(X,Y,C) ==> maplist(invert,C,C2), sort(C2, C3), c(Y,X,C3).
inv <=> true.

%% Transitive Closure
trans, c(X,Y,C) \ c(X,Y,C) <=> true.
trans, c(X,Y,C1), c(Y,Z,C2) ==> c(X,Z,[<,=,>]). 
trans <=> true.

%% Special Cases
consistent, c(I,J,[]) <=> false.
%% equality relation on the node self
consistent, c(I,I,C) ==> memberchk(=,C).
consistent, c(I,I,[=]) \ c(I,I,C) <=> true.
consistent, c(I,I,C) <=> \+ [=]=C | c(I,I,[=]).

%% Path Consistency
dup @ consistent, c(X,Y,C) \ c(X,Y,C) <=> true.
path_consistency @ consistent, c(I,K,R1), c(K,J,R2) \ c(I,J,R3) <=>
    composition(R1,R2,R12), intersection(R12,R3,R123), sort(R123, R123s),
    R123s\==R3 | 
    c(I,J,R123s).

%% Labeling
label, c(I,J,R) <=> \+ [_] = R | member(B,R), c(I,J,[B]), label.

%% Aux
ids([]).
ids([X|Xs]) :- c(X,X,[=]), ids(Xs).
                                        
invert(<,>).
invert(=,=).
invert(>,<).

comp(<,<,<). comp(=,<,<). comp(>,<,<). comp(>,<,=). comp(>,<,>). 
comp(<,=,<). comp(=,=,=). comp(>,=,>). 
comp(<,>,<). comp(=,>,>). comp(>,>,>). 
comp(<,>,=).
comp(<,>,>).

intersection([], _, []).
intersection([Head|L1tail], L2, L3) :-
  memberchk(Head, L2),
  !,
  L3 = [Head|L3tail],
  intersection(L1tail, L2, L3tail).
intersection([_|L1tail], L2, L3) :-
  intersection(L1tail, L2, L3).

composition(R1,R2,R12) :-
  bagof(Z, X^Y^(member(X,R1), member(Y,R2), comp(X,Y,Z)), R3),
  list_to_ord_set(R3, R12).

%% Examples

%% ?- c(I,K,[<]), c(K,I,[<]),
%%    complete([I,J,K]), consistent.
%@ false.

%% ?- c(I,K,[<,=]), c(K,J,[<]),
%%    complete([I,J,K]), consistent.
%@ c($VAR(J),$VAR(I),[>]),
%@ c($VAR(I),$VAR(J),[<]),
%@ c($VAR(K),$VAR(I),[=,>]),
%@ c($VAR(J),$VAR(K),[>]),
%@ c($VAR(K),$VAR(K),[=]),
%@ c($VAR(J),$VAR(J),[=]),
%@ c($VAR(I),$VAR(I),[=]),
%@ c($VAR(K),$VAR(J),[<]),
%@ c($VAR(I),$VAR(K),[<,=]),
%@ consistent.

%% ?- c(i,k,[<,=]), c(k,j,[<]),
%%     complete([i,j,k]), consistent.
%@ c(j,i,[>]),
%@ c(i,j,[<]),
%@ c(k,i,[=,>]),
%@ c(j,k,[>]),
%@ c(k,k,[=]),
%@ c(j,j,[=]),
%@ c(i,i,[=]),
%@ c(k,j,[<]),
%@ c(i,k,[<,=]),
%@ consistent.

%% ?- c(I,K,[<,=]), c(K,J,[<,=]), c(I,J,[=,>]),
%%    complete([I,J,K]),consistent.
%@ c($VAR(K),$VAR(I),[=]),
%@ c($VAR(K),$VAR(J),[=]),
%@ c($VAR(I),$VAR(J),[=]),
%@ c($VAR(I),$VAR(K),[=]),
%@ c($VAR(J),$VAR(K),[=]),
%@ c($VAR(J),$VAR(I),[=]),
%@ c($VAR(K),$VAR(K),[=]),
%@ c($VAR(J),$VAR(J),[=]),
%@ c($VAR(I),$VAR(I),[=]),
%@ consistent.

%% ?- c(i,k,[<,=]), c(k,j,[<,=]), c(i,j,[>,=]),
%%    complete([i,j,k]),consistent.
%@ c(j,k,[=]),
%@ c(j,i,[=]),
%@ c(k,i,[=]),
%@ c(k,j,[=]),
%@ c(i,j,[=]),
%@ c(i,k,[=]),
%@ c(k,k,[=]),
%@ c(j,j,[=]),
%@ c(i,i,[=]),
%@ consistent.

%% ----

%% https://youtu.be/NOtSLqIawk8?t=1895
%% "I read newspapers during breakfast and after breakfast I walked to my office."
%% "Did I start reading before finishing the walk and stop reading after finishing the walk?"

%% ?- c(bs,be,[<]), c(be,ws,[=]), c(ws,we,[<]),
%%    c(bs,rs,[<,=]), c(rs,re,[<]), c(re,be,[<,=]),
%%    c(rs,we,[<]), c(we,re,[<]),
%%    complete([bs,be,ws,we,rs,re]), consistent.
%@ false.

%% https://youtu.be/NOtSLqIawk8?t=2228
%% PC verifies consistency but does not remove redundant constraints
%% The following network is consistent, but no primitive relation will removed.
%% The = relation between s and e are redundant, because it is impossible exist in
%% any world.

%% ?- c(t1,t2,[<,>]), c(t1,e,[<,=]),
%%    c(t2,e,[<,=]),
%%    c(s,t1,[<,=]), c(s,e,[<,=]), c(s,t2,[<,=]),
%%    complete([t1,t2,e,s]), consistent.
%@ c(t2,t1,[<,>]),
%@ c(e,t1,[=,>]),
%@ c(e,t2,[=,>]),
%@ c(t1,s,[=,>]),
%@ c(e,s,[=,>]),
%@ c(t2,s,[=,>]),
%@ c(s,s,[=]),
%@ c(e,e,[=]),
%@ c(t2,t2,[=]),
%@ c(t1,t1,[=]),
%@ c(s,t2,[<,=]),
%@ c(s,e,[<,=]),
%@ c(s,t1,[<,=]),
%@ c(t2,e,[<,=]),
%@ c(t1,e,[<,=]),
%@ c(t1,t2,[<,>]),
%@ consistent.

%% ?- c(t1,t2,[<,>]), c(t1,e,[<,=]),
%%    c(t2,e,[<,=]),
%%    c(s,t1,[<,=]), c(s,e,[<,=]), c(s,t2,[<,=]),
%%    complete([t1,t2,e,s]), consistent, label.
%@ c(t1,e,[=]),
%@ c(t1,t2,[>]),
%@ c(s,t2,[=]),
%@ c(t2,s,[=]),
%@ c(t1,s,[>]),
%@ c(e,s,[>]),
%@ c(e,t2,[>]),
%@ c(e,t1,[=]),
%@ c(s,t1,[<]),
%@ c(s,e,[<]),
%@ c(t2,e,[<]),
%@ c(t2,t1,[<]),
%@ c(s,s,[=]),
%@ c(e,e,[=]),
%@ c(t2,t2,[=]),
%@ c(t1,t1,[=]),
%@ consistent,
%@ label ;
%@ c(t1,t2,[>]),
%@ c(t1,e,[=]),
%@ c(s,t2,[<]),
%@ c(t2,s,[>]),
%@ c(t1,s,[>]),
%@ c(e,s,[>]),
%@ c(e,t2,[>]),
%@ c(e,t1,[=]),
%@ c(s,t1,[<]),
%@ c(s,e,[<]),
%@ c(t2,e,[<]),
%@ c(t2,t1,[<]),
%@ c(s,s,[=]),
%@ c(e,e,[=]),
%@ c(t2,t2,[=]),
%@ c(t1,t1,[=]),
%@ consistent,
%@ label ;
%@ c(t1,e,[<]),
%@ c(t1,t2,[>]),
%@ c(s,t2,[=]),
%@ c(t2,s,[=]),
%@ c(t1,s,[>]),
%@ c(e,t2,[>]),
%@ c(e,s,[>]),
%@ c(e,t1,[>]),
%@ c(s,t1,[<]),
%@ c(s,e,[<]),
%@ c(t2,e,[<]),
%@ c(t2,t1,[<]),
%@ c(s,s,[=]),
%@ c(e,e,[=]),
%@ c(t2,t2,[=]),
%@ c(t1,t1,[=]),
%@ consistent,
%@ label ;
%@ c(t1,t2,[>]),
%@ c(t1,e,[<]),
%@ c(s,t2,[<]),
%@ c(t2,s,[>]),
%@ c(t1,s,[>]),
%@ c(e,t2,[>]),
%@ c(e,s,[>]),
%@ c(e,t1,[>]),
%@ c(s,t1,[<]),
%@ c(s,e,[<]),
%@ c(t2,e,[<]),
%@ c(t2,t1,[<]),
%@ c(s,s,[=]),
%@ c(e,e,[=]),
%@ c(t2,t2,[=]),
%@ c(t1,t1,[=]),
%@ consistent,
%@ label ;
%@ c(t2,e,[=]),
%@ c(s,t1,[=]),
%@ c(t1,s,[=]),
%@ c(s,t2,[<]),
%@ c(s,e,[<]),
%@ c(t1,e,[<]),
%@ c(t1,t2,[<]),
%@ c(e,t2,[=]),
%@ c(e,t1,[>]),
%@ c(e,s,[>]),
%@ c(t2,s,[>]),
%@ c(t2,t1,[>]),
%@ c(s,s,[=]),
%@ c(e,e,[=]),
%@ c(t2,t2,[=]),
%@ c(t1,t1,[=]),
%@ consistent,
%@ label ;
%@ c(t2,e,[=]),
%@ c(s,t1,[<]),
%@ c(t1,s,[>]),
%@ c(s,t2,[<]),
%@ c(s,e,[<]),
%@ c(t1,e,[<]),
%@ c(t1,t2,[<]),
%@ c(e,t2,[=]),
%@ c(e,t1,[>]),
%@ c(e,s,[>]),
%@ c(t2,s,[>]),
%@ c(t2,t1,[>]),
%@ c(s,s,[=]),
%@ c(e,e,[=]),
%@ c(t2,t2,[=]),
%@ c(t1,t1,[=]),
%@ consistent,
%@ label ;
%@ c(t2,e,[<]),
%@ c(s,t1,[=]),
%@ c(s,t2,[<]),
%@ c(s,e,[<]),
%@ c(t1,e,[<]),
%@ c(t1,t2,[<]),
%@ c(t1,s,[=]),
%@ c(e,t2,[>]),
%@ c(e,t1,[>]),
%@ c(e,s,[>]),
%@ c(t2,s,[>]),
%@ c(t2,t1,[>]),
%@ c(s,s,[=]),
%@ c(e,e,[=]),
%@ c(t2,t2,[=]),
%@ c(t1,t1,[=]),
%@ consistent,
%@ label ;
%@ c(t1,t2,[<]),
%@ c(t1,e,[<]),
%@ c(t2,e,[<]),
%@ c(s,t1,[<]),
%@ c(s,e,[<]),
%@ c(s,t2,[<]),
%@ c(t1,s,[>]),
%@ c(e,t2,[>]),
%@ c(e,t1,[>]),
%@ c(e,s,[>]),
%@ c(t2,s,[>]),
%@ c(t2,t1,[>]),
%@ c(s,s,[=]),
%@ c(e,e,[=]),
%@ c(t2,t2,[=]),
%@ c(t1,t1,[=]),
%@ consistent,
%@ label ;
%@ false.