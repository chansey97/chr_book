%% 8.2.2 Path consistency
%% Incomplete graphs.

%% The primitive constraints in c/3 must be ordered (<,=,>).

%% 8.2.4 Temporal reasoning with path consistency

%% N.B.
%% A PA network (X,C) is consistent if and only if the path consistency
%% algorithm does not make any constraint empty.

:- use_module(library(chr)).

:- chr_constraint c/3, label/0.

%% ps :-
%%   findall(Name,(current_chr_constraint(Module:Name)), Qs),
%%   writeln('constraint store contains:'),
%%   writeln(Qs).

%% Debugging
%% c(I,J,K) ==> format("==> c(~w,~w,~w) ~n", [I,J,K]), ps ,writeln("===").

%% Special Cases
c(I,J,[]) <=> false.
%% equality relation on the node self
c(I,I,C) ==> memberchk(=,C).
c(I,I,[=]) \ c(I,I,C) <=> true.
c(I,I,C) <=> \+ [=]=C | c(I,I,[=]).

%% 注意：
%% 以下光有 intersect rule 是不够的，必须有 subset rule 来保证 termination。
%% 如果没有 subset rule，intersect 产生的 c 尽管会变得更小，但它不会被记录，
%% 当该 c 在 composition rules 里匹配后，会产生一个更大的 c，而原来那个小的 c 被遗忘。
%% subset rule 在 kept 位置记录了那个最小的 c。

%% Intersection
subset @ c(I,J,C1) \ c(I,J,C2) <=> intersection(C1,C2,C3), C1==C3 | true. % important for termination!
intersect @ c(I,J,C1), c(I,J,C2) <=> intersection(C1,C2,C3) | c(I,J,C3).

%% Composition
composition_1 @ c(I,K,C1), c(K,J,C2) ==> composition(C1,C2,C3), c(I,J,C3).
composition_2 @ c(K,I,C1), c(K,J,C2) ==> composition(C1,C3,C2), c(I,J,C3).
composition_3 @ c(I,K,C1), c(J,K,C2) ==> composition(C3,C2,C1), c(I,J,C3).

%% Labeling
label, c(I,J,R) <=> \+ [_] = R | member(B,R), c(I,J,[B]), label.

%% Aux
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

%% TODO: Is it possible to implement a relational version of composition?
composition(R1,R2,R3) :- ground(R1), ground(R2), !,
  bagof(Z, X^Y^(member(X,R1), member(Y,R2), comp(X,Y,Z)), R4),
  list_to_ord_set(R4, R3).
composition(R1,R2,R3) :- ground(R1), ground(R3), !,
  bagof(Y, X^Z^(member(X,R1), member(Z,R3), comp(X,Y,Z)), R4),
  list_to_ord_set(R4, R2).
composition(R1,R2,R3) :- ground(R2), ground(R3), !,
  bagof(X, Y^Z^(member(Y,R2), member(Z,R3), comp(X,Y,Z)), R4),
  list_to_ord_set(R4, R1).


%% ?- c(I,K,[<]), c(K,I,[<]).
%@ false.

%% ?- c(I,K,[<,=]), c(K,J,[<]).
%@ c($VAR(J),$VAR(K),[>]),
%@ c($VAR(K),$VAR(K),[=]),
%@ c($VAR(K),$VAR(I),[=,>]),
%@ c($VAR(I),$VAR(I),[=]),
%@ c($VAR(J),$VAR(I),[>]),
%@ c($VAR(J),$VAR(J),[=]),
%@ c($VAR(I),$VAR(J),[<]),
%@ c($VAR(K),$VAR(J),[<]),
%@ c($VAR(I),$VAR(K),[<,=]).

%% ?- c(i,k,[<,=]), c(k,j,[<]).
%@ c(j,k,[>]),
%@ c(k,k,[=]),
%@ c(k,i,[=,>]),
%@ c(i,i,[=]),
%@ c(j,i,[>]),
%@ c(j,j,[=]),
%@ c(i,j,[<]),
%@ c(k,j,[<]),
%@ c(i,k,[<,=]).

%% ?- c(I,K,[<,=]), c(K,J,[<,=]), c(I,J,[=,>]).
%@ c($VAR(K),$VAR(I),[=]),
%@ c($VAR(J),$VAR(I),[=]),
%@ c($VAR(J),$VAR(K),[=]),
%@ c($VAR(K),$VAR(J),[=]),
%@ c($VAR(I),$VAR(K),[=]),
%@ c($VAR(I),$VAR(J),[=]),
%@ c($VAR(K),$VAR(K),[=]),
%@ c($VAR(I),$VAR(I),[=]),
%@ c($VAR(J),$VAR(J),[=]).

%% ?- c(i,k,[<,=]), c(k,j,[<,=]), c(i,j,[>,=]).
%@ c(k,i,[=]),
%@ c(j,i,[=]),
%@ c(j,k,[=]),
%@ c(k,j,[=]),
%@ c(i,k,[=]),
%@ c(i,j,[=]),
%@ c(k,k,[=]),
%@ c(i,i,[=]),
%@ c(j,j,[=]).

%% ----

%% https://youtu.be/NOtSLqIawk8?t=1895
%% "I read newspapers during breakfast and after breakfast I walked to my office."
%% "Did I start reading before finishing the walk and stop reading after finishing the walk?"

%% ?- c(bs,be,[<]), c(be,ws,[=]), c(ws,we,[<]),
%%    c(bs,rs,[<,=]), c(rs,re,[<]), c(re,be,[<,=]),
%%    c(rs,we,[<]), c(we,re,[<]).
%@ false.

%% https://youtu.be/NOtSLqIawk8?t=2228
%% PC verifies consistency but does not remove redundant constraints
%% The following network is consistent, but no primitive relation will removed.
%% The = relation between s and e are redundant, because it is impossible exist in
%% any world.

%% ?- c(t1,t2,[<,>]), c(t1,e,[<,=]),
%%    c(t2,e,[<,=]),
%%    c(s,t1,[<,=]), c(s,e,[<,=]), c(s,t2,[<,=]).
%@ c(t2,s,[=,>]),
%@ c(s,t2,[<,=]),
%@ c(t1,s,[=,>]),
%@ c(e,s,[=,>]),
%@ c(s,s,[=]),
%@ c(s,e,[<,=]),
%@ c(s,t1,[<,=]),
%@ c(e,t2,[=,>]),
%@ c(t2,e,[<,=]),
%@ c(e,t1,[=,>]),
%@ c(t1,t1,[=]),
%@ c(t2,t1,[<,>]),
%@ c(t2,t2,[=]),
%@ c(e,e,[=]),
%@ c(t1,e,[<,=]),
%@ c(t1,t2,[<,>]).

%% ?- c(t1,t2,[<,>]), c(t1,e,[<,=]),
%%    c(t2,e,[<,=]),
%%    c(s,t1,[<,=]), c(s,e,[<,=]), c(s,t2,[<,=]),label.
%@ c(t1,e,[=]),
%@ c(e,t1,[=]),
%@ c(s,t2,[=]),
%@ c(t1,t2,[>]),
%@ c(t1,s,[>]),
%@ c(s,t1,[<]),
%@ c(e,t2,[>]),
%@ c(e,s,[>]),
%@ c(s,e,[<]),
%@ c(t2,e,[<]),
%@ c(t2,t1,[<]),
%@ c(t2,s,[=]),
%@ c(s,s,[=]),
%@ c(t1,t1,[=]),
%@ c(t2,t2,[=]),
%@ c(e,e,[=]),
%@ label ;
%@ c(t1,e,[<]),
%@ c(e,t1,[>]),
%@ c(s,t2,[=]),
%@ c(t1,t2,[>]),
%@ c(t1,s,[>]),
%@ c(s,t1,[<]),
%@ c(e,t2,[>]),
%@ c(e,s,[>]),
%@ c(s,e,[<]),
%@ c(t2,e,[<]),
%@ c(t2,t1,[<]),
%@ c(t2,s,[=]),
%@ c(s,s,[=]),
%@ c(t1,t1,[=]),
%@ c(t2,t2,[=]),
%@ c(e,e,[=]),
%@ label ;
%@ c(t2,e,[=]),
%@ c(e,t2,[=]),
%@ c(s,t1,[=]),
%@ c(t2,t1,[>]),
%@ c(e,t1,[>]),
%@ c(t1,e,[<]),
%@ c(t1,t2,[<]),
%@ c(t1,s,[=]),
%@ c(s,t2,[<]),
%@ c(s,e,[<]),
%@ c(e,s,[>]),
%@ c(t2,s,[>]),
%@ c(s,s,[=]),
%@ c(t1,t1,[=]),
%@ c(t2,t2,[=]),
%@ c(e,e,[=]),
%@ label ;
%@ c(t2,e,[<]),
%@ c(e,t2,[>]),
%@ c(s,t1,[=]),
%@ c(t2,t1,[>]),
%@ c(e,t1,[>]),
%@ c(t1,e,[<]),
%@ c(t1,t2,[<]),
%@ c(t1,s,[=]),
%@ c(s,t2,[<]),
%@ c(s,e,[<]),
%@ c(e,s,[>]),
%@ c(t2,s,[>]),
%@ c(s,s,[=]),
%@ c(t1,t1,[=]),
%@ c(t2,t2,[=]),
%@ c(e,e,[=]),
%@ label ;
%@ c(t2,e,[=]),
%@ c(t2,t1,[>]),
%@ c(t1,e,[<]),
%@ c(t1,t2,[<]),
%@ c(e,t1,[>]),
%@ c(e,t2,[=]),
%@ c(s,t1,[<]),
%@ c(t1,s,[>]),
%@ c(s,t2,[<]),
%@ c(s,e,[<]),
%@ c(e,s,[>]),
%@ c(t2,s,[>]),
%@ c(s,s,[=]),
%@ c(t1,t1,[=]),
%@ c(t2,t2,[=]),
%@ c(e,e,[=]),
%@ label ;
%@ c(t1,e,[=]),
%@ c(t1,t2,[>]),
%@ c(t2,t1,[<]),
%@ c(e,t1,[=]),
%@ c(t2,e,[<]),
%@ c(e,t2,[>]),
%@ c(s,t1,[<]),
%@ c(t1,s,[>]),
%@ c(s,t2,[<]),
%@ c(s,e,[<]),
%@ c(e,s,[>]),
%@ c(t2,s,[>]),
%@ c(s,s,[=]),
%@ c(t1,t1,[=]),
%@ c(t2,t2,[=]),
%@ c(e,e,[=]),
%@ label ;
%@ c(t1,t2,[>]),
%@ c(t2,t1,[<]),
%@ c(t1,e,[<]),
%@ c(e,t1,[>]),
%@ c(t2,e,[<]),
%@ c(e,t2,[>]),
%@ c(s,t1,[<]),
%@ c(t1,s,[>]),
%@ c(s,t2,[<]),
%@ c(s,e,[<]),
%@ c(e,s,[>]),
%@ c(t2,s,[>]),
%@ c(s,s,[=]),
%@ c(t1,t1,[=]),
%@ c(t2,t2,[=]),
%@ c(e,e,[=]),
%@ label ;
%@ c(t1,t2,[<]),
%@ c(t2,t1,[>]),
%@ c(t1,e,[<]),
%@ c(e,t1,[>]),
%@ c(t2,e,[<]),
%@ c(e,t2,[>]),
%@ c(s,t1,[<]),
%@ c(t1,s,[>]),
%@ c(s,t2,[<]),
%@ c(s,e,[<]),
%@ c(e,s,[>]),
%@ c(t2,s,[>]),
%@ c(s,s,[=]),
%@ c(t1,t1,[=]),
%@ c(t2,t2,[=]),
%@ c(e,e,[=]),
%@ label.

