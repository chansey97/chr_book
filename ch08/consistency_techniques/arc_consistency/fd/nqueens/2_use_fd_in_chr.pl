%% 8.2.3 Finite domain arc consistency
%% Example 8.2.1

:- use_module(library(chr)).
:- use_module( library(lists), 
               [member/2,memberchk/2,select/3]).

%% ------------ FD Arc Consistency ------------

:- op(700, xfx, in).
:- op(700, xfx, ne).
:- op(700, xfx, le).

:- chr_constraint in/2, ne/2, le/2,  add/3, enum/1, indomain/1.

inconsistency @ X in [] <=> false.

intersect@ X in L1, X in L2 <=> intersection(L1,L2,L3) | X in L3.

le @ X le Y, X in L1, Y in L2 <=>
                            min_list(L1,MinX), min_list(L2,MinY), MinX > MinY |
                            filter_min(L2,L1,L3),                        
                            X le Y, X in L1, Y in L3.
le @ X le Y, X in L1, Y in L2 <=> 
                            max_list(L1,MaxX), max_list(L2,MaxY), MaxX > MaxY |
                            filter_max(L1,L2,L3),
                            X le Y, X in L3, Y in L2.

ne @ X in [V] \ X ne Y, Y in D <=> member(V,D) | X ne Y, select(V,D,D1), Y in D1.
ne @ Y in [V] \ X ne Y, X in C <=> member(V,C) | X ne Y,  select(V,C,C1), X in C1.

add(X,Y,Z), X in L1, Y in L2 ==> all_addition(L1,L2,L3), Z in L3.

enum([]) <=> true.
enum([X|L]) <=> indomain(X), enum(L).
indomain(X), X in [V|L] <=> L=[_|_] |
    (   X in [V]
    ;   X in L, indomain(X)).

filter_max(L1, L2, L3) :-
  max_list(L2,MaxL2), remove_higher(MaxL2,L1,L3).

filter_min(L1, L2, L3) :- 
  min_list(L2,MinL2), remove_lower(MinL2,L1,L3).

remove_lower(_,[],L1):- !, L1=[].
remove_lower(Min,[X|L],L1):-
	X@<Min,
	!,
	remove_lower(Min,L,L1).
remove_lower(Min,[X|L],[X|L1]):-
	remove_lower(Min,L,L1).

remove_higher(_,[],L1):- !, L1=[].
remove_higher(Max,[X|L],L1):-
	X@>Max,
	!,
	remove_higher(Max,L,L1).
remove_higher(Max,[X|L],[X|L1]):-
	remove_higher(Max,L,L1).

intersection([], _, []).
intersection([Head|L1tail], L2, L3) :-
        memberchk(Head, L2),
        !,
        L3 = [Head|L3tail],
        intersection(L1tail, L2, L3tail).
intersection([_|L1tail], L2, L3) :-
        intersection(L1tail, L2, L3).

all_addition(L1,L2,L3) :- 
	setof(Z, X^Y^(member(X,L1), member(Y,L2), Z is X + Y), L3).

%% ------------ N-queens ------------

:- chr_constraint solve/2, queens/1, safe/3, no_attack/3.

solve(N,Qs)         <=> makedomains(N,Qs), queens(Qs), enum(Qs).

queens([])          <=> true.
queens([Q|Qs])      <=> safe(Q,Qs,1), queens(Qs).

safe(_,[],_)        <=> true.
safe(X,[Y|Qs],N)    <=> no_attack(X,Y,N), N1 is N+1, safe(X,Qs,N1).

%% implement no_attack constraint by FD arithmetic operator
no_attack(X,Y,N) <=>
  X ne Y,
  N2 in [N],
  add(X,N2,XN), XN ne Y,
  add(Y,N2,YN), YN ne X.

%% Aux

%% makedomains(N,Qs): Qs is an N-elem. list, create 'X in [1..N]' constraints
makedomains(N,Qs) :- length(Qs,N), upto(N,D), domain(Qs,D).

upto(0,[]).
upto(N,[N|L]) :- N>0, N1 is N-1, upto(N1,L).

domain([],_).
domain([Q|Qs],D) :- Q in D, domain(Qs,D).


%% SAMPLE QUERIES

%% ?- solve(1,S).
%@ S = [$VAR(_A)],
%@ $VAR(_A)in[1],
%@ indomain($VAR(_A)) ;
%@ false.

%% ?- solve(2,S).
%@ false.

%% ?- solve(3,S).
%@ false.

%% ?- solve(4,S).
%@ S = [$VAR(_A),$VAR(_B),$VAR(_C),$VAR(_D)],
%@ $VAR(_E)in[2],
%@ $VAR(_F)in[6],
%@ $VAR(_G)in[5],
%@ $VAR(_H)in[5],
%@ $VAR(_I)in[5],
%@ $VAR(_J)in[4],
%@ $VAR(_K)in[3],
%@ $VAR(_D)in[2],  %%
%@ $VAR(_C)in[4],  %%
%@ $VAR(_L)in[2],
%@ $VAR(_M)in[3],
%@ $VAR(_B)in[1],  %%
%@ $VAR(_N)in[4],
%@ $VAR(_O)in[5],
%@ $VAR(_P)in[6],
%@ $VAR(_A)in[3],  %%
%@ $VAR(_Q)in[1],
%@ $VAR(_R)in[2],
%@ $VAR(_S)in[1],
%@ $VAR(_T)in[3],
%@ $VAR(_U)in[2],
%@ $VAR(_V)in[1],
%@ $VAR(_C)ne$VAR(_D),
%@ $VAR(_L)ne$VAR(_C),
%@ $VAR(_B)ne$VAR(_C),
%@ $VAR(_B)ne$VAR(_D),
%@ $VAR(_N)ne$VAR(_B),
%@ $VAR(_E)ne$VAR(_A),
%@ $VAR(_A)ne$VAR(_B),
%@ $VAR(_F)ne$VAR(_A),
%@ $VAR(_A)ne$VAR(_C),
%@ $VAR(_A)ne$VAR(_D),
%@ $VAR(_K)ne$VAR(_C),
%@ $VAR(_H)ne$VAR(_D),
%@ $VAR(_J)ne$VAR(_B),
%@ $VAR(_M)ne$VAR(_D),
%@ $VAR(_G)ne$VAR(_B),
%@ $VAR(_I)ne$VAR(_A),
%@ $VAR(_P)ne$VAR(_D),
%@ $VAR(_O)ne$VAR(_C),
%@ add($VAR(_D),$VAR(_Q),$VAR(_K)),
%@ add($VAR(_C),$VAR(_Q),$VAR(_H)),
%@ add($VAR(_D),$VAR(_R),$VAR(_J)),
%@ add($VAR(_B),$VAR(_R),$VAR(_M)),
%@ add($VAR(_C),$VAR(_S),$VAR(_G)),
%@ add($VAR(_B),$VAR(_S),$VAR(_L)),
%@ add($VAR(_D),$VAR(_T),$VAR(_I)),
%@ add($VAR(_A),$VAR(_T),$VAR(_P)),
%@ add($VAR(_C),$VAR(_U),$VAR(_F)),
%@ add($VAR(_A),$VAR(_U),$VAR(_O)),
%@ add($VAR(_B),$VAR(_V),$VAR(_E)),
%@ add($VAR(_A),$VAR(_V),$VAR(_N)),
%@ indomain($VAR(_D)),
%@ indomain($VAR(_C)),
%@ indomain($VAR(_B)) ;
%@ S = [$VAR(_A),$VAR(_B),$VAR(_C),$VAR(_D)],
%@ $VAR(_E)in[3],
%@ $VAR(_F)in[2],
%@ $VAR(_G)in[2],
%@ $VAR(_H)in[6],
%@ $VAR(_I)in[5],
%@ $VAR(_J)in[4],
%@ $VAR(_D)in[3],  %%
%@ $VAR(_C)in[1],  %%
%@ $VAR(_K)in[5],
%@ $VAR(_L)in[5],
%@ $VAR(_M)in[6],
%@ $VAR(_B)in[4],  %%
%@ $VAR(_N)in[3],
%@ $VAR(_O)in[4],
%@ $VAR(_P)in[5],
%@ $VAR(_A)in[2],  %%
%@ $VAR(_Q)in[1],
%@ $VAR(_R)in[2],
%@ $VAR(_S)in[1],
%@ $VAR(_T)in[3],
%@ $VAR(_U)in[2],
%@ $VAR(_V)in[1],
%@ $VAR(_C)ne$VAR(_D),
%@ $VAR(_F)ne$VAR(_B),
%@ $VAR(_B)ne$VAR(_D),
%@ $VAR(_N)ne$VAR(_B),
%@ $VAR(_O)ne$VAR(_C),
%@ $VAR(_K)ne$VAR(_A),
%@ $VAR(_A)ne$VAR(_B),
%@ $VAR(_A)ne$VAR(_C),
%@ $VAR(_A)ne$VAR(_D),
%@ $VAR(_J)ne$VAR(_C),
%@ $VAR(_G)ne$VAR(_D),
%@ $VAR(_I)ne$VAR(_B),
%@ $VAR(_M)ne$VAR(_D),
%@ $VAR(_L)ne$VAR(_C),
%@ $VAR(_B)ne$VAR(_C),
%@ $VAR(_H)ne$VAR(_A),
%@ $VAR(_P)ne$VAR(_D),
%@ $VAR(_E)ne$VAR(_A),
%@ add($VAR(_D),$VAR(_Q),$VAR(_J)),
%@ add($VAR(_C),$VAR(_Q),$VAR(_G)),
%@ add($VAR(_D),$VAR(_R),$VAR(_I)),
%@ add($VAR(_B),$VAR(_R),$VAR(_M)),
%@ add($VAR(_C),$VAR(_S),$VAR(_F)),
%@ add($VAR(_B),$VAR(_S),$VAR(_L)),
%@ add($VAR(_D),$VAR(_T),$VAR(_H)),
%@ add($VAR(_A),$VAR(_T),$VAR(_P)),
%@ add($VAR(_C),$VAR(_U),$VAR(_E)),
%@ add($VAR(_A),$VAR(_U),$VAR(_O)),
%@ add($VAR(_B),$VAR(_V),$VAR(_K)),
%@ add($VAR(_A),$VAR(_V),$VAR(_N)),
%@ indomain($VAR(_D)),
%@ indomain($VAR(_C)) ;
%@ false.
