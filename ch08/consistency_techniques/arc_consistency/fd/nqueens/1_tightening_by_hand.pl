%% 8.2.3 Finite domain arc consistency
%% Example 8.2.1

:- use_module(library(chr)).
:- use_module(library(lists)).

:- op(700,xfx,'in').

:- chr_constraint solve/2, queens/1, safe/3, noattack/3, in/2, enum/1, indomain/1.

solve(N,Qs)         <=> makedomains(N,Qs) , queens(Qs), enum(Qs).

queens([])          <=> true.
queens([Q|Qs])      <=> safe(Q,Qs,1), queens(Qs).

safe(_,[],_)        <=> true.
safe(X,[Y|Qs],N)    <=> noattack(X,Y,N), N1 is N+1, safe(X,Qs,N1).

inconsistency @ _ in []             <=> fail.

% N.B. The noattack can be seen as arc constraints in arc consistency
X in [V] \ noattack(X,Y,N), Y in D <=> V1 is V+N, V2 is V-N, remove(D,[V,V1,V2],D1) | Y in D1.
X in [V] \ noattack(Y,X,N), Y in D <=> V1 is V+N, V2 is V-N, remove(D,[V,V1,V2],D1) | Y in D1.

%% labling
enum([]) <=> true.
enum([X|L]) <=> indomain(X), enum(L).
indomain(X), X in [V|L] <=> L=[_|_] |
    (   X in [V]
    ;   X in L, indomain(X)).

%% Aux

%% makedomains(N,Qs): Qs is an N-elem. list, create 'X in [1..N]' constraints
makedomains(N,Qs) :- length(Qs,N), upto(N,D), domain(Qs,D).

upto(0,[]).
upto(N,[N|L]) :- N>0, N1 is N-1, upto(N1,L).

domain([],_).
domain([Q|Qs],D) :- Q in D, domain(Qs,D).

%% The remove(D,L,D1) holds if D1 is D without the values in L
%% and at least one value has been removed.
remove(D,L,D1) :- remove_list(D,L,D1), D\==D1.
remove_list([], _, []).
remove_list([X|Tail], L2, Result):- member(X, L2), !, remove_list(Tail, L2, Result). 
remove_list([X|Tail], L2, [X|Result]):- remove_list(Tail, L2, Result).


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
%@ $VAR(_D)in[2],
%@ $VAR(_C)in[4],
%@ $VAR(_B)in[1],
%@ $VAR(_A)in[3],
%@ indomain($VAR(_D)),
%@ indomain($VAR(_C)),
%@ indomain($VAR(_B)) ;
%@ S = [$VAR(_A),$VAR(_B),$VAR(_C),$VAR(_D)],
%@ $VAR(_D)in[3],
%@ $VAR(_C)in[1],
%@ $VAR(_B)in[4],
%@ $VAR(_A)in[2],
%@ indomain($VAR(_D)),
%@ indomain($VAR(_C)),
%@ indomain($VAR(_B)) ;
%@ false.