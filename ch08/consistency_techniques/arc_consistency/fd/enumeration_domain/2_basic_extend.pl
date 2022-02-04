%% 8.2.3 Finite domain arc consistency

:- use_module(library(chr)).

:- use_module( library(lists), 
               [member/2,memberchk/2,select/3,
                last/2,is_list/1,min_list/2, max_list/2,
                remove_duplicates/2]).

% for domain constraints
:- op(700, xfx, in).
:- op(650, xfx, '..').

% for inequality constraints
:- op(700, xfx, eq).
:- op(700, xfx, le).
:- op(700, xfx, ne).
:- op(700, xfx, lt).

:- chr_constraint in/2, eq/2, le/2, ne/2, lt/2, add/3, mult/3, enum/1, indomain/1.

inconsistency @ X in [] <=> false.

% intersection of domains for the same variable

X in L, X in Min..Max <=> remove_lower(Min,L,L1), remove_higher(Max,L1,L2), X in L2.

intersect@ X in L1, X in L2 <=> intersection(L1,L2,L3) | X in L3.

% Inequality -------------------------------

%% N.B. Because of arc consistency, an atomic constraint has semantically a pair of symmetry rules. 

le @ X le Y, X in L1, Y in L2 <=>
                            min_list(L1,MinX), min_list(L2,MinY), MinX > MinY |
                            filter_min(L2,L1,L3),                        
                            X le Y, X in L1, Y in L3.
le @ X le Y, X in L1, Y in L2 <=> 
                            max_list(L1,MaxX), max_list(L2,MaxY), MaxX > MaxY |
                            filter_max(L1,L2,L3),
                            X le Y, X in L3, Y in L2.

lt @ X lt Y, X in L1, Y in L2 <=> 
                            min_list(L1,MinX), min_list(L2,MinY), MinX1 is MinX + 1, MinX1 > MinY |
                            remove_lower(MinX1,L2,L3),
                            X lt Y, X in L1, Y in L3.                            
lt @ X lt Y, X in L1, Y in L2 <=> 
                            max_list(L1,MaxX), max_list(L2,MaxY), MaxY1 is MaxY - 1, MaxY1 < MaxX |
                            remove_higher(MaxY1,L1,L3),
                            X lt Y, X in L3, Y in L2.

eq @ X eq Y, X in L1, Y in L2 <=> intersection(L1, L2, L3), L3 \= [] | 
     X eq Y, X in L3, Y in L3.

ne @ X in [V] \ X ne Y, Y in D <=> member(V,D) | X ne Y, select(V,D,D1), Y in D1.
ne @ Y in [V] \ X ne Y, X in C <=> member(V,C) | X ne Y,  select(V,C,C1), X in C1.

% Addition, Multiplication -------------------------------

% interaction with addition
% no backpropagation yet!

add(X,Y,Z), X in L1, Y in L2 ==> all_addition(L1,L2,L3), Z in L3.

% interaction with multiplication
% no backpropagation yet!

mult(X,Y,Z), X in L1, Y in L2 ==> all_multiplication(L1,L2,L3), Z in L3.

% Labeling --------------------------------------------------------

enum([]) <=> true.
enum([X|L]) <=> indomain(X), enum(L).
indomain(X), X in [V|L] <=> L=[_|_] |
    (   X in [V]
    ;   X in L, indomain(X)).

% auxiliary predicates =============================================

%% filter_max removes all values from a list that
%% are larger than any value in another list.
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

all_multiplication(L1,L2,L3) :-
	setof(Z, X^Y^(member(X,L1), member(Y,L2), Z is X * Y), L3).

%% Examples

%% ?- X le Y, X in [4,6,7], Y in [3,7].
%@ $VAR(Y)in[7],
%@ $VAR(X)in[4,6,7],
%@ $VAR(X)le$VAR(Y).

%% ?- X le Y, X in [2,3,4,5], Y in [1,2,3].
%@ $VAR(Y)in[2,3],
%@ $VAR(X)in[2,3],
%@ $VAR(X)le$VAR(Y).

%% ?- X le Y, X in [2,3,4], Y in [0,1].
%@ false.

%% For enumeration domains, each value in the domain (implemented as a list)
%% is tried. X=V is expressed as X in [V] in this solver program.

%% TODO: make unique solutions visible as bindings.

%% ?- X in [2,3,4], enum([X]).
%@ $VAR(X)in[2] ;
%@ $VAR(X)in[3] ;
%@ $VAR(X)in[4],
%@ indomain($VAR(X)).


% EXAMPLE ==========================================================

%% ?- X in [1,2,3,4,5,6,7], Y in [2,4,6,7,8,0], Y lt X, X in 4..9, X ne Y, 
%%    add(X,Y,Z), mult(X,Y,Z).
%@ $VAR(Z)in[8,10,12],
%@ $VAR(X)in[4,5,6,7],
%@ $VAR(Y)in[2,4,6,0],
%@ $VAR(X)ne$VAR(Y),
%@ $VAR(Y)lt$VAR(X),
%@ add($VAR(X),$VAR(Y),$VAR(Z)),
%@ mult($VAR(X),$VAR(Y),$VAR(Z)).

%% ?- X in [1,2,3,4,5,6,7], Y in [2,4,6,7,8,0], Y lt X, X in 4..9, X ne Y, 
%%    add(X,Y,Z), mult(X,Y,Z), enum([X,Y]).
%@ false.
