%% 8.2.3 Finite domain arc consistency

:- use_module(library(chr)).

:- use_module( library(lists), 
               [member/2,memberchk/2,select/3,
                last/2,is_list/1,min_list/2, max_list/2,
                remove_duplicates/2]).

% for domain constraints
:- op(700, xfx, in).

% for inequality constraints
:- op(700, xfx, eq).

:- chr_constraint in/2, eq/2, enum/1, indomain/1.

inconsistency @ X in [] <=> false.

% intersection of domains for the same variable

intersect@ X in L1, X in L2 <=> intersection(L1,L2,L3) | X in L3.

% Inequality -------------------------------

eq @ X eq Y, X in L1, Y in L2 <=> intersection(L1, L2, L3), L3 \= [] | 
     X eq Y, X in L3, Y in L3.

% Labeling --------------------------------------------------------

enum([]) <=> true.
enum([X|L]) <=> indomain(X), enum(L).
indomain(X), X in [V|L] <=> L=[_|_] |
    (   X in [V]
    ;   X in L, indomain(X)).

% auxiliary predicates =============================================

intersection([], _, []).
intersection([Head|L1tail], L2, L3) :-
        memberchk(Head, L2),
        !,
        L3 = [Head|L3tail],
        intersection(L1tail, L2, L3tail).
intersection([_|L1tail], L2, L3) :-
        intersection(L1tail, L2, L3).

%% Examples

%% For enumeration domains, each value in the domain (implemented as a list)
%% is tried. X=V is expressed as X in [V] in this solver program.

%% TODO: make unique solutions visible as bindings.

%% ?- X in [2,3,4], enum([X]).
%@ $VAR(X)in[2] ;
%@ $VAR(X)in[3] ;
%@ $VAR(X)in[4],
%@ indomain($VAR(X)).
