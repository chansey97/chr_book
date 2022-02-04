%% 6.3.1 Prolog and constraint logic programming

:- use_module(library(chr)).

:- chr_constraint appendo/3,new/2.

%% appraoch 1

%% r1 @ appendo([],L1,L2) <=> L1=L2.
%% r2 @ appendo([X1|L1],Y,[X2|L2]) <=> X1=X2, appendo(L1,Y,L2).

%?- appendo([],[1,2,3],L).
%@ L = [1,2,3].

%% approach 2

r1 @ appendo([],L,L) <=> true.
r2 @ appendo([X|L1],Y,[X|L2]) <=> appendo(L1,Y,L2).

%?- appendo([],[1,2,3],L).
%@ appendo([],[1,2,3],$VAR(L)).

%% N.B. Both approach 1 and 2 are wrong, because
%% 1. CHR is single side unification.
%% 2. CHR is committed-choice language instead of nondeterminism-choice language.

