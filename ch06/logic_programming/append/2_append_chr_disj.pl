%% 6.3.1 Prolog and constraint logic programming

%% CHR with disjunction (CHR∨, cf. Section 3.3.2) provides a declarative
%% formulation of and a clear distinction between don’t-care nondeterminism
%% and don’t-know nondeterminism. Any Horn clause (CLP) program can be
%% converted into an equivalent CHR∨ program. One moves CLP head unification
%% and clause choices to the body of a single CHR∨ rule.

:- use_module(library(chr)).

:- chr_constraint appendo/3.

%% append([],L,L) ← true.
%% append([X|L1],Y,[X|L2]) ← append(L1,Y,L2).

%% The translated CHR∨ program for append is:

appendo(X,Y,Z) <=> (
                    X=[], Y=L, Z=L
                  ; X=[H|L1], Y=L2 ,Z=[H|L3], appendo(L1, L2, L3)
                   ).

%?- appendo([1,2,3],[],L).
%@ L = [1,2,3] ;
%@ false.

%?- appendo([],[1,2,3],L).
%@ L = [1,2,3] ;
%@ false.

%?- appendo([1,2],[3,4,5],L).
%@ L = [1,2,3,4,5] ;
%@ false.

%?- appendo([1,2],L,[1,2,3,4,5]).
%@ L = [3,4,5] ;
%@ false.

%?- appendo(L,[3,4,5],[1,2,3,4,5]).
%@ L = [1,2] ;
%@ false.

%?- appendo(L,M,[1,2,3]).
%@ L = [],
%@ M = [1,2,3] ;
%@ L = [1],
%@ M = [2,3] ;
%@ L = [1,2],
%@ M = [3] ;
%@ L = [1,2,3],
%@ M = [] ;
%@ false.

%?- appendo(L,M,O).
%@ L = [],
%@ M = $VAR(O) ;
%@ L = [$VAR(_A)],
%@ O = [$VAR(_A)|$VAR(M)] ;
%@ L = [$VAR(_A),$VAR(_B)],
%@ O = [$VAR(_A),$VAR(_B)|$VAR(M)] ;
%@ L = [$VAR(_A),$VAR(_B),$VAR(_C)],
%@ O = [$VAR(_A),$VAR(_B),$VAR(_C)|$VAR(M)] ;
%@ L = [$VAR(_A),$VAR(_B),$VAR(_C),$VAR(_D)],
%@ O = [$VAR(_A),$VAR(_B),$VAR(_C),$VAR(_D)|$VAR(M)] ;
%@ L = [$VAR(_A),$VAR(_B),$VAR(_C),$VAR(_D),$VAR(_E)],
%@ O = [$VAR(_A),$VAR(_B),$VAR(_C),$VAR(_D),$VAR(_E)|$VAR(M)] ;