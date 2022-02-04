%% 6.3.1 Prolog and constraint logic programming

%% An example of don't know nondeterminism in prolog.

appendo([],L,L) :- true.
appendo([X|L1],Y,[X|L2]) :- appendo(L1,Y,L2).

%?- appendo([1,2,3],[],L).
%@ L = [1,2,3].

%?- appendo([],[1,2,3],L).
%@ L = [1,2,3].

%?- appendo([1,2],[3,4,5],L).
%@ L = [1,2,3,4,5].

%?- appendo([1,2],L,[1,2,3,4,5]).
%@ L = [3,4,5].

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