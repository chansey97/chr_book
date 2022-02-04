%% 8.2.3 Finite domain arc consistency

:- use_module(library(chr)).

% for domain constraints
:- op(700, xfx, in).
%% :- op(600, xfx, ':'). % operator already defined in SWI Prolog

:- op(700, xfx, le).
:- op(700, xfx, eq).
:- op(700, xfx, ne).

:- chr_constraint (in)/2, le/2, eq/2, ne/2, add/3, mult/3, enum/1, indomain/1.
  
% X in Min:Max - X is between the numbers Min and Max, inclusively
% X must always be a unbound variable (!), and Min and Max evaluable
% (i.e. ground) arithmetic expressions (or numbers)

%% conveniently compute arithmetic expressions (e.g. min, max) in domain
compute @ X in A:B <=> \+ (number(A),number(B)) | C is A, D is B, X in C:D.

inconsistency @ X in A:B <=> A>B | false.
redundant @ X in A:B \ X in C:D <=> C=<A, B=<D | true.
intersection @ X in A:B , X in C:D <=>  X in max(A,C):min(B,D).

% Inequality -------------------------------

%% TODO: Why not use `\`, e.g.
%% le @ X le Y \ X in A:B, Y in C:D <=> B>D |
%%               X in A:D, Y in C:D.
%% le @ X le Y \ X in A:B, Y in C:D <=> C<A |
%%               X in A:B, Y in A:D.
%% eq @ X eq Y \ X in A:B, Y in C:D <=> A=\=C |
%%               X in max(A,C):B, Y in max(C,A):D.
%% eq @ X eq Y \ X in A:B, Y in C:D <=> B=\=D |
%%               X in A:min(B,D), Y in C:min(D,B).

%% N.B. Because of arc consistency, an atomic constraint has semantically a pair of symmetry rules. 

le @ X le Y, X in A:B, Y in C:D <=> B>D |
     X le Y, X in A:D, Y in C:D.
le @ X le Y, X in A:B, Y in C:D <=> C<A |
     X le Y, X in A:B, Y in A:D.
eq @ X eq Y, X in A:B, Y in C:D <=> A=\=C |
     X eq Y, X in max(A,C):B, Y in max(C,A):D.
eq @ X eq Y, X in A:B, Y in C:D <=> B=\=D |
     X eq Y, X in A:min(B,D), Y in C:min(D,B).

%% The ne constraint can only cause a domain tightening if one of the intervals
%% denote a unique value that happens to be the bound of the other intervals.
%% TODO: What happen if (A+1) out of bound?
ne @ X ne Y, X in A:B, Y in C:D <=> A==C,C==D |
     X ne Y, X in (A+1):B, Y in C:D.
ne @ X ne Y, X in A:B, Y in C:D <=> B==C,C==D |
     X ne Y, X in A:(B-1), Y in C:D.
ne @ X ne Y, X in A:B, Y in C:D <=> C==A,A==B |
     X ne Y, Y in (C+1):D, X in A:B.
ne @ X ne Y, X in A:B, Y in C:D <=> D==A,A==B |
     X ne Y, Y in C:(D-1), X in A:B.

% Addition X+Y=Z -------------------------------

add @ add(X,Y,Z), X in A:B, Y in C:D, Z in E:F <=>
        \+ (A>=E-D,B=<F-C,C>=E-B,D=<F-A,E>=A+C,F=<B+D) |
      add(X,Y,Z),
      X in max(A,E-D):min(B,F-C),
      Y in max(C,E-B):min(D,F-A),
      Z in max(E,A+C):min(F,B+D).

%% % Labeling --------------------------------------------------------

enum([]) <=> true.
enum([X|L]) <=> indomain(X), enum(L).
indomain(X), X in A:B <=> A<B |
    (   X in A:((A+B)//2), indomain(X)
    ;   X in ((A+B)//2+1):B, indomain(X)).

%% Examples

%% ?- U in 2:3, V in 1:2, U le V.
%@ $VAR(V)in 2:2,
%@ $VAR(U)in 2:2,
%@ $VAR(U)le$VAR(V).

%% ?- U in 1:3, V in 2:4, W in 0:4, add(U,V,W).
%@ $VAR(W)in 3:4,
%@ $VAR(V)in 2:3,
%@ $VAR(U)in 1:2,
%@ add($VAR(U),$VAR(V),$VAR(W)).

%% ?- U in 1:1, V in 1:1, ne(U,V).
%@ false.

%% ?- U in 1:5, V in 3:7, ne(U,V).
%@ $VAR(V)in 3:7,
%@ $VAR(U)in 1:5,
%@ $VAR(U)ne$VAR(V).

%% ?- U in 1:5, V in 1:1, ne(U,V).
%@ $VAR(V)in 1:1,
%@ $VAR(U)in 2:5,
%@ $VAR(U)ne$VAR(V).

%% N.B. X=V is expressed as X in X in 1:1 in this solver program.
%% TODO: make unique solutions visible as bindings.
%% ?- U in 1:5, enum([U]).
%@ $VAR(U)in 1:1,
%@ indomain($VAR(U)) ;
%@ $VAR(U)in 2:2,
%@ indomain($VAR(U)) ;
%@ $VAR(U)in 3:3,
%@ indomain($VAR(U)) ;
%@ $VAR(U)in 4:4,
%@ indomain($VAR(U)) ;
%@ $VAR(U)in 5:5,
%@ indomain($VAR(U)).

%% N.B. The current implementation doesn't handle single point, e.g. V=1.
%% ?- U in 1:5, V=1, ne(U,V).
%@ V = 1,
%@ $VAR(U)in 1:5,
%@ $VAR(U)ne 1.

%% N.B. The current implementation doesn't support disjoint domain (e.g. multi-interval arithmetic)
%% ?- U in 1:5, V in 3:3, ne(U,V).
%@ $VAR(V)in 3:3,
%@ $VAR(U)in 1:5,
%@ $VAR(U)ne$VAR(V).

%% TODO: eliminate le if the domains of U and V are disjoint
%% ?- U in 1:2, V in 3:4, le(U,V).
%@ $VAR(V)in 3:4,
%@ $VAR(U)in 1:2,
%@ $VAR(U)le$VAR(V).

%% TODO: eliminate ne if the domains of U and V are disjoint
%% ?- U in 1:2, V in 3:4, ne(U,V).
%@ $VAR(V)in 3:4,
%@ $VAR(U)in 1:2,
%@ $VAR(U)ne$VAR(V).

%% Arc consistency does not imply satisfiability.

%% ?- X in 1:2, Y in 1:2, X ne Y, X eq Y.
%@ $VAR(Y)in 1:2,
%@ $VAR(X)in 1:2,
%@ $VAR(X)eq$VAR(Y),
%@ $VAR(X)ne$VAR(Y).

%% ?- X in 1:2, Y in 1:2, X ne Y, X eq Y, enum([X,Y]).
%@ false.

%% ?- U in 1:2, V in 2:3, le(U,V), U=3.
%@ U = 3,
%@ $VAR(V)in 2:3,
%@ 3 in 1:2,
%@ 3 le $VAR(V).

%% N.B. Cannot interact with `=`, use `in` and `eq` instead.

%% ?- X in 1:2, X=1, enum([X]).
%@ X = 1,
%@ 1 in 1:1,
%@ indomain(1) ;
%@ X = 1,
%@ 1 in 2:2,
%@ indomain(1).

%% ?- X in 1:2, X in 1:1, enum([X]).
%@ $VAR(X)in 1:1,
%@ indomain($VAR(X)).


% EXAMPLES ================================================================

%% ?- X in 3:5,X in 2:4.
%@ $VAR(X)in 3:4.

%% ?- X=Y, X in 3:5, Y in 2:4.
%@ X = $VAR(Y),
%@ $VAR(Y)in 3:4.

%% ?- X in 3:5, Y in 2:4, X=Y.
%@ X = $VAR(Y),
%@ $VAR(Y)in 3:4.

%% ?- X in 3:3.
%@ $VAR(X)in 3:3.

%% ?- X le Y, X in 3:5,X in 2:4.
%@ $VAR(X)in 3:4,
%@ $VAR(X)le$VAR(Y).

%% ?- X le Y, X in 3:5, Y in 3:5.
%@ $VAR(Y)in 3:5,
%@ $VAR(X)in 3:5,
%@ $VAR(X)le$VAR(Y).

%% ?- X le Y, X in 3:5, Y in 2:4.
%@ $VAR(Y)in 3:4,
%@ $VAR(X)in 3:4,
%@ $VAR(X)le$VAR(Y).

%% ?- add(X,Y,Z), X in 2:5, Y in 3:4, Z in 1:7.
%@ $VAR(Z)in 5:7,
%@ $VAR(Y)in 3:4,
%@ $VAR(X)in 2:4,
%@ add($VAR(X),$VAR(Y),$VAR(Z)).
