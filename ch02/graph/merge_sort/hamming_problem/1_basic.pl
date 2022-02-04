%% 2.5 Exercises
%% 2.25

:- use_module(library(chr)).

:- op(600, xfx, →).

:- chr_constraint
  (→)/2, hamming/1, next/1.

%% Merge sorting
A→A <=> true.
A→B \ A→B <=> true.
A→B \ A→C <=> A<B,B<C | B→C.

%% Hamming
hamming(X) <=> X1 is X*2, X2 is X*3, X3 is X*5, X→X1, X→X2, X→X3, next(X).
X→A \ next(X) <=> writeln(X), hamming(A).

