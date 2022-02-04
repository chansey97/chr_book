%% 2.4.4 Ordered merging and sorting

:- use_module(library(chr)).

:- op(600, xfx, →).

:- chr_constraint (→)/2,hamming/1,next/1.

%% The rule 'A→B \ A→B <=> true.' can be made redundant when we slightly generalize
%% the guard of our initial merge rule: A→B \ A→C <=> A<B,B=<C | B→C.

%% A→A <=> true.
%% A→B \ A→B <=> true.
%% A→B \ A→C <=> A<B,B<C | B→C.

A→A <=> true.
A→B \ A→C <=> A<B,B=<C | B→C.

%?- 0→2, 0→5, 0→1, 0→7.
%@ 5→7,
%@ 1→2,
%@ 0→1,
%@ 2→5.