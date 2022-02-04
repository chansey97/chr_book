%% 6.2.3 Petri nets

%% Refined semantics is not fair...

%% TODO:
%% 1. Add printing
%% 2. test in Parallel-CHR?

:- use_module(library(chr)).

:- chr_constraint
  t1/0, t2/0, t3/0,
  f1/0, f2/0, f3/0,  
  e1/0, e2/0, e3/0.

%% Fig. 6.1. The three dining philosophers problem modeled as a Petri net

te1@ t1, f1, f2 <=> e1.
te2@ t2, f2, f3 <=> e2.
te3@ t3, f3, f1 <=> e3.
et1@ e1 <=> t1, f1, f2.
et2@ e2 <=> t2, f2, f3.
et3@ e3 <=> t3, f3, f1.

%?- t1,t2,t3,f1,f2,f3.
%% loop
