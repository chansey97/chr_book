%% 1.1.1 Propositional rules
%% Example 1.1.2 (Walk)

%% simplify a given walk to one with a minimal number of steps
%% that reaches the same position.

%% vector addition is commutative, just like addition of real numbers.

:- use_module(library(chr)).

:- chr_constraint left/0, right/0, forward/0, backward/0.

left, right <=> true.
forward, backward <=> true.

%?- left, forward, right, right, forward, forward, backward, left, left.
%@ left,
%@ forward,
%@ forward.
