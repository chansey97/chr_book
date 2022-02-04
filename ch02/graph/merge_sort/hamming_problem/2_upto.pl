%% 2.5 Exercises
%% 2.25

:- use_module(library(chr)).

:- op(600, xfx, →).

:- chr_constraint
  (→)/2, hamming/1, next/1, upto/2,
  %% cont can be used for debuuging (like break point!)
  cont/0.

%% Merge sorting
A→A <=> true.
A→B \ A→B <=> true.
A→B \ A→C <=> A<B,B<C | B→C.

%% Hamming
hamming(X) /*, cont*/ <=> X1 is X*2, X2 is X*3, X3 is X*5, X→X1, X→X2, X→X3, next(X).
X→A \ next(X), upto(N,Max) <=> N+1 =< Max | writeln(X), N1 is N+1, upto(N1, Max), hamming(A).

%% 关键是 X→X1, X→X2, X→X3 这3个 goals, 会自动 merge sorting，
%% 接下来 next(X)，虽然 X 是头，但是它不是会分叉成三个，而是在下面那条 rule 里匹配 到A which is X 的下一个节点，
%% 它就是不断生成然后 merge，然后 focus position 向前移动的过程。

%% ?- hamming(1), upto(0,10).
%@ 1
%@ 2
%@ 3
%@ 4
%@ 5
%@ 6
%@ 8
%@ 9
%@ 10
%@ 12
%@ 60→75,
%@ 50→60,
%@ 36→40,
%@ 30→36,
%@ 45→50,
%@ 40→45,
%@ 27→30,
%@ 25→27,
%@ 24→25,
%@ 20→24,
%@ 16→18,
%@ 15→16,
%@ 18→20,
%@ 12→15,
%@ 10→12,
%@ 8→9,
%@ 6→8,
%@ 9→10,
%@ 5→6,
%@ 4→5,
%@ 3→4,
%@ 2→3,
%@ 1→2,
%@ next(15),
%@ upto(10,10).

