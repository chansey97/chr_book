%% 2.2.1 Minimum
%% Abstract semantics.

%% In abstract semantics,
%% the query min(A), min(B), A=<B will reduce to min(A), A=<B
%% The rule below does not help, because `A=<B` contains variables,
%% min(N) \ min(M) <=> ground(N), ground(M), N=<M | true.

%% Attempt to simulate abstract semantics by clpr.

:- use_module(library(chr)).
:- use_module(library(clpr)).

:- chr_constraint min/1, trig/0.

trig, min(N) \ min(M) <=> entailed(N<M) | true.
trig, min(N) \ min(M) <=> entailed(N=M) | true.
trig <=> true.

%% N.B.

%% 1. We must use entailed(N=<M) instead of N=<N in the guard, for example:

%% ?- entailed(N=<M).
%@ false.

%% ?- N=<M
%@ {$VAR(A)=<1.0}.

%% > In general, under the abstract operational semantics of CHR, even though not necessarily
%% in a given implementation, the guard is made out of built-in constraints that hold if
%% they are logically implied by the current store.

%% So in guard, we must use entailed to test the store instead of attach constraints. It is
%% something like closed-world assumption. Not that clpfd even has no entailed predicate!

%% 2. It doesn't work if no trig, since the wake-up policy of the current implementation
%% doesn't seem consider builit-in constraints other than =. For example, the newly
%% added constraint {A=<B} further constrains variables A and B, but doesn't wake the
%% constraint min. see https://swi-prolog.discourse.group/t/interaction-between-chr-and-clp-r/4982

%% In addition, even if we have trig, when some propagation rule's body has a {N=<M}, 
%% it still can not wake the constrains related to N and M. It is very sad,
%% see clpr implementation of lexicographic_order_global_constraint (2_add_constraint_propagation_l4.pl).

%% The conclusion is
%% in general, clpr cannot simulate abstract semantics, although it is OK in this example.

%% TODO: any workaround?
%% 1. Find a way to let clpr constraints waken chr constraints
%% e.g. is there any chr predicate can reactive a chr constraint manually?
%% 2. Implement partial order relation that supports entailed check? (still require a wake-up mechanism)

%% Examples

%?- min(A), min(B), trig.
%@ min($VAR(B)),
%@ min($VAR(A)).

%?- min(A), min(B), A=1, B=2, trig.
%@ A = 1,
%@ B = 2,
%@ min(1).

%% > Under the abstract semantics, it turns out that the two rules are weaker than
%% the single initial rule. Consider the previous examples. Most of them still
%% work, but the query min(A), min(B), A=<B will not reduce at all, because
%% the built-in constraint A=<B is too weak to imply one of the guards of the
%% two rules, A<B or A=B. We say that these two programs are not operationally
%% equivalent. Operational equivalence is discussed in Section 5.5.1. However,
%% the programs are logically equivalent. The logical reading of rules as formulas
%% is the declarative semantics as discussed in Section 3.4.

%?- min(A), min(B), {A=<B}, trig.
%@ min($VAR(B)),
%@ min($VAR(A)),
%@ {$VAR(B)= $VAR(A)+ $VAR(_A),$VAR(_A)>=0.0}.

