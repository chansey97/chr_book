%% 6.2.1 Term rewriting systems

:- use_module(library(chr)).

:- op(600, xfx, eq).

:- chr_constraint eq/2, unflatten/0.

%% TRS:
%% 0+Y -> Y.
%% s(X)+Y -> s(X+Y).

%% N.B. We need `eq` instead of `=`, because `=` is built-in constraint
%% which cannot be manipulated in CHR.

T eq T1+T2, T1 eq 0, T2 eq Y <=> T eq Y.
T eq T1+T2, T1 eq s(T3), T3 eq X, T2 eq Y <=> T eq s(T4), T4 eq T5+T6, T5 eq X, T6 eq Y.

%% TRS: 0+s(0)
%% ------> T eq 0+s(0)
%% --cps-> T eq T1+T2, T1 eq 0, T2 eq s(0).
%% --cps-> T eq T1+T2, T1 eq 0, T2 eq s(T3), T3 eq 0.

%?- T eq T1+T2, T1 eq 0, T2 eq s(T3), T3 eq 0.
%@ $VAR(T3)eq 0,
%@ $VAR(T)eq s($VAR(T3)).


%% TRS: s(0)+s(0)
%% ------> T eq 0+1.
%% --cps-> T eq T1+T2, T1 eq s(T2), T2 eq 0, T2 eq s(T4), T4 eq 0.

%?- T eq T1+T2, T1 eq s(T2), T2 eq 0, T2 eq s(T4), T4 eq 0.
%@ $VAR(_A)eq 0,
%@ $VAR(_B)eq s($VAR(_A)),
%@ $VAR(T)eq s($VAR(_B)).

%% unflatten
unflatten @ unflatten \ T eq T1 <=> T = T1.

%?- T eq T1+T2, T1 eq s(T2), T2 eq 0, T2 eq s(T4), T4 eq 0, unflatten.
%@ T = s(s(0)),
%@ unflatten.

%% TODO: create cps transformation rule/predicate for term






