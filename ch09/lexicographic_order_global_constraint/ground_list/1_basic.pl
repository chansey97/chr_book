%% 9.2 Lexicographic order global constraint

%% We assuming with the same length.

:- use_module(library(chr)).

:- set_prolog_flag(double_quotes, codes).

:- op(700, xfx, lex).

:- chr_constraint lex/2.

l1     @ [] lex [] <=> true.
l2     @ [X|L1] lex [Y|L2] <=> X@<Y | true.
l3     @ [X|L1] lex [Y|L2] <=> X==Y | L1 lex L2.
l4     @ [X|L1] lex [Y|L2] ==> X@>Y | fail.

%% ?-"alpha" lex "alternative".
%@ true.

%% ?-"alternative" lex "alpha".
%@ false.

%% ?-"alpha" lex "alpha".
%@ true.

%% ?-"alpha" lex "alphabet".
%@ []lex[98,101,116]. % since different length

%% ?-"alphabet" lex "alpha".
%@ false.

%% ?-[1] lex [2].
%@ true.