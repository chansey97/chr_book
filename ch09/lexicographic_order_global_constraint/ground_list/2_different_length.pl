%% 9.2 Lexicographic order global constraint

%% Work with different length.

:- use_module(library(chr)).

:- set_prolog_flag(double_quotes, codes).

:- op(700, xfx, lex).

:- chr_constraint lex/2.

l1     @ [] lex _ <=> true.
l1_len @ [_|_]  lex []      <=> fail.
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
%@ true.

%% ?-"alphabet" lex "alpha".
%@ false.

%% ?-[1] lex [2].
%@ true.