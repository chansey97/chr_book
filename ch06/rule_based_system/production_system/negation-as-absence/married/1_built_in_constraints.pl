%% 6.1.2 Negation-as-absence

:- use_module(library(chr)).

:- chr_constraint person/1, married/1, single/1.

%% (p default
%%    (person ^name <x>)
%%    -(married ^name <x>)
%%    -->
%%    (make single ^name <x>)
%%    )

default @ person(X) ==> \+ find_chr_constraint(married(X)) | single(X).

%?- person(linda),married(linda).
%@ person(linda),
%@ married(linda),
%@ single(linda).

%?- married(linda), person(linda).
%@ person(linda),
%@ married(linda).
