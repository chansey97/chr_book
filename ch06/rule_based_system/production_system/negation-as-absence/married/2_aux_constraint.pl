%% 6.1.2 Negation-as-absence

:- use_module(library(chr)).

:- chr_constraint person/1, married/1, single/1, check/1.

%% (p default
%%    (person ^name <x>)
%%    -(married ^name <x>)
%%    -->
%%    (make single ^name <x>)
%%    )

person(X) ==> check(person(X)).
married(X) \ check(person(X)) <=> true.
person(X) \ check(person(X)) <=> single(X).

%?- person(linda), married(linda).
%@ person(linda),
%@ married(linda),
%@ single(linda).

%?- married(linda), person(linda).
%@ person(linda),
%@ married(linda).
