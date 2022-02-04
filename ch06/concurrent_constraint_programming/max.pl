%% 6.3.2 Concurrent constraint programming

:- use_module(library(chr)).

:- chr_constraint max/3, ask/1.

:- op(800, xfx, →).

%% We concentrate on the committed-choice fragment of the CC language

%% CC language
%% max(X,Y,Z) ← (X=<Y → Y=Z) + (Y=<X → X=Z).

%% CHR translation
max(X,Y,Z) <=> ask((X=<Y → Y=Z) + (Y=<X → X=Z)).

ask((X=<Y → Y=Z) + (Y=<X → X=Z)) <=> X=<Y | Y=Z.
ask((X=<Y → Y=Z) + (Y=<X → X=Z)) <=> Y=<X | X=Z.

%?- max(1,2,Z).
%@ Z = 2.
