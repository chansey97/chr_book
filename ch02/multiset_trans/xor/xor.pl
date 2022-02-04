%% 2.2.2 Boolean Exclusive Or

:- use_module(library(chr)).

:- chr_constraint xor/1.

%% xor(0), xor(0) <=> xor(0).
%% xor(0), xor(1) <=> xor(1).
%% xor(1), xor(0) <=> xor(1).
%% xor(1), xor(1) <=> xor(0).

xor(X), xor(X) <=> xor(0).
xor(1) \ xor(0) <=> true.

%?- xor(1), xor(1).
%@ xor(0).

%?- xor(1), xor(0).
%@ xor(1).

%?- xor(0), xor(1).
%@ xor(1).

%% xor is confluence
%?- xor(1), xor(1), xor(0).
%@ xor(0).