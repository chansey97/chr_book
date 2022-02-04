%% 2.2.5 Exchange sort

:- use_module(library(chr)).

:- chr_constraint a/2, trig/0.

trig \ a(I,V), a(J,W) <=> I>J, V<W | a(I,W), a(J,V).

%% ?-  a(0,1), a(1,5), a(3,7), a(4,9), a(2,10), trig.
%@ a(2,7),
%@ a(3,9),
%@ a(4,10),
%@ a(1,5),
%@ a(0,1),
%@ trig.
