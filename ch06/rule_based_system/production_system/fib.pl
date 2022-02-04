%% 6.1.1 Production rule systems

:- use_module(library(chr)).

:- chr_constraint limit/1, fibonacci/3.

%% (p next-fib
%%    (limit ^is <limit>)
%%    {(fibonacci ^index {<i> <= <limit>}
%%                ^this-value <v1>
%%                ^last-value <v2>) <fib>}
%%    -->
%%    (modify <fib> ^index (compute <i> + 1)
%%            ^this-value (compute <v1> + <v2>)
%%            ^last-value <v1>)
%%    (write (crlf) Fib <i> is <v1>)
%%    )

next-fib @ limit(Limit) \ fibonacci(I,V1,V2) <=> I =< Limit |
           I1 is I+1, V12 is V1+V2, fibonacci(I1,V12,V1), write(fib(I,V1)), nl.

%% f0 = 1, f1 = 1, f10 = 89
%?- limit(10), fibonacci(1,1,1).
%@ fib(10,89)
%@ fib(9,55)
%@ fib(8,34)
%@ fib(7,21)
%@ fib(6,13)
%@ fib(5,8)
%@ fib(4,5)
%@ fib(3,3)
%@ fib(2,2)
%@ fib(1,1)
%@ limit(10),
%@ fibonacci(11,144,89).


