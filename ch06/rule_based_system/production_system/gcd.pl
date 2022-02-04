%% 6.1.1 Production rule systems

:- use_module(library(chr)).

:- chr_constraint euclidean_pair/2.

%% (p done-no-divisors
%%    (euclidean-pair ^first <first> ^second 1) -->
%%    (write GCD is 1) (halt) )

%% (p found-gcd
%%    (euclidean-pair ^first <first> ^second <first>) -->
%%    (write GCD is <first>) (halt) )

%% (p switch-pair
%%    {(euclidean-pair ^first <first> ^second { <second> > <first>} )
%%    <e-pair>} -->
%%    (modify <e-pair> ^first <second> ^second <first>)
%%    (write <first> -- <second> (crlf)) )

%% (p reduce-pair
%%    {(euclidean-pair ^first <first> ^second { <second> < <first> } )
%%    <e-pair>} -->
%%    (modify <e-pair> ^first (compute <first> - <second>))
%%    (write <first> -- <second> (crlf)) )

done-no-divisors @ euclidean_pair(First, 1) <=>
        write(gcd is 1).

found-gcd @ euclidean_pair(First, First) <=>
        write(gcd is First).

switch-pair @ euclidean_pair(First, Second) <=>
        Second > First |
        euclidean_pair(Second, First),
        N is Second - First , write(N), nl.

reduce-pair @ euclidean_pair(First, Second) <=>
        Second < First |
        N is First - Second,
        euclidean_pair(N, Second),
        write(N), nl.

%?- euclidean_pair(150,200).
%@ gcd is 50
%@ 50
%@ 100
%@ 100
%@ 50
%@ 50
%@ true.

