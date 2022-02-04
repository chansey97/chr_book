%% 6.3.1 Prolog and constraint logic programming

%% An example of don't care nondeterminism (committed-choice) in prolog.

%% The typical Prolog program for the prime sieve is more contrived, because
%% one explicitly has to maintain a list of prime candidates and use the cut (!)
%% operator to commit to the current clause.

primes(N,Ps):- upto(2,N,Ns), sift(Ns,Ps).

upto(F,T,[]):- F>T, !.
upto(F,T,[F|Ns1]):- F1 is F+1, upto(F1,T,Ns1).

sift([],[]).
sift([P|Ns],[P|Ps1]):- filter(Ns,P,Ns1), sift(Ns1,Ps1).

filter([],P,[]).
filter([X|In],P,Out):- X mod P =:= 0, !, filter(In,P,Out).
filter([X|In],P,[X|Out1]):- filter(In,P,Out1).

%?- primes(10,Ps).
%@ Ps = [2,3,5,7].

%?- upto(2,10,Ps).
%@ Ps = [2,3,4,5,6,7,8,9,10].

%?- sift([2,3,4,5,6,7,8,9,10],Ps).
%@ Ps = [2,3,5,7].

%?- filter([3,4,5,6,7,8,9,10],2,Ns1).
%@ Ns1 = [3,5,7,9].

%?- sift([3,5,7,9],Ps1).
%@ Ps1 = [3,5,7].

%?- filter([5,7,9],3,Ns1).
%@ Ns1 = [5,7].

%?- filter([7],5,Ns1).
%@ Ns1 = [7].





