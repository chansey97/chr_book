%% 6.3.1 Prolog and constraint logic programming

p(From,To,Path,N) :- e(From,To,N).
p(From,To,Path,N) :-
  e(From,Via,1),
  \+ member(Via,Path),
  p(Via,To,[Via|Path],N1),
  N is N1+1.

shortestp(From,To,N) :-
  p(From,To,[],N),
  %% \+ (p(From,To,[],N1),N1<N), %% wrong, if comment this line
  true.

%% e(a,z,1).

%% e(a,b,1).
%% e(b,z,1).

%% e(a,c,1).
%% e(c,d,1).
%% e(d,z,1).

e(a,c,1).
e(c,d,1).
e(d,z,1).

e(a,b,1).
e(b,z,1).

e(a,z,1).

%?- p(a,z,[],N).
%@ N = 1 ;
%@ N = 2 ;
%@ N = 3 ;
%@ false.

%?- shortestp(a,z,N).
%@ N = 1 ;
%@ N = 2 ;
%@ N = 3 ;
%@ false.







