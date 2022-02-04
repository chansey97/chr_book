%% 6.3.1 Prolog and constraint logic programming

%% with the corresponding typical textbook program in Prolog that has to rely
%% on a list of visited nodes and negation-as-failure (not) to find the shortest
%% path:

p(From,To,Path,N) :- e(From,To,N).
p(From,To,Path,N) :-
  e(From,Via,1),
  \+ member(Via,Path),
  p(Via,To,[Via|Path],N1),
  N is N1+1.

shortestp(From,To,N) :-
  p(From,To,[],N),
  \+ (p(From,To,[],N1),N1<N).

%% e(a,b,1).
%% e(b,c,1).
%% e(c,d,1).
%% e(d,e,1).

%?- shortestp(a, e, N).
%@ N = 4 ;
%@ false.

%?- shortestp(From, To, N).
%@ From = a,
%@ To = b,
%@ N = 1 ;
%@ From = b,
%@ To = c,
%@ N = 1 ;
%@ From = c,
%@ To = d,
%@ N = 1 ;
%@ From = d,
%@ To = e,
%@ N = 1 ;
%@ From = a,
%@ To = c,
%@ N = 2 ;
%@ From = a,
%@ To = d,
%@ N = 3 ;
%@ From = a,
%@ To = e,
%@ N = 4 ;
%@ From = b,
%@ To = d,
%@ N = 2 ;
%@ From = b,
%@ To = e,
%@ N = 3 ;
%@ From = c,
%@ To = e,
%@ N = 2 ;
%@ false.

%% OK for circle
%% e(a,b,1).
%% e(b,a,1).
%% e(b,d,1).

%?- shortestp(From, To, N).
%@ From = a,
%@ To = b,
%@ N = 1 ;
%@ From = b,
%@ To = a,
%@ N = 1 ;
%@ From = b,
%@ To = d,
%@ N = 1 ;
%@ From = To, To = a,
%@ N = 2 ;
%@ From = a,
%@ To = d,
%@ N = 2 ;
%@ From = To, To = b,
%@ N = 2 ;
%@ false.
