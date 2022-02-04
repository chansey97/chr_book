%% 8.2.2 Path consistency
%% Incomplete graphs.

%% 8.2.4 Temporal reasoning with path consistency

%% Simple-Temporal-Network (STN)
 
%% Handles constraints about the approximate position and difference of time 
%% points.

%% N.B. Path consistency is a complete technique making STN consistent.

:- module(time_point, [pos/3, dist/4, start/1]).

:- use_module(library(chr)).

:- chr_constraint

% pos(T1,P,T2) means: time point P is between time T1 and T2 (numbers)
    pos/3, 
% dist(T1,P1,P2,T2) means: distance of points P1 and P2 is between T1 and T2
    dist/4, 
% start(P) means: P is the origin, equivalent: pos(0,P,0)
    start/1.


start @             start(X)   <=> pos(0,X,0).

inconsistent @      pos(A,_,B) <=> A>B | fail.

%intersect @         pos(A,Y,B), pos(C,Y,D) <=> 
%                        AC is max(A,C), BD is min(B,D), pos(AC,Y,BD)
%                        pragma already_in_heads.  %important for termination!
%% avoid "pragma already_in_heads" through following two rules:

subset @            pos(A,Y,B) \ pos(C,Y,D) <=> 
                        A is max(A,C), B is min(B,D) | true.

intersect @         pos(A,Y,B), pos(C,Y,D) <=> 
                        AC is max(A,C), BD is min(B,D), pos(AC,Y,BD).

intersect_dist @    dist(A,X,Y,B), dist(C,X,Y,D) <=> 
                        AC is max(A,C), BD is min(B,D), dist(AC,X,Y,BD).

propagate_forward @ pos(A,Y,B), dist(C,Y,Z,D) ==> 
                        AC is A+C, BD is B+D, pos(AC,Z,BD).

propagate_backward@ pos(A,Y,B), dist(C,Z,Y,D) ==> 
                        AD is A-D, BC is B-C, pos(AD,Z,BC).

%% HOW TO USE
%% Time is represented by float numbers, time points by variables.#
%% The following constraints are used:#
%% Time point P is between time T1 and T2: *pos(T1,P,T2)* #
%% Distance of points P1 and P2 is between T1 and T2: *dist(T1,P1,P2,T2)* #
%% P is the origin: *start(P)* (equivalent: *pos(0,P,0)*)

%% SAMPLE QUERIES
%% ?- start(A), dist(4,A,B,7), dist(1,B,C,4), pos(10,C,13).
%@ pos(6,$VAR(B),7),
%@ pos(10,$VAR(C),11),
%@ pos(0,$VAR(A),0),
%@ dist(1,$VAR(B),$VAR(C),4),
%@ dist(4,$VAR(A),$VAR(B),7).

%% ?- start(A), dist(4,A,B,7), dist(1,B,C,2), pos(10,C,13).
%@ false.

%% ?- start(X), dist(3,X,Y,10), dist(4,Y,Z,5).
%@ pos(7,$VAR(Z),15),
%@ pos(3,$VAR(Y),10),
%@ pos(0,$VAR(X),0),
%@ dist(4,$VAR(Y),$VAR(Z),5),
%@ dist(3,$VAR(X),$VAR(Y),10).

%% ?- start(A), dist(14.3,A,B,17.5), dist(14.3,A,B,16.3), dist(12.2,B,C,14.5).
%@ pos(26.5,$VAR(C),30.8),
%@ pos(14.3,$VAR(B),16.3),
%@ pos(0,$VAR(A),0),
%@ dist(12.2,$VAR(B),$VAR(C),14.5),
%@ dist(14.3,$VAR(A),$VAR(B),16.3).

%% ?-  dist(3,X,Y,10), dist(2,Y,Z,5), dist(40,X,Z,50).
%@ dist(40,$VAR(X),$VAR(Z),50),
%@ dist(2,$VAR(Y),$VAR(Z),5),
%@ dist(3,$VAR(X),$VAR(Y),10).

%% Examples

%% If the two time points coincide, the distance must be 0.
%% ?- start(X), dist(1,X,X,1).
%@ false.

%% https://youtu.be/NOtSLqIawk8?t=3099
%% "I got up at 6 o’clock. I read newspapers for 30 minutes during the breakfast (at least one minuts to take breakfast).
%% After the breakfast I walked to my office which took me 1 hour. I entered the office at 8:00 a.m."

%% ?- start(t0), dist(360,t0,bs,99999), dist(1,bs,be,99999), dist(0,bs,rs,99999), dist(30,rs,re,30),dist(0,re,be,99999),
%%    dist(0,be,ws,0), dist(60,ws,we,60), dist(480,t0,we,480).
%@ pos(360,bs,390),
%@ pos(360,rs,390),
%@ pos(390,re,420),
%@ pos(420,be,420),
%@ pos(420,ws,420),
%@ pos(480,we,480),
%@ pos(0,t0,0),
%@ dist(480,t0,we,480),
%@ dist(60,ws,we,60),
%@ dist(0,be,ws,0),
%@ dist(0,re,be,99999),
%@ dist(30,rs,re,30),
%@ dist(0,bs,rs,99999),
%@ dist(1,bs,be,99999),
%@ dist(360,t0,bs,99999).

%% Q: Does this network consistent?
%% A: Yes.

%% Q: When did I start my breakfast?
%% A: I started my breakfast between 6:00AM and 6:30AM, because
%% because 360 =< bs =< 390
