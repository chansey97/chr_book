%% 6.1.4 Event–condition–action rules

:- use_module(library(chr)).

:- chr_constraint t/1, insert/1, delete/1, update/2.

%% update(emp(Name,S1),emp(Name,S2)) <=> S2>S1*(1+0.1) | S3 is S1*1.1, update(emp(Name,S1),emp(Name,S3)).

ins_dup @ t(C) \ insert(C) <=> true.
upd_dup @ update(C,C) <=> true. %% 相当于posgres RETURN NULL

ins @ insert(C) ==> t(C).
del @ delete(P) \ t(C) <=> match(P,C) | true.
upd @ update(P,C1) \ t(C) <=> match(P,C), C1 \= C2 | t(C1).

insert(_) <=> true.
delete(_) <=> true.
update(_,_) <=> true.

match(P,C) :- P == C. 

%?- insert(emp(alice,10)), insert(emp(bob,50)).
%@ t(emp(bob,50)),
%@ t(emp(alice,10)).

%?- insert(emp(alice,10)), insert(emp(bob,50)), delete(emp(alice,10)).
%@ t(emp(bob,50)).

%?- insert(emp(alice,10)), insert(emp(bob,50)), update(emp(alice, 10), emp(alice, 20)).
%@ t(emp(alice,20)),
%@ t(emp(bob,50)).





