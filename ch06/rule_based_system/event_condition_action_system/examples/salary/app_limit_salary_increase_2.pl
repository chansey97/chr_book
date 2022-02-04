%% 6.1.4 Event–condition–action rules

:- use_module(library(chr)).

:- chr_constraint t/1, insert/1, delete/1, update/2.

ins @ insert(t(C)) ==> t(C).
del @ delete(t(P)) \ t(C) <=> match(P,C) | true.
upd @ update(t(P),t(C1)) \ t(C) <=> match(P,C), C1 \= C | t(C1).

update(t(emp(Name,S1)),t(emp(Name,S2))) <=> S2>S1*(1+0.1) | S3 is S1*1.1, update(t(emp(Name,S2)),t(emp(Name,S3))).

insert(t(_)) <=> true.
delete(t(_)) <=> true.
update(t(_),t(_)) <=> true.

match(P,C) :- P == C. 

%% tests

%?- insert(t(emp(alice,10))), update(t(emp(alice, 10)), t(emp(alice, 11))).
%@ t(emp(alice,11)).

%?- insert(t(emp(alice,10))), update(t(emp(alice, 10)), t(emp(alice, 20))).
%@ t(emp(alice,11.0)).


