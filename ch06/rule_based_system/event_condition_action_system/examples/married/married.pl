:- use_module(library(chr)).

:- chr_constraint t/1, insert/1, delete/1, update/2.

%% insert(t(person(X))) ==> insert(t(single(X))).
%% insert(t(married(X))), t(single(X)) ==> delete(t(single(X))).

ins @ insert(t(C)) ==> t(C).
del @ delete(t(P)) \ t(C) <=> match(P,C) | true.
upd @ update(t(P),t(C1)) \ t(C) <=> match(P,C), C1 \= C | t(C1).

%% 这个例子里，放前后都是一样的
insert(t(person(X))) ==> insert(t(single(X))).
insert(t(married(X))), t(single(X)) ==> delete(t(single(X))).

insert(t(_)) <=> true.
delete(t(_)) <=> true.
update(t(_),t(_)) <=> true.

match(P,C) :- P == C. 

%% tests

%?- insert(t(person(alice))).
%@ t(person(alice)),
%@ t(single(alice)).


%?- insert(t(person(alice))), insert(t(married(alice))).
%@ t(married(alice)),
%@ t(person(alice)).
