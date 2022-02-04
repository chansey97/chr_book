%% 6.1.4 Event–condition–action rules

%% match predicate 似乎可以不需要，直接把 P 和 C 的匹配写在 rule 头上
%% upd rule 里的的命名可以用 Old New, 更符合数据库 trigger style

:- use_module(library(chr)).

:- chr_constraint t/1, insert/1, delete/1, update/2.

ins @ insert(t(C)) ==> t(C).
del @ delete(t(P)) \ t(C) <=> match(P,C) | true.
upd @ update(t(P),t(C1)) \ t(C) <=> match(P,C), C1 \= C | t(C1).

insert(t(_)) <=> true.
delete(t(_)) <=> true.
update(t(_),t(_)) <=> true.

match(P,C) :- P == C. 

%% tests

%?- insert(t(emp(alice,10))), insert(t(emp(bob,50))).
%@ t(emp(bob,50)),
%@ t(emp(alice,10)).

%?- insert(t(emp(alice,10))), insert(t(emp(bob,50))), delete(t(emp(alice,10))).
%@ t(emp(bob,50)).

%?- insert(t(emp(alice,10))), insert(t(emp(bob,50))), update(t(emp(alice, 10)), t(emp(alice, 20))).
%@ t(emp(alice,20)),
%@ t(emp(bob,50)).

%% 注意：
%% 1. 由于这里 insert, delete, update 都是 event，而不是 command，它们应该都是 ground 值，
%% 下面企图把 event 当成 command 来用是错误的：
%?- insert(t(emp(alice,10))), insert(t(emp(bob,50))), delete(t(emp(alice,X))).
%@ t(emp(bob,50)),
%@ t(emp(alice,10)).
%% alice没有被删除

%% 2. insert 可以插入重复的值
%% 这是允许的，我们并没有定义primitive key
%?- insert(t(emp(alice,10))), insert(t(emp(alice,10))).
%@ t(emp(alice,10)),
%@ t(emp(alice,10)).

%% 3. update 相同的数据，Now OK
%?- insert(t(emp(alice,10))), update(t(emp(alice, 10)), t(emp(alice, 10))).
%@ t(emp(alice,10)).







