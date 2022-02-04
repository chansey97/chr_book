%% 6.1.4 Event–condition–action rules

%% match predicate 似乎可以不需要，直接把 P 和 C 的匹配写在 rule 头上
%% upd rule 里的的命名可以用 Old New, 更符合数据库 trigger style

:- use_module(library(chr)).

:- chr_constraint t/1, insert/1, delete/1, update/2.

before_ins @ insert(t(_NEW)) ==> writeln("before_ins").
before_del @ delete(t(_OLD)) ==> writeln("before_del").
before_upd @ update(t(_OLD),t(_NEW)) ==> writeln("before_upd").

ins @ insert(t(C)) ==> t(C).
del @ delete(t(P)) \ t(C) <=> match(P,C) | true.
upd @ update(t(P),t(C1)) \ t(C) <=> match(P,C), C1 \= C | t(C1).

after_ins @ insert(t(_NEW)) ==> writeln("after_ins").
after_del @ delete(t(_OLD)) ==> writeln("after_del").
after_upd @ update(t(_OLD),t(_NEW)) ==> writeln("after_upd").

insert(t(_)) <=> true.
delete(t(_)) <=> true.
update(t(_),t(_)) <=> true.

match(P,C) :- P == C. 

%% tests

%?- insert(t(emp(alice,10))).
%@ before_ins
%@ after_ins
%@ t(emp(alice,10)).

%% 重复更新 alice with 同样的值，会触发 update
%?- insert(t(emp(alice,10))), update(t(emp(alice, 10)), t(emp(alice, 20))), update(t(emp(alice, 20)), t(emp(alice, 20))).
%@ before_ins
%@ after_ins
%@ before_upd
%@ after_upd
%@ before_upd
%@ after_upd
%@ t(emp(alice,20)).

%% 删除已经不存在的 alice，会触发delete
%% 这说明，我们的 trigger 是 STATMENT Trigger 而不是ROW Trigger，尽管我们这里没有statement的概念（只能操作row）
%?- delete(t(emp(alice,10))).
%@ before_del
%@ after_del
%@ true.

%% 我们的实现可以吸收 event，只要使用化简规则




