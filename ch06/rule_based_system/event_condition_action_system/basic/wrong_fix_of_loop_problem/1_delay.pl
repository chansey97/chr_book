%% 6.1.4 Event–condition–action rules

:- use_module(library(chr)).

:- chr_constraint t/1, t_delay/1, insert/1, delete/1, update/2, end/0.

match(emp(Name,_),emp(Name,_)) :- true.

update(emp(Name,S1),emp(Name,S2)) <=> S2>S1*(1+0.1) | S3 is S1*1.1, update(emp(Name,S1),emp(Name,S3)).

ins @ insert(C) ==> t(C).
del @ delete(P) \ t(C) <=> match(P,C) | true.
upd @ update(P,C1) \ t(C) <=> match(P,C) | write("asdasd"), t_delay(C1).

insert(_) <=> true.
delete(_) <=> true.
update(_,_) <=> end.

end @ end, t_delay(C) <=> t(C).

%% 尽管 delay 可以fix这个临时问题，但是如果在 upd后面还有trigger，则会进一步产生新的delay，但是最后到底谁作为最终结果依赖于顺序！
%% 因此，delay 也不是一个好方法。。

%% ppt上提供了传递闭包的例子，好像使用自定义trigger去解决的

%?- insert(emp(alice,10)), insert(emp(bob,50)).
%@ t(emp(bob,50)),
%@ t(emp(alice,10)).
%@ t(emp(bob,50)),
%@ t(emp(alice,10)).

%?- insert(emp(alice,10)), insert(emp(bob,50)), update(emp(alice, 10), emp(alice, 11)).
%@ asdasd
%@ t(emp(alice,11)),
%@ t(emp(bob,50)).

%?- insert(emp(alice,10)), insert(emp(bob,50)), update(emp(alice, X), emp(alice, 11)).
