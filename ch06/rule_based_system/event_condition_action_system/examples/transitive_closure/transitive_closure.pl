%% 6.1.4 Event–condition–action rules

%% TODO

insert(p(X,Y)), p(X,Y) ==> delete(p(X,Y)).
insert(e(X,Y)) ==> insert(p(X,Y)).
insert(e(X,Y)), p(Y,Z) ==> insert(p(X,Z)).
e(X,Y), insert(p(Y,Z)) ==> insert(p(X,Z)).

