%% 6.1.4 Event–condition–action rules

%% TODO

insert(num(Y)), min(X) ==> Y<X | delete(min(X)).
num(Y), insert(min(X)) ==> Y<X | delete(min(X)).
insert(num(X)) ==> insert(min(X)).

