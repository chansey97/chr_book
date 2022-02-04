%% 2.3.3 Depth first search in trees

:- use_module(library(chr)).

:- chr_constraint dfsearch/2.

empty @ dfsearch(nil,X) <=> false.
found @ dfsearch(node(N,L,R),X) <=> X=N | true.
left @ dfsearch(node(N,L,R),X) <=> X<N | dfsearch(L,X).
right @ dfsearch(node(N,L,R),X) <=> X>N | dfsearch(R,X).

%% ?- dfsearch(node(5,node(3,node(1, nil, nil),
%%                           node(4, nil, nil)),
%%                    node(7,nil,nil)),
%%            1).
%@ true.

%% ?- dfsearch(node(5,node(3,node(1, nil, nil),
%%                           node(4, nil, nil)),
%%                    node(7,nil,nil)),
%%            2).
%@ false.
