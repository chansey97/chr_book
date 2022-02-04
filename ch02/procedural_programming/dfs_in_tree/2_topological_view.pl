%% 2.3.3 Depth first search in trees

:- use_module(library(chr)).

:- chr_constraint nil/1, node/4, dfsearch/2.

%% Actually, we can access the data directly by just mentioning it in the head
%% of the rule.

empty @ nil(I) \ dfsearch(I,X) <=> fail.
found @ node(I,N,L,R) \ dfsearch(I,X) <=> X=N | true.
left @ node(I,N,L,R) \ dfsearch(I,X) <=> X<N | dfsearch(L,X).
right @ node(I,N,L,R) \ dfsearch(I,X) <=> X>N | dfsearch(R,X).

%% The first argument of dfsearch is root node's identifier

%% ?- nil(id1), nil(id2), nil(id3), nil(id4), nil(id5), nil(id6),
%%    node(id7,1,id1,id2), node(id8,4,id3,id4), node(id9,3,id7,id8),
%%    node(id10,7,id5,id6), node(id11,5,id9,id10),
%%    dfsearch(id11, 4).
%@ nil(id6),
%@ nil(id5),
%@ nil(id4),
%@ nil(id3),
%@ nil(id2),
%@ nil(id1),
%@ node(id11,5,id9,id10),
%@ node(id10,7,id5,id6),
%@ node(id9,3,id7,id8),
%@ node(id8,4,id3,id4),
%@ node(id7,1,id1,id2).

%% ?- nil(id1), nil(id2), nil(id3), nil(id4), nil(id5), nil(id6),
%%    node(id7,1,id1,id2), node(id8,4,id3,id4), node(id9,3,id7,id8),
%%    node(id10,7,id5,id6), node(id11,5,id9,id10),
%%    dfsearch(id11, 2).
%@ false.
