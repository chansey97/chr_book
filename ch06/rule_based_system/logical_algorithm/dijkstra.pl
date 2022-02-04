%% 6.1.5 Logical algorithms formalism

%% TODO: Need CHRrp https://dtai.cs.kuleuven.be/CHR/CHRrp/
%% But it still doesn't seem to work for this example...

d1 @ source(V) ==> dist(V,0) pragma priority(1).
d2 @ dist(V,D1) \ dist(V,D2) <=> D1 < D2 | true pragma priority(1).
d3 @ dist(V,D), e(V,C,U) ==> dist(U,D+C) pragma priority(D+2).

%% ?- e(1,3,2),e(2,8,4), e(1,5,3),e(3,2,4),e(2,1,3),source(1),check_activation,show_store.
