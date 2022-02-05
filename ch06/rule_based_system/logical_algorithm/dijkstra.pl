%% 6.1.5 Logical algorithms formalism

%% TODO: Need CHRrp https://dtai.cs.kuleuven.be/CHR/CHRrp/
%% But it doesn’t seem to support dynamic priority?
%% See https://swi-prolog.discourse.group/t/package-of-chr-with-dynamic-rule-priorities/4874/2?u=chansey97
d1 @ source(V) ==> dist(V,0) pragma priority(1).
d2 @ dist(V,D1) \ dist(V,D2) <=> D1 < D2 | true pragma priority(1).
d3 @ dist(V,D), e(V,C,U) ==> dist(U,D+C) pragma priority(D+2).

%% Wrong result
%% ?- source(1), e(1,3,2),e(2,8,4),e(1,5,3),e(3,2,4),e(2,1,3), check_activation, show_store.
%% dist(1,0)
%% e(1,3,2)
%% e(2,8,4)
%% e(1,5,3)
%% e(3,2,4)
%% e(2,1,3)
%% source(1)
%% true.