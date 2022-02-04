%% 2.4.1 Transitive closure
%% Shortest paths.

:- use_module(library(chr)).

:- chr_constraint e/2, p/3.

%% 这个算法推广自 reachability/single_target.pl，而不 是reachability/single_source
%% 每输入一个 e(Y,Z)，得到对应 p(Y,Z,1)，然后往回找到 e(X,Y) => p(X,Z,N1)
%% 如果往回找不到，则 e(X,Y) 仍然是 active，就会往前找一个 p(Y,Z)，生成 p(X,Z,N1)。

p(X,Y,N) \ p(X,Y,M) <=> N=<M | true.
e(X,Y) ==> p(X,Y,1).
e(X,Y), p(Y,Z,N) ==> N1 is N+1, p(X,Z,N1).

%% 下面两个例子演示了，当路径汇合时，更短的路径将替换之前计算好的路径

%% ?- e(a,b),e(b,c), e(c,d),e(d,e).
%% e(d,e),
%% e(c,d),
%% e(b,c),
%% e(a,b),
%% p(a,b,1).
%% p(a,c,2),
%% p(a,d,3),
%% p(a,e,4),
%% p(b,c,1),
%% p(b,d,2),
%% p(b,e,3),
%% p(c,d,1),
%% p(c,e,2),
%% p(d,e,1),

%% 当 e(a,c) 达到后，它会产生一个 p(a,c,1)，然后替换原来的 p(a,c,2),
%% 接着 e(a,c) 仍然执行，它往前得到 p(a,d,2)，然后替换掉原来的原来的 p(a,d,3)，。。。
%% 这里利用了“如果往回找找不到，则 e(X,Y) 仍然是active，就会往前找一个 p(Y,Z),生成 p(X,Z,N1).”
%% 换句话说，当一 个edge 进入后，它归纳过去，更新未来

%% ?- e(a,b),e(b,c), e(c,d),e(d,e), e(a,c).
%% e(a,c),
%% e(d,e),
%% e(c,d),
%% e(b,c),
%% e(a,b),
%% p(a,b,1).
%% p(a,c,1),
%% p(a,d,2),
%% p(a,e,3),
%% p(b,c,1),
%% p(b,d,2),
%% p(b,e,3),
%% p(c,d,1),
%% p(c,e,2),
%% p(d,e,1),

%% 注意：
%% 这里非常顺序相关，如果我们的顺序是
%% ?- e(a,b),e(b,c), e(a,c), e(c,d),e(d,e).
%% 会更有效率，因为后面的 e(c,d),e(d,e) 基于优化的路径，而不必像上面那些重写汇合后的路径
%% 因此，输入最好是基于DFS顺序

%% OK for circle
%% ?- e(a,b),e(b,a),e(b,d).
%@ e(b,d),
%@ e(b,a),
%@ e(a,b),
%@ p(a,d,2),
%@ p(b,d,1),
%@ p(b,b,2),
%@ p(a,a,2),
%@ p(b,a,1),
%@ p(a,b,1).


%% ?- e(a,a), e(a,b),e(b,c), e(c,a).
%@ e(c,a),
%@ e(b,c),
%@ e(a,b),
%@ e(a,a),
%@ p(b,b,3),
%@ p(c,b,2),
%@ p(c,c,3),
%@ p(b,a,2),
%@ p(c,a,1),
%@ p(a,c,2),
%@ p(b,c,1),
%@ p(a,b,1),
%@ p(a,a,1).

%% from source to all vertex (not all other vertexs)
%% ?- e(a,b),e(b,c), e(c,a).
%@ e(c,a),
%@ e(b,c),
%@ e(a,b),
%@ p(b,b,3),
%@ p(c,b,2),
%@ p(c,c,3),
%@ p(a,a,3),
%@ p(b,a,2),
%@ p(c,a,1),
%@ p(a,c,2),
%@ p(b,c,1),
%@ p(a,b,1).