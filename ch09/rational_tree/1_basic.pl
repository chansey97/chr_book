%% 9.4 Rational trees

%% N.B. I comment module declare, because ediprolog seems cannot detect op(600,xfx,(~)) in a module.
%% Alternative approach is ediprolog-dwim the op(600,xfx,(~)) line first.
%% :- module(rationaltree, [(~)/2]).

:- use_module(library(chr)).
:- use_module(library(lists)).
user:library_directory('../../common/').
:- use_module(library(ordering)). % need stable order on variables

:- op(600,xfx,(~)).

:- chr_constraint (~)/2, eq_list/2.

% T1 ~ T2 means: term T1 is syntactically equal to term T2

eq_globalize        @ T1 ~ T2   ==> var_order:globalize(T1), var_order:globalize(T2).

eq_reflexivity      @ X ~ X     <=> var(X) | true.
eq_orientation      @ T ~ X     <=> var(X), lss(X,T) | X ~ T.   
eq_decomposition    @ T1 ~ T2   <=> nonvar(T1),nonvar(T2) | 
                                    same_functions(T1,T2).
eq_confrontation    @ X ~ T1 \ X ~ T2   <=> var(X), lss(X,T1), leq(T1,T2) | 
                                            T1 ~ T2.

% two same-length lists must be equal, 
% i.e., every pair of elements must be equal
eq_list([],[])                <=> true.
eq_list([X|L1],[Y|L2])        <=> X ~ Y, eq_list(L1,L2).

%% Auxiliary

% lss(X,Y): X is smaller than Y by term-size order
lss(X,Y) :- var(X),var(Y), var_order:var_compare(<,X,Y).      % stable var order
lss(X,Y) :- var(X),nonvar(Y).
lss(X,Y) :- nonvar(X),nonvar(Y), termsize(X,M),termsize(Y,N),M<N.

% leq(X,Y): X is smaller-eq than Y by term-size order
leq(X,Y) :- \+ lss(Y,X).

% functions must be equal
same_functions(T1,T2) :-
    T1=..[F|L1], T2=..[F|L2], same_length(L1,L2), eq_list(L1,L2).

% termsize
termsize_list([],0).
termsize_list([X|L],N) :- termsize(X,N1), termsize_list(L,N2), N is N1+N2.
termsize(X,0) :- var(X).
termsize(T,1) :- atom(T).
termsize(T,N) :- compound(T), T=..L, termsize_list(L,N).


%% Example 9.4.1

%?- h(Y,f(a),g(X,a)) ~ h(f(U),Y,g(h(Y),U)).
%@ $VAR(X)~h($VAR(Y)),
%@ $VAR(U)~a,
%@ $VAR(Y)~f($VAR(U)).

%% Example 9.4.2
%?- X ~ f(X), X ~ f(f(X)).
%@ $VAR(X)~f($VAR(X)).

%% SAMPLE QUERIES

%?- f(X,b)~f(a,Y).
%@ $VAR(Y)~b,
%@ $VAR(X)~a.

%?- f(a,b)~f(X).
%@ false.

%?- X~f(X), X~f(f(X)).
%@ $VAR(X)~f($VAR(X)).

%?- A~B,B~A,c~B.
%@ $VAR(B)~c,
%@ $VAR(A)~ $VAR(B).

%?- X~f(Y,f(a,X)), X~f(a,X).
%@ $VAR(Y)~a,
%@ $VAR(X)~f(a,$VAR(X)).


