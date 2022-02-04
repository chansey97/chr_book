%% 9.4 Rational trees

%% N.B. I comment module declare, because ediprolog seems cannot detect op(600,xfx,(~)) in a module.
%% Alternative approach is ediprolog-dwim the op(600,xfx,(~)) line first.
%% :- module(rationaltree, [(~)/2, (#~)/2, neq_list/2, label/0]).

:- use_module(library(chr)).
:- use_module(library(lists)).
user:library_directory('../../common/').
:- use_module(library(ordering)). % need stable order on variables

:- op(600,xfx,(~)).
:- op(600,xfx,(#~)).

:- chr_constraint (~)/2, (#~)/2, eq_list/2, neq_list/2, label/0.

% T1 ~ T2 means: term T1 is syntactically equal to term T2
% T1 #~ T2 means: term T1 is syntactically different from term T2

eq_globalize        @ T1 ~ T2   ==> var_order:globalize(T1), var_order:globalize(T2).
neq_globalize       @ T1 #~ T2  ==> var_order:globalize(T1), var_order:globalize(T2).

eq_reflexivity      @ X ~ X     <=> var(X) | true.
eq_orientation      @ T ~ X     <=> var(X), lss(X,T) | X ~ T.   
eq_decomposition    @ T1 ~ T2   <=> nonvar(T1),nonvar(T2) | 
                                    same_functions(T1,T2).
eq_confrontation    @ X ~ T1 \ X ~ T2   <=> var(X), lss(X,T1), leq(T1,T2) | 
                                            T1 ~ T2.


neq_reflexivity     @ X #~ X    <=> fail.
neq_orientation     @ T #~ X    <=> var(X), lss(X,T) | X #~ T. 
neq_decomposition   @ T1 #~ T2  <=> nonvar(T1), nonvar(T2) | 
                                    different_functions(T1,T2).
neq_confrontation   @ X ~ T1 \ X #~ T2  <=> T1 #~ T2. 


% two same-length lists must be equal, 
% i.e., every pair of elements must be equal
eq_list([],[])                <=> true.
eq_list([X|L1],[Y|L2])        <=> X ~ Y, eq_list(L1,L2).

% two same-length lists must not be equal, 
% i.e., at least one pair of elements must be different
neq_list([],[])               <=> fail.
neq_list([X],[Y])             <=> X #~ Y.
neq_list([X|L1],[X|L2])       <=> neq_list(L1,L2).
neq_list([X|L1],[Y|L2]), X~Y  <=> neq_list(L1,L2).

% label
label \ neq_list([X|L1],[Y|L2])#Id <=> true |  
                    (X #~ Y ; X ~ Y, neq_list(L1,L2))
                    pragma passive(Id).


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

% functions must be different
different_functions(T1,T2) :-
    T1=..[F|L1], T2=..[F|L2], same_length(L1,L2), !, neq_list(L1,L2).
different_functions(_,_).

% termsize
termsize_list([],0).
termsize_list([X|L],N) :- termsize(X,N1), termsize_list(L,N2), N is N1+N2.
termsize(X,0) :- var(X).
termsize(T,1) :- atom(T).
termsize(T,N) :- compound(T), T=..L, termsize_list(L,N).

%% SAMPLE QUERIES

%% ?- f(X,b)~f(a,Y).
%@ $VAR(Y)~b,
%@ $VAR(X)~a.

%% ?- f(X,b)~f(a,Y).
%@ $VAR(Y)~b,
%@ $VAR(X)~a.

%% ?- f(a,b)~f(X).
%@ false.

%% ?- X~f(X), X~f(f(X)).
%@ $VAR(X)~f($VAR(X)).

%% ?- A~B,B~A,c~B.
%@ $VAR(B)~c,
%@ $VAR(A)~ $VAR(B).

%% ?- X~f(Y,f(a,X)), X~f(a,X).
%@ $VAR(Y)~a,
%@ $VAR(X)~f(a,$VAR(X)).

%% ?- X#~a,X~b.
%@ $VAR(X)~b.

%% ?- A~B,B~C,C#~A.
%@ false.

%% ?- f(X,Y)#~f(Y,X).
%@ neq_list([$VAR(X),$VAR(Y)],[$VAR(Y),$VAR(X)]).

%% ?- f(X,Y)#~f(Y,X), label.
%@ $VAR(X)#~ $VAR(Y),
%@ label ;
%@ false.

%% ?- f(a,X,c)#~f(a,b,Y), X#~Y.
%@ $VAR(X)#~ $VAR(Y),
%@ neq_list([$VAR(X),c],[b,$VAR(Y)]).

%% ?- f(a,X,c)#~f(a,b,Y), X#~Y, label.
%@ $VAR(X)#~b,
%@ $VAR(X)#~ $VAR(Y),
%@ label ;
%@ $VAR(X)~b,
%@ $VAR(Y)#~c,
%@ $VAR(Y)#~b,
%@ label.

%% vs miniKanren

%% about labling constraint with disjunction

%% (run* (q)
%%       (fresh (x y) 
%%              (=/= '(5 6) `(,x ,y))
%%              (== q `(,x ,y))))
%% ;; '(((_.0 _.1) (=/= ((_.0 5) (_.1 6))))) 

%% ?- f(5,6)#~f(X,Y), label.
%@ $VAR(X)#~5,
%@ label ;
%@ $VAR(X)~5,
%@ $VAR(Y)#~6,
%@ label.

%% :- dif(f(5,6),f(X,Y)).
%@ dif(f($VAR(X),$VAR(Y)),f(5,6)).

%% ?- f(5,6)#~f(X,Y).
%@ neq_list([5,6],[$VAR(X),$VAR(Y)]).

%% subsume 1

%% (run* (q)
%%       (fresh (x y) 
%%              (=/= '(5 6) `(,x ,y))
%%              (=/= 5 x)
%%              (== q `(,x ,y))))
%% ;; '(((_.0 _.1) (=/= ((_.0 5)))))

%% ?- f(5,6)#~f(X,Y), 5#~X.
%@ $VAR(X)#~5,
%@ neq_list([5,6],[$VAR(X),$VAR(Y)]).

%% 5#~X should subsume neq_list([5,6],[$VAR(X),$VAR(Y)])

%% ?- f(5,6)#~f(X,Y), 5#~X, label.
%@ $VAR(X)#~5,
%@ $VAR(X)#~5,
%@ label ;
%@ false.

%% 这里lable似乎过早

%% Here the Prolog built-in is not good as well:

%% ?- dif(f(5,6),f(X,Y)), dif(5,X).
%@ dif(f($VAR(X),$VAR(Y)),f(5,6)),
%@ dif($VAR(X),5).

%% subsume 2

%% (run* (q)
%%       (=/= '(5 6) `(,q ,q)))
%% ;; '(_.0)

%% ?- f(X,X)#~f(5,6).
%@ neq_list([$VAR(X),$VAR(X)],[5,6]).

%% :- dif(f(X,X),f(5,6)).
%@ true. % Prolog built-in is OK, which like miniKanren


%% ?- f(X,X)#~f(5,6), label.
%@ $VAR(X)#~5,
%@ label ;
%@ $VAR(X)~5,
%@ label.

%% X should be arbitrary, not disjunction
%% Need to be simplified
%% This may influence miniKanren-smt


%% A complicated reifying

%% (run* (q)
%%       (fresh (x y z)
%%              (=/= 5 x)
%%              (=/= 6 x)
%%              (=/= `(,y 1) `(2 ,z))
%%              (== `(,x ,y ,z) q)))
%% '(((_.0 _.1 _.2) (=/= ((_.0 5)) ((_.0 6)) ((_.1 2) (_.2 1)))))

%% The constraints ((_.0 6)) and ((_.0 5)) are independent of each other, and indicate
%% that x can never be 5 or 6. However, ((_.1 2) (_.2 1)) represents a single
%% constraint, indicating that y cannot be 2 if z is 1.

%% ((_.1 2) (_.2 1)) 也可以indicating that z cannot be 1, if y is 2

%% ?- 5 #~ X, 6 #~ X, f(Y,1) #~ f(2,Z).
%@ $VAR(X)#~6,
%@ $VAR(X)#~5,
%@ neq_list([$VAR(Y),1],[2,$VAR(Z)]).

%% ?- 5 #~ X, 6 #~ X, f(Y,1) #~ f(2,Z), label.
%@ $VAR(Y)#~2,
%@ $VAR(X)#~6,
%@ $VAR(X)#~5,
%@ label ;
%@ $VAR(Y)~2,
%@ $VAR(Z)#~1,
%@ $VAR(X)#~6,
%@ $VAR(X)#~5,
%@ label.

%%%%% 有下面可见，似乎neq_list实际上已经有了语义？ miniKanren的reified语义？
%%%%% 它实际上已经表达了or

%% (run* (q)
%%       (fresh (x y z)
%%              (=/= 5 x)
%%              (=/= 6 x)
%%              (=/= `(111 ,y 1) `(111 2 ,z))
%%              (== `(,x ,y ,z) q)))
%% ;; '(((_.0 _.1 _.2) (=/= ((_.0 5)) ((_.0 6)) ((_.1 2) (_.2 1)))))


%% ?- 5 #~ X, 6 #~ X, f(111,Y,1) #~ f(111,2,Z).
%@ $VAR(X)#~6,
%@ $VAR(X)#~5,
%@ neq_list([$VAR(Y),1],[2,$VAR(Z)]).

%% ?- dif(5,X), dif(6,X), dif([111,Y,1], [111,2,Z]).
%@ dif($VAR(X),5),
%@ dif($VAR(X),6),
%@ dif(f($VAR(Y),$VAR(Z)),f(2,1)).
