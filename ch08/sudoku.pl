%% 8.8 Implement in CHR the game Sudoku (number place), a logic-based
%% placement puzzle on a 9×9 board. Fill in the grid so that every row,
%% every column, and every 3×3 box contains the digits 1 through 9.


/*
sudoku.pl: Sudoku solver
(C) Jon Murua Gonzalez and Henning Christiansen, 2005
(C) Thom.Fruehwirth at uni-ulm.de, 2005/11/18
(C) Jon.Sneyers at cs.kuleuven.be, 2005/11/22
This program is distributed under the terms of the GNU General Public License:
http://www.gnu.org/licenses/gpl.html

%% DESCRIPTION
This program solves Sudoku problems with an algorithm based on the heuristics 
try-most-constrained-field-first (first failure).

%% HOW TO USE
The board is considered as a 4-dimensional cube with dimensions {a,b,c} x 
{a,b,c} x {1,2,3} x {1,2,3} where the letters identify a "big square", each of
which has 3x3 atomic squares identified by a pair of numbers. For example, the
upper left atomic square is identified as (a,a,1,1). #
Fields with a determined value are identified by the constraint 
*cell(A,B,C,D,V)* with the coordinates A,B,C and D and value V. #
To change the riddle you adjust the predicate *init_data*. #
You get the first solution with *solve*, all solutions with *solveall*.

%% SAMPLE QUERIES
Q: solve.
A: ** displays solved puzzle **

Q: solveall.
A: ** display ALL solutions for puzzle **
*/

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% A CHR program for solving Sudoku problems
% 
% Derived on Nov 18, 2005 by Thom Fruehwirth from the procedural solver
% of Jon Murua Gonzalez and Henning Christiansen (c)
% by making it declarative, use more CHR. It became much shorter and faster.
% Bug fixed by Jon Sneyers.
%
% This program solves Sudoku problems in very short time
% algorithm based on the heuristics try-most-constrained-field-first 
% (first failure).
%
% It has been tested under SICStus Prolog and SWI-Prolog
%
% The board is considered as a 4-dimensional cube with dimensions
% {a,b,c} x {a,b,c} x {1,2,3} x {1,2,3}
% where the letters identify a "big square", each of which has 3x3 
% atomic squares identified by a pait of numbers.
% For example, upper left atomic square is identified as (a,a,1,1)

% To use the program, enter the initially given numbers for your problem
% by modifying the 'addinitial' predicate at the bottom of the file and call
% ?- solve.

% A state of the board is represented by 81 cell(ield) constraints of the form
% cell( 4 x Coordinates, ListLength, ListOfPossibleValues)
% ListOfPossibleValues indicates which numbers that can be placed in this 
% atomic field without violating the rules of the game.
% If ListLength=1, then ListOfPossibleValues contains the unqiue value of the 
% field and we replace it by cell/5 (for efficency)
% [ bugfix by Jon Sneyers: only replace cell/6 by cell/5 at the next 
% fillone(1), so the N1>0 tests are meaningful.    - Dec. 2005]


:- module(sudoku, [solve/0, solveall/0]).
:- use_module(library(chr)).
:- use_module(library(lists)).

:- chr_type list(X) ---> [] ; [X | list(X)].
:- chr_constraint 
   cell(+,+,+,+,+int,?list(int)),
   fillone(+int),
   cell(+,+,+,+,?int),
   print4(+,+,+,+).

fillone(N), cell(A,B,C,D,N2,L)#Id <=> N2=N | 
    member(V,L), cell(A,B,C,D,V), fillone(1) pragma passive(Id).
fillone(N) <=> N < 9 | N1 is N+1, fillone(N1).
fillone(_) <=> true.

cell(A,B,C,D,_) \ cell(A,B,C,D,_,_)#Id <=> true pragma passive(Id).

% same column
cell(_,B,_,D,V) \ cell(A,B,C,D,N,L)#Id <=> select(V,L,LL) | 
    N1 is N-1, N1>0, cell(A,B,C,D,N1,LL) pragma passive(Id).
% same row
cell(A,_,C,_,V) \ cell(A,B,C,D,N,L)#Id <=> select(V,L,LL) | 
    N1 is N-1, N1>0, cell(A,B,C,D,N1,LL) pragma passive(Id).
% same box
cell(A,B,_,_,V) \ cell(A,B,C,D,N,L)#Id <=> select(V,L,LL) | 
    N1 is N-1, N1>0, cell(A,B,C,D,N1,LL) pragma passive(Id).


%% Auxiliary

solveall :- solve, nl, fail.
solveall.

solve :- init_board, init_data, try, printsolution.
%% solve :- init_board, init_data. % uncomment to see potential values in each blank cell.


%% chooses a value from the most constrained cell first, so we must start from 1
%% and find the first cell which N closest to 1 and then assign it a potentially feasible value
try :- fillone(1).


init_board :-  fill1(a), fill1(b), fill1(c).
fill1(X) :- fill2(X,a),fill2(X,b),fill2(X,c).
fill2(X,Y) :- fill3(X,Y,1), fill3(X,Y,2), fill3(X,Y,3).
fill3(A,B,C) :- fill4(A,B,C,1), fill4(A,B,C,2), fill4(A,B,C,3).
fill4(A,B,C,D) :- cell(A,B,C,D,9,[1,2,3,4,5,6,7,8,9]). % 9 is size of [1,2,3,4,5,6,7,8,9]

% NB in different enumeration order - why?
printsolution:- print1(a), print1(b), print1(c).
print1(X) :- print2(X,1),print2(X,2),print2(X,3), nl.
print2(X,Y) :- print3(X,Y,a), print3(X,Y,b), print3(X,Y,c), nl.
print3(A,B,C) :- print4(A,B,C,1),print4(A,B,C,2),print4(A,B,C,3),write(' ').

print4(A,B,C,D), cell(A,C,B,D,Val) <=> write(Val).
print4(_,_,_,_) <=> write('.').


%% init_data :- cell(a,a,1,1,1),  cell(a,a,1,2,2),  cell(a,a,1,3,3),
%%              cell(a,a,2,1,4),  cell(a,a,2,2,5),  cell(a,a,2,3,6),
%%              cell(a,a,3,1,7),  cell(a,a,3,2,8),  cell(a,a,3,3,9),
%%              cell(b,b,1,1,1),  cell(b,b,1,2,2),  cell(b,b,1,3,3),
%%              cell(b,b,2,1,4),  cell(b,b,2,2,5),  cell(b,b,2,3,6),
%%              cell(b,b,3,1,7),  cell(b,b,3,2,8),  cell(b,b,3,3,9),
%%              cell(c,c,1,1,1),  cell(c,c,1,2,2),  cell(c,c,1,3,3),
%%              cell(c,c,2,1,4),  cell(c,c,2,2,5),  cell(c,c,2,3,6),
%%              cell(c,c,3,1,7),  cell(c,c,3,2,8),  cell(c,c,3,3,9).

%% ?- solve.
%% Too many solutions...


%% Just a board template
%% init_data :- cell(a,a,1,1,1),cell(a,a,1,2,2),cell(a,a,1,3,3), cell(a,b,1,1,1),cell(a,b,1,2,2),cell(a,b,1,3,3), cell(a,c,1,1,1),cell(a,c,1,2,2),cell(a,c,1,3,3),
%%              cell(a,a,2,1,4),cell(a,a,2,2,5),cell(a,a,2,3,6), cell(a,b,2,1,4),cell(a,b,2,2,5),cell(a,b,2,3,6), cell(a,c,2,1,4),cell(a,c,2,2,5),cell(a,c,2,3,6),
%%              cell(a,a,3,1,7),cell(a,a,3,2,8),cell(a,a,3,3,9), cell(a,b,3,1,7),cell(a,b,3,2,8),cell(a,b,3,3,9), cell(a,c,3,1,7),cell(a,c,3,2,8),cell(a,c,3,3,9),
             
%%              cell(b,a,1,1,1),cell(b,a,1,2,2),cell(b,a,1,3,3), cell(b,b,1,1,1),cell(b,b,1,2,2),cell(b,b,1,3,3), cell(b,c,1,1,1),cell(b,c,1,2,2),cell(b,c,1,3,3),
%%              cell(b,a,2,1,4),cell(b,a,2,2,5),cell(b,a,2,3,6), cell(b,b,2,1,4),cell(b,b,2,2,5),cell(b,b,2,3,6), cell(b,c,2,1,4),cell(b,c,2,2,5),cell(b,c,2,3,6),
%%              cell(b,a,3,1,7),cell(b,a,3,2,8),cell(b,a,3,3,9), cell(b,b,3,1,7),cell(b,b,3,2,8),cell(b,b,3,3,9), cell(b,c,3,1,7),cell(b,c,3,2,8),cell(b,c,3,3,9),
             
%%              cell(c,a,1,1,1),cell(c,a,1,2,2),cell(c,a,1,3,3), cell(c,b,1,1,1),cell(c,b,1,2,2),cell(c,b,1,3,3), cell(c,c,1,1,1),cell(c,c,1,2,2),cell(c,c,1,3,3),
%%              cell(c,a,2,1,4),cell(c,a,2,2,5),cell(c,a,2,3,6), cell(c,b,2,1,4),cell(c,b,2,2,5),cell(c,b,2,3,6), cell(c,c,2,1,4),cell(c,c,2,2,5),cell(c,c,2,3,6),
%%              cell(c,a,3,1,7),cell(c,a,3,2,8),cell(c,a,3,3,9), cell(c,b,3,1,7),cell(c,b,3,2,8),cell(c,b,3,3,9), cell(c,c,3,1,7),cell(c,c,3,2,8),cell(c,c,3,3,9).

%% [[5,3,-,-,7,-,-,-,-],
%%  [6,-,-,1,9,5,-,-,-],
%%  [-,9,8,-,-,-,-,6,-],
%%  [8,-,-,-,6,-,-,-,3],
%%  [4,-,-,8,-,3,-,-,1],
%%  [7,-,-,-,2,-,-,-,6],
%%  [-,6,-,-,-,-,2,8,-],
%%  [-,-,-,4,1,9,-,-,5],
%%  [-,-,-,-,8,-,-,7,9]]
init_data :- cell(a,a,1,1,5),cell(a,a,1,2,3),                                 cell(a,b,1,2,7), 
             cell(a,a,2,1,6),                                 cell(a,b,2,1,1),cell(a,b,2,2,9),cell(a,b,2,3,5), 
                             cell(a,a,3,2,9),cell(a,a,3,3,8),                                                                  cell(a,c,3,2,6),
             
             cell(b,a,1,1,8),                                                 cell(b,b,1,2,6),                                                 cell(b,c,1,3,3),
             cell(b,a,2,1,4),                                 cell(b,b,2,1,8),                cell(b,b,2,3,3),                                 cell(b,c,2,3,1),
             cell(b,a,3,1,7),                                                 cell(b,b,3,2,2),                                                 cell(b,c,3,3,6),
             
                             cell(c,a,1,2,6),                                                                  cell(c,c,1,1,2),cell(c,c,1,2,8),
                                                              cell(c,b,2,1,4),cell(c,b,2,2,1),cell(c,b,2,3,9),                                 cell(c,c,2,3,5),
                                                                              cell(c,b,3,2,8),                                 cell(c,c,3,2,8),cell(c,c,3,3,9).

%% ?- solve.
%@ 534 678 912 
%@ 672 195 348 
%@ 198 342 567 
%@ 
%@ 859 761 423 
%@ 426 853 791 
%@ 713 924 856 
%@ 
%@ 961 537 284 
%@ 287 419 635 
%@ 345 286 189 
%@ 
%@ true ;
%@ 534 678 192 
%@ 672 195 348 
%@ 198 342 567 
%@ 
%@ 859 761 423 
%@ 426 853 971 
%@ 713 924 856 
%@ 
%@ 961 537 284 
%@ 287 419 635 
%@ 345 286 789 
%@ 
%@ true.

