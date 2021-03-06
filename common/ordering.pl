%% Ported to hProlog by Tom Schrijvers, tom.schrijvers@cs.kuleuven.ac.be
%%
%% $Id$
%%
%% Used by gauss.chr

% Preliminary support for some CHR handlers
%
% Define a stable ordering on variables
% (Term/Var ordering changes under put_atts, delay, etc.)
%
% Bindings still brake our ordering!
%
%

:- module(ordering, [globalize/1, unglobalize/1, var_compare/3]).
:- use_module(library(terms)).

%% :- install_verify_attribute_handler(ordering,Attr,Other,attr_unify_hook(Attr,Other)).

%
% The exception mechanism copies the thrown term.
% Thus we cannot pass the variable to the catcher ...
%
attr_unify_hook(id(Id),Other) :-
	(   var(Other) ->
      (   get_attr(Other,ordering,id(_)) ->
          true
      ;   
          put_attr(Other,ordering,id(Id))
      )
	;   
      true
	).

globalize( Term) :-
	term_variables( Term, Vars),
	var_globalize( Vars).

var_globalize( X) :- var( X), !, % indexing only
	(   get_attr( X, ordering, id(_)) ->
	    true
	;   
	    put_attr( X, ordering, id(_))
	).
var_globalize( []).
var_globalize( [X|Xs]) :-
	var_globalize( X),
	var_globalize( Xs).

unglobalize( Term) :-
	term_variables( Term, Vars),
	var_unglobalize( Vars).

var_unglobalize( X) :- var( X), !, % indexing only
	del_attr( X, ordering).
var_unglobalize( []).
var_unglobalize( [X|Xs]) :-
	var_unglobalize( X),
	var_unglobalize( Xs).

var_compare( Rel, X, Y) :-
	(   var(X),get_attr( X, ordering, id(IdX)) ->
	    true
	;   
	    throw( not_globalized)
	),
	(   var(Y),get_attr( Y, ordering, id(IdY)) ->
	    true
	;   
	    throw( not_globalized)
	),
	compare( Rel, IdX, IdY).

