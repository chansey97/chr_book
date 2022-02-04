%% 9.3 Description logic

:- use_module(library(chr)).

:- op(1200,xfx, isa).		% concept definition
:- op(950,xfx,  '::').		% A-Box membership and role-filler assertion
:- op(940,xfy,  or).		% disjunction
:- op(930,xfy,  and).		% conjunction
:- op(700,xfx,  is).		% used in restricitions 
:- op(690,fy,   nota).		% complement		
:- op(650,fx,   some).		% exists-in restriction
:- op(650,fx,   every).   % value restriction
:- op(650,fx,   all).	% value restriction, book
:- op(100,yfx,of).           % role chain
:- op(650,fx, allsome).      % allsome quantifier

:- dynamic (isa)/2, feature/1.  % to allow scattered clauses

% CONSISTENCY CHECK
% A-box as constraint goals
% T-box asserted (concept definitions by isa-rules)

:- chr_constraint (::)/2, label/0.


% primitive clash
  I::nota Q, I::Q <=> false.		

% remove duplicates (both concepts and roles)
  I::C \ I::C <=> true.

% top and bottom
  I::top <=> true.
  I::bot <=> false.
  I::nota top <=> false.
  I::nota bot <=> true.

% complement nota/1

% nota-or
  I::nota (S or T)  <=> I::(nota S and nota T).

% nota-and
  I::nota (S and T) <=> I::(nota S or nota T).

% nota-nota
  I::nota (nota S)  <=> I::S.

% nota-every 
  I::nota (every R is S) <=> I::some R is nota S.

% nota-some 
  I::nota (some R is S)  <=> I::every R is nota S.

% conjunction
  I::S and T <=> I::S, I::T.

% exists-in restriction
  I::some R is S <=>  (I,J)::R, J::S.	% generate witness
% I::every R is D \ I::some R is S <=> (I,J)::R, J::S. % more lazy

% value restriction
  I::every R is S, (I,J)::R ==> J::S.

% disjunction (is delayed as choice)
  label, (I::S or T) <=> true | (I::S ; I::T), label. % search by labeling

% T-box unfolding using concept definition
  I::C <=> (C isa T) | I::T.
  I::nota C <=> (C isa T) | I::nota T.
% X::_ \ I::C <=> (C isa T) | I::T. % more lazy


%% Book example
male isa nota female.
parent isa human and some child is human.
mother isa parent and female.
proud_parent isa parent and all child is phd.
grandmother isa mother and some child is parent.
motherofsons isa mother and all child is male.

%?- sue::proud_parent.
%@ sue::all child is phd,
%@ $VAR(_A)::human,
%@ (sue,$VAR(_A))::child,
%@ sue::human.

%?- sue::proud_parent, (sue,joe)::child, joe::nota phd.
%@ joe::nota phd,
%@ (sue,joe)::child,
%@ sue::all child is phd,
%@ $VAR(_A)::human,
%@ (sue,$VAR(_A))::child,
%@ sue::human.
