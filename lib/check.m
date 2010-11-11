%------------------------------------------------------------------------------%
% Copyright (C) 2010 DFKI GmbH Talking Robots 
% Miroslav Janicek (miroslav.janicek@dfki.de) 
%
% This library is free software; you can redistribute it and/or
% modify it under the terms of the GNU Lesser General Public License 
% as published by the Free Software Foundation; either version 2.1 of
% the License, or (at your option) any later version.
%
% This library is distributed in the hope that it will be useful, but
% WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
% Lesser General Public License for more details.
%
% You should have received a copy of the GNU Lesser General Public
% License along with this program; if not, write to the Free Software
% Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
% 02111-1307, USA.
%------------------------------------------------------------------------------%

:- module check.
:- interface.

:- import_module list.
:- import_module term.
:- import_module lang.

:- pred find_singleton_vars(vscope(mrule(M))::in, list(var)::out) is det.

%------------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module pair, set, map.
:- import_module varset.

find_singleton_vars(vs(Rule, _VS), Singletons) :-
	Singletons = set.to_sorted_list(singletons(fold_vars_mrule(inc_use_count, Rule, init_use_count))).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

:- type use_count(T) == map(T, int).

:- func init_use_count = use_count(T).

init_use_count = map.init.

:- func inc_use_count(T, use_count(T)) = use_count(T).

inc_use_count(Item, UC0) = UC :-
	(if map.search(UC0, Item, Cnt)
	then map.det_update(UC0, Item, Cnt + 1, UC)
	else map.det_insert(UC0, Item, 1, UC)
	).

:- func singletons(use_count(T)) = set(T).

singletons(UC) = map.foldl(
	(func(Item, Cnt, S0) = S :-
		(if Cnt = 1
		then S = set.insert(S0, Item)
		else S = S0
		)), UC, set.init).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

:- func fold_vars_mrule(func(var, T) = T, mrule(M), T) = T.

fold_vars_mrule(Func, m(_MR, Ante-Head), A0) = A :-
	(
		Head = std(HMA),
		A1 = fold_vars_matom(Func, HMA, A0)
	;
		Head = test(HMT),
		A1 = fold_vars_mtest(Func, HMT, A0)
	),
	A = list.foldl((func(At, AIn) = AOut :-
		(
			At = std(cf(MA, _AF)),
			AOut = fold_vars_matom(Func, MA, AIn)
		;
			At = test(MT),
			AOut = fold_vars_mtest(Func, MT, AIn)
		)
			), Ante, A1).

:- func fold_vars_matom(func(var, T) = T, matom(M), T) = T.

fold_vars_matom(Func, m(_M, p(_PS, Args)), A0) = list.foldl(fold_vars_term(Func), Args, A0).

:- func fold_vars_term(func(var, T) = T, lang.term, T) = T.

fold_vars_term(Func, v(Var), A0) = Func(Var, A0).
fold_vars_term(Func, t(_F, Args), A0) = list.foldl(fold_vars_term(Func), Args, A0).

:- func fold_vars_mtest(func(var, T) = T, mtest(M), T) = T.

fold_vars_mtest(Func, prop(MA), A0) = fold_vars_matom(Func, MA, A0).
fold_vars_mtest(Func, impl(Ante, Head), A0) = A :-
	A1 = fold_vars_matom(Func, Head, A0),
	A = list.foldl(fold_vars_matom(Func), Ante, A1).
