%------------------------------------------------------------------------------%
% Copyright (C) 2009-2010 DFKI GmbH Talking Robots 
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

:- module context.

:- interface.

:- import_module lang, modality.
:- import_module list, set.
:- import_module assumability.

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

:- typeclass context(C, M) <= modality(M) where [

	pred find_fact(C, list(M), string, vscope(matom(M))),
	mode find_fact(in, in, in, out) is nondet,

	pred find_rule(C, list(M), string, vscope(mrule(M))),
	mode find_rule(in, in, in, out) is nondet,

	pred assumable_func(C, string, mgatom(M), float),
	mode assumable_func(in, in, out, out) is nondet,

	func min_assumption_cost(C, M) = float,

	pred find_disjoint_decl(C, set(mgatom(M))),
	mode find_disjoint_decl(in, out) is nondet
].

:- func assumption_cost(C, assumability_function, matom(M)) = float <= (context(C, M), modality(M)).

%------------------------------------------------------------------------------%

:- implementation.

:- import_module require, solutions.
:- import_module set, list, pair, string.

%------------------------------------------------------------------------------%

assumption_cost(_Ctx, const(Cost), _MF) = Cost.

assumption_cost(Ctx, f(Name), MF) = Cost :-
	solutions_set((pred(C::out) is nondet :- assumable_func(Ctx, Name, matom_to_ground_matom(MF), C)), Costs),
	(if set.singleton_set(Costs, Cost0)
	then Cost = Cost0
	else error("error in assumption_cost/3: " ++ string(MF)
			++ ", " ++ string.from_int(set.count(Costs)) ++ " alternatives")
	).

assumption_cost(_Ctx, not_assumable, MF) = _Cost :-
	error("non-assumable predicate in assumption_cost/3: " ++ string(MF)).
