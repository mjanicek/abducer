%------------------------------------------------------------------------------%
% Copyright (C) 2009-2011 DFKI GmbH Talking Robots 
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
:- import_module list, set, map.
:- import_module assumability.

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

:- typeclass context(C, M) <= ((C -> M), modality(M)) where [

	pred find_fact(C, list(M), string, vscope(matom(M))),
	mode find_fact(in, in, in, out) is nondet,

	pred find_rule(C, list(M), string, vscope(mrule(M))),
	mode find_rule(in, in, in, out) is nondet,

	pred assumable_func(C, string, mgatom(M), float),
	mode assumable_func(in, in, out, out) is nondet,

	pred find_disjoint_decl(C, set(mgatom(M))),
	mode find_disjoint_decl(in, out) is nondet
].

:- func assumption_cost(C, assumability_function, matom(M)) = float <= (context(C, M), modality(M)).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

:- typeclass modifiable(C, M) <= context(C, M) where [

		% getters
	func facts(C) = set(vscope(matom(M))),
	func rules(C) = set(vscope(mrule(M))),
	func assumables(C) = map(string, map(mgatom(M), float)),
	func disjoint_decls(C) = set(disjoint_decl(M)),

		% setters
	func 'facts :='(C, set(vscope(matom(M)))) = C,
	func 'rules :='(C, set(vscope(mrule(M)))) = C,
	func 'assumables :='(C, map(string, map(mgatom(M), float))) = C,
	func 'disjoint_decls :='(C, set(disjoint_decl(M))) = C,

		% more specialised incremental builders
	pred add_fact(vscope(matom(M)), C, C),
	mode add_fact(in, in, out) is det,

	pred add_rule(vscope(mrule(M)), C, C),
	mode add_rule(in, in, out) is det,

	pred set_assumability_function(string, map(mgatom(M), float), C, C),
	mode set_assumability_function(in, in, in, out) is det,

	pred add_disjoint_decl(disjoint_decl(M), C, C),
	mode add_disjoint_decl(in, in, out) is det
].

:- pred naive_add_fact(vscope(matom(M))::in, C::in, C::out) is det <= modifiable(C, M).
:- pred naive_add_rule(vscope(mrule(M))::in, C::in, C::out) is det <= modifiable(C, M).
:- pred naive_set_assumability_function(string::in, map(mgatom(M), float)::in, C::in, C::out) is det <= modifiable(C, M).
:- pred naive_add_disjoint_decl(disjoint_decl(M)::in, C::in, C::out) is det <= modifiable(C, M).

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

%------------------------------------------------------------------------------%

naive_add_fact(F, C0, C) :-
	C = C0^facts := set.insert(C0^facts, F).

naive_add_rule(R, C0, C) :-
	C = C0^rules := set.insert(C0^rules, R).

naive_set_assumability_function(Function, Assumables, C0, C) :-
	C = C0^assumables := map.set(C0^assumables, Function, Assumables).

naive_add_disjoint_decl(DD, C0, C) :-
	C = C0^disjoint_decls := set.insert(C0^disjoint_decls, DD).
