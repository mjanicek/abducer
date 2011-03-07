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

:- module ctx_loadable.

:- interface.

:- import_module context.
:- import_module ctx_modality.

:- type ctx.

:- instance context(ctx, ctx_modality).
:- instance modifiable(ctx, ctx_modality).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

:- func new_ctx = ctx.

%------------------------------------------------------------------------------%

:- implementation.

:- import_module solutions.
:- import_module set, list, pair, map, multi_map.
:- import_module lang, modality.

:- import_module io.  % for debugging

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

:- type ctx
	--->	ctx(
		ctx_facts :: multi_map(pair(list(ctx_modality), string), vscope(matom(ctx_modality))),
		ctx_rules :: multi_map(pair(list(ctx_modality), string), vscope(mrule(ctx_modality))),
		ctx_assumables :: map(string, map(mgatom(ctx_modality), float)),
		ctx_disjoint_decls :: set(set(mgatom(ctx_modality)))
	).

:- instance context(ctx, ctx_modality) where [
	pred(find_fact/4) is ctx_loadable.find_fact,
	pred(find_rule/4) is ctx_loadable.find_rule,
	pred(assumable_func/4) is ctx_loadable.assumable_func,
	pred(find_disjoint_decl/2) is ctx_loadable.find_disjoint_decl
].

:- instance modifiable(ctx, ctx_modality) where [
	func(facts/1) is ctx_loadable.facts,
	func(rules/1) is ctx_loadable.rules,
	func(assumables/1) is ctx_assumables,
	func(disjoint_decls/1) is ctx_disjoint_decls,

	func('facts :='/2) is ctx_loadable.'facts :=',
	func('rules :='/2) is ctx_loadable.'rules :=',
	func('assumables :='/2) is 'ctx_assumables :=',
	func('disjoint_decls :='/2) is 'ctx_disjoint_decls :=',

	pred(add_fact/3) is context.naive_add_fact,
	pred(add_rule/3) is context.naive_add_rule,
	pred(set_assumability_function/4) is context.naive_set_assumability_function,
	pred(add_disjoint_decl/3) is context.naive_add_disjoint_decl
].

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

new_ctx = ctx(multi_map.init, multi_map.init, map.init, set.init).

%------------------------------------------------------------------------------%

	% context instance

:- pred find_fact(ctx::in, list(ctx_modality)::in, string::in, vscope(matom(ctx_modality))::out) is nondet.
:- pred find_rule(ctx::in, list(ctx_modality)::in, string::in, vscope(mrule(ctx_modality))::out) is nondet.
:- pred assumable_func(ctx::in, string::in, mgatom(ctx_modality)::out, float::out) is nondet.
:- pred find_disjoint_decl(ctx::in, set(mgatom(ctx_modality))::out) is nondet.

find_fact(Ctx, Ms, PredSym, Fact) :-
	multi_map.nondet_search(Ctx^ctx_facts, Ms-PredSym, Fact).

find_rule(Ctx, Ms, PredSym, Rule) :-
	multi_map.nondet_search(Ctx^ctx_rules, Ms-PredSym, Rule).

assumable_func(Ctx, FuncName, MGAtom, Cost) :-
	map.search(Ctx^ctx_assumables, FuncName, MapCosts),
	trace[compile_time(flag("debug")), io(!IO)] ( print(stderr_stream, "A", !IO) ),
	map.member(MapCosts, MGAtom, Cost).

find_disjoint_decl(Ctx, DD) :-
	set.member(DD, Ctx^ctx_disjoint_decls).

%------------------------------------------------------------------------------%

	% modifiable instance

	% getters

:- func facts(ctx) = set(vscope(matom(ctx_modality))).
:- func rules(ctx) = set(vscope(mrule(ctx_modality))).

facts(Ctx) = solutions_set(pred(Fact::out) is nondet :-
	multi_map.member(Ctx^ctx_facts, _, Fact)
		).

rules(Ctx) = solutions_set(pred(Rule::out) is nondet :-
	multi_map.member(Ctx^ctx_rules, _, Rule)
		).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

	% setters

:- func 'facts :='(ctx, set(vscope(matom(ctx_modality)))) = ctx.
:- func 'rules :='(ctx, set(vscope(mrule(ctx_modality)))) = ctx.

'facts :='(Ctx0, Facts) = Ctx :-
	set.fold((pred(Fact::in, Fs0::in, Fs::out) is det :-
		Fact = vs(m(Mod, p(PredSym, _Args)), _VS),
		Fs = multi_map.add(Fs0, Mod-PredSym, Fact)
			), Facts, multi_map.init, NewFacts),
	Ctx = Ctx0^ctx_facts := NewFacts.

'rules :='(Ctx0, Rules) = Ctx :-
	set.fold((pred(Rule::in, Rs0::in, Rs::out) is det :-
		Rs = insert_rule(Rule, Rs0)
			), Rules, multi_map.init, NewRules),
	Ctx = Ctx0^ctx_rules := NewRules.

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%
/*
	% incremental adders

:- pred add_fact(vscope(matom(ctx_modality))::in, ctx::in, ctx::out) is det.
:- pred add_rule(vscope(mrule(ctx_modality))::in, ctx::in, ctx::out) is det.

add_fact(MAtom, Ctx0, Ctx) :-
	Facts = Ctx0^ctx_facts,
	MAtom = vs(m(Mod, p(PredSym, _Args)), _VS),
	Ctx = Ctx0^ctx_facts := multi_map.add(Facts, Mod-PredSym, MAtom).

add_rule(MRule, Ctx0, Ctx) :-
	Rules = Ctx0^ctx_rules,
	MRule = vs(m(Mod, _-Head), _VS),
	m(ModH, p(PredSym, _)) = rule_head_matom(Head),
	Ctx = Ctx0^ctx_rules := multi_map.add(Rules, (Mod++ModH)-PredSym, MRule).
*/
%------------------------------------------------------------------------------%

	% utils

:- func insert_fact(vscope(matom(ctx_modality)), multi_map(pair(list(ctx_modality), string), vscope(matom(ctx_modality))))
		= multi_map(pair(list(ctx_modality), string), vscope(matom(ctx_modality))).

insert_fact(VsMA, I0) = I :-
	VsMA = vs(m(M, p(PredSym, _Args)), _VS),
	I = multi_map.add(I0, M-PredSym, VsMA).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

:- func insert_rule(vscope(mrule(ctx_modality)), multi_map(pair(list(ctx_modality), string), vscope(mrule(ctx_modality))))
		= multi_map(pair(list(ctx_modality), string), vscope(mrule(ctx_modality))).

insert_rule(VsR, I0) = I :-
	VsR = vs(m(M, _-Head), _VS),
	m(MHead, p(PredSym, _)) = rule_head_matom(Head),
	I = multi_map.add(I0, compose_list(M++MHead)-PredSym, VsR).
