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

:- module ctx_loadable.

:- interface.

:- import_module set, map.
:- import_module lang, lang_io.
:- import_module context.
:- import_module ctx_modality.

:- type ctx.
:- instance context(ctx, ctx_modality).

:- func new_ctx = ctx.

:- pred add_fact(vscope(matom(ctx_modality))::in, ctx::in, ctx::out) is det.
:- pred add_rule(vscope(mrule(ctx_modality))::in, ctx::in, ctx::out) is det.
:- pred add_assumable(assumable_function_def(ctx_modality)::in, ctx::in, ctx::out) is det.
:- pred add_disjoint_decl(disjoint_decl(ctx_modality)::in, ctx::in, ctx::out) is det.

:- pred set_facts(set(vscope(matom(ctx_modality)))::in, ctx::in, ctx::out) is det.
:- pred set_rules(set(vscope(mrule(ctx_modality)))::in, ctx::in, ctx::out) is det.
:- pred set_assumables(map(string, map(mgatom(ctx_modality), float))::in, ctx::in, ctx::out)
		is det.
:- pred set_disjoint_decls(set(disjoint_decl(ctx_modality))::in, ctx::in, ctx::out) is det.

	% for debugging purposes only!
:- func facts(ctx) = set(vscope(matom(ctx_modality))).
:- func rules(ctx) = set(vscope(mrule(ctx_modality))).
:- func assumables(ctx) = map(string, map(mgatom(ctx_modality), float)).
:- func disjoint_decls(ctx) = set(disjoint_decl(ctx_modality)).

%------------------------------------------------------------------------------%

:- implementation.

:- import_module solutions.
:- import_module list, pair, map, multi_map.

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
	pred(find_fact/4) is find_ctx_fact,
	pred(find_rule/4) is find_ctx_rule,
	pred(assumable_func/4) is ctx_assumable_func,
	func(min_assumption_cost/2) is ctx_min_assumption_cost,
	pred(find_disjoint_decl/2) is find_ctx_disjoint_decl
].

new_ctx = ctx(multi_map.init, multi_map.init, map.init, set.init).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

add_fact(Prop, Ctx0, Ctx) :-
	Facts = Ctx0^ctx_facts,
	Prop = vs(m(Mod, p(PredSym, _Args)), _VS),
	Ctx = Ctx0^ctx_facts := multi_map.add(Facts, Mod-PredSym, Prop).

add_rule(Rule, Ctx0, Ctx) :-
	Rules = Ctx0^ctx_rules,
	Rule = vs(m(Mod, _-Head), _VS),
	m(ModH, p(PredSym, _)) = rule_head_matom(Head),
	Ctx = Ctx0^ctx_rules := multi_map.add(Rules, (Mod++ModH)-PredSym, Rule).

add_assumable(FuncName-Costs, Ctx0, Ctx) :-
	AssumFuncs = Ctx0^ctx_assumables,
	Ctx = Ctx0^ctx_assumables := map.set(AssumFuncs, FuncName, Costs).

add_disjoint_decl(DD, Ctx0, Ctx) :-
	Ctx = Ctx0^ctx_disjoint_decls := set.insert(Ctx0^ctx_disjoint_decls, DD).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

	% XXX these!

set_facts(Facts, Ctx0, Ctx) :-
	set.fold((pred(Fact::in, Fs0::in, Fs::out) is det :-
		Fact = vs(m(Mod, p(PredSym, _Args)), _VS),
		Fs = multi_map.add(Fs0, Mod-PredSym, Fact)
			), Facts, multi_map.init, NewFacts),
	Ctx = Ctx0^ctx_facts := NewFacts.

set_rules(_Rules, Ctx0, Ctx) :-
	Ctx = Ctx0^ctx_rules := multi_map.init.

set_assumables(Assumables, Ctx0, Ctx) :-
	Ctx = Ctx0^ctx_assumables := Assumables.

set_disjoint_decls(DDs, Ctx0, Ctx) :-
	Ctx = Ctx0^ctx_disjoint_decls := DDs.

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

facts(Ctx) = solutions_set(pred(Fact::out) is nondet :-
	multi_map.member(Ctx^ctx_facts, _, Fact)
		).

rules(Ctx) = solutions_set(pred(Rule::out) is nondet :-
	multi_map.member(Ctx^ctx_rules, _, Rule)
		).

assumables(Ctx) = Ctx^ctx_assumables.

disjoint_decls(Ctx) = Ctx^ctx_disjoint_decls.

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

:- pred find_ctx_fact(ctx::in, list(ctx_modality)::in, string::in, vscope(matom(ctx_modality))::out) is nondet.

find_ctx_fact(Ctx, Ms, PredSym, Fact) :-
	multi_map.nondet_search(Ctx^ctx_facts, Ms-PredSym, Fact).
%find_ctx_fact(Ctx, Ms, PredSym, vs(m(Ms, p(PredSym, Args)), VS)) :-
%	set.member(vs(m(Ms, p(PredSym, Args)), VS), Ctx^ctx_facts).

%find_ctx_fact(_Ctx, Ms, "=", vs(m(Ms, p("=", [v(V), v(V)])), VS)) :-
%	new_named_var(varset.init, "X", V, VS).

:- pred find_ctx_rule(ctx::in, list(ctx_modality)::in, string::in, vscope(mrule(ctx_modality))::out) is nondet.

find_ctx_rule(Ctx, Ms, PredSym, Rule) :-
	multi_map.nondet_search(Ctx^ctx_rules, Ms-PredSym, Rule).
%find_ctx_rule(Ctx, Ms, PredSym, VSMRule) :-
%	set.member(VSMRule, Ctx^ctx_rules).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

:- pred ctx_assumable_func(ctx::in, string::in, mgatom(ctx_modality)::out, float::out) is nondet.

ctx_assumable_func(Ctx, FuncName, GProp, Cost) :-
	map.search(Ctx^ctx_assumables, FuncName, MapCosts),
	trace[compile_time(flag("debug")), io(!IO)] ( print(stderr_stream, "A", !IO) ),
	map.member(MapCosts, GProp, Cost).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

:- pred find_ctx_disjoint_decl(ctx::in, set(mgatom(ctx_modality))::out) is nondet.

find_ctx_disjoint_decl(Ctx, DD) :-
	set.member(DD, Ctx^ctx_disjoint_decls).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

:- func ctx_min_assumption_cost(ctx, ctx_modality) = float.

ctx_min_assumption_cost(_, _) = 0.1.
