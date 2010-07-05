:- module ctx_specific.

:- interface.

:- import_module set, map, pair.
:- import_module formula.
:- import_module context.
:- import_module ctx_modality.
:- import_module costs.

:- import_module belief_model.
:- import_module ctx_ontology.

:- import_module unit.

:- type ctx
	--->	ctx(
		ctx_expl_rules :: set(vscope(mrule(ctx_modality))),  % explicit rules
		ctx_expl_facts :: set(mgprop(ctx_modality)),  % explicit rules
		bm :: belief_model(string, string, string),
		rrel :: unit,
		ont :: ctx_ontology
	).
:- instance context(ctx, ctx_modality).

:- func new_ctx = ctx.

:- type assumable_function_def(M) == pair(cost_function_name, map(mgprop(M), float)).

:- pred add_explicit_rule(vscope(mrule(ctx_modality))::in, ctx::in, ctx::out) is det.
:- pred set_explicit_rules(set(vscope(mrule(ctx_modality)))::in, ctx::in, ctx::out) is det.

:- pred add_explicit_fact(mgprop(ctx_modality)::in, ctx::in, ctx::out) is det.
:- pred set_explicit_facts(set(mgprop(ctx_modality))::in, ctx::in, ctx::out) is det.

	% for debugging purposes only!
:- func explicit_rules(ctx) = set(vscope(mrule(ctx_modality))).
:- func explicit_facts(ctx) = set(mgprop(ctx_modality)).

%------------------------------------------------------------------------------%

:- implementation.

:- import_module require.
:- import_module list, pair, map, float, int.
:- import_module costs, varset, lf, lf_io, ontology.
:- import_module model.
:- import_module modality, enumerable.
:- import_module ling.

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

:- instance context(ctx, ctx_modality) where [
	pred(find_fact/4) is find_ctx_fact,
	pred(find_rule/4) is find_ctx_rule,
	pred(fact_found/3) is ctx_fact,
	pred(rule_found/3) is ctx_rule,
	pred(assumable_func/4) is ctx_assumable_func,
	func(min_assumption_cost/2) is ctx_min_assumption_cost
].

new_ctx = ctx(set.init, set.init, belief_model.init, unit, ctx_ontology.init).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

add_explicit_rule(Rule, Ctx0, Ctx) :-
	Rules = Ctx0^ctx_expl_rules,
	Ctx = Ctx0^ctx_expl_rules := set.insert(Rules, Rule).

set_explicit_rules(Rules, Ctx0, Ctx) :-
	Ctx = Ctx0^ctx_expl_rules := Rules.

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

add_explicit_fact(Fact, Ctx0, Ctx) :-
	Facts = Ctx0^ctx_expl_facts,
	Ctx = Ctx0^ctx_expl_facts := set.insert(Facts, Fact).

set_explicit_facts(Facts, Ctx0, Ctx) :-
	Ctx = Ctx0^ctx_expl_facts := Facts.

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

explicit_rules(Ctx) = Ctx^ctx_expl_rules.
explicit_facts(Ctx) = Ctx^ctx_expl_facts.

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

:- func ctx_min_assumption_cost(ctx, ctx_modality) = float.

ctx_min_assumption_cost(_, _) = 0.1.

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

:- pred find_ctx_fact(ctx::in, list(ctx_modality)::in, string::in, vscope(mprop(ctx_modality))::out) is nondet.

find_ctx_fact(Ctx, Ms, PredSym, VSMProp) :-
	fail.

:- pred find_ctx_rule(ctx::in, list(ctx_modality)::in, string::in, vscope(mrule(ctx_modality))::out) is nondet.

find_ctx_rule(Ctx, Ms, PredSym, VSMRule) :-
	fail.

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

:- pred ctx_rule(ctx::in, vscope(mprop(ctx_modality))::in, vscope(mrule(ctx_modality))::out) is nondet.

ctx_rule(Ctx, _, Rule) :-
	set.member(Rule, Ctx^ctx_expl_rules).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

:- pred ctx_fact(ctx::in, vscope(mprop(ctx_modality))::in, vscope(mprop(ctx_modality))::out) is nondet.

	% explicit fact
ctx_fact(Ctx, vs(m(_, p(PredSym, _)), _), vs(m(Mod, Prop), varset.init)) :-
	set.member(m(Mod, GProp), Ctx^ctx_expl_facts),
	Prop = ground_formula_to_formula(GProp).

	% generate a LF from the belief model
ctx_fact(Ctx, vs(m(Mod, _), _), VSMProp) :-
	compose_list(Mod) = [k(STF, Belief)],
	k_fact(Ctx^ont, Ctx^rrel, Ctx^bm, STF, Belief, LF),
	VSMProp = vs(m(Mod, ground_formula_to_formula(lf_to_ground_atomic_formula(LF))), varset.init).

	% validate a LF against the belief model
ctx_fact(Ctx, vs(m(Mod, Prop), VS), vs(m(Mod, Prop), VS)) :-
	compose_list(Mod) = [k(STF, Belief)],
	k_model(Ctx^ont, Ctx^rrel, Ctx^bm, STF, Belief, M),
	satisfies(Ctx^ont, M, ground_atomic_formula_to_lf(formula_to_ground_formula(Prop))).

	% test subsumption in the used ontology
ctx_fact(Ctx, vs(m(Mod, p("<<", [_, _])), VS), vs(m(Mod, p("<<", [t(Sub, []), t(Super, [])])), VS)) :-
	generate(Ctx^ont, Sub),
	generate(Ctx^ont, Super),
	isa(Ctx^ont, Sub, Super).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

:- pred ctx_assumable_func(ctx::in, cost_function_name::in, mgprop(ctx_modality)::out, float::out) is nondet.

ctx_assumable_func(Ctx, _, m(Mod, GProp), Cost) :-
	Mod = [att],
	att_model(Ctx^ont, Ctx^rrel, Ctx^bm, AM),
	map.member(AM^worlds, WName, Sort),
	(
		map.search(AM^props, i(WName), Props),
		set.member(Prop, Props),
		noun_gender(Prop, neut)
	;
		noun_gender(Sort, neut)
	),
	Dist = min_dist_from_set(Ctx^ont, AM, fg_anchors(Ctx^bm), WName),
	Cost = 0.5 + float(Dist),
	GProp = p("it", [t(WName, [])]).
