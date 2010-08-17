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

:- module abduction.

:- interface.

:- import_module list, bag.
:- import_module varset.

:- import_module modality.
:- import_module formula, costs, context.

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

:- type query(M)
	--->	proved(mprop(M))
	;	assumed(mprop(M), cost_function)
	;	unsolved(mprop(M), cost_function)
	;	asserted(mtest(M))
	.

%:- type marked(T) == pair(T, marking).

:- type step(M)
	--->	assume(vscope(mprop(M)), subst, cost_function)
	;	resolve_rule(vscope(mrule(M)), subst)
	;	use_fact(vscope(mprop(M)), subst)
	;	factor(subst, varset)
	.

:- type proof(M) == vscope(list(query(M))).

:- type costs
	--->	costs(
		fact_cost :: float,
		assertion_cost :: float
	).

%:- type goal(M) == vscope(list(query(M))).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

:- func new_proof(list(query(M)), varset) = proof(M) <= modality(M).

:- pred prove(float::in, float::in, proof(M)::in, proof(M)::out, costs::in, C::in) is nondet <= (modality(M), context(C, M)).

%:- func last_goal(proof(M)) = vscope(list(query(M))) <= modality(M).

:- func goal_assumptions(proof(M)) = bag(with_cost_function(mgprop(M))) <= modality(M).
:- func goal_assertions(proof(M)) = bag(vscope(mtest(M))) <= modality(M).

:- func query_cost(C, varset, query(M), costs) = float <= (context(C, M), modality(M)).
:- func cost(C, proof(M), costs) = float <= (context(C, M), modality(M)).
%:- func goal_cost(C, goal(M), float) = float <= (context(C, M), modality(M)).

%------------------------------------------------------------------------------%

:- implementation.

:- import_module require.
:- import_module map, set, assoc_list, pair.
:- import_module string, float.
:- import_module modality.

new_proof(Goal, Varset) = vs(Goal, Varset).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

goal_assumptions(vs(Qs, _VS)) = As :-
	As = bag.from_list(list.filter_map((func(assumed(MProp, Func)) = AnnotMGProp is semidet :-
		MProp = m(Mod, Prop),
		AnnotMGProp = cf(m(Mod, det_formula_to_ground_formula(Prop)), Func)
			), Qs)).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

goal_assertions(vs(Qs, VS)) = As :-
	As = bag.from_list(list.filter_map((func(asserted(MProp)) = vs(MProp, VS) is semidet), Qs)).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

query_cost(_Ctx, _VS, unsolved(_, _), _) = 0.0.
query_cost(Ctx, VS, assumed(MProp, CostFunction), _Costs) = context.cost(Ctx, CostFunction, vs(MProp, VS)).
query_cost(_Ctx, _VS, proved(_), Costs) = Costs^fact_cost.
query_cost(_Ctx, _VS, asserted(_), Costs) = Costs^assertion_cost.

cost(Ctx, vs(Qs, VS), Costs) = Cost :-
	list.foldl((pred(Q::in, C0::in, C::out) is det :-
		C = C0 + query_cost(Ctx, VS, Q, Costs)
			), Qs, 0.0, Cost).

/*
goal_cost(Ctx, vs(Qs, VS), Costs) = Cost :-
	list.foldl((pred(MProp-Marking::in, C0::in, C::out) is det :-
		( Marking = unsolved(_), C = C0
		; Marking = resolved, C = C0 + CostForUsingFacts
		; Marking = assumed(CostFunction), C = C0 + context.cost(Ctx, CostFunction, vs(MProp, VS))
		; Marking = asserted, C = C0
		)
			), Qs, 0.0, Cost).
*/

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

:- pred goal_solved(list(query(M))::in) is semidet <= modality(M).

goal_solved(L) :-
	list.all_true((pred(Q::in) is semidet :-
		Q \= unsolved(_, _)
			), L).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

prove(CurCost, CostBound, P0, P, Costs, Ctx) :-
	P0 = vs(L0, VS0),
	(if
		goal_solved(L0)
	then
		% check that all assumptions, assertions are ground
		% (because we may have constant weight functions)
		% XXX: check assertions?
		% XXX: check resolved stuff too?
		LAss = list.filter_map((func(Q) = MPr is semidet :-
			Q = assumed(MPr, _)
				), L0),
		all_true((pred(MProp::in) is semidet :-
			ground_formula(MProp^p, _)
				), LAss),
		P = P0
	else
		transform(L0, VS0, L, VS, Ctx),
		P1 = vs(L, VS),

%		StepCost = step_cost(Ctx, Step, Costs),
		% XXX TODO: costs here!
		StepCost = 1.0,
		CurCost + StepCost =< CostBound,

		prove(CurCost + StepCost, CostBound, P1, P, Costs, Ctx)
	).

%------------------------------------------------------------------------------%

:- pred segment_proof_state(list(query(M))::in,
		{list(query(M)), with_cost_function(mprop(M)), list(query(M))}::out) is semidet
		<= modality(M).

segment_proof_state(Qs, {QsL, cf(QUnsolved, F), QsR} ) :-
	list.takewhile((pred(Q0::in) is semidet :-
		Q0 \= unsolved(_, _)
			), Qs, QsL, [unsolved(QUnsolved, F) | QsR]).

%------------------------------------------------------------------------------%

:- import_module io, formula_io.

%------------------------------------------------------------------------------%

:- pred transform(
		list(query(M))::in, varset::in,
		list(query(M))::out, varset::out,
		C::in) is nondet <= (modality(M), context(C, M)).

transform(L0, VS0, L, VS, Ctx) :-
	segment_proof_state(L0, SegL0),
	(if
	 	% try to factor
		do_factoring(SegL0, VS0, L1, VS1, Ctx)
	then
		% if it succeeds
		(if
			goal_solved(L1)
		then
			% we're done! the factoring was the last step
			L = L1,
			VS = VS1
		else
			% make the step on the factored proof
			segment_proof_state(L1, SegL1),
			step(_Step, SegL1, VS1, L, VS, Ctx)
		)
	else
		% if not, continue as before
		step(_Step, SegL0, VS0, L, VS, Ctx)
	).
		

%	segment_proof_state(L0, SegL0),
%	trace[compile_time(flag("debug")), io(!IO)] ( print(stderr_stream, "[", !IO) ),
%	step(_Step, SegL0, VS0, L, VS, Ctx),
%	trace[compile_time(flag("debug")), io(!IO)] ( print(stderr_stream, "]", !IO) ).

%------------------------------------------------------------------------------%

:- pred step(
		step(M)::out, 

			% input
		{
			list(query(M)),  % unsolved (preceding propositions)
			with_cost_function(mprop(M)),  % proposition under examination + its assump.cost
			list(query(M))  % following propositions
		}::in,
		varset::in,  % variables used in the proof

			% output
		list(query(M))::out,  % resulting goal after performing the step
		varset::out,  % variables used in the goal

		C::in  % knowledge base
	) is nondet <= (modality(M), context(C, M)).


step(assume(vs(m(MQ, PQ), VS), map.init, const(Cost)),
		{QsL, cf(m(MQ, PQ), const(Cost)), QsR}, VS,
		QsL ++ [assumed(m(MQ, PQ), const(Cost))] ++ QsR, VS,
		_Ctx).

step(assume(vs(m(MQ, PQ), VS), Uni, f(Func)),
		{QsL0, cf(m(MQ, PQ0), f(Func)), QsR0}, VS,
		QsL ++ [assumed(m(MQ, PQ), f(Func))] ++ QsR, VS,
		Ctx) :-

	assumable_func(Ctx, Func, m(MQ, GroundProp), _Cost),
	ground_formula(Prop, GroundProp),
	unify_formulas(PQ0, Prop, Uni),

	PQ = apply_subst_to_formula(Uni, PQ0),
	QsL = list.map(apply_subst_to_query(Uni), QsL0),
	QsR = list.map(apply_subst_to_query(Uni), QsR0).

/*
	% assumption
step(assume(vs(m(MQ, PQ), VS), Uni, F),
		{QsL0, cf(m(MQ, PQ0), F), QsR0}, VS0,
		QsL ++ [assumed(m(MQ, PQ), F)] ++ QsR, VS,
		Ctx) :-


	assumable(Ctx, vs(m(MQ, PQ0), VS0), F, vs(m(MA, PA0), VSA), _Cost),

	trace[compile_time(flag("debug")), io(!IO)] ( print(stderr_stream, "a{(" ++ atomic_formula_to_string(VSA, PA0), !IO) ),
	match(compose_list(MQ), compose_list(MA)),
	trace[compile_time(flag("debug")), io(!IO)] ( print(stderr_stream, "~", !IO) ),

	varset.merge_renaming(VS0, VSA, VS, Renaming),
	trace[compile_time(flag("debug")), io(!IO)] ( print(stderr_stream, "!", !IO) ),
	PA = rename_vars_in_formula(Renaming, PA0),

	trace[compile_time(flag("debug")), io(!IO)] ( print(stderr_stream, "@", !IO) ),
	unify_formulas(PQ0, PA, Uni),

	trace[compile_time(flag("debug")), io(!IO)] ( print(stderr_stream, "#", !IO) ),
	PQ = apply_subst_to_formula(Uni, PQ0),
	trace[compile_time(flag("debug")), io(!IO)] ( print(stderr_stream, "$", !IO) ),
	QsL = list.map(apply_subst_to_query(Uni), QsL0),
	trace[compile_time(flag("debug")), io(!IO)] ( print(stderr_stream, "%", !IO) ),
	QsR = list.map(apply_subst_to_query(Uni), QsR0),
	trace[compile_time(flag("debug")), io(!IO)] ( print(stderr_stream, "}", !IO) ).
*/
%	formula.is_ground(Q^p).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

	% resolution with a fact
step(use_fact(vs(m(MF, PF), VS), Uni),
		{QsL0, cf(m(MQ, PQ0), not_assumable), QsR0}, VS0,
		QsL ++ [proved(m(MQ, PQ))] ++ QsR, VS,
		Ctx) :-

	PQ0 = p(PredSym, _),
	find_fact(Ctx, MQ, PredSym, vs(m(MF, PF0), VSF)),
%	fact_found(Ctx, vs(m(MQ, PQ0), VS0), vs(m(MF, PF0), VSF)),
	trace[compile_time(flag("debug")), io(!IO)] ( print(stderr_stream, "f{", !IO) ),
	match(compose_list(MF), compose_list(MQ)),

	varset.merge_renaming(VS0, VSF, VS, Renaming),
	PF = rename_vars_in_formula(Renaming, PF0),

	unify_formulas(PF, PQ0, Uni),

	trace[compile_time(flag("debug")), io(!IO)] ( print(stderr_stream, "\\" ++ PredSym ++ "/", !IO) ),

	PQ = apply_subst_to_formula(Uni, PQ0),
	QsL = list.map(apply_subst_to_query(Uni), QsL0),
	QsR = list.map(apply_subst_to_query(Uni), QsR0),
	trace[compile_time(flag("debug")), io(!IO)] ( print(stderr_stream, "}", !IO) ).

step(use_fact(vs(m(MQ, PQ), VS), map.init),
		{QsL0, cf(m(MQ, PQ0), _F), QsR0}, VS,
		QsL ++ [proved(m(MQ, PQ))] ++ QsR, VS,
		_Ctx) :-

	PQ0 = p("=", [T1, T2]),
	unify_terms(T1, T2, Uni),
	PQ = apply_subst_to_formula(Uni, PQ0),
	QsL = list.map(apply_subst_to_query(Uni), QsL0),
	QsR = list.map(apply_subst_to_query(Uni), QsR0).

	% built-in (isn't it actually a rule?)
step(use_fact(vs(m(MQ, PQ), VS), map.init),
		{QsL, cf(m(MQ, PQ), _F), QsR}, VS,
		QsL ++ [proved(m(MQ, PQ))] ++ QsR, VS,
		_Ctx) :-

	PQ = p("\\=", [T1, T2]),
	not unify_terms(T1, T2, _Subst),
	trace[compile_time(flag("debug")), io(!IO)] ( print(stderr_stream, "!", !IO) ).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

	% resolution with a rule
step(resolve_rule(vs(m(MR, Ante-RHead), VS), Uni),
		{QsL0, cf(m(MQ, PQ), not_assumable), QsR0}, VS0,
		QsL ++ QsInsert ++ QsR, VS,
		Ctx) :-

	PQ = p(PredSym, _),
	find_rule(Ctx, MQ, PredSym, Rule),
%	rule_found(Ctx, vs(m(MQ, PQ), VS0), Rule),
	trace[compile_time(flag("debug")), io(!IO)] ( print(stderr_stream, "r{", !IO) ),
	Rule = vs(m(MR, _-RHead0), VSR),
	( RHead0 = std(m(MH, _))
	; RHead0 = test(prop(m(MH, _)))
	; RHead0 = test(impl(_, m(MH, _)))
	),

	match(compose_list(MR ++ MH), compose_list(MQ)),

	varset.merge_renaming(VS0, VSR, VS, Renaming),
	m(MR, Ante-RHead) = rename_vars_in_mrule(Renaming, Rule^body),

	( RHead = std(m(_, PH))
	; RHead = test(prop(m(_, PH)))
	; RHead = test(impl(_, m(_, PH)))
	),

	unify_formulas(PH, PQ, Uni),

	(
		RHead = std(_),
		QHead = proved(m(MQ, apply_subst_to_formula(Uni, PQ)))
	;
		RHead = test(MTest),
		QHead = asserted(apply_subst_to_mtest(Uni, MTest))
	),

		% XXX have assertion in another rule?
	QsInsert = list.map((func(A) = UniA :-
		( A = std(cf(P, F)), UniA = unsolved(apply_subst_to_mprop(Uni, P), F)
		; A = test(T), UniA = asserted(apply_subst_to_mtest(Uni, T))
		)
			), Ante)
			++ [QHead],

%	QsInsert = list.map((func(cf(P, F)) = apply_subst_to_mprop(Uni, P)-unsolved(F)), Ante)
%			++ [m(MQ, apply_subst_to_formula(Uni, PQ))-resolved],

	QsL = list.map(apply_subst_to_query(Uni), QsL0),
	QsR = list.map(apply_subst_to_query(Uni), QsR0),
	trace[compile_time(flag("debug")), io(!IO)] ( print(stderr_stream, "}", !IO) ).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

:- func head_mprop(query(M)) = mprop(M) is det <= modality(M).

head_mprop(proved(MProp)) = MProp.
head_mprop(unsolved(MProp, _)) = MProp.
head_mprop(assumed(MProp, _)) = MProp.
head_mprop(asserted(prop(MProp))) = MProp.
head_mprop(asserted(impl(_, MProp))) = MProp.

:- pred leftmost_unifiable(mprop(M)::in, list(mprop(M))::in, subst::out) is semidet.

leftmost_unifiable(m(Mod, Pred), [m(ModH, PredH) | T], Subst) :-
	(if
		Mod = ModH,
		unify_formulas(PredH, Pred, Subst0)
	then
		Subst = Subst0
	else
		leftmost_unifiable(m(Mod, Pred), T, Subst)
	).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

:- pred do_factoring(
			% input
		{
			list(query(M)),  % unsolved (preceding propositions)
			with_cost_function(mprop(M)),  % proposition under examination + its assump.cost
			list(query(M))  % following propositions
		}::in,
		varset::in,  % variables used in the proof

			% output
		list(query(M))::out,  % resulting goal after performing the step
		varset::out,  % variables used in the goal

		C::in  % knowledge base
	) is semidet <= (modality(M), context(C, M)).

	% factoring
do_factoring(
		{QsL0, cf(m(MQ, PQ), _F), QsR0}, VS,
		QsL ++ QsR, VS,
		_Ctx) :-

	% find leftmost modalised formula that might be unified with Q
	leftmost_unifiable(m(MQ, PQ), list.map(head_mprop, QsL0), Uni),

	trace[compile_time(flag("debug")), io(!IO)] ( print(stderr_stream, "t{", !IO) ),

/*
	member(Prev, QsL0),
	( Prev = proved(MProp)
	; Prev = unsolved(MProp, _)
	; Prev = assumed(MProp, _)
	; Prev = asserted(prop(MProp))
	; Prev = asserted(impl(_, MProp))
	),

	MProp = m(MP, PP),
	match(compose_list(MP), compose_list(MQ)),

	unify_formulas(PP, PQ, Uni),
*/

	QsL = list.map(apply_subst_to_query(Uni), QsL0),
	QsR = list.map(apply_subst_to_query(Uni), QsR0),
	trace[compile_time(flag("debug")), io(!IO)] ( print(stderr_stream, "}", !IO) ).

%------------------------------------------------------------------------------%

:- func map_fst(func(T) = V, list(pair(T, U))) = list(pair(V, U)).

map_fst(Func, LIn) = LOut :-
	LOut = list.map((func(K-V) = apply(Func, K)-V), LIn).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

:- func apply_subst_to_query(subst, query(M)) = query(M) <= modality(M).

apply_subst_to_query(Subst, unsolved(MProp, F)) = unsolved(apply_subst_to_mprop(Subst, MProp), F).
apply_subst_to_query(Subst, proved(MProp)) = proved(apply_subst_to_mprop(Subst, MProp)).
apply_subst_to_query(Subst, assumed(MProp, F)) = assumed(apply_subst_to_mprop(Subst, MProp), F).
apply_subst_to_query(Subst, asserted(MTest)) = asserted(apply_subst_to_mtest(Subst, MTest)).
