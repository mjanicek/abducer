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

:- import_module list, set, bag.
:- import_module varset.

:- import_module modality.
:- import_module formula, costs, context.
:- import_module blacklist.

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

:- type goal(M) == vscope(list(query(M))).

:- type proof(M)
	--->	proof(
		goal :: goal(M),
		blacklist :: blacklist(M)
	).

:- type costs
	--->	costs(
		fact_cost :: float,
		assertion_cost :: float
	).

%:- type goal(M) == vscope(list(query(M))).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

:- func new_proof(C, list(query(M)), varset) = proof(M) <= (context(C, M), modality(M)).

:- type iddfs_increment
	--->	absolute(float)
%	;	logarithmic(float)
	.

:- pred prove(float::in, iddfs_increment::in, proof(M)::in, set(proof(M))::out, costs::in, C::in) is det <= (modality(M), context(C, M)).

%:- func last_goal(proof(M)) = vscope(list(query(M))) <= modality(M).

:- func goal_assumptions(goal(M)) = bag(with_cost_function(mgprop(M))) <= modality(M).
:- func goal_assertions(goal(M)) = bag(vscope(mtest(M))) <= modality(M).

:- func query_cost(C, varset, query(M), costs) = float <= (context(C, M), modality(M)).
:- func cost(C, proof(M), costs) = float <= (context(C, M), modality(M)).
%:- func goal_cost(C, goal(M), float) = float <= (context(C, M), modality(M)).

%------------------------------------------------------------------------------%

:- implementation.

:- import_module require, solutions.
:- import_module map, assoc_list, pair.
:- import_module string, float, bool.
:- import_module modality.

:- import_module anytime.

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

new_proof(Ctx, Goal, Varset) = proof(vs(Goal, Varset), blacklist.init(Ctx)).

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

cost(Ctx, proof(vs(Qs, VS), _), Costs) = Cost :-
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

prove(CostBound, Increment, P0, Ps, Costs, Ctx) :-
	Ps0 = solutions_set((pred(P::out) is nondet :-
		prove_bound(CostBound, P0, P, Costs, Ctx)
			)),
	(if
		anytime.pure_signalled(no)
	then
		(
			Increment = absolute(AbsValue),
			NewCostBound = CostBound + AbsValue
		),
		prove(NewCostBound, Increment, P0, Ps1, Costs, Ctx),
		Ps = set.union(Ps0, Ps1)
	else
		Ps = Ps0
	).

:- pred prove_bound(float::in, proof(M)::in, proof(M)::out, costs::in, C::in) is nondet <= (modality(M), context(C, M)).

prove_bound(CostBound, P0, P, Costs, Ctx) :-
	anytime.pure_signalled(no),

	P0 = proof(vs(L0, VS0), BL0),
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
		transform(L0, VS0, BL0, L, VS, BL, Ctx),
		P1 = proof(vs(L, VS), BL),

		cost(Ctx, P1, Costs) < CostBound,
		prove_bound(CostBound, P1, P, Costs, Ctx)
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

:- pragma promise_pure(transform/7).

:- pred transform(
		list(query(M))::in, varset::in, blacklist(M)::in,
		list(query(M))::out, varset::out, blacklist(M)::out,
		C::in) is nondet <= (modality(M), context(C, M)).

transform(L0, VS0, BL0, L, VS, BL, Ctx) :-
	impure anytime.signalled(no),
	segment_proof_state(L0, SegL0),

	step(_Step, SegL0, VS0, BL0, L1, VS1, BL1, Ctx),
	(if
		factor_proof_state([], L1, VS1, BL1, L2, [], VS2, BL2, Ctx)
	then
		L = L2,
		VS = VS2,
		BL3 = BL2
	else
		L = L1,
		VS = VS1,
		BL3 = BL1
	),
	check_disjoints(L, BL3, BL).

/*
	(if
	 	% try to factor
%		do_factoring(SegL0, VS0, BL0, L1, VS1, BL1, Ctx)
		fail
	then
		% if it succeeds
		(if
			goal_solved(L1)
		then
			% we're done! the factoring was the last step
			L = L1,
			VS = VS1,
			BL = BL1
		else
			% make the step on the factored proof
			segment_proof_state(L1, SegL1),
			step(_Step, SegL1, VS1, BL1, L, VS, BL, Ctx)
		)
	else
		% if not, continue as before
		step(_Step, SegL0, VS0, BL0, L, VS, BL, Ctx)
	).
*/
		

%	segment_proof_state(L0, SegL0),
%	trace[compile_time(flag("debug")), io(!IO)] ( print(stderr_stream, "[", !IO) ),
%	step(_Step, SegL0, VS0, L, VS, Ctx),
%	trace[compile_time(flag("debug")), io(!IO)] ( print(stderr_stream, "]", !IO) ).

%------------------------------------------------------------------------------%

%:- pragma promise_pure(step/8).

:- pred step(
		step(M)::out, 

			% input
		{
			list(query(M)),  % unsolved (preceding propositions)
			with_cost_function(mprop(M)),  % proposition under examination + its assump.cost
			list(query(M))  % following propositions
		}::in,
		varset::in,  % variables used in the proof
		blacklist(M)::in,  % blacklisted ground formulas

			% output
		list(query(M))::out,  % resulting goal after performing the step
		varset::out,  % variables used in the goal
		blacklist(M)::out,  % blacklisted ground formulas

		C::in  % knowledge base
	) is nondet <= (modality(M), context(C, M)).


step(assume(vs(m(MQ, PQ), VS), map.init, const(Cost)),
		{QsL, cf(m(MQ, PQ), const(Cost)), QsR}, VS, BL,
		QsL ++ [assumed(m(MQ, PQ), const(Cost))] ++ QsR, VS, BL,
		_Ctx) :-
	anytime.pure_signalled(no).

step(assume(vs(m(MQ, PQ), VS), Uni, f(Func)),
		{QsL0, cf(m(MQ, PQ0), f(Func)), QsR0}, VS, BL0,
		QsL ++ [assumed(m(MQ, PQ), f(Func))] ++ QsR, VS, BL,
		Ctx) :-

	anytime.pure_signalled(no),

	assumable_func(Ctx, Func, m(MQ, GroundProp), _Cost),
	ground_formula(Prop, GroundProp),
	unify_formulas(PQ0, Prop, Uni),

	PQ = apply_subst_to_formula(Uni, PQ0),
%	QsL = list.map(apply_subst_to_query(Uni), QsL0),
%	QsR = list.map(apply_subst_to_query(Uni), QsR0).
	list.map_foldl(apply_subst_to_query_blacklist(Uni, Ctx), QsL0, QsL, BL0, BL1),
	list.map_foldl(apply_subst_to_query_blacklist(Uni, Ctx), QsR0, QsR, BL1, BL).

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
		{QsL0, cf(m(MQ, PQ0), not_assumable), QsR0}, VS0, BL0,
		QsL ++ [proved(m(MQ, PQ))] ++ QsR, VS, BL,
		Ctx) :-

	anytime.pure_signalled(no),

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
%	QsL = list.map(apply_subst_to_query(Uni), QsL0),
%	QsR = list.map(apply_subst_to_query(Uni), QsR0),
	list.map_foldl(apply_subst_to_query_blacklist(Uni, Ctx), QsL0, QsL, BL0, BL1),
	list.map_foldl(apply_subst_to_query_blacklist(Uni, Ctx), QsR0, QsR, BL1, BL),
	trace[compile_time(flag("debug")), io(!IO)] ( print(stderr_stream, "}", !IO) ).

step(use_fact(vs(m(MQ, PQ), VS), map.init),
		{QsL0, cf(m(MQ, PQ0), _F), QsR0}, VS, BL0,
		QsL ++ [proved(m(MQ, PQ))] ++ QsR, VS, BL,
		Ctx) :-

	anytime.pure_signalled(no),

	PQ0 = p("=", [T1, T2]),
	unify_terms(T1, T2, Uni),
	PQ = apply_subst_to_formula(Uni, PQ0),
%	QsL = list.map(apply_subst_to_query(Uni), QsL0),
%	QsR = list.map(apply_subst_to_query(Uni), QsR0).
	list.map_foldl(apply_subst_to_query_blacklist(Uni, Ctx), QsL0, QsL, BL0, BL1),
	list.map_foldl(apply_subst_to_query_blacklist(Uni, Ctx), QsR0, QsR, BL1, BL).

	% built-in (isn't it actually a rule?)
step(use_fact(vs(m(MQ, PQ), VS), map.init),
		{QsL, cf(m(MQ, PQ), _F), QsR}, VS, BL,
		QsL ++ [proved(m(MQ, PQ))] ++ QsR, VS, BL,
		_Ctx) :-

	anytime.pure_signalled(no),

	PQ = p("\\=", [T1, T2]),
	not unify_terms(T1, T2, _Subst),
	trace[compile_time(flag("debug")), io(!IO)] ( print(stderr_stream, "!", !IO) ).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

	% resolution with a rule
step(resolve_rule(vs(m(MR, Ante-RHead), VS), Uni),
		{QsL0, cf(m(MQ, PQ), not_assumable), QsR0}, VS0, BL0,
		QsL ++ QsInsert ++ QsR, VS, BL,
		Ctx) :-

	anytime.pure_signalled(no),

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

%	QsL = list.map(apply_subst_to_query(Uni), QsL0),
%	QsR = list.map(apply_subst_to_query(Uni), QsR0),
	list.map_foldl(apply_subst_to_query_blacklist(Uni, Ctx), QsL0, QsL, BL0, BL1),
	list.map_foldl(apply_subst_to_query_blacklist(Uni, Ctx), QsR0, QsR, BL1, BL),
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
		blacklist(M)::in,  % blacklisted formulas

			% output
		list(query(M))::out,  % resulting goal after performing the step
		varset::out,  % variables used in the goal
		blacklist(M)::out,  % blacklisted formulas

		C::in  % knowledge base
	) is semidet <= (modality(M), context(C, M)).

	% factoring
do_factoring(
		{QsL0, cf(m(MQ, PQ), _F), QsR0}, VS, BL0,
		QsL ++ QsR, VS, BL,
		Ctx) :-

	anytime.pure_signalled(no),

	% find leftmost modalised formula that might be unified with Q
	leftmost_unifiable(m(MQ, PQ), list.map(head_mprop, QsR0), Uni),

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

%	QsL = list.map(apply_subst_to_query(Uni), QsL0),
%	QsR = list.map(apply_subst_to_query(Uni), QsR0),
	list.map_foldl(apply_subst_to_query_blacklist(Uni, Ctx), QsL0, QsL, BL0, BL1),
	list.map_foldl(apply_subst_to_query_blacklist(Uni, Ctx), QsR0, QsR, BL1, BL),
	trace[compile_time(flag("debug")), io(!IO)] ( print(stderr_stream, "}", !IO) ).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

:- pred factor_proof_state(
			% input
		list(query(M))::in,
		list(query(M))::in,
		varset::in,  % variables used in the proof
		blacklist(M)::in,  % blacklisted formulas

			% output
		list(query(M))::out,
		list(query(M))::out,
		varset::out,  % variables used in the goal
		blacklist(M)::out,  % blacklisted formulas

		C::in  % knowledge base
	) is nondet <= (modality(M), context(C, M)).


factor_proof_state(QsL, [], VS, BL, QsL, [], VS, BL, _Ctx).

factor_proof_state(
		QsL0, [H|T], VS, BL0,
		QsL, QsR, VS, BL,
		Ctx) :-

	anytime.pure_signalled(no),

	(if
		solved_unifiable(H, T, Uni)
	then
		list.map_foldl(apply_subst_to_query_blacklist(Uni, Ctx), QsL0, QsL1, BL0, BL1),
		list.map_foldl(apply_subst_to_query_blacklist(Uni, Ctx), T, QsR1, BL1, BL2),
		factor_proof_state(QsL1, QsR1, VS, BL2, QsL, QsR, VS, BL, Ctx)
	else
		factor_proof_state(QsL0 ++ [H], T, VS, BL0, QsL, QsR, VS, BL, Ctx)
	).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

:- pred solved_unifiable(query(M)::in, list(query(M))::in, subst::out) is nondet <= modality(M).

solved_unifiable(Q, [QH|_T], Uni) :-
	not Q = unsolved(_, _),
	not QH = unsolved(_, _),
	m(HM, HF) = head_mprop(QH),
	m(M, F) = head_mprop(Q),
	match(compose_list(HM), compose_list(M)),
	unify_formulas(HF, F, Uni).

solved_unifiable(Q, [_QH|T], Uni) :-
	solved_unifiable(Q, T, Uni).

%------------------------------------------------------------------------------%

:- pred check_disjoints(list(query(M))::in, blacklist(M)::in, blacklist(M)::out) is semidet <= modality(M).

check_disjoints(Qs, BL0, BL) :-
	anytime.pure_signalled(no),
	list.foldl((pred(Q::in, BL1::in, BL2::out) is semidet :-
		(if ground_mprop(head_mprop(Q), G)
		then check_mgprop(G, BL1, BL2)
		else BL2 = BL1
			)), Qs, BL0, BL).

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

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

	% TODO: apply blacklist incrementally
:- pred apply_subst_to_query_blacklist(subst::in, C::in, query(M)::in, query(M)::out, blacklist(M)::in, blacklist(M)::out) is det
		<= (modality(M), context(C, M)).

apply_subst_to_query_blacklist(Subst, _Ctx, QIn, QOut, BL, BL) :-
	QOut = apply_subst_to_query(Subst, QIn).

%apply_subst_to_query_blacklist(Subst, Ctx, QIn, QOut, BLIn, BLOut) :-
%	QOut = apply_subst_to_query(Subst, QIn),
%	(if ground_mprop(head_mprop(QOut), G)
%	then check_mgprop(G, BLIn, BLOut)
%	else BLOut = BLIn
%	).
