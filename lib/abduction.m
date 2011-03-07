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

:- module abduction.

:- interface.

:- import_module list, set.
:- import_module varset.

:- import_module modality.
:- import_module lang, assumability, context.
:- import_module blacklist.

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

:- type query(M)
	--->	proved(matom(M))
	;	assumed(matom(M), assumability_function)
	;	unsolved(matom(M), assumability_function)
	;	asserted(mtest(M))
	.

:- type goal(M) == vscope(list(query(M))).

:- type proof(M)
	--->	proof(
		goal :: goal(M),
		blacklist :: blacklist(M)
	).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

:- type structure_costs
	--->	structure_costs(
		fact_cost :: float,
		assertion_cost :: float
	).

:- func minimal_structure_costs = structure_costs.

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

:- type proof_search_method
	--->	unbounded_dfs
	;	bounded_dfs(float)
	;	iddfs(float, bound_transform)
	.

:- inst proof_search_method_inst
	--->	unbounded_dfs
	;	bounded_dfs(ground)
	;	iddfs(ground, bound_transform)
	.

:- type bound_transform == (func(float) = float).
:- inst bound_transform == (func(in) = out is semidet).

:- func add_cost(float::in) `with_type` bound_transform `with_inst` bound_transform.
:- func multiply_cost(float::in) `with_type` bound_transform `with_inst` bound_transform.

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

:- func new_proof(C, list(query(M)), varset) = proof(M) <= (context(C, M), modality(M)).

:- pred prove(proof_search_method::in(proof_search_method_inst), proof(M)::in, set(proof(M))::out, structure_costs::in, C::in) is det <= (modality(M), context(C, M)).

:- func proof_cost(C, proof(M), structure_costs) = float <= (context(C, M), modality(M)).

%------------------------------------------------------------------------------%

:- implementation.

:- import_module solutions.
:- import_module map, pair, maybe.
:- import_module string, float, bool.
:- import_module modality.

:- import_module anytime.

:- import_module io.  % for debugging purposes

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

minimal_structure_costs = structure_costs(1.0, 1.0).

%------------------------------------------------------------------------------%

new_proof(Ctx, Goal, Varset) = proof(vs(Goal, Varset), blacklist.init(Ctx)).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

:- func query_cost(C, query(M), structure_costs) = float <= (context(C, M), modality(M)).

query_cost(_Ctx, unsolved(_, _), _) = 0.0.
query_cost(Ctx, assumed(MAtom, CostFunction), _Costs) = assumption_cost(Ctx, CostFunction, MAtom).
query_cost(_Ctx, proved(_), Costs) = Costs^fact_cost.
query_cost(_Ctx, asserted(_), Costs) = Costs^assertion_cost.

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

proof_cost(Ctx, proof(vs(Qs, _VS), _), Costs) = Cost :-
	list.foldl((pred(Q::in, C0::in, C::out) is det :-
		C = C0 + query_cost(Ctx, Q, Costs)
			), Qs, 0.0, Cost).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

:- pred goal_solved(list(query(M))::in) is semidet <= modality(M).

goal_solved(L) :-
	list.all_true((pred(Q::in) is semidet :-
		Q \= unsolved(_, _)
			), L).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

add_cost(Inc, Bound) = Bound + Inc.

multiply_cost(Arg, Bound) = Bound * Arg.

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

prove(unbounded_dfs, P0, Ps, Costs, Ctx) :-
	Ps = solutions_set((pred(P::out) is nondet :-
		prove_bound(no, P0, P, Costs, Ctx)
			)).

prove(bounded_dfs(Bound), P0, Ps, Costs, Ctx) :-
	Ps = solutions_set((pred(P::out) is nondet :-
		prove_bound(yes(Bound), P0, P, Costs, Ctx)
			)).

prove(iddfs(Bound, BoundTransform), P0, Ps, Costs, Ctx) :-
	prove(bounded_dfs(Bound), P0, Ps0, Costs, Ctx), 
	(if
		anytime.pure_signalled(no)
	then
		(if
			NewBound0 = BoundTransform(Bound)
		then
			NewBound = NewBound0,
			prove(iddfs(NewBound, BoundTransform), P0, Ps1, Costs, Ctx),
			Ps = set.union(Ps0, Ps1)
		else
			Ps = Ps0
		)
	else
		Ps = Ps0
	).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

:- pred prove_bound(maybe(float)::in, proof(M)::in, proof(M)::out, structure_costs::in, C::in) is nondet <= (modality(M), context(C, M)).

prove_bound(MayBound, P0, P, Costs, Ctx) :-
	anytime.pure_signalled(no),

	P0 = proof(vs(L0, VS0), BL0),
	(if
		goal_solved(L0)
	then
		% check that all assumptions, assertions are ground
		% (because we may have constant weight functions)
		% XXX: check assertions?
		% XXX: check resolved stuff too?
/*
		LAss = list.filter_map((func(Q) = MPr is semidet :-
			Q = assumed(MPr, _)
				), L0),
		all_true((pred(m(_M, F)::in) is semidet :-
			ground_formula(F, _)
				), LAss),
*/
		P = P0
	else
		transform(L0, VS0, BL0, L, VS, BL, Ctx),
		P1 = proof(vs(L, VS), BL),

		(
			MayBound = yes(Bound),
			proof_cost(Ctx, P1, Costs) < Bound
		;
			MayBound = no
		),
		prove_bound(MayBound, P1, P, Costs, Ctx)
	).

%------------------------------------------------------------------------------%

:- pred segment_proof_state(list(query(M))::in,
		{list(query(M)), with_assumability_function(matom(M)), list(query(M))}::out) is semidet
		<= modality(M).

segment_proof_state(Qs, {QsL, cf(QUnsolved, F), QsR} ) :-
	list.takewhile((pred(Q0::in) is semidet :-
		Q0 \= unsolved(_, _)
			), Qs, QsL, [unsolved(QUnsolved, F) | QsR]).

%------------------------------------------------------------------------------%

:- pred transform(
		list(query(M))::in, varset::in, blacklist(M)::in,
		list(query(M))::out, varset::out, blacklist(M)::out,
		C::in) is nondet <= (modality(M), context(C, M)).

transform(L0, VS0, BL0, L, VS, BL, Ctx) :-
	anytime.pure_signalled(no),
	segment_proof_state(L0, SegL0),

	proof_step(SegL0, VS0, BL0, L1, VS1, BL1, Ctx),
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

%------------------------------------------------------------------------------%

:- pred proof_step(
			% input
		{
			list(query(M)),  % unsolved (preceding propositions)
			with_assumability_function(matom(M)),  % proposition under examination + its assump.cost
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

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

	% Assumption

proof_step({QsL, cf(m(MQ, PQ), const(Cost)), QsR}, VS, BL,
		QsL ++ [assumed(m(MQ, PQ), const(Cost))] ++ QsR, VS, BL,
		_Ctx) :-
	anytime.pure_signalled(no).

proof_step({QsL0, cf(m(MQ, PQ0), f(Func)), QsR0}, VS, BL0,
		QsL ++ [assumed(m(MQ, PQ), f(Func))] ++ QsR, VS, BL,
		Ctx) :-

	anytime.pure_signalled(no),

	assumable_func(Ctx, Func, m(MQ, GroundAtom), _Cost),
	ground_formula(Atom, GroundAtom),
	unify_formulas(PQ0, Atom, Uni),

	PQ = apply_subst_to_formula(Uni, PQ0),
%	QsL = list.map(apply_subst_to_query(Uni), QsL0),
%	QsR = list.map(apply_subst_to_query(Uni), QsR0).
	list.map_foldl(apply_subst_to_query_blacklist(Uni, Ctx), QsL0, QsL, BL0, BL1),
	list.map_foldl(apply_subst_to_query_blacklist(Uni, Ctx), QsR0, QsR, BL1, BL).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

	% Resolution with a fact

proof_step({QsL0, cf(m(MQ, PQ0), not_assumable), QsR0}, VS0, BL0,
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

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

	% Equality

proof_step({QsL0, cf(m(MQ, PQ0), _F), QsR0}, VS, BL0,
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

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

	% Non-equality

proof_step({QsL, cf(m(MQ, PQ), _F), QsR}, VS, BL,
		QsL ++ [proved(m(MQ, PQ))] ++ QsR, VS, BL,
		_Ctx) :-

	anytime.pure_signalled(no),

	PQ = p("\\=", [T1, T2]),
	not unify_terms(T1, T2, _Subst),
	trace[compile_time(flag("debug")), io(!IO)] ( print(stderr_stream, "!", !IO) ).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

	% resolution with a rule
proof_step({QsL0, cf(m(MQ, PQ), not_assumable), QsR0}, VS0, BL0,
		QsL ++ QsInsert ++ QsR, VS, BL,
		Ctx) :-

	anytime.pure_signalled(no),

	PQ = p(PredSym, _),
	find_rule(Ctx, MQ, PredSym, Rule),
%	rule_found(Ctx, vs(m(MQ, PQ), VS0), Rule),
	trace[compile_time(flag("debug")), io(!IO)] ( print(stderr_stream, "r{", !IO) ),
	Rule = vs(RuleBody, VSR),
	RuleBody = m(MR, _-RHead0),
	( RHead0 = std(m(MH, _))
	; RHead0 = test(prop(m(MH, _)))
	; RHead0 = test(impl(_, m(MH, _)))
	),

	match(compose_list(MR ++ MH), compose_list(MQ)),

	varset.merge_renaming(VS0, VSR, VS, Renaming),
	m(MR, Ante-RHead) = rename_vars_in_mrule(Renaming, RuleBody),

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
		( A = std(cf(P, F)), UniA = unsolved(apply_subst_to_matom(Uni, P), F)
		; A = test(T), UniA = asserted(apply_subst_to_mtest(Uni, T))
		)
			), Ante)
			++ [QHead],

%	QsInsert = list.map((func(cf(P, F)) = apply_subst_to_matom(Uni, P)-unsolved(F)), Ante)
%			++ [m(MQ, apply_subst_to_formula(Uni, PQ))-resolved],

%	QsL = list.map(apply_subst_to_query(Uni), QsL0),
%	QsR = list.map(apply_subst_to_query(Uni), QsR0),
	list.map_foldl(apply_subst_to_query_blacklist(Uni, Ctx), QsL0, QsL, BL0, BL1),
	list.map_foldl(apply_subst_to_query_blacklist(Uni, Ctx), QsR0, QsR, BL1, BL),
	trace[compile_time(flag("debug")), io(!IO)] ( print(stderr_stream, "}", !IO) ).

%------------------------------------------------------------------------------%

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
	m(HM, HF) = head_matom(QH),
	m(M, F) = head_matom(Q),
	match(compose_list(HM), compose_list(M)),
	unify_formulas(HF, F, Uni).

solved_unifiable(Q, [_QH|T], Uni) :-
	solved_unifiable(Q, T, Uni).

%------------------------------------------------------------------------------%

:- pred check_disjoints(list(query(M))::in, blacklist(M)::in, blacklist(M)::out) is semidet <= modality(M).

check_disjoints(Qs, BL0, BL) :-
	anytime.pure_signalled(no),
	list.foldl((pred(Q::in, BL1::in, BL2::out) is semidet :-
		(if ground_matom(head_matom(Q), G)
		then check_mgatom(G, BL1, BL2)
		else BL2 = BL1
			)), Qs, BL0, BL).

%------------------------------------------------------------------------------%

:- func head_matom(query(M)) = matom(M) is det <= modality(M).

head_matom(proved(MAtom)) = MAtom.
head_matom(unsolved(MAtom, _)) = MAtom.
head_matom(assumed(MAtom, _)) = MAtom.
head_matom(asserted(prop(MAtom))) = MAtom.
head_matom(asserted(impl(_, MAtom))) = MAtom.

:- pred leftmost_unifiable(matom(M)::in, list(matom(M))::in, subst::out) is semidet.

leftmost_unifiable(m(Mod, Pred), [m(ModH, PredH) | T], Subst) :-
	(if
		Mod = ModH,
		unify_formulas(PredH, Pred, Subst0)
	then
		Subst = Subst0
	else
		leftmost_unifiable(m(Mod, Pred), T, Subst)
	).

%------------------------------------------------------------------------------%

:- func map_fst(func(T) = V, list(pair(T, U))) = list(pair(V, U)).

map_fst(Func, LIn) = LOut :-
	LOut = list.map((func(K-V) = apply(Func, K)-V), LIn).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

:- func apply_subst_to_query(subst, query(M)) = query(M) <= modality(M).

apply_subst_to_query(Subst, unsolved(MAtom, F)) = unsolved(apply_subst_to_matom(Subst, MAtom), F).
apply_subst_to_query(Subst, proved(MAtom)) = proved(apply_subst_to_matom(Subst, MAtom)).
apply_subst_to_query(Subst, assumed(MAtom, F)) = assumed(apply_subst_to_matom(Subst, MAtom), F).
apply_subst_to_query(Subst, asserted(MTest)) = asserted(apply_subst_to_mtest(Subst, MTest)).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

	% TODO: apply blacklist incrementally
:- pred apply_subst_to_query_blacklist(subst::in, C::in, query(M)::in, query(M)::out, blacklist(M)::in, blacklist(M)::out) is det
		<= (modality(M), context(C, M)).

apply_subst_to_query_blacklist(Subst, _Ctx, QIn, QOut, BL, BL) :-
	QOut = apply_subst_to_query(Subst, QIn).

%apply_subst_to_query_blacklist(Subst, Ctx, QIn, QOut, BLIn, BLOut) :-
%	QOut = apply_subst_to_query(Subst, QIn),
%	(if ground_matom(head_matom(QOut), G)
%	then check_mgatom(G, BLIn, BLOut)
%	else BLOut = BLIn
%	).
