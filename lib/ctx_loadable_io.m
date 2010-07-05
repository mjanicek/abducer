% $Id: test_abduction.m 2485 2009-09-20 00:32:15Z janicek $

:- module ctx_loadable_io.

:- interface.

:- import_module io, list, varset, bag.
:- import_module abduction, ctx_modality, costs, formula, stringable, modality.
:- import_module ctx_loadable.

:- pred print_facts(ctx::in, string::in, io::di, io::uo) is det.
:- pred print_facts(io.output_stream::in, ctx::in, string::in, io::di, io::uo) is det.

:- pred print_assumables(ctx::in, string::in, io::di, io::uo) is det.
:- pred print_assumables(io.output_stream::in, ctx::in, string::in, io::di, io::uo) is det.

:- pred print_rules(ctx::in, string::in, io::di, io::uo) is det.
:- pred print_rules(io.output_stream::in, ctx::in, string::in, io::di, io::uo) is det.

:- pred print_ctx(ctx::in, io::di, io::uo) is det.
:- pred print_ctx(io.output_stream::in, ctx::in, io::di, io::uo) is det.

:- pred print_proof_trace(ctx::in, proof(ctx_modality)::in, io::di, io::uo) is det.
:- pred print_proof_trace(io.output_stream::in, ctx::in, proof(ctx_modality)::in, io::di, io::uo) is det.

:- func assumptions_to_string(ctx, bag(with_cost_function(mgprop(ctx_modality)))) = string.
:- func assertions_to_string(ctx, bag(vscope(mtest(ctx_modality)))) = string.
:- func goal_to_string(vscope(list(query(ctx_modality)))) = string.
:- func step_to_string(step(M)) = string <= (modality(M), stringable(M)).
:- func query_to_string(varset, query(ctx_modality)) = string.
:- func proof_state_to_string(varset, list(query(ctx_modality))) = string.

%------------------------------------------------------------------------------%

:- implementation.

:- import_module require, solutions.
:- import_module map, set, list, pair, assoc_list, string, float, int, bag, bool.
:- import_module utils.
:- import_module abduction, formula, context, costs.

:- import_module ctx_modality, ctx_loadable, ctx_io.
:- import_module modality, stringable.

:- import_module parser, term_io, term, varset, formula_io, formula_ops, costs.

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

print_facts(Stream, Ctx, Indent, !IO) :-
	set.fold((pred(Fact::in, !.IO::di, !:IO::uo) is det :-
		print(Stream, Indent, !IO),
		print(Stream, vsmprop_to_string(Fact), !IO),
		nl(Stream, !IO)
			), facts(Ctx), !IO).

print_facts(Ctx, Indent, !IO) :-
	print_facts(stdout_stream, Ctx, Indent, !IO).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

print_rules(Stream, Ctx, Indent, !IO) :-
	set.fold((pred(Rule::in, !.IO::di, !:IO::uo) is det :-
			% XXX global context
		print(Stream, Indent, !IO),
		print(Stream, vsmrule_to_string(Rule), !IO),
		nl(Stream, !IO)
			), rules(Ctx), !IO).

print_rules(Ctx, Indent, !IO) :-
	print_rules(stdout_stream, Ctx, Indent, !IO).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

print_assumables(Stream, Ctx, Indent, !IO) :-
	map.foldl((pred(FuncName::in, Costs::in, !.IO::di, !:IO::uo) is det :-
		print(Stream, Indent, !IO),
		print(Stream, FuncName ++ " = ", !IO),

		CostStrs = list.map((func(m(Mod, GProp)-Cost) = S :-
			S = vsmprop_to_string(vs(m(Mod, ground_formula_to_formula(GProp)), varset.init))
					++ " = " ++ float_to_string(Cost)
				), map.to_assoc_list(Costs)),

		print(Stream, "[\n    " ++ string.join_list(",\n" ++ Indent ++ Indent, CostStrs) ++ "\n" ++ Indent ++ "].\n", !IO)
			), assumables(Ctx), !IO).

print_assumables(Ctx, Indent, !IO) :-
	print_assumables(stdout_stream, Ctx, Indent, !IO).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

print_ctx(Stream, Ctx, !IO) :-
	print(Stream, "facts:\n", !IO),
	print_facts(Stream, Ctx, "  ", !IO),

	nl(Stream, !IO),

	print(Stream, "assumables:\n", !IO),
	print_assumables(Stream, Ctx, "  ", !IO),

	nl(Stream, !IO),

	print(Stream, "rules:\n", !IO),
	print_rules(Stream, Ctx, "  ", !IO).

print_ctx(Ctx, !IO) :-
	print_ctx(stdout_stream, Ctx, !IO).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

print_proof_trace(Ctx, Proof, !IO) :-
	print_proof_trace(stdout_stream, Ctx, Proof, !IO).

print_proof_trace(Stream, _Ctx, Proof, !IO) :-
	true.
/*
	print(Stream, "proof trace:\n", !IO),
	Proof^p_goals = vs(RevGoals, Varset0),
	Qs = reverse(RevGoals),
	InitQs = det_head(Qs),
	RemQss = det_tail(Qs),

	print(Stream, "  " ++ proof_state_to_string(Varset0, InitQs) ++ "\n", !IO),

	GoalsStr = list.map((func(Step-Goal) = GStr :-
		GStr = "    --->\n"
				++ ">>  " ++ step_to_string(Step) ++ "\n  " ++ proof_state_to_string(Varset0, Goal)
				), from_corresponding_lists(reverse(Proof^p_steps), RemQss)),
	print(Stream, string.join_list("\n", GoalsStr) ++ "\n", !IO).
*/

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

step_to_string(assume(vs(MProp, Varset), Subst, F)) = "assume("
		++ vsmprop_to_string(vs(MProp, Varset)) ++ "), "
		++ subst_to_string(Varset, Subst) ++ ", cost=" ++ cost_function_to_string(F).
step_to_string(resolve_rule(vs(MRule, Varset), Subst)) = "resolve_rule("
		++ vsmrule_to_string(vs(MRule, Varset)) ++ "), " ++ subst_to_string(Varset, Subst).
step_to_string(use_fact(vs(MProp, Varset), Subst)) = "use_fact("
		++ vsmprop_to_string(vs(MProp, Varset)) ++ "), " ++ subst_to_string(Varset, Subst)
		++ ", cost=1.0".  % XXX DON'T have this hard-wired here!!!
step_to_string(factor(Subst, Varset)) = "factor, " ++ subst_to_string(Varset, Subst).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

query_to_string(VS, unsolved(MProp, F)) = mprop_to_string(VS, MProp)
		++ "[unsolved / " ++ cost_function_to_string(F) ++ "]".
query_to_string(VS, proved(MProp)) = mprop_to_string(VS, MProp) ++ "[proved]".
query_to_string(VS, assumed(MProp, F)) = mprop_to_string(VS, MProp)
		++ "[assumed / " ++ cost_function_to_string(F) ++ "]".
query_to_string(VS, asserted(MTest)) = mtest_to_string(VS, MTest) ++ "[asserted]".

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

goal_to_string(vs(G, VS)) = string.join_list(",\n  ", list.map(query_to_string(VS), G)).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

assumptions_to_string(Ctx, As) = Str :-
	(if not As = bag.init
	then
		Str = string.join_list("\n  ", list.map((func(cf(m(Mod, GProp), Func)) = S :-
			MProp = m(Mod, ground_formula_to_formula(GProp)),
			Cost = cost(Ctx, Func, vs(MProp, varset.init)),
			S = mprop_to_string(varset.init, MProp)
					++ " / " ++ cost_function_to_string(Func) ++ " = " ++ float_to_string(Cost)
				), bag.to_list(As)))
	else
		Str = "(none)"
	).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

assertions_to_string(_Ctx, As) = Str :-
	(if not As = bag.init
	then Str = string.join_list("\n  ", list.map((func(vs(MTest, VS)) = mtest_to_string(VS, MTest)),
			bag.to_list(As)))
	else Str = "(none)"
	).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

proof_state_to_string(Varset, L) = S :-
	S = string.join_list(",\n  ", list.map(query_to_string(Varset), L)).
