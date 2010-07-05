% $Id: test_abduction.m 2485 2009-09-20 00:32:15Z janicek $

:- module test_abduction.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

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

main(!IO) :-
	io.command_line_arguments(CmdArgs, !IO),
	(if
		CmdArgs = [Goal, GoalAssumeCost],
		string.to_float(GoalAssumeCost, InitAssumeCost)
	then
		some [!Ctx] (
			!:Ctx = new_ctx,

%			read_file_as_lines(FileName, Strs0, !IO),
%			preprocess_file(Strs0, Strs),

			do_while((pred(Continue::out, !.Ctx::in, !:Ctx::out, !.IO::di, !:IO::uo) is det :-
				term_io.read_term_with_op_table(init_wabd_op_table, ReadResult, !IO),
				(
					ReadResult = term(VS, Term),
					generic_term(Term),
					(if term_to_assumable_function_def(Term, AssumFuncDef)
					then add_assumable(AssumFuncDef, !Ctx), Continue = yes
					else
						(if term_to_mrule(Term, MRule)
						then add_rule(vs(MRule, VS), !Ctx), Continue = yes
						else
							(if term_to_mprop(Term, MProp)
							then add_fact(vs(MProp, VS), !Ctx), Continue = yes
							else
								context(FileName, Line) = get_term_context(Term),
								error("Syntax error in " ++ FileName
										++ " at line " ++ string.from_int(Line) ++ ".")
							)
						)
					)
				;
					ReadResult = error(Message, Linenumber),
					error(Message ++ " at line " ++ string.from_int(Linenumber) ++ ".")
				;
					ReadResult = eof,
					Continue = no
				)
					), !Ctx, !IO),

			vs(InitMProp, InitVarset) = det_string_to_vsmprop(Goal),

			P0 = proof(vs([[unsolved(InitMProp, const(InitAssumeCost))]], InitVarset), []),

			format("goal:\n  %s\n\n", [s(vsmprop_to_string(vs(InitMProp, InitVarset)))], !IO),

			print_ctx(!.Ctx, !IO),

			nl(!IO),

%			DC0 = new_d_ctx,

			Proofs0 = set.to_sorted_list(solutions_set((pred((Cost-G)-P::out) is nondet :-
				Costs = costs(1.0, 0.1, 0.1),
				prove(0.0, 11.0, P0, P, Costs, !.Ctx),
				G = last_goal(P),
				Cost = cost(!.Ctx, P, Costs)
					))),

			% TODO: derivations
			% deriv: map(proved_goal, set(list(steps)))

			list.foldl((pred((Cost-G)-P::in, M0::in, M::out) is det :-
				(if map.search(M0, Cost-G, D0)
				then D1 = D0
				else D1 = set.init
				),
				set.insert(D1, P, D2),
				map.set(M0, Cost-G, D2, M)
					), Proofs0, map.init, DerivsMap),

			list.sort((pred((CA-_)-_::in, (CB-_)-_::in, Comp::out) is det :-
				float_compare(CA, CB, Comp)
					), map.to_assoc_list(DerivsMap), DerivsSorted),

			format("found %d proofs.\n", [i(length(DerivsSorted))], !IO),

			list.foldl((pred((Cost-G)-Ds::in, !.IO::di, !:IO::uo) is det :-
				print("---------------------------------------------------------------------\n", !IO),
				format("proof cost = %f\n\n", [f(Cost)], !IO),
				print("proven goal:\n  " ++ goal_to_string(G) ++ "\n", !IO),
				nl(!IO),

				print("assumptions:\n", !IO),
				print("  " ++ assumptions_to_string(!.Ctx, goal_assumptions(G)) ++ "\n", !IO),
				nl(!IO),

				print("assertions:\n", !IO),
				print("  " ++ assertions_to_string(!.Ctx, goal_assertions(G)) ++ "\n", !IO),
				nl(!IO),

				print(string.from_int(set.count(Ds)) ++ " derivation" ++ plural_s(count(Ds)) ++ ".\n", !IO),

				print("- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -\n", !IO),

				set.fold((pred(Proof::in, !.IO::di, !:IO::uo) is det :-
					is_ctx_proof(Proof),
					print_proof_trace(!.Ctx, Proof, !IO),
					nl(!IO)
						), Ds, !IO)

					), DerivsSorted, !IO)
		)
	else
		io.progname("?", ProgName, !IO),
		format(stderr_stream, "Usage: %s GOAL GOAL_ASSUMPTION_COST < FILE\n", [s(ProgName)], !IO)
	).

%------------------------------------------------------------------------------%

:- func plural_s(int) = string.

plural_s(N) = S :-
	(if N > 1
	then S = "s"
	else S = ""
	).

%------------------------------------------------------------------------------%

:- pred term_to_assumable_function_def(term.term::in, assumable_function_def(M)::out) is semidet
		<= (modality(M), term_parsable(M)).

term_to_assumable_function_def(functor(atom("="), [FuncNameTerm, DefTerms], _), FuncDef) :-
	FuncNameTerm = functor(atom(FuncName), [], _),
	term_list(DefTerms, ListCostTerms),
	list.map((pred(AssignTerm::in, MGProp-Cost::out) is semidet :-
		AssignTerm = functor(atom("="), [MPropTerm, CostTerm], _),
		term_to_mprop(MPropTerm, m(Mod, Prop)),
		ground_formula(Prop, GProp),
		MGProp = m(Mod, GProp),
		CostTerm = functor(float(Cost), [], _)
			), ListCostTerms, Costs),
	FuncDef = FuncName-map.from_assoc_list(Costs).

:- pred term_list(term.term::in, list(term.term)::out) is semidet.

term_list(functor(atom("[]"), [], _), []).
term_list(functor(atom("[|]"), [HeadTerm, TailTerms], _), [HeadTerm | Tail]) :-
	term_list(TailTerms, Tail).

%------------------------------------------------------------------------------%

:- pred is_ctx_proof(proof(ctx_modality)::in) is det.

is_ctx_proof(_).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

:- pred print_ctx(ctx::in, io::di, io::uo) is det.

print_ctx(Ctx, !IO) :-
	print("facts:\n", !IO),
	set.fold((pred(Fact::in, !.IO::di, !:IO::uo) is det :-
		print("  ", !IO),
		print(vsmprop_to_string(Fact), !IO),
		nl(!IO)
			), facts(Ctx), !IO),

	nl(!IO),

	print("assumables:\n", !IO),
	map.foldl((pred(FuncName::in, Costs::in, !.IO::di, !:IO::uo) is det :-
		print("  ", !IO),
		print(FuncName ++ " = ", !IO),

		CostStrs = list.map((func(m(Mod, GProp)-Cost) = S :-
			S = vsmprop_to_string(vs(m(Mod, ground_formula_to_formula(GProp)), varset.init))
					++ " = " ++ float_to_string(Cost)
				), map.to_assoc_list(Costs)),

		print("[\n    " ++ string.join_list(",\n    ", CostStrs) ++ "\n  ].\n", !IO)
			), assumables(Ctx), !IO),

	nl(!IO),

	print("rules:\n", !IO),
	set.fold((pred(Rule::in, !.IO::di, !:IO::uo) is det :-
			% XXX global context
		print("  ", !IO),
		print(vsmrule_to_string(Rule), !IO),
		nl(!IO)
			), rules(Ctx), !IO).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

:- func step_to_string(step(M)) = string <= (modality(M), stringable(M)).

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

:- func query_to_string(varset, query(ctx_modality)) = string.

query_to_string(VS, unsolved(MProp, F)) = mprop_to_string(VS, MProp)
		++ "[unsolved / " ++ cost_function_to_string(F) ++ "]".
query_to_string(VS, proved(MProp)) = mprop_to_string(VS, MProp) ++ "[proved]".
query_to_string(VS, assumed(MProp, F)) = mprop_to_string(VS, MProp)
		++ "[assumed / " ++ cost_function_to_string(F) ++ "]".
query_to_string(VS, asserted(MTest)) = mtest_to_string(VS, MTest) ++ "[asserted]".

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

:- func goal_to_string(vscope(list(query(ctx_modality)))) = string.

goal_to_string(vs(G, VS)) = string.join_list(",\n  ", list.map(query_to_string(VS), G)).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

:- func assumptions_to_string(ctx, bag(with_cost_function(mgprop(ctx_modality)))) = string.

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

:- func assertions_to_string(ctx, bag(vscope(mtest(ctx_modality)))) = string.

assertions_to_string(_Ctx, As) = Str :-
	(if not As = bag.init
	then Str = string.join_list("\n  ", list.map((func(vs(MTest, VS)) = mtest_to_string(VS, MTest)),
			bag.to_list(As)))
	else Str = "(none)"
	).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

%:- pred print_proof(proof(M)::in, io::di, io::uo) is det <= (modality(M), stringable(M)).
:- pred print_proof_trace(ctx::in, proof(ctx_modality)::in, io::di, io::uo) is det.

print_proof_trace(_Ctx, Proof, !IO) :-
%	vs(LastGoal, Varset) = last_goal(Proof),

%	print("Proven goal:\n", !IO),
%	print("  " ++ goal_to_string(last_goal(Proof)) ++ "\n", !IO),

%	nl(!IO),

%	print("assumptions:\n", !io),
%	print("  " ++ assumptions_to_string(ctx, goal_assumptions(last_goal(proof))) ++ "\n", !io),

%	nl(!IO),

	print("proof trace:\n", !IO),
	Proof^p_goals = vs(RevGoals, Varset0),
	Qs = reverse(RevGoals),
	InitQs = det_head(Qs),
	RemQss = det_tail(Qs),

	print("  " ++ proof_state_to_string(Varset0, InitQs) ++ "\n", !IO),

	GoalsStr = list.map((func(Step-Goal) = GStr :-
		GStr = "    --->\n"
				++ ">>  " ++ step_to_string(Step) ++ "\n  " ++ proof_state_to_string(Varset0, Goal)
				), from_corresponding_lists(reverse(Proof^p_steps), RemQss)),
	print(string.join_list("\n", GoalsStr) ++ "\n", !IO).

/*
	print("Steps:\n", !IO),
	print("  " ++ string.join_list("\n  ", list.map(step_to_string, list.reverse(Proof^p_steps))), !IO),
	nl(!IO).
*/

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

:- func proof_state_to_string(varset, list(query(ctx_modality))) = string.

proof_state_to_string(Varset, L) = S :-
	S = string.join_list(",\n  ", list.map(query_to_string(Varset), L)).
