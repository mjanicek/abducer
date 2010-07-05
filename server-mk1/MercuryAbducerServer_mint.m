:- module 'MercuryAbducerServer_mint'.

:- interface.
:- import_module io.
:- import_module float.
:- import_module ctx_loadable, ctx_modality, abduction, formula.
:- import_module loading.
:- import_module varset.

:- func srv_init_ctx = ctx.

:- pred srv_clear_rules(ctx::in, ctx::out) is det.
:- pred srv_load_file(string::in, loading.load_result::out, ctx::in, ctx::out, io::di, io::uo) is det.

:- pred srv_clear_facts(ctx::in, ctx::out) is det.
:- pred srv_clear_e_facts(ctx::in, ctx::out) is det.
:- pred srv_clear_a_facts(ctx::in, ctx::out) is det.
:- pred srv_clear_i_facts(ctx::in, ctx::out) is det.
:- pred srv_clear_k_facts(ctx::in, ctx::out) is det.

:- pred srv_add_mprop_fact(varset::in, mprop(ctx_modality)::in, ctx::in, ctx::out) is det.

:- pred srv_clear_assumables(ctx::in, ctx::out) is det.
:- pred srv_add_assumable(string::in, mprop(ctx_modality)::in, float::in, ctx::in, ctx::out) is det.

%:- pred srv_prove_best(string::in, float::in, ctx::in, float::out, proof(ctx_modality)::out) is semidet.
:- pred srv_prove_best(proof(ctx_modality)::in, ctx::in, float::out, proof(ctx_modality)::out) is semidet.
:- pred srv_print_ctx(ctx::in, io::di, io::uo) is det.
:- pred srv_proof_summary(proof(ctx_modality)::in, ctx::in, io::di, io::uo) is det.

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

:- pred load_result_is_ok(load_result::in) is semidet.
:- pred load_result_is_file_read_error(load_result::in) is semidet.
:- pred load_result_is_syntax_error(load_result::in, string::out, int::out) is semidet.

%------------------------------------------------------------------------------%

:- implementation.
:- import_module set, map, list, bool, string, pair, bag, assoc_list, list.
:- import_module utils.
:- import_module term, term_io, formula.
:- import_module formula_io, formula_ops, ctx_io.
:- import_module solutions, require.
:- import_module varset, costs.
:- import_module modality, stringable, context, costs.
:- import_module belief_model, lf, stf.
:- import_module lf_io.
:- import_module varset.
:- import_module ctx_loadable_io.

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

:- pragma foreign_export("C", srv_init_ctx = out, "init_ctx").

srv_init_ctx = new_ctx.

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

:- func default_costs = costs.

default_costs = costs(1.0, 1.0, 0.1).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

:- pragma foreign_export("C", srv_print_ctx(in, di, uo), "print_ctx").

srv_print_ctx(C, !IO) :-
   	print_ctx(C, !IO).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

:- pragma foreign_export("C", srv_clear_rules(in, out), "clear_rules").

srv_clear_rules(!Ctx) :-
	set_rules(set.init, !Ctx).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

:- pragma foreign_export("C", srv_load_file(in, out, in, out, di, uo), "load_file").

srv_load_file(Filename, Result, !Ctx, !IO) :-
	loading.load_file(Filename, Result, !Ctx, !IO).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

	% TODO: full coverage

:- pragma foreign_export("C", srv_clear_facts(in, out), "clear_facts").
:- pragma foreign_export("C", srv_clear_e_facts(in, out), "clear_e_facts").
:- pragma foreign_export("C", srv_clear_a_facts(in, out), "clear_a_facts").
:- pragma foreign_export("C", srv_clear_i_facts(in, out), "clear_i_facts").
:- pragma foreign_export("C", srv_clear_k_facts(in, out), "clear_k_facts").

srv_clear_facts(!Ctx) :-
	set_facts(set.init, !Ctx).

srv_clear_e_facts(!Ctx) :-
	set_facts(set.filter((pred(vs(m(Mod, _), _)::in) is semidet :-
		Mod \= [evt|_]
			), !.Ctx^facts), !Ctx).

srv_clear_a_facts(!Ctx) :-
	set_facts(set.filter((pred(vs(m(Mod, _), _)::in) is semidet :-
		Mod \= [att|_]
			), !.Ctx^facts), !Ctx).

srv_clear_i_facts(!Ctx) :-
	set_facts(set.filter((pred(vs(m(Mod, _), _)::in) is semidet :-
		Mod \= [info|_]
			), !.Ctx^facts), !Ctx).

srv_clear_k_facts(!Ctx) :-
	set_facts(set.filter((pred(vs(m(Mod, _), _)::in) is semidet :-
		Mod \= [k(_, _)|_]
			), !.Ctx^facts), !Ctx).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

:- pragma foreign_export("C", srv_add_mprop_fact(in, in, in, out), "add_mprop_fact").

srv_add_mprop_fact(VS, MProp, !Ctx) :-
	add_fact(vs(MProp, VS), !Ctx).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

:- pragma foreign_export("C", srv_clear_assumables(in, out), "clear_assumables").

srv_clear_assumables(!Ctx) :-
	set_assumables(map.init, !Ctx).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

:- pragma foreign_export("C", srv_add_assumable(in, in, in, in, out), "add_assumable").

srv_add_assumable(Function, m(Mod, Prop), Cost, !Ctx) :-
	Ass = !.Ctx^assumables,
	(if map.search(Ass, Function, Mapping0)
	then Mapping = Mapping0
	else Mapping = map.init
	),
	map.set(Mapping, m(Mod, det_formula_to_ground_formula(Prop)), Cost, MappingNew),
	map.set(Ass, Function, MappingNew, Ass1),
	set_assumables(Ass1, !Ctx).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

:- import_module gc.

:- pragma foreign_export("C", srv_prove_best(in, in, out, out), "prove_best").

srv_prove_best(P0, Ctx, ProofCost, Proof) :-
	trace[compile_time(flag("debug")), io(!IO)] (print(stderr_stream, "in prove_best\n", !IO)),

	%trace[io(!IO)] ( garbage_collect(!IO) ),

%	VSMProp = vs(InitMProp, InitVarset),
%	vs(InitMProp, InitVarset) = det_string_to_vsmprop(GoalStr),

%	P0 = proof(vs([list.map((func(cf(MProp, Func)) = unsolved(MProp, Func)), AnnotMProps)], VS), []),

	Proofs0 = set.to_sorted_list(solutions_set((pred(Cost-P::out) is nondet :-
		prove(0.0, 200.0, P0, P, default_costs, Ctx),
		Cost = cost(Ctx, P, default_costs)
			))),

	trace[io(!IO)] (format("%d proofs found.\n", [i(list.length(Proofs0))], !IO)),

	list.sort((pred(CA-_::in, CB-_::in, Comp::out) is det :-
		float_compare(CA, CB, Comp)
			), Proofs0, [ProofCost-Proof|_]),
	trace[compile_time(flag("debug")), io(!IO)] (print(stderr_stream, "done prove_best\n", !IO)).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

:- pragma foreign_export("C", srv_proof_summary(in, in, di, uo), "proof_summary").

srv_proof_summary(Proof, Ctx, !IO) :-
	LastGoal = last_goal(Proof),

	print_ctx(Ctx, !IO),

	format("proof cost = %f\n\n", [f(cost(Ctx, Proof, default_costs))], !IO),
	print("proven goal:\n  " ++ goal_to_string(LastGoal) ++ "\n", !IO),
	nl(!IO),

	print("assumptions:\n", !IO),
	print("  " ++ assumptions_to_string(Ctx, goal_assumptions(LastGoal)) ++ "\n", !IO),
	nl(!IO),

	print("assertions:\n", !IO),
	print("  " ++ assertions_to_string(Ctx, goal_assertions(LastGoal)) ++ "\n", !IO),
	nl(!IO).

%	print_proof_trace(Ctx, Proof, !IO),
%	nl(!IO),
%	print("that's it for the summary.\n", !IO).

%------------------------------------------------------------------------------%

:- pragma foreign_export("C", load_result_is_ok(in), "load_result_is_ok").
:- pragma foreign_export("C", load_result_is_file_read_error(in), "load_result_is_file_read_error").
:- pragma foreign_export("C", load_result_is_syntax_error(in, out, out), "load_result_is_syntax_error").

load_result_is_ok(ok).
load_result_is_file_read_error(file_read_error).
load_result_is_syntax_error(syntax_error(Msg, Line), Msg, Line).
