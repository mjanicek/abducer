:- module 'abducer-lfd'.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%------------------------------------------------------------------------------%

:- implementation.

:- import_module require, solutions.
:- import_module map, set, list, pair, assoc_list, string, float, int, bag, bool, maybe.
:- import_module utils.
:- import_module abduction, formula, context, costs.
:- import_module loading.

:- import_module context, ctx_modality, ctx_loadable, ctx_io, ctx_loadable_io.
:- import_module modality, stringable.

:- import_module parser, term_io, term, varset, formula_io, formula_ops, costs.
:- import_module gc.
:- import_module protocol.

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

:- type srv_ctx
	--->	srv_ctx(
		cx :: ctx,
		best_proof :: maybe(proof(ctx_modality))
	).

main(!IO) :-
	inner_loop(srv_ctx(new_ctx, no), _, !IO).

:- pred inner_loop(srv_ctx::in, srv_ctx::out, io::di, io::uo) is det.

inner_loop(!Ctx, !IO) :-
	io.read_line_as_string(ReadResult, !IO),
	(
		ReadResult = ok(ReqStr),
		read_term_from_string("ICE request", ReqStr, _Pos, ReadTermResult),
		(
		 	ReadTermResult = term(_Varset, Term),
			(if
				term_to_type(Term, Request),
				generic_term(Term),
				is_request(Request)
			then
				trace[compile_time(flag("debug")), io(!IO)] ( print(stderr_stream, "[abd] got a valid request: " ++ ReqStr, !IO) ),
				process_request(Request, !Ctx, !IO),
				inner_loop(!Ctx, !IO)
			else
				trace[compile_time(flag("debug")), io(!IO)] ( print(stderr_stream, "failed to parse request:" ++ ReqStr ++ "\n", !IO) )
			)
		;
			ReadTermResult = error(_Err, _LineNum),
			trace[compile_time(flag("debug")), io(!IO)] ( print(stderr_stream, "got an error in read_term\n", !IO) )
		;
			ReadTermResult = eof,
			trace[compile_time(flag("debug")), io(!IO)] ( print(stderr_stream, "end of file in read_term\n", !IO) )
		)
	;
		ReadResult = error(_Err),
		trace[compile_time(flag("debug")), io(!IO)] ( print(stderr_stream, "error in read_line\n", !IO) ),
		true
	;
		ReadResult = eof,
		trace[compile_time(flag("debug")), io(!IO)] ( print(stderr_stream, "end of file in read_line\n", !IO) ),
		true
	).

:- pred process_request(protocol.request::in, srv_ctx::in, srv_ctx::out, io::di, io::uo) is det.

process_request(init_ctx, _, srv_ctx(new_ctx, no), !IO) :-
	trace[compile_time(flag("debug")), io(!IO)] ( print(stderr_stream, "[REQUEST] init_ctx\n", !IO) ).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

process_request(load_file(Filename), !SCtx, !IO) :-
	trace[compile_time(flag("debug")), io(!IO)] ( print(stderr_stream, "[REQUEST] load_file\n", !IO) ),
	loading.load_file(Filename, _Result, !.SCtx^cx, NewCtx, !IO),
	!:SCtx = !.SCtx^cx := NewCtx,
	print("ok.\n", !IO),
	flush_output(!IO),
	trace[compile_time(flag("debug")), io(!IO)] ( print(stderr_stream, "[done] load_file\n", !IO) ).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

process_request(clear_rules, !SCtx, !IO) :-
	trace[compile_time(flag("debug")), io(!IO)] ( print(stderr_stream, "[REQUEST] clear_rules\n", !IO) ),
	set_rules(set.init, !.SCtx^cx, NewCtx),
	!:SCtx = !.SCtx^cx := NewCtx,
	trace[compile_time(flag("debug")), io(!IO)] ( print(stderr_stream, "[done] clear_rules\n", !IO) ).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

process_request(clear_facts, !SCtx, !IO) :-
	trace[compile_time(flag("debug")), io(!IO)] ( print(stderr_stream, "[REQUEST] clear_facts\n", !IO) ),
	set_facts(set.init, !.SCtx^cx, NewCtx),
	!:SCtx = !.SCtx^cx := NewCtx,
	trace[compile_time(flag("debug")), io(!IO)] ( print(stderr_stream, "[done] clear_rules\n", !IO) ).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

process_request(clear_facts_by_modality(k), !SCtx, !IO) :-
	trace[compile_time(flag("debug")), io(!IO)] ( print(stderr_stream, "[REQUEST] clear_facts_by_modality(k)\n", !IO) ),
	set_facts(set.filter((pred(vs(m(Mod, _), _)::in) is semidet :-
		Mod \= [k(_, _)|_]
			), !.SCtx^cx^facts), !.SCtx^cx, NewCtx),
	!:SCtx = !.SCtx^cx := NewCtx,
	trace[compile_time(flag("debug")), io(!IO)] ( print(stderr_stream, "[done] clear_facts_by_modality(k)\n", !IO) ).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

process_request(clear_assumables, !SCtx, !IO) :-
	trace[compile_time(flag("debug")), io(!IO)] ( print(stderr_stream, "[REQUEST] clear_assumables\n", !IO) ),
	set_assumables(map.init, !.SCtx^cx, NewCtx),
	!:SCtx = !.SCtx^cx := NewCtx,
	trace[compile_time(flag("debug")), io(!IO)] ( print(stderr_stream, "[done] clear_rules\n", !IO) ).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

process_request(clear_assumable_function(Func), !SCtx, !IO) :-
	trace[compile_time(flag("debug")), io(!IO)] ( print(stderr_stream, "[REQUEST] clear_assumable_function\n", !IO) ),
	set_assumables(map.delete(!.SCtx^cx^assumables, Func), !.SCtx^cx, NewCtx),
	!:SCtx = !.SCtx^cx := NewCtx,
	trace[compile_time(flag("debug")), io(!IO)] ( print(stderr_stream, "[done] clear_assumable_function\n", !IO) ).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

process_request(add_fact(FactStr), !SCtx, !IO) :-
	trace[compile_time(flag("debug")), io(!IO)] ( print(stderr_stream, "[REQUEST] add_fact\n", !IO) ),
	vs(MProp, VS) = det_string_to_vsmprop(FactStr),
	add_fact(vs(MProp, VS), !.SCtx^cx, NewCtx),
	!:SCtx = !.SCtx^cx := NewCtx,
	trace[compile_time(flag("debug")), io(!IO)] ( print(stderr_stream, "[done] add_fact\n", !IO) ).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

process_request(add_assumable(Function, MPropStr, Cost), !SCtx, !IO) :-
	trace[compile_time(flag("debug")), io(!IO)] ( print(stderr_stream, "[REQUEST] add_assumable\n", !IO) ),
	vs(m(Mod, Prop), _VS) = det_string_to_vsmprop(MPropStr),
	Ass = !.SCtx^cx^assumables,
	(if map.search(Ass, Function, Mapping0)
	then Mapping = Mapping0
	else Mapping = map.init
	),
	map.set(Mapping, m(Mod, det_formula_to_ground_formula(Prop)), Cost, MappingNew),
	map.set(Ass, Function, MappingNew, Ass1),
	set_assumables(Ass1, !.SCtx^cx, NewCtx),
	!:SCtx = !.SCtx^cx := NewCtx,
	trace[compile_time(flag("debug")), io(!IO)] ( print(stderr_stream, "[done] add_assumable\n", !IO) ).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

process_request(prove(L), !SCtx, !IO) :-
	trace[compile_time(flag("debug")), io(!IO)] ( print(stderr_stream, "[REQUEST] prove\n", !IO) ),
	list.map((pred(S::in, Q::out) is det :-
		vs(MProp, _VS) = det_string_to_vsmprop(S),
		Q = unsolved(MProp, not_assumable)
			), L, Qs),

	% TODO: merge all varsets
	%varset.merge_renaming(VS0, VSA, VS, Renaming),
	%PA = rename_vars_in_formula(Renaming, PA0),

	P0 = vs(Qs, varset.init),
	is_ctx_proof(P0),

	print(stderr_stream, "facts:\n", !IO),
	print_facts(stderr_stream, !.SCtx^cx, "  ", !IO),
	nl(stderr_stream, !IO),
	print(stderr_stream, "assumables:\n", !IO),
	print_assumables(stderr_stream, !.SCtx^cx, "  ", !IO),
	nl(stderr_stream, !IO),

	Proofs0 = set.to_sorted_list(solutions_set((pred((Cost-P)::out) is nondet :-
		prove(0.0, 100.0, P0, P, default_costs, !.SCtx^cx),
		Cost = cost(!.SCtx^cx, P, default_costs)
			))),

	% examine derivations
/*
	list.foldl((pred((Cost-Gy)::in, M0::in, M::out) is det :-
		(if map.search(M0, Cost-Gy, D0)
		then D1 = D0
		else D1 = set.init
		),
		set.insert(D1, P, D2),
		map.set(M0, Cost-Gy, D2, M)
			), Proofs0, map.init, DerivsMap),
*/

	list.sort((pred((CA-_)::in, (CB-_)::in, Comp::out) is det :-
		float_compare(CA, CB, Comp)
			), Proofs0, Proofs),

	format(stderr_stream, "\n  %d proof(s) found.\n", [i(list.length(Proofs))], !IO),

	list.foldl((pred((Cost-Gz)::in, !.IO::di, !:IO::uo) is det :-
		print(stderr_stream, "---------------------------------------------------------------------\n", !IO),
		format(stderr_stream, "proof cost = %f\n\n", [f(Cost)], !IO),
		print(stderr_stream, "proven goal:\n  " ++ goal_to_string(Gz) ++ "\n", !IO),
		nl(stderr_stream, !IO)

%		print(stderr_stream, "assumptions:\n", !IO),
%		print(stderr_stream, "  " ++ assumptions_to_string(!.Ctx, goal_assumptions(Gz)) ++ "\n", !IO),
%		nl(stderr_stream, !IO),

%		print(stderr_stream, "assertions:\n", !IO),
%		print(stderr_stream, "  " ++ assertions_to_string(!.Ctx, goal_assertions(Gz)) ++ "\n", !IO),
%		nl(stderr_stream, !IO),

/*
		print(stderr_stream, string.from_int(set.count(Ds)) ++ " derivation(s).\n", !IO),

		trace[run_time(env("PRINT_DERIVATIONS")), io(!IO)] (
			print(stderr_stream, "- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -\n", !IO),
			set.fold((pred(Proof::in, !.IO::di, !:IO::uo) is det :-
				is_ctx_proof(Proof),
				print_proof_trace(stderr_stream, !.SCtx^cx, Proof, !IO),
				nl(stderr_stream, !IO)
					), Ds, !IO)
		)
*/
			), Proofs, !IO),

	(if
		Proofs = [(_Cost-G)|_]
	then
		Response = "success",
		!:SCtx = !.SCtx^best_proof := yes(G)
	else
		Response = "failure",
		!:SCtx = !.SCtx^best_proof := no
	),

	print(Response ++ ".\n", !IO),
	flush_output(!IO),
	trace[compile_time(flag("debug")), io(!IO)] ( print(stderr_stream, "[done] prove\n", !IO) ).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

process_request(get_best_proof, !SCtx, !IO) :-
	trace[compile_time(flag("debug")), io(!IO)] ( print(stderr_stream, "[REQUEST] get_best_proof\n", !IO) ),
	(
		!.SCtx^best_proof = yes(vs(Qs, VS)),
		format("%d\n", [i(list.length(Qs))], !IO),
		flush_output(!IO),
		list.foldl((pred(Q::in, !.IO::di, !:IO::uo) is det :-
			dissect_query(Q) = Marking-MProp,
			print(Marking ++ mprop_to_string(VS, MProp) ++ ".\n", !IO)
				), Qs, !IO),
		flush_output(!IO)
	;
		!.SCtx^best_proof = no,
		print("0\n", !IO),
		flush_output(!IO)
	),
	trace[compile_time(flag("debug")), io(!IO)] ( print(stderr_stream, "[done] get_best_proof\n", !IO) ).

%------------------------------------------------------------------------------%

:- func dissect_query(query(ctx_modality)) = pair(string, mprop(ctx_modality)).

dissect_query(proved(MProp)) = "P"-MProp.
dissect_query(unsolved(MProp, _)) = "U"-MProp.
dissect_query(assumed(MProp, _)) = "A"-MProp.
dissect_query(asserted(prop(MProp))) = "R"-MProp.
dissect_query(asserted(impl(_, MProp))) = "R"-MProp.

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

:- func default_costs = costs.

default_costs = costs(1.0, 1.0).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

:- pred is_ctx_proof(proof(ctx_modality)::in) is det.

is_ctx_proof(_).
