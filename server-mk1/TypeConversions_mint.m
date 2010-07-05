:- module 'TypeConversions_mint'.

:- interface.
:- import_module float, list, bool.
:- import_module ctx_modality, abduction, formula, belief_model, costs.
:- import_module varset.

:- pred is_function_term(formula.term::in, string::out, list(formula.term)::out) is semidet.
:- pred is_variable_term(varset::in, formula.term::in, string::out) is semidet.

:- pred new_with_const_cost_function(mprop(ctx_modality)::in, float::in, with_cost_function(mprop(ctx_modality))::out) is det.

:- pred new_mprop(list(ctx_modality)::in, atomic_formula::in, mprop(ctx_modality)::out, varset::in, varset::out) is det.

:- pred new_atomic_formula(string::in, list(formula.term)::in, atomic_formula::out, varset::in, varset::out) is det.
:- pred new_function_term(string::in, list(formula.term)::in, formula.term::out, varset::in, varset::out) is det.
:- pred new_variable_term(string::in, formula.term::out, varset::in, varset::out) is det.

:- pred new_varset(varset::out) is det.

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

:- pred empty_term_list(list(formula.term)::out) is det.
:- pred cons_term_list(formula.term::in, list(formula.term)::in, list(formula.term)::out) is det.

:- pred empty_mprop_list(list(mprop(ctx_modality))::out) is det.
:- pred cons_mprop_list(mprop(ctx_modality)::in, list(mprop(ctx_modality))::in, list(mprop(ctx_modality))::out) is det.

:- pred empty_marked_query_list(list(query(ctx_modality))::out) is det.
:- pred cons_marked_query_list(query(ctx_modality)::in, list(query(ctx_modality))::in, list(query(ctx_modality))::out) is det.

:- pred empty_ctx_modality_list(list(ctx_modality)::out) is det.
:- pred cons_ctx_modality_list(ctx_modality::in, list(ctx_modality)::in, list(ctx_modality)::out) is det.

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

:- pred modality_understanding(ctx_modality::out) is det.
:- pred modality_generation(ctx_modality::out) is det.
:- pred modality_event(ctx_modality::out) is det.
:- pred modality_intention(ctx_modality::out) is det.
:- pred modality_info(ctx_modality::out) is det.
:- pred modality_att(ctx_modality::out) is det.
:- pred modality_k_private(agent::in, ctx_modality::out) is det.
:- pred modality_k_attrib(agent::in, agent::in, ctx_modality::out) is det.
:- pred modality_k_mutual(ctx_modality::out) is det.

:- pred impure_print_list_modalities(list(ctx_modality)::in) is det.
:- pred impure_print_modality(ctx_modality::in) is det.

:- pred is_modality_understanding(ctx_modality::in) is semidet.
:- pred is_modality_generation(ctx_modality::in) is semidet.
:- pred is_modality_event(ctx_modality::in) is semidet.
:- pred is_modality_intention(ctx_modality::in) is semidet.
:- pred is_modality_info(ctx_modality::in) is semidet.
:- pred is_modality_att(ctx_modality::in) is semidet.
:- pred is_modality_k(ctx_modality::in, belief::out) is semidet.

:- pred agent_robot(agent::out) is det.
:- pred agent_human(agent::out) is det.

:- pred belief_private(belief::out) is det.

:- pred is_belief_private(belief::in, string::out) is semidet.
:- pred is_belief_attrib(belief::in, string::out, string::out) is semidet.
:- pred is_belief_mutual(belief::in, list(string)::out) is semidet.

%------------------------------------------------------------------------------%

:- pred const_cost_function(float::in, cost_function::out) is det.
:- pred named_cost_function(string::in, cost_function::out) is det.

:- pred is_const_cost_function(cost_function::in, float::out) is semidet.
:- pred is_named_cost_function(cost_function::in, string::out) is semidet.

%------------------------------------------------------------------------------%

:- pred proved_query(mprop(ctx_modality)::in, query(ctx_modality)::out) is det.
:- pred unsolved_query(mprop(ctx_modality)::in, cost_function::in, query(ctx_modality)::out) is det.
:- pred assumed_query(mprop(ctx_modality)::in, cost_function::in, query(ctx_modality)::out) is det.
:- pred asserted_query(mprop(ctx_modality)::in, list(mprop(ctx_modality))::in, query(ctx_modality)::out) is det.

:- pred is_proved_query(query(ctx_modality)::in, mprop(ctx_modality)::out) is semidet.
:- pred is_unsolved_query(query(ctx_modality)::in, mprop(ctx_modality)::out, cost_function::out) is semidet.
:- pred is_assumed_query(query(ctx_modality)::in, mprop(ctx_modality)::out, cost_function::out) is semidet.
:- pred is_asserted_query(query(ctx_modality)::in, mprop(ctx_modality)::out, list(mprop(ctx_modality))::out) is semidet.

%------------------------------------------------------------------------------%

:- pred dissect_term(varset::in, formula.term::in, bool::out, string::out, list(formula.term)::out) is det.
:- pred dissect_predicate(varset::in, atomic_formula::in, string::out, list(formula.term)::out) is det.
:- pred dissect_mprop(mprop(ctx_modality)::in, list(ctx_modality)::out, atomic_formula::out) is det.
:- pred dissect_proof(proof(ctx_modality)::in, varset::out, list(query(ctx_modality))::out) is det.

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

:- pred new_proof(list(query(ctx_modality))::in, varset::in, proof(ctx_modality)::out) is det.

%------------------------------------------------------------------------------%

:- implementation.
:- import_module io.
:- import_module set, map, list, bool, string, pair, bag, assoc_list.
:- import_module utils.
:- import_module term, term_io, formula.
:- import_module formula_io, formula_ops, ctx_io.
:- import_module solutions, require.
:- import_module varset, costs.
:- import_module modality, stringable, context, costs.
:- import_module belief_model, lf, stf.
:- import_module lf_io.
:- import_module varset.

:- pred print_err(string::in, io::di, io::uo) is det.

print_err(Str, !IO) :-
	print(stderr_stream, Str, !IO).

%------------------------------------------------------------------------------%

:- pragma foreign_export("C", is_function_term(in, out, out), "is_function_term").

is_function_term(t(Functor, Args), Functor, Args).

:- pragma foreign_export("C", is_variable_term(in, in, out), "is_variable_term").

is_variable_term(VS, v(Var), VarName) :-
	varset.lookup_name(VS, Var, "V_", VarName).

%------------------------------------------------------------------------------%

:- pragma foreign_export("C", new_mprop(in, in, out, in, out), "new_mprop").

new_mprop(Mod, F, MProp, !VS) :-
	MProp = m(Mod, F).
%	trace [io(!IO)] (print_err("MProp = " ++ string(MProp), !IO), nl(!IO) ).

:- pragma foreign_export("C", new_atomic_formula(in, in, out, in, out), "new_atomic_formula").

new_atomic_formula(PredSym, Args, F, !VS) :-
	F = p(PredSym, Args).
%	trace [io(!IO)] (print_err("Atomic formula = " ++ string(F), !IO), nl(!IO) ).

:- pragma foreign_export("C", new_function_term(in, in, out, in, out), "new_function_term").

new_function_term(Functor, Args, T, !VS) :-
	T = t(Functor, Args).
%	trace [io(!IO)] (print_err("Term = " ++ string(T), !IO), nl(!IO) ).

:- pragma foreign_export("C", new_variable_term(in, out, in, out), "new_variable_term").

new_variable_term(Name, T, !VS) :-
	varset.create_name_var_map(!.VS, VarNames),
	(if map.search(VarNames, Name, Var0)
	then Var = Var0
	else varset.new_named_var(!.VS, Name, Var, !:VS)
	),
	T = v(Var).
%	trace [io(!IO)] (print_err("Var = " ++ string(T), !IO), nl(!IO) ).

:- pragma foreign_export("C", new_varset(out), "new_varset").

new_varset(varset.init).

:- pragma foreign_export("C", new_with_const_cost_function(in, in, out), "new_with_const_cost_function").

new_with_const_cost_function(MProp, Cost, cf(MProp, const(Cost))).

%------------------------------------------------------------------------------%

:- pragma foreign_export("C", empty_term_list(out), "empty_term_list").
:- pragma foreign_export("C", cons_term_list(in, in, out), "cons_term_list").

empty_term_list([]).
cons_term_list(H, T, [H|T]).

:- pragma foreign_export("C", empty_mprop_list(out), "empty_mprop_list").
:- pragma foreign_export("C", cons_mprop_list(in, in, out), "cons_mprop_list").

empty_mprop_list([]).
cons_mprop_list(H, T, [H|T]).

:- pragma foreign_export("C", empty_marked_query_list(out), "empty_marked_query_list").
:- pragma foreign_export("C", cons_marked_query_list(in, in, out), "cons_marked_query_list").

empty_marked_query_list([]).
cons_marked_query_list(H, T, [H|T]) :- trace[compile_time(flag("debug")), io(!IO)] (print_err(string([H|T]), !IO)).

:- pragma foreign_export("C", empty_ctx_modality_list(out), "empty_ctx_modality_list").
:- pragma foreign_export("C", cons_ctx_modality_list(in, in, out), "cons_ctx_modality_list").

empty_ctx_modality_list([]).
cons_ctx_modality_list(H, T, [H|T]).

%------------------------------------------------------------------------------%

:- pragma foreign_export("C", agent_human(out), "agent_human").
:- pragma foreign_export("C", agent_robot(out), "agent_robot").

agent_human(human).
agent_robot(robot).

:- pragma foreign_export("C", modality_understanding(out), "modality_understanding").
:- pragma foreign_export("C", modality_generation(out), "modality_generation").
:- pragma foreign_export("C", modality_event(out), "modality_event").
:- pragma foreign_export("C", modality_intention(out), "modality_intention").
:- pragma foreign_export("C", modality_info(out), "modality_info").
:- pragma foreign_export("C", modality_att(out), "modality_att").
:- pragma foreign_export("C", modality_k_private(in, out), "modality_k_private").
:- pragma foreign_export("C", modality_k_attrib(in, in, out), "modality_k_attrib").
:- pragma foreign_export("C", modality_k_mutual(out), "modality_k_mutual").

modality_understanding(understanding).
modality_generation(generation).
modality_event(evt).
modality_intention(intention).
modality_info(info).
modality_att(att).
modality_k_private(Ag, k(now, private(Ag))).
modality_k_attrib(Ag, Ag2, k(now, attrib(Ag, Ag2))).
modality_k_mutual(k(now, mutual(set.from_list([robot, human])))).

:- pragma foreign_export("C", impure_print_modality(in), "print_modality").
:- pragma foreign_export("C", impure_print_list_modalities(in), "print_list_modalities").

impure_print_modality(Mod) :-
	trace[compile_time(flag("debug")), io(!IO)] (print_err(string(Mod), !IO)).

impure_print_list_modalities(Mod) :-
	trace[compile_time(flag("debug")), io(!IO)] (print_err(string(Mod), !IO)).

:- pragma foreign_export("C", is_modality_understanding(in), "is_modality_understanding").
:- pragma foreign_export("C", is_modality_generation(in), "is_modality_generation").
:- pragma foreign_export("C", is_modality_event(in), "is_modality_event").
:- pragma foreign_export("C", is_modality_intention(in), "is_modality_intention").
:- pragma foreign_export("C", is_modality_info(in), "is_modality_info").
:- pragma foreign_export("C", is_modality_att(in), "is_modality_att").
:- pragma foreign_export("C", is_modality_k(in, out), "is_modality_k").

is_modality_understanding(understanding) :- trace[compile_time(flag("debug")), io(!IO)] (print_err("merc: understanding", !IO)).
is_modality_generation(generation) :- trace[compile_time(flag("debug")), io(!IO)] (print_err("merc: generation", !IO)).
is_modality_event(evt) :- trace[compile_time(flag("debug")), io(!IO)] (print_err("merc: event", !IO)).
is_modality_intention(intention) :- trace[compile_time(flag("debug")), io(!IO)] (print_err("merc: intention", !IO)).
is_modality_info(info) :- trace[compile_time(flag("debug")), io(!IO)] (print_err("merc: info", !IO)).
is_modality_att(att) :- trace[compile_time(flag("debug")), io(!IO)] (print_err("merc: att", !IO)).
is_modality_k(k(now, Belief), Belief) :- trace[compile_time(flag("debug")), io(!IO)] (print_err("merc: k, bel=" ++ string(Belief) ++ "", !IO)).

:- pragma foreign_export("C", belief_private(out), "belief_private").

belief_private(private(human)).

:- pragma foreign_export("C", is_belief_private(in, out), "is_belief_private").
:- pragma foreign_export("C", is_belief_attrib(in, out, out), "is_belief_attrib").
:- pragma foreign_export("C", is_belief_mutual(in, out), "is_belief_mutual").

is_belief_private(private(Ag), to_string(Ag)).
is_belief_attrib(attrib(AgA, AgB), to_string(AgA), to_string(AgB)).
is_belief_mutual(mutual(SetAgs), ListAgs) :-
	ListAgs = list.map((func(Ag) = to_string(Ag)), set.to_sorted_list(SetAgs)).

%------------------------------------------------------------------------------%

:- pragma foreign_export("C", const_cost_function(in, out), "const_cost_function").
:- pragma foreign_export("C", named_cost_function(in, out), "named_cost_function").

const_cost_function(Num, const(Num)).
named_cost_function(Name, f(Name)).

:- pragma foreign_export("C", is_const_cost_function(in, out), "is_const_cost_function").
:- pragma foreign_export("C", is_named_cost_function(in, out), "is_named_cost_function").

is_const_cost_function(const(Num), Num).
is_named_cost_function(f(Name), Name).

%------------------------------------------------------------------------------%

:- pragma foreign_export("C", proved_query(in, out), "proved_query").
:- pragma foreign_export("C", unsolved_query(in, in, out), "unsolved_query").
:- pragma foreign_export("C", assumed_query(in, in, out), "assumed_query").
:- pragma foreign_export("C", asserted_query(in, in, out), "asserted_query").

proved_query(MProp, proved(MProp)) :- trace[compile_time(flag("debug")), io(!IO)] (print_err("proved_query :" ++ string(proved(MProp)), !IO)).
unsolved_query(MProp, CostFunc, unsolved(MProp, CostFunc)) :- trace[compile_time(flag("debug")), io(!IO)] (print_err("unsolved_query :" ++ string(unsolved(MProp, CostFunc)), !IO)).
assumed_query(MProp, CostFunc, assumed(MProp, CostFunc)) :- trace[compile_time(flag("debug")), io(!IO)] (print_err("proved_query :" ++ string(assumed(MProp, CostFunc)), !IO)).
asserted_query(MProp, [], asserted(prop(MProp))) :- trace[compile_time(flag("debug")), io(!IO)] (print_err("proved_query :" ++ string(asserted(prop(MProp))), !IO)).
asserted_query(MProp, [H|T], asserted(impl([H|T], MProp))) :- trace[compile_time(flag("debug")), io(!IO)] (print_err("proved_query :" ++ string(asserted(impl([H|T], MProp))), !IO)).

:- pragma foreign_export("C", is_proved_query(in, out), "is_proved_query").
:- pragma foreign_export("C", is_unsolved_query(in, out, out), "is_unsolved_query").
:- pragma foreign_export("C", is_assumed_query(in, out, out), "is_assumed_query").
:- pragma foreign_export("C", is_asserted_query(in, out, out), "is_asserted_query").

is_proved_query(proved(MProp), MProp) :- trace[compile_time(flag("debug")), io(!IO)] (print_err("is_proved_query : " ++ string(proved(MProp)), !IO)).
is_unsolved_query(unsolved(MProp, CostFunction), MProp, CostFunction) :- trace[compile_time(flag("debug")), io(!IO)] (print_err("is_unsolved_query : " ++ string(unsolved(MProp, CostFunction)), !IO)).
is_assumed_query(assumed(MProp, CostFunction), MProp, CostFunction) :- trace[compile_time(flag("debug")), io(!IO)] (print_err("is_assumed_query : " ++ string(assumed(MProp, CostFunction)), !IO)).
is_asserted_query(asserted(prop(MProp)), MProp, []) :- trace[compile_time(flag("debug")), io(!IO)] (print_err("is_asserted_query : " ++ string(asserted(prop(MProp))), !IO)).
is_asserted_query(asserted(impl(AnteProps, MProp)), MProp, AnteProps) :- trace[compile_time(flag("debug")), io(!IO)] (print_err("is_asserted_query : " ++ string(asserted(impl(AnteProps, MProp))), !IO)).

%------------------------------------------------------------------------------%

:- pragma foreign_export("C", dissect_term(in, in, out, out, out), "dissect_term").

dissect_term(VS, v(Var), yes, VarName, []) :-
	varset.lookup_name(VS, Var, "V_", VarName).

dissect_term(_VS, t(Functor, Args), no, Functor, Args).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

:- pragma foreign_export("C", dissect_predicate(in, in, out, out), "dissect_predicate").

dissect_predicate(_VS, p(PredSym, Args), PredSym, Args).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

:- pragma foreign_export("C", dissect_mprop(in, out, out), "dissect_mprop").

dissect_mprop(m(Mod, Pred), Mod, Pred).
%	trace [io(!IO)] (print_err("dissecting mprop: " ++ string(m(Mod, Pred)), !IO)),

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

:- pragma foreign_export("C", dissect_proof(in, out, out), "dissect_proof").

dissect_proof(Proof, VS, Qs) :-
	trace[compile_time(flag("debug")), io(!IO)] (print_err("in dissect_proof\n", !IO)),
	%trace[compile_time(flag("debug")), io(!IO)] (print_err(string(last_goal(Proof)) ++ "\n", !IO)),
	vs(Qs, VS) = last_goal(Proof),
	trace[compile_time(flag("debug")), io(!IO)] (print_err("end of dissect_proof\n", !IO)).

%------------------------------------------------------------------------------%

:- pragma foreign_export("C", new_proof(in, in, out), "new_proof").

new_proof(MQs, VS, proof(vs([MQs], VS), [])).
