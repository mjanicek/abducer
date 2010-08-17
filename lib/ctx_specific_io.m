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

:- module ctx_specific_io.

:- interface.
:- import_module ctx_specific, io, abduction, modality, stringable, list, varset, formula, bag, ctx_modality.

:- pred print_ctx(ctx::in, io::di, io::uo) is det.
:- pred print_proof_trace(ctx::in, proof(ctx_modality)::in, io::di, io::uo) is det.
:- func step_to_string(step(M)) = string <= (modality(M), stringable(M)).
:- func query_to_string(varset, query(ctx_modality)) = string.
:- func goal_to_string(vscope(list(query(ctx_modality)))) = string.
:- func assumptions_to_string(ctx, bag(with_cost_function(mgprop(ctx_modality)))) = string.
:- func assertions_to_string(ctx, bag(vscope(mtest(ctx_modality)))) = string.
:- func proof_state_to_string(varset, list(query(ctx_modality))) = string.

%------------------------------------------------------------------------------%

:- implementation.
:- import_module map, formula_io, ctx_io, string, belief_model, set, lf, lf_io, solutions, pair, assoc_list, costs, context.

print_ctx(Ctx, !IO) :-
	print("beliefs:\n", !IO),

	map.foldl((pred(Index::in, {Stf, Bel, LF}::in, !.IO::di, !:IO::uo) is det :-
		print("  ", !IO),
		(if member(Index, Ctx^bm^fg) then Fg = "*" else Fg = " "),
		print("(" ++ from_int(Index) ++ ")  " ++ Fg ++ " " ++ to_string(k(Stf, Bel)) ++ ": "
				++ lf_to_string(LF), !IO),
		nl(!IO)
			), Ctx^bm^k, !IO),

	nl(!IO),

	print("generated lfs:\n", !IO),

	KFacts = solutions_set((pred({STF, Bel, LF}::out) is nondet :-
		k_fact(Ctx^ont, Ctx^rrel, Ctx^bm, STF, Bel, LF)
			)),

	set.fold((pred({STF, Bel, LF}::in, !.IO::di, !:IO::uo) is det :-
		print("  " ++ to_string(k(STF, Bel)) ++ ": " ++ lf_to_string(LF) ++ "\n", !IO)
			), KFacts, !IO),

	nl(!IO),

	print("explicit facts:\n", !IO),
	set.fold((pred(Fact::in, !.IO::di, !:IO::uo) is det :-
			% XXX global context
		print("  ", !IO),
		Fact = m(Mod, GProp),
		print(mprop_to_string(varset.init, m(Mod, ground_formula_to_formula(GProp))), !IO),
		nl(!IO)
			), explicit_facts(Ctx), !IO),

	nl(!IO),

	print("explicit rules:\n", !IO),
	set.fold((pred(Rule::in, !.IO::di, !:IO::uo) is det :-
			% XXX global context
		print("  ", !IO),
		print(vsmrule_to_string(Rule), !IO),
		nl(!IO)
			), explicit_rules(Ctx), !IO).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

print_proof_trace(_Ctx, Proof, !IO) :-
	true.
/*
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

