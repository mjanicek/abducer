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

:- module ctx_loadable_io.

:- interface.

:- import_module io, list, varset, bag.
:- import_module abduction, ctx_modality, formula, stringable, modality, blacklist.
:- import_module ctx_loadable.

:- pred print_facts(ctx::in, string::in, io::di, io::uo) is det.
:- pred print_facts(io.output_stream::in, ctx::in, string::in, io::di, io::uo) is det.

:- pred print_assumables(ctx::in, string::in, io::di, io::uo) is det.
:- pred print_assumables(io.output_stream::in, ctx::in, string::in, io::di, io::uo) is det.

:- pred print_disjoints(io.output_stream::in, ctx::in, string::in, io::di, io::uo) is det.

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
:- func blacklist_to_string(blacklist(ctx_modality)) = string.

%------------------------------------------------------------------------------%

:- implementation.

:- import_module require, solutions.
:- import_module map, set, list, pair, assoc_list, string, float, int, bag, bool.
:- import_module utils.
:- import_module abduction, formula, context, costs, blacklist.

:- import_module ctx_modality, ctx_loadable, ctx_io.
:- import_module modality, stringable.

:- import_module parser, term_io, term, varset, formula_io, formula_ops, costs.

:- import_module tty.

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

print_facts(Stream, Ctx, Indent, !IO) :-
	set.fold((pred(Fact::in, !.IO::di, !:IO::uo) is det :-
		print(Stream, Indent, !IO),
		print(Stream, tty_vsmprop_to_string(Fact), !IO),
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

print_disjoints(Stream, Ctx, Indent, !IO) :-
	set.fold((pred(DD::in, !.IO::di, !:IO::uo) is det :-
		print(Stream, Indent, !IO),
		print(Stream, disjoint_to_string(DD), !IO),
		nl(Stream, !IO)
			), disjoints(Ctx), !IO).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

print_ctx(Stream, Ctx, !IO) :-
	print(Stream, "facts:\n", !IO),
	print_facts(Stream, Ctx, "  ", !IO),

	nl(Stream, !IO),

	print(Stream, "assumables:\n", !IO),
	print_assumables(Stream, Ctx, "  ", !IO),

	nl(Stream, !IO),

	print(Stream, "rules:\n", !IO),
	print_rules(Stream, Ctx, "  ", !IO),

	nl(Stream, !IO),

	print(Stream, "disjoint declarations:\n", !IO),
	print_disjoints(Stream, Ctx, "  ", !IO),

	nl(Stream, !IO),

	print(Stream, "disjoint blacklist:\n", !IO),
	print(Stream, blacklist_to_string(blacklist.init(Ctx)), !IO).

print_ctx(Ctx, !IO) :-
	print_ctx(stdout_stream, Ctx, !IO).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

print_proof_trace(Ctx, Proof, !IO) :-
	print_proof_trace(stdout_stream, Ctx, Proof, !IO).

print_proof_trace(_Stream, _Ctx, _Proof, !IO) :-
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

:- func protect(string) = string.

protect(S0) = S :-
	(if string.capitalize_first(S0, S0)
	then S = "'" ++ S0 ++ "'"
	else S = S0
	).

:- func tty_atomic_formula_to_string(varset, atomic_formula) = string.
:- func tty_formula_term_to_string(varset, formula.term) = string.

tty_atomic_formula_to_string(_Varset, p(PredSym, [])) = totty(bold) ++ protect(PredSym) ++ totty(reset).
tty_atomic_formula_to_string(Varset, p(PredSym, [H|T])) = totty(bold) ++ protect(PredSym) ++ totty(reset) ++ "(" ++ ArgStr ++ ")" :-
	ArgStr = string.join_list(", ", list.map(tty_formula_term_to_string(Varset), [H|T])).

tty_formula_term_to_string(Varset, Arg) = S :-
	(
		Arg = t(Functor, []),
		S = protect(Functor)
	;
		Arg = t(Functor, [H|T]),
		S = protect(Functor) ++ "(" ++ string.join_list(", ", list.map(tty_formula_term_to_string(Varset), [H|T])) ++ ")"
	;
		Arg = v(Var),
		S = totty(cyan) ++ varset.lookup_name(Varset, Var) ++ totty(reset)
	).

:- func tty_vsmprop_to_string(vscope(mprop(M))) = string <= (modality(M), stringable(M)).

tty_vsmprop_to_string(vs(m(K, P), Varset)) = Str :-
	Str = tty_modality_to_string(K) ++ tty_atomic_formula_to_string(Varset, P).

:- func tty_mprop_to_string(varset, mprop(M)) = string <= (modality(M), stringable(M)).

tty_mprop_to_string(Varset, MP) = tty_vsmprop_to_string(vs(MP, Varset)).

:- func tty_mtest_to_string(varset, mtest(M)) = string <= (modality(M), stringable(M)).

tty_mtest_to_string(Varset, prop(MProp)) = mprop_to_string(Varset, MProp).
tty_mtest_to_string(Varset, impl(MPs, HMP)) = string.join_list(", ", list.map(mprop_to_string(Varset), MPs))
		++ " -> " ++ mprop_to_string(Varset, HMP).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

:- func tty_modality_to_string(list(M)) = string <= (modality(M), stringable(M)).

tty_modality_to_string([]) = "".
tty_modality_to_string([H|T]) = totty(green) ++ string.join_list(" : ", list.map(to_string, [H|T])) ++ " : " ++ totty(reset).

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

query_to_string(VS, unsolved(MProp, F)) = tty_mprop_to_string(VS, MProp)
		++ "[unsolved / " ++ cost_function_to_string(F) ++ "]".
query_to_string(VS, proved(MProp)) = tty_mprop_to_string(VS, MProp) ++ totty(yellow) ++ "[proved]" ++ totty(reset).
query_to_string(VS, assumed(MProp, F)) = tty_mprop_to_string(VS, MProp)
		++ totty(red) ++ "[assumed / " ++ cost_function_to_string(F) ++ "]" ++ totty(reset).
query_to_string(VS, asserted(MTest)) = tty_mtest_to_string(VS, MTest) ++ totty(magenta) ++ "[asserted]" ++ totty(reset).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

goal_to_string(vs(G, VS)) = string.join_list(",\n  ", list.reverse(list.map(query_to_string(VS), G))).

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

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

blacklist_to_string(BL) = S :-
	S = "map:\n" ++ map.foldl((func(K, V, S0) = S0 ++ tty_mgprop_to_string(K) ++ " --> " ++ set_of_mgprop_to_string(V) ++ "\n"), BL^bl_map, "")
			++ "used: " ++ set_of_mgprop_to_string(BL^used) ++ "\n"
			++ "forbidden: " ++ set_of_mgprop_to_string(BL^forbidden) ++ "\n".

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

:- func tty_mgprop_to_string(mgprop(ctx_modality)) = string.

tty_mgprop_to_string(MGP) = tty_mprop_to_string(varset.init, ground_mprop_to_mprop(MGP)).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

:- func set_of_mgprop_to_string(set(mgprop(ctx_modality))) = string.

set_of_mgprop_to_string(Set) = "[" ++ string.join_list(", ", list.map(tty_mgprop_to_string, set.to_sorted_list(Set))) ++ "]".
