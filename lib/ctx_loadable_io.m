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

:- module ctx_loadable_io.

:- interface.

:- import_module io, list, varset.
:- import_module abduction, ctx_modality, lang, blacklist.
:- import_module ctx_loadable.

:- pred tty_print_facts(ctx::in, string::in, io::di, io::uo) is det.
:- pred tty_print_facts(io.output_stream::in, ctx::in, string::in, io::di, io::uo) is det.

:- pred tty_print_assumables(ctx::in, string::in, io::di, io::uo) is det.
:- pred tty_print_assumables(io.output_stream::in, ctx::in, string::in, io::di, io::uo) is det.

:- pred tty_print_disjoint_decls(io.output_stream::in, ctx::in, string::in, io::di, io::uo) is det.

:- pred tty_print_rules(ctx::in, string::in, io::di, io::uo) is det.
:- pred tty_print_rules(io.output_stream::in, ctx::in, string::in, io::di, io::uo) is det.

:- pred tty_print_ctx(ctx::in, io::di, io::uo) is det.
:- pred tty_print_ctx(io.output_stream::in, ctx::in, io::di, io::uo) is det.

%:- pred print_proof_trace(ctx::in, proof(ctx_modality)::in, io::di, io::uo) is det.
%:- pred print_proof_trace(io.output_stream::in, ctx::in, proof(ctx_modality)::in, io::di, io::uo) is det.

:- func goal_to_string(vscope(list(query(ctx_modality)))) = string.
:- func query_to_string(varset, query(ctx_modality)) = string.
:- func proof_state_to_string(varset, list(query(ctx_modality))) = string.
:- func blacklist_to_string(blacklist(ctx_modality)) = string.

%------------------------------------------------------------------------------%

:- implementation.

:- import_module map, set, list, pair, string.
:- import_module abduction, assumability, blacklist.

:- import_module context.
:- import_module ctx_modality, ctx_loadable, ctx_io.
:- import_module modality, stringable.

:- import_module term, varset, lang_io.

:- import_module tty.

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

tty_print_facts(Stream, Ctx, Indent, !IO) :-
	set.fold((pred(Fact::in, !.IO::di, !:IO::uo) is det :-
		print(Stream, Indent, !IO),
		print(Stream, tty_vsmatom_to_string(Fact), !IO),
		nl(Stream, !IO)
			), facts(Ctx), !IO).

tty_print_facts(Ctx, Indent, !IO) :-
	tty_print_facts(stdout_stream, Ctx, Indent, !IO).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

tty_print_rules(Stream, Ctx, Indent, !IO) :-
	set.fold((pred(Rule::in, !.IO::di, !:IO::uo) is det :-
		print(Stream, Indent, !IO),
		print(Stream, tty_vsmrule_to_string(Rule), !IO),
		nl(Stream, !IO)
			), rules(Ctx), !IO).

tty_print_rules(Ctx, Indent, !IO) :-
	tty_print_rules(stdout_stream, Ctx, Indent, !IO).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

tty_print_assumables(Stream, Ctx, Indent, !IO) :-
	map.foldl((pred(FuncName::in, Costs::in, !.IO::di, !:IO::uo) is det :-
		print(Stream, Indent, !IO),
		print(Stream, FuncName ++ " = ", !IO),

		CostStrs = list.map((func(m(Mod, GAtom)-Cost) = S :-
			S = tty_vsmatom_to_string(vs(m(Mod, ground_formula_to_formula(GAtom)), varset.init))
					++ " = " ++ float_to_string(Cost)
				), map.to_assoc_list(Costs)),

		print(Stream, "[\n    " ++ string.join_list(",\n" ++ Indent ++ Indent, CostStrs) ++ "\n" ++ Indent ++ "].\n", !IO)
			), assumables(Ctx), !IO).

tty_print_assumables(Ctx, Indent, !IO) :-
	tty_print_assumables(stdout_stream, Ctx, Indent, !IO).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

tty_print_disjoint_decls(Stream, Ctx, Indent, !IO) :-
	set.fold((pred(DD::in, !.IO::di, !:IO::uo) is det :-
		print(Stream, Indent, !IO),
		print(Stream, tty_disjoint_decl_to_string(DD), !IO),
		nl(Stream, !IO)
			), disjoint_decls(Ctx), !IO).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

tty_print_ctx(Stream, Ctx, !IO) :-
	print(Stream, "facts:\n", !IO),
	tty_print_facts(Stream, Ctx, "  ", !IO),

	nl(Stream, !IO),

	print(Stream, "assumables:\n", !IO),
	tty_print_assumables(Stream, Ctx, "  ", !IO),

	nl(Stream, !IO),

	print(Stream, "rules:\n", !IO),
	tty_print_rules(Stream, Ctx, "  ", !IO),

	nl(Stream, !IO),

	print(Stream, "disjoint declarations:\n", !IO),
	tty_print_disjoint_decls(Stream, Ctx, "  ", !IO),

	nl(Stream, !IO).

tty_print_ctx(Ctx, !IO) :-
	tty_print_ctx(stdout_stream, Ctx, !IO).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

/*
print_proof_trace(Ctx, Proof, !IO) :-
	print_proof_trace(stdout_stream, Ctx, Proof, !IO).

print_proof_trace(_Stream, _Ctx, _Proof, !IO) :-
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

:- func tty_atom_to_string(varset, atom) = string.
:- func tty_lang_term_to_string(varset, lang.term) = string.

tty_atom_to_string(_Varset, p(PredSym, [])) = totty(bold) ++ protect(PredSym) ++ totty(reset).
tty_atom_to_string(Varset, p(PredSym, [H|T])) = totty(bold) ++ protect(PredSym) ++ totty(reset) ++ "(" ++ ArgStr ++ ")" :-
	ArgStr = string.join_list(", ", list.map(tty_lang_term_to_string(Varset), [H|T])).

tty_lang_term_to_string(Varset, Arg) = S :-
	(
		Arg = t(Functor, []),
		S = protect(Functor)
	;
		Arg = t(Functor, [H|T]),
		S = protect(Functor) ++ "(" ++ string.join_list(", ", list.map(tty_lang_term_to_string(Varset), [H|T])) ++ ")"
	;
		Arg = v(Var),
		S = totty(cyan) ++ varset.lookup_name(Varset, Var) ++ totty(reset)
	).

:- func tty_vsmatom_to_string(vscope(matom(M))) = string <= (modality(M), stringable(M)).

tty_vsmatom_to_string(vs(m(K, P), Varset)) = Str :-
	Str = tty_modality_to_string(K) ++ tty_atom_to_string(Varset, P).

:- func tty_matom_to_string(varset, matom(M)) = string <= (modality(M), stringable(M)).

tty_matom_to_string(Varset, MP) = tty_vsmatom_to_string(vs(MP, Varset)).

:- func tty_mtest_to_string(varset, mtest(M)) = string <= (modality(M), stringable(M)).

tty_mtest_to_string(Varset, prop(MAtom)) = tty_matom_to_string(Varset, MAtom).
tty_mtest_to_string(Varset, impl(MPs, HMP)) = string.join_list(", ", list.map(tty_matom_to_string(Varset), MPs))
		++ " -> " ++ tty_matom_to_string(Varset, HMP).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

:- func tty_modality_to_string(list(M)) = string <= (modality(M), stringable(M)).

tty_modality_to_string([]) = "".
tty_modality_to_string([H|T]) = totty(green) ++ string.join_list(" : ", list.map(to_string, [H|T])) ++ " : " ++ totty(reset).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

query_to_string(VS, unsolved(MAtom, F)) = tty_matom_to_string(VS, MAtom)
		++ "[unsolved / " ++ assumability_function_to_string(F) ++ "]".
query_to_string(VS, proved(MAtom)) = tty_matom_to_string(VS, MAtom) ++ totty(yellow) ++ "[proved]" ++ totty(reset).
query_to_string(VS, assumed(MAtom, F)) = tty_matom_to_string(VS, MAtom)
		++ totty(red) ++ "[assumed / " ++ assumability_function_to_string(F) ++ "]" ++ totty(reset).
query_to_string(VS, asserted(MTest)) = tty_mtest_to_string(VS, MTest) ++ totty(magenta) ++ "[asserted]" ++ totty(reset).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

goal_to_string(vs(G, VS)) = string.join_list(",\n  ", list.reverse(list.map(query_to_string(VS), G))).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

proof_state_to_string(Varset, L) = S :-
	S = string.join_list(",\n  ", list.map(query_to_string(Varset), L)).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

blacklist_to_string(BL) = S :-
	S = "map:\n" ++ map.foldl((func(K, V, S0) = S0 ++ tty_mgatom_to_string(K) ++ " --> " ++ set_of_mgatom_to_string(V) ++ "\n"), BL^bl_map, "")
			++ "used: " ++ set_of_mgatom_to_string(BL^used) ++ "\n"
			++ "forbidden: " ++ set_of_mgatom_to_string(BL^forbidden) ++ "\n".

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

:- func tty_mgatom_to_string(mgatom(ctx_modality)) = string.

tty_mgatom_to_string(MGP) = tty_matom_to_string(varset.init, ground_matom_to_matom(MGP)).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

:- func set_of_mgatom_to_string(set(mgatom(ctx_modality))) = string.

set_of_mgatom_to_string(Set) = "[" ++ string.join_list(", ", list.map(tty_mgatom_to_string, set.to_sorted_list(Set))) ++ "]".

%------------------------------------------------------------------------------%

:- func tty_vsmrule_to_string(vscope(mrule(ctx_modality))) = string.

tty_vsmrule_to_string(vs(m(K, As-H), Varset)) = Str :-
	ModStr = tty_modality_to_string(K),
	RuleStr = tty_rule_head_to_string(Varset, H) ++ " <- "
			++ string.join_list(", ", list.map(tty_rule_antecedent_to_string(Varset), As)),
	(if ModStr = ""
	then Rest = RuleStr
	else Rest = "(" ++ RuleStr ++ ")"
	),
	Str = ModStr ++ Rest.

:- func tty_annot_vsmatom_to_string(vscope(with_assumability_function(matom(ctx_modality)))) = string.

tty_annot_vsmatom_to_string(vs(cf(MP, F), Varset)) = tty_vsmatom_to_string(vs(MP, Varset))
		++ "/" ++ assumability_function_to_string(F).

:- func tty_test_vsmatom_to_string(vscope(matom(ctx_modality))) = string.

tty_test_vsmatom_to_string(vs(MP, Varset)) = "?" ++ vsmatom_to_string(vs(MP, Varset)).

:- func tty_rule_antecedent_to_string(varset, rule_antecedent(ctx_modality)) = string.

tty_rule_antecedent_to_string(Varset, std(AnnotMAtom)) = tty_annot_vsmatom_to_string(vs(AnnotMAtom, Varset)).
tty_rule_antecedent_to_string(Varset, test(MTest)) = "<" ++ tty_mtest_to_string(Varset, MTest) ++ ">?".

:- func tty_rule_head_to_string(varset, rule_head(M)) = string <= (modality(M), stringable(M)).

tty_rule_head_to_string(Varset, std(MAtom)) = tty_matom_to_string(Varset, MAtom).
tty_rule_head_to_string(Varset, test(MTest)) = "<" ++ tty_mtest_to_string(Varset, MTest) ++ ">?".

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

:- func tty_disjoint_decl_to_string(disjoint_decl(ctx_modality)) = string.

tty_disjoint_decl_to_string(DD) = "disjoint([" ++ S ++ "])" :-
	S = string.join_list(", ", list.map((func(MGF) = S0 :- ground_matom(MF, MGF), S0 = tty_matom_to_string(varset.init, MF)), set.to_sorted_list(DD))).

