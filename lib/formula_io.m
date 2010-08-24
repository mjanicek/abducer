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

:- module formula_io.

:- interface.

:- import_module formula, modality.
:- import_module stringable.

:- import_module term, varset.

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

	% mprop

	% parsing
:- func string_to_vsmprop(string) = vscope(mprop(M)) is semidet <= (modality(M), term_parsable(M)).
:- func det_string_to_vsmprop(string) = vscope(mprop(M)) <= (modality(M), term_parsable(M)).

	% generation
:- func mprop_to_string(varset, mprop(M)) = string <= (modality(M), stringable(M)).
:- func vsmprop_to_string(vscope(mprop(M))) = string <= (modality(M), stringable(M)).

:- func atomic_formula_to_string(varset, atomic_formula) = string.
:- func formula_term_to_string(varset, formula.term) = string.

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

:- func mtest_to_string(varset, mtest(M)) = string <= (modality(M), stringable(M)).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

	% mrule

:- func string_to_vsmrule(string) = vscope(mrule(M)) is semidet <= (modality(M), term_parsable(M)).
:- func det_string_to_vsmrule(string) = vscope(mrule(M)) <= (modality(M), term_parsable(M)).

:- func vsmrule_to_string(vscope(mrule(M))) = string <= (modality(M), stringable(M)).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

:- func string_to_disjoint(string) = disjoint(M) is semidet <= (modality(M), term_parsable(M)).
:- func disjoint_to_string(disjoint(M)) = string <= (modality(M), stringable(M)).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

:- pred term_to_mprop(term.term::in, mprop(M)::out) is semidet <= (modality(M), term_parsable(M)).
:- pred term_to_mrule(term.term::in, mrule(M)::out) is semidet <= (modality(M), term_parsable(M)).
:- pred term_to_disjoint(term.term::in, disjoint(M)::out) is semidet <= (modality(M), term_parsable(M)).
:- pred term_to_assumable_function_def(term.term::in, assumable_function_def(M)::out) is semidet
		<= (modality(M), term_parsable(M)).

%------------------------------------------------------------------------------%

:- func subst_to_string(varset, subst) = string.

%------------------------------------------------------------------------------%

:- implementation.

:- import_module require.
:- import_module list, pair, map, set.
:- import_module string.
:- import_module assumability.
:- import_module prob.
:- import_module formula_ops.
:- import_module parser, term_io.

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

string_to_vsmprop(Str) = vs(P, Varset) :-
	read_term_from_string_with_op_table(init_wabd_op_table, "", Str, _, term(Varset, T)),
	generic_term(T),
	term_to_mprop(T, P).

det_string_to_vsmprop(S) = P :-
	(if P0 = string_to_vsmprop(S)
	then P = P0
	else error("Can't convert string \"" ++ S ++ "\" to a proposition.")
	).

mprop_to_string(Varset, MP) = vsmprop_to_string(vs(MP, Varset)).

vsmprop_to_string(vs(m(K, P), Varset)) = Str :-
	Str = modality_to_string(K) ++ atomic_formula_to_string(Varset, P).

:- func annot_vsmprop_to_string(vscope(with_assumability_function(mprop(M)))) = string
		<= (modality(M), stringable(M)).

annot_vsmprop_to_string(vs(cf(MP, F), Varset)) = vsmprop_to_string(vs(MP, Varset))
		++ "/" ++ assumability_function_to_string(F).

:- func test_vsmprop_to_string(vscope(mprop(M))) = string <= (modality(M), stringable(M)).

test_vsmprop_to_string(vs(MP, Varset)) = "?" ++ vsmprop_to_string(vs(MP, Varset)).

mtest_to_string(Varset, prop(MProp)) = mprop_to_string(Varset, MProp).
mtest_to_string(Varset, impl(MPs, HMP)) = string.join_list(", ", list.map(mprop_to_string(Varset), MPs))
		++ " -> " ++ mprop_to_string(Varset, HMP).

:- func rule_antecedent_to_string(varset, rule_antecedent(M)) = string <= (modality(M), stringable(M)).

rule_antecedent_to_string(Varset, std(AnnotMProp)) = annot_vsmprop_to_string(vs(AnnotMProp, Varset)).
rule_antecedent_to_string(Varset, test(MTest)) = "<" ++ mtest_to_string(Varset, MTest) ++ ">?".

:- func rule_head_to_string(varset, rule_head(M)) = string <= (modality(M), stringable(M)).

rule_head_to_string(Varset, std(MProp)) = mprop_to_string(Varset, MProp).
rule_head_to_string(Varset, test(MTest)) = "<" ++ mtest_to_string(Varset, MTest) ++ ">?".

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

string_to_vsmrule(Str) = vs(R, Varset) :-
	read_term_from_string_with_op_table(init_wabd_op_table, "", Str, _, term(Varset, T)),
	generic_term(T),
	term_to_mrule(T, R).

det_string_to_vsmrule(S) = R :-
	(if R0 = string_to_vsmrule(S)
	then R = R0
	else error("Can't convert string \"" ++ S ++ "\" to a rule.")
	).

vsmrule_to_string(vs(m(K, As-H), Varset)) = Str :-
	ModStr = modality_to_string(K),
	RuleStr = rule_head_to_string(Varset, H) ++ " <- "
			++ string.join_list(", ", list.map(rule_antecedent_to_string(Varset), As)),
	(if ModStr = ""
	then Rest = RuleStr
	else Rest = "(" ++ RuleStr ++ ")"
	),
	Str = ModStr ++ Rest.

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

term_to_mprop(T, m(Mod, P)) :-
	(if T = functor(atom(":"), [TM, TP], _)
	then 
		term_to_list_of_mods(TM, Mod),
		term_to_atomic_formula(TP, P)
	else
		Mod = [],
		term_to_atomic_formula(T, P)
	).

term_to_mrule(T, m(Mod, R)) :-
	(if T = functor(atom(":"), [TM, TR], _)
	then 
		term_to_list_of_mods(TM, Mod),
		term_to_nonmod_rule(TR, R)
	else
		Mod = [],
		term_to_nonmod_rule(T, R)
	).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

atomic_formula_to_string(_Varset, p(PredSym, [])) = "'" ++ PredSym ++ "'".
atomic_formula_to_string(Varset, p(PredSym, [H|T])) = "'" ++ PredSym ++ "'(" ++ ArgStr ++ ")" :-
	ArgStr = string.join_list(", ", list.map(formula_term_to_string(Varset), [H|T])).

formula_term_to_string(Varset, Arg) = S :-
	(
		Arg = t(Functor, []),
		S = "'" ++ Functor ++ "'"
	;
		Arg = t(Functor, [H|T]),
		S = "'" ++ Functor ++ "'(" ++ string.join_list(", ", list.map(formula_term_to_string(Varset), [H|T])) ++ ")"
	;
		Arg = v(Var),
		S = varset.lookup_name(Varset, Var)
	).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

:- pred term_to_list_of_mods(term.term::in, list(M)::out) is semidet <= (modality(M), term_parsable(M)).

term_to_list_of_mods(T, L) :-
	(if T = functor(atom(":"), [TML, TMR], _)
	then
		term_to_list_of_mods(TML, Ms),
		M = from_term(TMR),
		L = Ms ++ [M]
	else
		M = from_term(T),
		L = [M]
	).

:- func modality_to_string(list(M)) = string <= (modality(M), stringable(M)).

modality_to_string([]) = "".
modality_to_string([H|T]) = "{" ++ string.join_list(", ", list.map(to_string, [H|T])) ++ "}".

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

:- pred term_to_nonmod_rule(term.term::in, pair(list(rule_antecedent(M)), rule_head(M))::out) is semidet
		<= (modality(M), term_parsable(M)).

term_to_nonmod_rule(functor(atom("<-"), [THead, TAnte], _), Ante-Head) :-
	(if term_to_mtest(THead, MTest)
	then
		Head = test(MTest)
	else
		term_to_mprop(THead, MProp),
		Head = std(MProp)
	),
	term_to_list_of_rule_antecedents(TAnte, Ante).

:- pred term_to_list_of_rule_antecedents(term.term::in, list(rule_antecedent(M))::out) is semidet
		<= (modality(M), term_parsable(M)).

term_to_list_of_rule_antecedents(T, List) :-
	(if
		T = functor(atom(","), [TMP, TMPs], _)
	then
		term_to_rule_antecedent(TMP, This),
		term_to_list_of_rule_antecedents(TMPs, MPs),
		List = MPs ++ [This]
	else
		term_to_rule_antecedent(T, This),
		List = [This]
	).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

:- pred term_to_list_of_mprops(term.term::in, list(mprop(M))::out) is semidet <= (modality(M), term_parsable(M)).

term_to_list_of_mprops(T, List) :-
	(if
		T = functor(atom(","), [TMP, TMPs], _)
	then
		term_to_mprop(TMP, This),
		term_to_list_of_mprops(TMPs, MPs),
		List = [This|MPs]
	else
		term_to_mprop(T, This),
		List = [This]
	).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

:- pred term_to_mtest(term.term::in, mtest(M)::out) is semidet <= (modality(M), term_parsable(M)).

term_to_mtest(functor(atom("?"), [T], _), MTest) :-
	(if T = functor(atom("->"), [TMPs, THMP], _)
	then
		term_to_list_of_mprops(TMPs, MPs),
		term_to_mprop(THMP, HMP),
		MTest = impl(MPs, HMP)
	else
		term_to_mprop(T, MProp),
		MTest = prop(MProp)
	).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

:- pred term_to_rule_antecedent(term.term::in, rule_antecedent(M)::out) is semidet
		<= (modality(M), term_parsable(M)).

term_to_rule_antecedent(MainT, Antecedent) :-
	(if
		MainT = functor(atom("/"), [T, AnnotT], _)
	then
		% it's an annotated mprop
		term_to_mprop(T, MP),
		term_to_assumability_function(AnnotT, Func),
		Antecedent = std(cf(MP, Func))
	else
		(if
			MainT = functor(atom("?"), [_], _)
		then
			% it's an assertion
			term_to_mtest(MainT, MTest),
			Antecedent = test(MTest)
		else
			% it's a non-annotated mprop
			term_to_mprop(MainT, MP),
			Antecedent = std(cf(MP, not_assumable))
		)
	).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

:- pred term_to_assumability_function(term.term::in, assumability_function::out) is semidet.

term_to_assumability_function(functor(atom(FName), [], _), f(FName)).
term_to_assumability_function(functor(float(FValue), [], _), const(FValue)).
term_to_assumability_function(functor(atom("p"), [functor(float(Prob), [], _)], _), const(prob.to_cost(Prob))).

%------------------------------------------------------------------------------%

term_to_disjoint(functor(atom("disjoint"), [Arg], _), DD) :-
	parse_list(Arg, Ts),
	list.map((pred(T::in, MGF::out) is semidet :- term_to_mprop(T, MF), ground_mprop(MF, MGF)), Ts, MGFs),
	DD = set.from_list(MGFs).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

:- pred parse_list(term.term::in, list(term.term)::out) is semidet.

parse_list(functor(atom("[]"), [], _), []).
parse_list(functor(atom("[|]"), [TermH, TermT], _), [TermH|T]) :-
	parse_list(TermT, T).

%------------------------------------------------------------------------------%

string_to_disjoint(Str) = DD :-
	read_term_from_string_with_op_table(init_wabd_op_table, "", Str, _, term(_Varset, T)),
	generic_term(T),
	term_to_disjoint(T, DD).

disjoint_to_string(DD) = "disjoint([" ++ S ++ "])" :-
	S = string.join_list(", ", list.map((func(MGF) = S0 :- ground_mprop(MF, MGF), S0 = mprop_to_string(varset.init, MF)), set.to_sorted_list(DD))).

%------------------------------------------------------------------------------%

subst_to_string(Varset, Subst) = "{" ++ Str ++ "}" :-
	L = map.to_assoc_list(Subst),
	L0 = list.map((func(Var-Value) = S :-
		S = varset.lookup_name(Varset, Var) ++ "=" ++ formula_term_to_string(Varset, Value)), L),
	Str = string.join_list(", ", L0).

%------------------------------------------------------------------------------%

term_to_assumable_function_def(functor(atom("="), [FuncNameTerm, DefTerms], _), FuncDef) :-
	FuncNameTerm = functor(atom(FuncName), [], _),
	parse_list(DefTerms, ListCostTerms),
	list.map((pred(AssignTerm::in, MGProp-Cost::out) is semidet :-
		AssignTerm = functor(atom("="), [MPropTerm, CostTerm], _),
		term_to_mprop(MPropTerm, m(Mod, Prop)),
		ground_formula(Prop, GProp),
		MGProp = m(Mod, GProp),
		term_to_cost(CostTerm, Cost)
			), ListCostTerms, Costs),
	FuncDef = FuncName-map.from_assoc_list(Costs).

%------------------------------------------------------------------------------%

:- pred term_to_cost(term.term::in, float::out) is semidet.

term_to_cost(functor(float(Cost), [], _), Cost).
term_to_cost(functor(atom("p"), [functor(float(Prob), [], _)], _), prob.to_cost(Prob)).
