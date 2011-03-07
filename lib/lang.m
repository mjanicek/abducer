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

:- module lang.

:- interface.

:- import_module list, map, set, pair.
:- import_module term, varset.
:- import_module assumability.
:- import_module modality.

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

:- type term
	--->	v(var)
	;	t(string, list(lang.term))
	.

:- type ground_term
	---> 	t(string, list(ground_term))
	.

	% value under the open world assumption
:- type value(T)
	--->	true(T)
	;	false(T)
	.

	% prolog term <--> lang.term
:- func lang_term_to_term(lang.term) = term.term.

:- pred term_to_lang_term(term.term::in, lang.term::out) is semidet.
:- func term_to_lang_term(term.term) = lang.term is semidet.
:- func det_term_to_lang_term(term.term) = lang.term.

	% term <--> ground term
:- func ground_term_to_term(ground_term) = lang.term.
:- func term_to_ground_term(lang.term) = ground_term is semidet.

	% predicate version of the above
:- pred ground_term(lang.term, ground_term).
:- mode ground_term(in, out) is semidet.
:- mode ground_term(out, in) is det.

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

:- type atom
	--->	p(
		string,  % predicate symbol
		list(lang.term)
	).

:- type ground_atom
	--->	p(
		string,  % predicate symbol
		list(ground_term)
	).

	% prolog term <--> lang.atom
:- func atom_to_term(atom) = term.term.

:- pred term_to_atom(term.term::in, atom::out) is semidet.
:- func term_to_atom(term.term) = atom is semidet.
:- func det_term_to_atom(term.term) = atom.

	% formula <--> ground formula
:- func ground_formula_to_formula(ground_atom) = atom.
:- func formula_to_ground_formula(atom) = ground_atom is semidet.
:- func det_formula_to_ground_formula(atom) = ground_atom.

	% predicate version of the above
:- pred ground_formula(atom, ground_atom).
:- mode ground_formula(in, out) is semidet.
:- mode ground_formula(out, in) is det.


% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

:- type vscope(T)
	--->	vs(T, varset).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

:- type modalised(M, T)
	--->	m(M, T).

:- type with_assumability_function(T)
	--->	cf(T, assumability_function).

:- type tested(T)
	--->	test(T).

:- type rule_antecedent(M)
	--->	std(with_assumability_function(matom(M)))
	;	test(mtest(M))
	.

:- type rule_head(M)
	--->	std(matom(M))
	;	test(mtest(M))
	.

:- type mtest(M)
	--->	prop(matom(M))
	;	impl(list(matom(M)), matom(M))  % embedded implication
	.

:- type matom(M) == modalised(list(M), atom).
:- type mgatom(M) == modalised(list(M), ground_atom).
:- type mrule(M) == modalised(list(M), pair(list(rule_antecedent(M)), rule_head(M))).
:- type disjoint_decl(M) == set(mgatom(M)).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

:- type subst == map(var, lang.term).

:- func apply_subst_to_term(subst, lang.term) = lang.term.
:- func apply_subst_to_formula(subst, atom) = atom.
:- func apply_subst_to_matom(subst, matom(M)) = matom(M) <= modality(M).
:- func apply_subst_to_mtest(subst, mtest(M)) = mtest(M) <= modality(M).

:- func rename_vars_in_term(map(var, var), lang.term) = lang.term.
:- func rename_vars_in_formula(map(var, var), atom) = atom.
:- func rename_vars_in_matom(map(var, var), matom(M)) = matom(M) <= modality(M).
:- func rename_vars_in_annot_matom(map(var, var), with_assumability_function(matom(M))) = with_assumability_function(matom(M))
		<= modality(M).
:- func rename_vars_in_rule_antecedent(map(var, var), rule_antecedent(M)) = rule_antecedent(M) <= modality(M).
:- func rename_vars_in_rule_head(map(var, var), rule_head(M)) = rule_head(M) <= modality(M).
:- func rename_vars_in_mtest(map(var, var), mtest(M)) = mtest(M) <= modality(M).
:- func rename_vars_in_mrule(map(var, var), mrule(M)) = mrule(M) <= modality(M).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

:- pred ground_matom(matom(M), mgatom(M)) <= modality(M).
:- mode ground_matom(in, out) is semidet.
:- mode ground_matom(out, in) is det.

:- func ground_matom_to_matom(mgatom(M)) = matom(M) <= modality(M).
:- func matom_to_ground_matom(matom(M)) = mgatom(M) is semidet <= modality(M).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

:- pred unify_formulas(atom::in, atom::in, subst::out) is semidet.
:- pred unify_terms(lang.term::in, lang.term::in, subst::out) is semidet.

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

:- func rule_head_matom(rule_head(M)) = matom(M) is det <= modality(M).

%------------------------------------------------------------------------------%

:- implementation.

:- import_module require.
:- import_module string.

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

atom_to_term(p(PredSym, Args))
		= functor(atom(PredSym), list.map(lang_term_to_term, Args), context("", 0)).

term_to_atom(T, AF) :-
	AF = term_to_atom(T).

term_to_atom(functor(atom(PredSym), TermArgs, _))
		= p(PredSym, Args) :-
	list.map(term_to_lang_term, TermArgs, Args).

det_term_to_atom(T) = AF :-
	(if AF0 = term_to_atom(T)
	then AF = AF0
	else error("error in func det_term_to_atom/1 for \"" ++ string(T) ++ "\"")
	).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

lang_term_to_term(v(Var)) = variable(Var, context("", 0)).
lang_term_to_term(t(F, Args)) = functor(atom(F), list.map(lang_term_to_term, Args), context("", 0)).

term_to_lang_term(T, FT) :-
	FT = term_to_lang_term(T).

term_to_lang_term(variable(Var, _)) = v(Var).
term_to_lang_term(functor(atom(Functor), TermArgs, _)) = t(Functor, Args) :-
	list.map(term_to_lang_term, TermArgs, Args).
term_to_lang_term(functor(string(Str), TermArgs, _)) = t(Str, Args) :-
	list.map(term_to_lang_term, TermArgs, Args).

det_term_to_lang_term(T) = FT :-
	(if FT0 = term_to_lang_term(T)
	then FT = FT0
	else error("error in func det_term_to_lang_term/1 for \"" ++ string(T) ++ "\"")
	).

%------------------------------------------------------------------------------%

apply_subst_to_mtest(Subst, prop(MAtom)) = prop(apply_subst_to_matom(Subst, MAtom)).
apply_subst_to_mtest(Subst, impl(MPs, HMP)) = impl(list.map(apply_subst_to_matom(Subst), MPs),
		apply_subst_to_matom(Subst, HMP)).

apply_subst_to_matom(Subst, m(M, Atom)) = m(M, apply_subst_to_formula(Subst, Atom)).

apply_subst_to_formula(Subst, p(PredSym, Args)) = p(PredSym, SubstArgs) :-
	SubstArgs0 = list.map(apply_subst_to_term(Subst), Args),
	(if SubstArgs0 = Args
	then SubstArgs = SubstArgs0
	else p(_, SubstArgs) = apply_subst_to_formula(Subst, p(PredSym, SubstArgs0))
	).

apply_subst_to_term(Subst, t(Functor, Args)) = t(Functor, SubstArgs) :-
	SubstArgs0 = list.map(apply_subst_to_term(Subst), Args),
	(if SubstArgs0 = Args
	then SubstArgs = SubstArgs0
	else
		(if
			t(_, SubstArgs1) = apply_subst_to_term(Subst, t(Functor, SubstArgs0))
		then SubstArgs = SubstArgs1
		else error("in apply_subst_to_term/2")
		)
	).

apply_subst_to_term(Subst, v(Var)) = SubstVar :-
	(if Value = Subst^elem(Var)
	then SubstVar = Value
	else SubstVar = v(Var)
	).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

rename_vars_in_term(Renaming, v(Var)) = v(map.lookup(Renaming, Var)).
rename_vars_in_term(Renaming, t(F, Args)) = t(F, SubstArgs) :-
	SubstArgs = list.map(rename_vars_in_term(Renaming), Args).

rename_vars_in_formula(Renaming, p(PS, Args)) = p(PS, SubstArgs) :-
	SubstArgs = list.map(rename_vars_in_term(Renaming), Args).

rename_vars_in_matom(Renaming, m(M, Atom)) = m(M, rename_vars_in_formula(Renaming, Atom)).

rename_vars_in_annot_matom(Renaming, cf(MAtom, F)) = cf(rename_vars_in_matom(Renaming, MAtom), F).

rename_vars_in_mtest(Renaming, prop(MAtom)) = prop(rename_vars_in_matom(Renaming, MAtom)).
rename_vars_in_mtest(Renaming, impl(MPs, HMP)) = impl(list.map(rename_vars_in_matom(Renaming), MPs),
		rename_vars_in_matom(Renaming, HMP)).

rename_vars_in_rule_antecedent(Renaming, test(MTest)) = test(rename_vars_in_mtest(Renaming, MTest)).
rename_vars_in_rule_antecedent(Renaming, std(AnnotMAtom))
		= std(rename_vars_in_annot_matom(Renaming, AnnotMAtom)).

rename_vars_in_rule_head(Renaming, test(MTest)) = test(rename_vars_in_mtest(Renaming, MTest)).
rename_vars_in_rule_head(Renaming, std(MAtom)) = std(rename_vars_in_matom(Renaming, MAtom)).

rename_vars_in_mrule(Renaming, m(M, Ante-Succ)) =
		m(M, list.map(rename_vars_in_rule_antecedent(Renaming), Ante)-rename_vars_in_rule_head(Renaming, Succ)).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

ground_matom(m(M, F), m(M, GF)) :-
	ground_formula(F, GF).

ground_matom_to_matom(GM) = M :-
	ground_matom(M, GM).

matom_to_ground_matom(M) = GM :-
	ground_matom(M, GM).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

unify_formulas(A, B, U) :-
	unify_term(atom_to_term(A), atom_to_term(B), init, TermU),
	U = map.map_values((func(_, TermTgt) = det_term_to_lang_term(TermTgt)), TermU).

unify_terms(TA, TB, U) :-
	unify_term(lang_term_to_term(TA), lang_term_to_term(TB), init, TermU),
	U = map.map_values((func(_, TermTgt) = det_term_to_lang_term(TermTgt)), TermU).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

ground_formula_to_formula(p(PredSym, GroundArgs)) = p(PredSym, list.map(ground_term_to_term, GroundArgs)).

formula_to_ground_formula(p(PredSym, Args)) = p(PredSym, GroundArgs) :-
	list.map((pred(T::in, GT::out) is semidet :-
		GT = term_to_ground_term(T)
			), Args, GroundArgs).

det_formula_to_ground_formula(F) = GF :-
	(if GF0 = formula_to_ground_formula(F)
	then GF = GF0
	else error("det_formula_to_ground_formula/1: formula=" ++ string(F))
	).

:- pragma promise_equivalent_clauses(ground_formula/2).

ground_formula(ground_formula_to_formula(GroundFormula)::out, GroundFormula::in).
ground_formula(Formula::in, formula_to_ground_formula(Formula)::out).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

ground_term_to_term(t(Functor, GroundTerms)) = t(Functor, list.map(ground_term_to_term, GroundTerms)).

term_to_ground_term(t(Functor, Terms)) = t(Functor, GroundTerms) :-
	list.map((pred(T::in, GT::out) is semidet :-
		GT = term_to_ground_term(T)
			), Terms, GroundTerms).

:- pragma promise_equivalent_clauses(ground_term/2).

ground_term(ground_term_to_term(GroundTerm)::out, GroundTerm::in).
ground_term(Term::in, term_to_ground_term(Term)::out).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

rule_head_matom(std(MAtom)) = MAtom.
rule_head_matom(test(prop(MAtom))) = MAtom.
rule_head_matom(test(impl(_, MAtom))) = MAtom.
