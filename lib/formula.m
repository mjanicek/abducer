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

:- module formula.

:- interface.

:- import_module list, map, set, pair.
:- import_module term, varset.
:- import_module assumability.
:- import_module modality.

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

:- type atomic_formula
	--->	p(
		string,  % predicate symbol
		list(formula.term)
	).

:- type ground_atomic_formula
	--->	p(
		string,  % predicate symbol
		list(ground_term)
	).

	% prolog term <--> formula.atomic_formula
:- func atomic_formula_to_term(atomic_formula) = term.term.

:- pred term_to_atomic_formula(term.term::in, atomic_formula::out) is semidet.
:- func term_to_atomic_formula(term.term) = atomic_formula is semidet.
:- func det_term_to_atomic_formula(term.term) = atomic_formula.

	% formula <--> ground formula
:- func ground_formula_to_formula(ground_atomic_formula) = atomic_formula.
:- func formula_to_ground_formula(atomic_formula) = ground_atomic_formula is semidet.
:- func det_formula_to_ground_formula(atomic_formula) = ground_atomic_formula.

	% predicate version of the above
:- pred ground_formula(atomic_formula, ground_atomic_formula).
:- mode ground_formula(in, out) is semidet.
:- mode ground_formula(out, in) is det.

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

:- type term
	--->	v(var)
	;	t(string, list(formula.term))
	.

:- type ground_term
	---> 	t(string, list(ground_term))
	.

	% value under the open world assumption
:- type value(T)
	--->	true(T)
	;	false(T)
	.

	% prolog term <--> formula.term
:- func formula_term_to_term(formula.term) = term.term.

:- pred term_to_formula_term(term.term::in, formula.term::out) is semidet.
:- func term_to_formula_term(term.term) = formula.term is semidet.
:- func det_term_to_formula_term(term.term) = formula.term.

	% term <--> ground term
:- func ground_term_to_term(ground_term) = formula.term.
:- func term_to_ground_term(formula.term) = ground_term is semidet.

	% predicate version of the above
:- pred ground_term(formula.term, ground_term).
:- mode ground_term(in, out) is semidet.
:- mode ground_term(out, in) is det.

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

:- type vscope(T)
	--->	vs(
		body :: T,
		vars :: varset
	).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

:- type modalized(M, T)
	--->	m(
		m :: M,
		p :: T
	).

:- type with_assumability_function(T)
	--->	cf(T, assumability_function).

:- type tested(T)
	--->	test(T).

:- type rule_antecedent(M)
	--->	std(with_assumability_function(mprop(M)))
	;	test(mtest(M))
	.

:- type rule_head(M)
	--->	std(mprop(M))
	;	test(mtest(M))
	.

:- type mprop(M) == modalized(list(M), atomic_formula).
:- type mrule(M) == modalized(list(M), pair(list(rule_antecedent(M)), rule_head(M))).
%:- type mrule(M) == modalized(list(M), pair(list(with_cost_function(mprop(M))), mprop(M))).

:- type mtest(M)
	--->	prop(mprop(M))
	;	impl(list(mprop(M)), mprop(M))  % embedded implication
	.

:- type mgprop(M) == modalized(list(M), ground_atomic_formula).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

:- type disjoint(M) == set(mgprop(M)).
:- type assumable_function_def(M) == pair(string, map(mgprop(M), float)).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

:- type subst == map(var, formula.term).

:- func apply_subst_to_term(subst, formula.term) = formula.term.
:- func apply_subst_to_formula(subst, atomic_formula) = atomic_formula.
:- func apply_subst_to_mprop(subst, mprop(M)) = mprop(M) <= modality(M).
:- func apply_subst_to_mtest(subst, mtest(M)) = mtest(M) <= modality(M).

:- func rename_vars_in_term(map(var, var), formula.term) = formula.term.
:- func rename_vars_in_formula(map(var, var), atomic_formula) = atomic_formula.
:- func rename_vars_in_mprop(map(var, var), mprop(M)) = mprop(M) <= modality(M).
:- func rename_vars_in_annot_mprop(map(var, var), with_assumability_function(mprop(M))) = with_assumability_function(mprop(M))
		<= modality(M).
:- func rename_vars_in_rule_antecedent(map(var, var), rule_antecedent(M)) = rule_antecedent(M) <= modality(M).
:- func rename_vars_in_rule_head(map(var, var), rule_head(M)) = rule_head(M) <= modality(M).
:- func rename_vars_in_mtest(map(var, var), mtest(M)) = mtest(M) <= modality(M).
:- func rename_vars_in_mrule(map(var, var), mrule(M)) = mrule(M) <= modality(M).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

:- pred ground_mprop(mprop(M), mgprop(M)) <= modality(M).
:- mode ground_mprop(in, out) is semidet.
:- mode ground_mprop(out, in) is det.

:- func ground_mprop_to_mprop(mgprop(M)) = mprop(M) <= modality(M).
:- func mprop_to_ground_mprop(mprop(M)) = mgprop(M) is semidet <= modality(M).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

:- pred unify_formulas(atomic_formula::in, atomic_formula::in, subst::out) is semidet.
:- pred unify_terms(formula.term::in, formula.term::in, subst::out) is semidet.

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

:- func rule_head_mprop(rule_head(M)) = mprop(M) is det <= modality(M).

%------------------------------------------------------------------------------%

:- implementation.

:- import_module require.
:- import_module string.

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

atomic_formula_to_term(p(PredSym, Args))
		= functor(atom(PredSym), list.map(formula_term_to_term, Args), context("", 0)).

term_to_atomic_formula(T, AF) :-
	AF = term_to_atomic_formula(T).

term_to_atomic_formula(functor(atom(PredSym), TermArgs, _))
		= p(PredSym, Args) :-
	list.map(term_to_formula_term, TermArgs, Args).

det_term_to_atomic_formula(T) = AF :-
	(if AF0 = term_to_atomic_formula(T)
	then AF = AF0
	else error("error in func det_term_to_atomic_formula/1 for \"" ++ string(T) ++ "\"")
	).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

formula_term_to_term(v(Var)) = variable(Var, context("", 0)).
formula_term_to_term(t(F, Args)) = functor(atom(F), list.map(formula_term_to_term, Args), context("", 0)).

term_to_formula_term(T, FT) :-
	FT = term_to_formula_term(T).

term_to_formula_term(variable(Var, _)) = v(Var).
term_to_formula_term(functor(atom(Functor), TermArgs, _)) = t(Functor, Args) :-
	list.map(term_to_formula_term, TermArgs, Args).
term_to_formula_term(functor(string(Str), TermArgs, _)) = t(Str, Args) :-
	list.map(term_to_formula_term, TermArgs, Args).

det_term_to_formula_term(T) = FT :-
	(if FT0 = term_to_formula_term(T)
	then FT = FT0
	else error("error in func det_term_to_formula_term/1 for \"" ++ string(T) ++ "\"")
	).

%------------------------------------------------------------------------------%

apply_subst_to_mtest(Subst, prop(MProp)) = prop(apply_subst_to_mprop(Subst, MProp)).
apply_subst_to_mtest(Subst, impl(MPs, HMP)) = impl(list.map(apply_subst_to_mprop(Subst), MPs),
		apply_subst_to_mprop(Subst, HMP)).

apply_subst_to_mprop(Subst, m(M, Prop)) = m(M, apply_subst_to_formula(Subst, Prop)).

apply_subst_to_formula(Subst, p(PropSym, Args)) = p(PropSym, SubstArgs) :-
	SubstArgs0 = list.map(apply_subst_to_term(Subst), Args),
	(if SubstArgs0 = Args
	then SubstArgs = SubstArgs0
	else p(_, SubstArgs) = apply_subst_to_formula(Subst, p(PropSym, SubstArgs0))
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

rename_vars_in_mprop(Renaming, m(M, Prop)) = m(M, rename_vars_in_formula(Renaming, Prop)).

rename_vars_in_annot_mprop(Renaming, cf(MProp, F)) = cf(rename_vars_in_mprop(Renaming, MProp), F).

rename_vars_in_mtest(Renaming, prop(MProp)) = prop(rename_vars_in_mprop(Renaming, MProp)).
rename_vars_in_mtest(Renaming, impl(MPs, HMP)) = impl(list.map(rename_vars_in_mprop(Renaming), MPs),
		rename_vars_in_mprop(Renaming, HMP)).

rename_vars_in_rule_antecedent(Renaming, test(MTest)) = test(rename_vars_in_mtest(Renaming, MTest)).
rename_vars_in_rule_antecedent(Renaming, std(AnnotMProp))
		= std(rename_vars_in_annot_mprop(Renaming, AnnotMProp)).

rename_vars_in_rule_head(Renaming, test(MTest)) = test(rename_vars_in_mtest(Renaming, MTest)).
rename_vars_in_rule_head(Renaming, std(MProp)) = std(rename_vars_in_mprop(Renaming, MProp)).

rename_vars_in_mrule(Renaming, m(M, Ante-Succ)) =
		m(M, list.map(rename_vars_in_rule_antecedent(Renaming), Ante)-rename_vars_in_rule_head(Renaming, Succ)).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

ground_mprop(m(M, F), m(M, GF)) :-
	ground_formula(F, GF).

ground_mprop_to_mprop(GM) = M :-
	ground_mprop(M, GM).

mprop_to_ground_mprop(M) = GM :-
	ground_mprop(M, GM).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

unify_formulas(A, B, U) :-
	unify_term(atomic_formula_to_term(A), atomic_formula_to_term(B), init, TermU),
	U = map.map_values((func(_, TermTgt) = det_term_to_formula_term(TermTgt)), TermU).

unify_terms(TA, TB, U) :-
	unify_term(formula_term_to_term(TA), formula_term_to_term(TB), init, TermU),
	U = map.map_values((func(_, TermTgt) = det_term_to_formula_term(TermTgt)), TermU).

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

rule_head_mprop(std(MProp)) = MProp.
rule_head_mprop(test(prop(MProp))) = MProp.
rule_head_mprop(test(impl(_, MProp))) = MProp.
