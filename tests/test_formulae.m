% $Id: test_formulae.m 3449 2009-10-11 22:13:11Z janicek $

:- module test_formulae.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%------------------------------------------------------------------------------%

:- implementation.

:- import_module string, pair, list, map.
:- import_module varset, term, term_io, parser.
:- import_module formula, formula_io.

:- import_module ctx_modality, ctx_io.

main(!IO) :-
	test_mprop_parse("p(x).", !IO),
	test_mprop_parse("p(A).", !IO),
	test_mprop_parse("a.", !IO),
	test_mprop_parse("a(b(c)).", !IO),
	test_mprop_parse("e0:p(x).", !IO),
	test_mprop_parse("i0:a0:p(x).", !IO),
	test_mprop_parse("axiom : p(x).", !IO),
	test_mprop_parse("i : A \\= B.", !IO),
	test_mprop_parse("i : A = B.", !IO),

	nl(!IO),

	test_term_parse("a(x) ; b(x) ; c(y).", !IO),
	test_term_parse("axiom : p(x).", !IO),
	test_term_parse("[] : (p(X), q(X) -> r(X)) / fax.", !IO),
	test_term_parse("funcname = [a, b, c].", !IO),
	test_term_parse("\"Jakysi string\", g-q.", !IO),
	test_term_parse("\"Jakysi string\"::\"g-q\".", !IO),
	test_term_parse("e(now) : @(\"be1_1\"::\"ascription\", p(\"be\") ^ r(\"Mood\", p(\"int\")) ^ r(\"Tense\", p(\"pres\")) ^  r(\"Cop-Restr\", \"ball1_1\"::\"thing\" ^ p(\"ball\") ^ r(\"Delimitation\", p(\"unique\")) ^ r(\"Num\", p(\"sg\")) ^ r(\"Quantification\", p(\"specific\")))) ^  r(\"Cop-Scope\", \"red1_1\"::\"q-color\" ^ p(\"red\"))) ^  r(\"Subject\", \"ball1_1\"::\"thing\")).", !IO),
	test_term_parse("a = b.", !IO),
	test_term_parse("A = B.", !IO),
	test_term_parse("a \\= B.", !IO),
	test_term_parse("A \\= B.", !IO),

	nl(!IO),

	test_mrule_parse("b <- a / 0.1.", !IO),
	test_mrule_parse("d <- a / 0.1, b / 0.2, c / 0.3.", !IO),
	test_mrule_parse("d <- A = B / x, b / 0.2, c / 0.3.", !IO),
	test_mrule_parse("i0:(a0:head(x) <- e0:first(x) / f1, e0:second(x) / 0.1).", !IO),
	test_mrule_parse("[]:(a0:head(x) <- e0:first(f(x)) / f1, e0:second(x) / f2).", !IO),
	test_mrule_parse("[]:i0:(a0:head(x) <- e0:first(f(x)) / f1, e0:second(x) / f2).", !IO),

	nl(!IO),

	test_unify("p(X).", "p(Y).", !IO),
	test_unify("p(a, b).", "p(X, b).", !IO),
	test_unify("p(X, b).", "p(a, b).", !IO),
	test_unify("p(x).", "p(y).", !IO),
	test_unify("r(x).", "p(x).", !IO),
	test_unify("p(a, b).", "p(a).", !IO),
	test_unify("p(X, a).", "p(Y, Y).", !IO),
	test_unify("p(X, X).", "p(a, Y).", !IO),
	test_unify("p(X, Y).", "p(Z, Z).", !IO),
	test_unify("p(X).", "p(p(y)).", !IO).
	
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

:- import_module formula_ops.

:- pred test_term_parse(string::in, io::di, io::uo) is det.

test_term_parse(S, !IO) :-
	read_term_from_string_with_op_table(init_wabd_op_table, "", S, _, Result),
	(if Result = term(_Varset, Term)
	then generic_term(Term)
	else true
	),
	format("* `%s':\n  result=%s\n\n", [s(S), s(string(Result))], !IO).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

:- pred test_mprop_parse(string::in, io::di, io::uo) is det.

test_mprop_parse(S, !IO) :-
	format("(mprop) \"%s\" ... ", [s(S)], !IO),
	(if string_to_vsmprop(S) = P, P = vs(m(M, _), _), is_list_ctx_modality(M)
	then print(P, !IO), nl(!IO),
			format("        \"%s\"\n", [s(vsmprop_to_string(P))], !IO)
	else print("fail", !IO), nl(!IO)
	).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

:- pred is_list_ctx_modality(list(ctx_modality)::in) is det.

is_list_ctx_modality(_).

:- pred is_ctx_mprop(mprop(ctx_modality)::in) is det.

is_ctx_mprop(_).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

:- pred test_mrule_parse(string::in, io::di, io::uo) is det.

test_mrule_parse(S, !IO) :-
	format("(mrule) \"%s\" ... ", [s(S)], !IO),
	(if string_to_vsmrule(S) = R, R = vs(m(M, _), _), is_list_ctx_modality(M)
	then print(R, !IO), nl(!IO),
			format("        \"%s\"\n", [s(vsmrule_to_string(R))], !IO)
	else print("fail", !IO), nl(!IO)
	).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

:- pred test_unify(string::in, string::in, io::di, io::uo) is det.

test_unify(A, B, !IO) :-
	vs(MPA, VSA) = det_string_to_vsmprop(A),
	vs(MPB0, VSB) = det_string_to_vsmprop(B),

	is_ctx_mprop(MPA),
	is_ctx_mprop(MPB0),

	varset.merge_renaming(VSA, VSB, VS, Renaming),
	MPB = rename_vars_in_mprop(Renaming, MPB0),

	format("(unify) \"%s\" == \"%s\": ", [s(vsmprop_to_string(vs(MPA, VS))),
			s(vsmprop_to_string(vs(MPB, VS)))], !IO),

	MPA = m(_, PA),
	MPB = m(_, PB),

	(if
		unify_formulas(PA, PB, Unifier)
	then
		print(test_formulae.subst_to_string(VS, Unifier), !IO),
		print(" --> ", !IO),
		format("\"%s\", \"%s\"\n",
			[s(vsmprop_to_string(vs(apply_subst_to_mprop(Unifier, MPA), VS))),
			s(vsmprop_to_string(vs(apply_subst_to_mprop(Unifier, MPB), VS)))], !IO)
	else
		print("not unifiable.\n", !IO)
	).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

:- func subst_to_string(varset, subst) = string.

subst_to_string(Varset, Subst) = Str :-
	L = map.to_assoc_list(Subst),
	L0 = list.map((func(Var-Value) = S :-
		S = varset.lookup_name(Varset, Var) ++ "=" ++ formula_term_to_string(Varset, Value)), L),
	Str = string.join_list(", ", L0).
