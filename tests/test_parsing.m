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

:- module test_parsing.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%------------------------------------------------------------------------------%

:- implementation.

:- import_module string, pair, list, map.
:- import_module varset, term, term_io, parser.
:- import_module lang, lang_io.

:- import_module ctx_modality, ctx_io.

main(!IO) :-
	test_matom_parse("p(x).", !IO),
	test_matom_parse("p(A).", !IO),
	test_matom_parse("a.", !IO),
	test_matom_parse("a(b(c)).", !IO),
	test_matom_parse("e0:p(x).", !IO),
	test_matom_parse("i0:a0:p(x).", !IO),
	test_matom_parse("axiom : p(x).", !IO),
	test_matom_parse("i : A \\= B.", !IO),
	test_matom_parse("i : A = B.", !IO),

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
	test_term_parse("disjunct([p(x), p(y), q(z)]).", !IO),
	test_term_parse("(p(x) ; p(y)).", !IO),

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

:- import_module lang_ops.

:- pred test_term_parse(string::in, io::di, io::uo) is det.

test_term_parse(S, !IO) :-
	read_term_from_string_with_op_table(init_wabd_op_table, "", S, _, Result),
	(if Result = term(_Varset, Term)
	then generic_term(Term)
	else true
	),
	format("* `%s':\n  result=%s\n\n", [s(S), s(string(Result))], !IO).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

:- pred test_matom_parse(string::in, io::di, io::uo) is det.

test_matom_parse(S, !IO) :-
	format("(matom) \"%s\" ... ", [s(S)], !IO),
	(if string_to_vsmatom(S) = P, P = vs(m(M, _), _), is_list_ctx_modality(M)
	then print(P, !IO), nl(!IO),
			format("        \"%s\"\n", [s(vsmatom_to_string(P))], !IO)
	else print("fail", !IO), nl(!IO)
	).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

:- pred is_list_ctx_modality(list(ctx_modality)::in) is det.

is_list_ctx_modality(_).

:- pred is_ctx_matom(matom(ctx_modality)::in) is det.

is_ctx_matom(_).

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
	vs(MPA, VSA) = det_string_to_vsmatom(A),
	vs(MPB0, VSB) = det_string_to_vsmatom(B),

	is_ctx_matom(MPA),
	is_ctx_matom(MPB0),

	varset.merge_renaming(VSA, VSB, VS, Renaming),
	MPB = rename_vars_in_matom(Renaming, MPB0),

	format("(unify) \"%s\" == \"%s\": ", [s(vsmatom_to_string(vs(MPA, VS))),
			s(vsmatom_to_string(vs(MPB, VS)))], !IO),

	MPA = m(_, PA),
	MPB = m(_, PB),

	(if
		unify_formulas(PA, PB, Unifier)
	then
		print(test_parsing.subst_to_string(VS, Unifier), !IO),
		print(" --> ", !IO),
		format("\"%s\", \"%s\"\n",
			[s(vsmatom_to_string(vs(apply_subst_to_matom(Unifier, MPA), VS))),
			s(vsmatom_to_string(vs(apply_subst_to_matom(Unifier, MPB), VS)))], !IO)
	else
		print("not unifiable.\n", !IO)
	).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

:- func subst_to_string(varset, subst) = string.

subst_to_string(Varset, Subst) = Str :-
	L = map.to_assoc_list(Subst),
	L0 = list.map((func(Var-Value) = S :-
		S = varset.lookup_name(Varset, Var) ++ "=" ++ lang_term_to_string(Varset, Value)), L),
	Str = string.join_list(", ", L0).
