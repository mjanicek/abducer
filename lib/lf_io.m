:- module lf_io.

:- interface.

:- import_module lf.
:- import_module string, formula.
:- import_module stringable.

:- func ground_term_to_id(ground_term::in) = (id(I, S)::out) is semidet <= (parsable(I), parsable(S)).
:- func ground_term_to_lf(ground_term::in) = (lf(I, S, R)::out) is semidet <= (parsable(I), parsable(S), parsable(R)).

:- func ground_atomic_formula_to_lf(ground_atomic_formula::in) = (lf(I, S, R)::out) is semidet <= (parsable(I), parsable(S), parsable(R)).
:- func det_ground_atomic_formula_to_lf(ground_atomic_formula) = lf(I, S, R) <= (parsable(I), parsable(S), parsable(R)).

:- func lf_to_string(lf(I, S, R)) = string <= (stringable(I), stringable(S), stringable(R)).

:- func id_to_ground_term(id(I, S)) = ground_term <= (stringable(I), stringable(S)).
:- func lf_to_ground_term(lf(I, S, R)) = ground_term <= (stringable(I), stringable(S), stringable(R)).
:- func lf_to_ground_atomic_formula(lf(I, S, R)) = ground_atomic_formula <= (stringable(I), stringable(S), stringable(R)).

%------------------------------------------------------------------------------%

:- implementation.
:- import_module require.
:- import_module list.

%------------------------------------------------------------------------------%

ground_term_to_id(t("::", [t(Name, []), t(Sort, [])])) = of_sort(from_string(Name), from_string(Sort)).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

ground_term_to_lf(t("@", [NameSortTerm, LFTerm])) =
		at(ground_term_to_id(NameSortTerm), ground_term_to_lf(LFTerm)).

ground_term_to_lf(T) = i(ground_term_to_id(T)) :-
	T = t("::", [_,_]).

ground_term_to_lf(t("r", [t(RelName, []), LFTerm])) = r(from_string(RelName), ground_term_to_lf(LFTerm)).

ground_term_to_lf(t("p", [t(Prop, [])])) = p(Prop).

ground_term_to_lf(t("^", [LFA, LFB])) = and(ground_term_to_lf(LFA), ground_term_to_lf(LFB)).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

ground_atomic_formula_to_lf(p(PredSym, Args)) = ground_term_to_lf(t(PredSym, Args)).

det_ground_atomic_formula_to_lf(GF) = LF :-
	(if ground_atomic_formula_to_lf(GF) = LF0
	then LF = LF0
	else error("error in det_ground_atomic_formula_to_lf/1: " ++ string(GF))
	).

%------------------------------------------------------------------------------%

:- func id_to_string(id(I, S)) = string <= (stringable(I), stringable(S)).

id_to_string(of_sort(I, S)) = to_string(I) ++ ":" ++ to_string(S).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

lf_to_string(at(Id, LF)) = "@" ++ id_to_string(Id) ++ "(" ++ lf_to_string(LF) ++ ")".
lf_to_string(i(Id)) = id_to_string(Id).
lf_to_string(r(Rel, LF)) = "<" ++ to_string(Rel) ++ ">" ++ "(" ++ lf_to_string(LF) ++ ")".
lf_to_string(p(Prop)) = Prop.
lf_to_string(and(LF1, LF2)) = lf_to_string(LF1) ++ " ^ " ++ lf_to_string(LF2).

%------------------------------------------------------------------------------%

id_to_ground_term(of_sort(Idx, Sort)) = t("::", [t(to_string(Idx), []), t(to_string(Sort), [])]).

lf_to_ground_term(at(Id, LF)) = t("@", [id_to_ground_term(Id), lf_to_ground_term(LF)]).
lf_to_ground_term(i(Id)) = id_to_ground_term(Id).
lf_to_ground_term(r(Rel, LF)) = t("r", [t(to_string(Rel), []), lf_to_ground_term(LF)]).
lf_to_ground_term(p(Prop)) = t("p", [t(Prop, [])]).
lf_to_ground_term(and(LF1, LF2)) = t("^", [lf_to_ground_term(LF1), lf_to_ground_term(LF2)]).

lf_to_ground_atomic_formula(LF) = p(PredSym, Args) :-
	t(PredSym, Args) = lf_to_ground_term(LF).
