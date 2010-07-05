:- module ctx_io.

:- interface.
:- import_module stringable.

:- import_module belief_model.
:- import_module ctx_modality.

:- instance stringable(ctx_modality).
:- instance term_parsable(ctx_modality).

:- instance stringable(belief).
:- instance term_parsable(belief).

%------------------------------------------------------------------------------%

:- implementation.
:- import_module list, string, set, term, require.
:- import_module stf.

:- instance stringable(ctx_modality) where [
	func(to_string/1) is ctx_modality_to_string
].

:- instance term_parsable(ctx_modality) where [
	func(from_term/1) is ctx_modality_from_term
].

:- instance stringable(belief) where [
	func(to_string/1) is belief_to_string
].

:- instance term_parsable(belief) where [
	func(from_term/1) is term_to_belief
].

%------------------------------------------------------------------------------%

:- func l_to_r(pred(A, B), A) = B.
:- mode l_to_r(pred(in, out) is det, in) = (out) is det.

l_to_r(Pred, A) = B :-
	call(Pred, A, B).

:- func r_to_l(pred(A, B), B) = A.
:- mode r_to_l(pred(out, in) is semidet, in) = (out) is semidet.

r_to_l(Pred, B) = A :-
	call(Pred, A, B).

%------------------------------------------------------------------------------%

:- pred foreground_as_string(foreground, string).
:- mode foreground_as_string(in, out) is det.
:- mode foreground_as_string(out, in) is semidet.

foreground_as_string(com, "com").

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

:- func ctx_modality_to_string(ctx_modality) = string.
:- func ctx_modality_from_term(term.term::in) = (ctx_modality::out) is semidet.

	% axiom
ctx_modality_to_string(any) = "[]".
ctx_modality_from_term(functor(atom("[]"), [], _)) = any.

	% info
ctx_modality_to_string(info) = "i".
ctx_modality_from_term(functor(atom("i"), [], _)) = info.

	% attention state
ctx_modality_to_string(att) = "att".
ctx_modality_from_term(functor(atom("att"), [], _)) = att.

	% events
ctx_modality_to_string(evt) = "event".
ctx_modality_from_term(functor(atom("event"), [], _)) = evt.

	% "knows"
ctx_modality_to_string(k(STF, Ags)) = "k(" ++ to_string(STF) ++ "," ++ belief_to_string(Ags) ++ ")".

ctx_modality_from_term(functor(atom("k"), [
		STFTerm,
		BeliefTerm
	], _)) = k(from_term(STFTerm), term_to_belief(BeliefTerm)).

	% tasks
ctx_modality_to_string(t(STF, Ags)) = "t(" ++ to_string(STF) ++ "," ++ belief_to_string(Ags) ++ ")".

ctx_modality_from_term(functor(atom("t"), [
		STFTerm,
		BeliefTerm
	], _)) = t(from_term(STFTerm), term_to_belief(BeliefTerm)).

ctx_modality_to_string(intention) = "intention".
ctx_modality_from_term(functor(atom("intention"), [], _)) = intention.

ctx_modality_to_string(understanding) = "understand".
ctx_modality_from_term(functor(atom("understand"), [], _)) = understanding.

ctx_modality_to_string(generation) = "generate".
ctx_modality_from_term(functor(atom("generate"), [], _)) = generation.

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

:- func belief_to_string(belief) = string.

belief_to_string(private(A)) = "private(" ++ to_string(A) ++ ")".
belief_to_string(attrib(A, B)) = "attrib(" ++ to_string(A) ++ "," ++ to_string(B) ++ ")".
belief_to_string(mutual(AgS)) = "mutual(" ++ AgSStr ++ ")" :-
	AgSStr = string.join_list(",", list.map(to_string, set.to_sorted_list(AgS))).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

:- func term_to_belief(term.term::in) = (belief::out) is semidet.

term_to_belief(functor(atom("private"), [functor(atom(AStr), [], _)], _)) = private(from_string(AStr)).
term_to_belief(functor(atom("attrib"), [functor(atom(AStr), [], _), functor(atom(BStr), [], _)], _)) = attrib(from_string(AStr), from_string(BStr)).
term_to_belief(functor(atom("mutual"), ATerms, Context)) = mutual(set.from_list(As)) :-
	list.map((pred(functor(atom(AStr), [], _)::in, from_string(AStr)::out) is semidet), ATerms, As),
	(if As = []
	then
		Context = context(Filename, LineNo),
		error("empty set of mutually-believing agents in " ++ Filename ++ " at line " ++ string.from_int(LineNo))
	else
		true
	).
