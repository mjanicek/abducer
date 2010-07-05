:- module stringable.

:- interface.

:- import_module string, term.

:- typeclass stringable(T) where [
	func to_string(T) = string
].

:- instance stringable(string).

:- typeclass parsable(T) where [
	func from_string(string::in) = (T::out) is semidet
].

:- func det_from_string(string) = T <= parsable(T).

:- instance parsable(string).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

:- typeclass term_parsable(T) where [
	func from_term(term.term::in) = (T::out) is semidet
].

:- func det_from_term(term.term) = T <= term_parsable(T).

%------------------------------------------------------------------------------%

:- implementation.

:- import_module require.

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

det_from_string(S) = Rep :-
	(if Rep0 = from_string(S)
	then Rep = Rep0
	else error("failed to parse string `" ++ S ++ "' in func det_from_string/1.")
	).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

det_from_term(Rep) = Val :-
	(if Val0 = from_term(Rep)
	then Val = Val0
	else error("failed to parse `" ++ string(Rep) ++ "' in func det_parse/1.")
	).

%------------------------------------------------------------------------------%

:- instance stringable(string) where [
	(to_string(S) = S)
].

:- instance parsable(string) where [
	(from_string(S) = S)
].
