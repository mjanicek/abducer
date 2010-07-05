:- module stf.

:- interface.
:- import_module stringable.

:- type stf
	--->	now.

:- instance stringable(stf).
:- instance term_parsable(stf).

%------------------------------------------------------------------------------%

:- implementation.
:- import_module list, term.

:- instance stringable(stf) where [
	func(to_string/1) is stf_to_string
].

:- instance term_parsable(stf) where [
	func(from_term/1) is term_to_stf
].

:- func stf_to_string(stf) = string.
:- func term_to_stf(term.term::in) = (stf::out) is semidet.

stf_to_string(now) = "now".
term_to_stf(functor(atom("now"), [], _)) = now.

