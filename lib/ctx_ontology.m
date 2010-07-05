:- module ctx_ontology.

:- interface.
:- import_module ontology, enumerable.

:- type ctx_ontology.

:- instance generator(ctx_ontology, string).
:- instance isa_ontology(ctx_ontology, string).

:- func init = ctx_ontology.

%------------------------------------------------------------------------------%

:- implementation.
:- import_module set, map, pair.

:- instance generator(ctx_ontology, string) where [
	pred(generate/2) is ctx_ontology_generate
].

:- instance isa_ontology(ctx_ontology, string) where [
	pred(direct_isa/3) is ctx_ontology_direct_isa
].

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

:- type ctx_ontology
	--->	ctx_ontology(
		sorts :: set(string),
		disa :: set(pair(string, string))
	).

:- pred ctx_ontology_generate(ctx_ontology::in, string::out) is nondet.

ctx_ontology_generate(Ont, Mem) :-
	set.member(Mem, Ont^sorts).

:- pred ctx_ontology_direct_isa(ctx_ontology, string, string).
:- mode ctx_ontology_direct_isa(in, in, in) is semidet.
:- mode ctx_ontology_direct_isa(in, in, out) is nondet.

ctx_ontology_direct_isa(Ont, A, B) :-
	member(A-B, Ont^disa).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

init = ctx_ontology(set.init, set.init).
