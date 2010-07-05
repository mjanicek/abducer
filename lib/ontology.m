:- module ontology.

:- interface.

:- typeclass isa_ontology(OT, T) where [

		% direct_isa(Ont, B, A)
		% True iff B `is-a` A in Ont.
		%
		% That is, w:B -> w:A.
	pred direct_isa(OT, T, T),
	mode direct_isa(in, in, in) is semidet,
	mode direct_isa(in, in, out) is nondet
].

	% reflexive and transitive closure of direct_isa
:- pred isa(OT, T, T) <= isa_ontology(OT, T).
:- mode isa(in, in, in) is semidet.

	% more_specific(Ont, X, Y) = Z
	%
	% X is-a Y -> Z = X
	% Y is-a X -> Z = Y
	%
	% fail otherwise
	%
:- func more_specific(OT, T, T) = T <= isa_ontology(OT, T).
:- mode more_specific(in, in, in) = out is semidet.

%------------------------------------------------------------------------------%

:- implementation.

isa(_, X, X).
isa(O, X, Y) :-
	direct_isa(O, X, Z),
	Z \= X,
	isa(O, Z, Y).

more_specific(O, X, Y) = Z :-
	(isa(O, X, Y) -> Z = X ;
	(isa(O, Y, X) -> Z = Y ;
	fail
	)).
