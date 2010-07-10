% $Id: context.m 1266 2009-07-13 01:58:02Z janicek $

:- module ctx_modality.

:- interface.

:- import_module modality.

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

:- type ctx_modality
	--->	any  % the axiom
	;	truth
	;	evt
	;	belief
	;	intention
	;	attention
	;	understanding
	;	generation
	.

:- instance modality(ctx_modality).

:- pred is_ctx_modality(ctx_modality::in) is det.

%==============================================================================%

:- implementation.

:- import_module require.
:- import_module set, pair.

%------------------------------------------------------------------------------%

:- instance modality(ctx_modality) where [
	func(axiom/0) is ctx_modality_axiom,
	func(compose/2) is ctx_modality_compose
].

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

:- func ctx_modality_axiom = ctx_modality.

ctx_modality_axiom = any.

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

is_ctx_modality(_).

%------------------------------------------------------------------------------%

:- func ctx_modality_compose(ctx_modality::in, ctx_modality::in) = (ctx_modality::out) is failure.

%ctx_modality_compose(bel(private(A)), bel(private(B))) = bel(attrib(A, set(B))) :- A \= B.

ctx_modality_compose(A, B) = C :-
	fail.
