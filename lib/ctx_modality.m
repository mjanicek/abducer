% $Id: context.m 1266 2009-07-13 01:58:02Z janicek $

:- module ctx_modality.

:- interface.

:- import_module modality.
:- import_module stf, belief_model.

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

:- type ctx_modality
	--->	any  % the axiom
	;	att
	;	evt
	;	info
	;	k(stf, belief)
	;	t(stf, belief)
	;	intention
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

:- func stf_compose(stf::in, stf::in) = (stf::out) is semidet.

stf_compose(now, now) = now.

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

:- func belief_compose(belief::in, belief::in) = (belief::out) is semidet.

belief_compose(private(A), private(B)) = attrib(A, B) :- A \= B.

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

:- func ctx_modality_compose(ctx_modality::in, ctx_modality::in) = (ctx_modality::out) is semidet.

ctx_modality_compose(k(STF1, Bel1), k(STF2, Bel2)) = k(stf_compose(STF1, STF2), belief_compose(Bel1, Bel2)).
ctx_modality_compose(t(STF1, Bel1), t(STF2, Bel2)) = t(stf_compose(STF1, STF2), belief_compose(Bel1, Bel2)).
