%------------------------------------------------------------------------------%
% Copyright (C) 2009-2011 DFKI GmbH Talking Robots 
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

ctx_modality_compose(_A, _B) = _C :-
	fail.
