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

:- module ctx_io.

:- interface.
:- import_module stringable.
:- import_module ctx_modality.

:- instance stringable(ctx_modality).
:- instance term_parsable(ctx_modality).

%------------------------------------------------------------------------------%

:- implementation.
:- import_module list, term.

:- instance stringable(ctx_modality) where [
	func(to_string/1) is ctx_modality_to_string
].

:- instance term_parsable(ctx_modality) where [
	func(from_term/1) is ctx_modality_from_term
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

:- func ctx_modality_to_string(ctx_modality) = string.
:- func ctx_modality_from_term(term.term::in) = (ctx_modality::out) is semidet.

	% axiom
ctx_modality_to_string(any) = "[]".
ctx_modality_from_term(functor(atom("[]"), [], _)) = any.

	% info
ctx_modality_to_string(truth) = "i".
ctx_modality_from_term(functor(atom("i"), [], _)) = truth.

	% attention state
ctx_modality_to_string(attention) = "att".
ctx_modality_from_term(functor(atom("att"), [], _)) = attention.

	% events
ctx_modality_to_string(evt) = "event".
ctx_modality_from_term(functor(atom("event"), [], _)) = evt.

	% "knows"
ctx_modality_to_string(belief) = "bel".
ctx_modality_from_term(functor(atom("bel"), [], _)) = belief.

ctx_modality_to_string(intention) = "int".
ctx_modality_from_term(functor(atom("int"), [], _)) = intention.

ctx_modality_to_string(understanding) = "understand".
ctx_modality_from_term(functor(atom("understand"), [], _)) = understanding.

ctx_modality_to_string(generation) = "generate".
ctx_modality_from_term(functor(atom("generate"), [], _)) = generation.
