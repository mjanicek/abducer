%------------------------------------------------------------------------------%
% Copyright (C) 2009-2010 DFKI GmbH Talking Robots 
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
