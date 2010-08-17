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
