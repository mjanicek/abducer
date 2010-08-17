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

