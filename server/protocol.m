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

:- module protocol.

:- interface.
:- import_module list.
:- import_module ctx_modality.

:- type request
	--->	init_ctx
	;	load_file(string)
	;	clear_rules
	;	clear_facts
	;	clear_facts_by_modality(ctx_modality)
	;	clear_assumables
	;	clear_assumable_function(string)
	;	add_fact(string)
	;	add_assumable(string, string, float)
	;	prove(list(string))
	;	get_best_proof
	.

:- pred is_request(request::in) is det.

%------------------------------------------------------------------------------%

:- implementation.

is_request(_).
