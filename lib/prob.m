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

:- module prob.

:- interface.

:- import_module abduction.

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

:- func probabilistic_costs = structure_costs.

:- func to_cost(float) = float.
:- func from_cost(float) = float.

:- func bound_subtract(float::in) `with_type` bound_transform `with_inst` bound_transform.
:- func bound_divide(float::in) `with_type` bound_transform `with_inst` bound_transform.

%------------------------------------------------------------------------------%

:- implementation.

:- import_module float.
:- import_module math.

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

probabilistic_costs = structure_costs(0.0, 0.0).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

to_cost(P) = -math.ln(P).
from_cost(Cost) = math.exp(-Cost).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

bound_subtract(Dec, Bound) = NewBound :-
	P = math.exp(-Bound) - Dec,
	P =< 1.0, P >= 0.0,
	NewBound = -math.ln(P).

bound_divide(Arg, Bound) = Bound + math.ln(Arg).
