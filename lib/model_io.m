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

:- module model_io.

:- interface.

:- import_module model.
:- import_module string.

:- func model_to_string(model(I, S, R)) = string.

%------------------------------------------------------------------------------%

:- implementation.
:- import_module map, set, list, pair.

model_to_string(WM) = Str :-
	map.to_assoc_list(WM^worlds, LWorlds),
	set.to_sorted_list(WM^access, LAccess),
	map.to_assoc_list(WM^props, LProps),
	Str = "({"
		++ string.join_list(",",
				list.map((func(W-Sort) = XStr :-
						XStr = string(W) ++ ":" ++ string(Sort)
					), LWorlds))
		++ "}, {"
		++ string.join_list(",",
				list.map((func({Rel,W1,W2}) = YStr :-
						YStr = string(W1) ++ "<" ++ string(Rel) ++ ">" ++ string(W2)
					), LAccess))
		++ "}, {"
		++ string.join_list(",",
				list.map((func(W-Props) = ZStr :-
						set.to_sorted_list(Props, LLProps),
						ZStr = string(W) ++ "={" ++ string.join_list(",", LLProps) ++ "}"
					), LProps))
		++ "})".
