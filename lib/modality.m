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

:- module modality.

:- interface.

:- import_module list.

:- typeclass modality(T) where [
	func axiom = T,
	func compose(T::in, T::in) = (T::out) is semidet
].

:- pred match(list(T)::in, list(T)::in) is semidet <= modality(T).

:- func compose_list(list(T)) = list(T) <= modality(T).

%------------------------------------------------------------------------------%

:- implementation.

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

	% axiom has S4 semantics:
	%  (K) ... [](B -> A) -> ([]B -> []A)
	%  (T) ... []A -> A
	%  (4) ... []A -> [][]A
	%
	% plus specialisation:
	%  []A -> [a]A
	%    for every a

match([], []).
match([H|TL], [H|TR]) :- match(TL, TR).

match(L, [axiom|TR]) :- match([axiom|TR], L).  % symmetry of the matching relation
match([axiom|TL], R) :- match(TL, R).  % (T)
match([axiom|TL], [_|TR]) :- match([axiom|TL], TR).  % (4) + specialistaion

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

compose_list([]) = [].
compose_list([H]) = [H].

compose_list([H,I|T]) = C :-
	(if compose(H, I) = J
	then C = [J|compose_list(T)]
	else C = [H, I|compose_list(T)]
	).

