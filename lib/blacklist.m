%------------------------------------------------------------------------------%
% Copyright (C) 2010-2011 DFKI GmbH Talking Robots 
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

:- module blacklist.

:- interface.

:- import_module set, map.
:- import_module lang.
:- import_module context, modality.

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

:- type blacklist(M)
	--->	bl(
		bl_map :: map(mgatom(M), disjoint_decl(M)),
		used :: set(mgatom(M)),
		forbidden :: set(mgatom(M))
	).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

:- func init(C) = blacklist(M) <= (context(C, M), modality(M)).

:- pred check_mgatom(mgatom(M)::in, blacklist(M)::in, blacklist(M)::out) is semidet
		<= modality(M).

%------------------------------------------------------------------------------%

:- implementation.

:- import_module solutions.

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

init(Ctx) = bl(BLMap, set.init, set.init) :-
	% compile the map
	aggregate((pred(DDSel::out) is nondet :- find_disjoint_decl(Ctx, DDSel)),
			(pred(DD::in, M0::in, M::out) is det :-
			 	set.fold((pred(MGF::in, Mb0::in, Mb::out) is det :-
					(if map.search(Mb0, MGF, Vals0)
					then Vals = Vals0
					else Vals = set.init
					),
					set.delete(DD, MGF, Add),
					set.union(Vals, Add, New),
					map.set(Mb0, MGF, New, Mb)
						), DD, M0, M)
							), map.init, BLMap).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

check_mgatom(MGF, BL0, BL) :-
	not set.member(MGF, BL0^forbidden),
	BL1 = BL0^used := set.insert(BL0^used, MGF),
	(if map.search(BL1^bl_map, MGF, NewForbidden)
	then 
		BL = BL1^forbidden := set.union(BL1^forbidden, NewForbidden),
		set.empty(set.intersect(BL^used, BL^forbidden))
	else
		BL = BL1
	).

%	(if map.search(BL0^bl_map, MGF, NewForbidden)
%	then 
%		set.empty(set.intersect(BL0^used, NewForbidden)),
%		BL1 = BL0^forbidden := set.union(BL0^forbidden, NewForbidden)
%	else
%		BL1 = BL0
%	),
%	BL = BL0^used := set.insert(BL0^used, MGF).
