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

:- module test_modality.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%------------------------------------------------------------------------------%

:- implementation.

:- import_module list, string.
:- import_module ctx_modality, modality, ctx_io.
:- import_module stringable.

main(!IO) :-
	test_ctx_modality_match([attention], [any], !IO),
	test_ctx_modality_match([any], [attention], !IO),
	test_ctx_modality_match([attention], [attention], !IO),
	test_ctx_modality_match([any], [], !IO),
	test_ctx_modality_match([], [any], !IO),
	test_ctx_modality_match([any], [any], !IO),
	test_ctx_modality_match([any], [any, any], !IO),

	test_ctx_modality_match([attention], [any, attention, any], !IO),
	test_ctx_modality_match([attention], [any, evt], !IO),

	test_ctx_modality_match([], [], !IO).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

:- pred test_ctx_modality_match(list(ctx_modality)::in, list(ctx_modality)::in, io::di, io::uo) is det.

test_ctx_modality_match(L, R, !IO) :-
	test_match(L, R, !IO).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

:- pred test_match(list(T)::in, list(T)::in, io::di, io::uo) is det <= (modality(T), stringable(T)).

test_match(L, R, !IO) :-
	print(seq_to_string(L) ++ " ? " ++ seq_to_string(R) ++ " ... ", !IO),
	(if match(L, R)
	then print("yes", !IO)
	else print("no", !IO)
	),
	nl(!IO).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

:- func seq_to_string(list(T)) = string <= stringable(T).

seq_to_string(L) = "(" ++ string.join_list(", ", list.map(to_string, L)) ++ ")".
