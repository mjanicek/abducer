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

:- module utils.

:- interface.

:- import_module map, list.
:- import_module bool.
:- import_module io.

:- pred map_merge_op(pred(V, V, V), map(K, V), map(K, V), map(K, V)).
:- mode map_merge_op(pred(in, in, out) is det, in, in, out) is det.
:- mode map_merge_op(pred(in, in, out) is semidet, in, in, out) is semidet.

:- pred do_while(pred(bool, A, A, T, T), A, A, T, T).
:- mode do_while((pred(out, in, out, di, uo) is det), in, out, di, uo) is det.
:- mode do_while((pred(out, in, out, in, out) is det), in, out, in, out) is det.

:- pred do_while_result(pred(bool, RT, A, A, T, T), RT, A, A, T, T).
:- mode do_while_result((pred(out, out, in, out, di, uo) is det), out, in, out, di, uo) is det.
:- mode do_while_result((pred(out, out, in, out, in, out) is det), out, in, out, in, out) is det.

:- pred float_compare(float::in, float::in, comparison_result::out) is det.

:- pred read_file_as_lines(string::in, list(string)::out, io::di, io::uo) is det.

:- pred strip_ignore_comments(list(string)::in, list(string)::out) is det.

%------------------------------------------------------------------------------%

:- implementation.

:- import_module pair.
:- import_module string, float.

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

map_merge_op(Pred, M1, M2, M) :-
	map.to_sorted_assoc_list(M1, L1),
	map.to_sorted_assoc_list(M2, L2),
	assoc_lists_merge_op(Pred, L1, L2, L),
	M = map.from_assoc_list(L).

:- pred assoc_lists_merge_op(pred(V, V, V), list(pair(K, V)), list(pair(K, V)), list(pair(K, V))).
:- mode assoc_lists_merge_op(pred(in, in, out) is det, in, in, out) is det.
:- mode assoc_lists_merge_op(pred(in, in, out) is semidet, in, in, out) is semidet.

assoc_lists_merge_op(_, [], [], []).
assoc_lists_merge_op(_, [H|T], [], [H|T]).
assoc_lists_merge_op(_, [], [H|T], [H|T]).
assoc_lists_merge_op(MergePred, [K1-V1|T1], [K2-V2|T2], [K-V|T]) :-
	compare(Comp, K1, K2),
	(
		Comp = (=),
		K = K1,
		call(MergePred, V1, V2, V),
		assoc_lists_merge_op(MergePred, T1, T2, T)
	;
		Comp = (<),
		K = K1, V = V1,
		assoc_lists_merge_op(MergePred, T1, [K2-V2|T2], T)
	;
		Comp = (>),
		K = K2, V = V2,
		assoc_lists_merge_op(MergePred, [K1-V1|T1], T2, T)
	).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

do_while(Pred, A0, A, B0, B) :-
	call(Pred, Result, A0, A1, B0, B1),
	(
		Result = yes,
		do_while(Pred, A1, A, B1, B)
	;
		Result = no,
		A = A1, B = B1
	).

do_while_result(Pred, R, A0, A, B0, B) :-
	call(Pred, Result, LoopR, A0, A1, B0, B1),
	(
		Result = yes,
		do_while_result(Pred, R, A1, A, B1, B)
	;
		Result = no,
		R = LoopR,
		A = A1, B = B1
	).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

float_compare(A, B, R) :-
	(if A < B
	then R = (<)
	else
		(if A > B
		then R = (>)
		else R = (=)
		)
	).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

read_file_as_lines(FileName, Lines, !IO) :-
	see(FileName, SeeResult, !IO),
	(
		SeeResult = ok,
		read_file_as_string(ReadResult, !IO),
		(
			ReadResult = ok(S),
			Lines = string.words_separator((pred(C::in) is semidet :- C = '\n'), S)
		;
			ReadResult = error(_, _),
			Lines = []
		),
		seen(!IO)
	;
		SeeResult = error(_),
		Lines = []
	).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

strip_ignore_comments(LIn, LOut) :-
	list.filter_map((pred(L0::in, L::out) is semidet :-
		L1 = string.strip(L0),
		L = L1,
		not string.first_char(L, '#', _)
			), LIn, LOut).
