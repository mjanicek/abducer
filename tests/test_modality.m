:- module test_modality.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%------------------------------------------------------------------------------%

:- implementation.

:- import_module list, string.
:- import_module ctx_modality, modality, ctx_io, belief_model, stf.
:- import_module stringable.

main(!IO) :-
	test_ctx_modality_match([att], [any], !IO),
	test_ctx_modality_match([any], [att], !IO),
	test_ctx_modality_match([att], [att], !IO),
	test_ctx_modality_match([any], [], !IO),
	test_ctx_modality_match([], [any], !IO),
	test_ctx_modality_match([any], [any], !IO),
	test_ctx_modality_match([any], [any, any], !IO),

	test_ctx_modality_match([att], [any, att, any], !IO),
	test_ctx_modality_match([att], [any, evt], !IO),

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
