:- module 'check-file'.

:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

%------------------------------------------------------------------------------%

:- implementation.
:- import_module require.
:- import_module list, string, bool.
:- import_module term, term_io.
:- import_module utils.
:- import_module stringable.
:- import_module formula, formula_io, formula_ops, modality.
:- import_module ctx_loadable, ctx_loadable_io, ctx_io.
:- import_module loading.

main(!IO) :-
	command_line_arguments(CmdLineArgs, !IO),
	(if
		CmdLineArgs = [F|Fs]
	then
		some [!Ctx] (
			!:Ctx = new_ctx,
			check_facts_files([F|Fs], !Ctx, !IO),
			nl(!IO),
			print("Facts:\n", !IO),
			print_facts(!.Ctx, "  ", !IO),
			nl(!IO),
			print("Rules:\n", !IO),
			print_rules(!.Ctx, "  ", !IO)
		)
	else
		print("Usage: check-facts-file FILENAME[S...]\n", !IO)
	).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

:- pred check_facts_files(list(string)::in, ctx::in, ctx::out, io::di, io::uo) is det.

check_facts_files([], !Ctx, !IO).
check_facts_files([F|Fs], !Ctx, !IO) :-
	print("[" ++ F ++ "] ", !IO),
	load_file(F, Result, !Ctx, !IO),
	print(Result, !IO),
	nl(!IO),
	check_facts_files(Fs, !Ctx, !IO).
