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

:- module 'check-abd'.

:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

%------------------------------------------------------------------------------%

:- implementation.
:- import_module require.
:- import_module string, bool.
:- import_module list, set.
:- import_module term, term_io.
:- import_module utils.
:- import_module stringable.
:- import_module ctx_loadable, ctx_loadable_io, ctx_io.
:- import_module loading.

main(!IO) :-
	command_line_arguments(CmdLineArgs, !IO),

	(if CmdLineArgs = ["--ctx"|Files0]
	then
		PrintCtx = yes,
		Files = Files0
	else
		PrintCtx = no,
		Files = CmdLineArgs
	),

	(if Files = [F|Fs]
	then
		some [!Ctx] (
			!:Ctx = new_ctx,
			check_facts_files([F|Fs], !Ctx, !IO),

			(if PrintCtx = yes
			then
				nl(!IO),
				tty_print_ctx(stdout_stream, !.Ctx, !IO)
			else
				true
			)
		)
	else
		print("Usage: check-abd [--ctx] FILENAME[S...]\n", !IO)
	).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

:- pred check_facts_files(list(string)::in, ctx::in, ctx::out, io::di, io::uo) is det.

check_facts_files([], !Ctx, !IO).
check_facts_files([F|Fs], !Ctx, !IO) :-
	print("[" ++ F ++ "] ", !IO),
	load_file(F, Result, !Ctx, yes, [], Warns, !IO),
	print(string(Result) ++ "\n", !IO),
	(if Warns = []
	then
		true
	else
		print(string.join_list("\n", list.map(warning_to_string, Warns)) ++ "\n", !IO)
	),
	check_facts_files(Fs, !Ctx, !IO).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

:- func warning_to_string(loading.warning) = string.

warning_to_string(singleton_variable(VarName, Line)) =
		"    line " ++ string.from_int(Line) ++ ": "
		++ "variable " ++ pretty_escape(VarName) ++ " used only once".

warning_to_string(dangerous_names(Vars, Line)) =
		"    line " ++ string.from_int(Line) ++ ": "
		++ "potentially confusing variables " ++ VarS :-
	VarS = pretty_conjunction(list.map(pretty_escape, set.to_sorted_list(Vars))).

warning_to_string(multiply_used_anon(VarName, Line)) =
		"    line " ++ string.from_int(Line) ++ ": "
		++ "anonymised variable " ++ pretty_escape(VarName) ++ " used more than once".

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

:- func pretty_escape(string) = string.

pretty_escape(S) = "`" ++ S ++ "'".

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

:- func pretty_conjunction(list(string)) = string.

pretty_conjunction([]) = "".
pretty_conjunction([S]) = S.
pretty_conjunction([S1, S2]) = S1 ++ " and " ++ S2.
pretty_conjunction([S1, S2, S3|T]) = S1 ++ ", " ++ pretty_conjunction([S2, S3|T]).
