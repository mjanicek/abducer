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
			tty_print_ctx(stdout_stream, !.Ctx, !IO)
		)
	else
		print("Usage: check-file FILENAME[S...]\n", !IO)
	).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

:- pred check_facts_files(list(string)::in, ctx::in, ctx::out, io::di, io::uo) is det.

check_facts_files([], !Ctx, !IO).
check_facts_files([F|Fs], !Ctx, !IO) :-
	print("[" ++ F ++ "] ", !IO),
	load_file(F, Result, !Ctx, [], Warns, !IO),
	print(string(Result) ++ "\n", !IO),
	print(string.join_list("\n", list.map(warning_to_string, Warns)) ++ "\n", !IO),
	check_facts_files(Fs, !Ctx, !IO).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

:- func warning_to_string(loading.warning) = string.

warning_to_string(singleton_variable(VarName, Line)) =
		"    Warning: singleton variable `" ++ VarName ++ "' on line " ++ string.from_int(Line).
