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

:- module loading.
:- interface.

:- import_module list.
:- import_module io.
:- import_module ctx_loadable.

:- type load_result
	--->	ok
	;	file_read_error
	;	syntax_error(string, int)
	.

:- type warning
	--->	singleton_variable(string, int)
	.

:- pred load_stdin(load_result::out, ctx::in, ctx::out, list(warning)::in, list(warning)::out, io::di, io::uo) is det.
:- pred load_file(string::in, load_result::out, ctx::in, ctx::out, list(warning)::in, list(warning)::out, io::di, io::uo) is det.

%------------------------------------------------------------------------------%

:- implementation.
:- import_module bool.
:- import_module pair.
:- import_module utils.
:- import_module term, varset.
:- import_module lang, lang_io, lang_ops.
:- import_module context.
:- import_module term_io.
:- import_module ctx_modality, ctx_io.
:- import_module check.

load_stdin(Result, MainCtx0, MainCtx, MainWs0, MainWs, !IO) :-

	do_while_result((pred(Continue::out, LoopResult::out, Ctx0-Ws0::in, Ctx-Ws::out, !.IO::di, !:IO::uo) is det :-
		term_io.read_term_with_op_table(init_wabd_op_table, ReadResult, !IO),
		(
			ReadResult = term(VS, Term),
			generic_term(Term),
			context(_, Line) = get_term_context(Term),

			(if term_to_assumable_function_def(Term, FunctionName, FunctionValues)
			then
				set_assumability_function(FunctionName, FunctionValues, Ctx0, Ctx),
				Ws = Ws0,
				LoopResult = ok,
				Continue = yes
			else
				(if term_to_mrule(Term, MRule)
				then
					add_rule(vs(MRule, VS), Ctx0, Ctx),
					find_singleton_vars(vs(MRule, VS), Singletons),
					Ws = Ws0 ++ list.map((func(Var) = singleton_variable(varset.lookup_name(VS, Var), Line)), Singletons),
					LoopResult = ok,
					Continue = yes
				else
					(if term_to_disjoint_decl(Term, DD)
					then
						add_disjoint_decl(DD, Ctx0, Ctx),
						Ws = Ws0,
						LoopResult = ok,
						Continue = yes
					else
						(if term_to_matom(Term, MAtom)
						then
							add_fact(vs(MAtom, VS), Ctx0, Ctx),
							Ws = Ws0,
							LoopResult = ok,
							Continue = yes
						else
							Ctx = Ctx0,
							Ws = Ws0,
							LoopResult = syntax_error("Unable to convert term to rule or fact", Line),
							Continue = no
						)
					)
				)
			)
		;
			ReadResult = error(Message, Linenumber),
			LoopResult = syntax_error(Message, Linenumber),
			Ctx = Ctx0,
			Ws = Ws0,
			Continue = no
		;
			ReadResult = eof,
			Ctx = Ctx0,
			Ws = Ws0,
			LoopResult = ok,
			Continue = no
		)
			), Result, MainCtx0-MainWs0, MainCtx-MainWs, !IO).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

load_file(Filename, Result, !Ctx, !Warn, !IO) :-
	see(Filename, SeeRes, !IO),
	(if
		SeeRes = ok
	then
		load_stdin(Result, !Ctx, !Warn, !IO),
		seen(!IO)
	else
		Result = file_read_error
	).

