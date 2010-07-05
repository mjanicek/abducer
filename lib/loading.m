:- module loading.
:- interface.

:- import_module io.
:- import_module ctx_loadable.

:- type load_result
	--->	ok
	;	file_read_error
	;	syntax_error(string, int)
	.

:- pred load_stdin(load_result::out, ctx::in, ctx::out, io::di, io::uo) is det.
:- pred load_file(string::in, load_result::out, ctx::in, ctx::out, io::di, io::uo) is det.

%------------------------------------------------------------------------------%

:- implementation.
:- import_module bool.
:- import_module utils.
:- import_module term, varset.
:- import_module formula, formula_io, formula_ops.
:- import_module term_io.
:- import_module ctx_modality, ctx_io.
:- import_module stringable.

load_stdin(Result, !Ctx, !IO) :-
	do_while_result((pred(Continue::out, LoopResult::out, !.Ctx::in, !:Ctx::out, !.IO::di, !:IO::uo) is det :-
		term_io.read_term_with_op_table(init_wabd_op_table, ReadResult, !IO),
		(
			ReadResult = term(VS, Term),
			generic_term(Term),
			(if term_to_mrule(Term, MRule)
			then
				add_rule(vs(MRule, VS), !Ctx),
				LoopResult = ok,
				Continue = yes
			else
				(if term_to_mprop(Term, MProp)
				then
					add_fact(vs(MProp, VS), !Ctx),
					LoopResult = ok,
					Continue = yes
				else
					context(_, Line) = get_term_context(Term),
					LoopResult = syntax_error("Unable to convert term to rule or fact", Line),
					Continue = no
				)
			)
		;
			ReadResult = error(Message, Linenumber),
			LoopResult = syntax_error(Message, Linenumber),
			Continue = no
		;
			ReadResult = eof,
			LoopResult = ok,
			Continue = no
		)
			), Result, !Ctx, !IO).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

load_file(Filename, Result, !Ctx, !IO) :-
	see(Filename, SeeRes, !IO),
	(if
		SeeRes = ok
	then
		load_stdin(Result, !Ctx, !IO),
		seen(!IO)
	else
		Result = file_read_error
	).

