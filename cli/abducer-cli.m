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

:- module 'abducer-cli'.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%------------------------------------------------------------------------------%

:- implementation.

:- import_module require, solutions.
:- import_module map, set, list, pair, assoc_list, string, float, int, bag, bool.
:- import_module utils.
:- import_module abduction, lang, context, assumability.
:- import_module loading.
:- import_module prob.
:- import_module anytime.

:- import_module ctx_modality, ctx_loadable, ctx_io, ctx_loadable_io.
:- import_module modality, stringable.

:- import_module parser, term_io, term, varset, lang_io, lang_ops.

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

:- pragma promise_pure(main/2).

main(!IO) :-
	impure reset_signaller,
	io.command_line_arguments(CmdArgs, !IO),
	(if
		CmdArgs = [MethodStr, Goal],
		( MethodStr = "dfs"
		; MethodStr = "bdfs"
		; MethodStr = "iddfs"
		)
	then
		some [!Ctx] (
			!:Ctx = new_ctx,

			loading.load_stdin(Result, !Ctx, no, [], _Warns, !IO),
			format("read result: %s\n", [s(string(Result))], !IO),

			vs(InitMProp, InitVarset) = det_string_to_vsmatom(Goal),

			P0 = new_proof(!.Ctx, [unsolved(InitMProp, not_assumable)], InitVarset),
			P0 = proof(InitGoal, _InitBL),
			format("goal:\n  %s\n\n", [s(goal_to_string(InitGoal))], !IO),

			(
				MethodStr = "dfs",
				Method = unbounded_dfs
			;
				MethodStr = "bdfs",
				Method = bounded_dfs(prob.to_cost(0.2))
			;
				MethodStr = "iddfs",
				Method = iddfs(prob.to_cost(0.5), multiply_cost(2.0))
%				Method = iddfs(prob.to_cost(0.5), prob.bound_subtract(0.1))
			),

			prove(Method, P0, Ps, probabilistic_costs, !.Ctx),
			Proofs0 = list.map((func(P) = Cost-P :- Cost = proof_cost(!.Ctx, P, probabilistic_costs)), set.to_sorted_list(Ps)),

			list.sort((pred((CA-_)::in, (CB-_)::in, Comp::out) is det :-
				float_compare(CA, CB, Comp)
					), Proofs0, Proofs),

			format("\n  %d proof(s) found.\n", [i(list.length(Proofs))], !IO),

			list.foldl2((pred((Cost-proof(Gz, _BL))::in, Idx0::in, Idx::out, !.IO::di, !:IO::uo) is det :-
				print("---------------------------------------------------------------------\n", !IO),
				format("#%d, cost = %f (p=%f)\n\n", [i(Idx0), f(Cost), f(prob.from_cost(Cost))], !IO),
				print("proven goal:\n  " ++ goal_to_string(Gz) ++ "\n", !IO),
				nl(!IO),
				Idx = Idx0 + 1
					), Proofs, 1, _, !IO)
		)
	else
		io.progname("?", ProgName, !IO),
		format(stderr_stream, "Usage: %s METHOD GOAL < FILE\n", [s(ProgName)], !IO)
	).

%------------------------------------------------------------------------------%

:- pred is_ctx_proof(proof(ctx_modality)::in) is det.

is_ctx_proof(_).
