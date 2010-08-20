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

:- module 'abducer-cli'.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%------------------------------------------------------------------------------%

:- implementation.

:- import_module require, solutions.
:- import_module map, set, list, pair, assoc_list, string, float, int, bag, bool.
:- import_module utils.
:- import_module abduction, formula, context, costs.
:- import_module loading.

:- import_module ctx_modality, ctx_loadable, ctx_io, ctx_loadable_io.
:- import_module modality, stringable.

:- import_module parser, term_io, term, varset, formula_io, formula_ops, costs.

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

main(!IO) :-
	io.command_line_arguments(CmdArgs, !IO),
	(if
		CmdArgs = [Goal]
	then
		some [!Ctx] (
			!:Ctx = new_ctx,

			loading.load_stdin(Result, !Ctx, !IO),
			format("read result: %s\n", [s(string(Result))], !IO),

			vs(InitMProp, InitVarset) = det_string_to_vsmprop(Goal),

			P0 = new_proof(!.Ctx, [unsolved(InitMProp, not_assumable)], InitVarset),
%			P0 = vs([unsolved(InitMProp, const(InitAssumeCost))], InitVarset),

			P0 = proof(XG, _XBL),
			format("goal:\n  %s\n\n", [s(goal_to_string(XG))], !IO),

			print_ctx(!.Ctx, !IO),

			nl(!IO),

%			DC0 = new_d_ctx,

			Proofs0 = set.to_sorted_list(solutions_set((pred((Cost-P)::out) is nondet :-
%				Costs = costs(1.0, 1.0),
%				prove(0.0, InitAssumeCost, P0, P, Costs, !.Ctx),
%				G = last_goal(P),
%				Cost = cost(!.Ctx, P, Costs)

				prove(0.0, 100.0, P0, P, default_costs, !.Ctx),
				Cost = cost(!.Ctx, P, default_costs)
					))),

/*
			% TODO: derivations
			% deriv: map(proved_goal, set(list(steps)))

			list.foldl((pred((Cost-G)-P::in, M0::in, M::out) is det :-
				(if map.search(M0, Cost-G, D0)
				then D1 = D0
				else D1 = set.init
				),
				set.insert(D1, P, D2),
				map.set(M0, Cost-G, D2, M)
					), Proofs0, map.init, DerivsMap),

			list.sort((pred((CA-_)-_::in, (CB-_)-_::in, Comp::out) is det :-
				float_compare(CA, CB, Comp)
					), map.to_assoc_list(DerivsMap), DerivsSorted),

			format("found %d proofs.\n", [i(length(DerivsSorted))], !IO),
*/

			list.sort((pred((CA-_)::in, (CB-_)::in, Comp::out) is det :-
				float_compare(CA, CB, Comp)
					), Proofs0, Proofs),

			format("\n  %d proof(s) found.\n", [i(list.length(Proofs))], !IO),

			list.foldl((pred((Cost-proof(Gz, _BL))::in, !.IO::di, !:IO::uo) is det :-
				print("---------------------------------------------------------------------\n", !IO),
				format("proof cost = %f\n\n", [f(Cost)], !IO),
				print("proven goal:\n  " ++ goal_to_string(Gz) ++ "\n", !IO),
				nl(!IO)
					), Proofs, !IO)

/*
			list.foldl((pred((Cost-G)-Ds::in, !.IO::di, !:IO::uo) is det :-
				print("---------------------------------------------------------------------\n", !IO),
				format("proof cost = %f\n\n", [f(Cost)], !IO),
				print("proven goal:\n  " ++ goal_to_string(G) ++ "\n", !IO),
				nl(!IO),

				print("assumptions:\n", !IO),
				print("  " ++ assumptions_to_string(!.Ctx, goal_assumptions(G)) ++ "\n", !IO),
				nl(!IO),

				print("assertions:\n", !IO),
				print("  " ++ assertions_to_string(!.Ctx, goal_assertions(G)) ++ "\n", !IO),
				nl(!IO),

				print(string.from_int(set.count(Ds)) ++ " derivation" ++ plural_s(count(Ds)) ++ ".\n", !IO),

				print("- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -\n", !IO),

				set.fold((pred(Proof::in, !.IO::di, !:IO::uo) is det :-
					is_ctx_proof(Proof),
					print_proof_trace(!.Ctx, Proof, !IO),
					nl(!IO)
						), Ds, !IO)

					), DerivsSorted, !IO)
*/
		)
	else
		io.progname("?", ProgName, !IO),
		format(stderr_stream, "Usage: %s GOAL < FILE\n", [s(ProgName)], !IO)
	).

%------------------------------------------------------------------------------%

:- func default_costs = costs.

default_costs = costs(0.0, 0.0).

%------------------------------------------------------------------------------%

:- func plural_s(int) = string.

plural_s(N) = S :-
	(if N > 1
	then S = "s"
	else S = ""
	).

%------------------------------------------------------------------------------%

:- pred is_ctx_proof(proof(ctx_modality)::in) is det.

is_ctx_proof(_).
