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

:- module 'abducer-engine-pb'.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%------------------------------------------------------------------------------%

:- implementation.

:- import_module require, solutions, exception.
:- import_module map, set, list, pair, assoc_list, string, float, bitmap, int, bag, bool, maybe.
:- import_module utils.
:- import_module abduction, lang, context, assumability.
:- import_module loading.
:- import_module prob.
:- import_module anytime.

:- import_module context, ctx_modality, ctx_loadable, ctx_io, ctx_loadable_io.
:- import_module modality, stringable.

:- import_module parser, term_io, term, varset, lang_io, lang_ops.
:- import_module exception.
:- import_module protocol, protocol_loading.
:- import_module protobuf_runtime, stream.
:- import_module unix_socket.

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

:- type settings
	--->	settings(
		silent :: bool
	).

:- type srv_ctx
	--->	srv_ctx(
		settings :: settings,
		cx :: ctx
	).

main(!IO) :-
	io.command_line_arguments(AllArgs, !IO),

	(if AllArgs = ["--silent" | RestArgs0]
	then
		Silent = yes,
		RestArgs = RestArgs0
	else
		Silent = no,
		RestArgs = AllArgs
	),

	(if
		RestArgs = [SocketPath]
	then
		unix_socket.connect(SocketPath, ConnectResult, !IO),
		(
			ConnectResult = ok(UnSock),
			inner_loop(UnSock, UnSock, srv_ctx(settings(Silent), new_ctx), _, !IO)
		;
			ConnectResult = error(E),
			format(stderr_stream, "connection error: %s\n", [s(string(E))], !IO)
		)
	else
		io.progname("?", ProgName, !IO),
		format(stderr_stream, "Usage: %s [--silent] SOCKET_PATH\n", [s(ProgName)], !IO)
	).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

	% returns the bytes in reverse order
:- pred read_n_bytes(S::in, int::in, stream.result(list(byte), pb_read_error(E))::out, IO::di, IO::uo) is det
		<= ( stream.reader(S, byte, IO, E) ).

read_n_bytes(Stream, N, Result, !IO) :-
	(if N =< 0
	then Result = ok([])
	else
		get(Stream, ByteRes, !IO),
		(
			ByteRes = ok(Byte),
			read_n_bytes(Stream, N - 1, Result0, !IO),
			(
				Result0 = ok(Bytes0),
				Result = ok([Byte | Bytes0])
			;
				Result0 = error(Err),
				Result = error(Err)
			;
				Result0 = eof,
				Result = error(premature_eof(0))  % XXX
			)
		;
			ByteRes = error(Err),
			Result = error(stream_error(Err))
		;
			ByteRes = eof,
			Result = error(premature_eof(0))  % XXX
		)
	).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

	% assumes little-endianness
:- pred read_uint32(S::in, maybe(int)::out, IO::di, IO::uo) is det
		<= stream.reader(S, byte, IO, E).

read_uint32(Stream, MayInt, !IO) :-
	read_n_bytes(Stream, 4, ReadResult, !IO),
	(
		ReadResult = ok(Bytes),
		MayInt = yes(list.foldr((func(Byte, I) = I << 8 \/ (Byte /\ 0xff)), Bytes, 0))
	;
		ReadResult = error(_),
		MayInt = no
	;
		ReadResult = eof,
		MayInt = no
	).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

:- pred write_uint32(S::in, int::in, IO::di, IO::uo) is det
		<= stream.writer(S, byte, IO).

write_uint32(Stream, I, !IO) :-
	put(Stream, I /\ 0xff, !IO),
	put(Stream, (I >> 8) /\ 0xff, !IO),
	put(Stream, (I >> 16) /\ 0xff, !IO),
	put(Stream, (I >> 24) /\ 0xff, !IO).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

:- pred read_pb_message(S::in, maybe(Request)::out, io::di, io::uo) is det
		<= (stream.reader(S, byte, io, E), pb_message(Request)).

read_pb_message(Stream, MayRequest, !IO) :-
	read_uint32(Stream, MayMsgSize, !IO),
	trace[compile_time(flag("debug")), io(!IO)] ( print(stderr_stream, string(MayMsgSize) ++ "\n", !IO) ),
	(
		MayMsgSize = yes(MsgSize),
		get(pb_reader(Stream, MsgSize), GetRes, !IO),
		(
			GetRes = ok(pb_message(Request)),
			MayRequest = yes(Request),
			trace[compile_time(flag("debug")), io(!IO)] ( print(stderr_stream, string(Request) ++ "\n", !IO) )
		;
			GetRes = error(E),
			MayRequest = no,
			trace[compile_time(flag("debug")), io(!IO)] ( print(stderr_stream, string(E) ++ "\n", !IO) )
%			throw(GetRes)
		;
			GetRes = eof,
			trace[compile_time(flag("debug")), io(!IO)] ( print(stderr_stream, "eof" ++ "\n", !IO) ),
			MayRequest = no
		)
	;
		MayMsgSize = no,
		trace[compile_time(flag("debug")), io(!IO)] ( print(stderr_stream, "failed to read the message length" ++ "\n", !IO) ),
		MayRequest = no
	).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

:- pred write_pb_message(S::in, M::in, io::di, io::uo) is det
		<= (stream.writer(S, byte, io), pb_message(M)).

write_pb_message(Stream, Msg, !IO) :-
	write_uint32(Stream, message_size(Msg), !IO),
%	trace[io(!IO)] ( print(stderr_stream, string(message_size(Msg)) ++ " bytes for " ++ string(Msg) ++ "\n", !IO) ),
	put(pb_writer(Stream), pb_message(Msg), !IO).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

:- pred inner_loop(SIn::in, SOut::in, srv_ctx::in, srv_ctx::out, io::di, io::uo) is det
		<= (stream.reader(SIn, byte, io, E), stream.writer(SOut, byte, io)).

inner_loop(In, Out, !SCtx, !IO) :-
	read_pb_message(In, MayRequest, !IO),
	(if MayRequest = yes(Request)
	then
		process_request(Request, Cont, In, Out, !SCtx, !IO),
		(if Cont = yes
		then inner_loop(In, Out, !SCtx, !IO)
		else true
		)
	else
		% FIXME there is no guarantee that the client is listening for a RequestReply
		% at this point!
		write_pb_message(Out, request_reply(request_reply_return_code_protocolerror,
				yes("failed to read the request")), !IO)
	).

%------------------------------------------------------------------------------%

:- pred process_request(protocol.request::in, bool::out, SIn::in, SOut::in, srv_ctx::in, srv_ctx::out, io::di, io::uo) is det
		<= (stream.reader(SIn, byte, io, E), stream.writer(SOut, byte, io)).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

process_request(request(request_code_clearcontext), yes, _In, Out, SCtxOld, SCtxNew, !IO) :-
	SCtxNew = srv_ctx(SCtxOld^settings, new_ctx),
	write_pb_message(Out, request_reply(request_reply_return_code_ok, no), !IO).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

process_request(request(request_code_loadfile), Cont, In, Out, !SCtx, !IO) :-
	read_pb_message(In, MayArg, !IO),
	(if MayArg = yes(load_file(Filename))
	then
		loading.load_file(Filename, Result, !.SCtx^cx, NewCtx, no, [], _Warns, !IO),
		!:SCtx = !.SCtx^cx := NewCtx,
		write_pb_message(Out, load_result_to_proto(ok(Result)), !IO),
		Cont = yes
	else
		write_pb_message(Out, load_result_to_proto(error("failed to read argument in loadFile")), !IO),
		Cont = no
	).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

process_request(request(request_code_clearrules), yes, _In, Out, !SCtx, !IO) :-
	Ctx0 = !.SCtx^cx,
	Ctx = Ctx0^rules := set.init,
	!:SCtx = !.SCtx^cx := Ctx,
	write_pb_message(Out, request_reply(request_reply_return_code_ok, no), !IO).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

process_request(request(request_code_clearfacts), yes, _In, Out, !SCtx, !IO) :-
	Ctx0 = !.SCtx^cx,
	Ctx = Ctx0^facts := set.init,
	!:SCtx = !.SCtx^cx := Ctx,
	write_pb_message(Out, request_reply(request_reply_return_code_ok, no), !IO).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

process_request(request(request_code_clearfactsbymodality), Cont, In, Out, !SCtx, !IO) :-
	read_pb_message(In, MayArg, !IO),
	(if
		MayArg = yes(clear_facts_by_modality(PM)),
		proto_modality(PM, M)
	then
		Ctx0 = !.SCtx^cx,
		Ctx = Ctx0^facts := set.filter((pred(vs(m(Ms, _), _)::in) is semidet :-
			Ms \= [M|_]
				), !.SCtx^cx^facts),
		!:SCtx = !.SCtx^cx := Ctx,
		write_pb_message(Out, request_reply(request_reply_return_code_ok, no), !IO),
		Cont = yes
	else
		write_pb_message(Out, request_reply(request_reply_return_code_protocolerror,
				yes("failed to read argument in clearFactsByModality")), !IO),
		Cont = no
	).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

process_request(request(request_code_clearassumables), yes, _In, Out, !SCtx, !IO) :-
	Ctx0 = !.SCtx^cx,
	Ctx = Ctx0^assumables := map.init,
	!:SCtx = !.SCtx^cx := Ctx,
	write_pb_message(Out, request_reply(request_reply_return_code_ok, no), !IO).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

process_request(request(request_code_clearassumabilityfunction), Cont, In, Out, !SCtx, !IO) :-
	read_pb_message(In, MayArg, !IO),
	(if
		MayArg = yes(clear_assumability_function(FunctionName))
	then
		set_assumability_function(FunctionName, map.init, !.SCtx^cx, NewCtx),
		!:SCtx = !.SCtx^cx := NewCtx,
		write_pb_message(Out, request_reply(request_reply_return_code_ok, no), !IO),
		Cont = yes
	else
		write_pb_message(Out, request_reply(request_reply_return_code_protocolerror,
				yes("failed to read argument in clearAssumabilityFunction")), !IO),
		Cont = no
	).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

process_request(request(request_code_cleardisjointdecls), yes, _In, Out, !SCtx, !IO) :-
	Ctx0 = !.SCtx^cx,
	Ctx = Ctx0^disjoint_decls := set.init,
	!:SCtx = !.SCtx^cx := Ctx,
	write_pb_message(Out, request_reply(request_reply_return_code_ok, no), !IO).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

process_request(request(request_code_addrule), Cont, In, Out, !SCtx, !IO) :-
	read_pb_message(In, MayArg, !IO),
	(if
		MayArg = yes(add_rule(ProtoRule)),
		mrule_from_protocol(ProtoRule, Rule, varset.init, VS)
	then
		add_rule(vs(Rule, VS), !.SCtx^cx, NewCtx),
		!:SCtx = !.SCtx^cx := NewCtx,
		write_pb_message(Out, request_reply(request_reply_return_code_ok, no), !IO),
		Cont = yes
	else
		write_pb_message(Out, request_reply(request_reply_return_code_protocolerror,
				yes("failed to read argument in addFact")), !IO),
		Cont = no
	).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

process_request(request(request_code_addfact), Cont, In, Out, !SCtx, !IO) :-
	read_pb_message(In, MayArg, !IO),
	(if
		MayArg = yes(add_fact(ProtoFact)),
		matom_from_protocol(ProtoFact, Fact, varset.init, VS)
	then
		add_fact(vs(Fact, VS), !.SCtx^cx, NewCtx),
		!:SCtx = !.SCtx^cx := NewCtx,
		write_pb_message(Out, request_reply(request_reply_return_code_ok, no), !IO),
		Cont = yes
	else
		write_pb_message(Out, request_reply(request_reply_return_code_protocolerror,
				yes("failed to read argument in addFact")), !IO),
		Cont = no
	).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

process_request(request(request_code_addassumable), Cont, In, Out, !SCtx, !IO) :-
	read_pb_message(In, MayArg, !IO),
	(if
		MayArg = yes(add_assumable(Function, ProtoMAtom, Cost)),
		matom_from_protocol(ProtoMAtom, MAtom, varset.init, _VS)
	then
		(if
			MGAtom = matom_to_ground_matom(MAtom)
		then
			Ass = !.SCtx^cx^assumables,
			(if map.search(Ass, Function, Mapping0)
			then Mapping = Mapping0
			else Mapping = map.init
			),
			map.set(Mapping, MGAtom, Cost, MappingNew),
			map.set(Ass, Function, MappingNew, Ass1),
			Ctx0 = !.SCtx^cx,
			Ctx = Ctx0^assumables := Ass1,
			!:SCtx = !.SCtx^cx := Ctx,
			write_pb_message(Out, request_reply(request_reply_return_code_ok, no), !IO),
			Cont = yes
		else
			write_pb_message(Out, request_reply(request_reply_return_code_protocolerror,
					yes("argument not ground in addAssumable ")), !IO),
			Cont = yes
		)
	else
		write_pb_message(Out, request_reply(request_reply_return_code_protocolerror,
				yes("failed to read argument in addAssumable")), !IO),
		Cont = no
	).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

process_request(request(request_code_adddisjointdecl), Cont, In, Out, !SCtx, !IO) :-
	read_pb_message(In, MayArg, !IO),
	(if
		MayArg = yes(add_disjoint_decl(PDs)),
		list.map_foldl(matom_from_protocol, PDs, Ds, varset.init, _VS)
	then
		(if
			list.map((pred(MA::in, GMA::out) is semidet :-
				GMA = matom_to_ground_matom(MA)
					), Ds, GroundDs)
		then
			DD = set.from_list(GroundDs),
			Ctx0 = !.SCtx^cx,
			add_disjoint_decl(DD, Ctx0, Ctx),
			!:SCtx = !.SCtx^cx := Ctx,
			write_pb_message(Out, request_reply(request_reply_return_code_ok, no), !IO),
			Cont = yes
		else
			write_pb_message(Out, request_reply(request_reply_return_code_protocolerror,
					yes("argument not ground in addDisjointDeclaration")), !IO),
			Cont = yes
		)
	else
		write_pb_message(Out, request_reply(request_reply_return_code_protocolerror,
				yes("failed to read argument in addDisjointDeclaration")), !IO),
		Cont = no
	).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

process_request(request(request_code_prove), Cont, In, Out, !SCtx, !IO) :-
	read_pb_message(In, MayArg, !IO),
	(if
		MayArg = yes(protocol.prove(PQueries, PMethod)),
		list.map_foldl(query_from_protocol, PQueries, Queries, varset.init, VS),
		proof_search_method_from_protocol(PMethod, Method)
	then
		do_prove(Out, Method, Queries, VS, CostProofs, !SCtx, !IO),
		write_pb_message(Out, prove_reply(prove_reply_return_code_ok, list.map(cost_goal_to_proto, CostProofs)), !IO),
		Cont = yes
	else
		write_pb_message(Out, request_reply(request_reply_return_code_protocolerror,
				yes("failed to read argument in prove")), !IO),
		Cont = no
	).

%------------------------------------------------------------------------------%

:- pragma promise_pure(do_prove/9).

:- pred do_prove(SOut::in, abduction.proof_search_method::in(proof_search_method_inst),
		list(query(ctx_modality))::in, varset::in, list(pair(float, goal(ctx_modality)))::out,
		srv_ctx::in, srv_ctx::out, io::di, io::uo) is det
		<= stream.writer(SOut, byte, io).

do_prove(Out, Method, Qs, VS, CostProofs, !SCtx, !IO) :-
	impure reset_signaller,

	(if !.SCtx^settings^silent = no
	then
		print(stderr_stream, "Will start proving the following goal:\n  " ++ goal_to_string(vs(Qs, VS)) ++ "\n", !IO),
		print(stderr_stream, "Proof search method: " ++ string(Method) ++ "\n", !IO)
	else
		true
	),

	P0 = new_proof(!.SCtx^cx, Qs, VS),
	is_ctx_proof(P0),

%	print_ctx(stderr_stream, !.SCtx^cx, !IO),
	write_pb_message(Out, request_reply(request_reply_return_code_ok, no), !IO),

	promise_only_solution_io((pred(Result::out, !.IO::di, !:IO::uo) is cc_multi :-
		try_io((pred(PsX::out, !.IO::di, !:IO::uo) is det :-
			prove(Method, P0, PsX, probabilistic_costs, !.SCtx^cx)
		), Result, !IO)), ProveResult, !IO),

	(
		ProveResult = succeeded(Ps0),
		Ps = Ps0
	;
		ProveResult = failed,
		Ps = set.init
	;
		ProveResult = exception(Ex),
		print(stderr_stream, "Exception: " ++ string(Ex) ++ "\n", !IO),
		% FIXME: notify the caller about this!
		Ps = set.init
	),

	(if !.SCtx^settings^silent = no
	then print(stderr_stream, "Done with proving.\n", !IO)
	else true
	),

	Proofs0 = list.map((func(P) = Cost-P :- Cost = proof_cost(!.SCtx^cx, P, probabilistic_costs)), set.to_sorted_list(Ps)),

	list.sort((pred((CA-_)::in, (CB-_)::in, Comp::out) is det :-
		float_compare(CA, CB, Comp)
			), Proofs0, Proofs),

	(if !.SCtx^settings^silent = no
	then
		format(stderr_stream, "\n  %d proof(s) found.\n", [i(list.length(Proofs))], !IO),

		list.foldl2((pred((Cost-proof(Gz, _BL))::in, Idx0::in, Idx::out, !.IO::di, !:IO::uo) is det :-
			print(stderr_stream, "---------------------------------------------------------------------\n", !IO),
			format(stderr_stream, "#%d, cost = %f\n\n", [i(Idx0), f(Cost)], !IO),
			print(stderr_stream, "  " ++ goal_to_string(Gz) ++ "\n", !IO),
			Idx = Idx0 + 1
				), Proofs, 1, _, !IO),
		print(stderr_stream, "---------------------------------------------------------------------\n", !IO)
	else
		true
	),

	CostProofs = list.map((func(Cost-proof(G, _)) = Cost-G), Proofs).

%------------------------------------------------------------------------------%

:- type load_result_proto  % TODO: generalise
	--->	ok(loading.load_result)
	;	error(string)
	.

:- func load_result_to_proto(load_result_proto) = load_file_reply.

load_result_to_proto(ok(ok)) = load_file_reply(load_file_reply_return_code_ok, no, no).
load_result_to_proto(ok(file_read_error)) = load_file_reply(load_file_reply_return_code_ioerror, no, no).
load_result_to_proto(ok(syntax_error(Message, Line))) = load_file_reply(load_file_reply_return_code_syntaxerror, yes(Message), yes(Line)).
load_result_to_proto(error(ProtoError)) = load_file_reply(load_file_reply_return_code_protocolerror, yes(ProtoError), no).

%------------------------------------------------------------------------------%

:- pred is_ctx_proof(proof(ctx_modality)::in) is det.

is_ctx_proof(_).
