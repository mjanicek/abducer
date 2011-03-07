// ----------------------------------------------------------------------------
// Copyright (C) 2009-2011 DFKI GmbH Talking Robots 
// Miroslav Janicek (miroslav.janicek@dfki.de) 
//
// This library is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public License 
// as published by the Free Software Foundation; either version 2.1 of
// the License, or (at your option) any later version.
//
// This library is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
// Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public
// License along with this program; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
// 02111-1307, USA.
// ----------------------------------------------------------------------------

#include <poll.h>
#include <sys/time.h>
#include <sys/types.h>
#include <signal.h>

#include <log4cxx/logger.h>

#include "common.h"
#include "EngineProtobufWrapper.h"

#include "SliceToString.h"
#include "SliceToProto.h"
#include "ProtoToSlice.h"

#include "protocol.pb.h"

#include "ProtoUtil.h"
#include "ProtocolException.h"

using namespace std;
using namespace log4cxx;
using namespace ::de::dfki::lt::tr::infer::abducer;

#include <vector>

EngineProtobufWrapper::EngineProtobufWrapper(const string & name_, pid_t abducer_pid_, int fd_in_, int fd_out_)
: name(name_),
		abducer_pid(abducer_pid_),
		fd_in(fd_in_),
		fd_out(fd_out_),
		logger(Logger::getLogger("abducer.engines." + name))
{
	GOOGLE_PROTOBUF_VERIFY_VERSION;
	LOG4CXX_INFO(logger, "initialising engine wrapper, engine PID = " << abducer_pid);
}

EngineProtobufWrapper::~EngineProtobufWrapper()
{
//	close(fd_in);
	LOG4CXX_INFO(logger, "sending SIGKILL to PID " << abducer_pid);
	kill(abducer_pid, SIGKILL);
}

void
EngineProtobufWrapper::clearContext()
{
	LOG4CXX_DEBUG(logger, "clearing context");

	protocol::Request request;
	request.set_rt(protocol::Request::CLEARCONTEXT);
	writeMessageToFileDescriptor(logger, fd_out, request.SerializeAsString());

	checkOkReply();
}

void
EngineProtobufWrapper::checkOkReply()
{
	string s_ack = readMessageFromFileDescriptor(logger, fd_in);
	if (s_ack == "") {
		throw ProtocolException("failed to read the reply");
	}

	protocol::RequestReply ack;
	if (!ack.ParseFromString(s_ack)) {
		throw ProtocolException("failed to parse a RequestReply");
	}

	switch (ack.rc()) {
		case protocol::RequestReply::OK:
			return;

		case protocol::RequestReply::PROTOCOLERROR:
			if (ack.has_error_message()) {
				throw ProtocolException(ack.error_message());
			}
			return;

		default:
			throw ProtocolException("unknown protocol error");
			return;
	}
}

void
EngineProtobufWrapper::clearContext(const Ice::Current&)
{
	clearContext();
}

void
EngineProtobufWrapper::loadFile(const string& filename, const Ice::Current&)
{
	LOG4CXX_DEBUG(logger, "loading file [" << filename << "]");

	protocol::Request request;
	request.set_rt(protocol::Request::LOADFILE);
	writeMessageToFileDescriptor(logger, fd_out, request.SerializeAsString());

	protocol::LoadFile arg;
	arg.set_file_name(filename);
	writeMessageToFileDescriptor(logger, fd_out, arg.SerializeAsString());

	string s_reply = readMessageFromFileDescriptor(logger, fd_in);
	if (s_reply == "") {
		throw ProtocolException("failed to read a message");
	}

	protocol::LoadFileReply reply;
	if (!reply.ParseFromString(s_reply)) {
		throw ProtocolException("failed to parse a LoadFileResult");
	}

	switch (reply.rc()) {
		case protocol::LoadFileReply::OK:
			break;

		case protocol::LoadFileReply::PROTOCOLERROR:
			if (reply.has_error()) {
				throw ProtocolException(reply.error());
			}
			else {
				throw ProtocolException("unknown protocol error in loadFile");
			}
			break;

		case protocol::LoadFileReply::IOERROR:
			{
				LOG4CXX_ERROR(logger, "file read error");
				throw engine::FileReadErrorException(filename);
			}
			break;

		case protocol::LoadFileReply::SYNTAXERROR:
			{
				if (reply.has_error() && reply.has_line()) {
					LOG4CXX_ERROR(logger, "syntax error: " << reply.error() << " on line " << reply.line());
					throw engine::SyntaxErrorException(filename, reply.error(), reply.line());
				}
				else {
					LOG4CXX_ERROR(logger, "unknown syntax error");
					throw engine::SyntaxErrorException(filename, "unknown syntax error", 1);
				}
			}
			break;

		default:
			throw ProtocolException("unknown return code in load file");
			break;
	}
}

void
EngineProtobufWrapper::clearRules(const Ice::Current&)
{
	LOG4CXX_DEBUG(logger, "clearing rules");

	protocol::Request request;
	request.set_rt(protocol::Request::CLEARRULES);
	writeMessageToFileDescriptor(logger, fd_out, request.SerializeAsString());

	checkOkReply();
}

void
EngineProtobufWrapper::clearFacts(const Ice::Current&)
{
	LOG4CXX_DEBUG(logger, "clearing all facts");

	protocol::Request request;
	request.set_rt(protocol::Request::CLEARFACTS);
	writeMessageToFileDescriptor(logger, fd_out, request.SerializeAsString());

	checkOkReply();
}

void
EngineProtobufWrapper::clearFactsByModality(lang::Modality mod, const Ice::Current&)
{
	LOG4CXX_DEBUG(logger, "clearing [" << modalityToString(mod) << "] facts");

	protocol::Request request;
	request.set_rt(protocol::Request::CLEARFACTSBYMODALITY);
	writeMessageToFileDescriptor(logger, fd_out, request.SerializeAsString());

	protocol::ClearFactsByModality arg;
	arg.set_mod(protoModality(mod));
	writeMessageToFileDescriptor(logger, fd_out, arg.SerializeAsString());

	checkOkReply();
}

void
EngineProtobufWrapper::clearAssumables(const Ice::Current&)
{
	LOG4CXX_DEBUG(logger, "clearing assumables");

	protocol::Request request;
	request.set_rt(protocol::Request::CLEARASSUMABLES);
	writeMessageToFileDescriptor(logger, fd_out, request.SerializeAsString());

	checkOkReply();
}

void
EngineProtobufWrapper::clearAssumabilityFunction(const string & function, const Ice::Current&)
{
	LOG4CXX_DEBUG(logger, "clearing assumable function [" << function << "]");

	protocol::Request request;
	request.set_rt(protocol::Request::CLEARASSUMABILITYFUNCTION);
	writeMessageToFileDescriptor(logger, fd_out, request.SerializeAsString());

	protocol::ClearAssumabilityFunction arg;
	arg.set_function_name(function);
	writeMessageToFileDescriptor(logger, fd_out, arg.SerializeAsString());

	checkOkReply();
}

void
EngineProtobufWrapper::clearDisjointDeclarations(const Ice::Current&)
{
	LOG4CXX_DEBUG(logger, "clearing disjoint declarations");

	protocol::Request request;
	request.set_rt(protocol::Request::CLEARDISJOINTDECLS);
	writeMessageToFileDescriptor(logger, fd_out, request.SerializeAsString());

	checkOkReply();
}

void
EngineProtobufWrapper::addRule(const lang::RulePtr & rule, const Ice::Current&)
{
	LOG4CXX_DEBUG(logger, "adding a rule");

	protocol::Request request;
	request.set_rt(protocol::Request::ADDRULE);
	writeMessageToFileDescriptor(logger, fd_out, request.SerializeAsString());

	protocol::AddRule arg;
	arg.mutable_rule()->CopyFrom(protoModalisedRule(rule));
	writeMessageToFileDescriptor(logger, fd_out, arg.SerializeAsString());

	checkOkReply();
}

void
EngineProtobufWrapper::addFact(const lang::ModalisedAtomPtr & fact, const Ice::Current&)
{
	LOG4CXX_DEBUG(logger, "adding fact [" << modalisedAtomToString(fact) << "]");

	protocol::Request request;
	request.set_rt(protocol::Request::ADDFACT);
	writeMessageToFileDescriptor(logger, fd_out, request.SerializeAsString());

	protocol::AddFact arg;
	arg.mutable_fact()->CopyFrom(protoModalisedAtom(fact));
	writeMessageToFileDescriptor(logger, fd_out, arg.SerializeAsString());

	checkOkReply();
}

void
EngineProtobufWrapper::addAssumable(const string & function, const lang::ModalisedAtomPtr & a, float cost, const Ice::Current&)
{
	LOG4CXX_DEBUG(logger, "adding assumable [" << modalisedAtomToString(a) << " / " << function << "]");

	protocol::Request request;
	request.set_rt(protocol::Request::ADDASSUMABLE);
	writeMessageToFileDescriptor(logger, fd_out, request.SerializeAsString());

	protocol::AddAssumable arg;
	arg.set_function_name(function);
	arg.mutable_fact()->CopyFrom(protoModalisedAtom(a));
	arg.set_cost(cost);
	writeMessageToFileDescriptor(logger, fd_out, arg.SerializeAsString());

	checkOkReply();
}

void
EngineProtobufWrapper::addDisjointDeclaration(const lang::DisjointDeclarationPtr & dd, const Ice::Current&)
{
	LOG4CXX_DEBUG(logger, "adding a disjoint declaration");

	protocol::Request request;
	request.set_rt(protocol::Request::ADDDISJOINTDECL);
	writeMessageToFileDescriptor(logger, fd_out, request.SerializeAsString());

	protocol::AddDisjointDecl arg;
	vector<lang::ModalisedAtomPtr>::const_iterator i;
	for (i = dd->atoms.begin(); i != dd->atoms.end(); i++) {
		arg.add_dd()->CopyFrom(protoModalisedAtom(*i));
	}
	// FIXME: check that it's non-empty
	writeMessageToFileDescriptor(logger, fd_out, arg.SerializeAsString());

	checkOkReply();
}

void
EngineProtobufWrapper::startProvingWithMethod(const vector<proof::MarkedQueryPtr> & qs, const engine::ProofSearchMethodPtr & method, const Ice::Current&)
{
	LOG4CXX_DEBUG(logger, "proving started");

	protocol::Request request;
	request.set_rt(protocol::Request::PROVE);
	writeMessageToFileDescriptor(logger, fd_out, request.SerializeAsString());

	protocol::Prove arg;
	vector<proof::MarkedQueryPtr>::const_iterator i;
	for (i = qs.begin(); i != qs.end(); i++) {
		arg.add_queries()->CopyFrom(protoMarkedQuery(*i));
	}
	arg.mutable_method()->CopyFrom(protoProofSearchMethod(method));
	writeMessageToFileDescriptor(logger, fd_out, arg.SerializeAsString());

	checkOkReply();
}

void
EngineProtobufWrapper::startProving(const vector<proof::MarkedQueryPtr> & qs, const Ice::Current& c)
{
	LOG4CXX_WARN(logger, "using the deprecated startProving() interface");
	engine::DFSPtr method = new engine::DFS();
	startProvingWithMethod(qs, method, c);
}

vector<proof::ProofWithCostPtr>
EngineProtobufWrapper::getProofs(int timeout, const Ice::Current&)
{
	LOG4CXX_DEBUG(logger, "waiting for results, timeout=" << timeout);

	struct pollfd pfd[1];
	pfd[0].fd = fd_in;
	pfd[0].events = POLLIN;

	int rc = poll(pfd, 1, timeout);
	if (rc > 0) {
		// something is on stdin -> finished
		LOG4CXX_DEBUG(logger, "results ready before timeout");
	}
	else if (rc == 0) {
		// timeout -- send SIGUSR1 to the abducer and assume that it's enough
		LOG4CXX_DEBUG(logger, "timeout, sending SIGUSR1 to the engine");
		kill(abducer_pid, SIGUSR1);
	}
	else {
		// an error occurred
		LOG4CXX_ERROR(logger, "poll() error: " << strerror(errno));
		return vector<proof::ProofWithCostPtr>();
	}

	// retrieve all proofs
	const vector<proof::ProofWithCostPtr> & proofs = getProofs();
	LOG4CXX_DEBUG(logger, "got " << proofs.size() << " results");
	return proofs;
}

vector<proof::ProofWithCostPtr>
EngineProtobufWrapper::getProofs()
{
	LOG4CXX_TRACE(logger, "retrieving proofs");

	string s_reply = readMessageFromFileDescriptor(logger, fd_in);
	if (s_reply == "") {
		throw ProtocolException("failed to read a message");
	}

	protocol::ProveReply reply;
	if (!reply.ParseFromString(s_reply)) {
		throw ProtocolException("failed to parse a ProveReply");
	}

	if (reply.rc() == protocol::ProveReply::ABDUCERERROR) {
		throw engine::EngineException("abducer internal error");
	}

	LOG4CXX_TRACE(logger, "parsed the message, " << reply.proofs_size() << " proofs total");

	vector<proof::ProofWithCostPtr> result;
	for (int i = 0; i < reply.proofs_size(); i++) {
		result.push_back(proofWithCostFromProto(reply.proofs(i)));
	}
	return result;
}
