// ----------------------------------------------------------------------------
// Copyright (C) 2009-2010 DFKI GmbH Talking Robots 
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

#include "TtyUtils.h"

#include "ProtoUtil.h"
#include "ProtocolException.h"

using namespace std;
using namespace log4cxx;
using namespace Abducer;

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
	writeMessageToFileDescriptor(fd_out, request.SerializeAsString());

	checkOkReply();
}

void
EngineProtobufWrapper::checkOkReply()
{
	string s_ack = readMessageFromFileDescriptor(fd_in);
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
	writeMessageToFileDescriptor(fd_out, request.SerializeAsString());

	protocol::LoadFile arg;
	arg.set_file_name(filename);
	writeMessageToFileDescriptor(fd_out, arg.SerializeAsString());

	string s_reply = readMessageFromFileDescriptor(fd_in);
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
				throw FileReadErrorException(filename);
			}
			break;

		case protocol::LoadFileReply::SYNTAXERROR:
			{
				if (reply.has_error() && reply.has_line()) {
					LOG4CXX_ERROR(logger, "syntax error: " << reply.error() << " on line " << reply.line());
					throw SyntaxErrorException(filename, reply.error(), reply.line());
				}
				else {
					LOG4CXX_ERROR(logger, "unknown syntax error");
					throw SyntaxErrorException(filename, "unknown syntax error", 1);
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
	writeMessageToFileDescriptor(fd_out, request.SerializeAsString());

	checkOkReply();
}

void
EngineProtobufWrapper::clearFacts(const Ice::Current&)
{
	LOG4CXX_DEBUG(logger, "clearing all facts");

	protocol::Request request;
	request.set_rt(protocol::Request::CLEARFACTS);
	writeMessageToFileDescriptor(fd_out, request.SerializeAsString());

	checkOkReply();
}

void
EngineProtobufWrapper::clearFactsByModality(Modality mod, const Ice::Current&)
{
	LOG4CXX_DEBUG(logger, "clearing [" << modalityToString(mod) << "] facts");

	protocol::Request request;
	request.set_rt(protocol::Request::CLEARFACTSBYMODALITY);
	writeMessageToFileDescriptor(fd_out, request.SerializeAsString());

	protocol::ClearFactsByModality arg;
	arg.set_mod(protoModality(mod));
	writeMessageToFileDescriptor(fd_out, arg.SerializeAsString());

	checkOkReply();
}

void
EngineProtobufWrapper::clearAssumables(const Ice::Current&)
{
	LOG4CXX_DEBUG(logger, "clearing assumables");

	protocol::Request request;
	request.set_rt(protocol::Request::CLEARASSUMABLES);
	writeMessageToFileDescriptor(fd_out, request.SerializeAsString());

	checkOkReply();
}

void
EngineProtobufWrapper::clearAssumabilityFunction(const string & function, const Ice::Current&)
{
	LOG4CXX_DEBUG(logger, "clearing assumable function [" << function << "]");

	protocol::Request request;
	request.set_rt(protocol::Request::CLEARASSUMABILITYFUNCTION);
	writeMessageToFileDescriptor(fd_out, request.SerializeAsString());

	protocol::ClearAssumabilityFunction arg;
	arg.set_function_name(function);
	writeMessageToFileDescriptor(fd_out, arg.SerializeAsString());

	checkOkReply();
}

void
EngineProtobufWrapper::clearDisjointDeclarations(const Ice::Current&)
{
	LOG4CXX_DEBUG(logger, "clearing disjoint declarations");

	protocol::Request request;
	request.set_rt(protocol::Request::CLEARDISJOINTDECLS);
	writeMessageToFileDescriptor(fd_out, request.SerializeAsString());

	checkOkReply();
}

void
EngineProtobufWrapper::addRule(const RulePtr & rule, const Ice::Current&)
{
	LOG4CXX_DEBUG(logger, "adding a rule");

	protocol::Request request;
	request.set_rt(protocol::Request::ADDRULE);
	writeMessageToFileDescriptor(fd_out, request.SerializeAsString());

	protocol::AddRule arg;
	arg.mutable_rule()->CopyFrom(protoModalisedRule(rule));
	writeMessageToFileDescriptor(fd_out, arg.SerializeAsString());

	checkOkReply();
}

void
EngineProtobufWrapper::addFact(const ModalisedAtomPtr & fact, const Ice::Current&)
{
	LOG4CXX_DEBUG(logger, "adding fact [" << modalisedAtomToString(fact) << "]");

	protocol::Request request;
	request.set_rt(protocol::Request::ADDFACT);
	writeMessageToFileDescriptor(fd_out, request.SerializeAsString());

	protocol::AddFact arg;
	arg.mutable_fact()->CopyFrom(protoModalisedAtom(fact));
	writeMessageToFileDescriptor(fd_out, arg.SerializeAsString());

	checkOkReply();
}

void
EngineProtobufWrapper::addAssumable(const string & function, const ModalisedAtomPtr & a, float cost, const Ice::Current&)
{
	LOG4CXX_DEBUG(logger, "adding assumable [" << modalisedAtomToString(a) << " / " << function << "]");

	protocol::Request request;
	request.set_rt(protocol::Request::ADDASSUMABLE);
	writeMessageToFileDescriptor(fd_out, request.SerializeAsString());

	protocol::AddAssumable arg;
	arg.set_function_name(function);
	arg.mutable_fact()->CopyFrom(protoModalisedAtom(a));
	arg.set_cost(cost);
	writeMessageToFileDescriptor(fd_out, arg.SerializeAsString());

	checkOkReply();
}

void
EngineProtobufWrapper::addDisjointDeclaration(const DisjointDeclarationPtr & dd, const Ice::Current&)
{
	LOG4CXX_DEBUG(logger, "adding a disjoint declaration");

	protocol::Request request;
	request.set_rt(protocol::Request::ADDDISJOINTDECL);
	writeMessageToFileDescriptor(fd_out, request.SerializeAsString());

	protocol::AddDisjointDecl arg;
	vector<ModalisedAtomPtr>::const_iterator i;
	for (i = dd->atoms.begin(); i != dd->atoms.end(); i++) {
		arg.add_dd()->CopyFrom(protoModalisedAtom(*i));
	}
	// FIXME: check that it's non-empty
	writeMessageToFileDescriptor(fd_out, arg.SerializeAsString());

	checkOkReply();
}

void
EngineProtobufWrapper::startProvingWithMethod(const vector<MarkedQueryPtr> & qs, const ProofSearchMethodPtr & method, const Ice::Current&)
{
	LOG4CXX_DEBUG(logger, "proving started");

	protocol::Request request;
	request.set_rt(protocol::Request::PROVE);
	writeMessageToFileDescriptor(fd_out, request.SerializeAsString());

	protocol::Prove arg;
	vector<MarkedQueryPtr>::const_iterator i;
	for (i = qs.begin(); i != qs.end(); i++) {
		arg.add_queries()->CopyFrom(protoMarkedQuery(*i));
	}
	arg.mutable_method()->CopyFrom(protoProofSearchMethod(method));
	writeMessageToFileDescriptor(fd_out, arg.SerializeAsString());

	checkOkReply();
}

void
EngineProtobufWrapper::startProving(const vector<MarkedQueryPtr> & qs, const Ice::Current& c)
{
	LOG4CXX_WARN(logger, "using the deprecated startProving() interface");
	DFSPtr method = new DFS();
	startProvingWithMethod(qs, method, c);
}

vector<ProofWithCostPtr>
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
		return vector<ProofWithCostPtr>();
	}

	// retrieve all proofs
	const vector<ProofWithCostPtr> & proofs = getProofs();
	LOG4CXX_DEBUG(logger, "got " << proofs.size() << " results");
	return proofs;
}

vector<ProofWithCostPtr>
EngineProtobufWrapper::getProofs()
{
	LOG4CXX_TRACE(logger, "retrieving proofs");

	string s_reply = readMessageFromFileDescriptor(fd_in);
	if (s_reply == "") {
		throw ProtocolException("failed to read a message");
	}

	protocol::ProveReply reply;
	if (!reply.ParseFromString(s_reply)) {
		throw ProtocolException("failed to parse a ProveReply");
	}

	if (reply.rc() == protocol::ProveReply::ABDUCERERROR) {
		throw EngineException("abducer internal error");
	}

	LOG4CXX_TRACE(logger, "parsed the message, " << reply.proofs_size() << " proofs total");

	vector<ProofWithCostPtr> result;
	for (int i = 0; i < reply.proofs_size(); i++) {
		result.push_back(proofWithCostFromProto(reply.proofs(i)));
	}
	return result;
}
