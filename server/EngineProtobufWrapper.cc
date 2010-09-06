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

#include "common.h"
#include "EngineProtobufWrapper.h"

#include "SliceToString.h"
#include "SliceToProto.h"
#include "ProtoToSlice.h"

#include "protocol.pb.h"

#include "TtyUtils.h"

#include "Logging.h"

#include "ProtoUtil.h"
#include "ProtocolException.h"

using namespace std;
using namespace Abducer;

#include <vector>

EngineProtobufWrapper::EngineProtobufWrapper(pid_t abducer_pid_, int fd_in_, int fd_out_)
: abducer_pid(abducer_pid_),
		fd_in(fd_in_),
		fd_out(fd_out_)
{
	GOOGLE_PROTOBUF_VERIFY_VERSION;
	cerr << SERVER_MSG("initialising the abducer") << endl;
	clearContext();
}

EngineProtobufWrapper::~EngineProtobufWrapper()
{
//	close(fd_in);
//	cerr << NOTIFY_MSG("killing process " << abducer_pid) << endl;
	kill(abducer_pid, SIGKILL);
}

void
EngineProtobufWrapper::clearContext()
{
	cerr << REQUEST_MSG("clearing context") << endl;

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
	cerr << REQUEST_MSG("loading file [" << filename << "]") << endl;

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
				cerr << ERROR_MSG("file read error") << endl;
				throw FileReadErrorException(filename);
			}
			break;

		case protocol::LoadFileReply::SYNTAXERROR:
			{
				if (reply.has_error() && reply.has_line()) {
					cerr << ERROR_MSG("syntax error: " << reply.error() << " on line " << reply.line()) << endl;
					throw SyntaxErrorException(filename, reply.error(), reply.line());
				}
				else {
					cerr << ERROR_MSG("unknown syntax error") << endl;
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
	cerr << REQUEST_MSG("clearing rules") << endl;

	protocol::Request request;
	request.set_rt(protocol::Request::CLEARRULES);
	writeMessageToFileDescriptor(fd_out, request.SerializeAsString());

	checkOkReply();
}

void
EngineProtobufWrapper::clearFacts(const Ice::Current&)
{
	cerr << REQUEST_MSG("clearing all facts") << endl;

	protocol::Request request;
	request.set_rt(protocol::Request::CLEARFACTS);
	writeMessageToFileDescriptor(fd_out, request.SerializeAsString());

	checkOkReply();
}

void
EngineProtobufWrapper::clearFactsByModality(Modality mod, const Ice::Current&)
{
	cerr << REQUEST_MSG("clearing [" << modalityToString(mod) << "] facts") << endl;

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
	cerr << REQUEST_MSG("clearing assumables") << endl;

	protocol::Request request;
	request.set_rt(protocol::Request::CLEARASSUMABLES);
	writeMessageToFileDescriptor(fd_out, request.SerializeAsString());

	checkOkReply();
}

void
EngineProtobufWrapper::clearAssumabilityFunction(const string & function, const Ice::Current&)
{
	cerr << REQUEST_MSG("clearing assumable function [" << function << "]") << endl;

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
	cerr << REQUEST_MSG("clearing disjoint declarations") << endl;

	protocol::Request request;
	request.set_rt(protocol::Request::CLEARDISJOINTDECLS);
	writeMessageToFileDescriptor(fd_out, request.SerializeAsString());

	checkOkReply();
}

void
EngineProtobufWrapper::addRule(const RulePtr & rule, const Ice::Current&)
{
	cerr << REQUEST_MSG("adding a rule") << endl;

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
	cerr << REQUEST_MSG("adding fact [" << modalisedAtomToString(fact) << "]") << endl;

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
	cerr << REQUEST_MSG("adding assumable [" << modalisedAtomToString(a) << " / " << function << "]") << endl;

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
	cerr << REQUEST_MSG("adding a disjoint declaration") << endl;

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
EngineProtobufWrapper::startProving(const vector<MarkedQueryPtr> & qs, const Ice::Current&)
{
	cerr << REQUEST_MSG("proving started") << endl;

	protocol::Request request;
	request.set_rt(protocol::Request::PROVE);
	writeMessageToFileDescriptor(fd_out, request.SerializeAsString());

	protocol::Prove arg;
	vector<MarkedQueryPtr>::const_iterator i;
	for (i = qs.begin(); i != qs.end(); i++) {
		arg.add_queries()->CopyFrom(protoMarkedQuery(*i));
	}
	writeMessageToFileDescriptor(fd_out, arg.SerializeAsString());

	checkOkReply();
}

vector<ProofWithCostPtr>
EngineProtobufWrapper::getProofs(int timeout, const Ice::Current&)
{
	cerr << REQUEST_MSG("waiting for results, timeout=" << timeout) << endl;

	struct pollfd pfd[1];
	pfd[0].fd = fd_in;
	pfd[0].events = POLLIN;

	int rc = poll(pfd, 1, timeout);
	if (rc > 0) {
		// something is on stdin -> finished
		cerr << NOTIFY_MSG("results ready before timeout") << endl;
	}
	else if (rc == 0) {
		// timeout -- send SIGUSR1 to the abducer and assume that it's enough
		cerr << NOTIFY_MSG("timeout") << endl;
		kill(abducer_pid, SIGUSR1);
	}
	else {
		// an error occurred
		cerr << ERROR_MSG("poll() error") << endl;
		return vector<ProofWithCostPtr>();
	}

	// retrieve all proofs
	const vector<ProofWithCostPtr> & proofs = getProofs();
	cerr << NOTIFY_MSG("got " << proofs.size() << " results") << endl;
	return proofs;
}

vector<ProofWithCostPtr>
EngineProtobufWrapper::getProofs()
{
	debug(cerr << NOTIFY_MSG("retrieving proofs") << endl);

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

	debug(cerr << NOTIFY_MSG("parsed the message, " << reply.proofs_size() << " proofs total") << endl);

	vector<ProofWithCostPtr> result;
	for (int i = 0; i < reply.proofs_size(); i++) {
		result.push_back(proofWithCostFromProto(reply.proofs(i)));
	}
	return result;
}
