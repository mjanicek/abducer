// ----------------------------------------------------------------------------
// Copyright (C) 2010-2011 DFKI GmbH Talking Robots 
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

#include "ForkingServer.h"

#include <sys/types.h>
#include <sys/socket.h>
#include <sys/un.h>

#include <IceUtil/IceUtil.h>

#include <log4cxx/logger.h>

#include <sstream>

#include "EngineProtobufWrapper.h"

using namespace std;
using namespace log4cxx;
using namespace ::de::dfki::lt::tr::infer::abducer;

LoggerPtr serverLogger(Logger::getLogger("abducer.server"));

ForkingServer::ForkingServer(const vector<string> & engineArgV_, int socket_fd_)
: engineArgV(engineArgV_), socket_fd(socket_fd_),
		adapters(),
		identities(),
		communicators(),
		base_port(15200)
{
	string s;
	for (vector<string>::const_iterator it = engineArgV.begin(); it != engineArgV.end(); ++it) {
		s += "\"" + *it + "\"";
		if (it + 1 != engineArgV.end()) {
			s += ", ";
		}
	}
	LOG4CXX_INFO(serverLogger, "engines will be started as [" << s << "]");
}

ForkingServer::~ForkingServer()
{
//	cerr << NOTIFY_MSG("shutting down all engines") << endl;
	map<string, Ice::CommunicatorPtr>::iterator it;

	for (it = communicators.begin(); it != communicators.end(); ++it) {
		LOG4CXX_DEBUG(serverLogger, "shutting down " << it->first);
		it->second->destroy();
	}
	LOG4CXX_DEBUG(serverLogger, "server shut down");
}

engine::AbductionEnginePrx
ForkingServer::getEngineProxy(const string & name, const Ice::Current&)
{
	map<string, Ice::ObjectAdapterPtr>::iterator it = adapters.find(name);

	if (it == adapters.end()) {
		// not found, start a new server
		adapters[name] = startNewServer(name);
	}
	else {
		LOG4CXX_DEBUG(serverLogger, "engine " + name + " seems to be running, checking");
		adapters[name]->createProxy(identities[name])->ice_ping();  // FIXME: doesn't test the *engine*
		LOG4CXX_DEBUG(serverLogger, "the engine seems to be up");
	}

	Ice::ObjectPrx oprx = adapters[name]->createProxy(identities[name]);

	if (engine::AbductionEnginePrx eprx = engine::AbductionEnginePrx::checkedCast(oprx)) {
		return eprx;
	}
	else {
		LOG4CXX_ERROR(serverLogger, "cannot cast an ObjectPrx to AbductionEnginePrx");
		return 0;
	}
}

Ice::ObjectAdapterPtr
ForkingServer::startNewServer(const string & engineName)
{
	pid_t pchild;

	if ((pchild = fork()) == 0) {

		vector<char *> argv;
		for (vector<string>::iterator it = engineArgV.begin(); it != engineArgV.end(); ++it) {
			argv.push_back(const_cast<char *>(it->c_str()));
		}
		argv.push_back(NULL);

		execvp(argv[0], &argv[0]);
		perror("exec()");
		return 0;
	}
	else {
		LOG4CXX_INFO(serverLogger, "new abducer engine PID: " << pchild);

		int connection_fd;
		struct sockaddr_un address;
		socklen_t address_length = sizeof(address);

		LOG4CXX_DEBUG(serverLogger, "waiting for a connection on the socket");
		if ((connection_fd = accept(socket_fd, (struct sockaddr *) &address, &address_length)) == -1) {
			LOG4CXX_ERROR(serverLogger, "accept() failed: " << strerror(errno));
			return 0;
		}

		Ice::CommunicatorPtr ic = Ice::initialize();

		stringstream ss_endpoints;
		ss_endpoints << "tcp -p " << base_port++;
	
		string engineEndpoints = "tcp -p " + base_port++;

		LOG4CXX_INFO(serverLogger, "starting up an engine wrapper at " << engineName << ":" << ss_endpoints.str());

		Ice::ObjectAdapterPtr adapter
				= ic->createObjectAdapterWithEndpoints(engineName, ss_endpoints.str());

		Ice::Identity ident;
		ident.name = IceUtil::generateUUID();
		ident.category = "";

		Ice::ObjectPtr object = new EngineProtobufWrapper(engineName, pchild, connection_fd, connection_fd);
		adapter->add(object, ident);
		adapter->activate();

		communicators[engineName] = ic;
		identities[engineName] = ident;

		return adapter;
	}
}
