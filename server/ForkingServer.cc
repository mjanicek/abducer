// ----------------------------------------------------------------------------
// Copyright (C) 2010 DFKI GmbH Talking Robots 
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

#include "Logging.h"

#include <sys/types.h>
#include <sys/socket.h>
#include <sys/un.h>

#include "EngineProtobufWrapper.h"

using namespace std;
using namespace Abducer;

ForkingServer::ForkingServer(const string & enginePath_, const string & socketPath_, int socket_fd_)
: enginePath(enginePath_),
		socketPath(socketPath_), socket_fd(socket_fd_),
		engines()
{
}

AbductionEnginePrx
ForkingServer::getEngineProxy(const string & name, const Ice::Current&)
{
	// look up the server name
	return 0;
}

void
ForkingServer::startNewServer(const string & name)
{
	pid_t pchild;

	if ((pchild = fork()) == 0) {
		execlp(enginePath.c_str(), enginePath.c_str(), socketPath.c_str(), NULL);
		perror("exec()");
	}
	else {
		int connection_fd;
		struct sockaddr_un address;
		socklen_t address_length;

		cerr << NOTIFY_MSG("waiting for connections at [" << socketPath << "]") << endl;
		if ((connection_fd = accept(socket_fd, (struct sockaddr *) &address, &address_length)) == -1) {
			cerr << NOTIFY_MSG("accept() failed") << endl;
		}

		Ice::CommunicatorPtr ic = Ice::initialize();
		string engineEndpoints;  // TODO: use "name" directly?

		cerr << SERVER_MSG("starting up an engine wrapper at " << tty::white << name << ":" << engineEndpoints << tty::dcol) << endl;

		Ice::ObjectAdapterPtr adapter
				= ic->createObjectAdapterWithEndpoints("AbducerAdapter", engineEndpoints);

		Ice::ObjectPtr object = new EngineProtobufWrapper(pchild, connection_fd, connection_fd);
		adapter->add(object, ic->stringToIdentity(name)); // XXX here
		adapter->activate();
	}
}

/*
int
runServer(pid_t abducer_pid, const Settings & s, int fd)
{
//	printStatus(abducer_pid, s);
//	IceUtil::CtrlCHandler ctrlCHandler(shutdownServer);
	int status = 0;
	try {
		ic = Ice::initialize();

		cerr << SERVER_MSG("setting up server at " << tty::white << s.serverName<< ":" << s.serverEndpoints << tty::dcol) << endl;

		Ice::ObjectAdapterPtr adapter
				= ic->createObjectAdapterWithEndpoints("AbducerAdapter", s.serverEndpoints);

		Ice::ObjectPtr object = new EngineProtobufWrapper(abducer_pid, fd, fd);
		adapter->add(object, ic->stringToIdentity(s.serverName));
		adapter->activate();

		ic->waitForShutdown();
	}
	catch (const Abducer::EngineException & e) {
		cerr << ERROR_MSG("server exception: " << e.message) << endl;
	}
	catch (const Ice::Exception& e) {
		cerr << e << endl;
		status = 1;
	}
	catch (const char* msg) {
		cerr << msg << endl;
		status = 1;
	}

	cerr << SERVER_MSG("server shut down") << endl;

	if (ic) {
		try {
			ic->destroy();
		}
		catch (const Ice::Exception& e) {
			cerr << e << endl;
			status = 1;
		}
	}
	return status;
}
*/
