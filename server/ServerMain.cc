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

#include <unistd.h>
#include <sys/wait.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/un.h>

#include <Ice/Ice.h>
#include <IceUtil/CtrlCHandler.h>
#include "weigabd.h"

#include <iostream>

#include "ForwardedAbducerServer.h"
#include "TtyUtils.h"

#include "CLI.h"
#include "Version.h"
#include "Logging.h"

using namespace std;

#define PIPE_READ   0
#define PIPE_WRITE  1

const string DEFAULT_SERVER_NAME = "AbducerServer";
const string DEFAULT_SERVER_ENDPOINTS = "default -p 10000";
const string DEFAULT_ABDUCER_PATH = "/usr/bin/false";
const string DEFAULT_SOCKET_PATH = "./unsock-abducer";

// this probably shouldn't be static
static Ice::CommunicatorPtr ic;

int
runServer(pid_t abducer_pid, const Settings & s, int fd);

void
shutdownServer(int);

void
printStatus(pid_t abducerPID, const Settings & s);

void
printUsage();

void
printVersion();

string
getSocketName();

int
prepareSocket(const string & address);

int
main(int argc, char ** argv)
{
	Settings s;
	s.serverName = DEFAULT_SERVER_NAME;
	s.serverEndpoints = DEFAULT_SERVER_ENDPOINTS;
	s.abducerPath = DEFAULT_ABDUCER_PATH;

	switch (processCommandLineArgs(argc, argv, s)) {
	case Start:
		{
			printVersion();

			string socketPath = getSocketName();
			int socketFd;

			if ((socketFd = prepareSocket(socketPath)) == -1) {
				return EXIT_FAILURE;
			}

			pid_t pchild;

			if ((pchild = fork()) == 0) {
//				preparePlumbing(true);

				execlp(s.abducerPath.c_str(), s.abducerPath.c_str(), socketPath.c_str(), NULL);
				perror("Exec failed");
			}
			else {
//				preparePlumbing(false);

				int connectionFd;
				struct sockaddr_un address;
				socklen_t address_length;

				cerr << NOTIFY_MSG("waiting for connections at \"" << socketPath << "\"") << endl;
				if ((connectionFd = accept(socketFd, (struct sockaddr *) &address, &address_length)) == -1) {
					cerr << NOTIFY_MSG("accept() failed") << endl;
				}
				
				runServer(pchild, s, connectionFd);

				wait(0);
				cerr << NOTIFY_MSG("unlinking the socket") << endl;
				unlink(socketPath.c_str());
			}

			return EXIT_SUCCESS;
		}
		break;

	case PrintHelp:
		{
			printUsage();
			return EXIT_SUCCESS;
		}
		break;

	case Error:
	default:
		{
			cout << "Usage error." << endl;
			printUsage();
			return EXIT_FAILURE;
		}
		break;
	}
}

int
runServer(pid_t abducer_pid, const Settings & s, int fd)
{
	printStatus(abducer_pid, s);
	IceUtil::CtrlCHandler ctrlCHandler(shutdownServer);
	int status = 0;
	try {
		ic = Ice::initialize();

		cerr << SERVER_MSG("setting up server at " << tty::white << s.serverName<< ":" << s.serverEndpoints << tty::dcol) << endl;

		Ice::ObjectAdapterPtr adapter
				= ic->createObjectAdapterWithEndpoints("AbducerAdapter", s.serverEndpoints);

		Ice::ObjectPtr object = new ForwardedAbducerServer(abducer_pid, fd, fd);
		adapter->add(object, ic->stringToIdentity(s.serverName));
		adapter->activate();

		ic->waitForShutdown();
	}
	catch (const Abducer::ServerException & e) {
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

void
shutdownServer(int signum)
{
	cerr << endl;
	cerr << SERVER_MSG("received signal " << signum) << endl;
	try {
		ic->shutdown();
	}
	catch (const Ice::Exception e) {
		cerr << e << endl;
		exit(1);
	}
	// TODO: kill the child if it's still alive
}

void
printStatus(pid_t abducerPID, const Settings & s)
{
	const size_t cwd_length = 512;
	char * cwd = new char[512];
	getcwd(cwd, cwd_length);

	cerr << NOTIFY_MSG("server interface revision " << tty::white << ABDUCER_ICE_VERSION << tty::dcol) << endl;
	cerr << NOTIFY_MSG("abducer binary: " << s.abducerPath) << endl;
	if (s.abducerPath == DEFAULT_ABDUCER_PATH) {
		cerr << WARNING_MSG("abducer binary is set to default") << endl;
	}

	cerr << NOTIFY_MSG("abducer working directory: " << cwd) << endl;
	cerr << NOTIFY_MSG("abducer PID: " << abducerPID) << endl;

	delete cwd;
}

void
printUsage()
{
	cout << "abducer-server ARGS" << endl
		<< endl
		<< "ARGS may be the following (defaults in brackets):" << endl
		<< "  -n NAME         Name of the ICE server [" << DEFAULT_SERVER_NAME << "]" << endl
		<< "  -e ENDPOINTS    Endpoints of the ICE server [" << DEFAULT_SERVER_ENDPOINTS << "]" << endl
		<< "  -a ABDUCER_BIN  Path to the abducer binary [" << DEFAULT_ABDUCER_PATH << "]" << endl
		<< "  -h              Print (this) help" << endl;
}

void
printVersion()
{
	cerr << "Abducer server " << ABDUCER_VERSION << endl;
	cerr << "(c) 2009-2010 DFKI GmbH Talking Robots" << endl;
}

string
getSocketName()
{
	return DEFAULT_SOCKET_PATH;
}

int
prepareSocket(const string & socketPath)
{
	int socket_fd;

	socket_fd = socket(PF_UNIX, SOCK_STREAM, 0);
	if (socket_fd < 0) {
		cerr << ERROR_MSG("socket() failed") << endl;
		return -1;
	} 

	struct sockaddr_un address;
	address.sun_family = AF_UNIX;
	strcpy(address.sun_path, socketPath.c_str());
	size_t address_length = sizeof(address.sun_family) + strlen(address.sun_path) + 1;

	if (bind(socket_fd, (struct sockaddr *) &address, address_length) != 0) {
		cerr << ERROR_MSG("bind() to \"" << socketPath << "\" failed") << endl;
		return -1;
	}

	if (listen(socket_fd, 1) != 0) {
		cerr << ERROR_MSG("listen() failed") << endl;
		return -1;
	}

	return socket_fd;
}
