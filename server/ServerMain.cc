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
#include <stdlib.h>
#include <sys/wait.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/un.h>

#include <Ice/Ice.h>
#include <IceUtil/CtrlCHandler.h>
#include "weigabd.h"

#include <log4cxx/logger.h>
#include <log4cxx/xml/domconfigurator.h>

#include <iostream>

#include "ForkingServer.h"
#include "EngineProtobufWrapper.h"
#include "TtyUtils.h"

#include "CLI.h"
#include "Version.h"

using namespace std;
using namespace log4cxx;
using namespace log4cxx::xml;

LoggerPtr serverLogger(Logger::getLogger("abducer.main"));

const string DEFAULT_SERVER_NAME = "AbducerServer";
const string DEFAULT_SERVER_ENDPOINTS = "default -p 10000";
const string DEFAULT_ABDUCER_PATH = "/usr/bin/false";
const string SOCKET_FILE_TEMPLATE = "/tmp/abducer-socket.XXXXXX";
const string DEFAULT_LOGCONFIG_PATH = "Log4jConfig.xml";

// this probably shouldn't be static
static Ice::CommunicatorPtr ic;

bool
interfaceVersionOk();

int
runServer(const Settings & s, int socketFd, const string & socketPath);

void
shutdownServer(int);

void
printStatus(const Settings & s);

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
	s.logConfigPath = DEFAULT_LOGCONFIG_PATH;

	switch (processCommandLineArgs(argc, argv, s)) {
	case Start:
		{
			printVersion();
			DOMConfigurator::configure(s.logConfigPath);

			string socketPath = getSocketName();
			int socketFd;

			if ((socketFd = prepareSocket(socketPath)) == -1) {
				return EXIT_FAILURE;
			}
			LOG4CXX_DEBUG(serverLogger, "socket ready at `" << socketPath << "'");

			runServer(s, socketFd, socketPath);

			LOG4CXX_DEBUG(serverLogger, "unlinking `" << socketPath << "'");
			unlink(socketPath.c_str());

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
runServer(const Settings & s, int socketFd, const string & socketPath)
{
	printStatus(s);
	IceUtil::CtrlCHandler ctrlCHandler(shutdownServer);
	int status = 0;
	try {
		ic = Ice::initialize();

		LOG4CXX_INFO(serverLogger, "setting up ICE server at " << s.serverName << ":" << s.serverEndpoints);

		Ice::ObjectAdapterPtr adapter
				= ic->createObjectAdapterWithEndpoints("AbducerServerAdapter", s.serverEndpoints);

		vector<string> argv;
		argv.push_back(s.abducerPath);
		for (vector<string>::const_iterator it = s.abducerArgs.begin(); it != s.abducerArgs.end(); ++it) {
			argv.push_back(*it);
		}
		argv.push_back(socketPath);

		Ice::ObjectPtr object = new ForkingServer(argv, socketFd);
		adapter->add(object, ic->stringToIdentity(s.serverName));
		adapter->activate();

		ic->waitForShutdown();
	}
	catch (const Abducer::engine::EngineException & e) {
		LOG4CXX_ERROR(serverLogger, "server exception: \"" << e.message << "\"");
	}
	catch (const Ice::Exception& e) {
		cerr << e << endl;
		status = 1;
	}
	catch (const char* msg) {
		cerr << msg << endl;
		status = 1;
	}

	if (ic) {
		try {
			ic->destroy();
		}
		catch (const Ice::Exception& e) {
			cerr << e << endl;
			status = 1;
		}
	}
	wait(0);

	LOG4CXX_INFO(serverLogger, "server shut down");
	return status;
}

void
shutdownServer(int signum)
{
	LOG4CXX_DEBUG(serverLogger, "received signal " << signum << ", shutting down");
	try {
		ic->destroy();
	}
	catch (const Ice::Exception e) {
		cerr << e << endl;
		exit(1);
	}
	// TODO: kill the children if they're still alive
}

void
printStatus(const Settings & s)
{
	const size_t cwd_length = 512;
	char * cwd = new char[512];
	getcwd(cwd, cwd_length);

	if (!interfaceVersionOk()) {
		LOG4CXX_WARN(serverLogger, "server interface version " << Abducer::RELEASE << " may be incompatible");
	}

//	cerr << NOTIFY_MSG("abducer binary: [" << s.abducerPath << "]") << endl;
	if (s.abducerPath == DEFAULT_ABDUCER_PATH) {
		LOG4CXX_WARN(serverLogger, "engine binary is set to default");
	}

	delete cwd;
}

inline bool
interfaceVersionOk()
{
	return ABDUCER_VERSION.compare(0, Abducer::RELEASE.length(), Abducer::RELEASE) == 0;
}

void
printUsage()
{
	cout << "abducer-server ARGS" << endl
		<< endl
		<< "ARGS may be the following (defaults in brackets):" << endl
		<< "  -n NAME         Name of the ICE server [" << DEFAULT_SERVER_NAME << "]" << endl
		<< "  -e ENDPOINTS    Endpoints of the ICE server [" << DEFAULT_SERVER_ENDPOINTS << "]" << endl
		<< "  -a ABDUCER_BIN  Path to the engine binary [" << DEFAULT_ABDUCER_PATH << "]" << endl
		<< "  -x ARG          Add ARG to the engine arguments" << endl
		<< "  -l LOG_CONF     Path to the log4cxx config file [" << DEFAULT_LOGCONFIG_PATH << "]" << endl
		<< "  -h              Print (this) help" << endl;
}

void
printVersion()
{
	cout << "Abducer server " << ABDUCER_VERSION << endl;
	cout << "(c) 2009-2010 DFKI GmbH Talking Robots" << endl;
}

string
getSocketName()
{
	char * buf = new char[SOCKET_FILE_TEMPLATE.length() + 1];
	strcpy(buf, SOCKET_FILE_TEMPLATE.c_str());
	buf = mktemp(buf);
	string name(buf);
	delete buf;
	return name;
}

int
prepareSocket(const string & socketPath)
{
	int socket_fd;

	socket_fd = socket(PF_UNIX, SOCK_STREAM, 0);
	if (socket_fd < 0) {
		LOG4CXX_ERROR(serverLogger, "socket() failed: " << strerror(errno));
		return -1;
	} 

	struct sockaddr_un address;
	address.sun_family = AF_UNIX;
	strcpy(address.sun_path, socketPath.c_str());
	size_t address_length = sizeof(address.sun_family) + strlen(address.sun_path) + 1;

	if (bind(socket_fd, (struct sockaddr *) &address, address_length) != 0) {
		LOG4CXX_ERROR(serverLogger, "bind() to `" << socketPath << "' failed: " << strerror(errno));
		return -1;
	}

	if (listen(socket_fd, 1) != 0) {
		LOG4CXX_ERROR(serverLogger, "listen() failed: " << strerror(errno));
		return -1;
	}

	return socket_fd;
}
