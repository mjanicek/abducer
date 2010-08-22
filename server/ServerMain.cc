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

const char * DEFAULT_SERVER_NAME = "AbducerServer";
const char * DEFAULT_SERVER_ENDPOINTS = "default -p 10000";
const char * DEFAULT_ABDUCER_PATH = "/usr/bin/false";

// this probably shouldn't be static
static Ice::CommunicatorPtr ic;
static int pipe_to_child[2];
static int pipe_from_child[2];

int
runServer(pid_t abducer_pid, const Settings & s);

void
shutdownServer(int);

void
printStatus(pid_t abducerPID, const Settings & s);

void
printUsage();

void
printVersion();

void
preparePlumbing(bool child);

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

			pipe(pipe_to_child);
			pipe(pipe_from_child);

			pid_t pchild;

			if ((pchild = fork()) == 0) {
				preparePlumbing(true);

				execlp(s.abducerPath.c_str(), s.abducerPath.c_str(), NULL);
				perror("Exec failed");
			}
			else {
				preparePlumbing(false);
				runServer(pchild, s);

				wait(0);
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
runServer(pid_t abducer_pid, const Settings & s)
{
	printStatus(abducer_pid, s);
	IceUtil::CtrlCHandler ctrlCHandler(shutdownServer);
	int status = 0;
	try {
		ic = Ice::initialize();

		cerr << SERVER_MSG("server name = \"" << s.serverName<< "\"") << endl;
		cerr << SERVER_MSG("server endpoints = \"" << s.serverEndpoints << "\"") << endl;

		Ice::ObjectAdapterPtr adapter
				= ic->createObjectAdapterWithEndpoints("AbducerAdapter", s.serverEndpoints);

		Ice::ObjectPtr object = new ForwardedAbducerServer(abducer_pid);
		adapter->add(object, ic->stringToIdentity(s.serverName));
		adapter->activate();

		ic->waitForShutdown();
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
}

void
printStatus(pid_t abducerPID, const Settings & s)
{
	const size_t cwd_length = 512;
	char * cwd = new char[512];
	getcwd(cwd, cwd_length);

	cerr << SERVER_MSG("using server interface revision " << tty::white << ABDUCER_ICE_VERSION << tty::dcol) << endl;
	cerr << SERVER_MSG("path to the abducer [" << s.abducerPath << "]") << endl;
	cerr << SERVER_MSG("abducer PID [" << abducerPID << "]") << endl;
	cerr << SERVER_MSG("abducer working directory [" << cwd << "]") << endl;

	delete cwd;
}

void
printUsage()
{
	cout << "abducer-server [OPTIONS] PATH_TO_ABDUCER" << endl << endl;
	cout << "OPTIONS may be the following:" << endl;
	cout << "  -n SERVER_NAME        Name of the ICE server" << endl;
	cout << "  -e SERVER_ENDPOINT    Endpoints of the ICE server" << endl;
	cout << "  -h                    Print (this) help" << endl;
}

void
printVersion()
{
	cerr << "Abducer server " << ABDUCER_VERSION << endl;
	cerr << "(c) 2009-2010 DFKI GmbH Talking Robots" << endl;
}

void
preparePlumbing(bool child)
{
	if (child) {
		close(STDOUT_FILENO);
		dup(pipe_from_child[PIPE_WRITE]);

		close(STDIN_FILENO);
		dup(pipe_to_child[PIPE_READ]);

		close(pipe_to_child[0]);
		close(pipe_to_child[1]);
		close(pipe_from_child[0]);
		close(pipe_from_child[1]);
	}
	else {
		close(STDOUT_FILENO);
		dup(pipe_to_child[PIPE_WRITE]);

		close(STDIN_FILENO);
		dup(pipe_from_child[PIPE_READ]);

		close(pipe_to_child[0]);
		close(pipe_to_child[1]);
		close(pipe_from_child[0]);
		close(pipe_from_child[1]);
	}
}
