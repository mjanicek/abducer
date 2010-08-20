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

#include "Logging.h"

using namespace std;

#define PIPE_READ   0
#define PIPE_WRITE  1

const char * SERVER_NAME = "AbducerServer";
const char * SERVER_ENDPOINTS = "default -p 10000";

// this probably shouldn't be static
static Ice::CommunicatorPtr ic;
static int pipe_to_child[2];
static int pipe_from_child[2];

static const char * abducer_path = "/usr/bin/false";

int
runServer(pid_t abducer_pid);

void
shutdownServer(int);

void
printUsage();

void
preparePlumbing(bool child);

int
main(int argc, char ** argv)
{
	if (argc != 2) {
		cerr << "Usage error" << endl;
		return EXIT_FAILURE;
	}
	abducer_path = (const char *) argv[1];

	pipe(pipe_to_child);
	pipe(pipe_from_child);

	pid_t pchild;

	if ((pchild = fork()) == 0) {
		preparePlumbing(true);

		execlp(abducer_path, abducer_path, NULL);
		perror("Exec failed");
	}
	else {
		preparePlumbing(false);
		runServer(pchild);

		wait(0);
	}

	return EXIT_SUCCESS;
}

int
runServer(pid_t abducer_pid)
{
	printUsage();
	IceUtil::CtrlCHandler ctrlCHandler(shutdownServer);
	int status = 0;
	try {
		ic = Ice::initialize();

		cerr << SERVER_MSG("server name = \"" << SERVER_NAME << "\"") << endl;
		cerr << SERVER_MSG("server endpoints = \"" << SERVER_ENDPOINTS << "\"") << endl;

		Ice::ObjectAdapterPtr adapter
				= ic->createObjectAdapterWithEndpoints("AbducerAdapter", SERVER_ENDPOINTS);

		Ice::ObjectPtr object = new ForwardedAbducerServer(abducer_pid);
		adapter->add(object, ic->stringToIdentity(SERVER_NAME));
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
printUsage()
{
	const size_t cwd_length = 512;
	char * cwd = new char[512];
	getcwd(cwd, cwd_length);

	cerr << SERVER_MSG("using server interface revision " << tty::white << ABDUCER_ICE_VERSION << tty::dcol) << endl;
	cerr << SERVER_MSG("path to the abducer [" << abducer_path << "]") << endl;
	cerr << SERVER_MSG("abducer working directory [" << cwd << "]") << endl;

	delete cwd;
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
