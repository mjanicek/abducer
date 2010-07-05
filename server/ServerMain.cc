#include <unistd.h>
#include <sys/wait.h>
#include <sys/types.h>

#include <Ice/Ice.h>
#include <IceUtil/CtrlCHandler.h>
#include "wabd.h"

#include <iostream>

#include "ForwardedAbducerServer.h"
#include "TtyUtils.h"

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
runServer();

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
		runServer();

		wait(0);
	}

	return EXIT_SUCCESS;
}

int
runServer()
{
	printUsage();
	IceUtil::CtrlCHandler ctrlCHandler(shutdownServer);
	int status = 0;
	try {
		ic = Ice::initialize();

		cerr << tty::yellow << "* server name = \"" << SERVER_NAME << "\"" << tty::dcol << endl;
		cerr << tty::yellow << "* server endpoints = \"" << SERVER_ENDPOINTS << "\"" << tty::dcol << endl;

		Ice::ObjectAdapterPtr adapter
				= ic->createObjectAdapterWithEndpoints("AbducerAdapter", SERVER_ENDPOINTS);

		Ice::ObjectPtr object = new ForwardedAbducerServer();
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

	cerr << tty::yellow << "* server shut down" << tty::dcol << endl;

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
	cerr << tty::yellow << "* received signal " << signum << tty::dcol << endl;
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

	cerr << "* using server interface revision " << tty::white << ABDUCER_ICE_VERSION << tty::dcol << endl;
	cerr << tty::yellow << "* path to the abducer: " << abducer_path << tty::dcol << endl;
	cerr << tty::yellow << "* abducer working directory: " << cwd << tty::dcol << endl;

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
