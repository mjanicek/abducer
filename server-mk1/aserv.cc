#include "common.h"

extern "C" {
#include "mercury_imp.h"
#include "MercuryAbducerServer_mint.mh"
}

#include <Ice/Ice.h>
#include <IceUtil/CtrlCHandler.h>
#include <Abducer.h>
#include <aserv.h>

#include <vector>

#include "TypeConversions.h"
#include "MercuryAbducerServer.h"
#include "TtyUtils.h"

using namespace std;

const char * SERVER_NAME = "AbducerServer";
const char * SERVER_ENDPOINTS = "default -p 10000";

// this probably shouldn't be static
static Ice::CommunicatorPtr ic;

void
shutdownServer(int);

void
printUsage();

int
aserv_main()
{
	printUsage();
	//IceUtil::CtrlCHandler ctrlCHandler(shutdownServer);
	int status = 0;
	try {
		ic = Ice::initialize();

		cout << tty::yellow << "* server name = \"" << SERVER_NAME << "\"" << tty::dcol << endl;
		cout << tty::yellow << "* server endpoints = \"" << SERVER_ENDPOINTS << "\"" << tty::dcol << endl;

		Ice::ObjectAdapterPtr adapter
				= ic->createObjectAdapterWithEndpoints("AbducerAdapter", SERVER_ENDPOINTS);

		Ice::ObjectPtr object = new MercuryAbducerServer();
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

	cout << tty::yellow << "* server shut down" << tty::dcol << endl;

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
	cout << endl;
	cout << tty::yellow << "* received signal " << signum << tty::dcol << endl;
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
	cout << "* using server interface revision " << tty::white << ABDUCER_ICE_VERSION << tty::dcol << endl;
}
