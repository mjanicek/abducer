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

#ifndef FORKINGSERVER_H__
#define FORKINGSERVER_H__  1

#include "abducer.h"
#include <string>

#include <Ice/Ice.h>

#include <map>

namespace de {
namespace dfki {
namespace lt {
namespace tr {
namespace infer {
namespace abducer {

class ForkingServer : public engine::AbductionEngineServer {
public:
	ForkingServer(const std::vector<std::string> & engineArgV, int socket_fd);
	virtual ~ForkingServer();

	virtual engine::AbductionEnginePrx getEngineProxy(const std::string & name, const Ice::Current&);

protected:
	Ice::ObjectAdapterPtr startNewServer(const std::string & name);

	std::vector<std::string> engineArgV;
	int socket_fd;

	std::map<std::string, Ice::ObjectAdapterPtr> adapters;
	std::map<std::string, Ice::Identity> identities;
	std::map<std::string, Ice::CommunicatorPtr> communicators;

	int base_port;
};

}
}
}
}
}
}

#endif
