#ifndef FORKINGSERVER_H__
#define FORKINGSERVER_H__  1

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

#include "weigabd.h"
#include <string>

#include <Ice/Ice.h>

#include <map>

namespace Abducer = ::de::dfki::lt::tr::infer::weigabd::slice;

class ForkingServer : public Abducer::AbductionEngineServer {
public:
	ForkingServer(const std::string & enginePath, const std::string & socketPath, int socket_fd);
	virtual Abducer::AbductionEnginePrx getEngineProxy(const std::string & name, const Ice::Current&);
protected:
	void startNewServer(const std::string & name);

	std::string enginePath;
	std::string socketPath;
	int socket_fd;

	std::map<std::string, Ice::ObjectAdapterPtr> engines;
};

#endif
