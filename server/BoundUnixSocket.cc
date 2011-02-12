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

#include "BoundUnixSocket.h"

#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <sys/socket.h>
#include <sys/un.h>

using namespace std;

BoundUnixSocket::BoundUnixSocket(const string & dirprefix, const string & filename)
: fd(-1), dir(""), file(filename)
{
	// TODO sanity checks:
	//  - dirprefix nonempty
	//  - filename nonempty, doesn't contain "/"

	// create the directory
	string mkdtemp_template("/tmp/" + dirprefix + "-XXXXXX");
	char * buf = new char[mkdtemp_template.length() + 1];
	strcpy(buf, mkdtemp_template.c_str());
	buf = mkdtemp(buf);
	dir = string(buf);
	delete buf;

	string fullpath(dir + "/" + file);

	fd = socket(PF_UNIX, SOCK_STREAM, 0);
	if (fd < 0) {
		rmdir(dir.c_str());
		throw string("socket() failed: ") + strerror(errno);
	} 

	struct sockaddr_un address;
	address.sun_family = AF_UNIX;
	strcpy(address.sun_path, fullpath.c_str());
	size_t address_length = sizeof(address.sun_family) + strlen(address.sun_path) + 1;

	if (bind(fd, (struct sockaddr *) &address, address_length) != 0) {
		rmdir(dir.c_str());
		throw string("bind() to `") + fullpath + "' failed: " + strerror(errno);
	}

	if (listen(fd, 1) != 0) {
		unlink(fullpath.c_str());
		rmdir(dir.c_str());
		throw string("listen() failed: ") + strerror(errno);
	}

}

BoundUnixSocket::~BoundUnixSocket()
{
	unlink(getPath().c_str());
	rmdir(dir.c_str());  // TODO: make sure that this succeeded?
}
