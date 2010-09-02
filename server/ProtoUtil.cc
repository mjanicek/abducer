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

#include "ProtoUtil.h"
#include <unistd.h>
#include <stdint.h>

#include <iostream>  // for debugging purposes
#include "common.h"
#include "Logging.h"

const size_t MSG_SIZE_LIMIT = (4096 * 1024);  // 4 MiB should be more than enough!

using namespace std;

ssize_t
read_all(int fd, char * buf, size_t num)
{
	size_t remaining = num;
	ssize_t nr;

	while (remaining > 0 && (nr = read(fd, buf + (num - remaining), remaining)) > 0) {
		remaining = remaining - nr;
	}

	return (remaining > 0 || nr <= 0) ? -1 : num;
}

string
readMessageFromFileDescriptor(int fd)
{
	uint32_t msg_len = 0;
	ssize_t nr;

	char * buf = 0;

	if ((nr = read_all(fd, (char *) &msg_len, sizeof(msg_len))) == sizeof(msg_len)) {
		debug(cerr << NOTIFY_MSG("read: the message appears to be " << msg_len << " bytes long") << endl);
		if (msg_len < MSG_SIZE_LIMIT) {
			if (msg_len > 0) {
				buf = new char[msg_len];
				if ((nr = read_all(fd, buf, (size_t)msg_len)) == (ssize_t)msg_len) {
					string result(buf, msg_len);
					delete buf;
					return result;
				}
				else {
					cerr << ERROR_MSG("read: failed to read the entire message (read " << nr
							<< " out of " << msg_len << " bytes)") << endl;
					delete buf;
				}
			}
			else {
				cerr << WARNING_MSG("read: zero length message received") << endl;
				return string();
			}
		}
		else {
			cerr << ERROR_MSG("read: message size limit exceeded") << endl;
		}
	}
	else {
		cerr << ERROR_MSG("read: unable to read the message size") << endl;
	}

	return string();
}

void
writeMessageToFileDescriptor(int fd, const std::string & msg)
{
	uint32_t msg_len = msg.length();
	ssize_t nw;
	
	debug(cerr << NOTIFY_MSG("write: pbuf'd message for output is " << msg_len << " bytes long") << endl);
	if ((nw = write(fd, &msg_len, sizeof(msg_len))) == sizeof(msg_len)) {
		if ((nw = write(fd, msg.data(), (size_t)msg_len)) != (ssize_t)msg_len) {
			throw "unable to write the message";
		}
	}
	else {
		throw "unable to write message size";
	}
}
