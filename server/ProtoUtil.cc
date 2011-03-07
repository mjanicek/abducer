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

#include "ProtoUtil.h"
#include <unistd.h>
#include <stdint.h>
#include <errno.h>
#include <string.h>

#include <iostream>  // for debugging purposes
#include "common.h"

const size_t MSG_SIZE_LIMIT = (4096 * 1024);  // 4 MiB should be more than enough!

using namespace std;
using namespace log4cxx;

ssize_t
read_all(int fd, char * buf, size_t num)
{
	size_t remaining = num;
	ssize_t nr;

	while (remaining > 0 && (nr = read(fd, buf + (num - remaining), remaining)) > 0) {
		remaining = remaining - nr;
	}

	return (remaining > 0 || nr <= 0) ? nr : num;
}

string
readMessageFromFileDescriptor(LoggerPtr logger, int fd)
{
	uint32_t msg_len = 0;
	ssize_t nr;

	char * buf = 0;

	if ((nr = read_all(fd, (char *) &msg_len, sizeof(msg_len))) == sizeof(msg_len)) {
		LOG4CXX_TRACE(logger, "read: the message appears to be " << msg_len << " bytes long");
		if (msg_len < MSG_SIZE_LIMIT) {
			if (msg_len > 0) {
				buf = new char[msg_len];
				if ((nr = read_all(fd, buf, (size_t)msg_len)) == (ssize_t)msg_len) {
					string result(buf, msg_len);
					delete buf;
					return result;
				}
				else {
					LOG4CXX_ERROR(logger, "read: failed to read the entire message (read " << nr
							<< " out of " << msg_len << " bytes)");
					delete buf;
				}
			}
			else {
				LOG4CXX_WARN(logger, "read: zero length message received");
				return string();
			}
		}
		else {
			LOG4CXX_ERROR(logger, "read: message size limit exceeded");
		}
	}
	else {
		LOG4CXX_ERROR(logger, "read: unable to read the message size: read() = " << nr
				<< ": " << strerror(errno));
	}

	return string();
}

void
writeMessageToFileDescriptor(LoggerPtr logger, int fd, const std::string & msg)
{
	uint32_t msg_len = msg.length();
	ssize_t nw;
	
	LOG4CXX_TRACE(logger, "write: pbuf'd message for output is " << msg_len << " bytes long");
	if ((nw = write(fd, &msg_len, sizeof(msg_len))) == sizeof(msg_len)) {
		if ((nw = write(fd, msg.data(), (size_t)msg_len)) != (ssize_t)msg_len) {
			throw "unable to write the message";
		}
		else {
			LOG4CXX_TRACE(logger, "wrote " << nw << " bytes");
//			for (int i = 0; i < nw; i++) {
//				cerr << (unsigned int)((unsigned char)msg.data()[i]) << " ";
//			}
//			cerr << endl;
		}
	}
	else {
		throw "unable to write message size";
	}
}
