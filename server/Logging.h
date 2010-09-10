#ifndef LOGGING_H__
#define LOGGING_H__  1

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

#include <iostream>
#include "TtyUtils.h"

#define SERVER_MSG(arg)  tty::yellow << "[" << THIS << "]" << tty::dcol << " " << arg
#define REQUEST_MSG(arg)  tty::magenta << "[" << THIS << "]" << tty::dcol << " " << arg
#define WARNING_MSG(arg)  tty::red << "[" << THIS << "]" << tty::dcol << " " << arg
#define ERROR_MSG(arg)  tty::red << "[" << THIS << "] " << arg << tty::dcol
#define NOTIFY_MSG(arg)  tty::green << "[" << THIS << "]" << tty::dcol << " " << arg

#endif
