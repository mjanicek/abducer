// ----------------------------------------------------------------------------
// Copyright (C) 2011 DFKI GmbH Talking Robots 
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

#ifndef CLI_H__
#define CLI_H__  1

#include <string>
#include <vector>

enum CommandLineAction {
	Error,
	PrintHelp,
	PrintVersion,
	Start
};

struct Settings {
	std::string serverName;
	std::string serverEndpoints;
	std::string abducerPath;
	std::vector<std::string> abducerArgs;
	std::string logConfigPath;
};

CommandLineAction
processCommandLineArgs(int argc, char ** argv, Settings & s);

#endif
