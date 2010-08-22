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

#include "CLI.h"

extern "C" {
#include <unistd.h>
#include <getopt.h>
}

CommandLineAction
processCommandLineArgs(int argc, char ** argv, Settings & setup)
{
	bool help = false;

	static struct option longOptions[] = {
			{"help",       no_argument,       0, 'h'},
//			{"verbose",    no_argument,       0, 'v'},
			{"name",       required_argument, 0, 'n'},
			{"endpoints",  required_argument, 0, 'e'},
			{"abducer",    required_argument, 0, 'a'},
			{0, 0, 0, 0}
		};

	while (1) {
		int c;
		int idx = 0;

		c = getopt_long(argc, argv, "hn:e:a:", longOptions, &idx);
		if (c == -1) {
			break;
		}

		switch (c) {
		case 0:
			// flags
			break;

		case 'h':
			help = true;
			break;

//		case 'v':
//			isVerbose = true;
//			break;

		case 'n':
			setup.serverName = optarg;
			break;

		case 'e':
			setup.serverEndpoints = optarg;
			break;

		case 'a':
			setup.serverEndpoints = optarg;
			break;

		case '?':
			return Error;

		default:
			return Error;
		}
	}

	return (help ? PrintHelp : Start);
}