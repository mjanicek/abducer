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

#include "Terms.h"
#include "Tokens.h"

#include <string>
#include <sstream>

using namespace std;
using namespace Tokens;

//-------------------------------------------------------------------------

Term::Term()
: functor("?t")
{ }

string
Term::toMercuryString()
{
	stringstream ss;
	AtomToken funTok(functor);

	ss << funTok.toMercuryString();

	if (!args.empty()) {
		ss << "(";
		vector<Argument *>::iterator i = args.begin();
		while (i != args.end()) {
			ss << (*i)->toMercuryString();
			++i;
			if (i != args.end()) {
				ss << ", ";
			}
		}
		ss << ")";
	}
	return ss.str();
}

//-------------------------------------------------------------------------

Var::Var()
: name("?v")
{ }

string
Var::toMercuryString()
{
	return name;
}

//-------------------------------------------------------------------------

StringArgument::StringArgument()
: value("")
{ }

string
StringArgument::toMercuryString()
{
	StringToken strTok(value);
	return strTok.toMercuryString();
}

//-------------------------------------------------------------------------

FloatArgument::FloatArgument()
: value(0.0)
{ }

string
FloatArgument::toMercuryString()
{
	FloatToken floatTok(value);
	return floatTok.toMercuryString();
}

//-------------------------------------------------------------------------

Predicate::Predicate()
: predSym("?p")//, args()
{ }

string
Predicate::toMercuryString()
{
	stringstream ss;
	AtomToken symTok(predSym);

	ss << symTok.toMercuryString();

	if (!args.empty()) {
		ss << "(";
		vector<Argument *>::iterator i = args.begin();
		while (i != args.end()) {
			ss << (*i)->toMercuryString();
			++i;
			if (i != args.end()) {
				ss << ", ";
			}
		}
		ss << ")";
	}
	ss << ".";

	return ss.str();
}

//-------------------------------------------------------------------------
