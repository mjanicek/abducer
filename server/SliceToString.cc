// ----------------------------------------------------------------------------
// Copyright (C) 2009-2011 DFKI GmbH Talking Robots 
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

#include "SliceToString.h"

#include "Constants.h"

using namespace std;

namespace de {
namespace dfki {
namespace lt {
namespace tr {
namespace infer {
namespace abducer {

string
quoteGuard(string s)
{
	return string("'" + s + "'");
}

string
termToString(const lang::TermPtr & t)
{
	string s("");

	if (lang::FunctionTermPtr ft = lang::FunctionTermPtr::dynamicCast(t)) {
		s = quoteGuard(ft->functor);
		if (!ft->args.empty()) {
			s += "(";
			vector<lang::TermPtr>::iterator it = ft->args.begin();
			while (it != ft->args.end()) {
				s += termToString(*it);
				it++;
				if (it != ft->args.end()) {
					s += ", ";
				}
			}
			s += ")";
		}
		return s;
	}
	else if (lang::VariableTermPtr vt = lang::VariableTermPtr::dynamicCast(t)) {
		s = vt->name;
		return s;
	}
	else {
		return string("");
	}
}

string
atomToString(const lang::AtomPtr & a)
{
	string s("");

	s = quoteGuard(a->predSym);
	if (!a->args.empty()) {
		s += "(";
		vector<lang::TermPtr>::iterator it = a->args.begin();
		while (it != a->args.end()) {
			s += termToString(*it);
			it++;
			if (it != a->args.end()) {
				s += ", ";
			}
		}
		s += ")";
	}
	return s;
}

string
modalityToString(const lang::Modality m)
{
	switch (m) {
	case lang::Understanding:
		return UNDERSTANDING_STR;
	case lang::Generation:
		return GENERATION_STR;
	case lang::Truth:
		return TRUTH_STR;
	case lang::Event:
		return EVENT_STR;
	case lang::Intention:
		return INTENTION_STR;
	case lang::Attention:
		return ATTENTION_STR;
	case lang::Belief:
		return BELIEF_STR;
	default:
		return "unknown";
	}
}

string
modalisedAtomToString(const lang::ModalisedAtomPtr & ma)
{
	string s("");
	if (!ma->m.empty()) {
		vector<lang::Modality>::iterator it = ma->m.begin();
		for ( ; it != ma->m.end() ; it++) {
			s += modalityToString(*it) + ": ";
		}
	}
	s += atomToString(ma->a);
	return s;
}

}
}
}
}
}
}
