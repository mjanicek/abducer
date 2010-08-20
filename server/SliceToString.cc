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

#include "SliceToString.h"

#include "Constants.h"

using namespace std;
using namespace Abducer;

string
quoteGuard(string s)
{
	return string("'" + s + "'");
}

string
termToString(const TermPtr & t)
{
	string s("");

	if (FunctionTermPtr ft = FunctionTermPtr::dynamicCast(t)) {
		s = quoteGuard(ft->functor);
		if (!ft->args.empty()) {
			s += "(";
			vector<TermPtr>::iterator it = ft->args.begin();
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
	else if (VariableTermPtr vt = VariableTermPtr::dynamicCast(t)) {
		s = vt->name;
		return s;
	}
	else {
		return string("");
	}
}

string
predicateToString(const PredicatePtr & p)
{
	string s("");

	s = quoteGuard(p->predSym);
	if (!p->args.empty()) {
		s += "(";
		vector<TermPtr>::iterator it = p->args.begin();
		while (it != p->args.end()) {
			s += termToString(*it);
			it++;
			if (it != p->args.end()) {
				s += ", ";
			}
		}
		s += ")";
	}
	return s;
}

string
modalityToString(const Modality m)
{
	switch (m) {
	case Understanding:
		return UNDERSTANDING_STR;
	case Generation:
		return GENERATION_STR;
	case Truth:
		return TRUTH_STR;
	case Event:
		return EVENT_STR;
	case Intention:
		return INTENTION_STR;
	case Attention:
		return ATTENTION_STR;
	case Belief:
		return BELIEF_STR;
	default:
		return "unknown";
	}
}

string
modalisedFormulaToString(const ModalisedFormulaPtr & mf)
{
	string s("");
	if (!mf->m.empty()) {
		vector<Modality>::iterator it = mf->m.begin();
		for ( ; it != mf->m.end() ; it++) {
			s += modalityToString(*it) + ": ";
		}
	}
	s += predicateToString(mf->p);
	return s;
}
