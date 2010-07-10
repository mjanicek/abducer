#include "SliceToString.h"

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
		return "understand";
	case Generation:
		return "generate";
	case Truth:
		return "i";
	case Event:
		return "event";
	case Intention:
		return "int";
	case Attention:
		return "att";
	case Belief:
		return "bel";
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
