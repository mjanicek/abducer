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

	switch (t->type) {
		case Function: {
				FunctionTermPtr ft = FunctionTermPtr::dynamicCast(t);
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
			break;

		case Variable: {
				VariableTermPtr vt = VariableTermPtr::dynamicCast(t);
				s = vt->name;
				return s;
			}
			break;
	}

	return string("");
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
agentToString(Agent a)
{
	switch (a) {
		case human:
			return "h";
		case robot:
			return "r";
		default:
			return "?";
	}
}

string
modalityToString(const ModalityPtr & m)
{
	switch (m->type) {
		case Understanding:
			return "understand";
		case Generation:
			return "generate";
		case Event:
			return "event";
		case Intention:
			return "intention";
		case Info:
			return "i";
		case AttState:
			return "att";
		case K: {
				KModalityPtr km = KModalityPtr::dynamicCast(m);
				string s("k(now, ");

				switch (km->share) {
					case Private:
						s += "private(" + agentToString(km->ag) + ")";
						break;

					case Attribute:
						s += "attrib(" + agentToString(km->ag) + "," + agentToString(km->ag2) + ")";
						break;

					case Mutual:
						s += "mutual(h,r)";
						break;
				}
				s += ")";
				return s;
			}
			break;

		default:
			return "?";
	}
}

string
modalisedFormulaToString(const ModalisedFormulaPtr & mf)
{
	string s("");
	if (!mf->m.empty()) {
		vector<ModalityPtr>::iterator it = mf->m.begin();
		for ( ; it != mf->m.end() ; it++) {
			s += modalityToString(*it) + ": ";
		}
	}
	s += predicateToString(mf->p);
	return s;
}
