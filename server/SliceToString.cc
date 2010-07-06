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
	if (UnderstandingModalityPtr um = UnderstandingModalityPtr::dynamicCast(m)) {
		return "understand";
	}
	else if (GenerationModalityPtr gm = GenerationModalityPtr::dynamicCast(m)) {
		return "generate";
	}
	else if (EventModalityPtr em = EventModalityPtr::dynamicCast(m)) {
		return "event";
	}
	else if (IntentionModalityPtr tm = IntentionModalityPtr::dynamicCast(m)) {
		return "intention";
	}
	else if (InfoModalityPtr im = InfoModalityPtr::dynamicCast(m)) {
		return "i";
	}
	else if (AttStateModalityPtr am = AttStateModalityPtr::dynamicCast(m)) {
		return "att";
	}
	else if (KModalityPtr km = KModalityPtr::dynamicCast(m)) {
		string s("k(now, ");

		EpistemicStatusPtr e = km->epst;

		if (PrivateEpistemicStatusPtr pe = PrivateEpistemicStatusPtr::dynamicCast(e)) {
			s += "private(" + agentToString(pe->ag) + ")";
		}
		else if (AttributedEpistemicStatusPtr ae = AttributedEpistemicStatusPtr::dynamicCast(e)) {
			s += "attrib(" + agentToString(ae->ag) + "," + agentToString(ae->ag2) + ")";
		}
		else if (SharedEpistemicStatusPtr se = SharedEpistemicStatusPtr::dynamicCast(e)) {
			s += "mutual(h,r)";
		}

		s += ")";
		return s;
	}
	else {
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
