#include "StringToSlice.h"

using namespace std;
using namespace Abducer;

void
parseAndAddTermSeq(vector<Token *>::iterator & it, vector<TermPtr> & args)
{
	vector<Token *>::iterator orig = it;

	TermPtr arg = parseTerm(it);
	if (arg) {
		args.push_back(arg);

		if ((*it)->type() == Comma) {
			it++;
			parseAndAddTermSeq(it, args);
		}
	}
	else {
		// error in the child, abort too
		it = orig;
	}
}

void
parseAndAddModalitySeq(vector<Token *>::iterator & it, vector<ModalityPtr> & args)
{
	vector<Token *>::iterator orig = it;

	ModalityPtr arg = parseModality(it);
	if (arg) {
		args.push_back(arg);

		if ((*it)->type() == Comma) {
			it++;
			parseAndAddModalitySeq(it, args);
		}
	}
	else {
		// error in the child, abort too
		it = orig;
	}
}

Abducer::TermPtr
parseTerm(vector<Token *>::iterator & it)
{
	vector<Token *>::iterator orig = it;

	if ((*it)->type() == VariableName) {
		VariableNameToken * varTok = (VariableNameToken *) *it;
		VariableTermPtr vt = new VariableTerm();
		vt->name = varTok->name();
		it++;
		return vt;
	}

	if ((*it)->type() == Atom) {
		AtomToken * atomTok = (AtomToken *) *it;
		FunctionTerm * ft = new FunctionTerm();
		ft->functor = atomTok->value();
		it++;
		if ((*it)->type() == OpenParenthesis) {
			it++;
			parseAndAddTermSeq(it, ft->args);

			if ((*it)->type() == CloseParenthesis) {
				// ok
				it++;
				return ft;
			}
			else {
				// closing parenthesis expected
				it = orig;
				return NULL;
			}
		}
		else {
			// no arguments apparently
			return ft;
		}
	}

	it = orig;
	return NULL;
}

Abducer::PredicatePtr
parsePredicate(vector<Token *>::iterator & it)
{
	vector<Token *>::iterator orig = it;
	PredicatePtr p = new Predicate();

	if ((*it)->type() == Atom) {
		AtomToken * psymTok = (AtomToken *) *it;
		p->predSym = psymTok->value();

		it++;
		if ((*it)->type() == OpenParenthesis) {
			it++;

			parseAndAddTermSeq(it, p->args);

			if ((*it)->type() == CloseParenthesis) {
				it++;
				if ((*it)->type() == Dot) {
					// ok
					it++;
					return p;
				}
				else {
					// syntax error, dot expected
					it = orig;
					return NULL;
				}
			}
			else {
				// closing parenthesis expected
				it = orig;
				return NULL;
			}
		}
		else if ((*it)->type() == Dot) {
			// ok
			it++;
			return p;
		}
		else {
			// syntax error, dot expected
			it = orig;
			return NULL;
		}
	}
	else {
		// syntax error, atom expected
		it = orig;
		return NULL;
	}
}

ModalisedFormulaPtr
parseModalisedFormula(vector<Token *>::iterator & it)
{
	vector<Token *>::iterator orig = it;

	ModalisedFormulaPtr mf = new ModalisedFormula();
	vector<ModalityPtr> mod;

	if ((*it)->type() == OpenCurlyBracket) {
		it++;

		parseAndAddModalitySeq(it, mod);

		if ((*it)->type() == CloseCurlyBracket) {
			// ok
			it++;
			mf->m = mod;
		}
		else {
			it = orig;
			return NULL;
		}
	}

	PredicatePtr p = parsePredicate(it);

	if (p) {
		mf->p = p;
		return mf;
	}
	else {
		it = orig;
		return NULL;
	}
}

Abducer::Agent
tokenToAgent(const Token * tok)
{
//	cerr << "tokenToAgent: \"" << tok->toMercuryString() << "\"" << endl;
	if (tok->type() == Atom) {
		AtomToken * atok = (AtomToken *) tok;
		if (atok->value() == "h") {
//			cerr << "  human" << endl;
			return human;
		}
		else {
//			cerr << "  robot" << endl;
			return robot;
		}
	}
	else {
//		cerr << "not an atom!! type" << tok->type() << endl;
		return human;
	}
}

Abducer::ModalityPtr
parseModality(std::vector<Token *>::iterator & it)
{
	vector<Token *>::iterator orig = it;

	if ((*it)->type() == Atom) {
		AtomToken * atomTok = (AtomToken *) *it;
		if (atomTok->value() == string("i")) {
			it++;
			InfoModalityPtr im = new InfoModality();
			return im;
		}
		else if (atomTok->value() == string("event")) {
			it++;
			EventModalityPtr em = new EventModality();
			return em;
		}
		else if (atomTok->value() == string("intention")) {
			it++;
			IntentionModalityPtr im = new IntentionModality();
			return im;
		}
		else if (atomTok->value() == string("att")) {
			it++;
			AttStateModalityPtr am = new AttStateModality();
			return am;
		}
		else if (atomTok->value() == string("generate")) {
			it++;
			GenerationModalityPtr gm = new GenerationModality();
			return gm;
		}
		else if (atomTok->value() == string("understand")) {
			it++;
			UnderstandingModalityPtr um = new UnderstandingModality();
			return um;
		}
		else if (atomTok->value() == string("k")) {
			it++;
			KModalityPtr km = new KModality();
			if ((*it)->type() == OpenParenthesis) {
				it++;
				it++;  // skip the "now" atom
				it++;  // skip the comma
				if ((*it)->type() == Atom) {
					AtomToken * shareTok = (AtomToken *) *it;
//					cerr << (*it)->toMercuryString() << endl;
					it++;
//					cerr << (*it)->toMercuryString() << endl;
					it++;  // skip '('
//					cerr << (*it)->toMercuryString() << endl;

					if (shareTok->value() == "private") {
//						cerr << "private" << endl;
						km->share = Private;
						km->ag = tokenToAgent(*it);
						km->ag2 = km->ag;
						it++;
						it++;  // skip ')'
					}
					else if (shareTok->value() == "attrib") {
//						cerr << "attrib" << endl;
						km->share = Attribute;
						km->ag = tokenToAgent(*it);
						it++;
						it++;  // skip ','
						km->ag2 = tokenToAgent(*it);
						it++;
						it++;  // skip ')'
					}
					else if (shareTok->value() == "mutual") {
//						cerr << "mutual" << endl;
						km->share = Mutual;
						km->ag = tokenToAgent(*it);
						it++;
						it++;  // skip ','
						km->ag2 = tokenToAgent(*it);
						it++;
						it++;  // skip ')'
					}

					if ((*it)->type() == CloseParenthesis) {
						it++;
						return km;
					}
					else {
						it = orig;
						return NULL;
					}
				}
				else {
					it = orig;
					return NULL;
				}
			}
			else {
				it = orig;
				return NULL;
			}
		}
		else {
			it = orig;
			return NULL;
		}
	}
	else {
		it = orig;
		return NULL;
	}
}
