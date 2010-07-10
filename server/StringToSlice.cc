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
parseAndAddModalitySeq(vector<Token *>::iterator & it, vector<Modality> & args)
{
	vector<Token *>::iterator orig = it;

	Modality arg = parseModality(it);
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
	vector<Modality> mod;

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

Abducer::Modality
parseModality(std::vector<Token *>::iterator & it)
{
	vector<Token *>::iterator orig = it;

	if ((*it)->type() == Atom) {
		AtomToken * atomTok = (AtomToken *) *it;
		if (atomTok->value() == string("i")) {
			it++;
			return Truth;
		}
		else if (atomTok->value() == string("event")) {
			it++;
			return Event;
		}
		else if (atomTok->value() == string("int")) {
			it++;
			return Intention;
		}
		else if (atomTok->value() == string("att")) {
			it++;
			return Attention;
		}
		else if (atomTok->value() == string("generate")) {
			it++;
			return Generation;
		}
		else if (atomTok->value() == string("understand")) {
			it++;
			return Understanding;
		}
		else if (atomTok->value() == string("bel")) {
			it++;
			return Belief;
		}
		else {
			it = orig;
			return Truth;  // FIXME: failure flag
		}
	}
	else {
		it = orig;
		return Truth;  // FIXME
	}
}
