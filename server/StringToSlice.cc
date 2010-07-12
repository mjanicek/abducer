#include "StringToSlice.h"

#include "Constants.h"

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

	Modality arg;
	if (parseModality(it, arg)) {
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

bool
parseModality(std::vector<Token *>::iterator & it, Abducer::Modality & mod)
{
	vector<Token *>::iterator orig = it;

	if ((*it)->type() == Atom) {
		AtomToken * atomTok = (AtomToken *) *it;
		if (atomTok->value() == string(TRUTH_STR)) {
			it++;
			mod = Truth;
			return true;
		}
		else if (atomTok->value() == string(EVENT_STR)) {
			it++;
			mod = Event;
			return true;
		}
		else if (atomTok->value() == string(INTENTION_STR)) {
			it++;
			mod = Intention;
			return true;
		}
		else if (atomTok->value() == string(ATTENTION_STR)) {
			it++;
			mod = Attention;
			return true;
		}
		else if (atomTok->value() == string(GENERATION_STR)) {
			it++;
			mod = Generation;
			return true;
		}
		else if (atomTok->value() == string(UNDERSTANDING_STR)) {
			it++;
			mod = Understanding;
			return true;
		}
		else if (atomTok->value() == string(BELIEF_STR)) {
			it++;
			mod = Belief;
			return true;
		}
		else {
			it = orig;
			return false;
		}
	}
	else {
		it = orig;
		return false;
	}
}
