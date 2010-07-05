#include "Terms.h"
#include "Tokens.h"

#include <string>
#include <sstream>

using namespace std;

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
