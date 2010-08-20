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

/*
 * TODO: check for memory leaks
 */

#include "TermParsing.h"

#include "Tokens.h"
#include "Terms.h"

using namespace std;


void
parseAndAddArgumentSeq(vector<Token *>::iterator & it, vector<Argument *> & args)
{
	vector<Token *>::iterator orig = it;

	Argument * arg = parseArgument(it);
	if (arg != NULL) {
		args.push_back(arg);

		if ((*it)->type() == Comma) {
			it++;
			parseAndAddArgumentSeq(it, args);
		}
	}
	else {
		// error in the child, abort too
		it = orig;
	}
}

Argument *
parseArgument(vector<Token *>::iterator & it)
{
	vector<Token *>::iterator orig = it;

	if ((*it)->type() == String) {
		StringToken * strTok = (StringToken *) *it;
		StringArgument * sarg = new StringArgument();
		sarg->value = strTok->value();
		it++;
		return sarg;
	}

	if ((*it)->type() == Float) {
		FloatToken * floatTok = (FloatToken *) *it;
		FloatArgument * farg = new FloatArgument();
		farg->value = floatTok->value();
		it++;
		return farg;
	}

	if ((*it)->type() == VariableName) {
		VariableNameToken * varTok = (VariableNameToken *) *it;
		Var * varg = new Var();
		varg->name = varTok->name();
		it++;
		return varg;
	}

	if ((*it)->type() == Atom) {
		AtomToken * atomTok = (AtomToken *) *it;
		Term * targ = new Term();
		targ->functor = atomTok->value();
		it++;
		if ((*it)->type() == OpenParenthesis) {
			it++;
			parseAndAddArgumentSeq(it, targ->args);

			if ((*it)->type() == CloseParenthesis) {
				// ok
				it++;
				return targ;
			}
			else {
				// closing parenthesis expected
				it = orig;
				return NULL;
			}
		}
		else {
			// no arguments apparently
			return targ;
		}
	}

	it = orig;
	return NULL;
}

Predicate *
parsePredicate(vector<Token *>::iterator & it)
{
	vector<Token *>::iterator orig = it;
	Predicate * p = new Predicate();

	if ((*it)->type() == Atom) {
		AtomToken * psymTok = (AtomToken *) *it;
		p->predSym = psymTok->value();

		it++;
		if ((*it)->type() == OpenParenthesis) {
			it++;

			parseAndAddArgumentSeq(it, p->args);

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
