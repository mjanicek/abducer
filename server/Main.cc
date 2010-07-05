#include <vector>
#include <string>
#include <iostream>
#include <iterator>

#include "Token.h"
#include "TermTokeniser.h"
#include "TermParsing.h"

using namespace std;

int
main(int argc, void ** argv)
{
	//string s = "some_term(\"x\", 0.5, with_args(x, y), Vars, 'escpd'(u)).";
	string s = "some_term(X, \"x\", \"y\", 1.04, t, t(u)).";

	cout << s << endl;

	vector<Token *> toks = tokenise(s);
	vector<Token *>::iterator i = toks.begin();

	for ( ; i != toks.end(); ++i) {
		cout << (*i)->toMercuryString();
	}
	cout << endl;

	vector<Token *>::iterator pi = toks.begin();
	Predicate * p = parsePredicate(pi);

	if (p != NULL) {
		cout << p->toMercuryString() << endl;
	}

	return 0;
}
