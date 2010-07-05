#include "TermTokeniser.h"

#include <iostream>
#include <cstdlib>
#include <cctype>

using namespace std;

enum TokeniserState {
	TSUndecided,
	TSWhite,
	TSVar,
	TSAtom,
	TSVerbatimAtom,
	TSFloat,
	TSString,
	TSComma,
	TSDot,
	TSOpenParenth,
	TSCloseParenth,
	TSOpenBracket,
	TSCloseBracket,
	TSOpenCurly,
	TSCloseCurly
};

// true iff ch is alphanumeric or an underscore
inline int isundalnum(int ch)
{
	return (isalnum(ch) || ch == '_');
}

inline int escaped(int ch)
{
	switch (ch) {
		case 'n':
			return '\n';
			break;
		default:
			return ch;
			break;
	}
}

vector<Token *>
tokenise(const string & s)
{
	vector<Token *> v;

	const char * c_str = s.c_str();
	TokeniserState state = TSUndecided;

	// a finite state machine
	while (*c_str) {

		switch (state) {

			// *c_str is a white char
			case TSWhite:
				while (*c_str && isspace(*c_str)) {
					c_str++;
				}
				state = TSUndecided;
				break;

			case TSComma:
				c_str++;
				v.push_back(new CommaToken());
				state = TSUndecided;
				break;

			case TSDot:
				c_str++;
				v.push_back(new DotToken());
				state = TSUndecided;
				break;

			case TSOpenParenth:
				c_str++;
				v.push_back(new OpenParenthesisToken());
				state = TSUndecided;
				break;

			case TSCloseParenth:
				c_str++;
				v.push_back(new CloseParenthesisToken());
				state = TSUndecided;
				break;

			case TSOpenBracket:
				c_str++;
				v.push_back(new OpenBracketToken());
				state = TSUndecided;
				break;

			case TSCloseBracket:
				c_str++;
				v.push_back(new CloseBracketToken());
				state = TSUndecided;
				break;

			case TSOpenCurly:
				c_str++;
				v.push_back(new OpenCurlyBracketToken());
				state = TSUndecided;
				break;

			case TSCloseCurly:
				c_str++;
				v.push_back(new CloseCurlyBracketToken());
				state = TSUndecided;
				break;

			case TSVar: {
					string tmp = string("");
					while (*c_str && isundalnum(*c_str)) {
						tmp += *c_str;
						c_str++;
					}
					VariableNameToken * tok = new VariableNameToken(tmp);
					v.push_back(tok);
				}
				state = TSUndecided;
				break;

			case TSAtom: {
					string tmp = string("");
					while (*c_str && isundalnum(*c_str)) {
						tmp += *c_str;
						c_str++;
					}
					AtomToken * tok = new AtomToken(tmp);
					v.push_back(tok);
				}
				state = TSUndecided;
				break;

			case TSVerbatimAtom: {
					string tmp = string("");
					bool finished = false;
					while (*c_str && !finished) {
						if (*c_str == '\'') {
							finished = true;
						}
						else {
							tmp += *c_str;
						}
						c_str++;
					}
					AtomToken * tok = new AtomToken(tmp);
					v.push_back(tok);
				}
				state = TSUndecided;
				break;

			case TSFloat: {
					string tmp = string("");
					while (*c_str && isdigit(*c_str)) {
						tmp += *c_str;
						c_str++;
					}
					if (*c_str == '.' && isdigit(*(c_str+1))) {
						tmp += *c_str;
						c_str++;
						while (*c_str && isdigit(*c_str)) {
							tmp += *c_str;
							c_str++;
						}
						FloatToken * tok = new FloatToken(atof(tmp.c_str()));
						v.push_back(tok);
					}
					else {
						// FIXME: an integer, not a float!
						FloatToken * tok = new FloatToken(atoi(tmp.c_str()));
						v.push_back(tok);
					}
				}
				state = TSUndecided;
				break;

			case TSString: {
					string tmp = string("");
					bool finished = false;
					bool escape = false;
					while (*c_str && !finished) {
						if (escape) {
							tmp += escaped(*c_str);
							escape = false;
						}
						else if (*c_str == '\\') {
							escape = true;
						}
						else if (*c_str == '"') {
							finished = true;
						}
						else {
							tmp += *c_str;
						}
						c_str++;
					}
					StringToken * tok = new StringToken(tmp);
					v.push_back(tok);
				}
				state = TSUndecided;
				break;

			// look at the first char and decide its type
			case TSUndecided:
				if (isspace(*c_str)) {
					//cerr << "white" << endl;
					state = TSWhite;
				}
				else if (*c_str == ',') {
					//cerr << "comma" << endl;
					state = TSComma;
				}
				else if (*c_str == '.') {
					//cerr << "dot" << endl;
					state = TSDot;
				}
				else if (*c_str == '(') {
					//cerr << "open parenth" << endl;
					state = TSOpenParenth;
				}
				else if (*c_str == ')') {
					//cerr << "close parenth" << endl;
					state = TSCloseParenth;
				}
				else if (*c_str == '[') {
					//cerr << "open bracket" << endl;
					state = TSOpenBracket;
				}
				else if (*c_str == ']') {
					//cerr << "close bracket" << endl;
					state = TSCloseBracket;
				}
				else if (*c_str == '{') {
					//cerr << "open bracket" << endl;
					state = TSOpenCurly;
				}
				else if (*c_str == '}') {
					//cerr << "close bracket" << endl;
					state = TSCloseCurly;
				}
				else if (isdigit(*c_str)) {
					//cerr << "float" << endl;
					state = TSFloat;
				}
				else if (isupper(*c_str)) {
					//cerr << "var" << endl;
					state = TSVar;
				}
				else if (islower(*c_str)) {
					//cerr << "atom" << endl;
					state = TSAtom;
				}
				else if (*c_str == '\'') {
					//cerr << "verbatim atom" << endl;
					c_str++;  // skip the opening apostrophe
					state = TSVerbatimAtom;
				}
				else if (*c_str == '"') {
					//cerr << "string" << endl;
					c_str++;  // skip the opening quotation mark
					state = TSString;
				}
				else {
					c_str++;
				}
				break;

			default:
				// unimplemented
				c_str++;
				break;
		}
	}

	return v;
}
