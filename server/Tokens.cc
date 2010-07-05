#include "Tokens.h"

#include <sstream>

using namespace std;

//-------------------------------------------------------------------------

string CommaToken::toMercuryString() const
{
	return string(",");
}

TokenType CommaToken::type() const
{
	return Comma;
}

//-------------------------------------------------------------------------

string DotToken::toMercuryString() const
{
	return string(".");
}

TokenType DotToken::type() const
{
	return Dot;
}

//-------------------------------------------------------------------------

string OpenParenthesisToken::toMercuryString() const
{
	return string("(");
}

TokenType OpenParenthesisToken::type() const
{
	return OpenParenthesis;
}

//-------------------------------------------------------------------------

string CloseParenthesisToken::toMercuryString() const
{
	return string(")");
}

TokenType CloseParenthesisToken::type() const
{
	return CloseParenthesis;
}

//-------------------------------------------------------------------------

string OpenBracketToken::toMercuryString() const
{
	return string("[");
}

TokenType OpenBracketToken::type() const
{
	return OpenBracket;
}

//-------------------------------------------------------------------------

string CloseBracketToken::toMercuryString() const
{
	return string("]");
}

TokenType CloseBracketToken::type() const
{
	return CloseBracket;
}

//-------------------------------------------------------------------------

string OpenCurlyBracketToken::toMercuryString() const
{
	return string("{");
}

TokenType OpenCurlyBracketToken::type() const
{
	return OpenCurlyBracket;
}

//-------------------------------------------------------------------------

string CloseCurlyBracketToken::toMercuryString() const
{
	return string("}");
}

TokenType CloseCurlyBracketToken::type() const
{
	return CloseCurlyBracket;
}

//-------------------------------------------------------------------------

VariableNameToken::VariableNameToken(const string & s)
: nameValue(s)
{ }

string VariableNameToken::toMercuryString() const
{
	return nameValue;
}

TokenType VariableNameToken::type() const
{
	return VariableName;
}

string VariableNameToken::name() const
{
	return nameValue;
}

//-------------------------------------------------------------------------

AtomToken::AtomToken(const string & s)
: atomValue(s)
{ }

string AtomToken::toMercuryString() const
{
	return atomValue;
}

TokenType AtomToken::type() const
{
	return Atom;
}

string AtomToken::value() const
{
	return atomValue;
}

//-------------------------------------------------------------------------

FloatToken::FloatToken(double f)
: floatValue(f)
{ }


string FloatToken::toMercuryString() const
{
	stringstream ss;
	ss << floatValue;
	return ss.str();
}

TokenType FloatToken::type() const
{
	return Float;
}

double FloatToken::value() const
{
	return floatValue;
}

//-------------------------------------------------------------------------

StringToken::StringToken(const string & s)
: stringValue(s)
{ }

// FIXME: escapes!
string StringToken::toMercuryString() const
{
	return string("\"" + stringValue + "\"");
}

TokenType StringToken::type() const
{
	return String;
}

string StringToken::value() const
{
	return stringValue;
}
