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

#include "Tokens.h"

#include <sstream>

using namespace std;
using namespace Tokens;

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
