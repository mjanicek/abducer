#ifndef TOKENS_H__
#define TOKENS_H__  1

#include <string>

enum TokenType {
	Comma,
	Dot,
	OpenParenthesis,
	CloseParenthesis,
	OpenBracket,
	CloseBracket,
	OpenCurlyBracket,
	CloseCurlyBracket,
	VariableName,
	Atom,
	Float,
	String
};

/*
 * Base class for tokens.
 */
class Token {
public:
	virtual std::string toMercuryString() const = 0;
	virtual TokenType type() const = 0;
};

// ','
class CommaToken : public Token {
public:
	//CommaToken();
	virtual std::string toMercuryString() const;
	virtual TokenType type() const;
};

// '.'
class DotToken : public Token {
public:
	//DotToken();
	virtual std::string toMercuryString() const;
	virtual TokenType type() const;
};

// '('
class OpenParenthesisToken : public Token {
public:
	//OpenParenthesisToken();
	virtual std::string toMercuryString() const;
	virtual TokenType type() const;
};

// ')'
class CloseParenthesisToken : public Token {
public:
	//CloseParenthesisToken();
	virtual std::string toMercuryString() const;
	virtual TokenType type() const;
};

// '['
class OpenBracketToken : public Token {
public:
	//OpenBracketToken();
	virtual std::string toMercuryString() const;
	virtual TokenType type() const;
};

// ']'
class CloseBracketToken : public Token {
public:
	//CloseBracketToken();
	virtual std::string toMercuryString() const;
	virtual TokenType type() const;
};

// '{'
class OpenCurlyBracketToken : public Token {
public:
	//OpenBracketToken();
	virtual std::string toMercuryString() const;
	virtual TokenType type() const;
};

// '}'
class CloseCurlyBracketToken : public Token {
public:
	//CloseBracketToken();
	virtual std::string toMercuryString() const;
	virtual TokenType type() const;
};

// variable name
class VariableNameToken : public Token {
public:
	VariableNameToken(const std::string & s);
	virtual std::string toMercuryString() const;
	virtual TokenType type() const;
	std::string name() const;  // unescaped name
protected:
	std::string nameValue;
};

// atom: either a sequence of [a-zA-Z0-9_] or a sequence of chars
// enclosed in apostrophes.
class AtomToken : public Token {
public:
	AtomToken(const std::string & s);
	virtual std::string toMercuryString() const;
	virtual TokenType type() const;
	std::string value() const;  // unescaped value
protected:
	std::string atomValue;
};

// float
class FloatToken : public Token {
public:
	FloatToken(double f);
	virtual std::string toMercuryString() const;
	virtual TokenType type() const;
	double value() const;  // unescaped value
protected:
	double floatValue;
};

// string
class StringToken : public Token {
public:
	StringToken(const std::string & s);
	virtual std::string toMercuryString() const;
	virtual TokenType type() const;
	std::string value() const;  // unescaped
protected:
	std::string stringValue;
};

#endif
