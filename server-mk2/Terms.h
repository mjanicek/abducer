#ifndef TERMS_H__
#define TERMS_H__  1

#include <string>
#include <vector>

class MercuryTerm {
public:
	virtual std::string toMercuryString() = 0;
};

class Argument : public MercuryTerm {
};

class Term : public Argument {
public:
	Term();
	virtual std::string toMercuryString();

	std::string functor;
	std::vector<Argument *> args;
};

class Var : public Argument {
public:
	Var();
	virtual std::string toMercuryString();

	std::string name;
};

class StringArgument : public Argument {
public:
	StringArgument();
	virtual std::string toMercuryString();

	std::string value;
};

class FloatArgument : public Argument {
public:
	FloatArgument();
	virtual std::string toMercuryString();

	double value;
};

class Predicate : public MercuryTerm {
public:
	Predicate();
	virtual std::string toMercuryString();

	std::string predSym;
	std::vector<Argument *> args;
};


#endif
