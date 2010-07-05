#ifndef TERMPARSING_H__
#define TERMPARSING_H__  1

/*
SYNTAX
------

argument
	:= atom open_parenthesis arguments close_parenthesis
	:= atom
	:= var
	:= string
	:= float

arguments
	:= argument
	:= argument comma arguments

predicate
	:= atom open_parenthesis arguments close_parenthesis dot
	:= atom dot
*/

#include <string>
#include <vector>
#include <iterator>

#include "Tokens.h"
#include "Terms.h"

Argument *
parseArgument(std::vector<Token *>::iterator & it);

Predicate *
parsePredicate(std::vector<Token *>::iterator & it);

#endif
