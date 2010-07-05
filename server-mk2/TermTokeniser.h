#ifndef TERMTOKENISER_H__
#define TERMTOKENISER_H__  1

#include <vector>
#include <string>

#include "Tokens.h"

std::vector<Token *>
tokenise(const std::string & s);

#endif
