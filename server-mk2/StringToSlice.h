#ifndef STRINGTOSLICE_H__
#define STRINGTOSLICE_H__  1

#include <vector>
#include <iterator>

#include "Abducer.h"
#include "Tokens.h"

Abducer::TermPtr
parseTerm(std::vector<Token *>::iterator & it);

Abducer::PredicatePtr
parsePredicate(std::vector<Token *>::iterator & it);

Abducer::ModalisedFormulaPtr
parseModalisedFormula(std::vector<Token *>::iterator & it);

Abducer::ModalityPtr
parseModality(std::vector<Token *>::iterator & it);

#endif
