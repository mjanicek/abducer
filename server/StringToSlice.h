#ifndef STRINGTOSLICE_H__
#define STRINGTOSLICE_H__  1

#include <vector>
#include <iterator>

#include "weigabd.h"
#include "Tokens.h"

namespace Abducer = ::de::dfki::lt::tr::infer::weigabd::slice;

Abducer::TermPtr
parseTerm(std::vector<Token *>::iterator & it);

Abducer::PredicatePtr
parsePredicate(std::vector<Token *>::iterator & it);

Abducer::ModalisedFormulaPtr
parseModalisedFormula(std::vector<Token *>::iterator & it);

bool
parseModality(std::vector<Token *>::iterator & it, Abducer::Modality & mod);

#endif
