#ifndef SLICETOSTRING_H__
#define SLICETOSTRING_H__  1

#include "wabd.h"

#include <vector>
#include <string>

namespace Abducer = ::de::dfki::lt::tr::infer::wabd::slice;

std::string
predicateToString(const Abducer::PredicatePtr & p);

std::string
termToString(const Abducer::TermPtr & t);

std::string
modalityToString(const Abducer::ModalityPtr & m);

std::string
modalisedFormulaToString(const Abducer::ModalisedFormulaPtr & f);

#endif
