#ifndef SLICETOSTRING_H__
#define SLICETOSTRING_H__  1

#include "weigabd.h"

#include <vector>
#include <string>

namespace Abducer = ::de::dfki::lt::tr::infer::weigabd::slice;

std::string
predicateToString(const Abducer::PredicatePtr & p);

std::string
termToString(const Abducer::TermPtr & t);

std::string
modalityToString(const Abducer::Modality m);

std::string
modalisedFormulaToString(const Abducer::ModalisedFormulaPtr & f);

#endif
