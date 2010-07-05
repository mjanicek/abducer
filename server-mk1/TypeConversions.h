#ifndef TYPECONVERSIONS_H__
#define TYPECONVERSIONS_H__  1

extern "C" {
#include "mercury_imp.h"
}

#include <string>
#include <vector>

#include "Abducer.h"

typedef MR_Word  MR_ctx;
typedef MR_Word  MR_varset;
typedef MR_Word  MR_term;
typedef MR_Word  MR_atomic_formula;
typedef MR_Word  MR_mprop__ctx_modality;
typedef MR_Word  MR_with_cost_function__mprop__ctx_modality;
typedef MR_Word  MR_ctx_modality;
typedef MR_Word  MR_proof__ctx_modality;

typedef MR_Word  MR_list__term;
typedef MR_Word  MR_list__ctx_modality;

namespace cc2m {

	MR_String
	string(const std::string & s);

	MR_term
	term(const Abducer::TermPtr & t, MR_varset * w_vs);

}

namespace m2cc {

	Abducer::TermPtr
	term(MR_varset w_vs, MR_term w_t);

}

MR_atomic_formula
predicateToMercAtomicFormula(const Abducer::PredicatePtr & p, MR_varset * w_vs);

MR_mprop__ctx_modality
modalisedFormulaToMercMProp(const Abducer::ModalisedFormulaPtr & p, MR_varset * w_vs);

MR_with_cost_function__mprop__ctx_modality
withConstCostFunction(MR_mprop__ctx_modality w_mprop, double cost);

MR_ctx_modality
modalityToMercModality(const Abducer::ModalityPtr & m);

MR_list__ctx_modality
modalitySeqToMercListOfModalities(const Abducer::ModalitySeq & ms);

MR_Word
markedQueryToMercQuery(const Abducer::MarkedQueryPtr & mq, MR_Word * w_vs);

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Abducer::ModalityPtr
MR_WordToModality(MR_ctx_modality w);

Abducer::PredicatePtr
MR_WordToPredicate(MR_varset w_vs, MR_atomic_formula w_p);

Abducer::ModalisedFormulaPtr
MR_WordToModalisedFormula(MR_varset w_vs, MR_mprop__ctx_modality w_mf);

std::vector<Abducer::MarkedQueryPtr>
MR_WordToMarkedQuerySeq(MR_ctx w_ctx, MR_proof__ctx_modality w_proof);

#endif
