// ----------------------------------------------------------------------------
// Copyright (C) 2010 DFKI GmbH Talking Robots 
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

#include "ProtoToSlice.h"
#include <vector>

#include "ProtocolException.h"

Abducer::proof::ProofWithCostPtr
proofWithCostFromProto(const protocol::Proof & p_p)
{
	Abducer::proof::ProofWithCostPtr a_p = new Abducer::proof::ProofWithCost();
	a_p->cost = p_p.cost();
	for (int j = 0; j < p_p.proof_size(); j++) {
		a_p->proof.push_back(markedQueryFromProto(p_p.proof(j)));
	}
	return a_p;
}

Abducer::proof::MarkedQueryPtr
markedQueryFromProto(const protocol::MarkedQuery & p_q)
{
	switch (p_q.marking()) {
		case protocol::MarkedQuery::PROVED:
			{
				Abducer::proof::ProvedQueryPtr a_pq = new Abducer::proof::ProvedQuery();
				a_pq->atom = modalisedAtomFromProto(p_q.matom());
				return a_pq;
			}

		case protocol::MarkedQuery::UNSOLVED:
			{
				Abducer::proof::UnsolvedQueryPtr a_uq = new Abducer::proof::UnsolvedQuery();
				a_uq->atom = modalisedAtomFromProto(p_q.matom());
				if (p_q.has_function()) {
					a_uq->f = assumabilityFunctionFromProto(p_q.function());
					return a_uq;
				}
				else {
					throw ProtocolException("missing assumability function in an unsolved query");
				}
			}

		case protocol::MarkedQuery::ASSUMED:
			{
				Abducer::proof::AssumedQueryPtr a_sq = new Abducer::proof::AssumedQuery();
				a_sq->atom = modalisedAtomFromProto(p_q.matom());
				if (p_q.has_function()) {
					a_sq->f = assumabilityFunctionFromProto(p_q.function());
					return a_sq;
				}
				else {
					throw ProtocolException("missing assumability function in an assumed query");
				}
			}

		case protocol::MarkedQuery::ASSERTED:
			{
				Abducer::proof::AssertedQueryPtr a_rq = new Abducer::proof::AssertedQuery();
				a_rq->atom = modalisedAtomFromProto(p_q.matom());
				return a_rq;
			}

		default:
			throw ProtocolException("unknown query marking");
	}
}

Abducer::lang::AssumabilityFunctionPtr
assumabilityFunctionFromProto(const protocol::AssumabilityFunction & p_f)
{
	switch (p_f.function_type()) {
		case protocol::AssumabilityFunction::NOTASSUMABLE:
			{
				Abducer::lang::NullAssumabilityFunctionPtr a_nf = new Abducer::lang::NullAssumabilityFunction();
				return a_nf;
			}

		case protocol::AssumabilityFunction::CONST:
			{
				Abducer::lang::ConstAssumabilityFunctionPtr a_cf = new Abducer::lang::ConstAssumabilityFunction();
				if (p_f.has_cost()) {
					a_cf->cost = p_f.cost();
					return a_cf;
				}
				else {
					throw ProtocolException("missing cost in a const assumability function");
				}
			}

		case protocol::AssumabilityFunction::NAMED:
			{
				Abducer::lang::NamedAssumabilityFunctionPtr a_nf = new Abducer::lang::NamedAssumabilityFunction();
				if (p_f.has_function_name()) {
					a_nf->name = p_f.function_name();
					return a_nf;
				}
				else {
					throw ProtocolException("missing name in a named assumability function");
				}
			}

		default:
			throw ProtocolException("unknown assumability function type");
	}
}

Abducer::lang::ModalisedAtomPtr
modalisedAtomFromProto(const protocol::ModalisedAtom & p_ma)
{
	Abducer::lang::ModalisedAtomPtr result = new Abducer::lang::ModalisedAtom();
	for (int i = 0; i < p_ma.mod_size(); i++) {
		result->m.push_back(modalityFromProto(p_ma.mod(i)));
	}
	result->a = atomFromProto(p_ma.atom());
	return result;
}

Abducer::lang::Modality
modalityFromProto(protocol::Modality p_m)
{
	switch (p_m) {
		case protocol::UNDERSTANDING: return Abducer::lang::Understanding;
		case protocol::GENERATION:    return Abducer::lang::Generation;
		case protocol::EVENT:         return Abducer::lang::Event;
		case protocol::INTENTION:     return Abducer::lang::Intention;
		case protocol::ATTENTION:     return Abducer::lang::Attention;
		case protocol::BELIEF:        return Abducer::lang::Belief;
		case protocol::TRUTH:         return Abducer::lang::Truth;
		default: throw ProtocolException("unknown modality");
	}
}

Abducer::lang::AtomPtr
atomFromProto(const protocol::Atom & p_a)
{
	Abducer::lang::AtomPtr result = new Abducer::lang::Atom();
	result->predSym = p_a.pred_sym();
	for (int i = 0; i < p_a.args_size(); i++) {
		result->args.push_back(termFromProto(p_a.args(i)));
	}
	return result;
}

Abducer::lang::TermPtr
termFromProto(const protocol::Term & p_t)
{
	switch (p_t.type()) {
		case protocol::Term::FUNCTION:
			{
				Abducer::lang::FunctionTermPtr ft = new Abducer::lang::FunctionTerm();
				if (p_t.has_functor()) {
					ft->functor = p_t.functor();
					for (int i = 0; i < p_t.args_size(); i++) {
						ft->args.push_back(termFromProto(p_t.args(i)));
					}
					return ft;
				}
				else {
					throw ProtocolException("missing functor in a term");
				}
			}

		case protocol::Term::VARIABLE:
			{
				Abducer::lang::VariableTermPtr vt = new Abducer::lang::VariableTerm();
				if (p_t.has_var_name()) {
					vt->name = p_t.var_name();
					return vt;
				}
				else {
					throw ProtocolException("missing variable name");
				}
			}

		default:
			throw ProtocolException("unknown term type");
	}
}
