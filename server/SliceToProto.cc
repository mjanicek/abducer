// ----------------------------------------------------------------------------
// Copyright (C) 2010-2011 DFKI GmbH Talking Robots 
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

#include "SliceToProto.h"
#include <vector>

#include "ProtocolException.h"

using namespace std;

namespace de {
namespace dfki {
namespace lt {
namespace tr {
namespace infer {
namespace abducer {

protocol::Modality
protoModality(lang::Modality m) {
	switch (m) {
		case lang::Understanding: return protocol::UNDERSTANDING;
		case lang::Generation:    return protocol::GENERATION;
		case lang::Event:         return protocol::EVENT;
		case lang::Intention:     return protocol::INTENTION;
		case lang::Attention:     return protocol::ATTENTION;
		case lang::Belief:        return protocol::BELIEF;
		case lang::Truth:         return protocol::TRUTH;
		default: throw ProtocolException("unknown modality");
	}
}

protocol::Term
protoTerm(const lang::TermPtr & a_t)
{
	protocol::Term p_t;

	if (lang::FunctionTermPtr a_ft = lang::FunctionTermPtr::dynamicCast(a_t)) {
		p_t.set_type(protocol::Term::FUNCTION);
		p_t.set_functor(a_ft->functor);
		vector<lang::TermPtr>::const_iterator i;
		for (i = a_ft->args.begin(); i != a_ft->args.end(); i++) {
			p_t.add_args()->CopyFrom(protoTerm(*i));
		}
	}
	else if (lang::VariableTermPtr a_vt = lang::VariableTermPtr::dynamicCast(a_t)) {
		p_t.set_type(protocol::Term::VARIABLE);
		p_t.set_var_name(a_vt->name);
	}
	else {
		throw ProtocolException("unexpected term type");
	}

	return p_t;
}

protocol::Atom
protoAtom(const lang::AtomPtr & a_a)
{
	protocol::Atom p_a;
	p_a.set_pred_sym(a_a->predSym);

	vector<lang::TermPtr>::const_iterator i;
	for (i = a_a->args.begin(); i != a_a->args.end(); i++) {
		p_a.add_args()->CopyFrom(protoTerm(*i));
	}

	return p_a;
}

protocol::ModalisedAtom
protoModalisedAtom(const lang::ModalisedAtomPtr & a_ma)
{
	protocol::ModalisedAtom p_ma;

	vector<lang::Modality>::const_iterator i;
	for (i = a_ma->m.begin(); i != a_ma->m.end(); i++) {
		p_ma.add_mod(protoModality(*i));
	}
	p_ma.mutable_atom()->CopyFrom(protoAtom(a_ma->a));

	return p_ma;
}

protocol::ModalisedRule
protoModalisedRule(const lang::RulePtr & a_r)
{
	protocol::ModalisedRule p_r;

	p_r.mutable_head()->CopyFrom(protoModalisedAtom(a_r->head));

	vector<lang::AntecedentPtr>::const_iterator i;
	for (i = a_r->ante.begin(); i != a_r->ante.end(); i++) {
		p_r.add_ante()->CopyFrom(protoAntecedent(*i));
	}

	return p_r;
}

protocol::Antecedent
protoAntecedent(const lang::AntecedentPtr & a_a)
{
	protocol::Antecedent p_a;
	p_a.mutable_matom()->CopyFrom(protoModalisedAtom(a_a->matom));

	if (lang::AssumableAntecedentPtr a_aa = lang::AssumableAntecedentPtr::dynamicCast(a_a)) {
		p_a.set_type(protocol::Antecedent::ASSUMABLE);
		p_a.mutable_function()->CopyFrom(protoAssumabilityFunction(a_aa->f));
	}
	else if (lang::AssertionAntecedentPtr a_ra = lang::AssertionAntecedentPtr::dynamicCast(a_a)) {
		p_a.set_type(protocol::Antecedent::ASSERTED);
	}
	else {
		throw ProtocolException("unexpected antecedent type");
	}

	return p_a;
}

protocol::AssumabilityFunction
protoAssumabilityFunction(const lang::AssumabilityFunctionPtr & a_f)
{
	protocol::AssumabilityFunction p_f;

	if (lang::NullAssumabilityFunctionPtr a_nf = lang::NullAssumabilityFunctionPtr::dynamicCast(a_f)) {
		p_f.set_function_type(protocol::AssumabilityFunction::NOTASSUMABLE);
	}
	else if (lang::ConstAssumabilityFunctionPtr a_cf = lang::ConstAssumabilityFunctionPtr::dynamicCast(a_f)) {
		p_f.set_function_type(protocol::AssumabilityFunction::CONST);
		p_f.set_cost(a_cf->cost);
	}
	else if (lang::NamedAssumabilityFunctionPtr a_mf = lang::NamedAssumabilityFunctionPtr::dynamicCast(a_f)) {
		p_f.set_function_type(protocol::AssumabilityFunction::NAMED);
		p_f.set_function_name(a_mf->name);
	}
	else {
		throw ProtocolException("unexpected assumability function type");
	}

	return p_f;
}

protocol::MarkedQuery
protoMarkedQuery(const proof::MarkedQueryPtr & a_q)
{
	protocol::MarkedQuery p_q;

	p_q.mutable_matom()->CopyFrom(protoModalisedAtom(a_q->atom));

	if (proof::ProvedQueryPtr a_pq = proof::ProvedQueryPtr::dynamicCast(a_q)) {
		p_q.set_marking(protocol::MarkedQuery::PROVED);
	}
	else if (proof::UnsolvedQueryPtr a_uq = proof::UnsolvedQueryPtr::dynamicCast(a_q)) {
		p_q.set_marking(protocol::MarkedQuery::UNSOLVED);
		p_q.mutable_function()->CopyFrom(protoAssumabilityFunction(a_uq->f));
	}
	else if (proof::AssumedQueryPtr a_sq = proof::AssumedQueryPtr::dynamicCast(a_q)) {
		p_q.set_marking(protocol::MarkedQuery::ASSUMED);
		p_q.mutable_function()->CopyFrom(protoAssumabilityFunction(a_sq->f));
	}
	else if (proof::AssertedQueryPtr a_rq = proof::AssertedQueryPtr::dynamicCast(a_q)) {
		p_q.set_marking(protocol::MarkedQuery::ASSERTED);
	}
	else {
		throw ProtocolException("unexpected query marking");
	}

	return p_q;
}

protocol::ProofSearchMethod
protoProofSearchMethod(const engine::ProofSearchMethodPtr & a_meth)
{
	protocol::ProofSearchMethod p_meth;

	if (engine::IDDFSPtr a_iddfs = engine::IDDFSPtr::dynamicCast(a_meth)) {
		p_meth.set_method(protocol::ProofSearchMethod::IDDFS);
		p_meth.set_init_bound(a_iddfs->initBound);
		p_meth.set_multiplier(a_iddfs->multiplier);
	}
	else if (engine::BoundedDFSPtr a_bdfs = engine::BoundedDFSPtr::dynamicCast(a_meth)) {
		p_meth.set_method(protocol::ProofSearchMethod::BOUNDEDDFS);
		p_meth.set_max_bound(a_bdfs->bound);
	}
	else {
		p_meth.set_method(protocol::ProofSearchMethod::DFS);
	}

	return p_meth;
}

}
}
}
}
}
}
