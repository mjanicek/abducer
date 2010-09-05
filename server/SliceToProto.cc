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

#include "SliceToProto.h"
#include <vector>

#include "ProtocolException.h"

using namespace std;

protocol::Modality
protoModality(Abducer::Modality m) {
	switch (m) {
		case Abducer::Understanding: return protocol::UNDERSTANDING;
		case Abducer::Generation:    return protocol::GENERATION;
		case Abducer::Event:         return protocol::EVENT;
		case Abducer::Intention:     return protocol::INTENTION;
		case Abducer::Attention:     return protocol::ATTENTION;
		case Abducer::Belief:        return protocol::BELIEF;
		case Abducer::Truth:         return protocol::TRUTH;
		default: throw ProtocolException("unknown modality");
	}
}

protocol::Term
protoTerm(const Abducer::TermPtr & a_t)
{
	protocol::Term p_t;

	if (Abducer::FunctionTermPtr a_ft = Abducer::FunctionTermPtr::dynamicCast(a_t)) {
		p_t.set_type(protocol::Term::FUNCTION);
		p_t.set_functor(a_ft->functor);
		vector<Abducer::TermPtr>::const_iterator i;
		for (i = a_ft->args.begin(); i != a_ft->args.end(); i++) {
			p_t.add_args()->CopyFrom(protoTerm(*i));
		}
	}
	else if (Abducer::VariableTermPtr a_vt = Abducer::VariableTermPtr::dynamicCast(a_t)) {
		p_t.set_type(protocol::Term::VARIABLE);
		p_t.set_var_name(a_vt->name);
	}
	else {
		throw ProtocolException("unexpected term type");
	}

	return p_t;
}

protocol::Atom
protoAtom(const Abducer::AtomPtr & a_a)
{
	protocol::Atom p_a;
	p_a.set_pred_sym(a_a->predSym);

	vector<Abducer::TermPtr>::const_iterator i;
	for (i = a_a->args.begin(); i != a_a->args.end(); i++) {
		p_a.add_args()->CopyFrom(protoTerm(*i));
	}

	return p_a;
}

protocol::ModalisedAtom
protoModalisedAtom(const Abducer::ModalisedAtomPtr & a_ma)
{
	protocol::ModalisedAtom p_ma;

	vector<Abducer::Modality>::const_iterator i;
	for (i = a_ma->m.begin(); i != a_ma->m.end(); i++) {
		p_ma.add_mod(protoModality(*i));
	}
	p_ma.mutable_atom()->CopyFrom(protoAtom(a_ma->a));

	return p_ma;
}

protocol::ModalisedRule
protoModalisedRule(const Abducer::RulePtr & a_r)
{
	protocol::ModalisedRule p_r;

	p_r.mutable_head()->CopyFrom(protoModalisedAtom(a_r->head));

	vector<Abducer::AntecedentPtr>::const_iterator i;
	for (i = a_r->ante.begin(); i != a_r->ante.end(); i++) {
		p_r.add_ante()->CopyFrom(protoAntecedent(*i));
	}

	return p_r;
}

protocol::Antecedent
protoAntecedent(const Abducer::AntecedentPtr & a_a)
{
	protocol::Antecedent p_a;
	p_a.mutable_matom()->CopyFrom(protoModalisedAtom(a_a->matom));

	if (Abducer::AssumableAntecedentPtr a_aa = Abducer::AssumableAntecedentPtr::dynamicCast(a_a)) {
		p_a.set_type(protocol::Antecedent::ASSUMABLE);
		p_a.mutable_function()->CopyFrom(protoAssumabilityFunction(a_aa->f));
	}
	else if (Abducer::AssertionAntecedentPtr a_ra = Abducer::AssertionAntecedentPtr::dynamicCast(a_a)) {
		p_a.set_type(protocol::Antecedent::ASSERTED);
	}
	else {
		throw ProtocolException("unexpected antecedent type");
	}

	return p_a;
}

protocol::AssumabilityFunction
protoAssumabilityFunction(const Abducer::AssumabilityFunctionPtr & a_f)
{
	protocol::AssumabilityFunction p_f;

	if (Abducer::NullAssumabilityFunctionPtr a_nf = Abducer::NullAssumabilityFunctionPtr::dynamicCast(a_f)) {
		p_f.set_function_type(protocol::AssumabilityFunction::NOTASSUMABLE);
	}
	else if (Abducer::ConstAssumabilityFunctionPtr a_cf = Abducer::ConstAssumabilityFunctionPtr::dynamicCast(a_f)) {
		p_f.set_function_type(protocol::AssumabilityFunction::CONST);
		p_f.set_cost(a_cf->cost);
	}
	else if (Abducer::NamedAssumabilityFunctionPtr a_mf = Abducer::NamedAssumabilityFunctionPtr::dynamicCast(a_f)) {
		p_f.set_function_type(protocol::AssumabilityFunction::NAMED);
		p_f.set_function_name(a_mf->name);
	}
	else {
		throw ProtocolException("unexpected assumability function type");
	}

	return p_f;
}

protocol::MarkedQuery
protoMarkedQuery(const Abducer::MarkedQueryPtr & a_q)
{
	protocol::MarkedQuery p_q;

	p_q.mutable_matom()->CopyFrom(protoModalisedAtom(a_q->atom));

	if (Abducer::ProvedQueryPtr a_pq = Abducer::ProvedQueryPtr::dynamicCast(a_q)) {
		p_q.set_marking(protocol::MarkedQuery::PROVED);
	}
	else if (Abducer::UnsolvedQueryPtr a_uq = Abducer::UnsolvedQueryPtr::dynamicCast(a_q)) {
		p_q.set_marking(protocol::MarkedQuery::UNSOLVED);
		p_q.mutable_function()->CopyFrom(protoAssumabilityFunction(a_uq->f));
	}
	else if (Abducer::AssumedQueryPtr a_sq = Abducer::AssumedQueryPtr::dynamicCast(a_q)) {
		p_q.set_marking(protocol::MarkedQuery::ASSUMED);
		p_q.mutable_function()->CopyFrom(protoAssumabilityFunction(a_sq->f));
	}
	else if (Abducer::AssertedQueryPtr a_rq = Abducer::AssertedQueryPtr::dynamicCast(a_q)) {
		p_q.set_marking(protocol::MarkedQuery::ASSERTED);
	}
	else {
		throw ProtocolException("unexpected query marking");
	}

	return p_q;
}
