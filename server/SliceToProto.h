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

#ifndef SLICETOPROTO_H__
#define SLICETOPROTO_H__  1

#include "abducer.h"
#include "protocol.pb.h"

namespace de {
namespace dfki {
namespace lt {
namespace tr {
namespace infer {
namespace abducer {

protocol::Modality
protoModality(lang::Modality m);

protocol::ModalisedAtom
protoModalisedAtom(const lang::ModalisedAtomPtr & a_ma);

protocol::ModalisedRule
protoModalisedRule(const lang::RulePtr & a_r);

protocol::Antecedent
protoAntecedent(const lang::AntecedentPtr & a_a);

protocol::AssumabilityFunction
protoAssumabilityFunction(const lang::AssumabilityFunctionPtr & a_f);

protocol::MarkedQuery
protoMarkedQuery(const proof::MarkedQueryPtr & a_q);

protocol::ProofSearchMethod
protoProofSearchMethod(const engine::ProofSearchMethodPtr & a_meth);

}
}
}
}
}
}

#endif
