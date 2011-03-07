#ifndef PROTOTOSLICE_H__
#define PROTOTOSLICE_H__  1

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

#include "abducer.h"
#include "protocol.pb.h"

namespace de {
namespace dfki {
namespace lt {
namespace tr {
namespace infer {
namespace abducer {

proof::ProofWithCostPtr
proofWithCostFromProto(const protocol::Proof & p_p);

proof::MarkedQueryPtr
markedQueryFromProto(const protocol::MarkedQuery & p_q);

lang::AssumabilityFunctionPtr
assumabilityFunctionFromProto(const protocol::AssumabilityFunction & p_af);

lang::ModalisedAtomPtr
modalisedAtomFromProto(const protocol::ModalisedAtom & p_ma);

lang::Modality
modalityFromProto(protocol::Modality p_m);

lang::AtomPtr
atomFromProto(const protocol::Atom & p_a);

lang::TermPtr
termFromProto(const protocol::Term & p_t);

}
}
}
}
}
}

#endif
