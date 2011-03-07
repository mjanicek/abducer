// ----------------------------------------------------------------------------
// Copyright (C) 2009-2011 DFKI GmbH Talking Robots 
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

#ifndef SLICETOSTRING_H__
#define SLICETOSTRING_H__  1

#include "abducer.h"

#include <vector>
#include <string>

namespace de {
namespace dfki {
namespace lt {
namespace tr {
namespace infer {
namespace abducer {

std::string
atomToString(const lang::AtomPtr & a);

std::string
termToString(const lang::TermPtr & t);

std::string
modalityToString(const lang::Modality m);

std::string
modalisedAtomToString(const lang::ModalisedAtomPtr & ma);

}
}
}
}
}
}

#endif
