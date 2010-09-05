#ifndef FORWARDEDABDUCERSERVER_H__
#define FORWARDEDABDUCERSERVER_H__  1

// ----------------------------------------------------------------------------
// Copyright (C) 2009-2010 DFKI GmbH Talking Robots 
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

#include "weigabd.h"
#include <vector>
#include <unistd.h>

#include <iostream>

namespace Abducer = ::de::dfki::lt::tr::infer::weigabd::slice;

class ForwardedAbducerServer : public Abducer::AbductionEngine {

public:
	ForwardedAbducerServer(pid_t abducer_pid, int fd_out, int fd_in);

	virtual void clearContext(const Ice::Current&);

	virtual void loadFile(const std::string& filename, const Ice::Current&);

	virtual void clearRules(const Ice::Current&);
	virtual void clearFacts(const Ice::Current&);
	virtual void clearFactsByModality(Abducer::Modality mod, const Ice::Current&);
	virtual void clearAssumables(const Ice::Current&);
	virtual void clearAssumabilityFunction(const std::string & function, const Ice::Current&);
	virtual void clearDisjointDeclarations(const Ice::Current&);

	virtual void addRule(const Abducer::RulePtr & r, const Ice::Current&);
	virtual void addFact(const Abducer::ModalisedAtomPtr & a, const Ice::Current&);
	virtual void addAssumable(const std::string& function, const Abducer::ModalisedAtomPtr & a, float cost, const Ice::Current&);
	virtual void addDisjointDeclaration(const Abducer::DisjointDeclarationPtr & dd, const Ice::Current&);

//	virtual Abducer::ProveResult prove(const std::vector<Abducer::MarkedQueryPtr> & g, const Ice::Current&);

	virtual void startProving(const std::vector<Abducer::MarkedQueryPtr> & g, const Ice::Current&);
	virtual std::vector<Abducer::ProofWithCostPtr> getProofs(int timeout, const Ice::Current&);

protected:
	void checkOkReply();
	void clearContext();
	std::vector<Abducer::ProofWithCostPtr> getProofs();

	pid_t abducer_pid;

	int fd_in;
	int fd_out;
};

#endif
