#ifndef WEIGABD_ICE
#define WEIGABD_ICE

// =================================================================
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
// =================================================================

// ===================================================================
// MODULE: de.dfki.lt.tr.infer.slice.wabd
// 
// Defines the ICE interface and data structures used by the weighted
// abductive inference engine ("the abducer").
// 
// Authors:  Miroslav Janicek  <miroslav.janicek@dfki.de>
//
// ===================================================================

module de {
module dfki {
module lt {
module tr {
module infer {
module weigabd {
module slice {

	const string RELEASE = "2.0";

	//-----------------------------------------------------------------

	// TERMS & ATOMS

	// Base class for both types of terms.
	class Term { };

	sequence<Term> TermSeq;

	// The representation of a named variable.
	class VariableTerm extends Term {
		string name;
	};

	// Function term is a term of the form f(t1,...tn) where f is
	// a functor and t1... are terms.
	class FunctionTerm extends Term {
		string functor;
		TermSeq args;
	};

	// Atomic formula, consisting of a predicate symbol and a list of
	// arguments -- terms.
	class Atom {
		string predSym;
		TermSeq args;
	};

	//-----------------------------------------------------------------

	// MODALITIES

	enum Modality {
		Understanding,
		Generation,
		Event,
		Intention,
		Attention,
		Belief,
		Truth
	};

	sequence<Modality> ModalitySeq;

	//-----------------------------------------------------------------

	class ModalisedAtom {
		ModalitySeq m;
		Atom a;
	};

	sequence<ModalisedAtom> ModalisedAtomSeq;

	//-----------------------------------------------------------------

	enum Marking {
		Unsolved,
		Proved,
		Assumed,
		Asserted
	};

	class AssumabilityFunction { };

	class NullAssumabilityFunction extends AssumabilityFunction { };

	class ConstAssumabilityFunction extends AssumabilityFunction {
		float cost;
	};

	class NamedAssumabilityFunction extends AssumabilityFunction {
		string name;
	};

	//-----------------------------------------------------------------

	class Antecedent {
		ModalisedAtom matom;
	};

	class AssumableAntecedent extends Antecedent {
		AssumabilityFunction f;
	};

	class AssertionAntecedent extends Antecedent { };

	sequence<Antecedent> AntecedentSeq;

	class Rule {
		ModalisedAtom head;
		AntecedentSeq ante;
	};

	//-----------------------------------------------------------------

	class DisjointDeclaration {
		ModalisedAtomSeq atoms;
	};

	//-----------------------------------------------------------------

	// PIECES OF PROOF

	// base class for abductive proofs
	class MarkedQuery {
		ModalisedAtom atom;
	};

	// this predicate has been solved
	class ProvedQuery extends MarkedQuery { };

	// this predicate is yet to be solved
	class UnsolvedQuery extends MarkedQuery {
		AssumabilityFunction f;
	};

	// assumed predicate
	class AssumedQuery extends MarkedQuery {
		AssumabilityFunction f;
	};

	// asserted predicate
	class AssertedQuery extends MarkedQuery { };

	//-----------------------------------------------------------------

	// PROOF

	sequence<MarkedQuery> MarkedQuerySeq;

	class ProofWithCost {
		MarkedQuerySeq proof;
		float cost;
	};

	sequence<ProofWithCost> ProofWithCostSeq;

	//-----------------------------------------------------------------

	// SERVER INTERFACE

	enum ProveResult {
		ProofFound,
		NoProofFound,
		Error
	};

	//-----------------------------------------------------------------

	exception AbducerException { };
	
	exception FileReadErrorException extends AbducerException {
		string filename;
	};

	exception SyntaxErrorException extends AbducerException {
		string filename;
		string error;
		int line;
	};

	exception NoProofException extends AbducerException {};

	exception EngineException extends AbducerException {
		string message;
	};

	//-----------------------------------------------------------------

	interface AbductionEngine {

		void clearContext();

		void loadFile(string filename)
				throws FileReadErrorException, SyntaxErrorException;

		void clearRules();
		void clearFacts();
		void clearFactsByModality(Modality type);
		void clearAssumables();
		void clearAssumabilityFunction(string function);
		void clearDisjointDeclarations();

		void addRule(Rule r);
		void addFact(ModalisedAtom a);
		void addAssumable(string function, ModalisedAtom a, float cost);
		void addDisjointDeclaration(DisjointDeclaration dd);

		// Start proving the goal.
		void startProving(MarkedQuerySeq g);

		// If timeout > 0, it will be the maximum interval (in miliseconds)
		// to wait for the results -- after this interval has expired,
		// the best available result will be provided. If timeout == 0,
		// return the best results so far immediately; if timeout == -1,
		// wait indefinitely.
		ProofWithCostSeq getProofs(int timeout);
	};

	//-----------------------------------------------------------------

	interface AbductionEngineServer {
		AbductionEngine* getEngineProxy(string name);
	};

};
};
};
};
};
};
};

#endif
