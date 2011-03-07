#ifndef ABDUCER_ICE
#define ABDUCER_ICE

// =================================================================
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
// =================================================================

// ===================================================================
// MODULE: de.dfki.lt.tr.infer.abducer
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
module abducer {

	const string RELEASE = "2.1";

module lang {

	//-----------------------------------------------------------------
	// TERMS & ATOMS

	// Base class for both types of terms.
	class Term { };

	["java:type:java.util.ArrayList<Term>"] sequence<Term> TermSeq;

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

	["java:type:java.util.ArrayList<Modality>"] sequence<Modality> ModalitySeq;

	//-----------------------------------------------------------------
	// MODALISED ATOMS

	// Modalised atom, consisting of a sequence of modalities and an
	// atom.
	class ModalisedAtom {
		ModalitySeq m;
		Atom a;
	};

	["java:type:java.util.ArrayList<ModalisedAtom>"] sequence<ModalisedAtom> ModalisedAtomSeq;

	//-----------------------------------------------------------------
	// RULES

	// Base class for assumability functions.
	class AssumabilityFunction { };

	// "Null" assumability function. Such annotated antecedents cannot
	// be assumed (i.e. have to be proved).
	class NullAssumabilityFunction extends AssumabilityFunction { };

	// Constant assumability function. Such annotated antecedents
	// are always assumed under the given cost.
	class ConstAssumabilityFunction extends AssumabilityFunction {
		float cost;
	};

	// Named assumability function. Such annotated antecedents can
	// only be assumed iff they are in the domain of the assumability
	// function.
	class NamedAssumabilityFunction extends AssumabilityFunction {
		string name;
	};

	//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

	// Base class for rule antecedents.
	class Antecedent {
		ModalisedAtom matom;
	};

	// Antecedent assumable under the given assumability function
	// (including the NullAssumabilityFunction).
	class AssumableAntecedent extends Antecedent {
		AssumabilityFunction f;
	};

	// Asserted antecedent.
	class AssertionAntecedent extends Antecedent { };

	["java:type:java.util.ArrayList<Antecedent>"] sequence<Antecedent> AntecedentSeq;

	// A rule, consisting of a head and a sequence of antecedents.
	class Rule {
		ModalisedAtom head;
		AntecedentSeq ante;
	};

	//-----------------------------------------------------------------
	// DISJOINT DECLARATIONS

	// A disjoint declaration.
	class DisjointDeclaration {
		ModalisedAtomSeq atoms;
	};

};

module proof {

	//-----------------------------------------------------------------
	// QUERIES

	// Base class for abductive proofs.
	class MarkedQuery {
		lang::ModalisedAtom atom;
	};

	// "Proved". Marks modalised atoms that have been solved.
	class ProvedQuery extends MarkedQuery { };

	// "Unsolved". Marks modalised atoms that are yet to be solved.
	class UnsolvedQuery extends MarkedQuery {
		lang::AssumabilityFunction f;
	};

	// "Assumed". Marks modalised atoms that are assumed under the given
	// assumability function.
	class AssumedQuery extends MarkedQuery {
		lang::AssumabilityFunction f;
	};

	// "Asserted". Marks modalised atoms that are asserted.
	class AssertedQuery extends MarkedQuery { };

	//-----------------------------------------------------------------
	// PROOF

	["java:type:java.util.ArrayList<MarkedQuery>"] sequence<MarkedQuery> MarkedQuerySeq;

	class ProofWithCost {
		MarkedQuerySeq proof;
		float cost;
	};

	["java:type:java.util.ArrayList<ProofWithCost>"] sequence<ProofWithCost> ProofWithCostSeq;

};

module engine {

	//-----------------------------------------------------------------
	// SERVER INTERFACE

	//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Exceptions

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

	//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Proof search method

	class ProofSearchMethod { };

	class DFS extends ProofSearchMethod {
	};

	class BoundedDFS extends ProofSearchMethod {
		float bound;
	};

	class IDDFS extends ProofSearchMethod {
		float initBound;
		float multiplier;
	};

	//-----------------------------------------------------------------
	// Abduction engine

	interface AbductionEngine {

		void clearContext();

		void loadFile(string filename)
				throws FileReadErrorException, SyntaxErrorException;

		void clearRules();
		void clearFacts();
		void clearFactsByModality(lang::Modality type);
		void clearAssumables();
		void clearAssumabilityFunction(string function);
		void clearDisjointDeclarations();

		void addRule(lang::Rule r);
		void addFact(lang::ModalisedAtom a);
		void addAssumable(string function, lang::ModalisedAtom a, float cost);
		void addDisjointDeclaration(lang::DisjointDeclaration dd);

		// Start proving the goal.
		void startProvingWithMethod(proof::MarkedQuerySeq g, ProofSearchMethod method);
		void startProving(proof::MarkedQuerySeq g);  // for backward compatibility

		// If timeout > 0, it will be the maximum interval (in miliseconds)
		// to wait for the results -- after this interval has expired,
		// the best available result will be provided. If timeout == 0,
		// return the best results so far immediately; if timeout == -1,
		// wait indefinitely.
		proof::ProofWithCostSeq getProofs(int timeout);
	};

	//-----------------------------------------------------------------
	// Engine server 

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
