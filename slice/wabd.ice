#ifndef WABD_ICE
#define WABD_ICE
 
module de {
module dfki {
module lt {
module tr {
module infer {
module wabd {

	// TERMS & PREDICATES

	// Base class for both types of terms.
	class Term { };

	sequence<Term> TermSeq;

	// Variable.
	class VariableTerm extends Term {
		string name;
	};

	// Function term is a term of the form f(t1,...tn)
	// where f is a functor and t1... are terms.
	class FunctionTerm extends Term {
		string functor;
		TermSeq args;
	};

	class Predicate {
		string predSym;
		TermSeq args;
	};

	//-----------------------------------------------------------------

	// MODALITIES

	// FIXME: this is not very elegant (used only by AbducerServer::clearFactsByModality)
	enum ModalityType {
		Understanding,
		Generation,
		Event,
		Intention,
		Info,
		AttState,
		K
	};

	enum Agent {
		human,
		robot
	};

	enum Sharing {
		Private,
		Attribute,
		Mutual
	};

	// Base modality class.
	class Modality { };
	
	sequence<Modality> ModalitySeq;

	class UnderstandingModality extends Modality { };

	class GenerationModality extends Modality { };

	class EventModality extends Modality { };

	class IntentionModality extends Modality { };
	
	class InfoModality extends Modality { };
	
	class AttStateModality extends Modality { };

	class KModality extends Modality {
		Agent ag;
		Agent ag2;
		Sharing share;
	};

	//-----------------------------------------------------------------

	class ModalisedFormula {
		ModalitySeq m;
		Predicate p;
	};

	sequence<ModalisedFormula> ModalisedFormulaSeq;

	//-----------------------------------------------------------------

	// PIECES OF PROOF

	// marking as used in the abductive proof
	enum Marking {
		Proved,
		Unsolved,
		Assumed,
		Asserted
	};

	// base class for abductive proofs
	class MarkedQuery {
		Marking mark;
		ModalisedFormula body;
	};

	// this predicate has been solved
	class ProvedQuery extends MarkedQuery {};

	// this predicate is yet to be solved
	class UnsolvedQuery extends MarkedQuery {
		// isConst == true -> constCost valid, else costFunction valid
		bool isConst;
		float constCost;
		string costFunction;
	};

	// assumed predicate
	class AssumedQuery extends MarkedQuery {
		// TODO: would we perhaps prefer to have the actual used costs
		// in the returned proof?

		// isConst == true -> constCost valid, else costFunction valid
		bool isConst;
		float constCost;
		string costFunction;
	};

	// asserted predicate
	class AssertedQuery extends MarkedQuery {
		ModalisedFormulaSeq antecedents;
	};

	//-----------------------------------------------------------------

	// PROOF

	sequence<MarkedQuery> MarkedQuerySeq;

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

	exception ServerException extends AbducerException {
		string message;
	};

	//-----------------------------------------------------------------


	interface AbducerServer {

		void loadFile(string filename)
				throws FileReadErrorException, SyntaxErrorException;

		void clearRules();
		void clearFacts();
		void clearFactsByModality(ModalityType type);
		void clearAssumables();
		void clearAssumableFunction(string function);

		void addFact(ModalisedFormula f);
		void addAssumable(string function, ModalisedFormula f, float cost);

		ProveResult prove(MarkedQuerySeq g);
		MarkedQuerySeq getBestProof()
				throws NoProofException;
	};

};
};
};
};
};
};

#endif
