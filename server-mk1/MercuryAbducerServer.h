#ifndef MERCURYABDUCERSERVER_H__
#define MERCURYABDUCERSERVER_H__  1

extern "C" {
#include "mercury_imp.h"
}

#include "Abducer.h"
#include <vector>

class MercuryAbducerServer : public Abducer::AbducerServer {

public:
	MercuryAbducerServer();

	virtual void loadFile(const std::string& filename, const Ice::Current&);

	virtual void clearRules(const Ice::Current&);
	virtual void clearFacts(const Ice::Current&);
	virtual void clearFactsByModality(Abducer::ModalityType type, const Ice::Current&);
	virtual void clearAssumables(const Ice::Current&);

	virtual void addFact(const Abducer::ModalisedFormulaPtr & f, const Ice::Current&);
	virtual void addAssumable(const std::string& function, const Abducer::ModalisedFormulaPtr & f, float cost, const Ice::Current&);

	virtual Abducer::ProveResult prove(const std::vector<Abducer::MarkedQueryPtr> & g, const Ice::Current&);
	virtual std::vector<Abducer::MarkedQueryPtr> getBestProof(const Ice::Current&);

private:
	MR_Word ctx;  // Mercury type ctx

	bool haveProof;
	MR_Word curBestProof;  // Mercury type proof(ctx_modality)
};

#endif
