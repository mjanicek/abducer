#ifndef FORWARDEDABDUCERSERVER_H__
#define FORWARDEDABDUCERSERVER_H__  1

#include "weigabd.h"
#include <vector>

namespace Abducer = ::de::dfki::lt::tr::infer::weigabd::slice;

class ForwardedAbducerServer : public Abducer::AbducerServer {

public:
	ForwardedAbducerServer();

	virtual void loadFile(const std::string& filename, const Ice::Current&);

	virtual void clearRules(const Ice::Current&);
	virtual void clearFacts(const Ice::Current&);
	virtual void clearFactsByModality(Abducer::Modality mod, const Ice::Current&);
	virtual void clearAssumables(const Ice::Current&);
	virtual void clearAssumableFunction(const std::string & function, const Ice::Current&);

	virtual void addFact(const Abducer::ModalisedFormulaPtr & f, const Ice::Current&);
	virtual void addAssumable(const std::string& function, const Abducer::ModalisedFormulaPtr & f, float cost, const Ice::Current&);

	virtual Abducer::ProveResult prove(const std::vector<Abducer::MarkedQueryPtr> & g, const Ice::Current&);
	virtual std::vector<Abducer::MarkedQueryPtr> getBestProof(const Ice::Current&);
};

#endif
