#include "common.h"
#include "MercuryAbducerServer.h"

extern "C" {
#include "MercuryAbducerServer_mint.mh"
#include "TypeConversions_mint.mh"
#include <unistd.h>
}

#include "TypeConversions.h"
#include "TtyUtils.h"

using namespace std;
using namespace Abducer;

#include <vector>

MercuryAbducerServer::MercuryAbducerServer()
{
	cout << tty::green << "* initialising abducer context" << tty::dcol << endl;
	ctx = init_ctx();

	haveProof = false;
}

void
MercuryAbducerServer::loadFile(const string& filename, const Ice::Current&)
{
	char * s = cc2m::string(filename);
	cout << tty::green << "* loading file `" << s << "'" << tty::dcol << endl;

	MR_Word w_result;
	char * args;
	int argi;

	load_file(s, &w_result, ctx, &ctx);

	if (load_result_is_ok(w_result)) {
		cout << tty::green << "  result: ok" << tty::dcol << endl;
	}
	else if (load_result_is_file_read_error(w_result)) {
		cout << tty::red << "  file read error" << tty::dcol << endl;
		throw FileReadErrorException(filename);
	}
	else if (load_result_is_syntax_error(w_result, &args, &argi)) {
		cout << tty::red << "  syntax error: " << args << " on line " << argi << tty::dcol << endl;
		throw SyntaxErrorException(filename, args, argi);
	}
}

void
MercuryAbducerServer::clearRules(const Ice::Current&)
{
	cout << tty::green << "* clearing rules" << tty::dcol << endl;
	clear_rules(ctx, &ctx);
}

void
MercuryAbducerServer::clearFacts(const Ice::Current&)
{
	cout << tty::green << "* clearing all facts" << tty::dcol << endl;
	clear_facts(ctx, &ctx);
}

void
MercuryAbducerServer::clearFactsByModality(ModalityType type, const Ice::Current&)
{
	switch (type) {
		case Event:
			cout << tty::green << "* clearing Event facts" << tty::dcol << endl;
			clear_e_facts(ctx, &ctx);
			break;

		case Info:
			cout << tty::green << "* clearing Info facts" << tty::dcol << endl;
			clear_i_facts(ctx, &ctx);
			break;

		case AttState:
			cout << tty::green << "* clearing AttState facts" << tty::dcol << endl;
			clear_a_facts(ctx, &ctx);
			break;

		case K:
			cout << tty::green << "* clearing K facts" << tty::dcol << endl;
			clear_k_facts(ctx, &ctx);
			break;

		default:
			cout << tty::red << "* asked to clear facts with unknown modality!" << tty::dcol << endl;
			break;
	}
}

void
MercuryAbducerServer::clearAssumables(const Ice::Current&)
{
	cout << tty::green << "* clearing assumables" << tty::dcol << endl;
	clear_assumables(ctx, &ctx);
}

void
MercuryAbducerServer::addFact(const ModalisedFormulaPtr & fact, const Ice::Current&)
{
	cout << tty::green << "* adding fact: " << fact->p->predSym << "(...)" << tty::dcol << endl;

	MR_Word vs;
	new_varset(&vs);

	MR_Word mprop = modalisedFormulaToMercMProp(fact, &vs);
	add_mprop_fact(vs, mprop, ctx, &ctx);
}

void
MercuryAbducerServer::addAssumable(const string & function, const ModalisedFormulaPtr & f, float cost, const Ice::Current&)
{
	cout << tty::green << "* adding assumable: " << f->p->predSym << "(...) / " << function << tty::dcol << endl;

	MR_Word w_vs;
	new_varset(&w_vs);

	MR_Word w_mprop = modalisedFormulaToMercMProp(f, &w_vs);
	add_assumable(cc2m::string(function), w_mprop, cost, ctx, &ctx);	
}

ProveResult
MercuryAbducerServer::prove(const vector<MarkedQueryPtr> & goals, const Ice::Current&)
{
	cout << tty::green << "* proving" << tty::dcol << endl;

	MR_Word vs;
	new_varset(&vs);

	MR_Word mgs;
	empty_marked_query_list(&mgs);

	debug(cout << "  no of goals = " << goals.size() << endl);
	vector<MarkedQueryPtr>::const_reverse_iterator rit;
//	for (int i = goals.size() - 1; i >= 0; i--) {
	for (rit = goals.rbegin(); rit != goals.rend(); ++rit) {
		debug(cerr << "  doing a goal" << endl);
		MR_Word w_mq = markedQueryToMercQuery(*rit, &vs);
		//MR_Word w_mq = markedQueryToMercQuery(goals[i], vs);
		cons_marked_query_list(w_mq, mgs, &mgs);
	}
	MR_Word minitproof;
	new_proof(mgs, vs, &minitproof);

	double proofCost;

	print_ctx(ctx);
	cout << endl;

	if (prove_best(minitproof, ctx, &proofCost, &curBestProof)) {
		cout << "RESULT: proof found" << endl;
		cout << endl;
		proof_summary(curBestProof, ctx);
		haveProof = true;
		//sleep(1);
		debug(cerr << " we're still alive!" << endl);
		//sleep(1);
		return (ProofFound);
	}
	else {
		cout << "RESULT: no proof found" << endl;
		//print_ctx(ctx);
		haveProof = false;
		return (NoProofFound);
	}
}

vector<MarkedQueryPtr>
MercuryAbducerServer::getBestProof(const Ice::Current&)
{
	cout << tty::green << "* sending the last proof" << tty::dcol << endl;

	if (haveProof) {
		return MR_WordToMarkedQuerySeq(ctx, curBestProof);
	}
	else {
		cout << "ERROR: no proof" << endl;
		throw NoProofException();
	}
}

