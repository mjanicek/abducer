#include "common.h"
#include "ForwardedAbducerServer.h"

#include "Tokens.h"
#include "TermTokeniser.h"
#include "SliceToString.h"
#include "StringToSlice.h"

#include "TtyUtils.h"

using namespace std;
using namespace Abducer;

#include <vector>

static const size_t bufsize = 8192;

static char buf[bufsize];

ForwardedAbducerServer::ForwardedAbducerServer()
{
	cerr << tty::green << "* initialising abducer context" << tty::dcol << endl;
	cout << "init_ctx." << endl;
}

void
ForwardedAbducerServer::loadFile(const string& filename, const Ice::Current&)
{
	cerr << tty::green << "* loading file `" << filename << "'" << tty::dcol << endl;
	cout << "load_file(\"" << filename << "\")." << endl;

	if (cin) {
		cin.getline(buf, bufsize);
		cerr << tty::red << "  [unimplemented: check success]" << tty::dcol << endl;
		debug(cerr << "  load file reply: " << buf << endl);
	}

/*
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
*/
}

void
ForwardedAbducerServer::clearRules(const Ice::Current&)
{
	cerr << tty::green << "* clearing rules" << tty::dcol << endl;
	cout << "clear_rules." << endl;
}

void
ForwardedAbducerServer::clearFacts(const Ice::Current&)
{
	cerr << tty::green << "* clearing all facts" << tty::dcol << endl;
	cout << "clear_facts." << endl;
}

void
ForwardedAbducerServer::clearFactsByModality(ModalityType type, const Ice::Current&)
{
	switch (type) {
		case Event:
			cerr << tty::green << "* clearing Event facts" << tty::dcol << endl;
			cerr << tty::red << "  [unimplemented]" << tty::dcol << endl;
			break;

		case Info:
			cerr << tty::green << "* clearing Info facts" << tty::dcol << endl;
			cerr << tty::red << "  [unimplemented]" << tty::dcol << endl;
			break;

		case AttState:
			cerr << tty::green << "* clearing AttState facts" << tty::dcol << endl;
			cerr << tty::red << "  [unimplemented]" << tty::dcol << endl;
			break;

		case K:
			cerr << tty::green << "* clearing K facts" << tty::dcol << endl;
//			cerr << tty::red << "  [unimplemented]" << tty::dcol << endl;
			cout << "clear_facts_by_modality(k)." << endl;
			break;

		default:
			cerr << tty::red << "* asked to clear facts with unknown modality!" << tty::dcol << endl;
			break;
	}
}

void
ForwardedAbducerServer::clearAssumables(const Ice::Current&)
{
	cerr << tty::green << "* clearing assumables" << tty::dcol << endl;
	cout << "clear_assumables." << endl;
}

void
ForwardedAbducerServer::clearAssumableFunction(const string & function, const Ice::Current&)
{
	cerr << tty::green << "* clearing assumable function: " << function << tty::dcol << endl;
	cout << "clear_assumable_function(\"" << function << "\")." << endl;
}

void
ForwardedAbducerServer::addFact(const ModalisedFormulaPtr & fact, const Ice::Current&)
{
	cerr << tty::green << "* adding fact: " << fact->p->predSym << "(...)" << tty::dcol << endl;
//	cerr << tty::red << "  [unimplemented]" << tty::dcol << endl;
	cout << "add_fact(\"" << modalisedFormulaToString(fact) << ".\")." << endl;
}

string
ensureFloatPortrayal(double n)
{
	stringstream ss;
	ss << n;
	if (ss.str().find_first_of('.') == string::npos) {
		// portrayed as an integer
		ss << ".0";
	}
	return ss.str();
}


void
ForwardedAbducerServer::addAssumable(const string & function, const ModalisedFormulaPtr & f, float cost, const Ice::Current&)
{
	cerr << tty::green << "* adding assumable: " << f->p->predSym << "(...) / " << function << tty::dcol << endl;
	stringstream ss;
	ss << "add_assumable(\"" << function << "\", \"" << modalisedFormulaToString(f) << ".\", " << ensureFloatPortrayal(cost) << ")." << endl;

	debug(cerr << "  the request: " << ss.str());

//	cerr << tty::red << "  [unimplemented]" << tty::dcol << endl;
	cout << ss.str();
}

ProveResult
ForwardedAbducerServer::prove(const vector<MarkedQueryPtr> & goals, const Ice::Current&)
{
	cerr << tty::green << "* proving" << tty::dcol << endl;
//	cerr << tty::red << "  [unimplemented]" << tty::dcol << endl;
	string s("prove([");

	vector<MarkedQueryPtr>::const_iterator it = goals.begin();
	while (it != goals.end()) {
		s += "\"" + modalisedFormulaToString((*it)->formula) + ".\"";
		it++;
		if (it != goals.end()) {
			s += ", ";
		}
	}
	s += "]).";

	cout << s << endl;

	if (cin) {
		cin.getline(buf, bufsize);
		debug(cerr << "RESPONSE: " << buf << endl);

		if (*buf == 's') {
			cerr << tty::green << "  a proof was found" << tty::dcol << endl;
			return ProofFound;
		}
		else {
			cerr << tty::green << "  no proof found" << tty::dcol << endl;
			return NoProofFound;
		}
	}

	cerr << tty::red << "  abduction error" << tty::dcol << endl;
	return Error;
/*
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
*/
}

MarkedQueryPtr
markModalisedFormula(Marking mark, ModalisedFormulaPtr mf)
{
	switch (mark) {
		case Proved: {
				ProvedQueryPtr q = new ProvedQuery();
				q->formula = mf;
				return q;
			}
			break;

		case Unsolved: {
				UnsolvedQueryPtr q = new UnsolvedQuery();
				q->formula = mf;
				ConstAssumabilityFunctionPtr af = new ConstAssumabilityFunction();
				af->cost = 1.0;
				q->f = af;
				return q;
			}
			break;

		case Assumed: {
				AssumedQueryPtr q = new AssumedQuery();
				q->formula = mf;
				ConstAssumabilityFunctionPtr af = new ConstAssumabilityFunction();
				af->cost = 1.0;
				q->f = af;
				return q;
			}
			break;

		case Asserted: {
				AssertedQueryPtr q = new AssertedQuery();
				q->formula = mf;
				return q;
			}
			break;

	}
	return NULL;
}

vector<MarkedQueryPtr>
ForwardedAbducerServer::getBestProof(const Ice::Current&)
{
	cerr << tty::green << "* retrieving the last proof" << tty::dcol << endl;
//	cerr << tty::red << "  [unimplemented]" << tty::dcol << endl;
	cout << "get_best_proof." << endl;

	int num = 0;
	cin.getline(buf, bufsize);
	num = atoi(buf);

	debug(cerr << "reading " << num << " queries" << endl);

	vector<MarkedQueryPtr> vect;

	while (cin && num > 0) {
		cin.getline(buf, bufsize);

		Marking mark;
		switch (*buf) {
			case 'A':
				mark = Assumed;
				break;
			case 'P':
				mark = Proved;
				break;
			case 'U':
			default:
				mark = Unsolved;
				break;
			case 'R':
				mark = Asserted;
				break;
		}

		string mfStr(buf+1);
		debug(cerr << mfStr << endl);

		vector<Token *> toks = tokenise(mfStr);
		vector<Token *>::iterator it = toks.begin();
		ModalisedFormulaPtr mf = parseModalisedFormula(it);

		if (mf) {
			MarkedQueryPtr q = markModalisedFormula(mark, mf);
			if (q) {
				vect.push_back(q);
			}
			else {
				cerr << tty::red << "  NULL in parsing query" << tty::dcol << endl;
			}
		}
		else {
			cerr << tty::red << "  NULL in parsing modalised formula" << tty::dcol << endl;
		}

		num--;
	}

	return vect;

//	throw NoProofException();

/*
	if (haveProof) {
		return MR_WordToMarkedQuerySeq(ctx, curBestProof);
	}
	else {
		cout << "ERROR: no proof" << endl;
		throw NoProofException();
	}
*/
}
