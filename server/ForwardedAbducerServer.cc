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

//#include <poll.h>  // doesn't work on Darwin
#include <sys/time.h>
#include <sys/types.h>
#include <sys/select.h>
#include <signal.h>

#include "common.h"
#include "ForwardedAbducerServer.h"

#include "Tokens.h"
#include "TermTokeniser.h"
#include "SliceToString.h"
#include "StringToSlice.h"

#include "TtyUtils.h"

#include "Logging.h"

using namespace std;
using namespace Abducer;

#include <vector>

static const size_t bufsize = 16384;

static char buf[bufsize];

ForwardedAbducerServer::ForwardedAbducerServer(pid_t abducer_pid_)
: abducer_pid(abducer_pid_)
{
	cerr << SERVER_MSG("initialising abducer context") << endl;
	cout << "init_ctx." << endl;
}

void
ForwardedAbducerServer::loadFile(const string& filename, const Ice::Current&)
{
	cerr << REQUEST_MSG("loading file [" << filename << "]") << endl;
	cout << "load_file(\"" << filename << "\")." << endl;

	if (cin) {
		cin.getline(buf, bufsize);
		cerr << WARNING_MSG("unimplemented: check success") << endl;
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
	cerr << REQUEST_MSG("clearing rules") << endl;
	cout << "clear_rules." << endl;
}

void
ForwardedAbducerServer::clearFacts(const Ice::Current&)
{
	cerr << REQUEST_MSG("clearing all facts") << endl;
	cout << "clear_facts." << endl;
}

void
ForwardedAbducerServer::clearFactsByModality(Modality mod, const Ice::Current&)
{
	cerr << REQUEST_MSG("clearing [" << modalityToString(mod) << "] facts") << endl;
	cout << "clear_facts_by_modality(" << modalityToString(mod) << "). << endl";
}

void
ForwardedAbducerServer::clearAssumables(const Ice::Current&)
{
	cerr << REQUEST_MSG("clearing assumables") << endl;
	cout << "clear_assumables." << endl;
}

void
ForwardedAbducerServer::clearAssumableFunction(const string & function, const Ice::Current&)
{
	cerr << REQUEST_MSG("clearing assumable function [" << function << "]") << endl;
	cout << "clear_assumable_function(\"" << function << "\")." << endl;
}

void
ForwardedAbducerServer::addFact(const ModalisedAtomPtr & fact, const Ice::Current&)
{
	cerr << REQUEST_MSG("adding fact [" << modalisedAtomToString(fact) << "]") << endl;
	cout << "add_fact(\"" << modalisedAtomToString(fact) << ".\")." << endl;
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
ForwardedAbducerServer::addAssumable(const string & function, const ModalisedAtomPtr & a, float cost, const Ice::Current&)
{
	cerr << REQUEST_MSG("adding assumable [" << modalisedAtomToString(a) << " / " << function << "]") << endl;
	stringstream ss;
	ss << "add_assumable(\"" << function << "\", \"" << modalisedAtomToString(a) << ".\", " << ensureFloatPortrayal(cost) << ")." << endl;

	debug(cerr << "  the request: " << ss.str());

//	cerr << tty::red << "  [unimplemented]" << tty::dcol << endl;
	cout << ss.str();
}

void
ForwardedAbducerServer::startProving(const vector<MarkedQueryPtr> & goals, const Ice::Current&)
{
	cerr << REQUEST_MSG("proving started") << endl;
	string s("prove([");

	vector<MarkedQueryPtr>::const_iterator it = goals.begin();
	while (it != goals.end()) {
		s += "\"" + modalisedAtomToString((*it)->atom) + ".\"";
		it++;
		if (it != goals.end()) {
			s += ", ";
		}
	}
	s += "]).";
	cout << s << endl;
}

vector<MarkedQueryPtr>
ForwardedAbducerServer::getBestProof(int timeout, const Ice::Current&)
{
/*
	// XXX: poll() doesn't work with stdin on Darwin!

	struct pollfd pfd[1];
	pfd[0].fd = STDIN_FILENO;
	pfd[0].events = POLLIN;

	int rc = poll(pfd, 1, timeout);
	if (rc > 1) {
		// something is on stdin -> finished
		cerr << tty::green << "  [poll] results ready" << tty::dcol << endl;
	}
	else if (rc == 0) {
		// timeout -- send SIGUSR1 to the abducer and assume that it's enough
		cerr << tty::green << "  [poll] timeout" << tty::dcol << endl;
		kill(abducer_pid, SIGUSR1);
	}
	else {
		// an error occurred
		cerr << tty::red << "  [poll] error" << tty::dcol << endl;
		return vector<MarkedQueryPtr>();
	}
*/
	cerr << REQUEST_MSG("waiting for results, timeout=" << timeout) << endl;

	fd_set readfds;
	struct timeval tv;
	struct timeval * ptv;

	if (timeout < 0) {
		ptv = NULL;
	}
	else {
		tv.tv_sec = timeout / 1000;
		tv.tv_usec = (timeout % 1000) * 1000;
		ptv = &tv;
	}

	FD_ZERO(&readfds);
	FD_SET(STDIN_FILENO, &readfds);

	int rc = select(1, &readfds, NULL, NULL, &tv);

	if (rc < 0) {
		cerr << ERROR_MSG("error in select()") << endl;
		return vector<MarkedQueryPtr>();
	}

	if (FD_ISSET(STDIN_FILENO, &readfds)) {
		// something is on stdin -> finished
		cerr << NOTIFY_MSG("results ready before timeout") << endl;
	}
	else {
		// timeout -- send SIGUSR1 to the abducer and assume that it's enough
		cerr << NOTIFY_MSG("timeout") << endl;
		kill(abducer_pid, SIGUSR1);
	}

	bool gotResults = false;

	if (cin) {
		cin.getline(buf, bufsize);
		debug(cerr << "RESPONSE: " << buf << endl);

		if (*buf == 's') {
			cerr << NOTIFY_MSG("found a proof") << endl;
			gotResults = true;
		}
		else {
			cerr << NOTIFY_MSG("no proof found") << endl;
			gotResults = false;
		}
	}

	// retrieve the proof
	if (gotResults) {
		return getBestProof();
	}
	else {
		return vector<MarkedQueryPtr>();
	}
}

/*
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
*/
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
//}

MarkedQueryPtr
markModalisedAtom(Marking mark, ModalisedAtomPtr ma)
{
	switch (mark) {
		case Proved: {
				ProvedQueryPtr q = new ProvedQuery();
				q->atom = ma;
				return q;
			}
			break;

		case Unsolved: {
				UnsolvedQueryPtr q = new UnsolvedQuery();
				q->atom = ma;
				ConstAssumabilityFunctionPtr af = new ConstAssumabilityFunction();
				af->cost = 1.0;
				q->f = af;
				return q;
			}
			break;

		case Assumed: {
				AssumedQueryPtr q = new AssumedQuery();
				q->atom = ma;
				ConstAssumabilityFunctionPtr af = new ConstAssumabilityFunction();
				af->cost = 1.0;
				q->f = af;
				return q;
			}
			break;

		case Asserted: {
				AssertedQueryPtr q = new AssertedQuery();
				q->atom = ma;
				return q;
			}
			break;

	}
	return NULL;
}

vector<MarkedQueryPtr>
ForwardedAbducerServer::getBestProof()
{
	cerr << NOTIFY_MSG("retrieving the last proof") << endl;
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

		vector<Tokens::Token *> toks = tokenise(mfStr);
		vector<Tokens::Token *>::iterator it = toks.begin();
		ModalisedAtomPtr mf = parseModalisedAtom(it);

		if (mf) {
			MarkedQueryPtr q = markModalisedAtom(mark, mf);
			if (q) {
				vect.push_back(q);
			}
			else {
				cerr << tty::red << "  NULL in parsing query" << tty::dcol << endl;
			}
		}
		else {
			cerr << tty::red << "  NULL in parsing modalised atom (in \"" << mfStr << "\")" << tty::dcol << endl;
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
