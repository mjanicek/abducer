#include "common.h"
#include "TypeConversions.h"

#include <string>
#include <vector>
#include <cstdlib>
#include <iomanip>

extern "C" {
#include "TypeConversions_mint.mh"
#include "mercury_memory.h"
#include <unistd.h>
}

using namespace std;
using namespace Abducer;

MR_String
cc2m::string(const std::string & s)
{
	return MR_copy_string(s.c_str());
}

MR_term
cc2m::term(const TermPtr & t, MR_varset * w_vs)
{
	debug(cerr << "cc2m::term" << endl);

	switch (t->type) {

	case Function: {
			FunctionTermPtr f = FunctionTermPtr::dynamicCast(t);
			MR_term w_f;
			MR_list__term w_args;
			empty_term_list(&w_args);

			for (int i = f->args.size() - 1; i >= 0; i--) {
				MR_term w_arg = cc2m::term(f->args[i], w_vs);
				addr(w_arg);
				addr(w_args);
				cons_term_list(w_arg, w_args, &w_args);
				addr(w_args);
			}

			new_function_term(cc2m::string(f->functor), w_args, &w_f, *w_vs, w_vs);
			return w_f;
		}

	case Variable: {
			VariableTermPtr v = VariableTermPtr::dynamicCast(t);
			MR_term w_v;
			new_variable_term(cc2m::string(v->name), &w_v, *w_vs, w_vs);
			addr(w_v);
			addr(w_vs);
			return w_v;
		}

	default:
		cerr << "unknown termType" << endl;
		return 0;
	}
}

MR_atomic_formula
predicateToMercAtomicFormula(const PredicatePtr & p, MR_varset * vs)
{
	debug(cerr << "predicateToMercAtomicFormula" << endl);
	MR_atomic_formula mp;
	MR_list__term margs;
	empty_term_list(&margs);

	for (int i = p->args.size() - 1; i >= 0; i--) {
		MR_term arg = cc2m::term(p->args[i], vs);
		cons_term_list(arg, margs, &margs);
//		margs = MR_list_cons(arg, margs);
	}

	char * s = cc2m::string(p->predSym);
	new_atomic_formula(s, margs, &mp, *vs, vs);
//	delete s;

	return mp;
}

MR_mprop__ctx_modality
modalisedFormulaToMercMProp(const ModalisedFormulaPtr & p, MR_varset * vs)
{
	debug(cerr << "modalisedFormulaToMercProp" << endl);
	MR_atomic_formula maf = predicateToMercAtomicFormula(p->p, vs);
	MR_list__ctx_modality mm = modalitySeqToMercListOfModalities(p->m);

	MR_mprop__ctx_modality mprop;
	new_mprop(mm, maf, &mprop, *vs, vs);

	return mprop;
}

MR_with_cost_function__mprop__ctx_modality
withConstCostFunction(MR_mprop__ctx_modality mprop, double cost)
{
	debug(cerr << "withConstCostFunction" << endl);
	MR_with_cost_function__mprop__ctx_modality result;
	new_with_const_cost_function(mprop, cost, &result);
	return result;
}

MR_Word
agent(Agent ag) {
	MR_Word w = 0;
	switch (ag) {
		case human:
			agent_human(&w);
			break;
		case robot:
			agent_robot(&w);
			break;
	}
	return w;
}

MR_ctx_modality
modalityToMercModality(const ModalityPtr & m)
{
	debug(cerr << "modalityToMercModality" << endl);
	MR_ctx_modality mm;

	switch (m->type) {
		case Understanding:
			modality_understanding(&mm);
			break;
		case Generation:
			modality_generation(&mm);
			break;
		case Event:
			modality_event(&mm);
			break;
		case Intention:
			modality_intention(&mm);
			break;
		case Info:
			modality_info(&mm);
			break;
		case AttState:
			modality_att(&mm);
			break;
		case K: {
				KModalityPtr km = KModalityPtr::dynamicCast(m);

				switch (km->share) {
					case Private:
						modality_k_private(agent(km->ag), &mm);
						break;

					case Attribute:
						modality_k_attrib(agent(km->ag), agent(km->ag2), &mm);
						break;

					case Mutual:
						modality_k_mutual(&mm);
						break;

					default:
						cerr << "UNSUPPORTED AGENT STATUS" << endl;
				}
			}
			break;

		default:
			debug(cerr << "unsupported modality" << endl);
	}

	return mm;
}

MR_list__ctx_modality
modalitySeqToMercListOfModalities(const ModalitySeq & ms)
{
	debug(cerr << "modalitySeqToMercListOfModalities" << endl);
	MR_list__ctx_modality w_list;
	empty_ctx_modality_list(&w_list);

	vector<ModalityPtr>::const_reverse_iterator rit;
	debug(cerr << "  size = " << ms.size() << endl);
	for (rit = ms.rbegin(); rit != ms.rend(); ++rit) {
//	for (int i = ms.size() - 1; i >= 0; i--) {
		debug(cerr << "  trying" << endl);
		MR_ctx_modality w_m = modalityToMercModality(*rit);
		cons_ctx_modality_list(w_m, w_list, &w_list);
	}

	return w_list;
}

MR_Word
markedQueryToMercQuery(const MarkedQueryPtr & mq, MR_varset * w_vs)
{
	debug(cerr << "markedQueryToMercQuery" << endl);
	MR_Word w_mprop = modalisedFormulaToMercMProp(mq->body, w_vs);

	MR_Word w_query = 0;

	switch (mq->mark) {
	
	case Proved: {
			proved_query(w_mprop, &w_query);
		}
		break;

	case Unsolved: {
			UnsolvedQueryPtr uq = UnsolvedQueryPtr::dynamicCast(mq);
			MR_Word w_costfunc;
			if (uq->isConst) {
				const_cost_function(uq->constCost, &w_costfunc);
			}
			else {
				named_cost_function(cc2m::string(uq->costFunction), &w_costfunc);
			}
			unsolved_query(w_mprop, w_costfunc, &w_query);
		}
		break;
	
	case Assumed: {
			AssumedQueryPtr aq = AssumedQueryPtr::dynamicCast(mq);
			MR_Word w_costfunc;
			if (aq->isConst) {
				const_cost_function(aq->constCost, &w_costfunc);
			}
			else {
				named_cost_function(cc2m::string(aq->costFunction), &w_costfunc);
			}
			assumed_query(w_mprop, w_costfunc, &w_query);
		}
		break;
	
	case Asserted:
		{
			AssertedQueryPtr sq = AssertedQueryPtr::dynamicCast(mq);
			MR_Word w_list;
			empty_mprop_list(&w_list);

			for (int i = sq->antecedents.size() - 1; i >= 0; i--) {
				MR_ctx_modality w_m = modalisedFormulaToMercMProp(sq->antecedents[i], w_vs);
				cons_mprop_list(w_m, w_list, &w_list);
			}

			assumed_query(w_mprop, w_list, &w_query);
		}
		break;
	
	default:
		debug(cerr << "unknown marking in markedQueryToMercQuery!" << endl);
	}
	return w_query;
}

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Agent
stringToAgent(const char * s)
{
	debug(cerr << "stringToAgent" << endl);
	if (strcmp(s, "h") == 0) {
		debug(cerr << "human" << endl);
		return human;
	}
	else if (strcmp(s, "r") == 0) {
		debug(cerr << "robot" << endl);
		return robot;
	}
	else {
		debug(cerr << "unknown agent in stringToAgent: \"" << s << "\"" << endl);
		return robot;
	}
}

ModalityPtr
MR_WordToModality(MR_Word w)
{
	debug(cerr << "MR_WordToModality" << endl);
	MR_Word w_bel;

//	print_modality(w);

	if (is_modality_understanding(w)) {
		debug(cerr << "cc: understanding" << endl);
		UnderstandingModalityPtr um = new UnderstandingModality();
		um->type = Understanding;
		return um;
	}
	if (is_modality_generation(w)) {
		debug(cerr << "cc: generate" << endl);
		GenerationModalityPtr gm = new GenerationModality();
		gm->type = Generation;
		return gm;
	}
	if (is_modality_event(w)) {
		debug(cerr << "cc: event" << endl);
		EventModalityPtr em = new EventModality();
		em->type = Event;
		return em;
	}
	if (is_modality_intention(w)) {
		debug(cerr << "cc: intention" << endl);
		IntentionModalityPtr nm = new IntentionModality();
		nm->type = Intention;
		return nm;
	}
	else if (is_modality_info(w)) {
		debug(cerr << "cc: info" << endl);
		InfoModalityPtr im = new InfoModality();
		im->type = Info;
		return im;
	}
	else if (is_modality_att(w)) {
		debug(cerr << "cc: att" << endl);
		ModalityPtr am = new Modality();
		am->type = AttState;
		return am;
	}
	else if (is_modality_k(w, &w_bel)) {
		debug(cerr << "cc: k" << endl);
		KModalityPtr km = new KModality();
		km->type = K;

		char * s1;
		char * s2;

		MR_Word w_strlist;

		if (is_belief_private(w_bel, &s1)) {
			debug(cerr << "private " << s1 << endl);
			km->share = Private;
			km->ag = stringToAgent(s1);
			km->ag2 = human; // so that it isn't uninitialised
		}
		else if (is_belief_attrib(w_bel, &s1, &s2)) {
			debug(cerr << "attrib " << s1 << " -> " << s2 << endl);
			km->share = Attribute;
			km->ag = stringToAgent(s1);
			km->ag2 = stringToAgent(s2);
		}
		else if (is_belief_mutual(w_bel, &w_strlist)) {
			debug(cerr << "mutual" << endl);
			// XXX this!!
			km->share = Mutual;
			km->ag = human;
			km->ag2 = robot;
		}
		else {
			debug(cerr << "unknown belief!" << endl);
			return 0;
		}
		debug(cerr << "share = " << km->share << endl);
		debug(cerr << "ag = " << km->ag << endl);
		debug(cerr << "ag2 = " << km->ag2 << endl);
		return km;
	}
	else {
		debug(cerr << "unknown modality!" << endl);
		return 0;
	}
}

ModalitySeq
MR_WordToModalitySeq(MR_Word w_list)
{
	debug(cerr << "MR_WordToModalitySeq" << endl);
	ModalitySeq seq = vector<ModalityPtr>();

//	print_list_modalities(w_list);

	MR_Word w_iter;
	for (w_iter = w_list; !MR_list_is_empty(w_iter); w_iter = MR_list_tail(w_iter)) {
		seq.push_back(MR_WordToModality(MR_list_head(w_iter)));
	}
	return seq;
}

TermPtr
m2cc::term(MR_Word w_vs, MR_Word w_t)
{
	debug(cerr << "m2cc::term" << endl);

	char * name;
	MR_Word w_args;

	if (is_function_term(w_t, &name, &w_args)) {
		debug(cerr << "function term" << endl);
		FunctionTermPtr f = new FunctionTerm();
		f->type = Function;
		f->functor = name;
		debug(cerr << "functor = " << f->functor << endl);
		f->args = vector<TermPtr>();

		MR_Word w_iter;
		for (w_iter = w_args; !MR_list_is_empty(w_iter); w_iter = MR_list_tail(w_iter)) {
			f->args.push_back(m2cc::term(w_vs, MR_list_head(w_iter)));
		}
		return f;
	}
	else if (is_variable_term(w_vs, w_t, &name)) {
		debug(cerr << "variable term" << endl);
		VariableTermPtr v = new VariableTerm();
		v->type = Variable;
		v->name = name;
		return v;
	}
	else {
		cerr << "neither function, nor var" << endl;
		return 0;
	}
}

/*
TermPtr
MR_WordToTerm(MR_Word w_vs, MR_Word w_t)
{
	debug(cerr << "MR_WordToTerm" << endl);
	char * name;
	MR_Bool is_var;
	MR_Word w_list;
	dissect_term(w_vs, w_t, (MR_Word*)&is_var, &name, &w_list);

	TermPtr t = new Term();
	t->variable = (is_var == MR_YES);
	t->name = name;
	t->args = vector<TermPtr>();

	if (t->variable) {
		MR_Word w_iter;
		for (w_iter = w_list; !MR_list_is_empty(w_iter); w_iter = MR_list_tail(w_iter)) {
			t->args.push_back(MR_WordToTerm(w_vs, MR_list_head(w_iter)));
		}
	}

	return t;
}
*/

PredicatePtr
MR_WordToPredicate(MR_Word w_vs, MR_Word w_p)
{
	debug(cerr << "MR_WordToPredicate" << endl);
	char * predSym;
	MR_Word w_list;
	dissect_predicate(w_vs, w_p, &predSym, &w_list);

	PredicatePtr p = new Predicate();
	p->predSym = predSym;
	p->args = vector<TermPtr>();

	MR_Word w_iter;
	for (w_iter = w_list; !MR_list_is_empty(w_iter); w_iter = MR_list_tail(w_iter)) {
		p->args.push_back(m2cc::term(w_vs, MR_list_head(w_iter)));
	}

	return p;
}

ModalisedFormulaPtr
MR_WordToModalisedFormula(MR_Word w_vs, MR_Word w_mf)
{
	debug(cerr << "MR_WordToModalisedFormula" << endl);
	ModalisedFormulaPtr f = new ModalisedFormula();
	MR_Word w_m;
	MR_Word w_p;
	dissect_mprop(w_mf, &w_m, &w_p);
	f->m = MR_WordToModalitySeq(w_m);
	f->p = MR_WordToPredicate(w_vs, w_p);

	return f;
}

MarkedQueryPtr
MR_WordToMarkedQuery(MR_Word w_vs, MR_Word w_mq)
{
	debug(cerr << "MR_WordToMarkedQuery" << endl);
	MR_Word w_arg1;
	MR_Word w_arg2;

	if (is_proved_query(w_mq, &w_arg1)) {
		debug(cerr << "  cc: is_proved_query" << endl);
		ProvedQueryPtr pq = new ProvedQuery();
		pq->mark = Proved;
		pq->body = MR_WordToModalisedFormula(w_vs, w_arg1);
		return pq;
	}
	else if (is_unsolved_query(w_mq, &w_arg1, &w_arg2)) {
		debug(cerr << "  cc: is_unsolved_query" << endl);
		UnsolvedQueryPtr uq = new UnsolvedQuery();
		uq->mark = Unsolved;
		uq->body = MR_WordToModalisedFormula(w_vs, w_arg1);
		uq->isConst = true;
		uq->constCost = 1.0;
		uq->costFunction = "";
		return uq;
	}
	else if (is_assumed_query(w_mq, &w_arg1, &w_arg2)) {
		debug(cerr << "  cc: is_assumed_query" << endl);
		AssumedQueryPtr asmq = new AssumedQuery();
		asmq->mark = Assumed;
		asmq->body = MR_WordToModalisedFormula(w_vs, w_arg1);
		asmq->isConst = true;
		asmq->constCost = 1.0;
		asmq->costFunction = "";
		return asmq;
	}
	else if (is_asserted_query(w_mq, &w_arg1, &w_arg2)) {
		debug(cerr << "  cc: is_asserted_query" << endl);
		AssertedQueryPtr asrq = new AssertedQuery();
		asrq->mark = Asserted;
		asrq->body = MR_WordToModalisedFormula(w_vs, w_arg1);
		asrq->antecedents = vector<ModalisedFormulaPtr>();
		MR_Word w_iter;
		for (w_iter = w_arg2; !MR_list_is_empty(w_iter); w_iter = MR_list_tail(w_iter)) {
			asrq->antecedents.push_back(MR_WordToModalisedFormula(w_vs, MR_list_head(w_iter)));
		}
		return asrq;
	}
	else {
		debug(cerr << "unknown marked query!" << endl);
		return 0;
	}
}

vector<MarkedQueryPtr>
MR_WordToMarkedQuerySeq(MR_Word w_ctx, MR_Word w_proof)
{
	debug(cerr << "MR_WordToMarkedQuerySeq" << endl);
//	AbductiveProofPtr p = new AbductiveProof();
	vector<MarkedQueryPtr> qs = vector<MarkedQueryPtr>();
//	p->body = vector<MarkedQueryPtr>();
	
	MR_Word w_vs;
	MR_Word w_list;

	debug(cerr << "about to call dissect_proof, w_proof=" << hex << w_proof << dec << endl);

	dissect_proof(w_proof, &w_vs, &w_list);

	debug(cerr << "done dissect_proof" << endl);

	MR_Word w_iter;
	for (w_iter = w_list; !MR_list_is_empty(w_iter); w_iter = MR_list_tail(w_iter)) {
		qs.push_back(MR_WordToMarkedQuery(w_vs, MR_list_head(w_iter)));
	}

	debug(cerr << "done the vector build" << endl);

//	p->cost = cost;

	return qs;
}
