%------------------------------------------------------------------------------%
% Copyright (C) 2009-2010 DFKI GmbH Talking Robots 
% Miroslav Janicek (miroslav.janicek@dfki.de) 
%
% This library is free software; you can redistribute it and/or
% modify it under the terms of the GNU Lesser General Public License 
% as published by the Free Software Foundation; either version 2.1 of
% the License, or (at your option) any later version.
%
% This library is distributed in the hope that it will be useful, but
% WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
% Lesser General Public License for more details.
%
% You should have received a copy of the GNU Lesser General Public
% License along with this program; if not, write to the Free Software
% Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
% 02111-1307, USA.
%------------------------------------------------------------------------------%

:- module belief_model.

:- interface.

:- import_module set, int, map, int.
:- import_module lf.
:- import_module stf, model, ontology.
:- import_module stringable.

:- type agent
	--->	robot
	;	human
	.

:- instance stringable(agent).
:- instance parsable(agent).

:- type belief
	--->	private(agent)
	;	attrib(agent, agent)
	;	mutual(set(agent))
	.

:- type foreground
	--->	com
	.

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

:- type belief_model(I, S, R)
	--->	bm(
		k :: map(int, {stf, belief, lf(I, S, R)}),
		fg :: set(int),
		next_index :: int
	).

:- func init = belief_model(I, S, R).

:- pred add_lf_to_k(OS::in, stf::in, belief::in, lf(I, S, R)::in, int::out,
		belief_model(I, S, R)::in, belief_model(I, S, R)::out) is det <= isa_ontology(OS, S).

:- pred foreground(OS::in, int::in, belief_model(I, S, R)::in, belief_model(I, S, R)::out) is det
		<= isa_ontology(OS, S). 

:- pred k_fact(OS::in, RT::in, belief_model(I, S, R)::in, stf::out, belief::out, lf(I, S, R)::out) is nondet
		<= (isa_ontology(OS, S), accessibility(RT, R)).

:- pred k_model(OS::in, RT::in, belief_model(I, S, R)::in, stf::in, belief::in, model(I, S, R)::out) is semidet
		<= (isa_ontology(OS, S), accessibility(RT, R)).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

:- func fg_anchors(belief_model(I, S, R)) = set(I).

:- pred att_model(OS::in, RT::in, belief_model(I, S, R)::in, model(I, S, R)::out) is det
		<= (isa_ontology(OS, S), accessibility(RT, R)).

:- func min_dist(OS::in, model(I, S, R)::in, I::in, I::in) = (int::out) is semidet <= isa_ontology(OS, S).

:- func min_dist_from_set(OS::in, model(I, S, R)::in, set(I)::in, I::in) = (int::out) is semidet
		<= isa_ontology(OS, S).

%------------------------------------------------------------------------------%

:- implementation.
:- import_module require, solutions.
:- import_module map, list, lf_io.
:- import_module string.

init = bm(map.init, set.init, 1).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

:- instance stringable(agent) where [
	func(to_string/1) is agent_to_string
].

:- instance parsable(agent) where [
	func(from_string/1) is string_to_agent
].

:- pred agent_as_string(agent, string).
:- mode agent_as_string(in, out) is det.
:- mode agent_as_string(out, in) is semidet.

agent_as_string(human, "h").
agent_as_string(robot, "r").

:- func agent_to_string(agent) = string.

agent_to_string(A) = S :- agent_as_string(A, S).

:- func string_to_agent(string::in) = (agent::out) is semidet.

string_to_agent(S) = A :- agent_as_string(A, S).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

	% error if multiple K's found for the V
:- func reverse_search(map(K, V)::in, V::in) = (K::out) is semidet.

reverse_search(Map, V) = K :-
	map.search(reverse_map(Map), V, Ks),
	(Ks = set.init -> fail ;
	(singleton_set(Ks, K0) -> K = K0 ;
	error("multiple keys in func reverse_search/2")
	)).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

add_lf_to_k(_Ont, STF, Bel, LF, Index, !BM) :-
	(if OldIndex = reverse_search(!.BM^k, {STF, Bel, LF})
	then
		% already there
		Index = OldIndex
	else
		Index = !.BM^next_index,
		!:BM = !.BM^k := map.set(!.BM^k, Index, {STF, Bel, LF}),
		!:BM = !.BM^next_index := !.BM^next_index + 1
	).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

foreground(_Ont, Index, !BM) :-
	(if map.search(!.BM^k, Index, _)
	then !:BM = !.BM^fg := set.insert(!.BM^fg, Index)
	else error("foregrounding a non-existent belief/task")
	).

%------------------------------------------------------------------------------%

:- type mbm(I, S, R) == map(stf, map(belief, model(I, S, R))).

:- func add_lf_to_mbm(OS, RT, stf, belief, lf(I, S, R), mbm(I, S, R)) = mbm(I, S, R)
		<= (isa_ontology(OS, S), accessibility(RT, R)).

add_lf_to_mbm(Ont, RT, STF, Bel, LF, MBM0) = MBM :-
	(if map.search(MBM0, STF, BelMap0)
	then BelMap = BelMap0
	else BelMap = map.init
	),
	(if map.search(BelMap, Bel, MFound)
	then M0 = MFound
	else M0 = model.init
	),
	(if add_lf(Ont, RT, M0, LF, M)
	then
		map.set(BelMap, Bel, M, NewBelMap),
		map.set(MBM0, STF, NewBelMap, MBM)
	else
		error("inconsistent addition of LF \"" ++ string(LF) ++ "\" in add_lf_to_mbm")
	).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

:- func get_mbm(OS, RT, set({stf, belief, lf(I, S, R)})) = mbm(I, S, R)
		<= (isa_ontology(OS, S), accessibility(RT, R)).

get_mbm(Ont, RT, Set) = MBM :-
	MBM = set.fold((func({STF, Bel, LF}, MBM0) = add_lf_to_mbm(Ont, RT, STF, Bel, LF, MBM0)), Set, map.init).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

k_fact(Ont, RT, BM, STF, Bel, LF) :-
	MBM = get_mbm(Ont, RT, set.from_list(map.values(BM^k))),
	map.member(MBM, STF, BelMap),
	map.member(BelMap, Bel, _M),
	k_model(Ont, RT, BM, STF, Bel, M),
	set.member(LF, lfs(M)).

%------------------------------------------------------------------------------%

k_model(Ont, RT, BM, STF, Bel, M) :-
	MBM = get_mbm(Ont, RT, set.from_list(map.values(BM^k))),
	map.search(MBM, STF, BelMap),
	(
		Bel = private(Ag),
		map.search(BelMap, Bel, M0),

			% all models of mutual beliefs this agent is member of
		solutions_set((pred(MutM::out) is nondet :-
			map.member(BelMap, mutual(Ags), MutM),
			member(Ag, Ags)
				), MutMs),

			% union with the private belief
		set.fold((pred(M2::in, M1::in, M3::out) is semidet :-
			model.union(Ont, RT, M1, M2, M3)
				), MutMs, M0, M)
	;
		Bel = mutual(_),
		map.search(BelMap, Bel, M)
	;
		Bel = attrib(_, _),
		map.search(BelMap, Bel, M)
	).

%------------------------------------------------------------------------------%

fg_anchors(BM) = As :-
	set.fold((pred(LFIdx::in, A0::in, A::out) is det :-
		map.lookup(BM^k, LFIdx, {_STF, _Bel, LF}),
		(if LF = at(of_sort(WName, _Sort), _)
		then set.insert(A0, WName, A)
		else A = A0
		)
			), BM^fg, set.init, As).

:- pred dist(OS::in, model(I, S, R)::in, world_id(I)::in, world_id(I)::in, int::in, int::out) is nondet
		<= isa_ontology(OS, S).

dist(_Ont, _M, W1, W1, D, D).
dist(Ont, M, W1, W2, D0, D) :-
	W1 \= W2,  % what about reflexive rels?
	set.member({_Rel, W1, W3}, M^access),
	W3 \= W1,
	dist(Ont, M, W3, W2, D0+1, D).

min_dist(Ont, M, W1, W2) = MinD :-
	Dists = solutions_set((pred(Dist::out) is nondet :-
		dist(Ont, M, i(W1), i(W2), 0, Dist)
			)),
	[MinD|_] = to_sorted_list(Dists).

min_dist_from_set(Ont, M, SW, W2) = MinWD :-
	WDists = solutions_set((pred(WDist::out) is nondet :-
		member(W1, SW),
		WDist = min_dist(Ont, M, W1, W2)
			)),
	[MinWD|_] = to_sorted_list(WDists).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

att_model(Ont, RT, BM, M) :-
	set.fold((pred(LFIdx::in, Ma0::in, Ma::out) is det :-
		(if map.search(BM^k, LFIdx, {_STF, _Bel, LF})
		then
			(if add_lf(Ont, RT, Ma0, LF, Ma1)
			then Ma = Ma1
			else error("inconsistency in att_model")
			)
		else Ma = Ma0
		)
			), BM^fg, model.init, M).
