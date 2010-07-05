:- module model.

:- interface.
:- import_module string, int, map, set.
:- import_module lf, ontology.

:- typeclass accessibility(RT, T) where [
	pred exclusive(RT, T),
	mode exclusive(in, in) is semidet
].

:- import_module unit.

:- instance accessibility(unit, string).

:- type world_id(Index)
	--->	initial
	;	u(int)  % unnamed
	;	i(Index)
	.

:- type model(Index, Sort, Rel).

:- func worlds(model(I, S, R)) = map(I, S).
:- func access(model(I, S, R)) = set({R, world_id(I), world_id(I)}).
:- func props(model(I, S, R)) = map(world_id(I), set(proposition)).

:- func unnamed_world_indices(model(I, S, R)) = set(int).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

:- type model == model(string, string, string).

:- func init = model(I, S, R).

	% add_lf(M0, LF, M)
	% True iff
	%   M0 \/ model-of(LF) = M
	%
	% i.e. add LF to M0 so that it is consistent, fail if not possible.
	%
:- pred add_lf(OS, RT, model(I, S, R), lf(I, S, R), model(I, S, R))
		<= (isa_ontology(OS, S), accessibility(RT, R)).
:- mode add_lf(in, in, in, in, out) is semidet.

:- pred union(OS, RT, model(I, S, R), model(I, S, R), model(I, S, R))
		<= (isa_ontology(OS, S), accessibility(RT, R)).
:- mode union(in, in, in, in, out) is semidet.

	% satisfies(M, LF)
	% True iff
	%   M |= LF
	%
:- pred satisfies(OS, model(I, S, R), lf(I, S, R)) <= isa_ontology(OS, S).
:- mode satisfies(in, in, in) is semidet.

:- func lfs(model(I, S, R)) = set(lf(I, S, R)).

%------------------------------------------------------------------------------%

:- implementation.
:- import_module solutions, require.
:- import_module list, pair.
:- import_module utils.

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

:- instance accessibility(unit, string) where [
	(exclusive(_, _X) :- fail)
].
	
:- type model(Index, Sort, Rel)
	--->	wm(
		worlds :: map(Index, Sort),  % sort
		access :: set({Rel, world_id(Index), world_id(Index)}),  % accessibility
		props :: map(world_id(Index), set(proposition)),  % valid propositions
		next_unnamed :: int  % lowest unused index for unnamed worlds
	).

%------------------------------------------------------------------------------%

init = wm(map.init, set.init, map.init, 0).

%init = wm(map.init, set.init, map.init, 0).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

unnamed_world_indices(M) = Ints :-
	Ints = solutions_set((pred(Int::out) is nondet :-
		( member({_, u(Int), _}, M^access)
		; member({_, _, u(Int)}, M^access)
		; member(u(Int), keys(M^props))
		)
			)).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

:- pred new_unnamed_world(int::out, model(I, S, R)::in, model(I, S, R)::out) is det.

new_unnamed_world(Id, WM0, WM) :-
	Id = WM0^next_unnamed,
	WM = WM0^next_unnamed := Id + 1.

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

:- pred well_formed(OS::in, RT::in, model(I, S, R)::in) is semidet
		<= (isa_ontology(OS, S), accessibility(RT, R)).

well_formed(Ont, RT, M) :-
	UsedRels = set.to_sorted_list(solutions_set((pred(Rel::out) is nondet :-
		set.member({Rel, _, _}, M^access)
			))),
	list.all_true((pred(Rel::in) is semidet :-
		(if exclusive(RT, Rel)
		then
			Arcs = solutions_set((pred(W1-W2::out) is nondet :-
				set.member({Rel, W1, W2}, M^access)
					)),
			singleton_set(Arcs, _)
		else
			true
		)
			), UsedRels).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

:- pred rename_merge_world(OS::in, int::in, world_id(I)::in, model(I, S, R)::in,
		model(I, S, R)::out) is det <= isa_ontology(OS, S).

rename_merge_world(Ont, Old, New, WM0, WM) :-
	Reach = set.map((func({Rel, OldIdA, OldIdB}) = {Rel, NewIdA, NewIdB} :-
		(OldIdA = u(Old) -> NewIdA = New ; NewIdA = OldIdA),
		(OldIdB = u(Old) -> NewIdB = New ; NewIdB = OldIdB)
			), WM0^access),

	(if map.search(WM0^props, u(Old), OldsProps0)
	then OldsProps = OldsProps0
	else OldsProps = set.init
	),
	(if map.search(WM0^props, New, NewsProps0)
	then NewsProps = set.union(NewsProps0, OldsProps)
	else NewsProps = OldsProps
	),
	DelProps = map.delete(WM0^props, u(Old)),
	(if NewsProps = set.init
	then Props = DelProps
	else Props = map.set(DelProps, New, NewsProps)
	),

	WM = wm(WM0^worlds, Reach, Props, WM0^next_unnamed).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

:- pred enforce_access_exclusivity(OS::in, RT::in, model(I, S, R)::in, model(I, S, R)::out)
		is semidet <= (isa_ontology(OS, S), accessibility(RT, R)).

enforce_access_exclusivity(Ont, RT, M0, M) :-
	ExclRels = set.to_sorted_list(set.filter((pred({Rel, _, _}::in) is semidet :-
		exclusive(RT, Rel)
			), M0^access)),
	first_mergable_worlds(ExclRels, Res),
	(
	 	Res = merge(W2, W3Num),
		rename_merge_world(Ont, W3Num, W2, M0, M1),
		enforce_access_exclusivity(Ont, RT, M1, M)
	;
		Res = none,
		M = M0
	).

%------------------------------------------------------------------------------%

add_lf(Ont, RT, !.M, LF, !:M) :-
	add_lf0(Ont, RT, initial, _, LF, !M),
	enforce_access_exclusivity(Ont, RT, !M).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

:- pred add_lf0(OS::in, RT::in, world_id(I)::in, world_id(I)::out, lf(I, S, R)::in,
		model(I, S, R)::in, model(I, S, R)::out) is semidet
		<= (isa_ontology(OS, S), accessibility(RT, R)).

add_lf0(Ont, RT, Cur, Cur, at(of_sort(WName, Sort), LF), WM0, WM) :-
	% add the referenced world
	(if map.search(WM0^worlds, WName, OldSort)
	then
		% we've already been there
		NewSort = more_specific(Ont, OldSort, Sort)
	else
		% it's a new one
		NewSort = Sort
	),
	WM1 = WM0^worlds := map.set(WM0^worlds, WName, NewSort),
	add_lf0(Ont, RT, i(WName), _, LF, WM1, WM).

add_lf0(Ont, RT, Cur, i(WName), i(of_sort(WName, Sort)), WM0, WM) :-
	(
	 	Cur = initial,
		fail  % should we perhaps allow this?
	;
		Cur = i(WName),
		map.search(WM0^worlds, WName, OldSort),
		NewSort = more_specific(Ont, OldSort, Sort),
		WM = WM0^worlds := map.set(WM0^worlds, WName, NewSort)
	;
		Cur = u(Num),
		(if map.search(WM0^worlds, WName, OldSort)
		then NewSort = more_specific(Ont, OldSort, Sort)
		else NewSort = Sort
		),
		WM1 = WM0^worlds := map.set(WM0^worlds, WName, NewSort),
		rename_merge_world(Ont, Num, i(WName), WM1, WM)
	).

add_lf0(Ont, RT, Cur, Cur, r(Rel, LF), WM0, WM) :-
	(if
		exclusive(RT, Rel)
	then
		Accessible = solutions((pred(W::out) is nondet :-
			set.member({Rel, Cur, W}, WM0^access)
				)),
		(
			Accessible = [],
			new_unnamed_world(Num, WM0, WM1),
			WM2 = WM1^access := set.insert(WM0^access, {Rel, Cur, u(Num)}),
			add_lf0(Ont, RT, u(Num), _, LF, WM2, WM)
		;
			Accessible = [W],
			add_lf0(Ont, RT, W, _, LF, WM0, WM)
		;
			% we're already inconsistent!
			Accessible = [_|_],
			fail
		)
	else
		new_unnamed_world(Num, WM0, WM1),
		WM2 = WM1^access := set.insert(WM0^access, {Rel, Cur, u(Num)}),
		add_lf0(Ont, RT, u(Num), _, LF, WM2, WM)
	).

add_lf0(Ont, RT, Cur, Cur, p(Prop), WM0, WM) :-
	(if OldProps = map.search(WM0^props, Cur)
	then NewProps = set.insert(OldProps, Prop)
	else NewProps = set.make_singleton_set(Prop)
	),
	WM = WM0^props := map.set(WM0^props, Cur, NewProps).
	
add_lf0(Ont, RT, Cur0, Cur, and(LF1, LF2), WM0, WM) :-
	add_lf0(Ont, RT, Cur0, Cur1, LF1, WM0, WM1),
	add_lf0(Ont, RT, Cur1, Cur, LF2, WM1, WM).

%------------------------------------------------------------------------------%

	% merge M2 into M1
union(Ont, RT, M1, M2, M) :-
	ListUM2 = to_sorted_list(unnamed_world_indices(M2)),

		% rename unnamed worlds so that we don't have any name clashes
	list.foldr((pred(Int::in, Mx0::in, Mx::out) is semidet :-
		rename_merge_world(Ont, Int, u(Int + M1^next_unnamed), Mx0, Mx)
			), ListUM2, M2, M2R),

		% increase the generator counter correspondingly
	NextUnnamed = M1^next_unnamed + M2^next_unnamed,

		% merge worlds and sorts
	map_merge_op((pred(S1::in, S2::in, S::out) is semidet :-
		S = more_specific(Ont, S1, S2)
			), M1^worlds, M2R^worlds, Worlds),

		% merge accessibility relations
	set.union(M1^access, M2R^access, Reach),

		% merge propositions
	map_merge_op((pred(Ps1::in, Ps2::in, Ps::out) is det :-
		set.union(Ps1, Ps2, Ps)
			), M1^props, M2R^props, Props),

	M3 = wm(Worlds, Reach, Props, NextUnnamed),
	enforce_access_exclusivity(Ont, RT, M3, M).

%------------------------------------------------------------------------------%

satisfies(Ont, M, LF) :-
	satisfies0(Ont, initial, M, LF).

:- pred satisfies0(OS::in, world_id(I)::in, model(I, S, R)::in, lf(I, S, R)::in) is semidet
		<= isa_ontology(OS, S).

satisfies0(Ont, _WCur, M, at(of_sort(Idx, Sort), LF)) :-
	satisfies0(Ont, i(Idx), M, i(of_sort(Idx, Sort))),
	satisfies0(Ont, i(Idx), M, LF).

satisfies0(Ont, i(Idx), M, i(of_sort(Idx, LFSort))) :-
	map.search(M^worlds, Idx, Sort),
	isa(Ont, Sort, LFSort).

satisfies0(Ont, WCur, M, r(Rel, LF)) :-
	set.member({Rel, WCur, WReach}, M^access),
	satisfies0(Ont, WReach, M, LF).

satisfies0(Ont, WCur, M, p(Prop)) :-
	set.member(Prop, map.search(M^props, WCur)).

satisfies0(Ont, WCur, M, and(LF1, LF2)) :-
	satisfies0(Ont, WCur, M, LF1),
	satisfies0(Ont, WCur, M, LF2).

%------------------------------------------------------------------------------%
	
:- type mergable_result(I)
	--->	merge(world_id(I), int)  % merge 2nd into 1st
	;	none
	.

	% Fails when a accessibility relation that cannot be reduced to a partial
	% function is found.
:- pred first_mergable_worlds(list({R, world_id(I), world_id(I)})::in, mergable_result(I)::out) is semidet.

first_mergable_worlds([], none).
first_mergable_worlds([{R, W1, W2} | Rest], Result) :-
	(if
		list.find_first_map((pred({Rx, W1x, W3x}::in, W3x::out) is semidet :-
			Rx = R, W1x = W1), Rest, W3)
	then
		(W2 = u(_), W3 = u(Num) -> Result = merge(W2, Num) ;
		(W2 = i(_), W3 = u(Num) -> Result = merge(W2, Num) ;
		(W2 = u(Num), W3 = i(_) -> Result = merge(W3, Num) ;
		fail  % W2 and W3 are both indexed by nominals => non-reducible
		)))
	else
		first_mergable_worlds(Rest, Result)
	).

%------------------------------------------------------------------------------%

lfs(M) = LFs :-
	LFs = solutions_set((pred(LF::out) is nondet :-
		(
			map.member(M^props, W, SetProps),
			W = i(Idx), M^worlds^elem(Idx) = Sort,
			set.member(Prop, SetProps),
			LF = at(of_sort(Idx, Sort), p(Prop))
		;
			set.member({Rel, W1, W2}, M^access),
			W1 = i(Idx1), M^worlds^elem(Idx1) = Sort1,
			W2 = i(Idx2), M^worlds^elem(Idx2) = Sort2,
			LF = at(of_sort(Idx1, Sort1), r(Rel, i(of_sort(Idx2, Sort2))))
		)
			)).
