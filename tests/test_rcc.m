:- module test_rcc.

:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

%------------------------------------------------------------------------------%

:- implementation.

:- import_module rcc, set, list, string.
:- import_module solutions.

main(!IO) :-
	solutions_set((pred(LRels::out) is multi :-
		holds(road, Rels, property1),
		set.to_sorted_list(Rels, LRels)
			), Sols),

	set.fold((pred(Sol::in, !.IO::di, !:IO::uo) is det :-
		print("* ", !IO),
		print(Sol, !IO),
		nl(!IO)
			), Sols, !IO).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

:- type region
	--->	house1
	;	house2
	;	property1
	;	property2
	;	road
	.

:- pred a_region(region::out) is multi.

a_region(house1).
a_region(house2).
a_region(property1).
a_region(property2).
a_region(road).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

:- pred holds(region::in, set(rcc.relation)::out, region::in) is multi.

holds(A, Rels, B) :-
	(if holds0(A, LRels, B)
	then Rels = set.from_list(LRels)
	else Rels = set.init
		
	).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

:- pred holds0(region::in, list(rcc.relation)::out, region::in) is semidet.

holds0(house1, [dc], house2).
holds0(house1, [tpp, ntpp], property1).
holds0(house1, [dc, ec], property2).
holds0(house1, [ec], road).
holds0(house2, [dc, ec], property1).
holds0(house2, [ntpp], property2).
holds0(house2, [ec], road).
holds0(property1, [dc, ec], property2).
holds0(road, [dc, ec, tpp, tppi, po, eq, ntpp, ntppi], property1).
holds0(road, [dc, ec, tpp, tppi, po, eq, ntpp, ntppi], property2).

