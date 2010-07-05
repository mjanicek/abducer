:- module modality.

:- interface.

:- import_module list.

:- typeclass modality(T) where [
	func axiom = T,
	func compose(T::in, T::in) = (T::out) is semidet
].

:- func compose_list(list(T)) = list(T) <= modality(T).

:- pred match(list(T)::in, list(T)::in) is semidet <= modality(T).

%------------------------------------------------------------------------------%

:- implementation.

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

	% axiom has S4 semantics:
	%  (K) ... [](B -> A) -> ([]B -> []A)
	%  (T) ... []A -> A
	%  (4) ... []A -> [][]A
	%
	% plus specialisation:
	%  []A -> [a]A
	%    for every a

match([], []).
match([H|TL], [H|TR]) :- match(TL, TR).

match(L, [axiom|TR]) :- match([axiom|TR], L).  % symmetry of the matching relation
match([axiom|TL], R) :- match(TL, R).  % (T)
match([axiom|TL], [_|TR]) :- match([axiom|TL], TR).  % (4) + specialistaion

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

compose_list([]) = [].
compose_list([H]) = [H].

compose_list([H,I|T]) = C :-
	(if compose(H, I) = J
	then C = [J|compose_list(T)]
	else C = [H, I|compose_list(T)]
	).
