:- module enumerable.

:- interface.

:- typeclass generator(T1, T2) where [
	pred generate(T1, T2),
	mode generate(in, out) is nondet
].
