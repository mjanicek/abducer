:- module ling.

:- interface.
:- import_module string.

:- type gender
	--->	masc
	;	fem
	;	neut
	.

:- pred noun_gender(string, gender).
:- mode noun_gender(in, out) is semidet.

%------------------------------------------------------------------------------%

:- implementation.

noun_gender("object", neut).
noun_gender("thing", neut).
noun_gender("colour", neut).
