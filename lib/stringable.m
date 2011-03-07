%------------------------------------------------------------------------------%
% Copyright (C) 2009-2011 DFKI GmbH Talking Robots 
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

:- module stringable.

:- interface.

:- import_module term.

:- typeclass stringable(T) where [
	func to_string(T) = string
].

:- instance stringable(string).

:- typeclass parsable(T) where [
	func from_string(string::in) = (T::out) is semidet
].

:- func det_from_string(string) = T <= parsable(T).

:- instance parsable(string).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

:- typeclass term_parsable(T) where [
	func from_term(term.term::in) = (T::out) is semidet
].

:- func det_from_term(term.term) = T <= term_parsable(T).

%------------------------------------------------------------------------------%

:- typeclass convertible(T1, T2) where [
	func convert_from(T1::in) = (T2::out) is semidet
].

:- func det_convert_from(T1) = T2 <= convertible(T1, T2).

%------------------------------------------------------------------------------%

:- implementation.

:- import_module require.
:- import_module string.

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

det_from_string(S) = Rep :-
	(if Rep0 = from_string(S)
	then Rep = Rep0
	else error("failed to parse string `" ++ S ++ "' in func det_from_string/1.")
	).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

det_from_term(Rep) = Val :-
	(if Val0 = from_term(Rep)
	then Val = Val0
	else error("failed to parse `" ++ string(Rep) ++ "' in func det_parse/1.")
	).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

det_convert_from(From) = To :-
	(if To0 = convert_from(From)
	then To = To0
	else error("failed to convert `" ++ string(From) ++ "' in func det_convert_from/1.")
	).

%------------------------------------------------------------------------------%

:- instance stringable(string) where [
	(to_string(S) = S)
].

:- instance parsable(string) where [
	(from_string(S) = S)
].
