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

:- module rcc.

:- interface.

%:- typeclass region(T).

:- type relation
	--->	dc  % disconnected
	;	ec  % externally connected
	;	eq  % equal
	;	po  % partially overlapping
	;	tpp  % tangential proper part
	;	tppi  % tangential proper part inverse
	;	ntpp  % non-tangential proper part
	;	ntppi  % non-tangential proper part inverse
	.

	% compose(A, B, C)
	% True iff
	%    A o B = C
	% where `o' stands for the composition relation.
:- pred compose(relation, relation, relation).
:- mode compose(in, in, out) is multi.
:- mode compose(in, in, in) is semidet.
:- mode compose(out, in, in) is multi.
:- mode compose(in, out, in) is multi.

%------------------------------------------------------------------------------%

:- implementation.

:- pred any_relation(relation).
:- mode any_relation(out) is multi.
:- mode any_relation(in) is det.

any_relation(dc).
any_relation(ec).
any_relation(eq).
any_relation(po).
any_relation(tpp).
any_relation(tppi).
any_relation(ntpp).
any_relation(ntppi).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

compose(dc, dc, Any) :- any_relation(Any).
compose(dc, ec, dc).
compose(dc, ec, ec).
compose(dc, ec, po).
compose(dc, ec, tpp).
compose(dc, ec, ntpp).
compose(dc, po, dc).
compose(dc, po, ec).
compose(dc, po, po).
compose(dc, po, tpp).
compose(dc, po, ntpp).
compose(dc, tpp, dc).
compose(dc, tpp, ec).
compose(dc, tpp, po).
compose(dc, tpp, tpp).
compose(dc, tpp, ntpp).
compose(dc, ntpp, dc).
compose(dc, ntpp, ec).
compose(dc, ntpp, po).
compose(dc, ntpp, tpp).
compose(dc, ntpp, ntpp).
compose(dc, tppi, dc).
compose(dc, ntppi, dc).
compose(dc, eq, dc).

compose(ec, dc, dc).
compose(ec, dc, ec).
compose(ec, dc, po).
compose(ec, dc, tppi).
compose(ec, dc, ntppi).
compose(ec, ec, dc).
compose(ec, ec, ec).
compose(ec, ec, po).
compose(ec, ec, tpp).
compose(ec, ec, tppi).
compose(ec, ec, eq).
compose(ec, po, dc).
compose(ec, po, ec).
compose(ec, po, po).
compose(ec, po, tpp).
compose(ec, po, ntpp).
compose(ec, tpp, ec).
compose(ec, tpp, po).
compose(ec, tpp, tpp).
compose(ec, tpp, ntpp).
compose(ec, ntpp, po).
compose(ec, ntpp, tpp).
compose(ec, ntpp, ntpp).
compose(ec, tppi, dc).
compose(ec, tppi, ec).
compose(ec, ntppi, dc).
compose(ec, eq, ec).

compose(po, dc, dc).
compose(po, dc, ec).
compose(po, dc, po).
compose(po, dc, tppi).
compose(po, dc, ntppi).
compose(po, ec, dc).
compose(po, ec, ec).
compose(po, ec, po).
compose(po, ec, tppi).
compose(po, ec, ntppi).
compose(po, po, Any) :- any_relation(Any).
compose(po, tpp, po).
compose(po, tpp, tpp).
compose(po, tpp, ntpp).
compose(po, ntpp, po).
compose(po, ntpp, tpp).
compose(po, ntpp, ntpp).
compose(po, tppi, dc).
compose(po, tppi, ec).
compose(po, tppi, po).
compose(po, tppi, tppi).
compose(po, tppi, ntppi).
compose(po, ntppi, dc).
compose(po, ntppi, ec).
compose(po, ntppi, po).
compose(po, ntppi, tppi).
compose(po, ntppi, ntppi).
compose(po, eq, po).

compose(tpp, dc, dc).
compose(tpp, ec, dc).
compose(tpp, ec, ec).
compose(tpp, po, dc).
compose(tpp, po, ec).
compose(tpp, po, po).
compose(tpp, po, tpp).
compose(tpp, po, ntpp).
compose(tpp, tpp, tpp).
compose(tpp, tpp, ntpp).
compose(tpp, ntpp, ntpp).
compose(tpp, tppi, dc).
compose(tpp, tppi, ec).
compose(tpp, tppi, po).
compose(tpp, tppi, tpp).
compose(tpp, tppi, tppi).
compose(tpp, tppi, eq).
compose(tpp, ntppi, dc).
compose(tpp, ntppi, ec).
compose(tpp, ntppi, po).
compose(tpp, ntppi, tppi).
compose(tpp, ntppi, ntppi).
compose(tpp, eq, tpp).

compose(ntpp, dc, dc).
compose(ntpp, ec, dc).
compose(ntpp, po, dc).
compose(ntpp, po, ec).
compose(ntpp, po, po).
compose(ntpp, po, tpp).
compose(ntpp, po, ntpp).
compose(ntpp, tpp, ntpp).
compose(ntpp, ntpp, ntpp).
compose(ntpp, tppi, dc).
compose(ntpp, tppi, ec).
compose(ntpp, tppi, po).
compose(ntpp, tppi, tpp).
compose(ntpp, tppi, ntpp).
compose(ntpp, ntppi, Any) :- any_relation(Any).
compose(ntpp, eq, ntpp).

compose(tppi, dc, dc).
compose(tppi, dc, ec).
compose(tppi, dc, po).
compose(tppi, dc, tppi).
compose(tppi, dc, ntppi).
compose(tppi, ec, ec).
compose(tppi, ec, po).
compose(tppi, ec, tppi).
compose(tppi, ec, ntppi).
compose(tppi, po, po).
compose(tppi, po, tppi).
compose(tppi, po, ntppi).
compose(tppi, tpp, po).
compose(tppi, tpp, tpp).
compose(tppi, tpp, tppi).
compose(tppi, tpp, eq).
compose(tppi, ntpp, po).
compose(tppi, ntpp, tpp).
compose(tppi, ntpp, ntpp).
compose(tppi, tppi, tppi).
compose(tppi, tppi, ntppi).
compose(tppi, ntppi, ntppi).
compose(tppi, eq, tppi).

compose(ntppi, dc, dc).
compose(ntppi, dc, ec).
compose(ntppi, dc, po).
compose(ntppi, dc, tppi).
compose(ntppi, dc, ntppi).
compose(ntppi, ec, po).
compose(ntppi, ec, tppi).
compose(ntppi, ec, ntppi).
compose(ntppi, po, po).
compose(ntppi, po, tppi).
compose(ntppi, po, ntppi).
compose(ntppi, tpp, po).
compose(ntppi, tpp, tppi).
compose(ntppi, tpp, ntppi).
compose(ntppi, ntpp, po).
compose(ntppi, ntpp, tpp).
compose(ntppi, ntpp, ntpp).
compose(ntppi, ntpp, tppi).
compose(ntppi, ntpp, ntppi).
compose(ntppi, ntpp, eq).
compose(ntppi, tppi, ntppi).
compose(ntppi, ntppi, ntppi).
compose(ntppi, eq, ntppi).

compose(eq, dc, dc).
compose(eq, ec, ec).
compose(eq, po, po).
compose(eq, tpp, tpp).
compose(eq, ntpp, ntpp).
compose(eq, tppi, tppi).
compose(eq, ntppi, ntppi).
compose(eq, eq, eq).
