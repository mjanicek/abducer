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

% NOTE: the original taken from the Mercury standard library, which is LGPL'd

:- module lang_ops.
:- interface.

:- import_module ops.

:- type wabd_op_table.
:- instance op_table(wabd_op_table).

:- func init_wabd_op_table = (wabd_op_table::uo) is det.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module list.

:- type wabd_op_table
    --->    wabd_op_table.

init_wabd_op_table = wabd_op_table.

:- instance ops.op_table(wabd_op_table) where [
    pred(ops.lookup_infix_op/5) is lookup_wabd_infix_op,
    pred(ops.lookup_prefix_op/4) is lookup_wabd_prefix_op,
    pred(ops.lookup_binary_prefix_op/5) is lookup_wabd_binary_prefix_op,
    pred(ops.lookup_postfix_op/4) is lookup_wabd_postfix_op,
    pred(ops.lookup_op/2) is lookup_wabd_op,
    pred(ops.lookup_op_infos/4) is lookup_wabd_op_infos,
    pred(ops.lookup_operator_term/4) is lookup_wabd_operator_term,
    func(ops.max_priority/1) is wabd_max_priority,
    func(ops.arg_priority/1) is wabd_arg_priority
].

:- pred lookup_wabd_infix_op(wabd_op_table::in, string::in,
    ops.priority::out, ops.assoc::out, ops.assoc::out) is semidet.

lookup_wabd_infix_op(_OpTable, Name, Priority,
        LeftAssoc, RightAssoc) :-
    op_table(Name, Info),
    (
        Info = op_info(Class, PriorityPrime),
        Class = infix(LeftAssocPrime, RightAssocPrime)
    ->
        LeftAssoc = LeftAssocPrime,
        RightAssoc = RightAssocPrime,
        Priority = PriorityPrime
    ;
		fail
	).

:- pred lookup_wabd_prefix_op(wabd_op_table::in,
    string::in, ops.priority::out, ops.assoc::out) is semidet.

lookup_wabd_prefix_op(_OpTable, Name, Priority, LeftAssoc) :-
    op_table(Name, Info),
    ( Info = op_info(prefix(LeftAssocPrime), PriorityPrime) ->
        LeftAssoc = LeftAssocPrime,
        Priority = PriorityPrime
    ; fail
    ).

:- pred lookup_wabd_binary_prefix_op(wabd_op_table::in, string::in,
    ops.priority::out, ops.assoc::out, ops.assoc::out) is semidet.

lookup_wabd_binary_prefix_op(_OpTable, Name, Priority,
        LeftAssoc, RightAssoc) :-
    op_table(Name, Info),
    (
        Info = op_info(Class, PriorityPrime),
        Class = binary_prefix(LeftAssocPrime, RightAssocPrime)
    ->
        LeftAssoc = LeftAssocPrime,
        RightAssoc = RightAssocPrime,
        Priority = PriorityPrime
    ;
		fail
    ).

:- pred lookup_wabd_postfix_op(wabd_op_table::in,
    string::in, ops.priority::out, ops.assoc::out) is semidet.

lookup_wabd_postfix_op(_OpTable, Name, Priority, LeftAssoc) :-
    op_table(Name, Info),
    ( Info = op_info(postfix(LeftAssocPrime), PriorityPrime) ->
        LeftAssoc = LeftAssocPrime,
        Priority = PriorityPrime
    ;
		fail
    ).

:- pred lookup_wabd_op(wabd_op_table::in, string::in) is semidet.

lookup_wabd_op(_OpTable, Name) :-
    op_table(Name, _).

:- pred lookup_wabd_op_infos(wabd_op_table::in, string::in,
    op_info::out, list(op_info)::out) is semidet.

lookup_wabd_op_infos(_OpTable, Name, Info, []) :-
    op_table(Name, Info).

:- pred lookup_wabd_operator_term(wabd_op_table::in,
    ops.priority::out, ops.assoc::out, ops.assoc::out) is det.

    % Left associative, lower priority than everything except record syntax.
lookup_wabd_operator_term(_OpTable, 50, y, x).  % was 120

:- func wabd_max_priority(wabd_op_table) = ops.priority.

wabd_max_priority(_Table) = 1200.

:- func wabd_arg_priority(wabd_op_table) = ops.priority.

    % This needs to be less than the priority of the ','/2 operator.
wabd_arg_priority(_Table) = 999.

:- pred op_table(string::in, op_info::out) is semidet.

op_table(Op, Info) :-
	( Op = "<-",    Info = op_info(infix(x, x), 1200)
	; Op = ",",     Info = op_info(infix(x, y), 1000)
	; Op = ":",     Info = op_info(infix(y, x), 120)
	; Op = "/",     Info = op_info(infix(y, x), 400)  % 400
	; Op = "->",    Info = op_info(infix(x, y), 500)  % 1050
	; Op = "~",     Info = op_info(prefix(y), 900)
	; Op = "?",     Info = op_info(prefix(y), 130)  % 1100, 100

	; Op = "=",     Info = op_info(infix(x, x), 100)  % 700
	; Op = "\\=",   Info = op_info(infix(x, x), 100)  % 700

%	; Op = "<<",    Info = op_info(infix(y, x), 100)
	; Op = "::",    Info = op_info(infix(x, x), 90)
	; Op = "^",     Info = op_info(infix(x, y), 450)

%	; Op = "+",     Info = op_info(infix(y, x), 500)
%	; Op = "-",     Info = op_info(infix(y, x), 500)
%	; Op = ":-",    Info = op_info(infix(x, x), 1200)
%	; Op = "^",     Info = op_info(infix(x, y), 99)
%	; Op = "*",     Info = op_info(infix(y, x), 400)
%	; Op = "**",    Info = op_info(infix(x, y), 200)
%	; Op = "-->",   Info = op_info(infix(x, x), 1200)
%	; Op = "//",    Info = op_info(infix(y, x), 400)
%	; Op = "/\\",   Info = op_info(infix(y, x), 500)
%	; Op = ";",     Info = op_info(infix(x, y), 1100)
%	; Op = "<",     Info = op_info(infix(x, x), 700)
%	; Op = "<<",    Info = op_info(infix(y, x), 400)
%	; Op = "=..",   Info = op_info(infix(x, x), 700)
%	; Op = "=:=",   Info = op_info(infix(x, x), 700)    % (*)
%	; Op = "=<",    Info = op_info(infix(x, x), 700)
%	; Op = "==",    Info = op_info(infix(x, x), 700)    % (*)
%	; Op = "=\\=",  Info = op_info(infix(x, x), 700)    % (*)
%	; Op = ">",     Info = op_info(infix(x, x), 700)
%	; Op = ">=",    Info = op_info(infix(x, x), 700)
%	; Op = ">>",    Info = op_info(infix(y, x), 400)
%	; Op = "?-",    Info = op_info(prefix(x), 1200)     % (*)
%	; Op = "@<",    Info = op_info(infix(x, x), 700)
%	; Op = "@=<",   Info = op_info(infix(x, x), 700)
%	; Op = "@>",    Info = op_info(infix(x, x), 700)
%	; Op = "@>=",   Info = op_info(infix(x, x), 700)
%	; Op = "\\",    Info = op_info(prefix(x), 200)
%	; Op = "\\+",   Info = op_info(prefix(y), 900)
%	; Op = "\\/",   Info = op_info(infix(y, x), 500)
%	; Op = "\\=",   Info = op_info(infix(x, x), 700)
%	; Op = "\\==",  Info = op_info(infix(x, x), 700)    % (*)
%	; Op = "div",   Info = op_info(infix(y, x), 400)
%	; Op = "is",    Info = op_info(infix(x, x), 701)    % ISO: prec 700
%	; Op = "mod",   Info = op_info(infix(x, x), 400)
%	; Op = "rem",   Info = op_info(infix(x, x), 400)
%	; Op = "~",     Info = op_info(prefix(y), 900)     % (*)
%	; Op = "~=",    Info = op_info(infix(x, x), 700)    % (*)
%	; Op = "and",   Info = op_info(infix(x, y), 720)
%	; Op = "or",    Info = op_info(infix(x, y), 740)
%	; Op = "rule",  Info = op_info(prefix(x), 1199)
%	; Op = "when",  Info = op_info(infix(x, x), 900)    % (*)
%	; Op = "where", Info = op_info(infix(x, x), 1175)   % (*)
%	; Op = "<=",    Info = op_info(infix(x, y), 920)
%	; Op = "<=>",   Info = op_info(infix(x, y), 920)
%	; Op = "=>",    Info = op_info(infix(x, y), 920)
%	; Op = "all",   Info = op_info(binary_prefix(x, y), 950)
%	; Op = "some",  Info = op_info(binary_prefix(x, y), 950)
%	; Op = "if",    Info = op_info(prefix(x), 1160)
%	; Op = "then",  Info = op_info(infix(x, x), 1150)
%	; Op = "else",  Info = op_info(infix(x, y), 1170)
%	; Op = "catch", Info = op_info(infix(x, y), 1180)
%	; Op = "catch_any", Info = op_info(infix(x, y), 1190)
%	; Op = "not",   Info = op_info(prefix(y), 900)
%	; Op = "pred",  Info = op_info(prefix(x), 800)
%	; Op = "!",                 Info = op_info(prefix(x), 40)
%	; Op = "!.",                Info = op_info(prefix(x), 40)
%	; Op = "!:",                Info = op_info(prefix(x), 40)
%	; Op = "&",                 Info = op_info(infix(x, y), 1025)
%	; Op = "++",                Info = op_info(infix(x, y), 500)
%	; Op = "--",                Info = op_info(infix(y, x), 500)
%	; Op = "--->",              Info = op_info(infix(x, y), 1179)
%	; Op = ".",                 Info = op_info(infix(y, x), 10)
%	; Op = "..",                Info = op_info(infix(x, x), 550)
%	; Op = "::",                Info = op_info(infix(x, x), 1175)
%	; Op = ":=",                Info = op_info(infix(x, x), 650)
%	; Op = "==>",               Info = op_info(infix(x, x), 1175)
%	; Op = "=^",                Info = op_info(infix(x, x), 650)
%	; Op = "@",                 Info = op_info(infix(x, x), 90)
%	; Op = "or_else",           Info = op_info(infix(x, y), 1100)
%	; Op = "end_module",        Info = op_info(prefix(x), 1199)
%	; Op = "event",             Info = op_info(prefix(x), 100)
%	; Op = "finalise",          Info = op_info(prefix(x), 1199)
%	; Op = "finalize",          Info = op_info(prefix(x), 1199)
%	; Op = "func",              Info = op_info(prefix(x), 800)
%	; Op = "import_module",     Info = op_info(prefix(x), 1199)
%	; Op = "impure",            Info = op_info(prefix(y), 800)
%	; Op = "include_module",    Info = op_info(prefix(x), 1199)
%	; Op = "initialise",        Info = op_info(prefix(x), 1199)
%	; Op = "initialize",        Info = op_info(prefix(x), 1199)
%	; Op = "inst",              Info = op_info(prefix(x), 1199)
%	; Op = "instance",          Info = op_info(prefix(x), 1199)
%	; Op = "mode",              Info = op_info(prefix(x), 1199)
%	; Op = "module",            Info = op_info(prefix(x), 1199)
%	; Op = "pragma",            Info = op_info(prefix(x), 1199)
%	; Op = "promise",           Info = op_info(prefix(x), 1199)
%	; Op = "semipure",          Info = op_info(prefix(y), 800)
%	; Op = "solver",            Info = op_info(prefix(y), 1181)
%	; Op = "type",              Info = op_info(prefix(x), 1180)
%	; Op = "typeclass",         Info = op_info(prefix(x), 1199)
%	; Op = "use_module",        Info = op_info(prefix(x), 1199)
	).
