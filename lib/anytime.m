%------------------------------------------------------------------------------%
% Copyright (C) 2010 DFKI GmbH Talking Robots 
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

:- module anytime.

:- interface.

:- impure pred reset_signaller is det.
:- semipure pred signalled is semidet.

%------------------------------------------------------------------------------%

:- implementation.

:- pragma foreign_decl("C", "#include <signal.h>").

:- import_module io.
:- import_module bool.

:- mutable(signalled, bool, no, ground, [untrailed, foreign_name("C", "anytime_sig")]).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

:- initialise register_handler/0.

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

:- pragma foreign_decl("C", "void set_signalled(int sig_num);").
:- pragma foreign_code("C", "
void set_signalled(int sig_num)
{
	anytime_sig = MR_YES;
}
").

:- impure pred register_handler is det.
:- pragma foreign_proc("C", register_handler, [will_not_call_mercury],
"
	signal(SIGUSR1, set_signalled);
").

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

reset_signaller :-
	impure set_signalled(no).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

signalled :-
	semipure get_signalled(yes).
