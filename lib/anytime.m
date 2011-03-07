%------------------------------------------------------------------------------%
% Copyright (C) 2010-2011 DFKI GmbH Talking Robots 
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

:- import_module bool.

:- impure pred reset_signaller is det.
:- impure pred signalled(bool::out) is det.
:- pred pure_signalled(bool::out) is det.

%------------------------------------------------------------------------------%

:- implementation.

:- pragma foreign_decl("C", "#include <stdio.h>").
:- pragma foreign_decl("C", "#include <signal.h>").

:- pragma foreign_decl("C", "extern MR_Bool anytime_sig;").
:- pragma foreign_code("C", "MR_Bool anytime_sig;").

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

:- initialise register_handler/0.

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

:- pragma foreign_decl("C", "void set_signalled(int sig_num);").
:- pragma foreign_code("C", "
void set_signalled(int sig_num)
{
	anytime_sig = MR_YES;
/*	fprintf(stderr, \"[signalled]\"\n); */
}
").

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

:- impure pred register_handler is det.
:- pragma foreign_proc("C", register_handler, [will_not_call_mercury], "
/*	fprintf(stderr, \"[register]\"\n); */

	struct sigaction new_action, old_action;
	new_action.sa_handler = set_signalled;
	new_action.sa_flags = SA_RESTART;
	sigemptyset(&new_action.sa_mask);

	sigaction(SIGUSR1, &new_action, &old_action);
/*
	signal(SIGUSR1, set_signalled);
*/
	anytime_sig = MR_NO;
").

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

:- pragma foreign_proc("C", reset_signaller, [will_not_call_mercury], "
	anytime_sig = MR_NO;
/*	fprintf(stderr, \"[reset]\"\n); */
").

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

:- pragma foreign_proc("C", signalled(Flag::out), [will_not_call_mercury], "
	Flag = anytime_sig;
	/*
	if (anytime_sig == MR_YES) {
		fprintf(stderr, \"Y\");
	}
	else {
		fprintf(stderr, \"N\");
	}
	*/
").

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

%:- import_module io, list.

:- pragma promise_pure(pure_signalled/1).

pure_signalled(Flag) :-
	impure signalled(Flag).
%	trace[io(!IO)] ( (if Flag = yes then print(stderr_stream, ".", !IO) else print(stderr_stream, ":", !IO)) ).
