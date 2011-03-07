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

:- module tty.

:- interface.

:- type ansi_sequence
	--->	reset
	;	bold
	;	black
	;	red
	;	green
	;	yellow
	;	blue
	;	magenta
	;	cyan
	;	white
	.

:- func totty(ansi_sequence) = string.

%------------------------------------------------------------------------------%

:- implementation.

totty(reset) = "\033[0m".
totty(bold) = "\033[1m".
totty(black) = "\033[30m".
totty(red) = "\033[31m".
totty(green) = "\033[32m".
totty(yellow) = "\033[33m".
totty(blue) = "\033[34m".
totty(magenta) = "\033[35m".
totty(cyan) = "\033[36m".
totty(white) = "\033[37m".
