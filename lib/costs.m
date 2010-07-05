% $Id: costs.m 3618 2009-10-15 09:24:49Z janicek $

:- module costs.

:- interface.

:- type cost_function_name == string.

:- type cost_function
	--->	f(cost_function_name)  % named cost function
	;	const(float)  % constant cost function
	;	not_assumable  % not an assumable predicate
	.

:- func cost_function_to_string(cost_function) = string.

%------------------------------------------------------------------------------%

:- implementation.

:- import_module string.

cost_function_to_string(f(S)) = S.
cost_function_to_string(const(Float)) = string.float_to_string(Float).
cost_function_to_string(not_assumable) = "T".
