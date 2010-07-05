:- module protocol.

:- interface.
:- import_module list.

:- type modality
	--->	k
	.

:- type request
	--->	init_ctx
	;	load_file(string)
	;	clear_rules
	;	clear_facts
	;	clear_facts_by_modality(modality)
	;	clear_assumables
	;	clear_assumable_function(string)
	;	add_fact(string)
	;	add_assumable(string, string, float)
	;	prove(list(string))
	;	get_best_proof
	.

:- pred is_request(request::in) is det.

%------------------------------------------------------------------------------%

:- implementation.

is_request(_).
