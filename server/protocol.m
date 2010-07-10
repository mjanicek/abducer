:- module protocol.

:- interface.
:- import_module list.
:- import_module ctx_modality.

:- type request
	--->	init_ctx
	;	load_file(string)
	;	clear_rules
	;	clear_facts
	;	clear_facts_by_modality(ctx_modality)
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
