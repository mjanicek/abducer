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

:- module protocol_loading.

:- interface.

:- import_module pair.
:- import_module varset.
:- import_module lang, assumability.
:- import_module ctx_modality.
:- import_module abduction.
:- import_module protocol.

:- pred proto_modality(protocol.modality, ctx_modality).
:- mode proto_modality(in, out) is det.
:- mode proto_modality(out, in) is det.
:- func modality_from_proto(protocol.modality) = ctx_modality.
:- func modality_to_proto(ctx_modality) = protocol.modality.

:- pred term_from_protocol(protocol.term::in, lang.term::out, varset::in, varset::out) is semidet.
:- pred atom_from_protocol(protocol.atom::in, lang.atom::out, varset::in, varset::out) is semidet.
:- pred matom_from_protocol(protocol.modalised_atom::in, matom(ctx_modality)::out, varset::in, varset::out) is semidet.
:- pred mrule_from_protocol(protocol.modalised_rule::in, mrule(ctx_modality)::out, varset::in, varset::out) is semidet.
:- pred query_from_protocol(protocol.marked_query::in, query(ctx_modality)::out, varset::in, varset::out) is semidet.

:- pred assumability_function_from_protocol(protocol.assumability_function::in, assumability.assumability_function::out) is semidet.

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

:- pred proof_search_method_from_protocol(protocol.proof_search_method::in, abduction.proof_search_method::out(abduction.proof_search_method_inst)) is semidet.

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

:- func cost_goal_to_proto(pair(float, goal(ctx_modality))) = protocol.proof.
:- func query_to_proto(varset, query(ctx_modality)) = protocol.marked_query.
:- func assumability_function_to_proto(assumability.assumability_function) = protocol.assumability_function.
:- func matom_to_proto(varset, matom(ctx_modality)) = protocol.modalised_atom.
:- func atom_to_proto(varset, lang.atom) = protocol.atom.
:- func term_to_proto(varset, lang.term) = protocol.term.

%------------------------------------------------------------------------------%

:- implementation.

:- import_module map, list, maybe.

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

proto_modality(modality_any, any).
proto_modality(modality_truth, truth).
proto_modality(modality_event, evt).
proto_modality(modality_belief, belief).
proto_modality(modality_intention, intention).
proto_modality(modality_attention, attention).
proto_modality(modality_understanding, understanding).
proto_modality(modality_generation, generation).

modality_from_proto(PM) = M :-
	proto_modality(PM, M).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

term_from_protocol(term(term_term_type_variable, no, [], yes(VarName)), v(Var), VS0, VS) :-
	(if OldVar = map.search(varset.create_name_var_map(VS0), VarName)  % assumes a single scope
	then Var = OldVar, VS = VS0
	else varset.new_named_var(VS0, VarName, Var, VS)
	).

term_from_protocol(term(term_term_type_function, yes(Functor), PArgs, no), t(Functor, Args), VS0, VS) :-
	list.map_foldl(term_from_protocol, PArgs, Args, VS0, VS).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

atom_from_protocol(atom(PredSym, PArgs), p(PredSym, Args), VS0, VS) :-
	list.map_foldl(term_from_protocol, PArgs, Args, VS0, VS).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

matom_from_protocol(modalised_atom(PM, PA), m(M, A), VS0, VS) :-
	M = list.map(modality_from_proto, PM),
	atom_from_protocol(PA, A, VS0, VS).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

mrule_from_protocol(modalised_rule(PHead, PAntes), m([], Antes-std(Head)), !VS) :-
	matom_from_protocol(PHead, Head, !VS),
	list.map_foldl(antecedent_from_protocol, PAntes, Antes, !VS).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

:- pred antecedent_from_protocol(protocol.antecedent::in, rule_antecedent(ctx_modality)::out, varset::in, varset::out) is semidet.

antecedent_from_protocol(protocol.antecedent(antecedent_type_assumable, PMAtom, yes(PAF)),
		std(cf(MAtom, AF)), !VS) :-
	matom_from_protocol(PMAtom, MAtom, !VS),
	assumability_function_from_protocol(PAF, AF).

antecedent_from_protocol(protocol.antecedent(antecedent_type_asserted, PMAtom, no),
		test(prop(MAtom)), !VS) :-
	matom_from_protocol(PMAtom, MAtom, !VS).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

query_from_protocol(marked_query(PMAtom, marked_query_marking_unsolved, yes(PAssFunc)), unsolved(MAtom, AssFunc), VS0, VS) :-
	matom_from_protocol(PMAtom, MAtom, VS0, VS),
	assumability_function_from_protocol(PAssFunc, AssFunc).

query_from_protocol(marked_query(PMAtom, marked_query_marking_proved, no), proved(MAtom), VS0, VS) :-
	matom_from_protocol(PMAtom, MAtom, VS0, VS).

query_from_protocol(marked_query(PMAtom, marked_query_marking_assumed, yes(PAssFunc)), assumed(MAtom, AssFunc), VS0, VS) :-
	matom_from_protocol(PMAtom, MAtom, VS0, VS),
	assumability_function_from_protocol(PAssFunc, AssFunc).

query_from_protocol(marked_query(PMAtom, marked_query_marking_asserted, no), asserted(prop(MAtom)), VS0, VS) :-
	matom_from_protocol(PMAtom, MAtom, VS0, VS).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

assumability_function_from_protocol(assumability_function(assumability_function_function_type_notassumable, no, no), not_assumable).
assumability_function_from_protocol(assumability_function(assumability_function_function_type_const, yes(Cost), no), const(Cost)).
assumability_function_from_protocol(assumability_function(assumability_function_function_type_named, no, yes(Name)), f(Name)).

%------------------------------------------------------------------------------%

proof_search_method_from_protocol(proof_search_method(proof_search_method_type_dfs, _, _, _), unbounded_dfs).
proof_search_method_from_protocol(proof_search_method(proof_search_method_type_boundeddfs, yes(MaxBound), _, _), bounded_dfs(MaxBound)).
proof_search_method_from_protocol(proof_search_method(proof_search_method_type_iddfs, _, yes(InitBound), yes(Multiplier)), iddfs(InitBound, multiply_cost(Multiplier))).

%------------------------------------------------------------------------------%

cost_goal_to_proto(Cost-vs(Qs, VS0)) = proof(list.map(query_to_proto(VS), Qs), Cost) :-
	varset.ensure_unique_names(list.map(fst, varset.var_name_list(VS0)), "_", VS0, VS).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

query_to_proto(VS, unsolved(MAtom, AssFunc)) = marked_query(matom_to_proto(VS, MAtom), marked_query_marking_unsolved, yes(assumability_function_to_proto(AssFunc))).
query_to_proto(VS, proved(MAtom)) = marked_query(matom_to_proto(VS, MAtom), marked_query_marking_proved, no).
query_to_proto(VS, assumed(MAtom, AssFunc)) = marked_query(matom_to_proto(VS, MAtom), marked_query_marking_assumed, yes(assumability_function_to_proto(AssFunc))).
query_to_proto(VS, asserted(MTest)) = marked_query(matom_to_proto(VS, MAtom), marked_query_marking_asserted, no) :-
	( MTest = prop(MAtom)
	; MTest = impl(_, MAtom)
	).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

assumability_function_to_proto(not_assumable) = assumability_function(assumability_function_function_type_notassumable, no, no).
assumability_function_to_proto(const(Cost)) = assumability_function(assumability_function_function_type_const, yes(Cost), no).
assumability_function_to_proto(f(Name)) = assumability_function(assumability_function_function_type_named, no, yes(Name)).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

matom_to_proto(VS, m(Ms, A)) = modalised_atom(list.map(modality_to_proto, Ms), atom_to_proto(VS, A)).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

atom_to_proto(VS, p(PredSym, Args)) = atom(PredSym, list.map(term_to_proto(VS), Args)).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

term_to_proto(VS, t(Functor, Args)) = term(term_term_type_function, yes(Functor), list.map(term_to_proto(VS), Args), no).
term_to_proto(VS, v(Var)) = term(term_term_type_variable, no, [], yes(varset.lookup_name(VS, Var))).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

modality_to_proto(M) = PM :-
	proto_modality(PM, M).
