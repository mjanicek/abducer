:- module model_io.

:- interface.

:- import_module model.
:- import_module string.

:- func model_to_string(model(I, S, R)) = string.

%------------------------------------------------------------------------------%

:- implementation.
:- import_module map, set, list, pair.

model_to_string(WM) = Str :-
	map.to_assoc_list(WM^worlds, LWorlds),
	set.to_sorted_list(WM^access, LAccess),
	map.to_assoc_list(WM^props, LProps),
	Str = "({"
		++ string.join_list(",",
				list.map((func(W-Sort) = XStr :-
						XStr = string(W) ++ ":" ++ string(Sort)
					), LWorlds))
		++ "}, {"
		++ string.join_list(",",
				list.map((func({Rel,W1,W2}) = YStr :-
						YStr = string(W1) ++ "<" ++ string(Rel) ++ ">" ++ string(W2)
					), LAccess))
		++ "}, {"
		++ string.join_list(",",
				list.map((func(W-Props) = ZStr :-
						set.to_sorted_list(Props, LLProps),
						ZStr = string(W) ++ "={" ++ string.join_list(",", LLProps) ++ "}"
					), LProps))
		++ "})".
