:- module 'abducer-server-bin'.

:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

%------------------------------------------------------------------------------%

:- implementation.
:- import_module list, string, int.
:- import_module 'MercuryAbducerServer_mint', 'TypeConversions_mint'.

:- pragma foreign_decl("C", "#include \"aserv.h\"").

main(!IO) :-
	aserv_main(RetVal, !IO),
	io.set_exit_status(RetVal, !IO).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%

:- pred aserv_main(int::out, io::di, io::uo) is det.

:- pragma foreign_proc("C", aserv_main(RetVal::out, IO0::di, IO::uo),
		[may_call_mercury, promise_pure],
"
	RetVal = aserv_main();
	IO = IO0;
").
