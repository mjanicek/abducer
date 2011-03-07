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
% Based on tcp.m from Mercury Extras, written by Peter Ross.
%------------------------------------------------------------------------------%

:- module unix_socket.
:- interface.

:- import_module io.
:- import_module stream, bitmap.
:- import_module string.

%-----------------------------------------------------------------------------%

:- type unix_socket.
:- type bound_unix_socket.

:- type unix_socket.result(T)
    --->    ok(T)
    ;       error(string).

:- type address == string.

:- pred unix_socket.connect(address::in, unix_socket.result(unix_socket)::out, io::di, io::uo)
    is det.

:- pred unix_socket.shutdown(unix_socket::in, io::di, io::uo) is det.

:- func socket_fd(unix_socket) = int.
    
    % Sending data to a broken pipe will cause the SIGPIPE signal to be
    % sent to the process.  If SIGPIPE is ignored or blocked then send()
    % fails with EPIPE.  This predicate causes SIGPIPE signals to be
    % ignored.
    %
:- pred unix_socket.ignore_sigpipe(io::di, io::uo) is det.

    % Restores the SIGPIPE signal handler before the last
    % unix_socket.ignore_sigpipe() call.
    %
:- pred unix_socket.unignore_sigpipe(io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%
% Stream type class instances
%

:- type unix_socket.error.

:- instance stream(unix_socket, io.state).
:- instance error(unix_socket.error).

:- instance input(unix_socket, io.state).
:- instance reader(unix_socket, byte, io.state, unix_socket.error).

:- instance output(unix_socket, io.state).
:- instance writer(unix_socket, byte, io.state).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- import_module char.
:- import_module list.
:- import_module require.

%-----------------------------------------------------------------------------%

:- type unix_socket
    --->    unix_socket(
                name    :: string,
                handle  :: unix_socket_handle
            ).

:- type bound_unix_socket
    --->    bound_unix_socket(
                int,        % socket fd
                c_pointer   % struct sockaddr
            ).

%-----------------------------------------------------------------------------%

unix_socket.connect(Path, Result, !IO) :-
    handle_connect(Path, Handle, Errno, !IO),
    (if Errno = 0
    then Result = ok(unix_socket(Path, Handle))
    else Result = unix_socket.error(unix_socket.error_message(Errno))
    ).

%-----------------------------------------------------------------------------%

unix_socket.shutdown(unix_socket(_, Handle), !IO) :-
    handle_shutdown(Handle, !IO).

:- pred handle_shutdown(unix_socket_handle::in, io::di, io::uo) is det.
:- pragma foreign_proc("C",
    handle_shutdown(UNSOCK::in, IO0::di, IO::uo),
    [may_call_mercury, thread_safe, promise_pure, tabled_for_io],
"
    struct linger sockets_linger = { MR_TRUE, 2 };
    ML_unix_socket *sock;
    int shutdown_erro = 0;

    sock = (ML_unix_socket *) UNSOCK;

/*  setsockopt(sock->socket, SOL_SOCKET, SO_LINGER,
            &sockets_linger, sizeof(sockets_linger));*/

    errno=0;      
    if (close(((int)sock->socket)) == SOCKET_ERROR) {
        ML_throw_unix_socket_exception((MR_String) ""unix_socket.shutdown failed (close)"");
    }

    IO = IO0;
").

%-----------------------------------------------------------------------------%

:- type unix_socket_handle
    --->    socket(c_pointer).

:- pragma foreign_decl("C", "
#include <errno.h>
#include <unistd.h>
#include <netdb.h>

#include <sys/types.h>
#include <sys/socket.h>
#include <sys/un.h>

#define  ML_error()       errno

#define  INVALID_SOCKET   -1
#define  SOCKET_ERROR     -1

#define BACKLOG 16
#define FULL    2

#define UNSOCK_BUFSIZE     1024

typedef struct {
    int socket;
    int error;
    size_t  buf_len;
    off_t   buf_pos;
    char    buf[UNSOCK_BUFSIZE];
} ML_unix_socket;
").

:- pred handle_connect(string::in, unix_socket_handle::out, int::out,
    io::di, io::uo) is det.

:- pragma foreign_proc("C",
    handle_connect(Path::in, UNSOCK::out, Errno::out, IO0::di, IO::uo), 
    [will_not_call_mercury, thread_safe, promise_pure, tabled_for_io],
"
    ML_unix_socket      *sock;
    struct sockaddr_un  *addr;

    sock = MR_GC_NEW(ML_unix_socket);

    sock->socket = socket(PF_UNIX, SOCK_STREAM, 0);
    sock->error = 0;
    sock->buf_len = 0;
    sock->buf_pos = 0;

    if (sock->socket == INVALID_SOCKET) {
        sock->error = ML_error();
    }
    else {
        addr = MR_GC_NEW(struct sockaddr_un);
        MR_memset(addr, 0, sizeof(struct sockaddr_un));
        addr->sun_family = AF_UNIX;
        MR_memcpy(&(addr->sun_path), Path, strlen(Path) + 1);
        size_t addrlen = sizeof(addr->sun_family) + strlen(addr->sun_path) + 1;

        if (connect(sock->socket, (struct sockaddr *)addr, addrlen) == SOCKET_ERROR) {
            sock->error = ML_error();
        }
    }

    Errno = sock->error;
    UNSOCK = (MR_Word) sock;
    IO = IO0;
").

socket_fd(UnSock) = socket_fd_c(UnSock ^ handle).

:- func socket_fd_c(unix_socket_handle) = int.
:- pragma foreign_proc("C",
    socket_fd_c(UnSock::in) = (FD::out),
    [will_not_call_mercury, thread_safe, promise_pure],
"
    ML_unix_socket *sock = (ML_unix_socket *) UnSock;
    FD = sock->socket;
").

%-----------------------------------------------------------------------------%

:- type unix_socket.error
    --->    errno(int).

:- instance stream(unix_socket, io.state) where [
    name(UNSOCK, UNSOCK ^ name, !IO)
].

:- instance error(unix_socket.error) where [
    (error_message(E) = S :-
        get_error(E, S)
    )
].

:- instance input(unix_socket, io.state) where [].

:- instance reader(unix_socket, byte, io.state, unix_socket.error) where [
    (get(T, Result, !IO) :-
        unix_socket.read_char(T ^ handle, Char, ErrCode, !IO),
        ( ErrCode = -1 ->
            Result = eof
        ; ErrCode = -2 ->
            get_errno(T ^ handle, Errno, !IO),
            Result = error(Errno)
        ;
            Result = ok(Char)
        )
    )
].

:- instance output(unix_socket, io) where [
        % XXX can one flush a socket?
    flush(_, !IO)
].

:- instance writer(unix_socket, byte, io.state) where [
    (put(T, C, !IO) :-
        unix_socket.write_char(T ^ handle, C, B, !IO),
        ( B = yes,
            true
        ; B = no,
            get_errno(T ^ handle, Errno, !IO),
            get_error(Errno, String),
            error("put(char): " ++ String)
        )
    )
].

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- pragma foreign_decl("C", "
    /*
    ** Note: some Mercury code uses the -1 and -2 constants directly.
    */
    #define UNSOCK_EOF     -1
    #define UNSOCK_ERROR   -2

    int UNSOCK_get_char(ML_unix_socket *sock, int *ec);
").

:- pragma foreign_code("C", "
    int UNSOCK_get_char(ML_unix_socket *sock, int *ec)
    {
        if (sock->buf_pos >= sock->buf_len) {
            /* Refill buffer. */
            ssize_t nchars = read(sock->socket,
                sock->buf, sizeof(sock->buf));
            if (nchars == SOCKET_ERROR) {
                sock->error = ML_error();
                *ec = UNSOCK_ERROR;
				return 0;
            } else if (nchars == 0) {
                *ec = UNSOCK_EOF;
				return 0;
            } else {
				*ec = 0;
                sock->buf_pos = 1;
                sock->buf_len = nchars;
                return ((unsigned char)sock->buf[0]);
            }
        } else {
			*ec = 0;
            return sock->buf[sock->buf_pos++];
        }
    }
").

:- pred unix_socket.read_char(unix_socket_handle::in, int::out, int::out, io::di, io::uo) is det.
:- pragma foreign_proc("C",
    unix_socket.read_char(Socket::in, Chr::out, ErrCode::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, thread_safe, promise_pure, tabled_for_io],
"
    ML_unix_socket *sock = (ML_unix_socket *) Socket;
/*    fprintf(stderr, \"{\"); */
	int ec;
    Chr = UNSOCK_get_char(sock, &ec);
	ErrCode = ec;
/*    fprintf(stderr, \"%c,%d}\", (char)Chr, Chr); */
").

:- pred unix_socket.write_char(unix_socket_handle::in, byte::in, bool::out,
    io::di, io::uo) is det.
:- pragma foreign_proc(c,
    unix_socket.write_char(Socket::in, Chr::in, Success::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, thread_safe, promise_pure, tabled_for_io],
"
    ML_unix_socket *sock = (ML_unix_socket *) Socket;

    if (send(sock->socket, &Chr, 1, 0) == SOCKET_ERROR) {
        sock->error = ML_error();
        Success = MR_FALSE;
    } else {
        Success = MR_TRUE;
    }
").

:- pred get_errno(unix_socket_handle::in, unix_socket.error::out, io::di, io::uo) is det.
:- pragma foreign_proc("C",
    get_errno(Socket::in, Errno::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, thread_safe, promise_pure, tabled_for_io],
"
    ML_unix_socket *sock = (ML_unix_socket *) Socket;
    Errno = sock->error;
").

:- pred unix_socket.get_error(unix_socket.error::in, string::out) is det.
:- pragma foreign_proc("C",
    unix_socket.get_error(Errno::in, Msg::out),
    [will_not_call_mercury, thread_safe, promise_pure],
"
    MR_save_transient_hp();
    MR_make_aligned_string_copy(Msg, strerror(Errno));
    MR_restore_transient_hp();
").

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- func error_message(int) = string.

:- pragma foreign_proc("C",
    error_message(Errno::in) = (Err::out),
    [promise_pure, will_not_call_mercury],
"
    MR_make_aligned_string_copy(Err, strerror(Errno));
").

:- pred throw_unix_socket_exception(string::in) is erroneous.
:- pragma foreign_export("C", throw_unix_socket_exception(in),
    "ML_throw_unix_socket_exception").

throw_unix_socket_exception(S) :-
    error(S).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- pragma foreign_decl("C", "
    #include <signal.h>

    extern void *UNSOCK__prev_sigpipe_handler;
").

:- pragma foreign_code("C", "
    void *UNSOCK__prev_sigpipe_handler = SIG_DFL;
").

:- pragma foreign_proc("C",
    unix_socket.ignore_sigpipe(IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    UNSOCK__prev_sigpipe_handler = signal(SIGPIPE, SIG_IGN);
    IO = IO0;
").

:- pragma foreign_proc("C",
    unix_socket.unignore_sigpipe(IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    signal(SIGPIPE, UNSOCK__prev_sigpipe_handler);
    IO = IO0;
").
