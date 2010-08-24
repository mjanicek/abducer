/*
 * timeout.c
 *
 * Copyright (C) 2009-2010  Miroslav Janicek <sandius@matfyz.cz>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of version 2 of the GNU General Public License as
 * published by the Free Software Foundation.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */

/*
 * TODO:
 * - shortcut syntax? e.g. "timeout 10 ls -l"
 * - properly check errors everywhere
 */

#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>
#include <unistd.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <signal.h>
#include <pthread.h>

#include "sig2str.h"

#define MAX_SIGNALLERS  128

#define PROGRAM_NAME  "timeout"
#define VERSION       "0.2"

#define EXIT_CANNOT_INVOKE  127
#define EXIT_ERROR          128

/* TODO; mention return status here */
#define USAGE_INFO \
"Run a program and send it a signal or a sequence of signals after\n" \
"a given time.\n" \
"\n" \
"Usage: timeout TIMEOUT[...] [--] COMMAND ARG...\n" \
"\n" \
"TIMEOUT is a timeout specification, which can be of either of the following\n" \
"forms:\n" \
"\n" \
"  -SIGNAL_NAME MSEC    (e.g. \"-kill 10000\"), or\n" \
"  -SIGNAL_NUMBER MSEC  (e.g. \"-9 10000\")\n" \
"\n" \
"MSEC is an integer. At least one timeout specification is required.\n"

#define VERSION_INFO \
PROGRAM_NAME " " VERSION "\n" \
"Copyright (C) 2009-2010  Miroslav Janicek <sandius@matfyz.cz>\n" \
"\n" \
"This program is free software; you can redistribute it and/or modify\n" \
"it under the terms of version 2 of the GNU General Public License as\n" \
"published by the Free Software Foundation.\n"

static pid_t wrkpid = 0;

struct signaller {
	int signal;
	unsigned int usec;
} signallers[MAX_SIGNALLERS];

int num_signallers = 0;

void
ignore_signals(void);

unsigned int
string_to_useconds(const char * str);

void *
usleep_and_kill(void * arg);

char *
strtoupper(char * str);

int
main(int argc, char ** argv)
{
	int argi;

	int last_sig = 0;
	int last_sig_argind = 0;

	for (argi = 1; argv[argi] != NULL; ) {
		char * s = argv[argi];

		if (last_sig) {
			unsigned int t = string_to_useconds(s);
			if (t) {

				if (num_signallers < MAX_SIGNALLERS) {
					signallers[num_signallers].signal = last_sig;
					signallers[num_signallers].usec = t;
					++num_signallers;
				}
				else {
					fprintf(stderr, PROGRAM_NAME
							": Too many timeouts defined\n");
					exit(EXIT_ERROR);
				}

				last_sig = 0;
				++argi;
				continue;
			}
			else {
				fprintf(stderr, PROGRAM_NAME
						": Invalid argument at `%s'\n",
						argv[last_sig_argind]);
				exit(EXIT_ERROR);
			}
		}

		if (s[0] == '-') {
			int sig;

			if (strlen(s) == 1) {
				fprintf(stderr, PROGRAM_NAME ": Signal name expected\n");
				exit(EXIT_ERROR);
			}

			if (strlen(s) == 2) {
				if (s[1] == '-') {
					++argi;
					break;
				}
				switch (s[1]) {
				case 'v':
					fprintf(stdout, VERSION_INFO);
					exit(EXIT_SUCCESS);
				case 'h':
					fprintf(stdout, USAGE_INFO);
					exit(EXIT_SUCCESS);
				default:
					break;
				}
			}

			/* assume it's a signal name */
			strtoupper(s + 1);
			if (str2sig(s + 1, &sig) != -1) {
				last_sig = sig;
				last_sig_argind = argi;
			}
			else {
				fprintf(stderr, PROGRAM_NAME
						": Unknown signal: SIG%s\n", s + 1);
				exit(EXIT_ERROR);
			}
		}
		else {
			/* not a switch => it's the command */
			break;
		}

		++argi;
	}

/*
	int i;
	for (i = 0; i < num_signallers; ++i) {
		fprintf(stderr, "#%d: %s ... %d us\n", i+1, sys_signame[signallers[i].signal], signallers[i].usec);
	}
*/

	if (last_sig) {
		fprintf(stderr, PROGRAM_NAME ": `%s' requires an argument\n",
				argv[last_sig_argind]);
		exit(EXIT_ERROR);
	}

	if (argi == argc) {
		fprintf(stderr, PROGRAM_NAME ": No command to run\n");
		fprintf(stderr, PROGRAM_NAME ": Type " PROGRAM_NAME
				" -h for usage\n");
		exit(EXIT_ERROR);
	}

	if (num_signallers == 0) {
		fprintf(stderr, PROGRAM_NAME ": No timeout defined\n");
		fprintf(stderr, PROGRAM_NAME ": Type " PROGRAM_NAME
				" -h for usage\n");
		exit(EXIT_ERROR);
	}

	wrkpid = fork();

	if (wrkpid == -1) {
		perror(PROGRAM_NAME);
		exit(EXIT_ERROR);
	}

	if (wrkpid) {
		pid_t sigpid;

		ignore_signals();
		sigpid = fork();

		if (sigpid == -1) {
			/* unable to fork further, kill the worker process and exit */
			perror(PROGRAM_NAME);
			kill(wrkpid, SIGKILL);
			exit(EXIT_ERROR);
		}

		if (sigpid) {
			/* waiter */
			int status;

			waitpid(wrkpid, &status, WCONTINUED);
			kill(sigpid, SIGKILL);

			if (WIFEXITED(status)) {
				exit(WEXITSTATUS(status));
			}
			if (WIFSIGNALED(status)) {
				exit(128 + WTERMSIG(status));
			}
		}
		else {
			/* signaller */
			pthread_t sigthread[MAX_SIGNALLERS];

			int i;
			for (i = 0; i < num_signallers; ++i) {
				/* TODO: check for errors */
				pthread_create(&sigthread[i], NULL, usleep_and_kill,
						(void *) i);
			}
			pthread_exit(NULL);
		}
	}
	else {
		/* worker */
		argv = argv + argi;
		execvp(argv[0], argv);

		fprintf(stderr, PROGRAM_NAME ": %s: ", argv[0]);
		perror(NULL);
		switch (errno) {
		case ENOENT:
			exit(EXIT_ERROR);
		case EACCES:
		default:
			exit(EXIT_CANNOT_INVOKE);
		}
	}

	/* should never reach this */
	return EXIT_ERROR;
}

/*
 * Convert a string to useconds_t.
 * Return 0 if the string is not a valid number, i.e. a positive
 * integer.
 */
unsigned int
string_to_useconds(const char * str)
{
	int s = atoi(str);
	if (s > 0) {
		return (s * 1000);
	}
	else {
		return 0;
	}
}

/*
 * Ignore (almost) all signals.
 */
void
ignore_signals(void)
{
	int i;
	for (i = 1; i < 32; ++i) {
		switch (i) {

		/* SIGCHLD must pass through so that wait() would work */
		case SIGCHLD:
			break;

		default:
			signal(i, SIG_IGN);
			break;
		}
	}
}

void *
usleep_and_kill(void * arg)
{
	int signal = signallers[(int)arg].signal;
	unsigned int usec = signallers[(int)arg].usec;
	usleep(usec);
	kill(wrkpid, signal);
	pthread_exit(NULL);
}


/*
 * Convert the string to upper case.
 */
char *
strtoupper(char * str)
{
	char * s = str;
	while (*s) {
		*s = toupper(*s);
		++s;
	}
	return str;
}
