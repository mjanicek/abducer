#include "TtyUtils.h"

extern "C" {
#include <stdio.h>
#include <unistd.h>
}

using namespace std;

// terminal escape sequences

const char * RESET = "\033[0m";

const char * FG_BLACK   = "\033[30m";
const char * FG_RED     = "\033[31m";
const char * FG_GREEN   = "\033[32m";
const char * FG_YELLOW  = "\033[33m";
const char * FG_BLUE    = "\033[34m";
const char * FG_MAGENTA = "\033[35m";
const char * FG_CYAN    = "\033[36m";
const char * FG_WHITE   = "\033[37m";

// ----------------------------------------------------------------

ostream & tty_print(ostream & out, const char * seq)
{
	// FIXME: This is a nasty hack. There is no guarantee that
	// the output stream is actually stderr.
	if (isatty(fileno(stderr)))
		return out << seq;
	else
		return out;
}

// ----------------------------------------------------------------

ostream & tty::dcol(ostream & out) { return tty_print(out, RESET); }

ostream & tty::black(ostream & out)   { return tty_print(out, FG_BLACK); }
ostream & tty::red(ostream & out)     { return tty_print(out, FG_RED); }
ostream & tty::green(ostream & out)   { return tty_print(out, FG_GREEN); }
ostream & tty::yellow(ostream & out)  { return tty_print(out, FG_YELLOW); }
ostream & tty::blue(ostream & out)    { return tty_print(out, FG_BLUE); }
ostream & tty::magenta(ostream & out) { return tty_print(out, FG_MAGENTA); }
ostream & tty::cyan(ostream & out)    { return tty_print(out, FG_CYAN); }
ostream & tty::white(ostream & out)   { return tty_print(out, FG_WHITE); }
