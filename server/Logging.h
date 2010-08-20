#ifndef LOGGING_H__
#define LOGGING_H__  1

#include <iostream>
#include "TtyUtils.h"

#define SERVER_MSG(arg)  tty::yellow << "[SERVER]" << tty::dcol << "  " << arg
#define REQUEST_MSG(arg)  tty::magenta << "[REQUEST]" << tty::dcol << " " << arg
#define WARNING_MSG(arg)  tty::red << "[WARNING]" << tty::dcol << " " << arg
#define ERROR_MSG(arg)  tty::red << "[ERROR] " << arg << tty::dcol
#define NOTIFY_MSG(arg)  tty::green << "[NOTIFY]" << tty::dcol << " " << arg

#endif
