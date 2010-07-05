#ifndef TTYUTILS_H__
#define TTYUTILS_H__  1

#include <iostream>

namespace tty {

std::ostream & dcol(std::ostream & out);

std::ostream & black(std::ostream & out);
std::ostream & red(std::ostream & out);
std::ostream & green(std::ostream & out);
std::ostream & yellow(std::ostream & out);
std::ostream & blue(std::ostream & out);
std::ostream & magenta(std::ostream & out);
std::ostream & cyan(std::ostream & out);
std::ostream & white(std::ostream & out);

};

#endif
