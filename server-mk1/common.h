#ifndef COMMON_H__
#define COMMON_H__

#ifdef DEBUG
#define debug(COMMAND)  COMMAND
#else
#define debug(COMMAND)  { }
#endif

#ifdef ADDR_DEBUG
#define addr(VAR)  std::cerr << std::hex << VAR << std::dec << std::endl
#else
#define addr(VAR)  { }
#endif

#endif
