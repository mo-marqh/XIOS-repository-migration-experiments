#ifndef __XIOS_BACKTRACE_HPP__
#define __XIOS_BACKTRACE_HPP__

#include <sstream>
#include <string>

namespace xios
{
  namespace MemTrack
  {
    void backTrace(std::ostringstream& stack, int n=0) ;

    inline const std::string backTrace(int n=0)
    {
      std::ostringstream stack ;
      backTrace(stack,n) ;
      return stack.str() ;
    }
  } 
}

#endif
