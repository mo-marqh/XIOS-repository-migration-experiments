#ifndef __XIOS_CPin__
#define __XIOS_CPin__

#include "garbage_collector.hpp"

namespace xios
{
  class CContext ;
  class CPin
  {
    
    public:
      CPin(CGarbageCollector& gc) : context_(gc.getContext()) {} 
      CContext* getContext(void) {return context_;}
    
    protected:
      CContext* context_ ;
  } ;
}

#endif