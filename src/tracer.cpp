#include "tracer.hpp"

#if defined(VTRACE)

#include <vt_user.h>

#elif defined(SCOREP)

#include <scorep/SCOREP_User.h>

#endif

#include <string>

namespace xios
{
  using namespace std ;
  
  void traceOn(void)
  {
#if defined(VTRACE)
    VT_ON() ;
#elif defined(SCOREP)
    SCOREP_RECORDING_ON()
#endif
  }
  
  void traceOff(void) 
  {
#if defined(VTRACE)
    VT_OFF() ;
#elif defined(SCOREP)
    SCOREP_RECORDING_OFF()
#endif
  }
  
  void traceBegin(const string& name)
  {
#if defined(VTRACE)
    VT_USER_START(name.c_str()) ;
#elif defined(SCOREP)
    SCOREP_USER_REGION_BY_NAME_BEGIN(name.c_str(),SCOREP_USER_REGION_TYPE_COMMON)
#endif
  }
  
  void traceEnd(const string& name)
  {
#ifdef defined(VTRACE)
    VT_USER_END(name.c_str()) ;
#elif defined(SCOREP)
    SCOREP_USER_REGION_BY_NAME_END(name.c_str())
#endif
  }
  
//  void marker(const string& name,const string& text) ;
  
  
}
