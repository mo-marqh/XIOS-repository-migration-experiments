#include "tracer.hpp"
#ifdef VTRACE
//#include <vt_user.h>
#include <VT.h>
#endif
#include <string>

namespace xios
{
  using namespace std ;
  
  void traceOn(void)
  {
#ifdef VTRACE
    //VT_ON() ;
    VT_traceon() ;
#endif
  }
  
  void traceOff(void) 
  {
#ifdef VTRACE
    //VT_OFF() ;
    VT_traceoff() ;
#endif
  }
  
  void traceBegin(const string& name)
  {
#ifdef VTRACE
    //VT_USER_START(name.c_str()) ;
#endif
  }
  
  void traceEnd(const string& name)
  {
#ifdef VTRACE
    //VT_USER_END(name.c_str()) ;
#endif
  }
  
//  void marker(const string& name,const string& text) ;
  
  
}
