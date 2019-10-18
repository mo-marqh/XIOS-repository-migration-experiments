#ifndef __XIOS_TIMER_HPP__
#define __XIOS_TIMER_HPP__

#include <string>
#include <map>

namespace xios
{
  class CTimer
  {
    public:
      double cumulatedTime;
      double lastTime;
      bool suspended;
      bool isTracing_ ;
      std::string name;

      CTimer(const std::string& name, bool trace=true);
      void suspend(void);
      void resume(void);
      void reset(void);
      double getCumulatedTime(void);
      static std::map<std::string,CTimer> allTimer;
      static double getTime(void);
      static CTimer& get(std::string name, bool trace=true);
      static std::string getAllCumulatedTime(void) ;
  };
}



#endif
