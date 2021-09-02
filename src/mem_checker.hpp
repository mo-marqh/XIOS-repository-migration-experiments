#ifndef __XIOS_MEM_CHECKER_HPP__
#define __XIOS_MEM_CHECKER_HPP__

#include <string>
#include <map>

namespace xios
{
  class CMemChecker
  {
    public:
      CMemChecker(const std::string& name);
      void suspend(void);
      void resume(void);
      void reset(void);
      double getCumulatedMem(void);
      static double getMem(void);
      static CMemChecker& get(std::string name);
      static std::string getAllCumulatedMem(void) ;
      static void disable(void) { enabled_=false ;}
      static void enable(void) {enabled_=true ;}
    private:
      static void check(void) ;
      double cumulatedMem_;
      double lastMem_;
      bool suspended_;
      std::string name_;

      static std::map<std::string,CMemChecker> allMemChecker_;
      static CMemChecker dummy_ ;
      static bool first_ ;
      static bool enabled_ ;
  };
}



#endif
