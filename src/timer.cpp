#include "timer.hpp"
#include "mpi.hpp"
#include <string>
#include <map>
#include <iostream>
#include <sstream>
#include "tracer.hpp"

namespace xios
{
  std::map<std::string,CTimer> CTimer::allTimer;
  
  CTimer::CTimer(const std::string& name_, bool trace) : name(name_) 
  { 
    isTracing_=trace ;
    reset();
  }

  double CTimer::getTime(void)
  {
    return MPI_Wtime();
  }
  
  void CTimer::suspend(void)
  {
    if (!suspended) 
    {
      if (isTracing_) traceEnd(name);
      cumulatedTime += getTime() - lastTime;
    }
    suspended = true;
  }
  
  void CTimer::resume(void)
  {
    if (suspended) 
    {
      lastTime = getTime();
      if (isTracing_) traceBegin(name);
    }
    suspended = false;
  }
  
  void CTimer::reset(void)
  {
    cumulatedTime = 0.;
    suspended = true;
  }
  
  double CTimer::getCumulatedTime(void)
  {
    return cumulatedTime;
  }
  
  CTimer& CTimer::get(const std::string name, bool trace)
  {
    std::map<std::string,CTimer>::iterator it = allTimer.find(name);
    if (it == allTimer.end())
      it = allTimer.insert(std::make_pair(name, CTimer(name,trace))).first;
    return it->second;
  }

  string CTimer::getAllCumulatedTime(void)
  {
    std::ostringstream strOut ;
    for(std::map<std::string,CTimer>::iterator it=allTimer.begin();it!=allTimer.end();++it)
      strOut<<"Timer : "<<it->first<<"    -->   cumulated time : "<<it->second.getCumulatedTime()<<std::endl ;
    return strOut.str() ;
  }
}
