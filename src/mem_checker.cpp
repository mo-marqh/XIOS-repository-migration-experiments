#include "mem_checker.hpp"
#include "mpi.hpp"
#include <string>
#include <map>
#include <iostream>
#include <sstream>
#include <fstream>

namespace xios
{
  CMemChecker CMemChecker::dummy_("") ;
  std::map<std::string,CMemChecker> CMemChecker::allMemChecker_;
  bool CMemChecker::enabled_=true;
  bool CMemChecker::first_=true;

  CMemChecker::CMemChecker(const std::string& name) : name_(name) 
  { 
    if (first_) check() ;
    reset();
  }

  void CMemChecker::check(void)
  {
    std::ifstream statStream("/proc/self/stat",std::ios_base::in);
    enabled_ &= statStream.good() ;
    first_=false ;
  }
  double CMemChecker::getMem(void)
  {
    if (first_) check() ;
    if (!enabled_) return 0;
    std::ifstream statStream("/proc/self/stat",std::ios_base::in);
    std::string dummy ;
    for(int i=1;i<=22;i++) statStream>>dummy ;
    unsigned long vsize; 
    statStream>>vsize ;
    return vsize ;
  }
  
  void CMemChecker::suspend(void)
  {
    if (first_) check() ;
    if (!enabled_) return ;
    if (!suspended_) cumulatedMem_ += getMem() - lastMem_;
    suspended_ = true;
  }
  
  void CMemChecker::resume(void)
  {
    if (first_) check() ;
    if (!enabled_) return ;
    if (suspended_) lastMem_ = getMem();
    suspended_ = false;
  }
  
  void CMemChecker::reset(void)
  {
    if (first_) check() ;
    if (!enabled_) return ;
    cumulatedMem_ = 0.;
    suspended_ = true;
  }
  
  double CMemChecker::getCumulatedMem(void)
  {
    if (first_) check() ;
    if (!enabled_) return 0;
    return cumulatedMem_;
  }
  
  CMemChecker& CMemChecker::get(const std::string name)
  {
    if (first_) check() ;
    if (!enabled_) return dummy_ ;
    else
    {
      std::map<std::string,CMemChecker>::iterator it = allMemChecker_.find(name);
      if (it == allMemChecker_.end())
        it = allMemChecker_.insert(std::make_pair(name, CMemChecker(name))).first;
      return it->second;
    }
  }

  std::string CMemChecker::getAllCumulatedMem(void)
  {
    if (first_) check() ;
    if (!enabled_) return std::string(" MemChecker : memory consumption report not available") ; 
    std::ostringstream strOut ;
    const double Kb=1024 ;
    const double Mb=Kb*1024 ;
    const double Gb=Mb*1024 ;
    const double Tb=Mb*1024 ;
    for(std::map<std::string,CMemChecker>::iterator it=allMemChecker_.begin();it!=allMemChecker_.end();++it)
    {  
      strOut<<"MemChecker : "<<it->first<<"    -->   consumed memory : " ;
      double mem=it->second.getCumulatedMem() ;
      if (mem>=Tb) strOut<< mem / Tb<<" Tb"<<std::endl ;
      else if (mem>=Gb) strOut<< mem / Gb<<" Gb"<<std::endl ;
      else if (mem>=Mb) strOut<< mem / Mb<<" Mb"<<std::endl ;
      else if (mem>=Kb) strOut<< mem / Kb<<" Kb"<<std::endl ;
      else strOut<< mem <<" bytes"<<std::endl ;
    }
    return strOut.str() ;
  }
}
