#include "log_type.hpp"
#include "cxios.hpp"

namespace xios
{
  using namespace std ;
  
  int CLogType::globalBitPos_ = 0 ;

  CLogType::CLogType(const std::string& name)
  {
    bitPos_ = CSetLog::registerLog(name, globalBitPos_) ;
    if (bitPos_==-1) 
    {  
       bitPos_ = globalBitPos_ ;
       globalBitPos_++ ;
       if (globalBitPos_>63) ERROR("CLogType::CLogType(const std::string& name)",<<"max registred Log must be <64, need to modify CLogTyPe class algorithm") ;
    }
    type_ = (uint64_t)1 << bitPos_ ;
  }

  CSetLog::CSetLog(void)
  {
    std::string strIds = CXios::getin<std::string>("log_type","") ;
    std::vector<std::string> names = splitRegex(strIds,"\\s* \\s*") ;
    
    auto& registredLog=getRegistredLog() ;

    for(auto& name : names) 
    {
      auto it = registredLog.find(name) ;
      if (it != registredLog.end()) CLogType::bitSet(logType_, it->second) ;
    }
  }

  int CSetLog::registerLog(const std::string& name, int bitPos) 
  { 
    auto& registredLog = getRegistredLog() ;
    auto it = registredLog.find(name) ;
    if (it == registredLog.end()) 
    {
      registredLog.insert(pair<string, int>(name, bitPos)) ;
      return -1 ;
    }
    else return it->second ;
  }
  
  std::map<std::string, int>& CSetLog::getRegistredLog(void)
  {
    static std::map<std::string, int> registredLog ;
    return registredLog ;
  }

}
